// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License. See the top level LICENSE file for the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/sail/eval.rs

// Iterative stack evaluator for Sail. Defines a stack format, utility
// functions, and logic to evaluate any valid Sail expression one
// stack frame at a time.

// <>

use super::core::*;
use super::memmgt;
use super::{SP_DEF, SP_DO, SP_EVAL, SP_FN, SP_IF, SP_QUOTE, SP_SET, SP_WHILE};

use std::alloc;
use std::convert::TryInto;
use std::ptr;

// TODO: reference counting and garbage collection have to work with
// the references kept by the evaluation machinery

/// Sail evaluation stack
pub struct EvalStack {
    /// First (bottom) element of the stack
    stack_start: *mut usize,
    /// Maximum stack element address
    stack_max: *mut usize,
    /// Current top of the stack (frame_start is more useful)
    stack_top: *mut usize,
    /// Start of the stack's top frame
    frame_start: *mut usize,
}

impl EvalStack {
    /// Create and initialize a new Sail stack and associated memory
    pub fn new(size: usize) -> Self {
        let size_bytes = size * 8;
        unsafe {
            let layout = alloc::Layout::from_size_align_unchecked(size_bytes, 8);
            let stack = alloc::alloc(layout);

            EvalStack {
                stack_start: stack as *mut usize,
                stack_max: stack.add(size_bytes) as *mut usize,
                stack_top: stack as *mut usize,
                frame_start: stack as *mut usize,
            }
        }
    }

    /// Resize the stack, acquiring new memory if necessary
    fn resize(&mut self, size: usize) {
        let size_bytes = size * 8;
        let old_start = self.stack_start as usize;
        let old_top = self.stack_top as usize;
        let top_offset = old_top - old_start;
        if top_offset > size_bytes {
            return;
        }

        let old_max = self.stack_max as usize;
        let frame_offset = self.frame_start as usize - old_start;

        let (new_start, new_max, new_top, new_frame) = unsafe {
            let old_layout = alloc::Layout::from_size_align_unchecked(old_max - old_start, 8);
            let start = alloc::realloc(self.stack_start as *mut u8, old_layout, size_bytes);
            (
                start as *mut usize,
                start.add(size_bytes) as *mut usize,
                start.add(top_offset) as *mut usize,
                start.add(frame_offset) as *mut usize,
            )
        };

        if new_start as usize != old_start {
            // iterate over the stack, updating those pointers that
            // point elsewhere within the stack to conform with the
            // new memory location
            for p in 0..=(top_offset / 8) {
                unsafe {
                    let address = new_start.add(p);
                    let pointer = ptr::read(address);

                    if pointer >= old_start && pointer <= old_top {
                        let addr_offset = pointer - old_start;
                        ptr::write(address, new_start as usize + addr_offset);
                    }
                }
            }
        }

        self.stack_start = new_start;
        self.stack_max = new_max;
        self.stack_top = new_top;
        self.frame_start = new_frame;
    }

    /// Pushes a single word to the stack, which will always be a
    /// pointer to a Sail object
    #[inline(always)]
    pub fn push(&mut self, word: SlHndl) {
        if cfg!(feature = "stkdbg") {
            print!("push 1; ");
        }

        unsafe {
            if self.stack_top as usize + 8 >= self.stack_max as usize - 8 {
                self.resize((self.stack_max as usize - self.stack_start as usize) / 4)
            }
            let new_top = self.stack_top.add(1);
            self.stack_top = new_top;

            let wordr = word.get_raw();
            inc_refc(wordr);
            ptr::write(new_top, wordr as usize);
        }

        if cfg!(feature = "stkdbg") {
            println!(
                "size: {}",
                (self.stack_top as usize - self.stack_start as usize) / 8
            );
        }
    }

    fn push_slot(&mut self) {
        unsafe { self.stack_top = self.stack_top.add(1) }
    }

    /// Pops a single word off the stack
    #[inline(always)]
    fn pop(&mut self) {
        if cfg!(feature = "stkdbg") {
            print!("pop 1; ");
        }

        unsafe {
            let word = *(self.stack_top) as *mut SlHead;

            if !nil_p(word) && dec_refc(word) {
                destroy_obj(self.frame_top().1, word)
            }

            self.stack_top = self.stack_top.sub(1);
        }

        if cfg!(feature = "stkdbg") {
            println!(
                "size: {}",
                (self.stack_top as usize - self.stack_start as usize) / 8
            );
        }
    }

    /// Push a complete frame head onto the stack
    ///
    /// Components:
    /// - `ret`: return address for this frame's result
    /// - `opc`: opcode which dictates this frame's format & behavior
    /// - `env`: environment containing bindings for this frame
    #[inline(always)]
    pub fn push_frame_head(&mut self, mut ret: *mut *mut SlHead, opc: Opcode, env: SlHndl) {
        if cfg!(feature = "stkdbg") {
            print!("PUSH: {:?}; ", opc);
        }

        unsafe {
            if self.stack_top as usize + 24 >= self.stack_max as usize - 8 {
                let old_top = self.stack_top as usize;
                self.resize((self.stack_max as usize - self.stack_start as usize) / 4);
                ret = ((ret as usize - old_top) + self.stack_top as usize) as *mut *mut SlHead;
            }
            let new_start = self.stack_top.add(1);
            self.stack_top = new_start.add(2);

            ptr::write(
                new_start.add(FrameOffset::LastTop as usize),
                self.frame_start as usize,
            );
            ptr::write(new_start.add(FrameOffset::Return as usize), ret as usize);
            ptr::write(
                new_start.add(FrameOffset::EnvOpc as usize),
                ((env.get_raw() as usize) << 16) + opc as usize,
            );

            inc_refc(env.get_raw());

            self.frame_start = new_start;
        }

        if cfg!(feature = "stkdbg") {
            println!(
                "size: {}",
                (self.stack_top as usize - self.stack_start as usize) / 8
            );
        }
    }

    /// Pop a full frame off of the stack
    #[inline(always)]
    fn pop_frame(&mut self) {
        if cfg!(feature = "stkdbg") {
            print!("POP: {:?}; ", self.frame_opc());
        }

        // TODO: IMPORTANT: decrement reference counts of all Sail
        // objects referenced by the frame, then of the frame
        // environment

        let env = self.frame_top().1;

        unsafe {
            let this_frame_top = self.frame_start;
            let this_frame_bot = self.stack_top;

            let last_frame_top = ptr::read(this_frame_top);

            // TODO: ...

            for p in std::slice::from_raw_parts_mut(
                (this_frame_top.add(FrameOffset::ArgZero as usize)) as *mut *mut SlHead,
                (this_frame_bot as usize
                    - (this_frame_top as usize + (FrameOffset::ArgZero as usize * 8)))
                    / 8,
            ) {
                let o = *p;
                if !nil_p(o) && dec_refc(o) {
                    destroy_obj(env.clone(), o);
                }
            }

            dec_refc(env.get_raw());

            self.stack_top = this_frame_top.sub(1);
            self.frame_start = last_frame_top as *mut usize;
        }

        if cfg!(feature = "stkdbg") {
            println!(
                "size: {}",
                (self.stack_top as usize - self.stack_start as usize) / 8
            );
        }
    }

    // TODO: use more of a condition system than an exception system eventually
    fn unwind(&mut self, error: SlHndl) {
        // destroy stack frames until reaching an error catch or the bottom
        while self.frame_start > self.stack_start {
            self.pop_frame();
        }

        unsafe {
            let out = error.get_raw();
            ptr::write(self.frame_ret(), out);
            inc_refc(out);
        }
    }

    /// Starts evaluating a Sail expression with the provided return location
    ///
    /// Returns false and does nothing if the stack is already in use
    pub fn start(&mut self, ret: *mut *mut SlHead, env: SlHndl, expr: SlHndl) -> bool {
        if !self.is_empty() {
            false
        } else {
            self.eval_expr(ret, env, expr);
            true
        }
    }

    /// Starts evaluating a Sail expression that will not return outside the stack
    ///
    /// Works even when other expressions are evaluating on the stack
    ///
    /// **Warning**:
    /// - Takes over the stack until the expression is finished
    /// - An error in this expression destroys the entire stack
    /// - This function is temporary and using it is a bad idea
    pub fn start_no_ret(&mut self, env: SlHndl, expr: SlHndl) {
        if expr.nnil_ref_p() {
            self.push_frame_head(self.stack_max as *mut *mut SlHead, Opcode::Eval, env);
            self.push(ref_get(expr).unwrap());
        }
    }

    /// Determines whether the stack is currently empty
    #[inline(always)]
    pub fn is_empty(&mut self) -> bool {
        self.stack_start == self.stack_top
    }

    /// Gets the return address of the current top frame
    #[inline(always)]
    fn frame_ret(&mut self) -> *mut *mut SlHead {
        unsafe {
            ptr::read(self.frame_start.add(FrameOffset::Return as usize) as *mut *mut *mut SlHead)
        }
    }

    /// Gets the opcode of the current top frame
    #[inline(always)]
    fn frame_opc(&mut self) -> Opcode {
        unsafe {
            ((ptr::read(self.frame_start.add(FrameOffset::EnvOpc as usize)) & 0x000000000000FFFF)
                as u8)
                .try_into()
                .unwrap()
        }
    }

    /// Returns all frame head components of the current top frame
    #[inline(always)]
    fn frame_top(&mut self) -> (*mut *mut SlHead, SlHndl, Opcode) {
        let env_and_opc = unsafe { ptr::read(self.frame_start.add(FrameOffset::EnvOpc as usize)) };
        let env = unsafe { SlHndl::from_raw_unchecked((env_and_opc >> 16) as _) };
        inc_refc(unsafe { env.get_raw() });
        (
            self.frame_ret(),
            env,
            ((env_and_opc & 0x000000000000FFFF) as u8)
                .try_into()
                .unwrap(),
        )
    }

    /// Returns the address of a location offset into the current top
    /// frame
    #[inline(always)]
    fn frame_addr(&mut self, offset: usize) -> *mut *mut SlHead {
        unsafe { self.frame_start.add(FrameOffset::ArgZero as usize + offset) as *mut *mut SlHead }
    }

    /// Returns the pointer stored in a location offset into the
    /// current top frame
    #[inline(always)]
    fn frame_obj(&mut self, offset: usize) -> SlHndl {
        unsafe {
            let obj = SlHndl::from_raw_unchecked(ptr::read(self.frame_addr(offset)) as *mut SlHead);
            inc_refc(obj.get_raw());
            obj
        }
    }

    /// Evaluates any Sail expression in object form, using the given
    /// environment and returning to the given location
    #[inline(always)]
    fn eval_expr(&mut self, ret: *mut *mut SlHead, env: SlHndl, expr: SlHndl) {
        if expr.nnil_ref_p() {
            self.push_frame_head(ret, Opcode::Eval, env);
            self.push(ref_get(expr).unwrap());
        } else {
            let out = if expr.basic_sym_p() {
                match env_lookup(env, expr) {
                    Some(obj) => unsafe { obj.get_raw() },
                    None => nil(),
                }
            } else {
                unsafe { expr.get_raw() }
            };

            if !nil_p(out) {
                inc_refc(out);
            }

            unsafe { ptr::write(ret, out) };
        }
    }

    /// Consumes one frame off the top of the stack and executes it
    ///
    /// This is the core of Sail evaluation logic. In the limit,
    /// executing this function repeatedly will evaluate any Sail
    /// expression. It handles all defined opcodes and adds more
    /// frames to the stack as necessary, but never uses recursion.
    pub fn iter_once(&mut self, reg: *mut memmgt::Region, tbl: SlHndl) -> bool {
        // ***********************************
        // * Sail stack-based evaluation logic
        // ***********************************

        if self.is_empty() {
            return false;
        }

        let (ret, env, opc) = self.frame_top();

        if cfg!(feature = "stkdbg") {
            println!("ENTER: {:?}", opc);
        }

        match opc {
            Opcode::PreEval => {
                let expr = self.frame_obj(0);
                self.pop_frame();
                self.eval_expr(ret, env, expr);
            }
            Opcode::Eval => {
                let list = self.frame_obj(0);
                self.pop_frame();

                let raw_op = list.clone();
                let raw_args = get_next_list_elt(list);

                if raw_op.basic_sym_p() {
                    match sym_get_id(raw_op.clone()) {
                        id if id == SP_DEF.0 => {
                            // needs: symbol to bind, object to bind to it
                            self.push_frame_head(ret, Opcode::Bind, env.clone());
                            self.push(raw_args.clone().unwrap());
                            self.push_slot();

                            let value = get_next_list_elt(raw_args.unwrap()).unwrap();
                            let return_to = self.frame_addr(1);

                            self.eval_expr(return_to, env, value);
                            return true;
                        }
                        id if id == SP_DO.0 => {
                            // needs: current remaining list of expressions
                            self.push_frame_head(ret, Opcode::DoSeq, env);
                            self.push(raw_args.unwrap());
                            return true;
                        }
                        id if id == SP_EVAL.0 => {
                            self.push_frame_head(ret, Opcode::PreEval, env.clone());
                            self.push_slot();

                            let return_to = self.frame_addr(0);

                            self.eval_expr(return_to, env, raw_args.unwrap());
                            return true;
                        }
                        id if id == SP_FN.0 => {
                            // needs: nothing else evaluated
                            // TODO: type annotations
                            let argvec = raw_args.unwrap();
                            let argct = stdvec_get_len(argvec.clone()) as u16;
                            let proc = proc_lambda_make(reg, argct);
                            for i in 0..argct {
                                proc_lambda_set_arg(
                                    proc.clone(),
                                    i,
                                    sym_get_id(stdvec_idx(argvec.clone(), i as u32)),
                                );
                            }
                            proc_lambda_set_body(proc.clone(), get_next_list_elt(argvec).unwrap());
                            unsafe {
                                let out = proc.get_raw();
                                ptr::write(ret, out);
                                inc_refc(out);
                            }
                            return true;
                        }
                        id if id == SP_IF.0 => {
                            // needs: evaluated test and both branches
                            self.push_frame_head(ret, Opcode::Branch, env.clone());
                            self.push_slot();
                            self.push(get_next_list_elt(raw_args.clone().unwrap()).unwrap());
                            self.push(
                                get_next_list_elt(
                                    get_next_list_elt(raw_args.clone().unwrap()).unwrap(),
                                )
                                .unwrap(),
                            );

                            let return_to = self.frame_addr(0);

                            self.eval_expr(return_to, env, raw_args.unwrap());
                            return true;
                        }
                        id if id == SP_QUOTE.0 => {
                            // needs: nothing else evaluated
                            unsafe {
                                let out = raw_args.unwrap().get_raw();
                                ptr::write(ret, out);
                                inc_refc(out);
                            }
                            return true;
                        }
                        id if id == SP_SET.0 => {
                            // needs: symbol to bind, object to bind to it
                            self.push_frame_head(ret, Opcode::Mutate, env.clone());
                            self.push(raw_args.clone().unwrap());
                            self.push_slot();

                            let value = get_next_list_elt(raw_args.unwrap()).unwrap();
                            let return_to = self.frame_addr(1);

                            self.eval_expr(return_to, env, value);
                            return true;
                        }
                        id if id == SP_WHILE.0 => {
                            self.push_frame_head(ret, Opcode::While, env.clone());
                            self.push(raw_args.clone().unwrap());
                            self.push_slot();
                            self.push(get_next_list_elt(raw_args.clone().unwrap()).unwrap());

                            let return_to = self.frame_addr(1);

                            self.eval_expr(return_to, env, raw_args.unwrap());
                            return true;
                        }
                        _ => {}
                    }
                }

                if raw_op.nnil_ref_p() {
                    self.push_frame_head(ret, Opcode::PreApp, env.clone());
                    self.push_slot();
                    self.push(raw_args.unwrap());

                    let return_to = self.frame_addr(0);

                    self.push_frame_head(return_to, Opcode::Eval, env);
                    self.push(ref_get(raw_op).unwrap());
                } else {
                    let proc = if raw_op.basic_sym_p() {
                        match env_lookup(env.clone(), raw_op) {
                            Some(obj) => obj,
                            None => panic!("symbol not bound in env"),
                        }
                    } else {
                        raw_op
                    };
                    assert!(proc.proc_p());

                    self.push_frame_head(ret, Opcode::Apply, env.clone());

                    self.push(proc.clone());
                    for _ in 0..proc_get_argct(proc.clone()) {
                        self.push_slot();
                    }
                    let apply_start = self.frame_start;

                    // TODO: loop below could probably be much simpler

                    'tr: for i in 0..proc_get_argct(proc.clone()) {
                        let mut arg = raw_args.clone().unwrap();
                        for _ in 0..i {
                            if let Some(a) = get_next_list_elt(arg) {
                                arg = a
                            } else {
                                continue 'tr;
                            }
                        }

                        let return_to = unsafe {
                            apply_start.add(FrameOffset::ArgZero as usize + 1 + i as usize)
                        } as *mut *mut SlHead;

                        self.eval_expr(return_to, env.clone(), arg);
                    }
                }
            }
            Opcode::Bind => {
                let symbol = self.frame_obj(0);
                assert!(symbol.basic_sym_p());
                let value = self.frame_obj(1);

                env_scope_ins(reg, env, symbol.clone(), value);
                self.pop_frame();

                unsafe {
                    let out = symbol.get_raw();
                    ptr::write(ret, out);
                    inc_refc(out);
                }
            }
            Opcode::Mutate => {
                let symbol = self.frame_obj(0);
                assert!(symbol.basic_sym_p());
                let value = self.frame_obj(1);

                if !env_scope_mut(env, symbol.clone(), value) {
                    panic!("symbol not in env")
                }

                self.pop_frame();

                unsafe {
                    let out = symbol.get_raw();
                    ptr::write(ret, out);
                    inc_refc(out);
                }
            }
            Opcode::DoSeq => {
                let remainder = self.frame_obj(0);
                let second = get_next_list_elt(remainder.clone());
                match second {
                    None => {
                        self.pop_frame();
                        self.eval_expr(ret, env, remainder);
                    }
                    Some(sob) => {
                        self.pop();
                        self.push(sob);
                        if remainder.nnil_ref_p() {
                            self.push_frame_head(
                                self.stack_max as *mut *mut SlHead,
                                Opcode::Eval,
                                env,
                            );
                            self.push(ref_get(remainder).unwrap());
                        }
                    }
                }
            }
            Opcode::While => {
                let pred = self.frame_obj(0);
                let result = self.frame_obj(1);
                let body = self.frame_obj(2);

                if result.truthy() {
                    let return_to = self.frame_addr(1);
                    self.eval_expr(return_to, env.clone(), pred);

                    self.push_frame_head(self.stack_max as *mut *mut SlHead, Opcode::DoSeq, env);
                    self.push(body);
                } else {
                    self.pop_frame();
                    unsafe {
                        let out = result.get_raw();
                        ptr::write(ret, out);
                        inc_refc(out);
                    }
                }
            }
            Opcode::Branch => {
                let pred_res = self.frame_obj(0);
                let true_body = self.frame_obj(1);
                let false_body = self.frame_obj(2);
                self.pop_frame();

                if pred_res.truthy() {
                    self.eval_expr(ret, env, true_body);
                } else {
                    self.eval_expr(ret, env, false_body);
                }
            }
            Opcode::PreApp => {
                let proc = self.frame_obj(0);
                assert!(proc.proc_p());
                let raw_args = self.frame_obj(1);
                self.pop_frame();

                self.push_frame_head(ret, Opcode::Apply, env.clone());

                self.push(proc.clone());
                for _ in 0..proc_get_argct(proc.clone()) {
                    self.push_slot();
                }
                let apply_start = self.frame_start;

                let mut arg = raw_args;
                for i in 0..proc_get_argct(proc.clone()) {
                    let return_to =
                        unsafe { apply_start.add(FrameOffset::ArgZero as usize + 1 + i as usize) }
                            as *mut *mut SlHead;

                    self.eval_expr(return_to, env.clone(), arg.clone());

                    arg = get_next_list_elt(arg).unwrap();
                }
            }
            Opcode::Apply => {
                let proc = self.frame_obj(0);
                let typ = match proc.core_type() {
                    Some(t) if t == CoreType::ProcLambda => true,
                    Some(t) if t == CoreType::ProcNative => false,
                    _ => panic!("not a proc"),
                };

                let argct = proc_get_argct(proc.clone());

                if typ {
                    let proc_env = env_create(reg, Some(env));

                    for i in 0..argct {
                        env_scope_ins_by_id(
                            reg,
                            proc_env.clone(),
                            proc_lambda_get_arg_id(proc.clone(), i),
                            self.frame_obj(i as usize + 1),
                        );
                    }

                    self.pop_frame();

                    self.push_frame_head(ret, Opcode::DoSeq, proc_env);
                    self.push(proc_lambda_get_body(proc).unwrap());
                } else {
                    let args: &[SlHndl] = unsafe {
                        std::slice::from_raw_parts(self.frame_addr(1) as _, argct as usize)
                    };

                    let fn_rslt = proc_native_get_body(proc)(reg, tbl, env, args);

                    unsafe {
                        let out = fn_rslt.get_raw();
                        ptr::write(ret, out);
                        inc_refc(out);
                    }

                    self.pop_frame();
                }
            }
        }

        true
    }
}

impl Drop for EvalStack {
    fn drop(&mut self) {
        unsafe {
            let layout = alloc::Layout::from_size_align_unchecked(
                (self.stack_max as usize - self.stack_start as usize) + 8,
                8,
            );
            alloc::dealloc(self.stack_start as *mut u8, layout);
        }
    }
}

/// Evaluates a Sail expression in a freshly created stack
pub fn eval(reg: *mut memmgt::Region, tbl: SlHndl, env: SlHndl, expr: SlHndl) -> SlHndl {
    let mut result: *mut SlHead = ptr::null_mut();
    let ret_addr: *mut *mut SlHead = &mut result as *mut *mut SlHead;

    let mut stack = EvalStack::new(10000);

    stack.start(ret_addr, env, expr);

    while stack.iter_once(reg, tbl.clone()) {}

    inc_refc(result);
    unsafe { SlHndl::from_raw_unchecked(result) }
}

enum_and_tryfrom! {
    /// Operation code for the frame; stored as a tag at the frame start
    #[derive(Debug, PartialEq, Eq)]
    #[repr(u8)]
    pub enum Opcode {
        /// Expression to be evaluated
        PreEval,

        /// List to be evaluated
        Eval,

        /// Symbol, object
        Bind,

        /// Symbol, object
        Mutate,

        /// Remainder of list to do
        DoSeq,

        /// Predicate, result, loop body
        While,

        /// Predicate result, true path, false path
        Branch,

        /// Procedure, argument list
        PreApp,

        /// Procedure, all arguments
        Apply,

        // /// Function to run with error
        // Catch,

        // /// Function to run, error caught
        // Caught,
    }
}

/// Contents of a frame head in this Sail evaluation stack
struct _Frame {
    /// Pointer to the frame immediately before this one on the stack
    last_top_frame: usize,

    /// Address to which to return the value this frame produces
    return_address: usize,

    /// Sail environment and opcode for this frame (tagged pointer)
    env_and_opcode: usize,
}

/// Offsets into an evaluation stack frame (header included)
#[repr(u8)]
enum FrameOffset {
    /// Top of the last frame
    LastTop = 0,
    /// Return address
    Return = 1,
    /// Environment and opcode (tagged pointer)
    EnvOpc = 2,
    /// First body word
    ArgZero = 3,
}

// TODO: call lambda functions using the stack?
// TODO: **Macros**, closures, continuations
// TODO: special forms may be examples for creating / using native functions cleanly
// TODO: just like native functions, special forms should check for type
// TODO: match the argument structure to the number of arguments needed
// TODO: tail call optimization

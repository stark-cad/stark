// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/eval.rs

// Iterative stack evaluator for Sail. Defines a stack format, utility
// functions, and logic to evaluate any valid Sail expression one
// stack frame at a time.

// <>

use super::core::*;
use super::debug;
use super::memmgt;
use super::thread;

use super::Hdl;
use super::{SP_COND, SP_DEF, SP_DO, SP_EVAL, SP_FN, SP_IF, SP_QUOTE, SP_SET, SP_WHILE};

use std::alloc;
use std::ptr;

enum_and_tryfrom! {
    /// Operation code for the frame; stored as a tag at the frame start
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

        /// Remaining predicate / consequent pairs
        CondSeq,

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

        /// Env (binding dictionary)
        Scope,

        // /// Function to run with error
        // Catch,

        // /// Function to run, error caught
        // Caught,
    }
}

// TODO: ApplyTail and Module opcodes?

const OPC_CT: u8 = std::mem::variant_count::<Opcode>() as u8;

/// Contents of a Sail evaluation stack frame head
#[repr(C)]
struct FrameHead {
    /// Determines how this frame will be retired
    opcode: Opcode,
    /// Length of the previous frame on the stack
    prev_len: u8,
    /// Full length of the current frame
    this_len: u8,

    // TODO: save four bytes of space to store the symbol which
    // referred to the module or procedure represented here?
    // unused: u32,
    /// Return address for the value this frame produces
    return_address: *mut *mut SlHead,
}

// Scope frames hold a reference to the last scope pointer (in the
// prior Scope frame) in their return address slot. They hold the
// scope pointer itself in the first body slot.

/// Offsets into an evaluation stack frame (header included)
#[repr(u8)]
enum FrameOffset {
    /// Header with opcode, size, etc
    _Meta = 0,
    /// Return address
    Ret = 1,
    /// First body word
    Arg0 = 2,
}


/// Sail evaluation stack
pub struct EvalStack {
    /// First element of the operation frame stack
    start: *mut usize,
    /// Start of the stack's top frame
    current_frame: *mut usize,
    /// Topmost scope pointer
    current_scope: *mut usize,
    /// Space allocated for the evaluation stack
    stack_ext: u32,
}

impl EvalStack {
    /// Create and initialize a new Sail stack and associated memory
    pub fn new(size: u16) -> Self {
        let size_bytes = size as usize * 8;

        unsafe {
            let frm_stk_layout = alloc::Layout::from_size_align_unchecked(size_bytes, PTR_LEN as _);
            let frm_stk_ptr = alloc::alloc(frm_stk_layout);

            // TODO: frame cap and frame len in words?
            EvalStack {
                start: frm_stk_ptr as *mut usize,
                current_frame: frm_stk_ptr as *mut usize,
                current_scope: ptr::null_mut(),
                stack_ext: size_bytes as _,
            }
        }
    }

    fn check_invariants(self: *const Self) -> bool {
        unsafe {
            if self.is_empty() {
                assert_eq!((*self).current_frame, (*self).start);
                assert_eq!(self.frame_end(), (*self).start);
            } else {
                assert!(self.frame_end() > (*self).start);
                assert!((*self).current_scope > (*self).start);

                let mut prev_frm_len = 0;
                let mut curr_frm_ptr = (*self).start as *mut FrameHead;
                let mut prev_frm_ptr;

                loop {
                    let opcode_discrim = *(curr_frm_ptr as *const u8);

                    assert!(opcode_discrim < OPC_CT);
                    assert_eq!((*curr_frm_ptr).prev_len, prev_frm_len);
                    assert!((*curr_frm_ptr).this_len >= 2);
                    // assert!(curr_frm_ref.unused == 0);

                    prev_frm_len = (*curr_frm_ptr).this_len;

                    if opcode_discrim == Opcode::Scope as u8 {
                        // TODO: check scope invariants or improve stack again
                    }

                    prev_frm_ptr = curr_frm_ptr;
                    curr_frm_ptr = prev_frm_ptr.byte_add(prev_frm_len as usize * 8);

                    if curr_frm_ptr as usize >= self.frame_end() as usize {
                        assert_eq!(curr_frm_ptr as usize, self.frame_end() as usize);
                        break;
                    }

                    if curr_frm_ptr as usize > (*self).start as usize {
                        assert_eq!((*prev_frm_ptr).this_len, (*curr_frm_ptr).prev_len);
                    }
                }

                assert!(
                    (*self).stack_ext as usize > self.frame_end() as usize - (*self).start as usize
                );
            }
        }

        true
    }

    /// Pushes this env onto the scope stack
    pub fn scope_push(self: *mut Self, env: SlHndl, call_p: bool) {
        unsafe {
            self.frame_push_head((*self).current_scope as _, Opcode::Scope);
            (*self).current_scope = self.frame_addr(0) as *mut usize;
            self.ref_push(env);
        }

        // println!("Pushed scope {:x}", unsafe { env.get_raw() as usize });
    }

    fn scope_cur(self: *const Self) -> SlHndl {
        let env =
            unsafe { SlHndl::from_raw(*((*self).current_scope as *mut *mut SlHead)).unwrap() };
        // println!("Current scope {:x}", unsafe { env.get_raw() as usize });
        env
    }

    // /// Returns handle to nth environment scope from top
    // fn scope_idx(self: *const Self, n: u16) -> SlHndl {
    //     assert!(n < self.scope_len);

    //     unsafe {
    //         let slot =
    //             self.scope_start.add((self.scope_len - (n + 1)) as usize) as *mut *mut SlHead;
    //         SlHndl::from_raw(ptr::read(slot)).unwrap()
    //     }
    // }

    /// Returns length of stack area, in words
    pub fn size(self: *const Self) -> u16 {
        let bytes = unsafe { (*self).stack_ext };
        debug_assert_eq!(bytes % 8, 0);
        (bytes / 8) as _
    }

    /// Resize the stack, acquiring new memory if necessary
    fn resize(self: *mut Self, size: u16) {
        let size_bytes = size as usize * 8;
        let old_sb = unsafe { (*self).stack_ext as usize };
        if old_sb > size_bytes {
            return;
        }

        println!("stack resized");

        unsafe {
            let old_start = (*self).start as usize;
            let frame_offset = (*self).current_frame as usize - old_start;

            let (new_start, new_frame) = {
                let old_layout = alloc::Layout::from_size_align_unchecked(old_sb, 8);
                let start = alloc::realloc((*self).start as *mut u8, old_layout, size_bytes);
                (start as *mut usize, start.add(frame_offset) as *mut usize)
            };

            // use length of top frame
            let top_offset = frame_offset + (*self.frame_idx(0)).this_len as usize;

            if new_start as usize != old_start {
                // iterate over the stack, updating those pointers that
                // point elsewhere within the stack to conform with the
                // new memory location
                for p in 0..=(top_offset / 8) {
                    let address = new_start.add(p);
                    let pointer = ptr::read(address);

                    if pointer >= old_start && pointer <= (old_start + top_offset) {
                        let addr_offset = pointer - old_start;
                        ptr::write(address, new_start as usize + addr_offset);
                    }
                }
            }

            (*self).start = new_start;
            (*self).current_frame = new_frame;
        }
    }

    fn frame_end(self: *const Self) -> *mut usize {
        unsafe {
            if self.is_empty() {
                return (*self).start;
            }

            (*self)
                .current_frame
                .add((*((*self).current_frame as *mut FrameHead)).this_len as usize)
        }
    }

    fn frame_max(self: *const Self) -> *mut usize {
        unsafe { (*self).start.byte_add((*self).stack_ext as usize) }
    }

    fn frame_idx(self: *mut Self, i: i16) -> *mut FrameHead {
        assert!(!self.is_empty());

        unsafe {
            if i >= 0 {
                let mut site = (*self).current_frame;

                for _ in 0..i {
                    assert!(site >= (*self).start);
                    assert!(site < self.frame_max());
                    site = site.sub((*(site as *mut FrameHead)).prev_len as usize)
                }

                (site as *mut FrameHead).as_mut().unwrap()
            } else {
                let site = (*self).start;
                // TODO: negative indices
                (site as *mut FrameHead).as_mut().unwrap()
            }
        }
    }

    /// Pushes a single word to the stack, which will always be a
    /// pointer to a Sail object
    #[inline(always)]
    pub fn ref_push(self: *mut Self, word: SlHndl) {
        if cfg!(feature = "stkdbg") {
            print!("push 1; ");
        }

        unsafe {
            if self.frame_end().add(1) > (*self).start.byte_add((*self).stack_ext as usize) {
                self.resize(((*self).stack_ext / 4) as u16);
            }

            let wordr = word.get_raw();
            inc_refc(wordr);
            ptr::write(self.frame_end() as *mut _, wordr);

            (*self.frame_idx(0)).this_len += 1;

            if cfg!(feature = "stkdbg") {
                println!(
                    "size: {}",
                    (self.frame_end() as usize - (*self).start as usize) / 8
                );
            }
        }
    }

    fn ref_push_slot(self: *mut Self) {
        unsafe {
            let top_fr_len = (*self.frame_idx(0)).this_len as usize;

            if (*self).current_frame as usize + top_fr_len + 1
                > (*self).start as usize + (*self).stack_ext as usize
            {
                self.resize(((*self).stack_ext / 4) as u16);
            }

            (*self.frame_idx(0)).this_len += 1;
        }
    }

    /// Pops a single word off the stack
    #[inline(always)]
    fn ref_pop(self: *mut Self) {
        if cfg!(feature = "stkdbg") {
            print!("pop 1; ");
        }

        unsafe {
            let doomed_slot = self.frame_end().sub(1);

            let word = *doomed_slot as *mut SlHead;

            if !nil_p(word) && dec_refc(word) {
                // TODO: requires scope stack traversal?
                destroy_obj(self.scope_cur(), word)
            }

            (*self.frame_idx(0)).this_len -= 1;

            if cfg!(feature = "stkdbg") {
                println!(
                    "size: {}",
                    (self.frame_end() as usize - (*self).start as usize) / 8
                );
            }
        }
    }

    /// Push a complete frame head onto the stack
    ///
    /// Components:
    /// - `ret`: return address for this frame's result
    /// - `opc`: opcode which dictates this frame's format & behavior
    /// - `env`: environment containing bindings for this frame
    #[inline(always)]
    pub fn frame_push_head(self: *mut Self, mut ret: *mut *mut SlHead, opc: Opcode) {
        if cfg!(feature = "stkdbg") {
            print!("PUSH: {:?}; ", opc);
        }

        let firstp = self.is_empty();

        unsafe {
            if self.frame_end().add(2) > self.frame_max() {
                let old_start = (*self).start as usize;
                self.resize(((*self).stack_ext / 4) as _);
                if !ret.is_null() {
                    ret = ((ret as usize - old_start) + (*self).start as usize) as *mut *mut SlHead;
                }
            }

            let new_frame_start = self.frame_end();

            let new_head = FrameHead {
                opcode: opc,
                prev_len: if firstp {
                    0
                } else {
                    (*self.frame_idx(0)).this_len
                },
                this_len: 2,
                // unused: 0,
                return_address: ret,
            };

            ptr::write(new_frame_start as *mut FrameHead, new_head);

            (*self).current_frame = new_frame_start;

            if cfg!(feature = "stkdbg") {
                println!(
                    "size: {}",
                    (self.frame_end() as usize - (*self).start as usize) / 8
                );
            }
        }
    }

    fn frame_fold(self: *mut Self, opc: Opcode) {
        if cfg!(feature = "stkdbg") {
            print!("PFOLD: {:?}; ", opc);
        }

        // TODO: not permitted for Scope

        let lenv = self.scope_cur();

        // drop references within old frame
        unsafe {
            // the frame is entirely between these addresses
            let our_frame = (*self).current_frame;
            let our_frame_old_end = self.frame_end();

            // decrement reference count of all Sail objects which the
            // frame points to
            let old_data_start = our_frame.add(FrameOffset::Arg0 as usize);
            for p in std::slice::from_raw_parts_mut(
                old_data_start as *mut *mut SlHead,
                (our_frame_old_end as usize - old_data_start as usize) / 8,
            ) {
                let o = *p;
                if !nil_p(o) && dec_refc(o) {
                    destroy_obj(lenv.clone(), o);
                }
            }

            // rewrite frame head
            let our_frm_ref = self.frame_idx(0);
            (*our_frm_ref).opcode = opc;
            (*our_frm_ref).this_len = 2;

            if cfg!(feature = "stkdbg") {
                println!(
                    "size: {}",
                    (self.frame_end() as usize - (*self).start as usize) / 8
                );
            }
        }
    }

    /// Pop a full frame off of the stack
    #[inline(always)]
    fn frame_pop(self: *mut Self) {
        if cfg!(feature = "stkdbg") {
            print!("POP: {:?}; ", self.frame_opc());
        }

        unsafe {
            // the frame is entirely between these addresses
            let doomed_frame = (*self).current_frame;
            let doomed_frame_end = self.frame_end();

            let prev_frame = doomed_frame.sub((*(doomed_frame as *mut FrameHead)).prev_len as _);
            let is_scope = (*(doomed_frame as *mut FrameHead)).opcode == Opcode::Scope;

            // decrement reference count of all Sail objects which the
            // frame points to
            let frm_data_start = doomed_frame.add(FrameOffset::Arg0 as usize);
            if is_scope {
                if !(*(frm_data_start as *mut *mut SlHead)).is_null() {
                    dec_refc(*(frm_data_start as *mut _));
                }
            } else {
                for p in std::slice::from_raw_parts_mut(
                    frm_data_start as *mut *mut SlHead,
                    (doomed_frame_end as usize - frm_data_start as usize) / 8,
                ) {
                    // TODO: allow for destruction of objects in outer scopes!
                    let o = *p;
                    if !nil_p(o) && dec_refc(o) {
                        let env = self.scope_cur();
                        destroy_obj(env.clone(), o);
                    }
                }
            }

            if is_scope {
                (*self).current_scope = (*(doomed_frame as *mut FrameHead)).return_address as _;
            }

            (*self).current_frame = prev_frame;

            if cfg!(feature = "stkdbg") {
                println!(
                    "size: {}",
                    (self.frame_end() as usize - (*self).start as usize) / 8
                );
            }
        }
    }

    // TODO: use more of a condition system than an exception system eventually
    fn unwind(self: *mut Self, error: SlHndl) {
        // destroy stack frames until reaching an error catch or the bottom
        while unsafe { (*self).current_frame > (*self).start } {
            self.frame_pop();
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
    pub fn start(self: *mut Self, ret: *mut *mut SlHead, env: SlHndl, expr: SlHndl) -> bool {
        if !self.is_empty() {
            false
        } else {
            self.scope_push(env, false);
            self.import_module(M_BASE.0);
            // TODO: survive when expression is unbound symbol
            self.eval_expr(ret, expr).unwrap();
            true
        }
    }

    pub fn start_proc(
        self: *mut Self,
        ret: *mut *mut SlHead,
        env: SlHndl,
        proc: SlHndl,
        args: &[SlHndl],
    ) -> bool {
        if !self.is_empty() {
            false
        } else {
            self.scope_push(env, false);
            self.frame_push_head(ret, Opcode::Apply);
            self.ref_push(proc);
            for a in args {
                self.ref_push(a.clone());
            }
            true
        }
    }

    /// Determines whether the stack is currently empty
    #[inline(always)]
    pub fn is_empty(self: *const Self) -> bool {
        unsafe { (*self).current_scope.is_null() }
    }

    /// Gets the return address of the current top frame
    #[inline(always)]
    fn frame_ret(self: *mut Self) -> *mut *mut SlHead {
        unsafe {
            ptr::read((*self).current_frame.add(FrameOffset::Ret as usize) as *mut *mut *mut SlHead)
        }
    }

    /// Gets the opcode of the current top frame
    #[inline(always)]
    fn frame_opc(self: *mut Self) -> Opcode {
        unsafe { (*self.frame_idx(0)).opcode }
    }

    // /// Returns all frame head components of the current top frame
    // #[inline(always)]
    // fn frame_top(self: *mut Self) -> FrameHead {
    //     unsafe { ptr::read(self.frame_start as *mut _) }
    // }

    /// Returns the address of a location offset into the current top
    /// frame
    #[inline(always)]
    fn frame_addr(self: *mut Self, offset: usize) -> *mut *mut SlHead {
        unsafe {
            (*self)
                .current_frame
                .add(FrameOffset::Arg0 as usize + offset) as *mut *mut SlHead
        }
    }

    /// Returns the pointer stored in a location offset into the
    /// current top frame
    #[inline(always)]
    fn frame_obj(self: *mut Self, offset: usize) -> SlHndl {
        unsafe {
            let obj = SlHndl::from_raw_unchecked(ptr::read(self.frame_addr(offset)) as *mut SlHead);
            inc_refc(obj.get_raw());
            obj
        }
    }

    /// Returns the object bound to the given symbol if binding exists
    /// in the outer env -> the current stack of env scopes
    fn scope_search(self: *const Self, mut query: SlHndl) -> Option<SlHndl> {
        let mut scope = unsafe { (*self).current_scope as *mut *mut SlHead };
        assert!(!scope.is_null());

        let (path_p, sym) = if coretypp!(query ; Symbol) {
            (false, sym_get_id(query.clone()))
        } else if coretypp!(query ; VecPath) {
            (true, sympath_idx(query.clone(), 0))
        } else {
            panic!()
        };

        let mut result = None;

        while !scope.is_null() {
            // println!("Searching scope {:x}", unsafe { *scope as usize });
            let env = unsafe { SlHndl::from_raw(*scope).unwrap() };
            match env_lookup_by_id(env, sym) {
                Some(s) => result = Some(s),
                _ => (),
            }

            scope = unsafe { ptr::read(scope.sub(1) as _) };
        }

        if path_p && result.is_some() {
            for i in 1..sympath_get_len(query.clone()) {
                match env_lookup_by_id(result.clone().unwrap(), sympath_idx(query.clone(), i)) {
                    Some(s) => result = Some(s),
                    _ => break,
                }
            }
        }

        result
    }

    // no mutation of modules?
    fn bind_local(
        self: *const Self,
        reg: *mut memmgt::Region,
        name: SlHndl,
        value: SlHndl,
    ) -> bool {
        let mut scope = unsafe { (*self).current_scope as *mut *mut SlHead };
        assert!(!scope.is_null());

        coretypck!(name.clone() ; Symbol);
        unsafe {
            while (*(scope.sub(2) as *mut FrameHead)).mod_p {
                scope = ptr::read(scope.sub(1) as _);
            }
        }

        let env = unsafe { SlHndl::from_raw(*scope).unwrap() };
        env_ins_by_id(reg, env, sym_get_id(name), value)
    }

    fn mutate_binding(self: *const Self, name: SlHndl, value: SlHndl) -> bool {
        let mut scope = unsafe { (*self).current_scope as *mut *mut SlHead };
        assert!(!scope.is_null());

        coretypck!(name.clone() ; Symbol);
        while !{
            let env = unsafe { SlHndl::from_raw(*scope).unwrap() };
            env_mut_entry_by_id(env, sym_get_id(name.clone()), value.clone())
        } {
            unsafe {
                while (*(scope.sub(2) as *mut FrameHead)).mod_p {
                    scope = ptr::read(scope.sub(1) as _);
                    if scope.is_null() {
                        return false;
                    }
                }
            }
        }

        true
    }

    /// Evaluates any Sail expression in object form, using the given
    /// environment and returning to the given location
    #[inline(always)]
    fn eval_expr(self: *mut Self, ret: *mut *mut SlHead, mut expr: SlHndl) -> Result<(), u32> {
        if expr.nnil_ref_p() {
            self.frame_push_head(ret, Opcode::Eval);
            self.ref_push(ref_get(expr).unwrap());
            return Ok(());
        }

        if ret.is_null() {
            return Ok(());
        }

        // look back through stack
        let out = if expr.resolv_p() {
            match self.scope_search(expr.clone()) {
                Some(obj) => unsafe { obj.get_raw() },
                None => return Err(sym_get_id(expr)),
            }
        } else {
            unsafe { expr.get_raw() }
        };

        if !nil_p(out) {
            inc_refc(out);
        }

        unsafe { ptr::write(ret, out) };

        Ok(())
    }

    /// Evaluates expression on stack, folding up a frame
    fn eval_expr_fold(self: *mut Self, mut expr: SlHndl) -> Result<(), u32> {
        if expr.nnil_ref_p() {
            self.frame_fold(Opcode::Eval);
            self.ref_push(ref_get(expr).unwrap());
            return Ok(());
        }

        let ret = self.frame_ret();
        if ret.is_null() {
            self.frame_pop();
            return Ok(());
        }

        // look at supplied environment, then back through stack
        let out = if expr.resolv_p() {
            match self.scope_search(expr.clone()) {
                Some(obj) => unsafe { obj.get_raw() },
                None => return Err(sym_get_id(expr)),
            }
        } else {
            unsafe { expr.get_raw() }
        };

        if !nil_p(out) {
            inc_refc(out);
        }

        unsafe { ptr::write(ret, out) };

        self.frame_pop();

        Ok(())
    }

    /// Writes the address of a Sail object to an aligned position;
    /// use for returning results
    #[inline(always)]
    fn write_addr_to(self: *mut Self, pos: *mut *mut SlHead, out: SlHndl) {
        if pos.is_null() {
            return;
        }

        unsafe {
            let out = out.get_raw();
            inc_refc(out);
            ptr::write(pos, out);
        }
    }

    /// Consumes one frame off the top of the stack and executes it
    ///
    /// This is the core of Sail evaluation logic. In the limit,
    /// executing this function repeatedly will evaluate any Sail
    /// expression. It handles all defined opcodes and adds more
    /// frames to the stack as necessary, and never uses recursion.
    pub fn iter_once(self: *mut Self, vessel_ref: Hdl<thread::Vessel>) -> bool {
        // ***********************************
        // * Sail stack-based evaluation logic
        // ***********************************
        debug_assert!(self.check_invariants());

        if self.is_empty() {
            return false;
        }

        // debug::env_print(
        //     unsafe { (*vessel_ref).context().symtab() },
        //     self.scope_cur(),
        // );

        let reg = vessel_ref.region();


        let (ret, opc) = (self.frame_ret(), self.frame_opc());

        if cfg!(feature = "stkdbg") {
            println!("ENTER: {:?}", opc);
            self.readout();
        }

        // NOTE: no matter what, one frame must go
        match opc {
            Opcode::PreEval => {
                let expr = self.frame_obj(0);
                self.eval_expr_fold(expr).unwrap();
            }
            Opcode::Eval => {
                let list = self.frame_obj(0);

                let mut raw_op = list;
                let raw_args = get_next_list_elt(raw_op.clone());

                // if raw_op.resolv_p() {
                if coretypp!(raw_op ; Symbol) {
                    match sym_get_id(raw_op.clone()) {
                        id if id == SP_COND.0 => {
                            self.frame_fold(Opcode::CondSeq);
                            self.ref_push_slot();

                            let predicate = raw_args.expect("cond called without arguments");
                            self.ref_push(
                                get_next_list_elt(predicate.clone())
                                    .expect("cond must have an even number of arguments"),
                            );

                            let pred_ret = self.frame_addr(0);
                            self.eval_expr(pred_ret, predicate).unwrap();
                            return true;
                        }
                        // TODO: fix insane scope / shadowing behavior
                        id if id == SP_DEF.0 => {
                            // needs: symbol to bind, object to bind to it
                            self.frame_fold(Opcode::Bind);
                            self.ref_push(raw_args.clone().unwrap());
                            self.ref_push_slot();

                            let value = get_next_list_elt(raw_args.unwrap()).unwrap();
                            let return_to = self.frame_addr(1);

                            self.eval_expr(return_to, value).unwrap();
                            return true;
                        }
                        id if id == SP_DO.0 => {
                            // needs: current remaining list of expressions
                            self.frame_fold(Opcode::DoSeq);
                            self.ref_push(raw_args.unwrap());
                            return true;
                        }
                        id if id == SP_EVAL.0 => {
                            self.frame_fold(Opcode::PreEval);
                            self.ref_push_slot();

                            let return_to = self.frame_addr(0);

                            match self.eval_expr(return_to, raw_args.unwrap()) {
                                Ok(_) => (),
                                Err(sym_id) => {
                                    println!(
                                        "SYMBOL: {}",
                                        String::from_utf8_lossy(
                                            vessel_ref
                                                .context()
                                                .symtab()
                                                .lookup_by_id(sym_id)
                                                .unwrap()
                                        )
                                    );
                                    panic!("symbol not bound in env")
                                }
                            }

                            return true;
                        }
                        id if id == SP_FN.0 => {
                            // needs: nothing else evaluated
                            self.frame_pop();

                            // TODO: type annotation syntax / handling
                            let argvec = raw_args.unwrap();
                            let argct = stdvec_get_len(argvec.clone()) as u16;
                            let proc = proc_lambda_make(reg, argct);

                            if cfg!(feature = "memdbg") {
                                let mid = raw_op.memdbg_obj_id();
                                let res = vessel_ref
                                    .src_dbg_info()
                                    .expect("we're debugging here")
                                    .lookup(mid);

                                let pid = unsafe { proc.memdbg_obj_id_immed() };

                                if let Some((src, ln)) = res {
                                    match src {
                                        debug::Source::File(pat) => println!(
                                            "S proc id {} created from code in {}, line {}",
                                            pid,
                                            pat.display(),
                                            ln
                                        ),
                                        _ => (),
                                    }
                                }
                            }

                            for i in 0..argct {
                                proc_lambda_set_arg(
                                    proc.clone(),
                                    i,
                                    sym_get_id(stdvec_idx(argvec.clone(), i as u32)),
                                );
                            }
                            proc_lambda_set_body(proc.clone(), get_next_list_elt(argvec).unwrap());

                            self.write_addr_to(ret, proc);

                            return true;
                        }
                        id if id == SP_IF.0 => {
                            // needs: evaluated test and both branches
                            self.frame_fold(Opcode::Branch);
                            self.ref_push_slot();
                            self.ref_push(get_next_list_elt(raw_args.clone().unwrap()).unwrap());
                            self.ref_push(
                                get_next_list_elt(
                                    get_next_list_elt(raw_args.clone().unwrap()).unwrap(),
                                )
                                .unwrap(),
                            );

                            // TODO: accept if with only one consequent (assume nil following)

                            let return_to = self.frame_addr(0);

                            self.eval_expr(return_to, raw_args.unwrap()).unwrap();
                            return true;
                        }
                        id if id == SP_QUOTE.0 => {
                            // needs: nothing else evaluated
                            self.frame_pop();
                            self.write_addr_to(ret, raw_args.unwrap());

                            return true;
                        }
                        id if id == SP_SET.0 => {
                            // needs: symbol to bind, object to bind to it
                            self.frame_fold(Opcode::Mutate);
                            self.ref_push(raw_args.clone().unwrap());
                            self.ref_push_slot();

                            let value = get_next_list_elt(raw_args.unwrap()).unwrap();
                            let return_to = self.frame_addr(1);

                            self.eval_expr(return_to, value).unwrap();
                            return true;
                        }
                        id if id == SP_WHILE.0 => {
                            self.frame_fold(Opcode::While);
                            self.ref_push(raw_args.clone().unwrap());
                            self.ref_push_slot();
                            self.ref_push(get_next_list_elt(raw_args.clone().unwrap()).unwrap());

                            let return_to = self.frame_addr(1);

                            self.eval_expr(return_to, raw_args.unwrap()).unwrap();
                            return true;
                        }
                        _ => {}
                    }
                }

                if raw_op.nnil_ref_p() {
                    self.frame_fold(Opcode::PreApp);
                    self.ref_push_slot();
                    self.ref_push(raw_args.unwrap());

                    let return_to = self.frame_addr(0);

                    self.frame_push_head(return_to, Opcode::Eval);
                    self.ref_push(ref_get(raw_op).unwrap());
                } else {
                    let mut proc = if raw_op.resolv_p() {
                        match self.scope_search(raw_op.clone()) {
                            Some(obj) => obj,
                            None => {
                                println!(
                                    "SYMBOL: {}",
                                    String::from_utf8_lossy(
                                        vessel_ref
                                            .context()
                                            .symtab()
                                            .lookup_by_id(sym_get_id(raw_op.clone()))
                                            .unwrap()
                                    )
                                );
                                panic!("symbol not bound in env")
                            }
                        }
                    } else {
                        raw_op.clone()
                    };
                    if !proc.proc_p() {
                        panic!(
                            "PS: {}",
                            String::from_utf8_lossy(
                                vessel_ref
                                    .context()
                                    .symtab()
                                    .lookup_by_id(sym_get_id(raw_op.clone()))
                                    .unwrap()
                            )
                        );
                    }
                    assert!(proc.proc_p());

                    let arg_ct = proc_get_argct(proc.clone());

                    // construct application frame; slots must come first!
                    self.frame_fold(Opcode::Apply);


                    self.ref_push(proc);
                    for _ in 0..arg_ct {
                        self.ref_push_slot()
                    }

                    // NOTE: we need to keep this because eval may add more frames!
                    let apply_start = unsafe { (*self).current_frame };

                    let mut arg = raw_args;

                    for i in 0..arg_ct {
                        let return_to =
                            unsafe { apply_start.add(FrameOffset::Arg0 as usize + 1 + i as usize) }
                                as *mut *mut SlHead;

                        // TODO: provide code context with this sort of error message
                        match self.eval_expr(
                            return_to,
                            arg.clone()
                                .unwrap_or_else(|| panic!("arg {i} of {arg_ct} does not exist")),
                        ) {
                            Ok(_) => (),
                            Err(sym_id) => {
                                println!(
                                    "SYMBOL: {}",
                                    String::from_utf8_lossy(
                                        vessel_ref.context().symtab().lookup_by_id(sym_id).unwrap()
                                    )
                                );
                                panic!("symbol not bound in env")
                            }
                        }

                        arg = get_next_list_elt(arg.unwrap())
                    }
                }
            }
            Opcode::Bind => {
                let symbol = self.frame_obj(0);
                let value = self.frame_obj(1);

                if self.bind_local(reg, symbol.clone(), value) {
                    // eprintln!(
                    //     "Replaced local binding for: {}",
                    //     String::from_utf8_lossy(
                    //         vessel_ref
                    //             .context()
                    //             .symtab()
                    //             .lookup_by_id(sym_get_id(symbol.clone()))
                    //             .unwrap()
                    //     )
                    // )
                }

                self.write_addr_to(ret, symbol);
                self.frame_pop();
            }
            Opcode::Mutate => {
                let symbol = self.frame_obj(0);
                let value = self.frame_obj(1);

                if !self.mutate_binding(symbol.clone(), value) {
                    println!(
                        "SYMBOL: {}",
                        String::from_utf8_lossy(
                            vessel_ref
                                .context()
                                .symtab()
                                .lookup_by_id(sym_get_id(symbol.clone()))
                                .unwrap()
                        )
                    );
                    panic!("symbol not in env")
                }

                self.write_addr_to(ret, symbol);
                self.frame_pop();
            }
            Opcode::DoSeq => {
                let mut remainder = self.frame_obj(0);
                let second = get_next_list_elt(remainder.clone());

                if let Some(sob) = second {
                    self.ref_pop();
                    self.ref_push(sob);
                    if remainder.nnil_ref_p() {
                        self.frame_push_head(ptr::null_mut() as _, Opcode::Eval);
                        self.ref_push(ref_get(remainder).unwrap());
                    }
                } else {
                    self.eval_expr_fold(remainder).unwrap();
                }
            }
            Opcode::While => {
                let pred = self.frame_obj(0);
                let mut result = self.frame_obj(1);
                let body = self.frame_obj(2);

                if result.truthy() {
                    let return_to = self.frame_addr(1);
                    self.frame_push_head(return_to, Opcode::PreEval);
                    self.ref_push(pred);

                    // TODO: this sort of thing should be a (Rust) macro!
                    unsafe {
                        if dec_refc(result.get_raw()) {
                            destroy_obj(self.scope_cur(), result.get_raw())
                        }
                    };

                    self.frame_push_head(ptr::null_mut() as _, Opcode::DoSeq);
                    self.ref_push(body);
                } else {
                    self.write_addr_to(ret, result);
                    self.frame_pop();
                }
            }
            Opcode::CondSeq => {
                let mut pred_res = self.frame_obj(0);
                let consequent = self.frame_obj(1);

                if pred_res.truthy() {
                    self.eval_expr_fold(consequent).unwrap();
                } else if let Some(next_pred) = get_next_list_elt(consequent) {
                    let next_cons = get_next_list_elt(next_pred.clone())
                        .expect("cond must have an even number of arguments");

                    let pred_ret = self.frame_addr(0);
                    let cons_keep = self.frame_addr(1);

                    self.write_addr_to(cons_keep, next_cons);
                    self.eval_expr(pred_ret, next_pred).unwrap();
                } else {
                    self.write_addr_to(ret, pred_res);
                    self.frame_pop();
                }
            }
            Opcode::Branch => {
                let mut pred_res = self.frame_obj(0);
                let true_body = self.frame_obj(1);
                let false_body = self.frame_obj(2);

                if pred_res.truthy() {
                    self.eval_expr_fold(true_body).unwrap();
                } else {
                    self.eval_expr_fold(false_body).unwrap();
                }
            }
            Opcode::PreApp => {
                let mut proc = self.frame_obj(0);
                assert!(proc.proc_p());
                let raw_args = self.frame_obj(1);

                // construct application frame; slots must come first!
                self.frame_fold(Opcode::Apply);
                self.ref_push(proc.clone());
                for _ in 0..proc_get_argct(proc.clone()) {
                    self.ref_push_slot()
                }

                // NOTE: we need to keep this because eval may add more frames!
                let apply_start = unsafe { (*self).current_frame };

                // NOTE: assume no particular argument evaluation order!
                let mut arg = Some(raw_args);
                for i in 0..proc_get_argct(proc) {
                    let return_to =
                        unsafe { apply_start.add(FrameOffset::Arg0 as usize + 1 + i as usize) }
                            as *mut *mut SlHead;

                    self.eval_expr(return_to, arg.clone().unwrap()).unwrap();

                    arg = get_next_list_elt(arg.unwrap());
                }
            }
            Opcode::Apply => {
                let mut proc = self.frame_obj(0);
                let argct = proc_get_argct(proc.clone());

                match proc.core_type() {
                    Some(CoreType::ProcLambda) => {
                        // TODO: restrict arguments while allowing access
                        // to outer env; use parameter alist?

                        let proc_env = env_make(reg);

                        // TODO: have inner frames write directly to the env?
                        for i in 0..argct {
                            env_ins_by_id(
                                reg,
                                proc_env.clone(),
                                proc_lambda_get_arg_id(proc.clone(), i),
                                self.frame_obj(i as usize + 1),
                            );
                        }


                        self.frame_pop();
                        self.scope_push(proc_env, true);
                        // self.frame_fold(Opcode::Scope, Some(proc_env));
                        self.frame_push_head(ret, Opcode::DoSeq);
                        self.ref_push(proc_lambda_get_body(proc).unwrap());
                    }
                    Some(CoreType::ProcNative) => {
                        let args: &[SlHndl] = unsafe {
                            std::slice::from_raw_parts(self.frame_addr(1) as _, argct as usize)
                        };

                        // TODO: allow native procedures to access scope stack?
                        let fn_rslt =
                            proc_native_get_body(proc)(vessel_ref, self.scope_cur(), args);

                        self.write_addr_to(ret, fn_rslt);

                        self.frame_pop();
                    }
                    _ => panic!("not a proc"),
                }
            }
            Opcode::Scope => {
                // TODO: scope-specific pop code here
                self.frame_pop();
            }
        }

        true
    }

    fn readout(self: *mut Self) {
        println!("\n<>");
        // println!("Scope pos: {:x}", self.frame_scope as usize);

        println!("TOP");

        let mut cur_frame = 0;

        loop {
            unsafe {
                println!("Opcode: {:?}", (*self.frame_idx(cur_frame)).opcode);
                println!("this len: {}", (*self.frame_idx(cur_frame)).this_len);
                println!("prev len: {}", (*self.frame_idx(cur_frame)).prev_len);
                // println!(
                //     "Ret: {:x}",
                //     self.frame_idx(cur_frame).return_address as usize
                // );
                // println!("Arg0: {:x}", unsafe {
                //     *(self.frame_idx(cur_frame) as *mut FrameHead as *mut usize)
                //         .add(FrameOffset::Arg0 as usize)
                // });

                if (*self.frame_idx(cur_frame)).prev_len > 0 {
                    cur_frame += 1;
                    println!("----------");
                    println!("FRAME {cur_frame}");
                } else {
                    break;
                }
            }
        }

        println!();
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rec_check() {
        let (_weft, vsl) = super::super::single_thread_setup();

        vsl.load_from_text(&"(+ (- 8 6) 2)", false).unwrap();

        while vsl.advance() {}

        assert!(vsl.inert_p());

        let result = vsl.result().unwrap();

        assert_eq!(i64_get(result), 4);
    }

    #[test]
    fn do_check() {
        let (_weft, vsl) = super::super::single_thread_setup();

        vsl.load_from_text(&"(do (+ 1 2) (- 4 3))", false).unwrap();

        while vsl.advance() {}

        assert!(vsl.inert_p());

        let result = vsl.result().unwrap();

        println!("{}", result.clone().refc_byte());

        assert_eq!(i64_get(result), 1);
    }
}

// TODO: call lambda functions using the stack?
// TODO: **macros**, closures, continuations
// TODO: special forms may be examples for creating / using native functions cleanly
// TODO: just like native functions, special forms should check for type
// TODO: match the argument structure to the number of arguments needed

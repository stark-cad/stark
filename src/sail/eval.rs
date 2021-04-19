use super::core::*;
use super::memmgt;
use super::{SP_DEF, SP_DO, SP_FN, SP_IF, SP_QUOTE};

use std::alloc;
use std::convert::TryInto;
use std::ptr;

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
    /// Location to write discarded return values
    null_loc: *mut usize,
}

impl EvalStack {
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
                null_loc: Box::into_raw(Box::from(0)),
            }
        }
    }

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

    #[inline(always)]
    pub fn push(&mut self, word: *mut SlHead) {
        if cfg!(feature = "stkdbg") {
            print!("push 1; ");
        }

        unsafe {
            if self.stack_top as usize + 8 >= self.stack_max as usize {
                self.resize((self.stack_max as usize - self.stack_start as usize) / 4)
            }
            let new_top = self.stack_top.add(1);
            self.stack_top = new_top;
            ptr::write(new_top, word as usize);
        }

        if cfg!(feature = "stkdbg") {
            println!(
                "size: {}",
                (self.stack_top as usize - self.stack_start as usize) / 8
            );
        }
    }

    #[inline(always)]
    fn pop(&mut self) {
        if cfg!(feature = "stkdbg") {
            print!("pop 1; ");
        }

        unsafe {
            self.stack_top = self.stack_top.sub(1);
        }

        if cfg!(feature = "stkdbg") {
            println!(
                "size: {}",
                (self.stack_top as usize - self.stack_start as usize) / 8
            );
        }
    }

    #[inline(always)]
    pub fn push_frame_head(&mut self, mut ret: *mut *mut SlHead, opc: Opcode, env: *mut SlHead) {
        if cfg!(feature = "stkdbg") {
            print!("PUSH: {:?}; ", opc);
        }

        unsafe {
            if self.stack_top as usize + 24 >= self.stack_max as usize {
                let old_top = self.stack_top as usize;
                self.resize((self.stack_max as usize - self.stack_start as usize) / 4);
                ret = ((ret as usize - old_top) + self.stack_top as usize) as *mut *mut SlHead;
            }
            let prev_top = self.stack_top;
            self.stack_top = prev_top.add(3);

            ptr::write(prev_top.add(1), self.frame_start as usize);
            ptr::write(prev_top.add(2), ret as usize);
            ptr::write(prev_top.add(3), ((env as usize) << 16) + opc as usize);

            self.frame_start = prev_top.add(1);
        }

        if cfg!(feature = "stkdbg") {
            println!(
                "size: {}",
                (self.stack_top as usize - self.stack_start as usize) / 8
            );
        }
    }

    #[inline(always)]
    fn pop_frame(&mut self) {
        if cfg!(feature = "stkdbg") {
            print!("POP: {:?}; ", self.frame_opc());
        }

        unsafe {
            let last_frame = ptr::read(self.frame_start);
            self.stack_top = self.frame_start.sub(1);
            self.frame_start = last_frame as *mut usize;
        }

        if cfg!(feature = "stkdbg") {
            println!(
                "size: {}",
                (self.stack_top as usize - self.stack_start as usize) / 8
            );
        }
    }

    #[inline(always)]
    pub fn is_empty(&mut self) -> bool {
        self.stack_start == self.stack_top
    }

    #[inline(always)]
    fn frame_opc(&mut self) -> Opcode {
        unsafe {
            ((ptr::read(self.frame_start.add(2)) & 0x000000000000FFFF) as u8)
                .try_into()
                .unwrap()
        }
    }

    #[inline(always)]
    fn frame_top(&mut self) -> (*mut *mut SlHead, *mut SlHead, Opcode) {
        let env_and_opc = unsafe { ptr::read(self.frame_start.add(2)) };
        (
            unsafe { ptr::read(self.frame_start.add(1) as *mut *mut *mut SlHead) },
            (env_and_opc >> 16) as *mut SlHead,
            ((env_and_opc & 0x000000000000FFFF) as u8)
                .try_into()
                .unwrap(),
        )
    }

    #[inline(always)]
    fn frame_obj(&mut self, offset: usize) -> *mut SlHead {
        unsafe { ptr::read(self.frame_start.add(3 + offset)) as *mut SlHead }
    }

    pub fn iter_once(&mut self, reg: *mut memmgt::Region, tbl: *mut SlHead) {
        // ***********************************
        // * Sail stack-based evaluation logic
        // ***********************************

        if self.stack_start == self.stack_top {
            return
        }

        let (ret, env, opc) = self.frame_top();

        if cfg!(feature = "stkdbg") {
            println!("ENTER: {:?}", opc);
        }

        match opc {
            Opcode::Eval => {
                let list = self.frame_obj(0);
                self.pop_frame();

                let raw_op = list;
                let raw_args = get_next_list_elt(list);

                if basic_sym_p(raw_op) {
                    match sym_get_id(raw_op) {
                        id if id == SP_DEF.0 => {
                            // needs: symbol to bind, value to bind to it
                            self.push_frame_head(ret, Opcode::Bind, env);
                            self.push(raw_args);

                            let value = get_next_list_elt(raw_args);
                            if nnil_ref_p(value) {
                                self.push(nil());

                                let return_to =
                                    unsafe { self.frame_start.add(4) } as *mut *mut SlHead;
                                self.push_frame_head(return_to, Opcode::Eval, env);
                                self.push(ref_get(value));
                            } else {
                                let out = if basic_sym_p(value) {
                                    env_lookup(env, value)
                                } else {
                                    value
                                };
                                self.push(out);
                            }
                            return;
                        }
                        id if id == SP_DO.0 => {
                            // needs: current remaining list of expressions
                            self.push_frame_head(ret, Opcode::DoSeq, env);
                            self.push(raw_args);
                            return;
                        }
                        id if id == SP_FN.0 => {
                            // needs: nothing else evaluated
                            // TODO: type annotations
                            let argvec = raw_args;
                            let argct = stdvec_get_len(argvec) as u16;
                            let out = init_proc_lambda(reg, argct);
                            for i in 0..argct {
                                proc_lambda_set_arg(
                                    out,
                                    i,
                                    sym_get_id(stdvec_idx(argvec, i as u32)),
                                );
                            }
                            proc_lambda_set_body(out, get_next_list_elt(raw_args));
                            unsafe { ptr::write(ret, out) };
                            return;
                        }
                        id if id == SP_IF.0 => {
                            // needs: evaluated test and both branches
                            self.push_frame_head(ret, Opcode::Branch, env);
                            self.push(nil());
                            self.push(get_next_list_elt(raw_args));
                            self.push(get_next_list_elt(get_next_list_elt(raw_args)));

                            let return_to = unsafe { self.frame_start.add(3) } as *mut *mut SlHead;

                            if nnil_ref_p(raw_args) {
                                self.push_frame_head(return_to, Opcode::Eval, env);
                                self.push(ref_get(raw_args));
                            } else {
                                let out = if basic_sym_p(raw_args) {
                                    env_lookup(env, raw_args)
                                } else {
                                    raw_args
                                };
                                unsafe { ptr::write(return_to, out) };
                            }
                            return;
                        }
                        id if id == SP_QUOTE.0 => {
                            // needs: nothing else evaluated
                            unsafe { ptr::write(ret, raw_args) };
                            return;
                        }
                        _ => {}
                    }
                }

                if nnil_ref_p(raw_op) {
                    self.push_frame_head(ret, Opcode::PreApp, env);
                    self.push(nil());
                    self.push(raw_args);

                    let return_to = unsafe { self.frame_start.add(3) } as *mut *mut SlHead;

                    self.push_frame_head(return_to, Opcode::Eval, env);
                    self.push(ref_get(raw_op));
                } else {
                    let proc = if basic_sym_p(raw_op) {
                        env_lookup(env, raw_op)
                    } else {
                        raw_op
                    };
                    assert!(proc_p(proc));

                    self.push_frame_head(ret, Opcode::Apply, env);

                    self.push(proc);
                    for _ in 0..proc_get_argct(proc) {
                        self.push(nil());
                    }
                    let apply_start = self.frame_start;
                    for i in 0..proc_get_argct(proc) {
                        let mut arg = raw_args;
                        for _ in 0..i {
                            arg = get_next_list_elt(arg);
                        }
                        let return_to =
                            unsafe { apply_start.add(4 + i as usize) } as *mut *mut SlHead;
                        if nnil_ref_p(arg) {
                            self.push_frame_head(return_to, Opcode::Eval, env);
                            self.push(ref_get(arg));
                        } else {
                            let out = if basic_sym_p(arg) {
                                env_lookup(env, arg)
                            } else {
                                arg
                            };
                            unsafe { ptr::write(return_to, out) };
                        }
                    }
                }
            }
            Opcode::Bind => {
                let symbol = self.frame_obj(0);
                assert!(basic_sym_p(symbol));
                let value = self.frame_obj(1);

                env_layer_ins_entry(reg, env, symbol, value);
                self.pop_frame();

                unsafe { ptr::write(ret, symbol) };
            }
            Opcode::DoSeq => {
                let remainder = self.frame_obj(0);
                if nil_p(get_next_list_elt(remainder)) {
                    self.pop_frame();
                    if nnil_ref_p(remainder) {
                        self.push_frame_head(ret, Opcode::Eval, env);
                        self.push(ref_get(remainder));
                    } else {
                        let out = if basic_sym_p(remainder) {
                            env_lookup(env, remainder)
                        } else {
                            remainder
                        };
                        unsafe { ptr::write(ret, out) };
                    }
                } else {
                    self.pop();
                    self.push(get_next_list_elt(remainder));
                    if nnil_ref_p(remainder) {
                        self.push_frame_head(self.null_loc as *mut *mut SlHead, Opcode::Eval, env);
                        self.push(ref_get(remainder));
                    }
                }
            }
            Opcode::Branch => {
                let pred_res = self.frame_obj(0);
                coretypck!(pred_res ; Bool);
                let true_body = self.frame_obj(1);
                let false_body = self.frame_obj(2);
                self.pop_frame();

                if bool_get(pred_res) {
                    if nnil_ref_p(true_body) {
                        self.push_frame_head(ret, Opcode::Eval, env);
                        self.push(ref_get(true_body));
                    } else {
                        let out = if basic_sym_p(true_body) {
                            env_lookup(env, true_body)
                        } else {
                            true_body
                        };
                        unsafe { ptr::write(ret, out) };
                    }
                } else {
                    if nnil_ref_p(false_body) {
                        self.push_frame_head(ret, Opcode::Eval, env);
                        self.push(ref_get(false_body));
                    } else {
                        let out = if basic_sym_p(false_body) {
                            env_lookup(env, false_body)
                        } else {
                            false_body
                        };
                        unsafe { ptr::write(ret, out) };
                    }
                }
            }
            Opcode::PreApp => {
                let proc = self.frame_obj(0);
                assert!(proc_p(proc));
                let raw_args = self.frame_obj(1);
                self.pop_frame();

                self.push_frame_head(ret, Opcode::Apply, env);

                self.push(proc);
                for _ in 0..proc_get_argct(proc) {
                    self.push(nil());
                }
                let apply_start = self.frame_start;
                for i in 0..proc_get_argct(proc) {
                    let mut arg = raw_args;
                    for _ in 0..i {
                        arg = get_next_list_elt(arg);
                    }
                    let return_to = unsafe { apply_start.add(4 + i as usize) } as *mut *mut SlHead;
                    if nnil_ref_p(arg) {
                        self.push_frame_head(return_to, Opcode::Eval, env);
                        self.push(ref_get(arg));
                    } else {
                        let out = if basic_sym_p(arg) {
                            env_lookup(env, arg)
                        } else {
                            arg
                        };
                        unsafe { ptr::write(return_to, out) };
                    }
                }
            }
            Opcode::Apply => {
                let proc = self.frame_obj(0);
                let typ = match core_type(proc) {
                    Some(t) if t == CoreType::ProcLambda => true,
                    Some(t) if t == CoreType::ProcNative => false,
                    _ => panic!("not a proc"),
                };

                let argct = proc_get_argct(proc);

                if typ {
                    let proc_env = env_new_arg_layer(reg);
                    set_next_list_elt(proc_env, env);

                    for i in 0..argct {
                        env_arg_layer_ins(
                            reg,
                            proc_env,
                            proc_lambda_get_arg(reg, proc, i),
                            self.frame_obj(i as usize + 1),
                        );
                    }

                    self.pop_frame();

                    self.push_frame_head(ret, Opcode::DoSeq, proc_env);
                    self.push(proc_lambda_get_body(proc));
                } else {
                    let args: &[*mut SlHead] = unsafe {
                        std::slice::from_raw_parts(
                            self.frame_start.add(4) as *mut *mut SlHead,
                            argct as usize,
                        )
                    };

                    let fn_rslt = proc_native_get_body(proc)(reg, tbl, env, args);

                    unsafe { ptr::write(ret, fn_rslt) };
                    self.pop_frame();
                }
            }
        }
    }
}

pub fn eval_expr(
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
    env: *mut SlHead,
    expr: *mut SlHead,
) -> *mut SlHead {
    let sigil = 1 as *mut SlHead;

    let mut result = sigil;
    let ret_addr: *mut *mut SlHead = &mut result;

    let mut stack = EvalStack::new(10000);

    if nnil_ref_p(expr) {
        stack.push_frame_head(ret_addr, Opcode::Eval, env);
        stack.push(ref_get(expr));
    } else {
        if basic_sym_p(expr) {
            result = env_lookup(env, expr);
        } else {
            result = expr;
        }
    }

    while result == sigil {
        stack.iter_once(reg, tbl);
    }

    result
}

enum_and_tryfrom! {
    /// Operation code for the frame; stored as a tag at the frame start
    #[derive(Debug, PartialEq, Eq)]
    #[repr(u8)]
    pub enum Opcode {
        /// List to be evaluated
        Eval,

        /// Symbol, value
        Bind,

        /// Remainder of list to do
        DoSeq,

        /// Predicate, true path, false path
        Branch,

        /// Procedure, argument list
        PreApp,

        /// Procedure, all arguments
        Apply,
    }
}

struct _Frame {
    last_frame: usize,
    return_address: usize,
    env_and_opcode: usize,
}

// TODO: call lambda functions using the stack?
// TODO: **Macros**, closures, continuations
// TODO: special forms may be examples for creating / using native functions cleanly
// TODO: just like native functions, special forms should check for type
// TODO: match the argument structure to the number of arguments needed
// TODO: tail call optimization

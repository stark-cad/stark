use super::core::*;
use super::memmgt;
use super::{SP_DEF, SP_DO, SP_FN, SP_IF, SP_QUOTE};

use std::alloc;
use std::convert::TryInto;
use std::ptr;

/// Sail evaluation stack
struct EvalStack {
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
    fn new(size: usize) -> Self {
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
            for p in 0..(top_offset / 8) {
                unsafe {
                    let address = new_start.add(p);
                    if *address >= old_start && *address <= old_top {
                        let addr_offset = *address - old_start;
                        *address = new_start as usize + addr_offset;
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
    fn push(&mut self, word: *mut SlHead) {
        print!("push 1; ");

        unsafe {
            let current_top = self.stack_top;
            if current_top as usize + 8 >= self.stack_max as usize {
                self.resize((self.stack_max as usize - self.stack_start as usize) / 4)
            }
            let new_top = self.stack_top.add(1);
            self.stack_top = new_top;
            ptr::write(new_top, word as usize);
        }

        println!(
            "size: {}",
            (self.stack_top as usize - self.stack_start as usize) / 8
        );
    }

    #[inline(always)]
    fn pop(&mut self) {
        print!("pop 1; ");

        unsafe {
            self.stack_top = self.stack_top.sub(1);
        }

        println!(
            "size: {}",
            (self.stack_top as usize - self.stack_start as usize) / 8
        );
    }

    #[inline(always)]
    fn push_frame_head(&mut self, ret: *mut *mut SlHead, opc: Opcode, env: *mut SlHead) {
        print!("PUSH: {:?}; ", opc);

        unsafe {
            let current_top = self.stack_top;
            if current_top as usize + 24 >= self.stack_max as usize {
                self.resize((self.stack_max as usize - self.stack_start as usize) / 4)
            }
            self.stack_top = current_top.add(3);

            ptr::write(current_top.add(1), self.frame_start as usize);
            ptr::write(current_top.add(2), ret as usize);
            ptr::write(current_top.add(3), ((env as usize) << 16) + opc as usize);

            self.frame_start = current_top.add(1);
        }

        println!(
            "size: {}",
            (self.stack_top as usize - self.stack_start as usize) / 8
        );
    }

    #[inline(always)]
    fn pop_frame(&mut self) {
        print!("POP: {:?}; ", self.frame_opc());

        unsafe {
            let last_frame = ptr::read(self.frame_start);
            self.stack_top = self.frame_start.sub(1);
            self.frame_start = last_frame as *mut usize;
        }

        println!(
            "size: {}",
            (self.stack_top as usize - self.stack_start as usize) / 8
        );
    }

    #[inline(always)]
    fn frame_ret(&mut self) -> *mut *mut SlHead {
        unsafe { ptr::read(self.frame_start.add(1) as *mut *mut *mut SlHead) }
    }

    #[inline(always)]
    fn frame_env(&mut self) -> *mut SlHead {
        unsafe { (ptr::read(self.frame_start.add(2)) >> 16) as *mut SlHead }
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
    fn frame_obj(&mut self, offset: usize) -> *mut SlHead {
        unsafe { ptr::read(self.frame_start.add(3 + offset)) as *mut SlHead }
    }

    // fn iter_once(&mut self) {}
}

pub fn eval_expr(
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
    env: *mut SlHead,
    expr: *mut SlHead,
) -> *mut SlHead {
    let mut result = nil();
    let ret_addr: *mut *mut SlHead = &mut result;

    let mut null = nil();
    let null_addr: *mut *mut SlHead = &mut null;

    let mut stack = EvalStack::new(10000);

    if ref_p(expr) {
        stack.push_frame_head(ret_addr, Opcode::Eval, env);
        stack.push(ref_get(expr));
    } else {
        if symbol_p(expr) {
            result = env_lookup(env, expr);
        } else {
            result = expr;
        }
    }

    // *********************************************************
    // * Sail stack-based evaluation logic
    // * TODO: split out to enable using a single thread stack
    // *********************************************************
    while stack.stack_top != stack.stack_start {
        let ret = stack.frame_ret();
        let env = stack.frame_env();
        let opc = stack.frame_opc();
        println!("ENTER: {:?}", opc);

        match opc {
            Opcode::Eval => {
                let list = stack.frame_obj(0);
                stack.pop_frame();

                let raw_op = list;
                let raw_args = get_next_list_elt(list);

                if symbol_p(raw_op) {
                    match sym_get_id(raw_op) {
                        id if id == SP_DEF.0 => {
                            // needs: symbol to bind, value to bind to it
                            stack.push_frame_head(ret, Opcode::Bind, env);
                            stack.push(raw_args);

                            let value = get_next_list_elt(raw_args);
                            if ref_p(value) {
                                stack.push(nil());

                                let return_to =
                                    unsafe { stack.frame_start.add(4) } as *mut *mut SlHead;
                                stack.push_frame_head(return_to, Opcode::Eval, env);
                                stack.push(ref_get(value));
                            } else {
                                let out = if symbol_p(value) {
                                    env_lookup(env, value)
                                } else {
                                    value
                                };
                                stack.push(out);
                            }
                            continue;
                        }
                        id if id == SP_DO.0 => {
                            // needs: current remaining list of expressions
                            stack.push_frame_head(ret, Opcode::DoSeq, env);
                            stack.push(raw_args);
                            continue;
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
                            continue;
                        }
                        id if id == SP_IF.0 => {
                            // needs: evaluated test and both branches
                            stack.push_frame_head(ret, Opcode::Branch, env);
                            stack.push(nil());
                            stack.push(get_next_list_elt(raw_args));
                            stack.push(get_next_list_elt(get_next_list_elt(raw_args)));

                            let return_to = unsafe { stack.frame_start.add(3) } as *mut *mut SlHead;

                            if ref_p(raw_args) {
                                stack.push_frame_head(return_to, Opcode::Eval, env);
                                stack.push(ref_get(raw_args));
                            } else {
                                let out = if symbol_p(raw_args) {
                                    env_lookup(env, raw_args)
                                } else {
                                    raw_args
                                };
                                unsafe { ptr::write(return_to, out) };
                            }
                            continue;
                        }
                        id if id == SP_QUOTE.0 => {
                            // needs: nothing else evaluated
                            unsafe { ptr::write(ret, raw_args) };
                            continue;
                        }
                        _ => {}
                    }
                }

                if ref_p(raw_op) {
                    stack.push_frame_head(ret, Opcode::PreApp, env);
                    stack.push(nil());
                    stack.push(raw_args);

                    let return_to = unsafe { stack.frame_start.add(3) } as *mut *mut SlHead;

                    stack.push_frame_head(return_to, Opcode::Eval, env);
                    stack.push(ref_get(raw_op));
                } else {
                    let proc = if symbol_p(raw_op) {
                        env_lookup(env, raw_op)
                    } else {
                        raw_op
                    };
                    assert!(proc_p(proc));

                    stack.push_frame_head(ret, Opcode::Apply, env);

                    stack.push(proc);
                    for _ in 0..proc_get_argct(proc) {
                        stack.push(nil());
                    }
                    let apply_start = stack.frame_start;
                    for i in 0..proc_get_argct(proc) {
                        let mut arg = raw_args;
                        for _ in 0..i {
                            arg = get_next_list_elt(arg);
                        }
                        let return_to =
                            unsafe { apply_start.add(4 + i as usize) } as *mut *mut SlHead;
                        if ref_p(arg) {
                            stack.push_frame_head(return_to, Opcode::Eval, env);
                            stack.push(ref_get(arg));
                        } else {
                            let out = if symbol_p(arg) {
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
                let symbol = stack.frame_obj(0);
                assert!(symbol_p(symbol));
                let value = stack.frame_obj(1);

                env_layer_ins_entry(reg, env, symbol, value);
                stack.pop_frame();

                unsafe { ptr::write(ret, symbol) };
            }
            Opcode::DoSeq => {
                let remainder = stack.frame_obj(0);
                if nil_p(get_next_list_elt(remainder)) {
                    stack.pop_frame();
                    if ref_p(remainder) {
                        stack.push_frame_head(ret, Opcode::Eval, env);
                        stack.push(ref_get(remainder));
                    } else {
                        let out = if symbol_p(remainder) {
                            env_lookup(env, remainder)
                        } else {
                            remainder
                        };
                        unsafe { ptr::write(ret, out) };
                    }
                } else {
                    stack.pop();
                    stack.push(get_next_list_elt(remainder));
                    if ref_p(remainder) {
                        stack.push_frame_head(null_addr, Opcode::Eval, env);
                        stack.push(ref_get(remainder));
                    }
                }
            }
            Opcode::Branch => {
                let pred_res = stack.frame_obj(0);
                coretypck!(pred_res ; Bool);
                let true_body = stack.frame_obj(1);
                let false_body = stack.frame_obj(2);
                stack.pop_frame();

                if bool_get(pred_res) {
                    if ref_p(true_body) {
                        stack.push_frame_head(ret, Opcode::Eval, env);
                        stack.push(ref_get(true_body));
                    } else {
                        let out = if symbol_p(true_body) {
                            env_lookup(env, true_body)
                        } else {
                            true_body
                        };
                        unsafe { ptr::write(ret, out) };
                    }
                } else {
                    if ref_p(false_body) {
                        stack.push_frame_head(ret, Opcode::Eval, env);
                        stack.push(ref_get(false_body));
                    } else {
                        let out = if symbol_p(false_body) {
                            env_lookup(env, false_body)
                        } else {
                            false_body
                        };
                        unsafe { ptr::write(ret, out) };
                    }
                }
            }
            Opcode::PreApp => {
                let proc = stack.frame_obj(0);
                assert!(proc_p(proc));
                let raw_args = stack.frame_obj(1);
                stack.pop_frame();

                stack.push_frame_head(ret, Opcode::Apply, env);

                stack.push(proc);
                for _ in 0..proc_get_argct(proc) {
                    stack.push(nil());
                }
                let apply_start = stack.frame_start;
                for i in 0..proc_get_argct(proc) {
                    let mut arg = raw_args;
                    for _ in 0..i {
                        arg = get_next_list_elt(arg);
                    }
                    let return_to = unsafe { apply_start.add(4 + i as usize) } as *mut *mut SlHead;
                    if ref_p(arg) {
                        stack.push_frame_head(return_to, Opcode::Eval, env);
                        stack.push(ref_get(arg));
                    } else {
                        let out = if symbol_p(arg) {
                            env_lookup(env, arg)
                        } else {
                            arg
                        };
                        unsafe { ptr::write(return_to, out) };
                    }
                }
            }
            Opcode::Apply => {
                let proc = stack.frame_obj(0);
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
                            stack.frame_obj(i as usize + 1),
                        );
                    }

                    stack.pop_frame();

                    stack.push_frame_head(ret, Opcode::DoSeq, proc_env);
                    stack.push(proc_lambda_get_body(proc));
                } else {
                    let args: &[*mut SlHead] = unsafe {
                        std::slice::from_raw_parts(
                            stack.frame_start.add(4) as *mut *mut SlHead,
                            argct as usize,
                        )
                    };

                    let fn_rslt = proc_native_get_body(proc)(reg, tbl, env, args);

                    unsafe { ptr::write(ret, fn_rslt) };
                    stack.pop_frame();
                }
            }
        }
    }

    result
}

enum_and_tryfrom! {
    /// Operation code for the frame; stored as a tag at the frame start
    #[derive(Debug, PartialEq, Eq)]
    #[repr(u8)]
    enum Opcode {
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

// endeavor to have all value slots filled by the time the frame arrives at the top
// return function arguments straight to the environment?
// how to call lambda functions using a stack?

// /// Evaluates a Sail value, returning the result
// /// TODO: **Macros**, closures, continuations
// pub fn eval(
//     reg: *mut memmgt::Region,
//     tbl: *mut SlHead,
//     env: *mut SlHead,
//     expr: *mut SlHead,
// ) -> Result<*mut SlHead, SailErr> {
//     // TODO: some kind of flag to indicate whether to assume listiness or assume atomicity?
//     if nil_p(cdr(expr)) {
//         if symbol_p(expr) {
//             return Ok(env_lookup(reg, env, expr));
//         } else {
//             return Ok(expr);
//         }
//     } else {
//         let lcar = car(expr);
//         let args = cdr(expr);

//         if symbol_p(lcar) {
//             // TODO: what other special forms are needed?
//             // TODO: is there a need for special forms? why not just make these native functions?
//             // TODO: these may be good examples for creating / using native functions cleanly
//             // TODO: just like native functions, these special forms should check for type
//             // TODO: split out into native functions to facilitate new evaluator architecture
//             let id = sym_get_id(lcar);
//             if id == SP_DEF.0 {
//                 env_layer_ins_entry(
//                     reg,
//                     car(env),
//                     car(args),
//                     eval(reg, tbl, env, car(cdr(args)))?,
//                 );
//                 return Ok(car(args));
//             } else if id == SP_DO.0 {
//                 let mut remain = args;
//                 let mut result = nil();
//                 while !nil_p(remain) {
//                     result = eval(reg, tbl, env, car(remain))?;
//                     remain = cdr(remain)
//                 }
//                 return Ok(result);
//             } else if id == SP_FN.0 {
//                 let argvec = car(args);
//                 let argct = stdvec_get_len(argvec) as u16;
//                 let out = init_proc_lambda(reg, argct);
//                 for i in 0..argct {
//                     proc_lambda_set_arg(out, i, sym_get_id(stdvec_idx(argvec, i as u32)));
//                 }
//                 proc_lambda_set_body(out, car(cdr(args)));
//                 return Ok(out);
//             } else if id == SP_IF.0 {
//                 let test = car(args);
//                 let fst = car(cdr(args));
//                 let snd = car(cdr(cdr(args)));
//                 if bool_get(eval(reg, tbl, env, test)?) {
//                     return eval(reg, tbl, env, fst);
//                 } else {
//                     return eval(reg, tbl, env, snd);
//                 }
//             } else if id == SP_QUOTE.0 {
//                 return Ok(car(args));
//             }
//         }
//         let operator = eval(reg, tbl, env, lcar)?;
//         // TODO: replace with next_list_elt(list_get(expr)) to avoid allocation
//         if proc_p(operator) {
//             return apply(reg, tbl, env, operator, args);
//         } else {
//             eprintln!("operator type error");
//             return Err(SailErr::Error);
//         }
//     }
// }

// /// Applies a Sail procedure to its arguments, returning the result
// /// TODO: execute multiple expressions in a lambda sequentially?
// /// TODO: match the argument structure to the number of arguments needed
// /// TODO: tail call optimization
// fn apply(
//     reg: *mut memmgt::Region,
//     tbl: *mut SlHead,
//     env: *mut SlHead,
//     proc: *mut SlHead,
//     args: *mut SlHead,
// ) -> Result<*mut SlHead, SailErr> {
//     let typ = match core_type(proc) {
//         Some(t) if t == CoreType::ProcLambda => true,
//         Some(t) if t == CoreType::ProcNative => false,
//         _ => return Err(SailErr::Error),
//     };

//     let argct = proc_get_argct(proc);
//     let proc_env = env_new_arg_layer(reg);

//     let mut arglist = args;
//     for i in 0..argct {
//         if nil_p(arglist) {
//             return Err(SailErr::Error);
//         }

//         let curarg = eval(reg, tbl, env, car(arglist))?;

//         if typ {
//             env_arg_layer_ins(reg, proc_env, proc_lambda_get_arg(reg, proc, i), curarg);
//         } else {
//             // TODO: need better call system for natives and maybe lambdas too
//             // special symbols "%0", "%1", "%2", etc for native arguments
//             let mut spec_str = String::from("%");
//             spec_str.push_str(&(i.to_string()));

//             let spec_sym_id = init_symbol(reg);
//             sym_set_id(spec_sym_id, sym_tab_get_id(reg, tbl, &spec_str));

//             env_arg_layer_ins(reg, proc_env, spec_sym_id, curarg);
//         }

//         arglist = cdr(arglist)
//     }

//     env_push_layer(env, proc_env);

//     let result = if typ {
//         eval(reg, tbl, env, proc_lambda_get_body(proc))
//     } else {
//         Ok(proc_native_get_body(proc)(tbl, env))
//     };

//     env_pop_layer(env);

//     result
// }

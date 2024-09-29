// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2023 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/thread.rs

// Sail execution threads

// <>

use std::pin::Pin;

use super::{eval, memmgt, parser, queue};
use super::{SlHndl, Stab, Styc};

/// Global context for inter-thread and human interaction
pub struct Tact {
    /// Table associating symbol names with symbol IDs
    symtab: Stab,

    /// Counter to issue unique IDs to every object type
    typctr: Styc,
}

impl Tact {
    pub fn create(symtab_size: usize) -> Self {
        Self {
            symtab: Stab::new(symtab_size),
            typctr: Styc::new(),
        }
    }

    pub fn symtab(&mut self) -> &mut Stab {
        &mut self.symtab
    }

    pub fn typctr(&mut self) -> &mut Styc {
        &mut self.typctr
    }
}

// TODO: track refs to thread from parent thread / other owners

// TODO: adjust the weft / thread data structures to avoid misuse of
// pointers; align with Rust norms to benefit from guarantees

// TODO: hand out handles to threads, not raw pointers

// TODO: structures / functions for arbitrary mapping between Sail
// threads and OS threads, in order to evenly and efficiently advance
// all active Sail threads

pub struct Weft {
    tact: Tact,
    nxt_tid: usize,

    threads: Vec<Pin<Box<ThreadHull>>>,

    append_lock: u8,
    // flags: Vec<u8>,
    // arcs: Vec<u32>,
}

impl Weft {
    pub fn create(tact: Tact) -> Self {
        Self {
            tact,
            nxt_tid: 0,
            threads: Vec::new(),
            append_lock: false as u8,
        }
    }

    pub fn ctx_mut(&mut self) -> &mut Tact {
        &mut self.tact
    }

    fn ctx_ptr(&self) -> *mut Tact {
        (&self.tact as *const Tact).cast_mut()
    }

    fn get_tid(&self) -> usize {
        unsafe {
            std::intrinsics::atomic_xadd_acqrel((&self.nxt_tid as *const usize).cast_mut(), 1)
        }
    }

    fn add_thread(&self, thread_hull: Pin<Box<ThreadHull>>) -> *mut ThreadHull {
        unsafe {
            let this = (self as *const Self).cast_mut().as_mut_unchecked();

            let lock: *mut u8 = &mut this.append_lock;
            while !std::intrinsics::atomic_cxchg_acqrel_acquire(lock, false as u8, true as u8).1 {
                std::hint::spin_loop();
            }

            let new_idx = self.threads.len();
            this.threads.push(thread_hull);

            std::intrinsics::atomic_store_release(lock, false as u8);

            Pin::into_inner_unchecked(Pin::as_mut(&mut this.threads[new_idx])) as *mut ThreadHull
        }
    }

    fn rmv_thread(&self, thread_ptr: *mut ThreadHull) {
        unsafe {
            let idx = match self.threads.iter().enumerate().find(|(_, b)| {
                Pin::into_inner_unchecked((*b).as_ref()) as *const _ as usize == thread_ptr as usize
            }) {
                Some((i, _)) => i,
                None => return,
            };

            let this = (self as *const Self).cast_mut().as_mut_unchecked();

            let lock: *mut u8 = &mut this.append_lock;
            while !std::intrinsics::atomic_cxchg_acqrel_acquire(lock, false as u8, true as u8).1 {
                std::hint::spin_loop();
            }

            let removed = this.threads.remove(idx);

            std::intrinsics::atomic_store_release(lock, false as u8);

            drop(removed);
        }
    }
}

// TODO: demonstrate spawning a thread from Sail code

pub struct ThreadHull {
    /// identifier (index in weft)
    pub id: usize,

    /// symtab and typctr (both shared)
    tact: *mut Tact,

    /// tracker for all extant threads
    weft: *const Weft,

    /// dedicated memory
    reg: memmgt::Region,

    /// queue target
    qin: queue::Inlet,

    /// top-level environment
    tenv: SlHndl,

    /// evaluator
    eval: eval::EvalStack,

    /// final return location
    out: *mut super::SlHead,

    /// does not implement Unpin
    _pin: std::marker::PhantomPinned,
}

unsafe impl Send for ThreadHull {}

// TODO: implement new-model threads and queues to cleanly support
// manager thread, record threads, and render thread

impl ThreadHull {
    pub fn summon(
        weft: &Weft,
        s_init_size: usize,
        r_zone_size: u32,
        env_parent: Option<SlHndl>,
    ) -> *mut Self {
        let it = {
            let mut uninit = Box::<Self>::new_uninit();
            let ptr = uninit.as_mut_ptr();

            unsafe {
                (&raw mut (*ptr).id).write(weft.get_tid());

                (&raw mut (*ptr).tact).write(weft.ctx_ptr());
                (&raw mut (*ptr).weft).write(weft);

                (&raw mut (*ptr).reg).write(memmgt::Region::new(r_zone_size));

                (&raw mut (*ptr).qin).write(queue::Inlet::new(&raw mut (*ptr).reg));
                (&raw mut (*ptr).tenv).write(super::env_create(&raw mut (*ptr).reg, env_parent));
                (&raw mut (*ptr).eval).write(eval::EvalStack::new(s_init_size));

                (&raw mut (*ptr).out).write(std::ptr::null_mut());
            }

            Box::into_pin(unsafe { uninit.assume_init() })
        };

        weft.add_thread(it)
    }

    pub fn spawn(&self, s_init_size: Option<usize>, r_zone_size: Option<u32>) -> *mut Self {
        let weft = unsafe { self.weft.as_ref_unchecked() };

        let it = {
            let mut uninit = Box::<Self>::new_uninit();
            let ptr = uninit.as_mut_ptr();

            unsafe {
                (&raw mut (*ptr).id).write(weft.get_tid());

                (&raw mut (*ptr).tact).write(weft.ctx_ptr());
                (&raw mut (*ptr).weft).write(weft);

                (&raw mut (*ptr).reg).write(memmgt::Region::new(
                    r_zone_size.unwrap_or(self.reg.zone_size),
                ));

                (&raw mut (*ptr).qin).write(queue::Inlet::new(&raw mut (*ptr).reg));
                (&raw mut (*ptr).tenv).write(super::env_create(
                    &raw mut (*ptr).reg,
                    Some(self.tenv.clone()),
                ));
                (&raw mut (*ptr).eval).write(eval::EvalStack::new(
                    s_init_size.unwrap_or(self.eval.size()),
                ));

                (&raw mut (*ptr).out).write(std::ptr::null_mut());
            }

            Box::into_pin(unsafe { uninit.assume_init() })
        };

        weft.add_thread(it)
    }

    pub fn context<'a, 'b>(&'a self) -> &'b mut Tact {
        // the TAble and CounTer will always outlive all threads
        unsafe { self.tact.as_mut_unchecked() }
    }

    pub fn region(&self) -> *mut memmgt::Region {
        ((&self.reg) as *const memmgt::Region).cast_mut()
    }

    pub fn queue_inlet(&self) -> *mut queue::Inlet {
        ((&self.qin) as *const queue::Inlet).cast_mut()
    }

    pub fn top_env(&self) -> SlHndl {
        self.tenv.clone()
    }

    pub fn load_from_text(&mut self, text: &str, prog: bool) -> Result<(), super::SlErrCode> {
        let expr_sexp = parser::parse(self.region(), self.context().symtab(), text, prog)?;

        assert!(self.eval.is_empty());
        self.eval.start(&mut self.out, self.top_env(), expr_sexp);

        Ok(())
    }

    pub fn load_proc_by_sym(&mut self, sym: u32) {
        let mut proc = super::env_lookup_by_id(self.top_env(), sym).expect("symbol not bound");
        coretypck!(proc ; ProcLambda);
        assert!(self.eval.is_empty());
        self.eval
            .push_frame_head(&mut self.out, eval::Opcode::Apply, self.top_env());
        self.eval.push(proc);
    }

    // TODO: somehow assert that code to run is in local region

    pub fn load_direct(&mut self, sexp: SlHndl) {
        assert!(self.eval.is_empty());
        self.eval.start(&mut self.out, self.top_env(), sexp);
    }

    pub fn inert_p(&self) -> bool {
        self.eval.is_empty()
    }

    pub fn advance(&mut self) -> bool {
        let self_ptr = self as *mut Self;
        self.eval.iter_once(self_ptr)
    }

    pub fn result(&self) -> Option<SlHndl> {
        unsafe { SlHndl::from_raw(self.out) }
    }

    pub fn clr_res(&mut self) {
        if !self.out.is_null() {
            unsafe {
                let old = SlHndl::from_raw_unchecked(self.out);
                self.out = std::ptr::null_mut();
                drop(old);
            }
        }
    }

    // pub fn insert_native_procs(&mut self, fns: &[(&str, super::NativeFn, u16)]) {
    //     super::insert_native_procs(self.region(), self.context().symtab(), self.top_env(), fns)
    // }

    // TODO: receive initial program over queue channel?
}

// check whether result slot is null as a basic way to know whether a
// thread is still "running"?

/// Use with std::thread::spawn to take over an OS thread and run to
/// completion of a Sail thread (simplest possible scheduler)
fn exec_thread(ptr: usize) {
    let hull = ptr as *mut ThreadHull;
    let th_ref = unsafe { &mut *hull };

    loop {
        if !th_ref.advance() {
            break;
        }
    }
}

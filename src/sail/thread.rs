// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2023 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/thread.rs

// Multi-threaded Sail execution. Mapping between M OS threads and N
// Sail vessels, with potentially N >>> M.

// <>

use std::pin::Pin;
use std::usize;

use std::intrinsics::AtomicOrdering as AO;

use super::{eval, memmgt, parser, queue};
use super::{SlHndl, Stab, Styc};

/// Global context data for inter-thread and human interaction
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

// TODO: track refs to vessel from parent / other owners

// TODO: adjust the weft / vessel data structures to avoid misuse of
// pointers; align with Rust norms to benefit from guarantees

// TODO: hand out handles to vessels, not raw pointers

// TODO: structures / functions for arbitrary mapping between Sail
// vessels and OS threads, in order to evenly and efficiently advance
// all active Sail vessels

pub struct SlVslHdl {
    raw: *mut Vessel,
}

// NOTE: once Sail vessels are added to the weft (so as soon as they
// are created), they should "magically" / automatically run

struct WorkTgtNode {
    next: *mut WorkTgtNode,
    prev: *mut WorkTgtNode,
    vsl: *mut Vessel,

    _pin: std::marker::PhantomPinned,
}

unsafe impl Send for WorkTgtNode {}
unsafe impl Sync for WorkTgtNode {}

// NOTE: code which runs in each OS thread to advance Sail vessels
// TODO: track stopped Sail vessels and thread::park when no work
// TODO: exit / terminate thread on command (sentinel in sentinel?)
fn worker_runtime(th_list_sentinel: usize) {
    let th_list_sentinel = th_list_sentinel as *const WorkTgtNode;
    let list_head = unsafe { th_list_sentinel.as_ref().unwrap() };

    // TODO: permit remote termination
    let mut cur_tgt = list_head;

    let mut count = 0;
    let mut last_len;

    // TODO: some way of assessing load on thread?
    loop {
        cur_tgt = unsafe {
            match cur_tgt.next.as_ref() {
                Some(r) => r,
                None => list_head,
            }
        };

        if list_head.vsl as usize == usize::MAX {
            break;
        }

        // TODO: somehow communicate when vessel done
        match unsafe { cur_tgt.vsl.as_mut() } {
            Some(vsl_ref) => {
                // TODO: re-freeze when stack empty
                if vsl_ref.advance() {
                    count += 1;
                }
            }
            None => {
                last_len = count;

                if last_len == 0 && count == 0 {
                    std::thread::park()
                }

                count = 0;
            }
        };
    }
}

// to add node to worker: insert immediately after its sentinel. write
// next element pointer of new node first, then write next element
// pointer of sentinel node, atomically. prev element pointer of next
// element and prev element pointer of new node can be handled
// separately.

// to remove node from worker: read prev and next node fields. write
// next node to prev element's next field, atomically. also write prev
// node to next element's prev field.

// the work target lists are rings, with the permanently owned empty
// sentinel always present.

// groups of vectors in the weft are parallel arrays, with equal
// element counts and associated indices!

// NOTE: other names: fiber? timber? member? strand? sheet?

// assign usizes are indices into the Sail vessel list

// TODO: should assign by vessel ID instead? might require mapping

// NOTE: can shorten structure by unifying parallel vector accounting

// TODO: basis lock and assignment lock?

pub struct Weft {
    pub tact: Tact,
    append_lock: u8,
    nxt_vsl_id: usize,

    /// Straight-line Sail execution contexts
    fleet: Vec<Pin<Box<Vessel>>>,
    // flags: Vec<u8>,
    // arcs: Vec<u32>,
    work_nodes: Vec<Pin<Box<WorkTgtNode>>>,

    /// OS threads used as workers
    warp: Vec<std::thread::JoinHandle<()>>,
    sentinels: Vec<Pin<Box<WorkTgtNode>>>,
    assignments: Vec<Vec<usize>>,

    special: Vec<usize>,

    frozen: Vec<usize>,
}

impl Drop for Weft {
    fn drop(&mut self) {
        for mut s in self.sentinels.drain(..) {
            unsafe { Pin::into_inner_unchecked(s.as_mut()) }.vsl = usize::MAX as *mut Vessel;
        }

        for t in self.warp.iter() {
            t.thread().unpark()
        }

        for t in self.warp.drain(..) {
            t.join().unwrap()
        }
    }
}

impl Weft {
    pub fn create(tact: Tact) -> Self {
        Self {
            tact,
            append_lock: false as u8,
            nxt_vsl_id: 2,

            fleet: Vec::with_capacity(4),
            work_nodes: Vec::with_capacity(4),

            warp: Vec::new(),
            sentinels: Vec::new(),
            assignments: Vec::new(),

            special: Vec::new(),

            frozen: Vec::new(),
        }
    }

    pub fn ctx_mut(&mut self) -> &mut Tact {
        &mut self.tact
    }

    fn ctx_ptr(&self) -> *mut Tact {
        (&self.tact as *const Tact).cast_mut()
    }

    fn get_tid(&mut self) -> usize {
        unsafe {
            std::intrinsics::atomic_xadd::<usize, usize, { AO::AcqRel }>(
                &mut self.nxt_vsl_id as *mut usize,
                1,
            )
        }
    }

    // NOTE: mutation through const pointers might require use of
    // unsafecell to be sound

    fn add_vessel(&mut self, vsl: Pin<Box<Vessel>>) -> *mut Vessel {
        unsafe {
            let lock: *mut u8 = &mut self.append_lock;
            while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                lock,
                false as u8,
                true as u8,
            )
            .1
            {
                std::hint::spin_loop();
            }

            let vsl_raw_pos =
                (Pin::into_inner_unchecked(Pin::as_ref(&vsl)) as *const Vessel).cast_mut();

            let new_idx = self.fleet.len();
            self.fleet.push(vsl);
            assert!(self.fleet.len() > new_idx);

            let node = Box::pin(WorkTgtNode {
                next: std::ptr::null_mut(),
                prev: std::ptr::null_mut(),
                vsl: vsl_raw_pos,

                _pin: std::marker::PhantomPinned,
            });

            self.work_nodes.push(node);
            self.frozen.push(new_idx);

            assert_eq!(self.fleet.len(), self.work_nodes.len());

            std::intrinsics::atomic_store::<_, { AO::Release }>(lock, false as u8);

            vsl_raw_pos
        }
    }

    fn rmv_vessel(&self, vsl_ptr: *mut Vessel) {
        unsafe {
            let idx = match self.fleet.iter().enumerate().find(|(_, b)| {
                Pin::into_inner_unchecked((*b).as_ref()) as *const _ as usize == vsl_ptr as usize
            }) {
                Some((i, _)) => i,
                None => return,
            };

            let this = (self as *const Self).cast_mut().as_mut_unchecked();

            let lock: *mut u8 = &mut this.append_lock;
            while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                lock,
                false as u8,
                true as u8,
            )
            .1
            {
                std::hint::spin_loop();
            }

            let removed = this.fleet.remove(idx);

            // TODO: remove all traces of the vessel in the structure

            std::intrinsics::atomic_store::<_, { AO::Release }, false>(lock, false as u8);

            drop(removed);
        }
    }

    pub fn add_worker(&mut self) {
        // TODO: maybe add reentry protection

        let sentinel = {
            let mut uninit = Box::<WorkTgtNode>::new_uninit();
            let ptr = uninit.as_mut_ptr();

            unsafe {
                (&raw mut (*ptr).next).write(ptr);
                (&raw mut (*ptr).prev).write(ptr);
                (&raw mut (*ptr).vsl).write(std::ptr::null_mut());
            }

            Box::into_pin(unsafe { uninit.assume_init() })
        };

        let sp = unsafe { Pin::into_inner_unchecked(sentinel.as_ref()) as *const _ } as usize;
        let new_thr = std::thread::Builder::new()
            .name(format!("worker {}", self.warp.len()))
            .spawn(move || worker_runtime(sp))
            .unwrap();

        self.warp.push(new_thr);
        self.sentinels.push(sentinel);
        self.assignments.push(Vec::new());

        assert_eq!(self.sentinels.len(), self.warp.len());
        assert_eq!(self.assignments.len(), self.warp.len());
    }

    fn apportion_work(&mut self) {
        // new vessels may appear in the weft at any time

        // check that all vessels are assigned to a hardware thread
        let live_unassigned = {
            let mut una = vec![true; self.work_nodes.len()];

            self.assignments
                .iter()
                .flatten()
                .for_each(|u| una[*u] = false);
            self.special.iter().for_each(|u| una[*u] = false);
            self.frozen.iter().for_each(|u| una[*u] = false);

            una.into_iter()
                .enumerate()
                .filter_map(|(i, b)| if b { Some(i) } else { None })
                .collect::<Vec<usize>>()
        };

        // for any unassigned vessels, assign them in a balanced manner
        let mut next_worker = 0;
        // TODO: this is not balanced
        for idx in live_unassigned {
            self.assignments[next_worker].push(idx);

            let insert_point =
                unsafe { Pin::into_inner_unchecked(self.sentinels[next_worker].as_mut()) };
            let to_insert = unsafe { Pin::into_inner_unchecked(self.work_nodes[idx].as_mut()) };

            to_insert.next = insert_point.next;
            to_insert.prev = insert_point;

            unsafe {
                insert_point.next.as_mut().unwrap().prev = to_insert;
            }

            unsafe {
                std::intrinsics::atomic_store::<_, { AO::Release }>(
                    &mut insert_point.next,
                    to_insert,
                );
            }

            self.warp[next_worker].thread().unpark();

            next_worker = (next_worker + 1) % self.warp.len();
        }

        // remove assignments of any finished vessels (?)
    }

    pub fn assign_special(&mut self, id: usize) {
        let tgt_idx = self
            .fleet
            .iter()
            .enumerate()
            .find_map(|(i, t)| if t.id == id { Some(i) } else { None })
            .unwrap();

        self.special.push(tgt_idx);

        // TODO: handle if already assigned to a worker?
        match self
            .frozen
            .iter()
            .enumerate()
            .find_map(|(fi, ti)| if *ti == tgt_idx { Some(fi) } else { None })
        {
            Some(fi) => assert_eq!(self.frozen.swap_remove(fi), tgt_idx),
            None => (),
        }
    }

    fn thaw(&mut self, id: usize) -> bool {
        let tgt_idx = self
            .fleet
            .iter()
            .enumerate()
            .find_map(|(i, t)| if t.id == id { Some(i) } else { None })
            .unwrap();

        match self
            .frozen
            .iter()
            .enumerate()
            .find_map(|(fi, ti)| if *ti == tgt_idx { Some(fi) } else { None })
        {
            Some(fi) => {
                // let this = unsafe { (self as *const Self).cast_mut().as_mut_unchecked() };
                self.frozen.swap_remove(fi);

                // TODO: start vessels only once ready (loaded)!
                if !self.warp.is_empty() {
                    self.apportion_work(); // ???
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }
}

// TODO: vessels could contain their own prev and next pointers for
// worker target tracking; then they'd be exactly 128 bytes (2 words)

#[repr(C)]
pub struct Vessel {
    /// identifier (index in weft)
    pub id: usize,

    /// tracker for all extant vessels
    weft: *mut Weft,

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

unsafe impl Send for Vessel {}

// TODO: implement vessels and queues to cleanly support governor,
// codex managers, record keepers, and render thread


// previously "ThreadHull"
impl Vessel {
    pub fn summon(
        weft: &mut Weft,
        s_init_size: usize,
        r_zone_size: u32,
        env_parent: Option<SlHndl>,
    ) -> *mut Self {
        let it = {
            let mut uninit = Box::<Self>::new_uninit();
            let ptr = uninit.as_mut_ptr();

            unsafe {
                (&raw mut (*ptr).id).write(weft.get_tid());
                (&raw mut (*ptr).weft).write(weft);

                (&raw mut (*ptr).reg).write(memmgt::Region::new(r_zone_size));
                (*ptr).reg.init();

                (&raw mut (*ptr).qin).write(queue::Inlet::new(&raw mut (*ptr).reg));
                (&raw mut (*ptr).tenv).write(super::env_create(&raw mut (*ptr).reg, env_parent));
                (&raw mut (*ptr).eval).write(eval::EvalStack::new(s_init_size));

                (&raw mut (*ptr).out).write(std::ptr::null_mut());
            }

            Box::into_pin(unsafe { uninit.assume_init() })
        };

        weft.add_vessel(it)
    }

    pub fn spawn(&self, s_init_size: Option<usize>, r_zone_size: Option<u32>) -> *mut Self {
        let weft = unsafe { self.weft.as_mut().unwrap() };

        let it = {
            let mut uninit = Box::<Self>::new_uninit();
            let ptr = uninit.as_mut_ptr();

            unsafe {
                (&raw mut (*ptr).id).write(weft.get_tid());
                (&raw mut (*ptr).weft).write(weft);

                (&raw mut (*ptr).reg).write(memmgt::Region::new(
                    r_zone_size.unwrap_or(self.reg.zone_size),
                ));
                (*ptr).reg.init();

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

        weft.add_vessel(it)
    }

    #[inline(always)]
    pub fn context<'a, 'b>(&'a self) -> &'b mut Tact {
        // the TAble and CounTer will always outlive all vessels
        // unsafe { self.tact.as_mut_unchecked() }
        unsafe { (&raw mut (*self.weft).tact).as_mut_unchecked() }
    }

    // TODO: again, switch to a handle type
    pub fn region(&self) -> *mut memmgt::Region {
        ((&self.reg) as *const memmgt::Region).cast_mut()
    }

    pub fn inlet_hdl(&mut self) -> queue::InletHdl {
        queue::InletHdl::new((&raw mut self.reg) as *mut queue::_SendFields)
    }

    pub fn msg_recv(&mut self) -> (usize, Option<SlHndl>) {
        self.qin.receive()
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

    pub fn load_proc_immed(&mut self, mut proc: SlHndl) {
        coretypck!(proc ; ProcLambda);
        assert!(self.eval.is_empty());

        self.eval
            .push_frame_head(&mut self.out, eval::Opcode::Apply, self.top_env());
        self.eval.push(proc);
    }

    pub fn load_proc_by_sym(&mut self, sym: u32) {
        let proc = super::env_lookup_by_id(self.top_env(), sym).expect("symbol not bound");
        self.load_proc_immed(proc)
    }

    // TODO: somehow assert that code to run is in local region

    pub fn load_direct(&mut self, sexp: SlHndl) {
        assert!(self.eval.is_empty());
        self.eval.start(&mut self.out, self.top_env(), sexp);
    }

    pub fn attempt_start(&self) -> bool {
        let weft = unsafe { self.weft.as_mut().unwrap() };
        weft.thaw(self.id)
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

    // TODO: this could be done much better (get it?)
    pub fn done_p(&self) -> bool {
        self.inert_p() && self.result().is_some()
    }

    // pub fn insert_native_procs(&mut self, fns: &[(&str, super::NativeFn, u16)]) {
    //     super::insert_native_procs(self.region(), self.context().symtab(), self.top_env(), fns)
    // }

    // TODO: receive initial program over queue channel?
}

/// Use with std::thread::spawn to take over an OS thread and run to
/// completion of a vessel (simplest possible scheduler)
pub fn exec_vessel(ptr: usize) {
    let vsl = ptr as *mut Vessel;
    let th_ref = unsafe { &mut *vsl };

    loop {
        if !th_ref.advance() {
            break;
        }
    }
}

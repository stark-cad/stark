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

use std::{alloc, ptr};

use std::intrinsics::AtomicOrdering as AO;

use super::{Hdl, SlHndl, SlSym, Stab, Styc};
use super::{debug, eval, memmgt, parser, queue};

/// Global context data for inter-thread and human interaction
pub struct Tact {
    /// Table associating symbol names with symbol IDs
    symtab: Stab,

    /// Counter to issue unique IDs to every object type
    typctr: Styc,
}

impl Tact {
    /// Instantiates symbol table and type layout counter
    pub fn create(symtab_size: usize) -> Self {
        Self {
            symtab: Stab::new(symtab_size),
            typctr: Styc::new(),
        }
    }

    /// Returns handle to symbol table
    pub fn symtab(self: Hdl<Self>) -> Hdl<Stab> {
        unsafe { Hdl::new(&raw mut (*self.raw()).symtab) }
    }

    /// Returns handle to type layout counter
    pub fn typctr(self: Hdl<Self>) -> Hdl<Styc> {
        unsafe { Hdl::new(&raw mut (*self.raw()).typctr) }
    }
}

// TODO: track refs to vessel from parent / other owners
// TODO: structures / functions for arbitrary mapping between Sail
// vessels and OS threads, in order to evenly and efficiently advance
// all active Sail vessels

// NOTE: once Sail vessels are added to the weft (so as soon as they
// are created), they should "magically" / automatically run

/// Represents one Vessel in the OS thread worklist
struct WorkTgtNode {
    next: *mut WorkTgtNode,
    prev: *mut WorkTgtNode,
    vsl: Option<Hdl<Vessel>>,
}

/// Flags for immediate communication to OS threads
struct WorkSemaphore {
    halt: bool,
}

/// Pointers to objects which must be visible to a worker OS thread
struct WorkerData {
    sentinel: *mut WorkTgtNode,
    semaphore: *mut WorkSemaphore,
}

unsafe impl Send for WorkerData {}

// TODO: track stopped Sail vessels and thread::park when no work
/// Code which runs in each worker OS thread to advance Sail vessels
fn worker_runtime(local_data: WorkerData) {
    let list_sentinel = local_data.sentinel;
    let semaphore = local_data.semaphore;

    let mut cur_tgt = list_sentinel;

    let mut count = 0;
    let mut last_len;

    // TODO: some way of assessing load on thread?
    loop {
        cur_tgt = unsafe { (*cur_tgt).next };

        if unsafe {
            std::intrinsics::atomic_load::<_, { AO::Acquire }, false>(
                &raw mut (*semaphore).halt as *const u8,
            ) != 0
        } {
            break;
        }

        // TODO: somehow communicate when vessel done
        match unsafe { (*cur_tgt).vsl } {
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

// TODO: should assign by vessel ID instead? might require mapping

// TODO: basis lock and assignment lock?

/// Handle representing ownership of a Sail multi-thread runtime manager
pub struct WeftOwner {
    pub raw: *mut Weft,
}

impl Drop for WeftOwner {
    fn drop(&mut self) {
        unsafe {
            ptr::drop_in_place(self.raw);

            let layout = alloc::Layout::for_value_raw(self.raw);
            alloc::dealloc(self.raw as _, layout);
        }
    }
}

// TODO: add special keeping for isolated modules (packages): a
// collection of regions owned directly by the Weft, possibly named;
// or choose another technique for packages later

/// Runtime manager for Sail: M Vessels x N OS threads
pub struct Weft {
    // Every structure that the Weft points at, it owns
    // The Weft must NOT move once it owns any Vessels
    // Vessels also must NOT move
    pub tact: Tact,
    append_lock: u8,
    nxt_vsl_id: usize,

    /// OS threads used as workers
    warp: RawVec<ThreadItems>,

    /// Straight-line Sail execution contexts
    fleet: RawVec<ExecItems>,

    temp_debug: Option<debug::SrcInfo>,
}

/// Weft-specific owning Vessel entry
struct ExecItems {
    vessel: ptr::NonNull<Vessel>,
    node: ptr::NonNull<WorkTgtNode>,
    /// if true, not to be assigned
    ignore: bool,
}

impl Drop for ExecItems {
    fn drop(&mut self) {
        // the weft owns these structures
        unsafe {
            drop(Box::from_raw(self.vessel.as_ptr()));

            alloc::dealloc(
                self.node.as_ptr() as _,
                alloc::Layout::for_value_raw(self.node.as_ptr()),
            );
        }
    }
}

/// Weft-specific owning OS thread entry
struct ThreadItems {
    os_thr_hdl: Option<std::thread::JoinHandle<()>>,
    sentinel: ptr::NonNull<WorkTgtNode>,
    semaphore: ptr::NonNull<WorkSemaphore>,
}

impl Drop for ThreadItems {
    fn drop(&mut self) {
        // the weft owns these structures
        unsafe {
            std::intrinsics::atomic_store::<_, { AO::Release }, false>(
                &raw mut (*self.semaphore.as_ptr()).halt as *mut u8,
                true as u8,
            );

            let thr_hdl = self.os_thr_hdl.take().unwrap();
            thr_hdl.thread().unpark();
            std::thread::sleep(std::time::Duration::from_millis(10));
            // if thr_hdl.is_finished() {
            thr_hdl.join().unwrap();
            // } else {
            // eprintln!("abandoning thread {:?}", thr_hdl.thread().name());
            // }

            alloc::dealloc(
                self.sentinel.as_ptr() as _,
                alloc::Layout::for_value_raw(self.sentinel.as_ptr()),
            );

            alloc::dealloc(
                self.semaphore.as_ptr() as _,
                alloc::Layout::for_value_raw(self.semaphore.as_ptr()),
            );
        }
    }
}

// TODO: the below belongs in a util module or similar

/// All ops raw
struct RawVec<T> {
    ptr: *mut T,
    len: usize,
    cap: usize,
}

impl<T> RawVec<T> {
    /// Creates a new heap-allocated vector with capacity
    fn new(cap: usize) -> Self {
        let init_layout = std::alloc::Layout::from_size_align(
            cap * std::mem::size_of::<T>(),
            std::mem::align_of::<T>(),
        )
        .unwrap();

        let ptr = unsafe { std::alloc::alloc(init_layout) as *mut T };

        Self { ptr, len: 0, cap }
    }

    /// Returns vector length
    fn len(self: *mut Self) -> usize {
        unsafe { (*self).len }
    }

    /// Gives vector new capacity, possibly moving memory
    fn resize(self: *mut Self, ncap: usize) {
        unsafe {
            let (len, ocap) = ((*self).len, (*self).cap);
            assert!(len < ncap);
            if ocap == ncap {
                return;
            }

            let new_layout = std::alloc::Layout::from_size_align(
                ncap * std::mem::size_of::<T>(),
                std::mem::align_of::<T>(),
            )
            .unwrap();

            let optr = (*self).ptr;
            let nptr = std::alloc::alloc(new_layout) as *mut T;

            std::ptr::copy_nonoverlapping(optr, nptr, (*self).len);

            (*self).ptr = nptr;

            let old_layout = std::alloc::Layout::from_size_align(
                ocap * std::mem::size_of::<T>(),
                std::mem::align_of::<T>(),
            )
            .unwrap();

            std::alloc::dealloc(optr as *mut _, old_layout);
        }
    }

    /// Adds value to the end of the vector
    fn push(self: *mut Self, item: T) {
        unsafe {
            let len = (*self).len;
            if len == (*self).cap {
                self.resize(len * 2);
            }

            ptr::write((*(&raw mut (*self).ptr)).add(len), item);
            (*self).len += 1;
        }
    }

    /// Returns pointer to value at offset i
    fn index(self: *mut Self, i: usize) -> *mut T {
        unsafe { (*self).ptr.add(i) }
    }
}

impl<T> Drop for RawVec<T> {
    fn drop(&mut self) {
        let layout = std::alloc::Layout::from_size_align(
            self.cap * std::mem::size_of::<T>(),
            std::mem::align_of::<T>(),
        )
        .unwrap();

        // Drop contents
        for i in 0..self.len {
            unsafe { std::ptr::drop_in_place(self.ptr.add(i)) }
        }

        unsafe {
            std::alloc::dealloc(self.ptr as *mut _, layout);
        }
    }
}

impl Drop for Weft {
    // invoked when WeftHandle gets dropped
    fn drop(&mut self) {
        eprintln!("DROP Weft");
        // anything special to do here?
    }
}

/// Methods to be received by *mut Self exclusively
impl Weft {
    // TODO: load initial interned symbols and stuff here
    /// Returns owning handle to a fresh Weft
    pub fn create(tact: Tact, enable_prototype_src_debug: bool) -> WeftOwner {
        let datum = Self {
            tact,
            append_lock: false as u8,
            nxt_vsl_id: 2,

            fleet: RawVec::new(10),

            // NOTE: warp size should be limited by processor count
            warp: RawVec::new(4),

            temp_debug: {
                if enable_prototype_src_debug {
                    Some(debug::SrcInfo::fresh())
                } else {
                    None
                }
            },
        };

        let raw = {
            let layout = alloc::Layout::new::<Weft>();
            let ptr;
            unsafe {
                ptr = alloc::alloc(layout) as *mut Weft;
                ptr.write(datum);
            }
            ptr
        };

        WeftOwner { raw }
    }

    /// Returns handle to symbol table and type layout counter together
    pub fn context(self: *mut Self) -> Hdl<Tact> {
        unsafe { Hdl::new(&raw mut (*self).tact) }
    }

    /// Returns next Vessel ID while advancing counter
    fn get_vsl_id(self: *mut Self) -> usize {
        unsafe {
            std::intrinsics::atomic_xadd::<usize, usize, { AO::AcqRel }>(
                &raw mut (*self).nxt_vsl_id,
                1,
            )
        }
    }

    // NOTE: ptr^.field and ptr^.method() are a good syntax idea for
    // ergonomic raw-pointer deref and use

    /// Adds provided Vessel to the Weft, taking ownership of it
    fn add_vessel(self: *mut Self, vsl: *mut Vessel) -> Hdl<Vessel> {
        // TODO: lock trait for these runtime structures
        unsafe {
            let lock: *mut u8 = &raw mut (*self).append_lock;
            while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                lock,
                false as u8,
                true as u8,
            )
            .1
            {
                std::hint::spin_loop();
            }

            let vsl_hdl = Hdl::new(vsl);

            let node = {
                let datum = WorkTgtNode {
                    next: ptr::null_mut(),
                    prev: ptr::null_mut(),
                    vsl: Some(vsl_hdl),
                };
                let layout = alloc::Layout::new::<WorkTgtNode>();
                let ptr = alloc::alloc(layout) as *mut WorkTgtNode;
                ptr.write(datum);
                ptr
            };

            let vsl_pack = ExecItems {
                vessel: ptr::NonNull::new_unchecked(vsl_hdl.raw()),
                node: ptr::NonNull::new(node).unwrap(),
                ignore: true,
            };

            (&raw mut (*self).fleet).push(vsl_pack);

            std::intrinsics::atomic_store::<_, { AO::Release }, false>(lock, false as u8);

            vsl_hdl
        }
    }

    // TODO: orderly wind-down and destruction of Sail runtime
    // fn rmv_vessel(self: *mut Self, vsl_ptr: SlVslHdl) {
    //     unsafe {
    //         let idx = match (*self)
    //             .fleet
    //             .iter()
    //             .enumerate()
    //             .find(|(_, b)| **b == vsl_ptr)
    //         {
    //             Some((i, _)) => i,
    //             None => return,
    //         };

    //         let lock: *mut u8 = &raw mut (*self).append_lock;
    //         while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
    //             lock,
    //             false as u8,
    //             true as u8,
    //         )
    //         .1
    //         {
    //             std::hint::spin_loop();
    //         }

    //         let removed = (*self).fleet.remove(idx);
    //         // TODO: remove all traces of the vessel in the structure

    //         std::intrinsics::atomic_store::<_, { AO::Release }, false>(lock, false as u8);

    //         drop(removed);
    //     }
    // }

    /// Adds worker OS thread to the Weft
    pub fn add_worker(self: *mut Self) {
        // TODO: maybe add reentry protection? who might add a worker?

        let sentinel = unsafe {
            let mut datum = WorkTgtNode {
                next: ptr::null_mut(),
                prev: ptr::null_mut(),
                vsl: None,
            };
            let layout = alloc::Layout::new::<WorkTgtNode>();
            let ptr = alloc::alloc(layout) as *mut WorkTgtNode;
            datum.next = ptr;
            datum.prev = ptr;
            ptr.write(datum);
            ptr
        };

        // TODO: standard function for heap allocation?
        let semaphore = unsafe {
            let datum = WorkSemaphore { halt: false };
            let layout = alloc::Layout::new::<WorkSemaphore>();
            let ptr = alloc::alloc(layout) as *mut WorkSemaphore;
            ptr.write(datum);
            ptr
        };

        let push_data = WorkerData {
            sentinel,
            semaphore,
        };

        unsafe {
            let new_thr = std::thread::Builder::new()
                .name(format!("worker {}", (&raw mut (*self).warp).len()))
                .spawn(move || worker_runtime(push_data))
                .unwrap();

            let thr_pack = ThreadItems {
                os_thr_hdl: Some(new_thr),
                sentinel: ptr::NonNull::new(sentinel).unwrap(),
                semaphore: ptr::NonNull::new(semaphore).unwrap(),
            };

            (&raw mut (*self).warp).push(thr_pack);
        }
    }

    /// Assigns free Vessels to worker OS threads for execution
    fn apportion_work(self: *mut Self) {
        // new vessels may appear in the weft at any time
        let mut live_nodes = vec![];
        unsafe {
            let fleet_ptr = &raw mut (*self).fleet;
            for i in 0..fleet_ptr.len() {
                if !(*fleet_ptr.index(i)).ignore {
                    live_nodes.push((*fleet_ptr.index(i)).node)
                }
            }
        }

        // protect against fleet order changing during collection
        live_nodes.sort();
        live_nodes.dedup();

        let mut assigned_nodes = std::collections::BTreeSet::new();
        let mut assign_counts = vec![];
        unsafe {
            let warp_ptr = &raw mut (*self).warp;
            for i in 0..warp_ptr.len() {
                let this_sent = (*warp_ptr.index(i)).sentinel;
                debug_assert!((*this_sent.as_ptr()).vsl.is_none());

                let mut count = 0;
                // TODO: atomic read?
                let mut node = (*this_sent.as_ptr()).next;
                while node != this_sent.as_ptr() {
                    assert!(!node.is_null());
                    if (*node).vsl.is_some() {
                        let p = assigned_nodes.insert(ptr::NonNull::new_unchecked(node));
                        assert!(p);
                    }
                    count += 1;
                    node = (*node).next;
                }
                assign_counts.push(count);
            }
        }

        // find any vessels that aren't assigned to a thread
        live_nodes.retain(|e| !assigned_nodes.contains(e));

        // for any unassigned vessels, assign them in a balanced manner
        let mut worker = assign_counts
            .into_iter()
            .enumerate()
            .min_by_key(|e| e.1)
            .unwrap()
            .0;

        unsafe {
            let warp_ptr = &raw mut (*self).warp;
            for node in live_nodes {
                let insert_point = (*warp_ptr.index(worker)).sentinel.as_ptr();

                (*node.as_ptr()).next = (*insert_point).next;
                (*node.as_ptr()).prev = insert_point;

                // insert_point^.next^.prev = node.as_ptr();
                (*(*insert_point).next).prev = node.as_ptr();

                std::intrinsics::atomic_store::<_, { AO::Release }, false>(
                    &raw mut (*insert_point).next,
                    node.as_ptr(),
                );

                (*warp_ptr.index(worker))
                    .os_thr_hdl
                    .as_ref()
                    .unwrap()
                    .thread()
                    .unpark();

                worker = (worker + 1) % warp_ptr.len();
            }
        }

        // TODO: remove assignments of any finished vessels (?)
    }

    // NOTE: maybe just don't thaw it
    // pub fn assign_special(self: *mut Self, id: usize) {
    //     unsafe {
    //         let tgt_idx = (*self)
    //             .fleet
    //             .iter()
    //             .enumerate()
    //             .find_map(|(i, t)| if (**t).id == id { Some(i) } else { None })
    //             .unwrap();

    //         (*self).special.push(tgt_idx);

    //         // TODO: what if already assigned to a worker?
    //         match (*self)
    //             .frozen
    //             .iter()
    //             .enumerate()
    //             .find_map(|(fi, ti)| if *ti == tgt_idx { Some(fi) } else { None })
    //         {
    //             Some(fi) => assert_eq!((*self).frozen.swap_remove(fi), tgt_idx),
    //             None => (),
    //         }
    //     }
    // }

    // TODO: prevent "thawing" of special vessels
    // TODO: by what value is it best to find a vessel?
    /// Free a Vessel entry for worker assignment
    fn thaw(self: *mut Self, id: usize) -> bool {
        unsafe {
            let fleet_ptr = &raw mut (*self).fleet;
            for i in 0..fleet_ptr.len() {
                if (*(*fleet_ptr.index(i)).vessel.as_ptr()).id == id {
                    (*fleet_ptr.index(i)).ignore = false;

                    assert!((&raw mut (*self).warp).len() > 0);
                    self.apportion_work();
                    return true;
                }
            }
        }
        false
    }
}

// TODO: vessels could contain their own prev and next pointers for
// worker target tracking; then they'd be 128 bytes (2 cache lines)

// NOTE: or we could expand Region by up to two words for large object
// accounting and possibly easier handling
/// Runtime data for a single strand of Sail execution
#[repr(C)]
pub struct Vessel {
    /// identifier (index in weft)
    id: usize,

    /// tracker for all extant vessels
    weft: *mut Weft,

    /// final return location
    out: *mut super::SlHead,

    /// top-level environment
    tenv: SlHndl,

    /// evaluator
    eval: eval::EvalStack,

    /// dedicated memory
    reg: memmgt::Region,

    /// queue target
    qin: queue::Inlet,
}

unsafe impl Send for Vessel {}

impl Drop for Vessel {
    fn drop(&mut self) {
        eprintln!("DROP Vessel");
        Hdl::new(self).clr_res();
    }
}

// TODO: implement vessels and queues to cleanly support governor,
// codex managers, record keepers, and render thread


impl Vessel {
    /// Creates new Vessel, adding it to a Weft
    pub fn summon(weft: &mut WeftOwner, s_init_size: u16, r_zone_ord: u8) -> Hdl<Self> {
        let it = Box::into_raw({
            let mut uninit = Box::<Self>::new_uninit();
            let ptr = uninit.as_mut_ptr();

            unsafe {
                (&raw mut (*ptr).id).write(weft.raw.get_vsl_id());
                (&raw mut (*ptr).weft).write(weft.raw);

                (&raw mut (*ptr).reg).write(memmgt::Region::new(r_zone_ord));
                Hdl::new(&raw mut (*ptr).reg).init();

                (&raw mut (*ptr).qin).write(queue::Inlet::new(&raw mut (*ptr).reg));
                (&raw mut (*ptr).tenv).write(super::env_make(&raw mut (*ptr).reg));
                (&raw mut (*ptr).eval).write(eval::EvalStack::new(s_init_size));

                (&raw mut (*ptr).out).write(ptr::null_mut());
            }

            unsafe { uninit.assume_init() }
        });

        // the result is always registered to the weft
        weft.raw.add_vessel(it)
    }

    /// Creates new Vessel based on one existing (same Weft)
    pub fn spawn(self: Hdl<Self>, s_init_size: Option<u16>, r_zone_ord: Option<u8>) -> Hdl<Self> {
        let it = Box::into_raw({
            let mut uninit = Box::<Self>::new_uninit();
            let ptr = uninit.as_mut_ptr();

            unsafe {
                (&raw mut (*ptr).id).write((*self.raw()).weft.get_vsl_id());
                (&raw mut (*ptr).weft).write((*self.raw()).weft);

                (&raw mut (*ptr).reg)
                    .write(memmgt::Region::new(r_zone_ord.unwrap_or(
                        memmgt::Region::zn_ord(Hdl::new(&raw mut (*self.raw()).reg)),
                    )));
                Hdl::new(&raw mut (*ptr).reg).init();

                (&raw mut (*ptr).qin).write(queue::Inlet::new(&raw mut (*ptr).reg));
                (&raw mut (*ptr).tenv).write(super::env_make(&raw mut (*ptr).reg));
                (&raw mut (*ptr).eval).write(eval::EvalStack::new(
                    s_init_size.unwrap_or((&raw const (*self.raw()).eval).size()),
                ));

                (&raw mut (*ptr).out).write(ptr::null_mut());
            }

            unsafe { uninit.assume_init() }
        });

        // the result is always registered to the weft
        unsafe { (*self.raw()).weft.add_vessel(it) }
    }

    /// Returns unique Vessel identifier
    pub fn id(self: Hdl<Self>) -> usize {
        unsafe { (*self.raw()).id }
    }


    #[inline(always)]
    /// Returns relevant symbol table and type layout counter
    pub fn context(self: Hdl<Self>) -> Hdl<Tact> {
        // the TAble and CounTer will always outlive all vessels
        // self^.weft~tact
        unsafe { (&raw mut *(*self.raw()).weft).context() }
    }

    /// Returns handle to this Vessel's memory region
    pub fn region(self: Hdl<Self>) -> Hdl<memmgt::Region> {
        unsafe { Hdl::new(&raw mut (*self.raw()).reg) }
    }

    /// Returns handle to this Vessel's queue inlet
    pub fn inlet_hdl(self: Hdl<Self>) -> queue::InletHdl {
        unsafe {
            // this works
            queue::InletHdl::new((&raw mut (*self.raw()).reg) as *mut queue::_SendFields)
        }
    }

    /// Returns top message from this Vessel's incoming queue
    pub fn msg_recv(self: Hdl<Self>) -> (usize, Option<SlHndl>) {
        unsafe { (&raw mut (*self.raw()).qin).receive() }
    }

    /// Returns top-level local environment for this Vessel
    pub fn top_env(self: Hdl<Self>) -> SlHndl {
        unsafe { (*self.raw()).tenv.clone() }
    }

    /// Returns reference to relevant source-level debug data
    pub fn src_dbg_info(self: Hdl<Self>) -> Option<&'static mut debug::SrcInfo> {
        unsafe { (&raw mut (*(*self.raw()).weft).temp_debug).as_mut_unchecked() }.as_mut()
    }

    /// Binds symbol in this Vessel's top-level local environment
    pub fn bind_sym(self: Hdl<Self>, sym_id: SlSym, obj: SlHndl) {
        let repp = super::env_ins_by_id(self.region(), self.top_env(), sym_id, obj);
        assert!(!repp);
    }

    /// Loads Sail program for evaluation from provided file
    pub fn load_from_file<T: AsRef<std::path::Path>>(
        self: Hdl<Self>,
        path: T,
    ) -> Result<(), debug::SlErrCode> {
        let sexp = parser::parse_file(
            self.region(),
            self.context().symtab(),
            path.as_ref().to_path_buf(),
            self.src_dbg_info(),
        )?;

        assert!(self.inert_p());
        unsafe {
            (&raw mut (*self.raw()).eval).start(&raw mut (*self.raw()).out, self.top_env(), sexp);
        }

        Ok(())
    }

    /// Loads Sail program for evaluation directly from provided text
    pub fn load_from_text(self: Hdl<Self>, text: &str, prog: bool) -> Result<(), debug::SlErrCode> {
        let expr_sexp = if prog {
            parser::parse_text(
                self.region(),
                self.context().symtab(),
                parser::wrap_prog(text.as_bytes()),
                self.src_dbg_info(),
            )?
        } else {
            // TODO: also modify debug state?
            parser::parse_text(
                self.region(),
                self.context().symtab(),
                Vec::from(text),
                self.src_dbg_info(),
            )?
        };

        assert!(self.inert_p());
        unsafe {
            (&raw mut (*self.raw()).eval).start(
                &raw mut (*self.raw()).out,
                self.top_env(),
                expr_sexp,
            );
        }

        Ok(())
    }

    /// Loads Sail procedure for immediate evaluation
    pub fn load_proc_immed(self: Hdl<Self>, mut proc: SlHndl, args: &[SlHndl]) {
        coretypck!(proc ; ProcLambda);
        assert!(self.inert_p());
        unsafe {
            (&raw mut (*self.raw()).eval).start_proc(
                &raw mut (*self.raw()).out,
                self.top_env(),
                proc,
                args,
            );
        }
    }

    /// Loads Sail procedure for immediate evaluation, by symbol lookup
    pub fn load_proc_by_sym(self: Hdl<Self>, sym: u32) {
        let proc = super::env_lookup_by_id(self.top_env(), sym).expect("symbol not bound");
        self.load_proc_immed(proc, &[])
    }

    // TODO: somehow assert that code to run is in local region

    /// Loads Sail expression for immediate evaluation
    pub fn load_direct(self: Hdl<Self>, sexp: SlHndl) {
        assert!(self.inert_p());
        unsafe {
            (&raw mut (*self.raw()).eval).start(&raw mut (*self.raw()).out, self.top_env(), sexp);
        }
    }

    /// Attempts to start evaluation running on Weft workers
    pub fn attempt_start(self: Hdl<Self>) -> bool {
        unsafe { (*self.raw()).weft.thaw((*self.raw()).id) }
    }

    /// True if the evaluator is empty of code
    pub fn inert_p(self: Hdl<Self>) -> bool {
        unsafe { (&raw const (*self.raw()).eval) }.is_empty()
    }

    // TODO: stack is Vessel's core; move to methods on Vessel itself?
    // define Vessel in eval?
    /// Advances evaluator by one stack frame
    pub fn advance(self: Hdl<Self>) -> bool {
        unsafe { (&raw mut (*self.raw()).eval) }.iter_once(self)
    }

    /// Returns final result of this Vessel
    pub fn result(self: Hdl<Self>) -> Option<SlHndl> {
        unsafe { SlHndl::from_raw((*self.raw()).out) }
    }

    /// Clears final result slot
    pub fn clr_res(self: Hdl<Self>) {
        unsafe {
            if !(*self.raw()).out.is_null() {
                let old = SlHndl::from_raw_unchecked((*self.raw()).out);
                (*self.raw()).out = ptr::null_mut();
                drop(old);
            }
        }
    }

    // TODO: this could be done much better (get it?)
    /// True if execution has finished with a result
    pub fn done_p(self: Hdl<Self>) -> bool {
        self.inert_p() && self.result().is_some()
    }

    // pub fn insert_native_procs(&mut self, fns: &[(&str, super::NativeFn, u16)]) {
    //     super::insert_native_procs(self.region(), self.context().symtab(), self.top_env(), fns)
    // }

    // TODO: receive initial program over queue channel?
}

/// Use with std::thread::spawn to take over an OS thread and run to
/// completion of a Vessel (simplest possible scheduler)
pub fn exec_vessel(ptr: Hdl<Vessel>) {
    loop {
        if !ptr.advance() {
            break;
        }
    }
}

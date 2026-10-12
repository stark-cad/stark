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

use std::{alloc, ptr, sync::atomic};

use std::intrinsics::AtomicOrdering as AO;

use super::{Hdl, SlHndl, SlSym, Stab, Styc};
use super::{debug, eval, memmgt, parser, queue, util};
use util::Lock;

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

/// Flags for immediate communication with OS threads
struct WorkSemaphore {
    halt: atomic::AtomicBool,
    park: atomic::AtomicBool,
}

/// Pointers to objects which must be visible to a worker OS thread
struct WorkerData {
    sentinel: *mut WorkTgtNode,
    semaphore: *mut WorkSemaphore,
}

unsafe impl Send for WorkerData {}

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

        if unsafe { (*semaphore).halt.load(atomic::Ordering::Acquire) } {
            break;
        }

        // TODO: somehow communicate when vessel done
        match unsafe { (*cur_tgt).vsl } {
            Some(vsl_ref) => {
                if vsl_ref.advance() {
                    count += 1;
                }
            }
            None => {
                // live vessels on last loop through work list
                last_len = count;

                if last_len == 0 && count == 0 {
                    unsafe { (*semaphore).park.store(true, atomic::Ordering::Release) };

                    std::thread::park();

                    unsafe { (*semaphore).park.store(false, atomic::Ordering::Release) };
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
    raw: *mut Weft,
}

impl WeftOwner {
    pub fn hdl(&self) -> Hdl<Weft> {
        Hdl::new(self.raw)
    }
}

impl Drop for WeftOwner {
    fn drop(&mut self) {
        eprintln!("DROP Weft owner");
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

    /// Straight-line Sail execution contexts
    fleet: util::RawVec<ExecEntry>,

    /// OS threads used as workers
    warp: util::RawVec<WorkerEntry>,

    /// Non-worker OS threads that need parking support
    special: util::RawVec<ThreadEntry>,

    temp_debug: Option<debug::SrcInfo>,
}

impl Lock for Weft {
    fn lock_pos(self: Hdl<Self>) -> *mut u8 {
        unsafe { &raw mut (*self.raw()).append_lock }
    }
}

/// Weft-specific owning Vessel entry
struct ExecEntry {
    vessel: ptr::NonNull<Vessel>,
    node: ptr::NonNull<WorkTgtNode>,
    /// if true, not to be assigned
    ignore: bool,
}

impl Drop for ExecEntry {
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
struct WorkerEntry {
    os_thr_hdl: Option<std::thread::JoinHandle<()>>,
    sentinel: ptr::NonNull<WorkTgtNode>,
    semaphore: ptr::NonNull<WorkSemaphore>,
}

impl Drop for WorkerEntry {
    fn drop(&mut self) {
        // the weft owns these structures
        unsafe {
            std::intrinsics::atomic_store::<_, { AO::Release }, false>(
                &raw mut (*self.semaphore.as_ptr()).halt as *mut u8,
                true as u8,
            );

            let thr_hdl = self.os_thr_hdl.take().unwrap();
            thr_hdl.thread().unpark();
            std::thread::sleep(std::time::Duration::from_millis(2));
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

/// Weft-specific entry for a separate thread which manages a Vessel
struct ThreadEntry {
    os_thr_hdl: std::thread::Thread,
    vsl_id: usize,
    park: atomic::AtomicBool,
}

/// Methods to be received by Hdl<Weft> exclusively
impl Weft {
    // TODO: load initial interned symbols and stuff here
    /// Returns owning handle to a fresh Weft
    pub fn create(tact: Tact, enable_prototype_src_debug: bool) -> WeftOwner {
        let datum = Self {
            tact,
            append_lock: false as u8,
            nxt_vsl_id: 2,

            fleet: util::RawVec::new(10),

            // NOTE: warp size should be limited by processor count
            warp: util::RawVec::new(4),

            special: util::RawVec::new(2),

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
    pub fn context(self: Hdl<Self>) -> Hdl<Tact> {
        unsafe { Hdl::new(&raw mut (*self.raw()).tact) }
    }

    /// Returns next Vessel ID while advancing counter
    fn get_vsl_id(self: Hdl<Self>) -> usize {
        unsafe {
            std::intrinsics::atomic_xadd::<usize, usize, { AO::AcqRel }>(
                &raw mut (*self.raw()).nxt_vsl_id,
                1,
            )
        }
    }

    fn fleet_ref(self: Hdl<Self>) -> util::RVH<ExecEntry> {
        unsafe { util::RVH(&raw mut (*self.raw()).fleet) }
    }

    fn warp_ref(self: Hdl<Self>) -> util::RVH<WorkerEntry> {
        unsafe { util::RVH(&raw mut (*self.raw()).warp) }
    }

    fn special_ref(self: Hdl<Self>) -> util::RVH<ThreadEntry> {
        unsafe { util::RVH(&raw mut (*self.raw()).special) }
    }

    // NOTE: ptr^.field and ptr^.method() are a good syntax idea for
    // ergonomic raw-pointer deref and use

    /// Adds provided Vessel to the Weft, taking ownership of it
    fn add_vessel(self: Hdl<Self>, vsl: Hdl<Vessel>) {
        let tok = self.take_lock();
        unsafe {
            let node = {
                let datum = WorkTgtNode {
                    next: ptr::null_mut(),
                    prev: ptr::null_mut(),
                    vsl: Some(vsl),
                };
                let layout = alloc::Layout::new::<WorkTgtNode>();
                let ptr = alloc::alloc(layout) as *mut WorkTgtNode;
                ptr.write(datum);
                ptr
            };

            let vsl_pack = ExecEntry {
                vessel: ptr::NonNull::new_unchecked(vsl.raw()),
                node: ptr::NonNull::new(node).unwrap(),
                ignore: true,
            };

            self.fleet_ref().push(vsl_pack);
        }
        self.release_lock(tok);
    }

    // fn rmv_vessel(self: Hdl<Self>, vsl_ptr: SlVslHdl) {
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

    //         let tok = self.take_lock();

    //         let removed = (*self).fleet.remove(idx);
    //         // TODO: remove all traces of the vessel in the structure

    //         self.release_lock(tok);

    //         drop(removed);
    //     }
    // }

    /// Adds worker OS thread to the Weft
    pub fn add_worker(self: Hdl<Self>) {
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
            let datum = WorkSemaphore {
                halt: atomic::AtomicBool::new(false),
                park: atomic::AtomicBool::new(false),
            };
            let layout = alloc::Layout::new::<WorkSemaphore>();
            let ptr = alloc::alloc(layout) as *mut WorkSemaphore;
            ptr.write(datum);
            ptr
        };

        let push_data = WorkerData {
            sentinel,
            semaphore,
        };

        let new_thr = std::thread::Builder::new()
            .name(format!("worker {}", self.warp_ref().len()))
            .spawn(move || worker_runtime(push_data))
            .unwrap();

        let thr_pack = WorkerEntry {
            os_thr_hdl: Some(new_thr),
            sentinel: ptr::NonNull::new(sentinel).unwrap(),
            semaphore: ptr::NonNull::new(semaphore).unwrap(),
        };

        self.warp_ref().push(thr_pack);
    }

    /// Adds unowned OS thread entry to the Weft, returns parking flag
    fn add_special(
        self: Hdl<Self>,
        th: std::thread::Thread,
        vsl: Hdl<Vessel>,
    ) -> *mut atomic::AtomicBool {
        unsafe {
            let fleet_ref = self.fleet_ref();
            for i in 0..fleet_ref.len() {
                if (*fleet_ref.index(i)).vessel.as_ptr() == vsl.raw() {
                    assert!((*fleet_ref.index(i)).ignore);
                    break;
                }
            }
            // TODO: ensure vessel is actually present?
        }

        let thr_pack = ThreadEntry {
            os_thr_hdl: th,
            vsl_id: vsl.id(),
            park: atomic::AtomicBool::new(false),
        };

        unsafe {
            let tlen = self.special_ref().len();
            self.special_ref().push(thr_pack);

            &raw mut (*self.special_ref().index(tlen)).park
        }
    }

    /// Assigns free Vessels to worker OS threads for execution
    fn apportion_work(self: Hdl<Self>) {
        // new vessels may appear in the weft at any time
        let mut live_nodes = vec![];
        unsafe {
            let fleet_ref = self.fleet_ref();
            for i in 0..fleet_ref.len() {
                if !(*fleet_ref.index(i)).ignore {
                    live_nodes.push((*fleet_ref.index(i)).node)
                }
            }
        }

        // protect against fleet order changing during collection
        live_nodes.sort();
        live_nodes.dedup();

        let mut assigned_nodes = std::collections::BTreeSet::new();
        let mut assign_counts = vec![];
        unsafe {
            let warp_ref = self.warp_ref();
            for i in 0..warp_ref.len() {
                let this_sent = (*warp_ref.index(i)).sentinel;
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
            let warp_ref = self.warp_ref();
            for node in live_nodes {
                let insert_point = (*warp_ref.index(worker)).sentinel.as_ptr();

                (*node.as_ptr()).next = (*insert_point).next;
                (*node.as_ptr()).prev = insert_point;

                // insert_point^.next^.prev = node.as_ptr();
                (*(*insert_point).next).prev = node.as_ptr();

                std::intrinsics::atomic_store::<_, { AO::Release }, false>(
                    &raw mut (*insert_point).next,
                    node.as_ptr(),
                );

                (*warp_ref.index(worker))
                    .os_thr_hdl
                    .as_ref()
                    .unwrap()
                    .thread()
                    .unpark();

                worker = (worker + 1) % warp_ref.len();
            }
        }

        // TODO: remove assignments of any finished vessels (?)
    }

    // TODO: prevent "thawing" of special vessels
    // TODO: by what value is it best to find a vessel?
    /// Free a Vessel entry for worker assignment
    fn thaw(self: Hdl<Self>, id: usize) -> bool {
        unsafe {
            let fleet_ref = self.fleet_ref();
            for i in 0..fleet_ref.len() {
                if (*(*fleet_ref.index(i)).vessel.as_ptr()).id == id {
                    (*fleet_ref.index(i)).ignore = false;

                    assert!(self.warp_ref().len() > 0);
                    self.apportion_work();
                    return true;
                }
            }
        }
        false
    }

    /// Un-park the worker the identified Vessel is assigned to
    fn notify(self: Hdl<Self>, id: usize) {
        // println!("notify vessel {id}");
        unsafe {
            // first try separately-managed threads
            let specials = self.special_ref();
            for i in 0..specials.len() {
                if (*specials.index(i)).vsl_id == id {
                    let parking = (*specials.index(i)).park.load(atomic::Ordering::Acquire);
                    if parking {
                        (*specials.index(i)).os_thr_hdl.unpark();
                    }
                    return;
                }
            }
        }

        unsafe {
            let workers = self.warp_ref();
            // find the worker
            for i in 0..workers.len() {
                let sentinel = (*workers.index(i)).sentinel;
                let mut work_item = sentinel.as_ptr();
                loop {
                    work_item = (*work_item).next;
                    if let Some(vsl) = (*work_item).vsl
                        && vsl.id() == id
                    {
                        // check its parking token
                        let parking = (*(*workers.index(i)).semaphore.as_ptr())
                            .park
                            .load(atomic::Ordering::Acquire);
                        if parking {
                            // unpark
                            (*workers.index(i))
                                .os_thr_hdl
                                .as_ref()
                                .unwrap()
                                .thread()
                                .unpark()
                        }
                        return;
                    } else if work_item == sentinel.as_ptr() {
                        break;
                    }
                }
            }
        }

        eprintln!("vessel {id} to notify not found");
    }
}

// NOTE: prefer to keep Vessel within 128 bytes (2 cache lines)

/// Runtime data for a single strand of Sail execution
#[repr(C)]
pub struct Vessel {
    /// unique identifier
    id: usize,

    /// TODO: how many know
    refct: usize,

    /// tracker for all extant vessels
    weft: Hdl<Weft>,

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

impl Vessel {
    /// Creates new Vessel, adding it to a Weft
    pub fn summon(weft: &mut WeftOwner, s_init_size: u16, r_zone_ord: u8) -> Hdl<Self> {
        let it = Box::into_raw({
            let mut uninit = Box::<Self>::new_uninit();
            let ptr = uninit.as_mut_ptr();

            unsafe {
                (&raw mut (*ptr).id).write(weft.hdl().get_vsl_id());
                (&raw mut (*ptr).weft).write(weft.hdl());

                (&raw mut (*ptr).reg).write(memmgt::Region::new(r_zone_ord));
                Hdl::new(&raw mut (*ptr).reg).init();

                (&raw mut (*ptr).qin).write(queue::Inlet::new());
                (&raw mut (*ptr).tenv).write(super::env_make(Hdl::new(&raw mut (*ptr).reg)));
                (&raw mut (*ptr).eval).write(eval::EvalStack::new(s_init_size));

                (&raw mut (*ptr).out).write(ptr::null_mut());
            }

            unsafe { uninit.assume_init() }
        });

        let vhdl = Hdl::new(it);
        vhdl.intern();

        // the result is always registered to the weft
        weft.hdl().add_vessel(vhdl);
        vhdl
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

                (&raw mut (*ptr).qin).write(queue::Inlet::new());
                (&raw mut (*ptr).tenv).write(super::env_make(Hdl::new(&raw mut (*ptr).reg)));
                (&raw mut (*ptr).eval).write(eval::EvalStack::new(
                    s_init_size.unwrap_or(eval::size(&raw mut (*self.raw()).eval)),
                ));

                (&raw mut (*ptr).out).write(ptr::null_mut());
            }

            unsafe { uninit.assume_init() }
        });

        let vhdl = Hdl::new(it);
        vhdl.intern();

        // the result is always registered to the weft
        unsafe {
            (*self.raw()).weft.add_vessel(vhdl);
        }
        vhdl
    }

    // TODO: special handle exclusively allowing `advance`?
    /// Calling thread claims this Vessel for its own management
    pub fn claim(self: Hdl<Self>, th: std::thread::Thread) -> *mut atomic::AtomicBool {
        unsafe { (*self.raw()).weft.add_special(th, self) }
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
        unsafe { (*self.raw()).weft.context() }
    }

    /// Returns handle to this Vessel's memory region
    pub fn region(self: Hdl<Self>) -> Hdl<memmgt::Region> {
        unsafe { Hdl::new(&raw mut (*self.raw()).reg) }
    }

    /// Transmits a message along this Vessel's queue
    pub fn msg_tmit(self: Hdl<Self>, source: usize, msg: SlHndl) {
        unsafe {
            // notify thread if the queue was empty
            if queue::transmit(&raw mut (*self.raw()).qin, self.region(), source, msg) {
                (*self.raw()).weft.notify(self.id());
            }
        }
    }

    /// Returns top message from this Vessel's incoming queue
    pub fn msg_recv(self: Hdl<Self>) -> (usize, Option<SlHndl>) {
        unsafe { queue::receive(&raw mut (*self.raw()).qin) }
    }

    /// Returns top-level local environment for this Vessel
    pub fn top_env(self: Hdl<Self>) -> SlHndl {
        unsafe { (*self.raw()).tenv.clone() }
    }

    /// Returns reference to relevant source-level debug data
    pub fn src_dbg_info(self: Hdl<Self>) -> Option<&'static mut debug::SrcInfo> {
        unsafe { (&raw mut (*(*self.raw()).weft.raw()).temp_debug).as_mut_unchecked() }.as_mut()
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
            eval::start(self.esp(), &raw mut (*self.raw()).out, self.top_env(), sexp);
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
            eval::start(
                self.esp(),
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
            eval::start_proc(
                self.esp(),
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

    /// Loads Sail expression for immediate evaluation
    pub fn load_direct(self: Hdl<Self>, sexp: SlHndl) {
        assert!(self.inert_p());
        assert!(
            self.region()
                .resident_p(unsafe { sexp.get_raw() as _ })
                .is_some()
        );

        unsafe {
            eval::start(self.esp(), &raw mut (*self.raw()).out, self.top_env(), sexp);
        }
    }

    /// Attempts to start evaluation running on Weft workers
    pub fn attempt_start(self: Hdl<Self>) -> bool {
        unsafe { (*self.raw()).weft.thaw((*self.raw()).id) }
    }

    /// True if the evaluator is empty of code
    pub fn inert_p(self: Hdl<Self>) -> bool {
        eval::empty_p(self.esp())
    }

    // TODO: stack is Vessel's core; move to methods on Vessel itself?
    // define Vessel in eval?

    /// Advances evaluator by one stack frame
    pub fn advance(self: Hdl<Self>) -> bool {
        eval::iter_once(self)
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

    /// Returns pointer to contained eval tracking structure
    pub fn esp(self: Hdl<Self>) -> *mut eval::EvalStack {
        unsafe { &raw mut (*self.raw()).eval }
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

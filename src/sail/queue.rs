// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/queue.rs

// Queues for communication between vessels (independent single-thread
// execution contexts) in Sail code.

// <>

// Each execution unit gets a blessed queue receiver. Any thread (for
// which the target is in scope?) can send messages / object copies
// down that channel.

use std::ptr;

use super::{core::*, memmgt, util};

#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
struct Qptr(usize);

impl Qptr {
    fn make(p: *mut QueueNode, c: u16) -> Self {
        Self(((p as usize) << 16) + c as usize)
    }
    fn get(self) -> (*mut QueueNode, u16) {
        (
            (self.0 >> 16) as *mut QueueNode,
            (self.0 & u16::MAX as usize) as u16,
        )
    }
}

struct QueueNode {
    source: usize,
    payload: Option<SlHndl>,
    next: Qptr,
}

#[repr(C)]
pub struct Inlet {
    head: Qptr,
    tail: Qptr,

    free_nodes: Qptr,
}

impl Drop for Inlet {
    fn drop(&mut self) {
        eprintln!("DROP queue Inlet");

        while receive(self as *mut Self).1.is_some() {}

        unsafe {
            let mut nodes = 1;
            while let Some(n) = node_pop(self as *mut Self) {
                drop(Box::from_raw(n.as_ptr()));
                nodes += 1;
            }

            drop(Box::from_raw(self.head.get().0));

            eprintln!("({nodes} nodes)");
        }
    }
}

impl Inlet {
    pub fn new() -> Self {
        // TODO: allocate queue nodes in inlet's region?
        let dummy_node = Box::into_raw(Box::new(QueueNode {
            source: 0,
            payload: None,
            next: Qptr::make(ptr::null_mut(), 0),
        }));

        Self {
            head: Qptr::make(dummy_node, 0),
            tail: Qptr::make(dummy_node, 0),
            free_nodes: Qptr::make(ptr::null_mut(), 0),
        }
    }
}

/// Receives a message from the given inlet's queue; returns it and
/// the source identifier it came with
pub fn receive(inlet: *mut Inlet) -> (usize, Option<SlHndl>) {
    let mut out;
    unsafe {
        let mut head;
        loop {
            head = atom_ops::load(&raw const (*inlet).head);
            let tail = atom_ops::load(&raw const (*inlet).tail);
            let next = atom_ops::load(&raw const (*head.get().0).next);

            if head == atom_ops::load(&raw const (*inlet).head) {
                if head.get().0 == tail.get().0 {
                    if next.get().0 == ptr::null_mut() {
                        return (0, None);
                    }
                    atom_ops::cxcg(
                        &raw mut (*inlet).tail,
                        tail,
                        Qptr::make(next.get().0, u16::wrapping_add(tail.get().1, 1)),
                    );
                } else {
                    out = ((*next.get().0).source, (*next.get().0).payload.take());
                    if atom_ops::cxcg(
                        &raw mut (*inlet).head,
                        head,
                        Qptr::make(next.get().0, u16::wrapping_add(head.get().1, 1)),
                    )
                    .1
                    {
                        break;
                    }
                }
            }
        }
        node_push(inlet, head.get().0);
        // drop(Box::from_raw(head.get().0));
    }

    debug_assert!(out.1.is_some());
    out

    // Core queue algorithm from (Michael & Scott, 1998)
}

/// Transmits a message to the given inlet's queue; may return true if
/// the queue was empty prior to sending
pub fn transmit(
    inlet: *mut Inlet,
    reg: util::Hdl<memmgt::Region>,
    source: usize,
    msg: SlHndl,
) -> bool {
    // create a queue node which points at msg in the new region
    let node = {
        let payload = {
            let new_loc = super::structure_copy(reg, msg.clone());
            Some(unsafe { SlHndl::from_raw_unchecked(new_loc) })
        };
        let next = Qptr::make(ptr::null_mut(), 0);

        match node_pop(inlet) {
            Some(n) => {
                let ptr = n.as_ptr();
                unsafe {
                    (*ptr).source = source;
                    (*ptr).payload = payload;
                    (*ptr).next = next;
                }
                ptr
            }
            None => Box::into_raw(Box::new(QueueNode {
                source,
                payload,
                next,
            })),
        }
    };

    // proceed to add the freshly instantiated node to the queue
    unsafe {
        // inlet head not accessed by core algorithm
        let mut tail;
        loop {
            tail = atom_ops::load(&raw const (*inlet).tail);
            let next = atom_ops::load(&raw const (*tail.get().0).next);

            if tail == atom_ops::load(&raw const (*inlet).tail) {
                if next.get().0.is_null() {
                    if atom_ops::cxcg(
                        &raw mut (*tail.get().0).next,
                        next,
                        Qptr::make(node, u16::wrapping_add(next.get().1, 1)),
                    )
                    .1
                    {
                        break;
                    }
                } else {
                    atom_ops::cxcg(
                        &raw mut (*inlet).tail,
                        tail,
                        Qptr::make(next.get().0, u16::wrapping_add(tail.get().1, 1)),
                    );
                }
            }
        }

        let head = atom_ops::load(&raw const (*inlet).head);
        let tail = atom_ops::cxcg(
            &raw mut (*inlet).tail,
            tail,
            Qptr::make(node, u16::wrapping_add(tail.get().1, 1)),
        )
        .0;

        head == tail
    }

    // Core queue algorithm from (Michael & Scott, 1998)
}

fn node_push(inlet: *mut Inlet, node: *mut QueueNode) {
    unsafe {
        (*node).next = Qptr::make(ptr::null_mut(), 0);
        loop {
            let top = atom_ops::load(&raw const (*inlet).free_nodes);
            (*node).next = Qptr::make(top.get().0, 0);
            if atom_ops::cxcg(
                &raw mut (*inlet).free_nodes,
                top,
                Qptr::make(node, u16::wrapping_add(top.get().1, 1)),
            )
            .1
            {
                break;
            }
        }
    }
}

fn node_pop(inlet: *mut Inlet) -> Option<ptr::NonNull<QueueNode>> {
    unsafe {
        let mut top;
        loop {
            top = atom_ops::load(&raw const (*inlet).free_nodes);
            if top.get().0.is_null() {
                return None;
            }
            if atom_ops::cxcg(
                &raw mut (*inlet).free_nodes,
                top,
                Qptr::make(
                    (*top.get().0).next.get().0,
                    u16::wrapping_add(top.get().1, 1),
                ),
            )
            .1
            {
                break;
            }
        }
        Some(ptr::NonNull::new_unchecked(top.get().0))
    }
}

mod atom_ops {
    use super::Qptr;

    use std::intrinsics::AtomicOrdering as AO;

    #[inline(always)]
    pub unsafe fn load(from: *const Qptr) -> Qptr {
        unsafe {
            Qptr(std::intrinsics::atomic_load::<_, { AO::Acquire }, false>(
                from as *const usize,
            ))
        }
    }
    #[inline(always)]
    pub unsafe fn cxcg(tgt: *mut Qptr, old: Qptr, new: Qptr) -> (Qptr, bool) {
        unsafe {
            let out = std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                tgt as *mut usize,
                std::mem::transmute::<Qptr, usize>(old),
                std::mem::transmute::<Qptr, usize>(new),
            );
            (Qptr(out.0), out.1)
        }
    }
}

#[cfg(test)]
mod inlet_tests {
    use super::super::{debug, parser};
    use super::*;

    #[repr(C)]
    struct TestQueueData {
        reg: memmgt::Region,
        qin: Inlet,
    }

    const TQL: std::alloc::Layout = std::alloc::Layout::new::<TestQueueData>();

    struct SendHdl(ptr::NonNull<TestQueueData>);

    unsafe impl Send for SendHdl {}

    impl SendHdl {
        fn tx(&self, source: usize, msg: SlHndl) {
            unsafe {
                transmit(
                    &raw mut (*self.0.as_ptr()).qin,
                    util::Hdl::new(&raw mut (*self.0.as_ptr()).reg),
                    source,
                    msg,
                );
            }
        }
    }

    fn test_queue(zo: u8) -> *mut TestQueueData {
        unsafe {
            let ptr = std::alloc::alloc(TQL) as *mut TestQueueData;
            (&raw mut (*ptr).reg).write(memmgt::Region::new(zo));
            util::Hdl::new(&raw mut (*ptr).reg).init();
            (&raw mut (*ptr).qin).write(Inlet::new());

            ptr
        }
    }

    fn tq_destroy(ptr: *mut TestQueueData) {
        unsafe {
            ptr.drop_in_place();
            std::alloc::dealloc(ptr as _, TQL);
        }
    }

    fn test_hdl(qd: *mut TestQueueData) -> SendHdl {
        SendHdl(ptr::NonNull::new(qd).unwrap())
    }

    #[test]
    fn basic() {
        let mut region = memmgt::Region::new(8);
        let tx_reg = util::Hdl::new(&raw mut region);

        let rx_in = test_queue(8);
        let tx_hdl = test_hdl(rx_in);

        let qin = unsafe { &raw mut (*rx_in).qin };

        assert!(receive(qin).1.is_none());

        tx_hdl.tx(1, i64_init(tx_reg, 7));
        tx_hdl.tx(1, i64_init(tx_reg, 14));

        assert_eq!(i64_get(receive(qin).1.unwrap()), 7);
        assert_eq!(i64_get(receive(qin).1.unwrap()), 14);

        tx_hdl.tx(1, i64_init(tx_reg, 21));

        assert_eq!(i64_get(receive(qin).1.unwrap()), 21);

        assert!(receive(qin).1.is_none());
        assert!(receive(qin).1.is_none());

        tx_hdl.tx(1, i64_init(tx_reg, 28));
        tx_hdl.tx(1, i64_init(tx_reg, 35));
        tx_hdl.tx(1, i64_init(tx_reg, 42));

        assert_eq!(i64_get(receive(qin).1.unwrap()), 28);
        assert_eq!(i64_get(receive(qin).1.unwrap()), 35);
        assert_eq!(i64_get(receive(qin).1.unwrap()), 42);

        assert!(receive(qin).1.is_none());

        tq_destroy(rx_in);
    }

    #[test]
    fn layers() {
        let mut table = super::super::Stab::new(51);
        let mut region = memmgt::Region::new(8);

        // TODO: macro: hdl!()
        let tref = util::Hdl::new(&raw mut table);
        let tx_reg = util::Hdl::new(&raw mut region);
        let rx_in = test_queue(8);

        let exp = String::from("(+ (() 42 (e) #T) #F 2.1 e)");
        let ogv = parser::parse_text(tx_reg, tref, exp.clone().into_bytes(), None).unwrap();

        test_hdl(rx_in).tx(1, ogv);

        let cpv = receive(unsafe { &raw mut (*rx_in).qin }).1.unwrap();
        let res = debug::bundle(tref, cpv).to_string();

        assert_eq!(exp, res);

        tq_destroy(rx_in);
    }

    #[test]
    fn multi() {
        const THCT: u32 = 4; // 7
        const SPAN: u32 = 500; // 5000

        fn send(id: usize, start: i64, extent: i64, tgt: SendHdl) {
            // NOTE: zone order can be calculated...
            let mut region = memmgt::Region::new(13);
            let tsr = util::Hdl::new(&raw mut region);

            for i in start..start + extent {
                tgt.tx(id, i64_init(tsr, i));
                // println!("{id} transmitted {i}");
            }
            println!("{id} done sending");
        }

        let rx_in = test_queue(17);

        let mut th = vec![];
        let mut acc = 0;
        for tid in 0..THCT {
            let hdl = test_hdl(rx_in);

            th.push(std::thread::spawn(move || {
                send(tid as _, acc, SPAN as _, hdl)
            }));
            acc += SPAN as i64;
        }

        let mut sheet = [false; (THCT * SPAN) as usize];
        let mut recvd = 0;

        while recvd < THCT * SPAN {
            match receive(unsafe { (&raw mut (*rx_in).qin) }).1 {
                Some(h) => {
                    recvd += 1;
                    let val = i64_get(h);
                    sheet[val as usize] |= true;

                    println!("received {val}; {recvd}/{}", THCT * SPAN);
                }
                None => (),
            }
        }

        for j in th {
            j.join().unwrap();
        }

        for i in 0..sheet.len() {
            assert!(sheet[i]);
        }

        tq_destroy(rx_in);
    }
}

// With thanks, core queue algorithm based on:

// Michael, M. M. & Scott, M. L. (1998). Non-Blocking Algorithms and
//      Preemption-Safe Locking on Multiprogrammed Shared Memory
//      Multiprocessors. Journal of Parallel and Distributed Computing,
//      51, 1-26.

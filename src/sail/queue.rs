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

use super::{core::*, memmgt};

#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct Qptr(usize);

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

#[repr(C)]
pub struct Inlet {
    tail: Qptr,

    // TODO: keep atomic reference count of senders
    refct: usize,
    // TODO: maintain a stack of existing nodes for reuse
    nodes: *mut QueueNode,

    head: Qptr,
}

impl Drop for Inlet {
    fn drop(&mut self) {
        while self.receive().1.is_some() {}

        unsafe {
            let _dummy_node = Box::from_raw(self.head.get().0);
        }
    }
}

struct QueueNode {
    source: usize,
    payload: Option<SlHndl>,
    next: Qptr,
}

impl Inlet {
    pub fn new(_target_region: *mut memmgt::Region) -> Self {
        // TODO: allocate queue nodes in inlet's region?
        let dummy_node = Box::into_raw(Box::new(QueueNode {
            source: 0,
            payload: None,
            next: Qptr::make(ptr::null_mut(), 0),
        }));

        Self {
            // region: target_region,
            tail: Qptr::make(dummy_node, 0),
            refct: 0,
            nodes: ptr::null_mut(),
            head: Qptr::make(dummy_node, 0),
        }
    }

    // pub fn transmit(&mut self, from: usize, msg: SlHndl) {
    //     tx_impl(self.region, &mut self.tail, from, msg)
    // }

    pub fn receive(&mut self) -> (usize, Option<SlHndl>) {
        let mut out;
        unsafe {
            let mut head;
            loop {
                head = atom_ops::load(&self.head);
                let tail = atom_ops::load(&self.tail);
                let next = atom_ops::load(&(*head.get().0).next);

                if head == atom_ops::load(&self.head) {
                    if head.get().0 == tail.get().0 {
                        if next.get().0 == ptr::null_mut() {
                            return (0, None);
                        }
                        atom_ops::cxcg(
                            &mut self.tail,
                            tail,
                            Qptr::make(next.get().0, u16::wrapping_add(tail.get().1, 1)),
                        );
                    } else {
                        out = ((*next.get().0).source, (*next.get().0).payload.take());
                        if atom_ops::cxcg(
                            &mut self.head,
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
            let _node_to_drop = Box::from_raw(head.get().0);
        }

        debug_assert!(out.1.is_some());
        out

        // Core queue algorithm from (Michael & Scott, 1998)
    }
}

/// Handle that only permits sends
#[derive(Clone)]
#[repr(transparent)]
pub struct InletHdl(*mut _SendFields);

unsafe impl Send for InletHdl {}

/// Window into part of the Vessel struct
#[repr(C)]
pub struct _SendFields {
    reg: memmgt::Region,
    tail: Qptr,
    refct: usize,
    nodes: *mut QueueNode,
}

impl InletHdl {
    pub fn new(tgt: *mut _SendFields) -> Self {
        Self(tgt)
    }

    pub fn transmit(&self, from: usize, msg: SlHndl) {
        tx_impl(
            unsafe { (&raw mut (*self.0).reg) },
            unsafe { (&raw mut (*self.0).tail).as_mut_unchecked() },
            from,
            msg,
        )
    }
}

fn tx_impl(reg: *mut memmgt::Region, q_tail: &mut Qptr, from: usize, msg: SlHndl) {
    let new_loc = super::structure_copy(reg, msg.clone());

    // create a queue node which points at msg in the new region
    let fresh_node = Box::into_raw(Box::new(QueueNode {
        source: from,
        payload: Some(unsafe { SlHndl::from_raw_unchecked(new_loc) }),
        next: Qptr::make(ptr::null_mut(), 0),
    }));

    // proceed to add the freshly instantiated node to the queue
    unsafe {
        let mut tail;
        loop {
            tail = atom_ops::load(q_tail);
            let next = atom_ops::load(&((*tail.get().0).next));

            if tail == atom_ops::load(q_tail) {
                if next.get().0.is_null() {
                    if atom_ops::cxcg(
                        &mut (*tail.get().0).next,
                        next,
                        Qptr::make(fresh_node, u16::wrapping_add(next.get().1, 1)),
                    )
                    .1
                    {
                        break;
                    }
                } else {
                    atom_ops::cxcg(
                        q_tail,
                        tail,
                        Qptr::make(next.get().0, u16::wrapping_add(tail.get().1, 1)),
                    );
                }
            }
        }

        atom_ops::cxcg(
            q_tail,
            tail,
            Qptr::make(fresh_node, u16::wrapping_add(tail.get().1, 1)),
        );
    }

    // Core queue algorithm from (Michael & Scott, 1998)
}

// TODO: use broadly
#[derive(Clone, Copy)]
pub struct RegHdl(pub *mut memmgt::Region);
unsafe impl Send for RegHdl {}

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
    use std::pin::Pin;

    use super::*;

    #[repr(C)]
    struct TestQueueData {
        reg: memmgt::Region,
        qin: Inlet,
    }

    fn test_queue(zs: u32) -> Pin<Box<TestQueueData>> {
        let it = {
            let mut uninit = Box::<TestQueueData>::new_uninit();
            let ptr = uninit.as_mut_ptr();

            unsafe {
                (&raw mut (*ptr).reg).write(memmgt::Region::new(zs));
                (*ptr).reg.init();
                (&raw mut (*ptr).qin).write(Inlet::new(&raw mut (*ptr).reg));
            }

            Box::into_pin(unsafe { uninit.assume_init() })
        };

        it
    }

    fn test_hdl(qd: &mut Pin<Box<TestQueueData>>) -> InletHdl {
        InletHdl(qd.as_mut().get_mut() as *mut _ as *mut _SendFields)
    }

    #[test]
    fn basic() {
        let tx_reg = memmgt::Region::acq(100);

        let mut rx_in = test_queue(100);
        let tx_hdl = test_hdl(&mut rx_in);

        assert!(rx_in.qin.receive().1.is_none());

        tx_hdl.transmit(1, i64_init(tx_reg, 7));
        tx_hdl.transmit(1, i64_init(tx_reg, 14));

        assert_eq!(i64_get(rx_in.qin.receive().1.unwrap()), 7);
        assert_eq!(i64_get(rx_in.qin.receive().1.unwrap()), 14);

        tx_hdl.transmit(1, i64_init(tx_reg, 21));

        assert_eq!(i64_get(rx_in.qin.receive().1.unwrap()), 21);

        assert!(rx_in.qin.receive().1.is_none());
        assert!(rx_in.qin.receive().1.is_none());

        tx_hdl.transmit(1, i64_init(tx_reg, 28));
        tx_hdl.transmit(1, i64_init(tx_reg, 35));
        tx_hdl.transmit(1, i64_init(tx_reg, 42));

        assert_eq!(i64_get(rx_in.qin.receive().1.unwrap()), 28);
        assert_eq!(i64_get(rx_in.qin.receive().1.unwrap()), 35);
        assert_eq!(i64_get(rx_in.qin.receive().1.unwrap()), 42);

        assert!(rx_in.qin.receive().1.is_none());
    }

    #[test]
    fn layers() {
        let mut tbl = super::super::Stab::new(51);

        let tx_reg = memmgt::Region::acq(100);

        let mut rx_in = test_queue(100);

        let exp = String::from("(+ (() 42 (e) #T) #F 2.1 e)");
        let ogv = parser::parse_text(tx_reg, tref, exp.clone().into_bytes(), None).unwrap();

        test_hdl(&mut rx_in).transmit(1, ogv);

        let cpv = rx_in.qin.receive().1.unwrap();

        let cpv = unsafe { &raw mut (*rx_in).qin }.receive().1.unwrap();
        let res = debug::bundle(tref, cpv).to_string();

        assert_eq!(exp, res);
    }

    #[test]
    fn multi() {
        const THCT: u32 = 7;
        const SPAN: u32 = 5000;

        fn send(id: usize, start: i64, extent: i64, src_reg: RegHdl, tgt: InletHdl) {
            for i in start..start + extent {
                tgt.transmit(id, i64_init(src_reg.0, i));
                println!("{id} transmitted {i}");
            }
        }

        let mut rx_in = test_queue(SPAN * 10);

        let mut th = vec![];
        let mut acc = 0;
        for tid in 0..THCT {
            let tsr = RegHdl(memmgt::Region::acq(SPAN));

            let hdl = test_hdl(&mut rx_in);

            th.push(std::thread::spawn(move || {
                send(tid as _, acc, SPAN as _, tsr, hdl)
            }));
            acc += SPAN as i64;
        }

        let mut sheet = [false; (THCT * SPAN) as usize];
        let mut recvd = 0;

        while recvd < THCT * SPAN {
            match rx_in.qin.receive().1 {
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
    }
}

// With thanks, core queue algorithm based on:

// Michael, M. M. & Scott, M. L. (1998). Non-Blocking Algorithms and
//      Preemption-Safe Locking on Multiprogrammed Shared Memory
//      Multiprocessors. Journal of Parallel and Distributed Computing,
//      51, 1-26.

// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/queue.rs

// Queues for communication between threads in Sail code.

// <>

// Each execution thread gets a blessed queue receiver. Any thread
// (for which the target is in scope?) can send messages / object
// copies down that channel.

use std::ptr;

use super::{core::*, memmgt};

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

// TODO: queue handle that only permits sends
pub struct SendTgt {
    tgt: *mut Inlet,
}

pub struct Inlet {
    region: *mut memmgt::Region,
    // could be a linked list with nodes pointing to Sail objects
    // - same layout as current
    // - no resize handling
    head: Qptr,
    tail: Qptr,
    // TODO: maintain a stack of existing nodes for reuse
    // TODO: keep atomic reference count of senders

    // or could be a ring buffer with cells pointing to the same
    // - certainly faster in most cases
    // - will need to deal with resize
    // buffer: *mut SlHndl,
    // head: usize,
    // tail: usize,
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
    pub fn new(target_region: *mut memmgt::Region) -> Self {
        let dummy_node = Box::into_raw(Box::new(QueueNode {
            source: 0,
            payload: None,
            next: Qptr::make(ptr::null_mut(), 0),
        }));

        Self {
            region: target_region,

            head: Qptr::make(dummy_node, 0),
            tail: Qptr::make(dummy_node, 0),
        }
    }

    pub fn transmit(&mut self, from: usize, msg: SlHndl) {
        let new_loc = super::structure_copy(self.region, msg.clone());

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
                tail = atom_ops::load(&self.tail);
                let next = atom_ops::load(&((*tail.get().0).next));

                if tail == atom_ops::load(&self.tail) {
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
                            &mut self.tail,
                            tail,
                            Qptr::make(next.get().0, u16::wrapping_add(tail.get().1, 1)),
                        );
                    }
                }
            }

            atom_ops::cxcg(
                &mut self.tail,
                tail,
                Qptr::make(fresh_node, u16::wrapping_add(tail.get().1, 1)),
            );
        }

        // Core queue algorithm from (Michael & Scott, 1998)
    }

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

mod atom_ops {
    use super::Qptr;

    #[inline(always)]
    pub unsafe fn load(from: *const Qptr) -> Qptr {
        Qptr(std::intrinsics::atomic_load_acquire(from as *const usize))
    }
    #[inline(always)]
    pub unsafe fn cxcg(tgt: *mut Qptr, old: Qptr, new: Qptr) -> (Qptr, bool) {
        let out = std::intrinsics::atomic_cxchg_acqrel_acquire(
            tgt as *mut usize,
            std::mem::transmute::<Qptr, usize>(old),
            std::mem::transmute::<Qptr, usize>(new),
        );
        (Qptr(out.0), out.1)
    }
}

#[cfg(test)]
mod inlet_tests {
    use super::*;

    #[test]
    fn basic() {
        let tx_reg = memmgt::Region::acq(100);
        let rx_reg = memmgt::Region::acq(100);

        let mut rx_inlet = Inlet::new(rx_reg);

        assert!(rx_inlet.receive().1.is_none());

        rx_inlet.transmit(1, i64_init(tx_reg, 7));
        rx_inlet.transmit(1, i64_init(tx_reg, 14));

        assert_eq!(i64_get(rx_inlet.receive().1.unwrap()), 7);
        assert_eq!(i64_get(rx_inlet.receive().1.unwrap()), 14);

        rx_inlet.transmit(1, i64_init(tx_reg, 21));

        assert_eq!(i64_get(rx_inlet.receive().1.unwrap()), 21);

        assert!(rx_inlet.receive().1.is_none());
        assert!(rx_inlet.receive().1.is_none());

        rx_inlet.transmit(1, i64_init(tx_reg, 28));
        rx_inlet.transmit(1, i64_init(tx_reg, 35));
        rx_inlet.transmit(1, i64_init(tx_reg, 42));

        assert_eq!(i64_get(rx_inlet.receive().1.unwrap()), 28);
        assert_eq!(i64_get(rx_inlet.receive().1.unwrap()), 35);
        assert_eq!(i64_get(rx_inlet.receive().1.unwrap()), 42);

        assert!(rx_inlet.receive().1.is_none());
    }

    #[test]
    fn layers() {
        let mut tbl = super::super::Stab::new(51);

        let tx_reg = memmgt::Region::acq(100);
        let rx_reg = memmgt::Region::acq(100);

        let mut rx_inlet = Inlet::new(rx_reg);

        let exp = String::from("(+ (() 42 (e) #T) #F 2.1 e)");

        let ogv = super::super::parser::parse(tx_reg, &mut tbl, &exp, false).unwrap();

        rx_inlet.transmit(1, ogv);

        let cpv = rx_inlet.receive().1.unwrap();

        let res = super::super::context(&tbl, cpv).to_string();

        assert_eq!(exp, res);
    }

    #[test]
    fn multi() {
        const THCT: u32 = 7;
        const SPAN: u32 = 5000;

        fn send(id: usize, start: i64, extent: i64, tx_reg: usize, tgt: usize) {
            let target = unsafe { std::mem::transmute::<usize, &mut Inlet>(tgt) };

            for i in start..start + extent {
                target.transmit(id, i64_init(tx_reg as *mut memmgt::Region, i));
            }
        }

        let rx_reg = memmgt::Region::acq(SPAN * 10);
        let mut inlet = Inlet::new(rx_reg);
        let hdl = unsafe { std::mem::transmute::<&mut Inlet, usize>(&mut inlet) };

        let mut th = vec![];
        let mut acc = 0;
        for tid in 0..THCT {
            let tsr = memmgt::Region::acq(SPAN) as usize;
            th.push(std::thread::spawn(move || {
                send(tid as _, acc, SPAN as _, tsr, hdl)
            }));
            acc += SPAN as i64;
        }

        let mut sheet = [false; (THCT * SPAN) as usize];
        let mut recvd = 0;

        while recvd < THCT * SPAN {
            match inlet.receive().1 {
                Some(h) => {
                    recvd += 1;
                    let val = i64_get(h);
                    sheet[val as usize] |= true;

                    // println!("received {val}; {recvd}/{}", THCT * SPAN);
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

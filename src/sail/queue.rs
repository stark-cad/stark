// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License. See the top level LICENSE file for the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/sail/queue.rs

// Queues for communication between threads in Sail code.

// <>

use super::{core::*, memmgt, SlHead};

// TODO: global identifiers for queues?

// TODO: the receiver could also be a B16, and keep a pointer to its
// sender to be sure of emptiness?

// TODO: ensure that out of order message arrival does not persist

pub fn queue_create(
    tx_region: *mut memmgt::Region,
    rx_region: *mut memmgt::Region,
) -> (*mut SlHead, *mut SlHead) {
    unsafe {
        let sender = memmgt::alloc(tx_region, 16, Cfg::B16Other as u8);
        let receiver = memmgt::alloc(rx_region, 8, Cfg::B8Other as u8);

        super::set_self_type(sender, super::T_QUEUE_TX.0);
        super::set_self_type(receiver, super::T_QUEUE_RX.0);

        write_field_unchecked(sender, 8, rx_region as u64);

        write_field_unchecked(sender, 0, receiver);
        write_field_unchecked(receiver, 0, sender);

        (sender, receiver)
    }
}

pub fn queue_tx(loc: *mut SlHead, item: *mut SlHead) {
    assert_eq!(super::get_self_type(loc), super::T_QUEUE_TX.0);
    assert_eq!(get_base_size(loc), BaseSize::B16);

    unsafe {
        // create new list element containing the item
        // TODO: must change to permit copying arbitrary values
        let elt = core_copy_val(
            read_field_unchecked::<u64>(loc, 8) as *mut memmgt::Region,
            item,
        );

        loop {
            // point the element at the sender
            set_next_list_elt(elt, loc);

            // point the current queue head at the element
            let head = read_field_atomic_unchecked(loc, 0);

            if super::get_self_type(head) == super::T_QUEUE_RX.0 {
                if !write_field_cmpxcg_unchecked(head, 0, loc, elt) {
                    continue;
                }
            } else {
                set_next_list_elt(head, elt);
            }

            // point the sender at the element
            if !write_field_cmpxcg_unchecked(loc, 0, head, elt) {
                continue;
            }

            return;
        }
    }
}

pub fn queue_rx(loc: *mut SlHead) -> *mut SlHead {
    assert_eq!(super::get_self_type(loc), super::T_QUEUE_RX.0);
    assert_eq!(get_base_size(loc), BaseSize::B8);

    unsafe {
        loop {
            // get the item pointed to by the receiver
            let item = read_field_atomic_unchecked(loc, 0);

            if super::get_self_type(item) == super::T_QUEUE_TX.0 {
                return nil();
            } else {
                let next = get_next_list_elt(item);

                // point the receiver at the item pointed to
                if !write_field_cmpxcg_unchecked(loc, 0, item, next) {
                    continue;
                }

                if super::get_self_type(next) == super::T_QUEUE_TX.0 {
                    if !write_field_cmpxcg_unchecked(next, 0, item, loc) {
                        continue;
                    }
                }

                // return the received item
                return item;
            }
        }
    }
}

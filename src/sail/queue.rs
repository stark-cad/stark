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

// TODO: take action to avoid the "ABA problem"

// TODO: repurpose refcount as ABA problem avoidance count while in
// queue?

// fn qptr_make(ptr: *mut SlHead, ct: u16) -> u64 {
//    ptr as u64 + ((ct as u64) << 48)
// }

// fn qptr_get(qptr: u64) -> (*mut SlHead, u16) {
//     ((qptr & 0x0000FFFFFFFFFFFF) as *mut SlHead, (qptr >> 48) as u16)
// }

pub fn queue_create(
    tx_region: *mut memmgt::Region,
    rx_region: *mut memmgt::Region,
) -> (*mut SlHead, *mut SlHead) {
    unsafe {
        let sender = memmgt::alloc(tx_region, 16, Cfg::B16Other as u8);
        let receiver = memmgt::alloc(rx_region, 16, Cfg::B16Other as u8);

        super::set_self_type(sender, super::T_QUEUE_TX.0);
        super::set_self_type(receiver, super::T_QUEUE_RX.0);

        write_field_unchecked(sender, 0, receiver);
        write_field_unchecked(sender, 8, rx_region as u64);

        write_field_unchecked(receiver, 0, nil());
        write_field_unchecked(receiver, 8, sender);

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

        let mut tail;
        loop {
            // get the current list tail (sender's perspective)
            tail = read_field_atomic_unchecked(loc, 0);

            // get pointer to the tail's next element
            let (is_head, next) = if super::get_self_type(tail) == super::T_QUEUE_RX.0 {
                (true, read_field_atomic_unchecked(tail, 0))
            } else {
                (false, get_next_list_elt(tail))
            };

            if tail == read_field_atomic_unchecked(loc, 0) {
                if nil_p(next) {
                    // if the next element is nil, add the new element at the tail
                    if if is_head {
                        write_field_cmpxcg_unchecked(tail, 0, next, elt)
                    } else {
                        set_next_list_elt_cmpxcg(tail, next, elt)
                    } {
                        // end loop if successful
                        break;
                    }
                } else {
                    // if next element not nil, advance tail pointer towards the tail
                    write_field_cmpxcg_unchecked(loc, 0, tail, next);
                }
            }
        }
        // attempt to change the tail pointer to the new node
        write_field_cmpxcg_unchecked(loc, 0, tail, elt);
    }
}

pub fn queue_rx(loc: *mut SlHead) -> *mut SlHead {
    assert_eq!(super::get_self_type(loc), super::T_QUEUE_RX.0);
    assert_eq!(get_base_size(loc), BaseSize::B16);

    unsafe {
        loop {
            // get the head of the queue list
            let head = read_field_atomic_unchecked(loc, 0);

            // get the tail of the queue list
            let sender = read_field_unchecked(loc, 8);
            let tail = read_field_atomic_unchecked(sender, 0);

            if head == read_field_atomic_unchecked(loc, 0) {
                if tail == loc {
                    // if this is the list tail and the head is nil, the queue is empty
                    if nil_p(head) {
                        return nil();
                    }
                    // if the head isn't nil, shift the tail down the queue
                    write_field_cmpxcg_unchecked(sender, 0, tail, head);
                } else {
                    let next = get_next_list_elt(head);

                    // if next element up is not null, just attempt to advance to the next node
                    // otherwise, try to make the receiver the list tail, then attempt to advance
                    if (!nil_p(next) || write_field_cmpxcg_unchecked(sender, 0, tail, loc))
                        && write_field_cmpxcg_unchecked(loc, 0, head, next)
                    {
                        set_next_list_elt(head, nil());
                        return head;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn q_one_reg() {
        unsafe {
            let region = memmgt::acquire_mem_region(100);
            let (send, recv) = queue_create(region, region);

            let item = bool_init(region, true);

            queue_tx(send, item);

            let out = queue_rx(recv);

            assert_eq!(bool_get(out), true)
        }
    }

    #[test]
    fn q_two_reg() {
        unsafe {
            let tx_reg = memmgt::acquire_mem_region(100);
            let rx_reg = memmgt::acquire_mem_region(100);

            let (send, recv) = queue_create(tx_reg, rx_reg);

            queue_tx(send, i64_init(tx_reg, 7));
            queue_tx(send, i64_init(tx_reg, 14));

            assert_eq!(i64_get(queue_rx(recv)), 7);
            assert_eq!(i64_get(queue_rx(recv)), 14);

            queue_tx(send, i64_init(tx_reg, 21));

            assert_eq!(i64_get(queue_rx(recv)), 21);

            queue_tx(send, i64_init(tx_reg, 28));
            queue_tx(send, i64_init(tx_reg, 35));

            assert_eq!(i64_get(queue_rx(recv)), 28);
            assert_eq!(i64_get(queue_rx(recv)), 35);

            queue_tx(send, i64_init(tx_reg, 42));

            assert_eq!(i64_get(queue_rx(recv)), 42);
        }
    }
}

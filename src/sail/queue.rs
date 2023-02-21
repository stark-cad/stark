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

/// Creates a queue sender and receiver as a linked pair
pub fn queue_create(
    tx_region: *mut memmgt::Region,
    rx_region: *mut memmgt::Region,
) -> (*mut SlHead, *mut SlHead) {
    unsafe {
        let sender = memmgt::alloc(tx_region, 16, super::T_QUEUE_TX_ID.0);
        let receiver = memmgt::alloc(rx_region, 16, super::T_QUEUE_RX_ID.0);

        // will not work
        // super::set_self_type(sender, super::T_QUEUE_TX.0);
        // super::set_self_type(receiver, super::T_QUEUE_RX.0);

        write_ptr_unsafe_unchecked(sender, 0, receiver);
        write_field_unchecked(sender, 8, rx_region as u64);

        write_ptr_unsafe_unchecked(receiver, 0, nil());
        write_ptr_unsafe_unchecked(receiver, 8, sender);

        inc_refc(receiver);
        inc_refc(sender);

        (sender, receiver)
    }
}

/// Transmits a copy of the given Sail object along the queue
pub fn queue_tx(env: *mut SlHead, loc: *mut SlHead, item: *mut SlHead) {
    assert_eq!(get_type_id(loc), super::T_QUEUE_TX_ID.0);
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
            tail = read_ptr_atomic(loc, 0);

            // TODO: assign type IDs to all types with object representations

            // get pointer to the tail's next element
            let (is_head, next) = if type_fld_p(tail) && get_type_id(tail) == super::T_QUEUE_RX_ID.0
            {
                (true, read_ptr_atomic(tail, 0))
            } else {
                (false, get_next_list_elt(tail))
            };

            if tail == read_ptr_atomic(loc, 0) {
                if nil_p(next) {
                    // if the next element is nil, add the new element at the tail
                    if if is_head {
                        write_ptr_cmpxcg(env, tail, 0, next, elt)
                    } else {
                        set_next_list_elt_cmpxcg(env, tail, next, elt)
                    } {
                        // end loop if successful
                        break;
                    }
                } else {
                    // if next element not nil, advance tail pointer towards the tail
                    write_ptr_cmpxcg(env, loc, 0, tail, next);
                }
            }
        }
        // attempt to change the tail pointer to the new node
        write_ptr_cmpxcg(env, loc, 0, tail, elt);
    }
}

/// Receives and returns the object at the head of the queue
pub fn queue_rx(env: *mut SlHead, loc: *mut SlHead) -> *mut SlHead {
    assert_eq!(get_type_id(loc), super::T_QUEUE_RX_ID.0);
    assert_eq!(get_base_size(loc), BaseSize::B16);

    unsafe {
        loop {
            // get the head of the queue list
            let head = read_ptr_atomic(loc, 0);

            // get the tail of the queue list
            let sender = read_ptr(loc, 8);
            let tail = read_ptr_atomic(sender, 0);

            if head == read_ptr_atomic(loc, 0) {
                if tail == loc {
                    // if this is the list tail and the head is nil, the queue is empty
                    if nil_p(head) {
                        return nil();
                    }
                    // if the head isn't nil, shift the tail down the queue
                    write_ptr_cmpxcg(env, sender, 0, tail, head);
                } else {
                    // TODO: this handler needs to work on windows
                    if nil_p(head) {
                        log::debug!("a queue head was nil");
                        write_ptr_cmpxcg(env, loc, 0, head, read_ptr_atomic(sender, 0));
                        continue;
                    }

                    let next = get_next_list_elt(head);

                    // if next element up is not nil, just attempt to advance to the next node
                    // otherwise, try to make the receiver the list tail, then attempt to advance
                    if (!nil_p(next) || write_ptr_cmpxcg(env, sender, 0, tail, loc))
                        && write_ptr_cmpxcg(env, loc, 0, head, next)
                    {
                        set_next_list_elt(env, head, nil());
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

            queue_tx(nil(), send, item);

            let out = queue_rx(nil(), recv);

            assert_eq!(bool_get(out), true)
        }
    }

    #[test]
    fn q_two_reg() {
        unsafe {
            let tx_reg = memmgt::acquire_mem_region(100);
            let rx_reg = memmgt::acquire_mem_region(100);

            let (send, recv) = queue_create(tx_reg, rx_reg);

            queue_tx(nil(), send, i64_init(tx_reg, 7));
            queue_tx(nil(), send, i64_init(tx_reg, 14));

            assert_eq!(i64_get(queue_rx(nil(), recv)), 7);
            assert_eq!(i64_get(queue_rx(nil(), recv)), 14);

            queue_tx(nil(), send, i64_init(tx_reg, 21));

            assert_eq!(i64_get(queue_rx(nil(), recv)), 21);

            queue_tx(nil(), send, i64_init(tx_reg, 28));
            queue_tx(nil(), send, i64_init(tx_reg, 35));

            assert_eq!(i64_get(queue_rx(nil(), recv)), 28);
            assert_eq!(i64_get(queue_rx(nil(), recv)), 35);

            queue_tx(nil(), send, i64_init(tx_reg, 42));

            assert_eq!(i64_get(queue_rx(nil(), recv)), 42);
        }
    }
}

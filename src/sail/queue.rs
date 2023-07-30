// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License. See the top level LICENSE file for the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/sail/queue.rs

// Queues for communication between threads in Sail code.

// <>

use super::{core::*, memmgt};

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
) -> (SlHndl, SlHndl) {
    unsafe {
        let sender =
            SlHndl::from_raw_unchecked(memmgt::alloc(tx_region, 16, super::T_QUEUE_TX_ID.0));
        let receiver =
            SlHndl::from_raw_unchecked(memmgt::alloc(rx_region, 16, super::T_QUEUE_RX_ID.0));

        inc_refc(receiver.get_raw());
        inc_refc(sender.get_raw());

        write_ptr_unsafe_unchecked(sender.clone(), 0, receiver.clone());
        write_field_unchecked(sender.clone(), PTR_LEN, rx_region as u64);

        // write_field_unchecked(receiver, 0, 0u64);
        write_ptr_unsafe_unchecked(receiver.clone(), PTR_LEN, sender.clone());

        (sender, receiver)
    }
}

/// Transmits a copy of the given Sail object along the queue
pub fn queue_tx(env: SlHndl, loc: SlHndl, item: SlHndl) {
    assert_eq!(loc.type_id(), super::T_QUEUE_TX_ID.0);
    assert_eq!(loc.base_size(), BaseSize::B16);

    // TODO: change to permit copying arbitrary values, could use
    // similar machinery to destroy_obj

    unsafe {
        // create new list element containing the item
        let elt = core_copy_val(
            read_field_unchecked::<u64>(loc.clone(), PTR_LEN) as *mut memmgt::Region,
            item,
        );

        let mut tail;
        loop {
            // get the current list tail (sender's perspective)
            tail = read_ptr_atomic(loc.clone(), 0).unwrap();

            // TODO: assign type IDs to all types with object representations

            // get pointer to the tail's next element
            let (is_head, next) = if tail.type_fld_p() && tail.type_id() == super::T_QUEUE_RX_ID.0 {
                (true, read_ptr_atomic(tail.clone(), 0))
            } else {
                (false, get_next_list_elt(tail.clone()))
            };

            if tail == read_ptr_atomic(loc.clone(), 0).unwrap() {
                match next {
                    None => {
                        // if the next element is nil, add the new element at the tail
                        if if is_head {
                            write_ptr_cmpxcg(env.clone(), tail.clone(), 0, None, elt.clone())
                        } else {
                            set_next_list_elt_cmpxcg(env.clone(), tail.clone(), None, elt.clone())
                        } {
                            // end loop if successful
                            break;
                        }
                    }
                    Some(nx) => {
                        // if next element not nil, advance tail pointer towards the tail
                        write_ptr_cmpxcg(env.clone(), loc.clone(), 0, Some(tail), nx);
                    }
                }
            }
        }
        // attempt to change the tail pointer to the new node
        write_ptr_cmpxcg(env, loc, 0, Some(tail), elt);
    }
}

/// Receives and returns the object at the head of the queue
pub fn queue_rx(env: SlHndl, loc: SlHndl) -> Option<SlHndl> {
    assert_eq!(loc.type_id(), super::T_QUEUE_RX_ID.0);
    assert_eq!(loc.base_size(), BaseSize::B16);

    loop {
        // get the head of the queue list
        let head = read_ptr_atomic(loc.clone(), 0);

        // get the tail of the queue list
        let sender = read_ptr(loc.clone(), PTR_LEN).unwrap();
        let tail = read_ptr_atomic(sender.clone(), 0);

        if head == read_ptr_atomic(loc.clone(), 0) {
            if tail == Some(loc.clone()) {
                // if this is the list tail and the head is nil, the queue is empty
                if head.is_none() {
                    return None;
                }
                // if the head isn't nil, shift the tail down the queue
                write_ptr_cmpxcg(env.clone(), sender.clone(), 0, tail, head.unwrap());
            } else {
                // TODO: this handler needs to work on windows
                if head.is_none() {
                    log::debug!("a queue head was nil");
                    write_ptr_cmpxcg_may_clr(
                        env.clone(),
                        loc.clone(),
                        0,
                        None,
                        read_ptr_atomic(sender, 0),
                    );
                    continue;
                }

                let next = get_next_list_elt(head.clone().unwrap());

                // if next element up is not nil, just attempt to advance to the next node
                // otherwise, try to make the receiver the list tail, then attempt to advance
                if (next.is_some() || write_ptr_cmpxcg(env.clone(), sender, 0, tail, loc.clone()))
                    && write_ptr_cmpxcg_may_clr(env.clone(), loc.clone(), 0, head.clone(), next)
                {
                    clr_next_list_elt(env.clone(), head.clone().unwrap());
                    return head;
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

            let dm_env = env_create(region, None);

            queue_tx(dm_env.clone(), send, item);

            let out = queue_rx(dm_env, recv).unwrap();

            assert_eq!(bool_get(out), true)
        }
    }

    #[test]
    fn q_two_reg() {
        unsafe {
            let tx_reg = memmgt::acquire_mem_region(100);
            let rx_reg = memmgt::acquire_mem_region(100);

            let (send, recv) = queue_create(tx_reg, rx_reg);

            let dm_env = env_create(tx_reg, None);

            queue_tx(dm_env.clone(), send.clone(), i64_init(tx_reg, 7));
            queue_tx(dm_env.clone(), send.clone(), i64_init(tx_reg, 14));

            assert_eq!(i64_get(queue_rx(dm_env.clone(), recv.clone()).unwrap()), 7);
            assert_eq!(i64_get(queue_rx(dm_env.clone(), recv.clone()).unwrap()), 14);

            queue_tx(dm_env.clone(), send.clone(), i64_init(tx_reg, 21));

            assert_eq!(i64_get(queue_rx(dm_env.clone(), recv.clone()).unwrap()), 21);

            queue_tx(dm_env.clone(), send.clone(), i64_init(tx_reg, 28));
            queue_tx(dm_env.clone(), send.clone(), i64_init(tx_reg, 35));

            assert_eq!(i64_get(queue_rx(dm_env.clone(), recv.clone()).unwrap()), 28);
            assert_eq!(i64_get(queue_rx(dm_env.clone(), recv.clone()).unwrap()), 35);

            queue_tx(dm_env.clone(), send, i64_init(tx_reg, 42));

            assert_eq!(i64_get(queue_rx(dm_env, recv).unwrap()), 42);
        }
    }
}

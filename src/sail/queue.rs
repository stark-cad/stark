use super::{memmgt, SlHead};

// TODO: probably will need to use atomic operations for these
// TODO: global identifiers for queues?

// unsafe fn atomic_ref_set(loc: *mut SlHead, next: *mut SlHead) {}

pub fn queue_create(
    tx_sector: *mut memmgt::Region,
    rx_sector: *mut memmgt::Region,
) -> (*mut SlHead, *mut SlHead) {
    unsafe {
        let sender = memmgt::alloc(tx_sector, 16, super::Cfg::B16Other as u8);
        let receiver = memmgt::alloc(rx_sector, 8, super::Cfg::B8Other as u8);

        super::set_self_type(sender, super::T_QUEUE_TX.0);
        super::set_self_type(receiver, super::T_QUEUE_RX.0);

        super::write_field_unchecked(sender, 8, rx_sector as u64);

        super::write_field_unchecked(sender, 0, receiver);
        super::write_field_unchecked(receiver, 0, sender);

        (sender, receiver)
    }
}

pub fn queue_tx(loc: *mut SlHead, item: *mut SlHead) {
    assert_eq!(super::get_self_type(loc), super::T_QUEUE_TX.0);
    assert_eq!(super::get_base_size(loc), super::BaseSize::B16);

    unsafe {
        // create new list element containing the item
        // TODO: must change to permit copying arbitrary values
        let elt = super::core_copy_val(
            super::read_field_unchecked::<u64>(loc, 8) as *mut memmgt::Region,
            item,
        );
        // point the element at the sender
        super::set_next_list_elt(elt, loc);
        // point the current queue head at the element
        let head = super::read_field_unchecked(loc, 0);
        if super::get_self_type(head) == super::T_QUEUE_RX.0 {
            super::write_field_unchecked(head, 0, elt);
        } else {
            super::set_next_list_elt(head, elt);
        }
        // point the sender at the element
        super::write_field_unchecked(loc, 0, elt);
    }
}

pub fn queue_rx(loc: *mut SlHead) -> *mut SlHead {
    assert_eq!(super::get_self_type(loc), super::T_QUEUE_RX.0);
    assert_eq!(super::get_base_size(loc), super::BaseSize::B8);

    // get the item pointed to by the receiver
    let item = unsafe { super::read_field_unchecked(loc, 0) };

    if super::get_self_type(item) == super::T_QUEUE_TX.0 {
        super::nil()
    } else {
        let next = super::get_next_list_elt(item);

        // point the receiver at the item pointed to
        unsafe {
            super::write_field_unchecked(loc, 0, next);
        }

        if super::get_self_type(next) == super::T_QUEUE_TX.0 {
            super::ref_set(next, loc);
        }

        // return the received item
        item
    }
}

// pub unsafe fn queue_rx_result(loc: *mut SlHead) -> Result<*mut SlHead, SailErr> {
//     typechk!(Ref QReceive ; loc);

//     // get the item pointed to by the receiver
//     let item = super::ref_get(loc);

//     if typep!(Ref QSend ; item) {
//         Err(SailErr::Error)
//     } else {
//         let next = super::next_list_elt(item);

//         // point the receiver at the item pointed to
//         super::ref_set(loc, next);

//         if typep!(Ref QSend ; next) {
//             super::ref_set(next, loc);
//         }

//         // return the received item
//         Ok(item)
//     }
// }

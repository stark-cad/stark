use super::{memmgt, SailErr, SlHead};

// TODO: probably will need to use atomic operations for these
// TODO: global identifiers for queues?
// TODO: queues manage their own memory sector for items in transit

// unsafe fn atomic_ref_set(loc: *mut SlHead, next: *mut SlHead) {}

// pub unsafe fn queue_create(
//     tx_sector: *mut memmgt::MemSector,
//     rx_sector: *mut memmgt::MemSector,
// ) -> (*mut SlHead, *mut SlHead) {
//     let sender = super::init_ref(tx_sector, false, super::SlRefMode::QSend);
//     let receiver = super::init_ref(rx_sector, false, super::SlRefMode::QReceive);

//     super::ref_qsend_set_target(sender, rx_sector);

//     super::ref_set(sender, receiver);
//     super::ref_set(receiver, sender);

//     (sender, receiver)
// }

// pub unsafe fn queue_tx(loc: *mut SlHead, item: *mut SlHead) {
//     typechk!(Ref QSend ; loc);

//     // create new list element containing the item
//     let elt = super::copy_val(super::ref_qsend_get_target(loc), item, true);
//     // point the element at the sender
//     super::set_list_elt(elt, loc);
//     // point the current queue head at the element
//     let head = super::ref_get(loc);
//     if typep!(head : Ref QReceive) {
//         super::ref_set(head, elt);
//     } else {
//         super::set_list_elt(head, elt);
//     }
//     // point the sender at the element
//     super::ref_set(loc, elt);
// }

// pub unsafe fn queue_rx(loc: *mut SlHead) -> *mut SlHead {
//     typechk!(Ref QReceive ; loc);

//     // get the item pointed to by the receiver
//     let item = super::ref_get(loc);

//     if typep!(Ref QSend ; item) {
//         let out = super::init_err(memmgt::which_mem_sector(loc), false);
//         out
//     } else {
//         let next = super::next_list_elt(item);

//         // point the receiver at the item pointed to
//         super::ref_set(loc, next);

//         if typep!(Ref QSend ; next) {
//             super::ref_set(next, loc);
//         }

//         // return the received item
//         item
//     }
// }

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

use super::SlHead;

// TODO: probably will need to use atomic operations for these
// TODO: global identifiers for queues?

pub unsafe fn queue_create() -> (*mut SlHead, *mut SlHead) {
    let sender = super::init_ref(false, super::SlRefMode::QSend);
    let receiver = super::init_ref(false, super::SlRefMode::QReceive);

    super::ref_set(sender, receiver);
    super::ref_set(receiver, sender);

    (sender, receiver)
}

pub unsafe fn queue_tx(loc: *mut SlHead, item: *mut SlHead) {
    typechk!(Ref QSend ; loc);

    // create new list element containing the item
    let elt = super::copy_val(item, true);
    // point the element at the sender
    super::set_list_elt(elt, loc);
    // point the current queue head at the element
    let head = super::ref_get(loc);
    if super::get_type(head) == super::SlType::Ref
        && super::ref_mode(head) == super::SlRefMode::QReceive
    {
        super::ref_set(head, elt);
    } else {
        super::set_list_elt(head, elt);
    }
    // point the sender at the element
    super::ref_set(loc, elt);
}

pub unsafe fn queue_rx(loc: *mut SlHead) -> *mut SlHead {
    typechk!(Ref QReceive ; loc);

    // get the item pointed to by the receiver
    let item = super::ref_get(loc);

    if super::get_type(item) == super::SlType::Ref
        && super::ref_mode(item) == super::SlRefMode::QSend
    {
        // TODO: replace all of these with Err returns
        let out = super::init_bool(false);
        super::bool_set(out, false);
        out
    } else {
        let next = super::next_list_elt(item);

        // point the receiver at the item pointed to
        super::ref_set(loc, next);

        if super::get_type(next) == super::SlType::Ref
            && super::ref_mode(next) == super::SlRefMode::QSend
        {
            super::ref_set(next, loc);
        }

        // return the received item
        item
    }
}

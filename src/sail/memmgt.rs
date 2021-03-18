use super::{SlHead, SlType, HEAD_LEN, PTR_LEN};

use std::alloc;
use std::mem;
use std::ptr;

// TODO: could use a freelist in empty portions of a memory zone

// TODO: current memory model does not work for multiple threads
static mut MEM_HEAD: *mut SlMemHead = ptr::null_mut();

// TODO: Enable getting and linking in more zones if we run out
// TODO: Probably make this private in the future
pub unsafe fn alloc(size: usize, list: bool, typ: SlType) -> *mut SlHead {
    if MEM_HEAD == ptr::null_mut() {
        MEM_HEAD = acquire_memory_zone(1000000);
    }

    let ptr = {
        let length = size + if list { HEAD_LEN + PTR_LEN } else { HEAD_LEN } as usize;

        if cfg!(feature = "memdbg") {
            log::debug!(
                "Allocating {} bytes for a {:?}; List elt: {}",
                length,
                typ,
                list
            );
        }

        let zone_ref = MEM_HEAD.as_mut().unwrap();

        if zone_ref.used + length >= zone_ref.size {
            panic!("tried to use too much memory");
        }

        let out = zone_ref.top;

        zone_ref.used += length;
        zone_ref.top = out.offset(length as isize);

        out as *mut SlHead
    };

    let conf = ((typ as u8) << 4) + ((list as u8) << 3);
    let head = SlHead { conf, rc: 1 };

    ptr::write_unaligned(ptr, head);

    if list {
        let next = (ptr as *mut u8).offset(HEAD_LEN as isize) as *mut *mut SlHead;
        ptr::write_unaligned(next, ptr::null_mut::<SlHead>());
    }

    ptr
}

// total size, size of used portion,  pointer to start, pointer to end of used portion
#[derive(Debug)]
struct SlMemHead {
    size: usize,
    used: usize,
    top: *mut u8,
}

const MEM_HEAD_SIZE: usize = mem::size_of::<SlMemHead>();

unsafe fn acquire_memory_zone(size: usize) -> *mut SlMemHead {
    let ptr = {
        let layout = alloc::Layout::from_size_align_unchecked(size + MEM_HEAD_SIZE, 1);
        alloc::alloc(layout)
    };

    let head = SlMemHead {
        size,
        used: 0,
        top: ptr.offset(MEM_HEAD_SIZE as isize),
    };

    ptr::write_unaligned(ptr as *mut SlMemHead, head);

    ptr as *mut SlMemHead
}

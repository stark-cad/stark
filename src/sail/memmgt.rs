use super::{SlHead, SlType, HEAD_LEN, PTR_LEN};

use std::alloc;
use std::mem;
use std::ptr;

/// TODO: allow the user to handle atomic operations if needed?
/// TODO: global memory, thread local memory
/// TODO: region and block instead of sector and zone

// TODO: if this is going to remain, writes must be atomic
// TODO: might need a different / nicer data structure
/// TODO: three parallel arrays altered via atomic operations (lock for expansion if needed)
/// zone start; zone end; sector the zone belongs to
static mut SECTOR_TABLE: Vec<(usize, usize, *mut MemSector)> = vec![];

// pub fn create(region: *mut MemSector, typ: u32, list: bool) -> *mut SlHead {

// }

// TODO: *garbage collection*
// TODO: separate zones for values with static size and those with variable size?
// TODO: could use a freelist in empty portions of a memory zone
// TODO: current memory model only sort of works for multiple threads
// TODO: Probably make this private in the future
pub unsafe fn alloc(sector: *mut MemSector, size: usize, list: bool, typ: SlType) -> *mut SlHead {
    assert_ne!(sector, ptr::null_mut());

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

        let sector_ref = sector.as_mut().unwrap();
        let mut zone_ref = sector_ref.head.as_mut().unwrap();

        while zone_ref.used + length >= sector_ref.zone_size {
            zone_ref = match zone_ref.next.as_mut() {
                Some(refer) => refer,
                None => {
                    new_mem_zone(sector);
                    sector_ref.head.as_mut().unwrap()
                }
            };
        }

        let lock: *mut u8 = &mut zone_ref.lock;
        while !std::intrinsics::atomic_cxchg_acq(lock, false as u8, true as u8).1 {
            std::hint::spin_loop();
        }

        let out = zone_ref.top;

        zone_ref.used += length;
        zone_ref.top = out.offset(length as isize);

        std::intrinsics::atomic_store_rel(lock, false as u8);

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

// A memory sector is a linked list of memory zones, all of the same size
#[derive(Debug)]
#[repr(C)]
pub struct MemSector {
    zone_size: usize,
    head: *mut MemZone,
}

// size of used portion, pointer to next zone, pointer to end of used portion, alloc lock?
#[derive(Debug)]
#[repr(C)]
struct MemZone {
    used: usize,
    top: *mut u8,
    next: *mut MemZone,
    lock: u8,
}

const MEM_SECTOR_HEAD_SIZE: usize = mem::size_of::<MemSector>();
const MEM_ZONE_HEAD_SIZE: usize = mem::size_of::<MemZone>();

pub unsafe fn acquire_mem_sector(zone_size: usize) -> *mut MemSector {
    // log::debug!("Creating mem sector / region");

    let out = Box::into_raw(Box::from(MemSector {
        zone_size,
        head: ptr::null_mut(),
    }));

    new_mem_zone(out);

    out
}

pub unsafe fn which_mem_sector(ptr: *mut SlHead) -> *mut MemSector {
    assert_ne!(ptr, ptr::null_mut());

    let pos = ptr as usize;
    for entry in &SECTOR_TABLE {
        if pos >= entry.0 && pos <= entry.1 {
            return entry.2;
        }
    }

    panic!("invalid memory sector")
    // ptr::null_mut()
}

unsafe fn new_mem_zone(sector: *mut MemSector) {
    assert_ne!(sector, ptr::null_mut());

    // log::debug!("Creating mem zone / block");

    let sector_head = sector.as_mut().unwrap();
    let size = sector_head.zone_size;
    let cur_head = sector_head.head;

    let ptr = {
        let layout = alloc::Layout::from_size_align_unchecked(size + MEM_ZONE_HEAD_SIZE, 1);
        alloc::alloc(layout) as *mut MemZone
    };

    let start = (ptr as *mut u8).offset(MEM_ZONE_HEAD_SIZE as isize);
    let end = start.offset(size as isize);

    SECTOR_TABLE.push((start as usize, end as usize, sector));

    let new_head = MemZone {
        used: 0,
        next: cur_head,
        top: start,
        lock: false as u8,
    };

    ptr::write_unaligned(ptr, new_head);

    // ptr::write_unaligned((sector as *mut usize).offset(1) as *mut *mut MemZone, ptr);

    std::intrinsics::atomic_store_rel((sector as *mut usize).offset(1) as *mut *mut MemZone, ptr);
}

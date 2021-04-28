// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License. See the top level LICENSE file for the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/sail/memmgt.rs

// Memory management for Sail. Creates regions and zones in which to
// store values, includes a purpose built allocator, and permits
// reflection on the region or zone of a particular value.

// <>

use super::{SlHead, HEAD_LEN, SYMBOL_LEN};

use std::alloc;
use std::mem;
use std::ptr;

/// TODO: allow the user to handle atomic operations if needed?
/// TODO: global memory, thread local memory

static mut REGION_TABLE: RegionTable = RegionTable::new();

/// Keeps track of memory zones and which regions they belong to
struct RegionTable {
    low_array: *mut usize,
    high_array: *mut usize,
    zone_array: *mut *mut Zone,
    region_array: *mut *mut Region,
    len: usize,
    cap: usize,
    lock: u8,
}

impl RegionTable {
    /// TODO: switch to intrinsics::const_alloc
    const fn new() -> Self {
        Self {
            low_array: ptr::null_mut(),
            high_array: ptr::null_mut(),
            zone_array: ptr::null_mut(),
            region_array: ptr::null_mut(),
            len: 0,
            cap: 0,
            lock: false as u8,
        }
    }

    unsafe fn setup(&mut self, cap: usize) {
        let layout = alloc::Layout::from_size_align_unchecked(cap * 8, 8);
        self.low_array = alloc::alloc(layout) as *mut usize;
        self.high_array = alloc::alloc(layout) as *mut usize;
        self.zone_array = alloc::alloc(layout) as *mut *mut Zone;
        self.region_array = alloc::alloc(layout) as *mut *mut Region;
        self.cap = cap;
    }
    unsafe fn resize(&mut self, cap: usize) {
        let lock: *mut u8 = &mut self.lock;
        while !std::intrinsics::atomic_cxchg_acq(lock, false as u8, true as u8).1 {
            std::hint::spin_loop();
        }

        let current_layout = alloc::Layout::from_size_align_unchecked(self.cap * 8, 8);
        self.low_array =
            alloc::realloc(self.low_array as *mut u8, current_layout, cap * 8) as *mut usize;
        self.high_array =
            alloc::realloc(self.high_array as *mut u8, current_layout, cap * 8) as *mut usize;
        self.zone_array =
            alloc::realloc(self.zone_array as *mut u8, current_layout, cap * 8) as *mut *mut Zone;
        self.region_array = alloc::realloc(self.region_array as *mut u8, current_layout, cap * 8)
            as *mut *mut Region;
        self.cap = cap;

        std::intrinsics::atomic_store_rel(lock, false as u8);
    }
    unsafe fn append(&mut self, start: usize, end: usize, zone: *mut Zone, region: *mut Region) {
        while self.lock == true as u8 {
            std::hint::spin_loop();
        }
        std::intrinsics::atomic_fence_acqrel();

        if self.len >= self.cap {
            self.resize(self.cap * 2);
        }

        let len_ptr: *mut usize = &mut self.len;
        let mut old_len = *len_ptr;
        while !std::intrinsics::atomic_cxchg_acqrel(len_ptr, old_len, old_len + 1).1 {
            old_len = *len_ptr;
        }

        // TODO: use atomic_store_rel if needed?
        ptr::write(self.low_array.add(old_len), start);
        ptr::write(self.high_array.add(old_len), end);
        ptr::write(self.zone_array.add(old_len), zone);
        ptr::write(self.region_array.add(old_len), region);
    }
    unsafe fn index(&mut self, idx: usize) -> (usize, usize, *mut Zone, *mut Region) {
        assert!(idx < self.len);

        while self.lock == true as u8 {
            std::hint::spin_loop();
        }
        std::intrinsics::atomic_fence_acqrel();

        // TODO: use atomic_load_acq if needed?
        let start = ptr::read(self.low_array.add(idx));
        let end = ptr::read(self.high_array.add(idx));
        let zone = ptr::read(self.zone_array.add(idx));
        let region = ptr::read(self.region_array.add(idx));

        (start, end, zone, region)
    }
}

// TODO: *garbage collection*
// TODO: separate zones for values with static size and those with variable size?
// TODO: implement the freelist in alloc, realloc, and dealloc
// TODO: current memory model only sort of works for multiple threads
// TODO: Probably make this private in the future
pub unsafe fn alloc(region: *mut Region, size: usize, cfg: u8) -> *mut SlHead {
    assert_ne!(region, ptr::null_mut());

    let ptr = {
        let size_type = cfg >> 5 == 7 || (cfg & 0b00011100) >> 2 == 7;
        let pred_type = cfg & 0b00000001 != 0;

        let length = if size_type && pred_type {
            HEAD_LEN + SYMBOL_LEN + SYMBOL_LEN
        } else if size_type || pred_type {
            HEAD_LEN + SYMBOL_LEN
        } else {
            HEAD_LEN
        } as usize
            + size;

        if cfg!(feature = "memdbg") {
            log::debug!("Allocating {} bytes with cfg: {:#010b}", length, cfg);
        }

        // TODO: search the freelist!

        let region_ref = region.as_mut().unwrap();
        let mut zone_ref = region_ref.head.as_mut().unwrap();

        while zone_ref.used + length >= region_ref.zone_size {
            zone_ref = match zone_ref.next.as_mut() {
                Some(refer) => refer,
                None => {
                    new_mem_zone(region);
                    region_ref.head.as_mut().unwrap()
                }
            };
        }

        let lock: *mut u8 = &mut zone_ref.lock;
        while !std::intrinsics::atomic_cxchg_acq(lock, false as u8, true as u8).1 {
            std::hint::spin_loop();
        }

        let out = zone_ref.top;

        zone_ref.used += length;
        zone_ref.top = out.add(length);

        std::intrinsics::atomic_store_rel(lock, false as u8);

        out as *mut SlHead
    };

    ptr::write_unaligned(ptr as *mut *mut SlHead, ptr::null_mut());

    let head = SlHead { cfg, rc: 1 };
    ptr::write_unaligned(ptr, head);

    ptr
}

// unsafe fn realloc(value: *mut SlHead, size: usize) -> *mut SlHead {
//     value
// }

// unsafe fn dealloc(value: *mut SlHead) {}

/// A memory region is a linked list of memory zones, all of the same size
#[derive(Debug)]
#[repr(C)]
pub struct Region {
    zone_size: usize,
    head: *mut Zone,
}

/// Size of used portion; pointers to end of used portion, start of freelist, & next zone; alloc lock
#[derive(Debug)]
#[repr(C)]
struct Zone {
    used: usize,
    top: *mut u8,
    free: *mut FreeBlock,
    next: *mut Zone,
    lock: u8,
}

/// Tagged pointer containing size of block and pointer to next free block
#[repr(C)]
struct FreeBlock {
    field: usize,
}

const MEM_REGION_HEAD_SIZE: usize = mem::size_of::<Region>();
const MEM_ZONE_HEAD_SIZE: usize = mem::size_of::<Zone>();

pub unsafe fn acquire_mem_region(zone_size: usize) -> *mut Region {
    if cfg!(feature = "memdbg") {
        log::debug!("Creating mem region");
    }

    if REGION_TABLE.cap == 0 {
        REGION_TABLE.setup(64);
    }

    let out = Box::into_raw(Box::from(Region {
        zone_size,
        head: ptr::null_mut(),
    }));

    new_mem_zone(out);

    out
}

pub unsafe fn which_mem_region(ptr: *mut SlHead) -> *mut Region {
    assert_ne!(ptr, ptr::null_mut());

    let pos = ptr as usize;
    for idx in 0..REGION_TABLE.len {
        let entry = REGION_TABLE.index(idx);
        if pos >= entry.0 && pos <= entry.1 {
            return entry.3;
        }
    }

    panic!("invalid memory area")
}

unsafe fn which_mem_zone(ptr: *mut SlHead) -> *mut Zone {
    assert_ne!(ptr, ptr::null_mut());

    let pos = ptr as usize;
    for idx in 0..REGION_TABLE.len {
        let entry = REGION_TABLE.index(idx);
        if pos >= entry.0 && pos <= entry.1 {
            return entry.2;
        }
    }

    panic!("invalid memory area")
}

unsafe fn new_mem_zone(region: *mut Region) {
    assert_ne!(region, ptr::null_mut());

    if cfg!(feature = "memdbg") {
        log::debug!("Creating mem zone");
    }

    let region_head = region.as_mut().unwrap();
    let size = region_head.zone_size;
    let cur_head = region_head.head;

    let ptr = {
        let layout = alloc::Layout::from_size_align_unchecked(size + MEM_ZONE_HEAD_SIZE, 1);
        alloc::alloc(layout) as *mut Zone
    };

    let start = (ptr as *mut u8).offset(MEM_ZONE_HEAD_SIZE as isize);
    let end = start.offset(size as isize);

    let new_head = Zone {
        used: 0,
        free: ptr::null_mut(),
        next: cur_head,
        top: start,
        lock: false as u8,
    };

    ptr::write_unaligned(ptr, new_head);

    REGION_TABLE.append(start as usize, end as usize, ptr, region);

    // ptr::write_unaligned((sector as *mut usize).offset(1) as *mut *mut MemZone, ptr);

    std::intrinsics::atomic_store_rel((region as *mut usize).offset(1) as *mut *mut Zone, ptr);
}

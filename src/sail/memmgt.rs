// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License. See the top level LICENSE file for the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/sail/memmgt.rs

// Memory management for Sail. Creates regions and zones in which to
// store objects, includes a purpose built allocator, and permits
// reflection on the region or zone of a particular object.

// <>

use super::{SlHead, HEAD_LEN, NUM_32_LEN};

use std::alloc;
use std::mem;
use std::ptr;

/// TODO: allow the user to handle atomic operations if needed?
/// TODO: global memory, thread local memory

static mut REGION_TABLE: RegionTable = RegionTable::new();

/// Keeps track of memory zones and which regions they belong to using
/// parallel arrays
struct RegionTable {
    /// Dynamic array of memory zone low ends
    low_array: *mut usize,
    /// Dynamic array of memory zone high ends
    high_array: *mut usize,
    /// Dynamic array of memory zones corresponding to above ranges
    zone_array: *mut *mut Zone,
    /// Dynamic array of memory regions corresponding to above zones
    region_array: *mut *mut Region,
    /// Length of all arrays above
    len: usize,
    /// Capacity of all arrays above
    cap: usize,
    /// Modification lock for region table
    lock: u8,
}

impl RegionTable {
    /// Create a new region table
    ///
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

    /// Allocate memory and initialize the table
    unsafe fn setup(&mut self, cap: usize) {
        let layout = alloc::Layout::from_size_align_unchecked(cap * 8, 8);
        self.low_array = alloc::alloc(layout) as *mut usize;
        self.high_array = alloc::alloc(layout) as *mut usize;
        self.zone_array = alloc::alloc(layout) as *mut *mut Zone;
        self.region_array = alloc::alloc(layout) as *mut *mut Region;
        self.cap = cap;
    }

    /// Resize the table, reallocating as necessary
    unsafe fn resize(&mut self, cap: usize) {
        let lock: *mut u8 = &mut self.lock;
        while !std::intrinsics::atomic_cxchg_acquire_acquire(lock, false as u8, true as u8).1 {
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

        std::intrinsics::atomic_store_release(lock, false as u8);
    }

    /// Add a new entry to the table
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
        while !std::intrinsics::atomic_cxchg_acqrel_acquire(len_ptr, old_len, old_len + 1).1 {
            old_len = *len_ptr;
        }

        // TODO: use atomic_store_rel if needed?
        ptr::write(self.low_array.add(old_len), start);
        ptr::write(self.high_array.add(old_len), end);
        ptr::write(self.zone_array.add(old_len), zone);
        ptr::write(self.region_array.add(old_len), region);
    }

    /// Gets a table entry by index
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

impl Drop for RegionTable {
    fn drop(&mut self) {
        println!("dropping region table");
        let mut last_reg = ptr::null_mut();
        unsafe {
            for i in 0..self.len {
                let cur_reg = ptr::read(self.region_array.add(i));
                if cur_reg != last_reg {
                    destroy_mem_region(cur_reg);
                    last_reg = cur_reg;
                }
            }

            let layout = alloc::Layout::from_size_align_unchecked(self.cap * 8, 8);
            alloc::dealloc(self.low_array as *mut u8, layout);
            alloc::dealloc(self.high_array as *mut u8, layout);
            alloc::dealloc(self.zone_array as *mut u8, layout);
            alloc::dealloc(self.region_array as *mut u8, layout);
        }
    }
}

// TODO: *garbage collection*
// TODO: separate zones for objects with static size and those with variable size?
// TODO: implement the freelist in alloc, realloc, and dealloc
// TODO: current memory model only sort of works for multiple threads
// TODO: Probably make this private in the future

/// Core Alloc Prep (CAP)
///
/// Utility function for allocating core types according to the rules
/// of the **alloc** *typ_id* parameter; generally do not use
#[inline(always)]
pub const fn cap(cfg: super::Cfg) -> u32 {
    assert!(cfg as u8 & 0b00011100 != 28);
    ((cfg as u32) >> 2) | 0x80000000
}

/// Allocates space in the given region for a Sail object, and preinitializes it
pub unsafe fn alloc(region: *mut Region, size: u32, typ_id: u32) -> *mut SlHead {
    //!
    //! Parameter Notes:
    //! **region** - the memory region wherein to allocate the object
    //! **size** - the object's size in bytes, after the head (full range)
    //! **typ_id** - the object's type identifier; MSB of 1 indicates
    //! core type, then *cfg* must be the least significant 6 bits

    assert_ne!(region, ptr::null_mut());

    let maybe_cfg = ((typ_id & 0b111111) as u8) << 2;

    let type_fld_p: bool = (typ_id >> 31) == 0;
    let size_fld_p: bool =
        (maybe_cfg >> 5 > 5) || (size != 16 && size != 8 && size != 4 && size > 2);

    let size_code =
        (7 * size_fld_p as u8) | ((size.trailing_zeros() as u8 + 1) * ((size != 0) as u8));

    let cfg: u8 = (((size_code << 5) + 28) * type_fld_p as u8) | (maybe_cfg * !(type_fld_p) as u8);

    let ptr = {
        let length = (HEAD_LEN
            + (type_fld_p as u32 * NUM_32_LEN)
            + (size_fld_p as u32 * NUM_32_LEN)) as usize
            + size as usize;

        // TODO: unique block IDs for all objects with =memdbg=

        if cfg!(feature = "memdbg") {
            log::debug!("Allocating {} bytes with cfg: {:#010b}", length, cfg);
        }

        // TODO: search the free tree!

        let region_ref = region.as_mut().unwrap();
        let mut zone_ref = region_ref.head.as_mut().unwrap();

        // the first condition is temporary (at least here) until we
        // have free tree search logic; it may also be useful for an
        // urgent / expedited allocation option
        while zone_ref.top as usize + length > zone_ref.end as usize
            || zone_ref.used as usize + length >= region_ref.zone_size as usize
            || (zone_ref.lblk != 0 && length > zone_ref.lblk as usize)
        {
            zone_ref = match zone_ref.next.as_mut() {
                Some(refer) => refer,
                None => {
                    new_mem_zone(region);
                    region_ref.head.as_mut().unwrap()
                }
            };
        }

        let lock: *mut u8 = &mut zone_ref.lock;
        while !std::intrinsics::atomic_cxchg_acquire_acquire(lock, false as u8, true as u8).1 {
            std::hint::spin_loop();
        }

        let out = zone_ref.top;

        assert!(out < zone_ref.end);

        // TODO: turns out length always needs to be a u32 anyway; fix
        zone_ref.used += length as u32;
        zone_ref.top = out.add(length);

        std::intrinsics::atomic_store_release(lock, false as u8);

        // zero the whole object
        ptr::write_bytes(out, 0, length);

        out as *mut SlHead
    };

    // zero the next list elt pointer
    // ptr::write_unaligned(ptr as *mut usize, 0);

    let head = SlHead { cfg, rc: 1 };
    ptr::write_unaligned(ptr, head);

    if type_fld_p {
        ptr::write_unaligned((ptr as *mut u32).add(2), typ_id);
    }

    if size_fld_p {
        ptr::write_unaligned((ptr as *mut u32).add(2 + type_fld_p as usize), size);
    }

    // log::info!("Allocated object");

    ptr
}

// unsafe fn realloc(obj: *mut SlHead, size: usize) -> *mut SlHead {
//     obj
// }

/// Get total length of live block
fn lblk_get_len(blk: *mut SlHead) -> u32 {
    (HEAD_LEN
        + (super::raw_typ_fld_p(blk) as u32 * NUM_32_LEN)
        + (super::raw_siz_fld_p(blk) as u32 * NUM_32_LEN))
        + super::raw_size(blk)
}

/// Adds memory that used to hold a Sail object to the freed memory
/// structure in its zone
pub unsafe fn dealloc(val: *mut SlHead) {
    // TODO: deallocations need to also handle objects' references

    // log::info!("Reclaiming object");

    let len = {
        let length = lblk_get_len(val);

        if length > !(1u32.reverse_bits()) {
            panic!("attempt to deallocate too-large object")
        }

        length
    };

    // TODO: panic if object appears to exceed zone's max obj size

    let zone = which_mem_area(val).1;

    let newblk = val as *mut FreeBlock;

    let trunk = {
        let f_trunk = (*zone).free;
        if f_trunk.is_null() {
            if (newblk as *mut u8).add(len as _) == (*zone).top {
                // TODO: change these operations (multiple) to atomic?
                (*zone).top = newblk as _;

                std::intrinsics::atomic_xsub_acqrel(&mut (*zone).used, len);
                std::intrinsics::atomic_umax_acqrel(&mut (*zone).lblk, 0);

                if cfg!(feature = "memdbg") {
                    __dbg_visualize_zone(zone)
                }

                return;
            }

            fblk_set_len(newblk, len);
            fblk_set_prev_ofs(newblk, FBLK_NULL_OFFSET);
            fblk_set_next_ofs(newblk, FBLK_NULL_OFFSET);

            // TODO: if this needs to be concurrency-safe, so does
            // every write to any offset pointer in the free
            // tree. probably better to keep a safe queue of objects
            // that require deallocation, then guarantee that
            // deallocation activity will only be undertaken by a
            // single thread. could also use a free tree lock. when
            // the lock is taken, reentrant deallocation runs are
            // prevented, as well as any allocation not from the
            // zone's empty area.

            // TODO: more clearly specify concurrency needs for zones

            let (n_trunk, done) = std::intrinsics::atomic_cxchg_acqrel_acquire(
                &mut (*zone).free,
                ptr::null_mut(),
                newblk,
            );

            if done {
                std::intrinsics::atomic_xsub_acqrel(&mut (*zone).used, len);
                std::intrinsics::atomic_umax_acqrel(&mut (*zone).lblk, len);
                return;
            } else {
                n_trunk
            }
        } else {
            f_trunk
        }
    };

    let (mut lower_bound, mut upper_bound, mut lb_parent, mut ub_parent) = {
        if newblk < trunk {
            (ptr::null_mut(), trunk, ptr::null_mut(), ptr::null_mut())
        } else if newblk > trunk {
            (trunk, ptr::null_mut(), ptr::null_mut(), ptr::null_mut())
        } else {
            panic!("double free attempted")
        }
    };

    let mut chg_p = true;

    while chg_p {
        chg_p = false;

        let (ub_last, lb_last) = (upper_bound, lower_bound);

        if !lower_bound.is_null() {
            let prospect = fblk_get_next_blk(zone, lower_bound);
            if prospect.is_null() {
            } else if prospect > lower_bound && prospect < newblk {
                if prospect != lb_last {
                    lb_parent = lower_bound
                }
                lower_bound = prospect;
            } else if prospect > lower_bound && prospect > newblk {
                if prospect != ub_last {
                    ub_parent = lower_bound
                }
                upper_bound = prospect;
            } else {
                panic!()
            }
        }

        if !upper_bound.is_null() {
            let prospect = fblk_get_prev_blk(zone, upper_bound);
            if prospect.is_null() {
            } else if prospect < upper_bound && prospect < newblk {
                if prospect != ub_last {
                    lb_parent = upper_bound
                }
                lower_bound = prospect;
            } else if prospect < upper_bound && prospect > newblk {
                if prospect != ub_last {
                    ub_parent = upper_bound
                }
                upper_bound = prospect;
            } else {
                panic!()
            }
        }

        if lb_last != lower_bound || ub_last != upper_bound {
            chg_p = true
        }
    }

    const BD_MAX: usize = FBLK_NULL_OFFSET as usize;

    let prior_merge = 'p: {
        if lower_bound.is_null() {
            break 'p false;
        }

        assert!(lower_bound < newblk);

        let lb_end = lower_bound as usize + fblk_get_len(lower_bound) as usize;
        let nw_start = newblk as usize;
        let diff = nw_start - lb_end;

        match diff {
            0 => true,
            1..=7 => panic!("memory management failed"),
            8..=BD_MAX => false,
            _ => panic!(),
        }
    };

    let follow_merge = 'n: {
        if upper_bound.is_null() {
            break 'n false;
        }

        assert!(upper_bound > newblk);

        let nw_end = newblk as usize + len as usize;
        let ub_start = upper_bound as usize;
        let diff = ub_start - nw_end;

        match diff {
            0 => true,
            1..=7 => panic!("memory management failed"),
            8..=BD_MAX => false,
            _ => panic!(),
        }
    };

    unsafe fn insert_fblk_at(zone: *const Zone, tgt: *mut FreeBlock, blk: *mut FreeBlock) {
        let mut cursor = tgt;
        loop {
            if blk < cursor {
                let prospect = fblk_get_prev_blk(zone, cursor);
                if prospect.is_null() {
                    fblk_set_prev_blk(zone, cursor, blk);
                    break;
                } else {
                    cursor = prospect
                }
            } else if blk > cursor {
                let prospect = fblk_get_next_blk(zone, cursor);
                if prospect.is_null() {
                    fblk_set_next_blk(zone, cursor, blk);
                    break;
                } else {
                    cursor = prospect
                }
            } else {
                unreachable!()
            }
        }
    }

    // All tree manipulation is restricted to this block
    let final_len = match (prior_merge, follow_merge) {
        (true, true) => {
            let (ubprev, ubnext) = (
                fblk_get_prev_blk(zone, upper_bound),
                fblk_get_next_blk(zone, upper_bound),
            );

            let lbnext = fblk_get_next_blk(zone, lower_bound);

            if lbnext == upper_bound {
                fblk_set_next_blk(zone, lower_bound, ptr::null_mut());
            }

            if !ub_parent.is_null() && lb_parent == upper_bound {
                if lower_bound < ub_parent {
                    fblk_set_prev_blk(zone, ub_parent, lower_bound)
                } else if lower_bound > ub_parent {
                    fblk_set_next_blk(zone, ub_parent, lower_bound)
                }
                ub_parent = ptr::null_mut()
            } else if ub_parent == lower_bound {
                fblk_set_next_blk(zone, lower_bound, ptr::null_mut());
                ub_parent = ptr::null_mut()
            }

            if !ub_parent.is_null() {
                if upper_bound < ub_parent {
                    assert!(newblk < ub_parent);
                    assert_eq!(upper_bound, fblk_get_prev_blk(zone, ub_parent));

                    if !ubnext.is_null() {
                        fblk_set_prev_blk(zone, ubnext, ubprev)
                    }

                    fblk_set_prev_blk(zone, ub_parent, ubnext)
                } else if upper_bound > ub_parent {
                    assert!(newblk > ub_parent);
                    assert_eq!(upper_bound, fblk_get_next_blk(zone, ub_parent));

                    if !ubprev.is_null() {
                        fblk_set_next_blk(zone, ubprev, ubnext)
                    }

                    fblk_set_next_blk(zone, ub_parent, ubprev)
                }
            } else {
                if !ubprev.is_null() && ubprev != lower_bound {
                    insert_fblk_at(zone, lower_bound, ubprev)
                }

                if !ubnext.is_null() {
                    insert_fblk_at(zone, lower_bound, ubnext)
                }
            }

            if upper_bound == trunk {
                (*zone).free = lower_bound
            }

            let full_len = len + fblk_get_len(lower_bound) + fblk_get_len(upper_bound);
            fblk_set_len(lower_bound, full_len);

            full_len
        }
        (true, false) => {
            if newblk as usize + len as usize == (*zone).top as usize {
                let (lbprev, lbnext) = (
                    fblk_get_prev_blk(zone, lower_bound),
                    fblk_get_next_blk(zone, lower_bound),
                );

                assert_eq!(lbnext, ptr::null_mut());

                if !lb_parent.is_null() && lower_bound < lb_parent {
                    unreachable!()
                } else if !lb_parent.is_null() && lower_bound > lb_parent {
                    assert!(newblk > lb_parent);
                    assert_eq!(lower_bound, fblk_get_next_blk(zone, lb_parent));

                    fblk_set_next_blk(zone, lb_parent, lbprev)
                }

                if lower_bound == trunk {
                    (*zone).free = lbprev
                }

                (*zone).top = lower_bound as _;

                0
            } else {
                let full_len = len + fblk_get_len(lower_bound);
                fblk_set_len(lower_bound, full_len);

                full_len
            }
        }
        (false, true) => {
            if !ub_parent.is_null() && upper_bound < ub_parent {
                assert!(newblk < ub_parent);
                assert_eq!(upper_bound, fblk_get_prev_blk(zone, ub_parent));

                fblk_set_prev_blk(zone, ub_parent, newblk)
            } else if !ub_parent.is_null() && upper_bound > ub_parent {
                assert!(newblk > ub_parent);
                assert_eq!(upper_bound, fblk_get_next_blk(zone, ub_parent));

                fblk_set_next_blk(zone, ub_parent, newblk)
            }

            fblk_set_prev_ofs(newblk, fblk_get_prev_ofs(upper_bound));
            fblk_set_next_ofs(newblk, fblk_get_next_ofs(upper_bound));

            if upper_bound == trunk {
                (*zone).free = newblk
            }

            let full_len = len + fblk_get_len(upper_bound);
            fblk_set_len(newblk, full_len);

            full_len
        }
        (false, false) => {
            if newblk as usize + len as usize == (*zone).top as usize {
                (*zone).top = newblk as _;
                0
            } else {
                if !lower_bound.is_null() && fblk_get_next_ofs(lower_bound) == FBLK_NULL_OFFSET {
                    fblk_set_next_blk(zone, lower_bound, newblk)
                } else if !upper_bound.is_null()
                    && fblk_get_prev_ofs(upper_bound) == FBLK_NULL_OFFSET
                {
                    fblk_set_prev_blk(zone, upper_bound, newblk)
                } else {
                    panic!("neither neighbor had an available field")
                }

                fblk_set_prev_ofs(newblk, FBLK_NULL_OFFSET);
                fblk_set_next_ofs(newblk, FBLK_NULL_OFFSET);
                fblk_set_len(newblk, len);

                len
            }
        }
    };

    std::intrinsics::atomic_xsub_acqrel(&mut (*zone).used, len);
    std::intrinsics::atomic_umax_acqrel(&mut (*zone).lblk, final_len);
}

/// Set the length of a free memory block in a zone
unsafe fn fblk_set_len(block: *mut FreeBlock, length: u32) {
    assert!(!block.is_null());
    assert!(length >= 8);

    let exceeds_head = length > 8;
    let exceeds_tag = length > 255;
    let exceeds_temporary_limit = length > (1 << 31) - 1;

    if exceeds_temporary_limit {
        panic!("attempt to assert free block too large")
    }

    {
        let block = block as *mut u8;
        if exceeds_head {
            if exceeds_tag {
                ptr::write_unaligned(block.add(HEAD_LEN as usize), 0);
                ptr::write_unaligned(block.add(HEAD_LEN as usize + 1) as *mut u32, length as u32);
            } else {
                ptr::write_unaligned(block.add(HEAD_LEN as usize), length as u8);
            }
        }
    }

    let head = ptr::read_unaligned(block as *mut u64);

    let next_ofs = head & 0x7FFFFFFF;
    let prev_ofs = (head >> 32) & 0x7FFFFFFF;

    let parity_flag = {
        let set_bits =
            (exceeds_head as u8).count_ones() + prev_ofs.count_ones() + next_ofs.count_ones();
        assert!(set_bits <= 63);

        !(set_bits as u64 & 1) & 1
    };

    let new_head =
        ((exceeds_head as u64) << 63) + (prev_ofs << 32) + (parity_flag << 31) + next_ofs;

    ptr::write_unaligned(block as *mut u64, new_head);
}

/// Get the length stored in a free memory block
unsafe fn fblk_get_len(block: *const FreeBlock) -> u32 {
    assert!(!block.is_null());

    let head = ptr::read_unaligned(block as *const u64);

    // odd parity check
    assert_eq!(head.count_ones() & 1, 1);

    let exceeds_head = (head >> 63) != 0;

    if exceeds_head {
        let tag = ptr::read_unaligned((block as *const u8).add(HEAD_LEN as usize));
        let exceeds_tag = tag < 9;
        if exceeds_tag {
            ptr::read_unaligned((block as *const u8).add(HEAD_LEN as usize + 1) as *mut u32)
        } else {
            tag as u32
        }
    } else {
        8
    }
}

// Free block head scheme: size bit, prev offset, parity bit, next offset

/// MSB 0; all other bits 1
const FBLK_NULL_OFFSET: u32 = (!1u32).reverse_bits();

/// Set a free block's offset pointer to the previous block
unsafe fn fblk_set_prev_ofs(block: *mut FreeBlock, offset: u32) {
    let head_ptr = block as *mut u64;
    let head = ptr::read_unaligned(head_ptr);

    let size_flag = head >> 63;

    assert_eq!(offset & 1u32.reverse_bits(), 0);
    let prev_ofs = offset as u64;

    let next_ofs = head & 0x7FFFFFFF;

    let parity_flag = {
        let set_bits = size_flag.count_ones() + prev_ofs.count_ones() + next_ofs.count_ones();
        assert!(set_bits <= 63);

        !(set_bits as u64 & 1) & 1
    };

    let new_head = (size_flag << 63) + (prev_ofs << 32) + (parity_flag << 31) + next_ofs;

    ptr::write_unaligned(head_ptr, new_head)
}

/// Get a free block's offset pointer to the previous block
unsafe fn fblk_get_prev_ofs(block: *const FreeBlock) -> u32 {
    let head_ptr = block as *mut u64;
    let head = ptr::read_unaligned(head_ptr);

    // odd parity check
    assert_eq!(head.count_ones() & 1, 1);

    (head >> 32) as u32 & 0x7FFFFFFF
}

/// Set a free block's offset pointer to the next block
unsafe fn fblk_set_next_ofs(block: *mut FreeBlock, offset: u32) {
    let head_ptr = block as *mut u64;
    let head = ptr::read_unaligned(head_ptr);

    let size_flag = head >> 63;

    let prev_ofs = (head >> 32) & 0x7FFFFFFF;

    assert_eq!(offset & 1u32.reverse_bits(), 0);
    let next_ofs = offset as u64;

    let parity_flag = {
        let set_bits = size_flag.count_ones() + prev_ofs.count_ones() + next_ofs.count_ones();
        assert!(set_bits <= 63);

        !(set_bits as u64 & 1) & 1
    };

    let new_head = (size_flag << 63) + (prev_ofs << 32) + (parity_flag << 31) + next_ofs;

    ptr::write_unaligned(head_ptr, new_head)
}

/// Get a free block's offset pointer to the next block
unsafe fn fblk_get_next_ofs(block: *const FreeBlock) -> u32 {
    let head_ptr = block as *mut u64;
    let head = ptr::read_unaligned(head_ptr);

    // odd parity check
    assert_eq!(head.count_ones() & 1, 1);

    head as u32 & 0x7FFFFFFF
}

/// Set which block a free block points to as the prior block
unsafe fn fblk_set_prev_blk(zone: *const Zone, block: *mut FreeBlock, prev: *const FreeBlock) {
    debug_assert_eq!(which_mem_area(block as *mut _).1 as *const _, zone);

    if prev.is_null() {
        fblk_set_prev_ofs(block, FBLK_NULL_OFFSET);
        return;
    }

    debug_assert_eq!(which_mem_area(prev as *mut _).1 as *const _, zone);

    let start = (*zone).bot;
    let prev_ofs = prev as u64 - start as u64;

    assert!(prev_ofs < FBLK_NULL_OFFSET as u64);

    fblk_set_prev_ofs(block, prev_ofs as u32)
}

/// Get the block a free block points to as the prior block
unsafe fn fblk_get_prev_blk(zone: *const Zone, block: *const FreeBlock) -> *mut FreeBlock {
    debug_assert_eq!(which_mem_area(block as *mut _).1 as *const _, zone);

    let start = (*zone).bot;
    let prev_ofs = fblk_get_prev_ofs(block);

    if prev_ofs == FBLK_NULL_OFFSET {
        return ptr::null_mut();
    }

    assert!(prev_ofs < FBLK_NULL_OFFSET);

    (start as u64 + prev_ofs as u64) as *mut FreeBlock
}

/// Set which block a free block points to as the following block
unsafe fn fblk_set_next_blk(zone: *const Zone, block: *mut FreeBlock, next: *const FreeBlock) {
    debug_assert_eq!(which_mem_area(block as *mut _).1 as *const _, zone);

    if next.is_null() {
        fblk_set_next_ofs(block, FBLK_NULL_OFFSET);
        return;
    }

    debug_assert_eq!(which_mem_area(next as *mut _).1 as *const _, zone);

    let start = (*zone).bot;
    let next_ofs = next as u64 - start as u64;

    assert!(next_ofs <= 1u32.reverse_bits() as u64 - 1);

    fblk_set_next_ofs(block, next_ofs as u32)
}

/// Get the block a free block points to as the following block
unsafe fn fblk_get_next_blk(zone: *const Zone, block: *const FreeBlock) -> *mut FreeBlock {
    debug_assert_eq!(which_mem_area(block as *mut _).1 as *const _, zone);

    let start = (*zone).bot;
    let next_ofs = fblk_get_next_ofs(block);

    if next_ofs == FBLK_NULL_OFFSET {
        return ptr::null_mut();
    }

    assert!(next_ofs < FBLK_NULL_OFFSET);

    (start as u64 + next_ofs as u64) as *mut FreeBlock
}

#[cfg(test)]
mod free_tree_tests {
    use super::*;

    #[test]
    fn set_block_fields() {
        let mut test_block = FreeBlock { prev: 0, next: 0 };

        unsafe {
            let block_ptr = &mut test_block;

            fblk_set_len(block_ptr, 8);
            fblk_set_prev_ofs(block_ptr, 101);
            fblk_set_next_ofs(block_ptr, 202);

            assert_eq!(8, fblk_get_len(block_ptr));
            assert_eq!(101, fblk_get_prev_ofs(block_ptr));
            assert_eq!(202, fblk_get_next_ofs(block_ptr));
        }

        assert_eq!(101, test_block.prev & !(1u32.reverse_bits()));
        assert_eq!(202, test_block.next & !(1u32.reverse_bits()));
    }

    #[test]
    fn block_lengths() {
        unsafe {
            let ptr =
                alloc::alloc(alloc::Layout::from_size_align(13, 1).unwrap()) as *mut FreeBlock;

            fblk_set_prev_ofs(ptr, 222222);
            fblk_set_next_ofs(ptr, 888888);

            fblk_set_len(ptr, 42);

            assert_eq!(42, fblk_get_len(ptr));
            assert_eq!(42, ptr::read((ptr as *mut u8).add(8)));

            fblk_set_len(ptr, 1234567);

            assert_eq!(1234567, fblk_get_len(ptr));
            assert_eq!(0, ptr::read((ptr as *mut u8).add(8)));
            assert_eq!(
                1234567,
                ptr::read_unaligned((ptr as *mut u8).add(9) as *mut u32)
            );

            assert_eq!(222222, fblk_get_prev_ofs(ptr));
            assert_eq!(888888, fblk_get_next_ofs(ptr));
        }
    }

    #[test]
    fn dealloc_in_zone() {
        use super::super::Cfg;

        unsafe {
            let region = acquire_mem_region(1000);
            let zone = (*region).head;

            let obj = alloc(region, 0, cap(Cfg::B0BoolT));
            let adrs = obj as usize;
            dealloc(obj);

            assert_eq!(0, (*zone).used);

            let obj_1 = alloc(region, 256, cap(Cfg::VecAny));
            let obj_2 = alloc(region, 16, cap(Cfg::B16U128));
            let obj_3 = alloc(region, 0, cap(Cfg::B0BoolF));
            let obj_4 = alloc(region, 42, cap(Cfg::VecStr));

            assert_eq!(adrs, obj_1 as _);

            let total_size = lblk_get_len(obj_1)
                + lblk_get_len(obj_2)
                + lblk_get_len(obj_3)
                + lblk_get_len(obj_4);

            assert_eq!(total_size, (*zone).used);
            assert_eq!(
                total_size as usize,
                (*zone).top as usize - (*zone).bot as usize
            );

            dealloc(obj_2);
            dealloc(obj_3);
            dealloc(obj_1);

            assert_eq!(obj_1 as *mut u8, (*zone).free as *mut u8);

            dealloc(obj_4);

            assert_eq!(0, (*zone).used);
            assert_eq!(ptr::null_mut(), (*zone).free);
            assert_eq!((*zone).bot, (*zone).top);
        }
    }
}


/// A memory region is a linked list of memory zones, all of the same size
#[derive(Debug)]
#[repr(C)]
pub struct Region {
    head: *mut Zone,
    zone_size: u32,
}

// Maximum zone size: 2GiB
// Maximum object size: 1/32 of zone size
// Larger objects will require a special allocation scheme (TODO)

/// A zone is a contiguous chunk of memory in which Sail objects may
/// be allocated
#[derive(Debug)]
#[repr(C)]
struct Zone {
    /// Size of used portion
    used: u32,
    /// Size of largest free block
    lblk: u32,
    /// Pointer to start of working memory
    bot: *mut u8,
    /// Pointer to last of used portion
    top: *mut u8,
    /// Pointer to end of available space
    end: *mut u8,
    /// Pointer to top of free tree
    free: *mut FreeBlock,
    /// Pointer to next zone in region
    next: *mut Zone,
    /// Zone allocation lock
    lock: u8,
}

#[repr(C)]
struct FreeBlock {
    /// The top bit of this offset is an odd parity bit, set to make
    /// the count of high bits in the leaf structure odd (experiment)
    next: u32,
    /// The top bit of this offset (also top bit of the word) is low if
    /// the block is 8 bytes long, and high if it is longer
    prev: u32,
}

// const MEM_REGION_HEAD_SIZE: usize = mem::size_of::<Region>();
const MEM_ZONE_HEAD_SIZE: usize = mem::size_of::<Zone>();

/// Creates a new memory region and accompanying zone
pub unsafe fn acquire_mem_region(zone_size: u32) -> *mut Region {
    if cfg!(feature = "memdbg") {
        log::debug!("Creating mem region");
    }

    // TODO: use const_allocate to do this at compile time
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

pub unsafe fn destroy_mem_region(reg: *mut Region) {
    let layout =
        alloc::Layout::from_size_align_unchecked((*reg).zone_size as usize + MEM_ZONE_HEAD_SIZE, 1);
    let mut next = (*reg).head;
    loop {
        let cur = next;
        next = (*cur).next;
        if next == ptr::null_mut() {
            break;
        }
        alloc::dealloc(cur as *mut u8, layout);
    }

    let reg = Box::from_raw(reg);
    drop(reg);
}

/// Returns the region and zone in which a given Sail object is stored
unsafe fn which_mem_area(ptr: *mut SlHead) -> (*mut Region, *mut Zone) {
    assert_ne!(ptr, ptr::null_mut());

    let pos = ptr as usize;
    for idx in 0..REGION_TABLE.len {
        let entry = REGION_TABLE.index(idx);
        if pos >= entry.0 && pos <= entry.1 {
            return (entry.3, entry.2);
        }
    }

    panic!("invalid memory area")
}

// unsafe fn which_mem_zone(ptr: *mut SlHead) -> *mut Zone {
//     assert_ne!(ptr, ptr::null_mut());

//     let pos = ptr as usize;
//     for idx in 0..REGION_TABLE.len {
//         let entry = REGION_TABLE.index(idx);
//         if pos >= entry.0 && pos <= entry.1 {
//             return entry.2;
//         }
//     }

//     panic!("invalid memory zone")
// }

/// Creates and links in a new memory zone within the given region
unsafe fn new_mem_zone(region: *mut Region) {
    assert_ne!(region, ptr::null_mut());

    if cfg!(feature = "memdbg") {
        log::debug!("Creating mem zone");
    }

    let region_head = region.as_mut().unwrap();
    let size = region_head.zone_size;
    let cur_head = region_head.head;

    let ptr = {
        let layout =
            alloc::Layout::from_size_align_unchecked(size as usize + MEM_ZONE_HEAD_SIZE, 1);
        alloc::alloc(layout) as *mut Zone
    };

    let start = (ptr as *mut u8).add(MEM_ZONE_HEAD_SIZE);
    let end = start.add(size as _);

    let new_head = Zone {
        used: 0,
        free: ptr::null_mut(),
        lblk: 0,
        next: cur_head,
        top: start,
        end,
        bot: start,
        lock: false as u8,
    };

    ptr::write_unaligned(ptr, new_head);

    REGION_TABLE.append(start as usize, end as usize, ptr, region);

    ptr::write_unaligned(&mut (*region).head, ptr);

    // std::intrinsics::atomic_store_rel((region as *mut usize).offset(1) as *mut *mut Zone, ptr);
}

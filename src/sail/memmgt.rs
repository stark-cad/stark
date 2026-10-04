// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/memmgt.rs

// Memory management for Sail. Creates regions and zones in which to
// store objects, includes a purpose built allocator, and permits
// reflection on the region or zone of a particular object.

// <>

use super::util;
use super::{HEAD_LEN, NUM_32_LEN, SlHead};

use std::alloc;
use std::mem;
use std::ptr;

use std::intrinsics::AtomicOrdering as AO;

#[macro_export]
macro_rules! report_site {
    ( $msg:literal ) => {
        if cfg!(feature = "memdbg") {
            println!("Report from {} at {}: {}", std::file!(), std::line!(), $msg);
        }
    };
}

static mut OBJECT_ID_CTR: u32 = 0;

static mut ZONE_ID_CTR: u32 = 0;

// TODO: allow the programmer to handle atomic operations if needed?
// TODO: global memory, thread local memory

static mut REGION_TABLE: RegionTable = RegionTable::new(64);

pub fn init() {
    unsafe { RegionTable::init(&raw mut REGION_TABLE) }
}

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

// TODO: test by prefix! search tree of zones, return region

// TODO: define region table concurrency characteristics
// NOTE: possibly excessive locking currently

// TODO: suspect rare failures connected to this data structure at present

impl RegionTable {
    /// Create a new region table
    const fn new(cap: usize) -> Self {
        Self {
            low_array: ptr::null_mut(),
            high_array: ptr::null_mut(),
            zone_array: ptr::null_mut(),
            region_array: ptr::null_mut(),
            len: 0,
            cap,
            lock: false as u8,
        }
    }

    unsafe fn init(tgt: *mut Self) {
        unsafe {
            let this = tgt.as_mut().unwrap();

            if this.low_array.is_null() {
                assert!(
                    this.low_array.is_null()
                        && this.high_array.is_null()
                        && this.zone_array.is_null()
                        && this.region_array.is_null()
                );

                let layout = alloc::Layout::from_size_align_unchecked(this.cap * 8, 8);

                this.low_array = alloc::alloc(layout) as *mut usize;
                this.high_array = alloc::alloc(layout) as *mut usize;
                this.zone_array = alloc::alloc(layout) as *mut *mut Zone;
                this.region_array = alloc::alloc(layout) as *mut *mut Region;
            }
        }
    }

    unsafe fn len(tgt: *mut Self) -> usize {
        unsafe {
            let lock: *mut u8 = &raw mut (*tgt).lock;
            while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                lock,
                false as u8,
                true as u8,
            )
            .1
            {
                std::hint::spin_loop();
            }

            let out = (*tgt).len;

            std::intrinsics::atomic_store::<_, { AO::Release }, false>(lock, false as u8);

            out
        }
    }

    /// Resize the table, reallocating as necessary (must be locked)
    unsafe fn resize(tgt: *mut Self, cap: usize) {
        unsafe {
            // let lock: *mut u8 = &mut self.lock;
            // while !std::intrinsics::atomic_cxchg_acqrel_acquire(lock, false as u8, true as u8).1 {
            //     std::hint::spin_loop();
            // }

            if (*tgt).cap != cap {
                let current_layout = alloc::Layout::from_size_align_unchecked((*tgt).cap * 8, 8);

                (*tgt).low_array =
                    alloc::realloc((*tgt).low_array as *mut u8, current_layout, cap * 8)
                        as *mut usize;
                (*tgt).high_array =
                    alloc::realloc((*tgt).high_array as *mut u8, current_layout, cap * 8)
                        as *mut usize;
                (*tgt).zone_array =
                    alloc::realloc((*tgt).zone_array as *mut u8, current_layout, cap * 8)
                        as *mut *mut Zone;
                (*tgt).region_array =
                    alloc::realloc((*tgt).region_array as *mut u8, current_layout, cap * 8)
                        as *mut *mut Region;

                (*tgt).cap = cap;
            }

            // std::intrinsics::atomic_store_release(lock, false as u8);
        }
    }

    /// Add a new entry to the table
    unsafe fn append(
        tgt: *mut Self,
        start: usize,
        end: usize,
        zone: *mut Zone,
        region: *mut Region,
    ) {
        unsafe {
            let lock: *mut u8 = &raw mut (*tgt).lock;
            while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                lock,
                false as u8,
                true as u8,
            )
            .1
            {
                std::hint::spin_loop();
            }

            if (*tgt).low_array.is_null() {
                if cfg!(test) {
                    Self::init(tgt)
                } else {
                    panic!("region table not initialized")
                }
            }

            let old_len = (*tgt).len;

            if old_len >= (*tgt).cap {
                Self::resize(tgt, (*tgt).cap * 2);
            }

            (*tgt).len += 1;

            ptr::write((*tgt).low_array.add(old_len), start);
            ptr::write((*tgt).high_array.add(old_len), end);
            ptr::write((*tgt).zone_array.add(old_len), zone);
            ptr::write((*tgt).region_array.add(old_len), region);

            std::intrinsics::atomic_store::<_, { AO::Release }, false>(lock, false as u8);

            println!("registered {:x} to {:x}", start, end);

            // if cfg!(feature = "memdbg") {

            // println!("- Zone added -");
            // for i in 0..self.len {
            //     let entry = self.index(i);
            //     if !entry.2.is_null() {
            //         println!(
            //             "Zone {}, idx {}: {:x} to {:x}",
            //             (*entry.2).id,
            //             i,
            //             entry.0,
            //             entry.1
            //         )
            //     }
            // }
            // println!()

            // }
        }
    }

    /// Gets a table entry by index
    unsafe fn index(tgt: *mut Self, idx: usize) -> (usize, usize, *mut Zone, *mut Region) {
        unsafe {
            let lock: *mut u8 = &raw mut (*tgt).lock;
            while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                lock,
                false as u8,
                true as u8,
            )
            .1
            {
                std::hint::spin_loop();
            }

            assert!(idx < (*tgt).len);

            let start = ptr::read((*tgt).low_array.add(idx));
            let end = ptr::read((*tgt).high_array.add(idx));
            let zone = ptr::read((*tgt).zone_array.add(idx));
            let region = ptr::read((*tgt).region_array.add(idx));

            std::intrinsics::atomic_store::<_, { AO::Release }, false>(lock, false as u8);

            (start, end, zone, region)
        }
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
                    drop(Box::from_raw(cur_reg));
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

#[cfg(test)]
mod table_tests {
    use super::RegionTable;

    // TODO: test table operations

    #[test]
    fn resize_post() {
        static mut TABLE: RegionTable = RegionTable::new(8);

        unsafe {
            RegionTable::init(&raw mut TABLE);

            for i in 1..13 {
                RegionTable::append(
                    &raw mut TABLE,
                    i,
                    i * 2,
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                );
            }

            assert_eq!((*(&raw const TABLE)).cap, 16);

            match RegionTable::index(&raw mut TABLE, 2) {
                (3, 6, _, _) => (),
                _ => panic!(),
            }

            match RegionTable::index(&raw mut TABLE, 10) {
                (11, 22, _, _) => (),
                _ => panic!(),
            }
        }
    }
}

// TODO: separate zones for objects with static size and those with variable size?
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

/// Returns a pointer to `sz` unused bytes somewhere in the region `reg`
unsafe fn acquire_raw(reg: *mut Region, sz: u32) -> *mut u8 {
    unsafe {
        let region_ref = reg.as_mut().unwrap();
        let mut zone_ref = region_ref.head.as_mut().unwrap();

        let ptr = loop {
            // // advance if definitely no space
            // if zone_ref.used + sz >= region_ref.zone_size {
            //     // zone_ref = zone_advance(region_ref, zone_ref)
            // }

            // try the free tree
            /*else*/
            if let Some(ptr) = free_tree_seek(zone_ref, sz) {
                break ptr;
            }
            // try empty space
            else if let Some(ptr) = void_seek(zone_ref, sz) {
                break ptr;
            }

            // advance if all individual spaces too small
            zone_ref = zone_advance(region_ref, zone_ref)
        };

        assert!(ptr >= zone_ref.bot);
        assert!(ptr < zone_ref.end);

        ptr
    }
}

pub fn copy_direct(src: *mut SlHead, into: *mut Region, rc: u8) -> *mut SlHead {
    assert!(rc > 0);

    let sz = lblk_get_len(src);

    unsafe {
        let new = acquire_raw(into, sz);
        ptr::copy(src as *mut u8, new, sz as usize);
        ptr::write(new.add(1), rc);

        new as *mut SlHead
    }
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

    let obj_len =
        (HEAD_LEN + (type_fld_p as u32 * NUM_32_LEN) + (size_fld_p as u32 * NUM_32_LEN)) + size;

    unsafe {
        let ptr = acquire_raw(region, obj_len);

        // zero the whole object
        ptr::write_bytes(ptr, 0, obj_len as _);

        let ptr = ptr as *mut SlHead;

        let head = SlHead { cfg, rc: 1 };
        ptr::write_unaligned(ptr, head);

        if type_fld_p {
            ptr::write_unaligned((ptr as *mut u8).add(HEAD_LEN as usize) as *mut _, typ_id);
        }

        if size_fld_p {
            ptr::write_unaligned(
                (ptr as *mut u8).add((HEAD_LEN + (type_fld_p as u32 * NUM_32_LEN)) as _) as *mut _,
                size,
            );
        }

        if cfg!(feature = "memdbg") {
            let obj_id =
                std::intrinsics::atomic_xadd::<_, _, { AO::AcqRel }>(&raw mut OBJECT_ID_CTR, 1u32);
            ptr::write_unaligned((ptr as *mut u32).add(2), obj_id);

            let ty = if type_fld_p {
                format!("{typ_id:b}")
            } else {
                format!("{:?}", super::Cfg::try_from(maybe_cfg).unwrap())
            };

            println!("O {obj_id} BIRTH (p {:x} s {size:3} t {ty})", ptr as usize);
        }

        ptr
    }
}

macro_rules! znlck {
    ( $zone:expr => $($labl:lifetime :)? $body:block ENDLCK) => {{
        // let _zn_id = (*$zone).id;

        let lock: *mut u8 = &raw mut (*$zone).lock;
        while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
            lock,
            false as u8,
            true as u8).1
        {
            std::hint::spin_loop();
        }

        // TODO: add if cfg("lckdbg") debug messages using unique IDs
        // for each zone, so we can see which locks are in contention

        // println!("Z {_zn_id} LOCK (l {})", line!());

        let p = $($labl :)? {$body};

        std::intrinsics::atomic_store::<_, { AO::Release }, false>(lock, false as u8);

        // println!("Z {_zn_id} OPEN (l {})", line!());

        p
    }};
}

/// Return the next zone in a region, creating one if none exists
unsafe fn zone_advance<'a>(region_ref: &mut Region, zone_ref: &'a mut Zone) -> &'a mut Zone {
    unsafe {
        match zone_ref.next.as_mut() {
            Some(refer) => refer,
            None => {
                new_mem_zone(region_ref);
                region_ref.head.as_mut().unwrap()
            }
        }
    }
}

/// Attempt to acquire the space in a free block for a new live block
unsafe fn fblk_try_acq(
    zone_ref: &mut Zone,
    cursor: *mut FreeBlock,
    parent: *mut FreeBlock,
    precedes_parent: bool,
    obj_len: u32,
) -> Option<*mut u8> {
    unsafe {
        // NOTE: functions beginning in `fblk` must ONLY be used on a
        // locked zone

        let fb_len_cur = fblk_get_len(cursor);

        let replace = if fb_len_cur == obj_len {
            true
        } else if fb_len_cur >= obj_len + HEAD_LEN {
            false
        } else {
            return None;
        };

        if !replace {
            let fb_len_new = fb_len_cur - obj_len;
            fblk_set_len(cursor, fb_len_new);

            zone_ref.used += obj_len;

            return Some((cursor as *mut u8).add(fb_len_new as _));
        }

        let (cur_prev, cur_next) = (
            fblk_get_prev_blk(zone_ref, cursor),
            fblk_get_next_blk(zone_ref, cursor),
        );

        if parent.is_null() {
            match (cur_prev.is_null(), cur_next.is_null()) {
                (true, true) => zone_ref.free = ptr::null_mut(),
                (true, false) => zone_ref.free = cur_next,
                (false, true) => zone_ref.free = cur_prev,
                (false, false) => {
                    zone_ref.free = cur_prev;
                    insert_fblk_at(zone_ref, cur_prev, cur_next);
                }
            }
        } else {
            if precedes_parent {
                fblk_set_prev_blk(zone_ref, parent, ptr::null_mut())
            } else {
                fblk_set_next_blk(zone_ref, parent, ptr::null_mut())
            }

            if !cur_prev.is_null() {
                insert_fblk_at(zone_ref, parent, cur_prev)
            }
            if !cur_next.is_null() {
                insert_fblk_at(zone_ref, parent, cur_next)
            }
        }

        zone_ref.used += obj_len;

        Some(cursor as _)
    }
}

/// Seek a zone's free tree and return the first available space
/// for a live block of the given length
unsafe fn free_tree_seek(zone_ref: &mut Zone, obj_len: u32) -> Option<*mut u8> {
    unsafe {
        let mut cursor = zone_ref.free;
        let mut parent = ptr::null_mut();
        let mut precedes_parent = false;
        let mut stack = vec![];

        if cursor.is_null() {
            return None;
        }

        znlck!(zone_ref => 'cs: {

    cursor = zone_ref.free;
    if cursor.is_null() {
        break 'cs None;
    }

    loop {
        let result_here = fblk_try_acq(zone_ref, cursor, parent, precedes_parent, obj_len);
        if result_here.is_some() {
            break result_here;
        }

        let prev = fblk_get_prev_blk(zone_ref, cursor);
        if !prev.is_null() {
            stack.push((cursor, prev))
        }

        let next = fblk_get_next_blk(zone_ref, cursor);
        if !next.is_null() {
            (parent, cursor) = (cursor, next);
            precedes_parent = false;
            continue;
        }

        if !stack.is_empty() {
            (parent, cursor) = stack.pop().unwrap();
            precedes_parent = true;
            continue;
        }

        break None;
    }

    } ENDLCK)
    }
}

/// Return space for a live block of the given length from the empty
/// area at the end of a zone, if available
unsafe fn void_seek(zone_ref: &mut Zone, obj_len: u32) -> Option<*mut u8> {
    unsafe {
        if zone_ref.top.add(obj_len as _) > zone_ref.end {
            return None;
        }

        znlck!(zone_ref => 'cs: {

            let ptr = zone_ref.top;

            if ptr.add(obj_len as _) > zone_ref.end {
                break 'cs None;
            }

            zone_ref.used += obj_len;
            zone_ref.top = ptr.add(obj_len as _);

            Some(ptr)

        } ENDLCK)
    }
}

// TODO: replace *mut SlHead with nonnull, Option-wrappable SlPtr?

/// Lengthen an object; may write to a new address, in which case a
/// redirect is left if there is more than one reference
pub unsafe fn realloc(obj: *mut SlHead, size: u32) -> *mut SlHead {
    unsafe {
        let cur_len = lblk_get_len(obj);
        let cur_size = super::raw_size(obj);
        if size == cur_size {
            return obj;
        }

        if size < cur_size {
            panic!("cannot yet decrease object size")
        }

        let diff = size - cur_size;

        let size_fld_p = super::raw_siz_fld_p(obj);
        let type_fld_p = super::raw_typ_fld_p(obj);

        let new_len =
            (HEAD_LEN + (type_fld_p as u32 * NUM_32_LEN) + (size_fld_p as u32 * NUM_32_LEN)) + size;

        let (c_regn, c_zone) = which_mem_area(obj);

        let mut ret_ptr = obj;

        let qp = znlck!(*c_zone => {
        // first, check whether we happen to be at the end of the used area; extend if so
        if super::raw_val_ptr(obj).add(cur_size as _) == (*c_zone).top {
            let ptr = (*c_zone).top;
            (*c_zone).used += diff;
            (*c_zone).top = ptr.add(diff as _);
            ptr::write_bytes(ptr, 0, diff as _);
            true
        }
        // second, (maybe) check whether we happen to precede a free block; ""
        else if false {
            true
        } else { false }
        } ENDLCK);

        // last, find available space in the region, acquire it, and copy into it
        if !qp {
            let old_ptr = obj as *mut u8;
            let new_ptr = acquire_raw(c_regn, new_len);

            ptr::copy_nonoverlapping(old_ptr, new_ptr, cur_len as _);
            ptr::write_bytes(new_ptr.add(cur_len as _), 0, diff as _);

            // set refct of new loc to 2, for the redir and the return
            ptr::write(new_ptr.add(1), 2);

            ret_ptr = new_ptr as _;

            // in the final case, proceed by replacing the original space with a redirect
            let old_refc = ptr::read(old_ptr.add(1));
            ptr::write(
                old_ptr as *mut u64,
                super::Cfg::B0Redir as u64 + ((old_refc as u64) << 8) + ((new_ptr as u64) << 16),
            );

            // free all space not required for redirect (> 8 bytes)
            reclaim_raw(old_ptr.add(HEAD_LEN as _), cur_len - HEAD_LEN);
        }

        if size_fld_p {
            ptr::write_unaligned(
                (ret_ptr as *mut u8).add((HEAD_LEN + (type_fld_p as u32 * NUM_32_LEN)) as _)
                    as *mut _,
                size,
            );
        }

        ret_ptr
    }
}

/// Get total length of live block
fn lblk_get_len(blk: *mut SlHead) -> u32 {
    (HEAD_LEN
        + (super::raw_typ_fld_p(blk) as u32 * NUM_32_LEN)
        + (super::raw_siz_fld_p(blk) as u32 * NUM_32_LEN))
        + super::raw_size(blk)
}

/// Begin accounting a span somewhere in a region as free space
unsafe fn reclaim_raw(ptr: *mut u8, sz: u32) {
    unsafe {
        assert!(
            sz < 1u32.reverse_bits(),
            "attempt to reclaim too-large span"
        );

        let zone = which_mem_area(ptr as _).1;
        let zone_ref = zone.as_mut().unwrap();

        let newblk = ptr as *mut FreeBlock;

        // TODO: establish concurrency characteristics of alloc / dealloc

        znlck!(zone_ref => 'lb: {

    let trunk = zone_ref.free;

    if trunk.is_null() {
        // if at end of used space, just release into free space
        if (newblk as *mut u8).add(sz as _) == (*zone).top {
            // TODO: change these operations (multiple) to atomic?
            (*zone).top = newblk as _;

            // std::intrinsics::atomic_xsub_acqrel(&mut (*zone).used, sz);

            zone_ref.used -= sz;
        } else {
            fblk_set_len(newblk, sz);
            fblk_set_prev_ofs(newblk, FBLK_NULL_OFFSET);
            fblk_set_next_ofs(newblk, FBLK_NULL_OFFSET);

            // TODO: keep a safe queue of objects that require
            // deallocation, then guarantee that deallocation activity
            // will only be undertaken by a single thread. could also
            // use a free tree lock. when the lock is taken, reentrant
            // deallocation runs are prevented, as well as any
            // allocation not from the zone's empty area.

            // TODO: more clearly specify concurrency needs for zones

            let (chkt, done) = std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                &mut (*zone).free,
                ptr::null_mut(),
                newblk,
            );
            debug_assert!(done, "{:x}", chkt as u64);

            // std::intrinsics::atomic_xsub_acqrel(&mut (*zone).used, sz);

            zone_ref.used -= sz;
        }
        break 'lb
    }

    let (mut lower_bound, mut upper_bound, mut lb_parent, mut ub_parent) = {
        if newblk < trunk {
            (ptr::null_mut(), trunk, ptr::null_mut(), ptr::null_mut())
        } else if newblk > trunk {
            (trunk, ptr::null_mut(), ptr::null_mut(), ptr::null_mut())
        } else {
            panic!("double free attempted")
        }
    };

    'sk: loop {
        let (ub_last, lb_last) = (upper_bound, lower_bound);

        if !lower_bound.is_null() {
            let prospect = fblk_get_next_blk(zone, lower_bound);
            if prospect.is_null() {
            } else if prospect > lower_bound && prospect < newblk {
                lb_parent = lower_bound;
                lower_bound = prospect;
            } else if prospect > lower_bound && prospect > newblk {
                ub_parent = lower_bound;
                upper_bound = prospect;
            } else {
                panic!()
            }
        }

        if !upper_bound.is_null() {
            let prospect = fblk_get_prev_blk(zone, upper_bound);
            if prospect.is_null() {
            } else if prospect < upper_bound && prospect < newblk {
                lb_parent = upper_bound;
                lower_bound = prospect;
            } else if prospect < upper_bound && prospect > newblk {
                ub_parent = upper_bound;
                upper_bound = prospect;
            } else {
                panic!()
            }
        }

        if lb_last == lower_bound && ub_last == upper_bound {
            break 'sk;
        }
    }

    const BD_MAX: usize = FBLK_NULL_OFFSET as usize;
    const MIN_FB: usize = HEAD_LEN as usize;
    const LTM_FB: usize = HEAD_LEN as usize - 1;

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
            1..=LTM_FB => panic!("memory management failed"),
            MIN_FB..=BD_MAX => false,
            _ => panic!(),
        }
    };

    let follow_merge = 'n: {
        if upper_bound.is_null() {
            break 'n false;
        }

        assert!(upper_bound > newblk);

        let nw_end = newblk as usize + sz as usize;
        let ub_start = upper_bound as usize;
        let diff = ub_start - nw_end;

        match diff {
            0 => true,
            1..=LTM_FB => panic!("memory management failed"),
            MIN_FB..=BD_MAX => false,
            _ => panic!(),
        }
    };

    // All tree manipulation is restricted to this block
    let _final_sz = match (prior_merge, follow_merge) {
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

            let full_sz = sz + fblk_get_len(lower_bound) + fblk_get_len(upper_bound);
            fblk_set_len(lower_bound, full_sz);

            full_sz
        }
        (true, false) => {
            if newblk as usize + sz as usize == (*zone).top as usize {
                let (lbprev, lbnext) = (
                    fblk_get_prev_blk(zone, lower_bound),
                    fblk_get_next_blk(zone, lower_bound),
                );

                assert!(lbnext.is_null());

                if !lb_parent.is_null() {
                    assert!(lower_bound > lb_parent);
                    assert!(lbprev.is_null() || lbprev > lb_parent);
                    assert_eq!(lower_bound, fblk_get_next_blk(zone, lb_parent));
                    fblk_set_next_blk(zone, lb_parent, lbprev)
                }

                if lower_bound == trunk {
                    (*zone).free = lbprev
                }

                (*zone).top = lower_bound as _;

                0
            } else {
                let full_sz = sz + fblk_get_len(lower_bound);
                fblk_set_len(lower_bound, full_sz);

                full_sz
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

            let full_sz = sz + fblk_get_len(upper_bound);
            fblk_set_len(newblk, full_sz);

            full_sz
        }
        (false, false) => {
            if (newblk as *mut u8).add(sz as _) == (*zone).top {
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
                    insert_fblk_at(zone, trunk, newblk)
                }

                fblk_set_prev_ofs(newblk, FBLK_NULL_OFFSET);
                fblk_set_next_ofs(newblk, FBLK_NULL_OFFSET);
                fblk_set_len(newblk, sz);

                sz
            }
        }
    };

            std::intrinsics::atomic_xsub::<_, _, { AO::AcqRel }>(&mut (*zone).used, sz);

    if cfg!(feature = "memdbg") {
        // __dbg_visualize_zone(zone)
    }

    } ENDLCK);
    }
}

/// Adds memory that used to hold a Sail object to the freed memory
/// structure in its zone
pub unsafe fn dealloc(val: *mut SlHead) {
    unsafe {
        if cfg!(feature = "memdbg") {
            let obj_id = ptr::read_unaligned((val as *mut u32).add(2));
            println!("O {obj_id} RECLAIM")
        }

        // TODO: panic if object appears to exceed zone's max obj size

        reclaim_raw(val as _, lblk_get_len(val))
    }
}

unsafe fn insert_fblk_at(zone: *const Zone, tgt: *mut FreeBlock, blk: *mut FreeBlock) {
    unsafe {
        assert_ne!(tgt, ptr::null_mut());
        assert_ne!(blk, ptr::null_mut());

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
}

/// Set the length of a free memory block in a zone
unsafe fn fblk_set_len(block: *mut FreeBlock, length: u32) {
    unsafe {
        assert!(!block.is_null());
        assert!(length >= HEAD_LEN);

        let exceeds_head = length > HEAD_LEN;
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
                    ptr::write_unaligned(
                        block.add(HEAD_LEN as usize + 1) as *mut u32,
                        length as u32,
                    );
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
}

/// Get the length stored in a free memory block
unsafe fn fblk_get_len(block: *const FreeBlock) -> u32 {
    unsafe {
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
            HEAD_LEN
        }
    }
}

// Free block head scheme: size bit, prev offset, parity bit, next offset

/// MSB 0; all other bits 1
const FBLK_NULL_OFFSET: u32 = (!1u32).reverse_bits();

/// Set a free block's offset pointer to the previous block
unsafe fn fblk_set_prev_ofs(block: *mut FreeBlock, offset: u32) {
    unsafe {
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
}

/// Get a free block's offset pointer to the previous block
unsafe fn fblk_get_prev_ofs(block: *const FreeBlock) -> u32 {
    unsafe {
        let head_ptr = block as *mut u64;
        let head = ptr::read_unaligned(head_ptr);

        // odd parity check
        assert_eq!(head.count_ones() & 1, 1);

        (head >> 32) as u32 & 0x7FFFFFFF
    }
}

/// Set a free block's offset pointer to the next block
unsafe fn fblk_set_next_ofs(block: *mut FreeBlock, offset: u32) {
    unsafe {
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
}

/// Get a free block's offset pointer to the next block
unsafe fn fblk_get_next_ofs(block: *const FreeBlock) -> u32 {
    unsafe {
        let head_ptr = block as *mut u64;
        let head = ptr::read_unaligned(head_ptr);

        // odd parity check
        assert_eq!(head.count_ones() & 1, 1);

        head as u32 & 0x7FFFFFFF
    }
}

/// Set which block a free block points to as the prior block
unsafe fn fblk_set_prev_blk(zone: *const Zone, block: *mut FreeBlock, prev: *const FreeBlock) {
    unsafe {
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
}

/// Get the block a free block points to as the prior block
unsafe fn fblk_get_prev_blk(zone: *const Zone, block: *const FreeBlock) -> *mut FreeBlock {
    unsafe {
        debug_assert_eq!(which_mem_area(block as *mut _).1 as *const _, zone);

        let start = (*zone).bot;
        let prev_ofs = fblk_get_prev_ofs(block);

        if prev_ofs == FBLK_NULL_OFFSET {
            return ptr::null_mut();
        }

        assert!(prev_ofs < FBLK_NULL_OFFSET);

        let out = (start as u64 + prev_ofs as u64) as *mut FreeBlock;
        debug_assert_eq!(which_mem_area(out as *mut _).1 as *const _, zone);
        out
    }
}

/// Set which block a free block points to as the following block
unsafe fn fblk_set_next_blk(zone: *const Zone, block: *mut FreeBlock, next: *const FreeBlock) {
    unsafe {
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
}

/// Get the block a free block points to as the following block
unsafe fn fblk_get_next_blk(zone: *const Zone, block: *const FreeBlock) -> *mut FreeBlock {
    unsafe {
        debug_assert_eq!(which_mem_area(block as *mut _).1 as *const _, zone);

        let start = (*zone).bot;
        let next_ofs = fblk_get_next_ofs(block);

        if next_ofs == FBLK_NULL_OFFSET {
            return ptr::null_mut();
        }

        assert!(next_ofs < FBLK_NULL_OFFSET);

        let out = (start as u64 + next_ofs as u64) as *mut FreeBlock;
        debug_assert_eq!(which_mem_area(out as *mut _).1 as *const _, zone);
        out
    }
}

fn __dbg_visualize_zone(zone: *const Zone) {
    unsafe {
        println!("--- Zone Report: {:x} ---", zone as usize);

        println!(
            "Offset to empty area: {}",
            (*zone).top as usize - (*zone).bot as usize
        );

        let trunk = (*zone).free;

        let mut free_blocks = if trunk == ptr::null_mut() {
            println!("No Trunk");
            vec![]
        } else {
            // (block address, (parent by index, prev or next))
            vec![(trunk, None)]
        };

        let mut n = 0;

        while n < free_blocks.len() {
            let (fb, co) = free_blocks[n];

            let prev = fblk_get_prev_blk(zone, fb);
            if !prev.is_null() {
                free_blocks.push((prev, Some((n, false))))
            }
            let next = fblk_get_next_blk(zone, fb);
            if !next.is_null() {
                free_blocks.push((next, Some((n, true))))
            }

            println!(
                "FBlock {}: offset {}; length {}; {}",
                n,
                fb as usize - (*zone).bot as usize,
                fblk_get_len(fb),
                match co {
                    None => String::from("TRUNK"),
                    Some((x, p)) => format!(
                        "{} child of {}",
                        if p { "following" } else { "preceding" },
                        x
                    ),
                }
            );

            n += 1;
        }

        free_blocks.sort_by_key(|e| e.0);

        let mut probe_pos = (*zone).bot as usize;

        for fb in free_blocks.iter().map(|e| e.0 as usize) {
            while probe_pos < fb {
                let len = lblk_get_len(probe_pos as *mut SlHead) as usize;
                println!(
                    "LBlock: offset {}; length {}",
                    probe_pos - (*zone).bot as usize,
                    len
                );
                probe_pos += len;
                assert!(probe_pos <= fb)
            }
            probe_pos = fb + fblk_get_len(fb as *mut FreeBlock) as usize
        }

        while probe_pos < (*zone).top as usize {
            let len = lblk_get_len(probe_pos as *mut SlHead) as usize;
            println!(
                "LBlock: offset {}; length {}",
                probe_pos - (*zone).bot as usize,
                len
            );
            probe_pos += len;
            assert!(probe_pos <= (*zone).top as usize)
        }

        println!()
    }
}

#[cfg(test)]
mod free_tree_tests {
    use super::*;

    #[test]
    fn set_block_fields() {
        let mut test_block = FreeBlock { prev: 0, next: 0 };

        unsafe {
            let block_ptr = &mut test_block;

            fblk_set_len(block_ptr, HEAD_LEN);
            fblk_set_prev_ofs(block_ptr, 101);
            fblk_set_next_ofs(block_ptr, 202);

            assert_eq!(HEAD_LEN, fblk_get_len(block_ptr));
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
            assert_eq!(42, ptr::read((ptr as *mut u8).add(HEAD_LEN as _)));

            fblk_set_len(ptr, 1234567);

            assert_eq!(1234567, fblk_get_len(ptr));
            assert_eq!(0, ptr::read((ptr as *mut u8).add(HEAD_LEN as _)));
            assert_eq!(
                1234567,
                ptr::read_unaligned((ptr as *mut u8).add(HEAD_LEN as usize + 1) as *mut u32)
            );

            assert_eq!(222222, fblk_get_prev_ofs(ptr));
            assert_eq!(888888, fblk_get_next_ofs(ptr));
        }
    }

    #[test]
    fn dealloc_in_zone() {
        use super::super::Cfg;

        unsafe {
            let region = Region::acq(1000);

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


// TODO: store zone location information?
// minimum zone size 2^8; maximum zone size 2^31; only powers of 2
// permitted, only 5 bits needed to specify.

// first field: initial 56 bits contain the prefix (first 16 bits will
// be empty). last 6 bits give the length of the prefix. bits 6 and 7
// (from right) signal type of following addresses; 7 for n0, 6 for
// n1; HIGH means a zone pointer (so test against zone address with
// known size), low means another node

const PTR_BIT_W: usize = super::PTR_LEN as usize * 8;

struct AddrPfxNode {
    // NOTE: bits?
    pfx: PfxInfo,
    slots: [PfxTgt; 2],
}

#[derive(Clone, Copy)]
#[repr(transparent)]
struct PfxInfo(usize);

impl PfxInfo {
    fn breakout(self) -> (usize, u8, bool, bool) {
        let width = (self.0 & 0b00111111) as u8;
        let slot_0_zone_p = self.0 & 0b10000000 != 0;
        let slot_1_zone_p = self.0 & 0b01000000 != 0;

        let prefix = take_msb_span(self.0, width);

        (prefix, width as u8, slot_0_zone_p, slot_1_zone_p)
    }
}

union PfxTgt {
    node: ptr::NonNull<AddrPfxNode>,
    zone: ptr::NonNull<Zone>,
}

fn cons_pfx(val: usize, width: u8, s0zp: bool, s1zp: bool) -> PfxInfo {
    let pfx_f = take_msb_span(val, width);
    let wid_f = (width & 0b00111111) as usize;
    let s0z_f = if s0zp { 0b10000000 } else { 0 };
    let s1z_f = if s1zp { 0b01000000 } else { 0 };

    PfxInfo(pfx_f + wid_f + s0z_f + s1z_f)
}

fn take_msb_span(val: usize, width: u8) -> usize {
    let w = width as usize;
    val & (((1 << w) - 1) << (PTR_BIT_W - w))
}

/// returns width of overlap from MSB end
fn find_overlap(va: usize, vb: usize) -> u8 {
    ((PTR_BIT_W as u32 - 1) - (va ^ vb).highest_one().unwrap()) as _
}

fn next_bit(val: usize, width: u8) -> bool {
    val & (1 << ((PTR_BIT_W - 1) - width as usize)) != 0
}

/// A memory region is a linked list of memory zones, all of the same size
#[repr(C)]
pub struct Region {
    /// pointer to top Zone
    list_head: *mut Zone,
    /// root AddrPfxNode; None if only one zone
    tree_root: Option<ptr::NonNull<AddrPfxNode>>,
    /// lock for the residence tree
    tree_lock: u8,
    /// common zone size as a power of 2
    zone_order: u8,
    // TODO: list of small objects for reuse?
    // TODO: list of big separate objects?
}

impl std::fmt::Debug for Region {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sp = self as *const Self;
        let zn_top = sp.get_list_head();
        let zn_ord = sp.zn_ord();
        let tree = sp.get_tree_root().map_or(ptr::null_mut(), |p| p.as_ptr());
        write!(
            f,
            "{{ zone order: {}; top zone addr: {:x}; tree addr: {:x} }}",
            zn_ord, zn_top as usize, tree as usize
        )
    }
}

impl Region {
    pub fn hdl(self: *mut Self) -> util::Hdl<Region> {
        // reference count?
        util::Hdl::new(self)
    }

    pub fn new(zone_order: u8) -> Self {
        match zone_order {
            8..=31 => (),
            z => panic!("invalid zone order {z}"),
        }

        Self {
            list_head: ptr::null_mut(),
            tree_root: None,
            tree_lock: false as u8,
            zone_order,
        }
    }

    pub fn init(self: *mut Self) {
        assert_eq!(self.get_list_head(), ptr::null_mut());
        self.add_zone();
    }

    /// Returns *previous* head zone
    fn set_head_zone(self: *mut Self, p: *mut Zone) -> *mut Zone {
        debug_assert!(p as usize & u8::MAX as usize == 0);
        unsafe {
            (*p).next = ptr::null_mut();
            loop {
                let top = self.get_list_head();
                (*p).next = top;

                if std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                    &raw mut (*self).list_head,
                    top,
                    p,
                )
                .1
                {
                    break top;
                }
            }
        }
    }

    fn get_list_head(self: *const Self) -> *mut Zone {
        unsafe {
            std::intrinsics::atomic_load::<_, { AO::Acquire }, false>(
                &raw const ((*self).list_head),
            )
        }
    }

    pub fn zn_ord(self: *const Self) -> u8 {
        unsafe { (*self).zone_order }
    }

    pub fn zone_size(self: *const Self) -> u32 {
        1u32 << self.zn_ord()
    }

    fn set_tree_root(self: *mut Self, ptr: *mut AddrPfxNode) {
        if !self.chk_tree_lck() {
            panic!("tree not locked");
        }

        unsafe {
            (*self).tree_root = ptr::NonNull::new(ptr);
        }
    }

    fn get_tree_root(self: *const Self) -> Option<ptr::NonNull<AddrPfxNode>> {
        unsafe { (*self).tree_root }
    }

    #[inline(always)]
    fn acq_tree_lck(self: *mut Self) {
        unsafe {
            while !std::intrinsics::atomic_cxchg::<_, { AO::Acquire }, { AO::Acquire }>(
                &raw mut (*self).tree_lock,
                false as u8,
                true as u8,
            )
            .1
            {
                std::hint::spin_loop();
            }
        }
    }

    #[inline(always)]
    fn rel_tree_lck(self: *mut Self) {
        unsafe {
            std::intrinsics::atomic_store::<_, { AO::Release }, false>(
                &raw mut (*self).tree_lock,
                false as u8,
            );
        }
    }

    fn chk_tree_lck(self: *mut Self) -> bool {
        unsafe {
            std::intrinsics::atomic_load::<_, { AO::Acquire }, false>(&raw mut (*self).tree_lock)
                != 0
        }
    }

    // TODO: this function must be reentrant
    /// Adds a zone to the top of the Region's list. Returns a pointer to the new zone.
    pub fn add_zone(self: *mut Self) -> *mut Zone {
        if cfg!(feature = "memdbg") {
            log::debug!("Creating mem zone");
        }

        debug_assert_eq!(1usize.highest_one(), Some(0));
        debug_assert_eq!(usize::MAX.highest_one(), Some(PTR_BIT_W as u32 - 1));

        let size = 1usize << (self.zn_ord());

        let new_zone_ptr = unsafe {
            let layout = alloc::Layout::from_size_align_unchecked(size, size);
            alloc::alloc(layout) as *mut Zone
        };

        assert_eq!(new_zone_ptr as usize & (size - 1), 0);

        unsafe {
            let start = (new_zone_ptr as *mut u8).add(MEM_ZONE_HEAD_SIZE);
            let end = (new_zone_ptr as *mut u8).add(size as _);

            RegionTable::append(
                &raw mut REGION_TABLE,
                start as usize,
                end as usize,
                new_zone_ptr,
                self,
            );

            let zn_id =
                std::intrinsics::atomic_xadd::<u32, u32, { AO::AcqRel }>(&raw mut ZONE_ID_CTR, 1);

            let new_head = Zone {
                used: 0,
                id: zn_id,
                free: ptr::null_mut(),
                next: ptr::null_mut(),
                top: start,
                end,
                bot: start,
                lock: false as u8,
            };

            ptr::write(new_zone_ptr, new_head);
        }

        // no local objects will be checked for residence before the
        // tree has been mutated
        let next_zone = self.set_head_zone(new_zone_ptr);

        self.acq_tree_lck();

        fn fresh_node(pfx: PfxInfo, slots: [PfxTgt; 2]) -> *mut AddrPfxNode {
            let datum = AddrPfxNode { pfx, slots };
            unsafe {
                let ptr = alloc::alloc(alloc::Layout::new::<AddrPfxNode>()) as *mut AddrPfxNode;
                ptr.write(datum);
                ptr
            }
        }

        // TODO: region tree instead of region table

        // TODO: break tree insertion out into another function
        if !next_zone.is_null() {
            match self.get_tree_root() {
                None => {
                    let pfx_width = find_overlap(new_zone_ptr as usize, next_zone as usize);
                    let pfx = cons_pfx(new_zone_ptr as usize, pfx_width, true, true);

                    let (zna, znb) = (
                        ptr::NonNull::new(next_zone).unwrap(),
                        ptr::NonNull::new(new_zone_ptr).unwrap(),
                    );

                    let slots = if next_bit(new_zone_ptr as usize, pfx_width) {
                        assert!(!next_bit(next_zone as usize, pfx_width));
                        [PfxTgt { zone: zna }, PfxTgt { zone: znb }]
                    } else {
                        assert!(next_bit(next_zone as usize, pfx_width));
                        [PfxTgt { zone: znb }, PfxTgt { zone: zna }]
                    };

                    let new_root = fresh_node(pfx, slots);

                    self.set_tree_root(new_root);
                }
                Some(rnn) => {
                    let mut root_node = rnn;
                    let mut cur_node = &raw mut root_node;

                    // begin looping traversal of the tree
                    loop {
                        let node_ptr = unsafe { *cur_node }.as_ptr();

                        let (cur_prefix, cur_width, cur_s0zp, cur_s1zp) =
                            unsafe { (*node_ptr).pfx }.breakout();

                        let diff_bit = ((new_zone_ptr as usize) ^ cur_prefix)
                            .highest_one()
                            .unwrap_or(0);

                        if diff_bit > ((PTR_BIT_W as u8 - 1) - cur_width) as u32 {
                            // in this case, replace present node (it gets linked as one of the leaves)
                            let new_width = (PTR_BIT_W as u8 - 1) - diff_bit as u8;

                            let (zna, znb) = (
                                ptr::NonNull::new(node_ptr).unwrap(),
                                ptr::NonNull::new(new_zone_ptr).unwrap(),
                            );

                            let (pfx, slots) = if cur_prefix & (1 << diff_bit) == 0 {
                                assert_ne!((new_zone_ptr as usize) & (1 << diff_bit), 0);
                                (
                                    cons_pfx(new_zone_ptr as usize, new_width, false, true),
                                    [PfxTgt { node: zna }, PfxTgt { zone: znb }],
                                )
                            } else {
                                assert_eq!((new_zone_ptr as usize) & (1 << diff_bit), 0);
                                (
                                    cons_pfx(new_zone_ptr as usize, new_width, true, false),
                                    [PfxTgt { zone: znb }, PfxTgt { node: zna }],
                                )
                            };

                            let new_node = fresh_node(pfx, slots);

                            // if we need to replace the root node, do it separately
                            if root_node.as_ptr() == node_ptr {
                                self.set_tree_root(new_node)
                            } else {
                                unsafe {
                                    ptr::write(cur_node, ptr::NonNull::new_unchecked(new_node))
                                }
                            }

                            break;
                        } else {
                            // in this case, select which leaf to point to as our next site
                            let (slot_i, zone_p) = if !next_bit(new_zone_ptr as usize, cur_width) {
                                (0, cur_s0zp)
                            } else {
                                (1, cur_s1zp)
                            };

                            if zone_p {
                                let shift_zone = unsafe { (*node_ptr).slots[slot_i].zone }.as_ptr();
                                let new_width =
                                    find_overlap(new_zone_ptr as usize, shift_zone as usize);
                                let pfx = cons_pfx(new_zone_ptr as usize, new_width, true, true);

                                let (zna, znb) = (
                                    ptr::NonNull::new(shift_zone).unwrap(),
                                    ptr::NonNull::new(new_zone_ptr).unwrap(),
                                );

                                let slots = if next_bit(new_zone_ptr as usize, new_width) {
                                    assert!(!next_bit(shift_zone as usize, new_width));
                                    [PfxTgt { zone: zna }, PfxTgt { zone: znb }]
                                } else {
                                    assert!(next_bit(shift_zone as usize, new_width));
                                    [PfxTgt { zone: znb }, PfxTgt { zone: zna }]
                                };

                                let new_node = fresh_node(pfx, slots);

                                unsafe {
                                    // make sure the current node metadata shows the child node
                                    ptr::write(
                                        &raw mut (*node_ptr).pfx,
                                        if slot_i == 0 {
                                            cons_pfx(cur_prefix, cur_width, false, cur_s1zp)
                                        } else {
                                            cons_pfx(cur_prefix, cur_width, cur_s0zp, false)
                                        },
                                    );
                                    ptr::write(
                                        &raw mut (*node_ptr).slots[slot_i].node,
                                        ptr::NonNull::new_unchecked(new_node),
                                    );
                                }

                                break;
                            } else {
                                unsafe {
                                    cur_node = &raw mut (*node_ptr).slots[slot_i].node;
                                }
                                continue;
                            }
                        }
                    }
                }
            }
        }

        // println!("{:x}", self as usize);
        // self.depict_tree();

        self.rel_tree_lck();

        new_zone_ptr
    }

    fn depict_tree(self: *const Self) {
        let tree = self.get_tree_root();

        if tree.is_none() {
            println!("ONE ZONE");
            return;
        }

        fn depict_tree_1(node: ptr::NonNull<AddrPfxNode>, l: usize) {
            let (pfx, w, z0, z1) = unsafe { (*node.as_ptr()).pfx }.breakout();

            let sp = String::from_utf8(vec![b' '; 2 * l]).unwrap();
            println!("{sp}Node: {:0x}; w{w}", pfx);

            if z0 {
                let zp = unsafe { (*node.as_ptr()).slots[0].zone };
                println!("{sp} Zone: {:0x}", zp.as_ptr() as usize);
            } else {
                let np = unsafe { (*node.as_ptr()).slots[0].node };
                depict_tree_1(np, l + 1);
            }

            if z1 {
                let zp = unsafe { (*node.as_ptr()).slots[1].zone };
                println!("{sp} Zone: {:0x}", zp.as_ptr() as usize);
            } else {
                let np = unsafe { (*node.as_ptr()).slots[1].node };
                depict_tree_1(np, l + 1);
            }
        }

        depict_tree_1(tree.unwrap(), 0)
    }

    fn resident_p(self: *const Self, loc: *mut u8) -> Option<ptr::NonNull<Zone>> {
        if self.get_list_head().is_null() {
            return None;
        }

        let tree = self.get_tree_root();

        let zw = PTR_BIT_W as u8 - self.zn_ord();

        fn test_pfx(pfx: usize, tgt: usize, w: u8) -> bool {
            let pmask = ((1 << w as usize) - 1) << (PTR_BIT_W - w as usize);

            let prefix_expect = pmask & pfx;
            let prefix_exist = pmask & tgt;

            prefix_exist == prefix_expect
        }

        if tree.is_none() {
            let addr = self.get_list_head();
            return if test_pfx(addr as usize, loc as usize, zw) {
                Some(unsafe { ptr::NonNull::new_unchecked(addr) })
            } else {
                None
            };
        }

        let mut tree = tree.unwrap();
        loop {
            let pfx = unsafe { (*tree.as_ptr()).pfx };
            let (_, width, s0zp, s1zp) = pfx.breakout();
            debug_assert!(width <= (PTR_BIT_W as u8 - 8));

            if !test_pfx(pfx.0, loc as usize, width) {
                return None;
            }

            let nmask = 1 << ((PTR_BIT_W - 1) - width as usize);
            let (slot_i, zone_p) = if nmask & loc as usize == 0 {
                (0, s0zp)
            } else {
                (1, s1zp)
            };

            if zone_p {
                let addr = unsafe { (*tree.as_ptr()).slots[slot_i].zone };
                return if test_pfx(addr.as_ptr() as usize, loc as usize, zw) {
                    Some(addr)
                } else {
                    None
                };
            } else {
                tree = unsafe { (*tree.as_ptr()).slots[slot_i].node }
            }
        }
    }
}

impl Drop for Region {
    fn drop(&mut self) {
        unsafe {
            let z_layout = alloc::Layout::from_size_align_unchecked(
                self.zone_size as usize + MEM_ZONE_HEAD_SIZE,
                1,
            );
            let mut next = self.head;
            loop {
                let cur = next;
                // TODO: fix unaligned zones
                next = (*cur).next;
                if next == ptr::null_mut() {
                    break;
                }
                alloc::dealloc(cur as *mut u8, z_layout);
            }
        }
    }
}

// NOTE: Maximum zone size: 2GiB
// NOTE: Maximum object size: 1/16 of zone size
// TODO: special allocation scheme for larger objects

/// A zone is a contiguous chunk of memory in which Sail objects may
/// be allocated
#[derive(Debug)]
#[repr(C)]
pub struct Zone {
    /// Size of used portion
    pub used: u32,
    /// Unique zone identifier
    id: u32,
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

const MEM_ZONE_HEAD_SIZE: usize = mem::size_of::<Zone>();

// TODO: this tests every zone, no logic; we can do better!
/// Returns the region and zone in which a given Sail object is stored
unsafe fn which_mem_area(ptr: *mut SlHead) -> (*mut Region, *mut Zone) {
    unsafe {
        assert_ne!(ptr, ptr::null_mut());

        let pos = ptr as usize;

        let mut idx = 0;
        loop {
            if idx >= RegionTable::len(&raw mut REGION_TABLE) {
                break;
            }

            let entry = RegionTable::index(&raw mut REGION_TABLE, idx);
            if pos >= entry.0 && pos < entry.1 {
                return (entry.3, entry.2);
            }

            idx += 1;
        }

        panic!("INVALID memory area for {:x}", ptr as usize)
    }
}

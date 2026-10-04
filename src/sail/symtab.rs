// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2024 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/symtab.rs

// A table to associate symbol names with efficient internal IDs.

// <>

// TODO: switch the system to lexical binding, to prevent "spooky
// action from within", or procedures changing variables in the
// calling scope; procedures should always either be pure (no
// environment besides arguments), or closures (carrying their
// creation environment with them)

use std::{alloc, mem, ptr, slice};

use super::Hdl;

use std::intrinsics::AtomicOrdering as AO;

/// A symbol table, to map symbol strings to symbol IDs and vice versa
///
/// This is a bimap, a 1 to 1 association between strings and IDs.
/// Two maps, one for each direction, pointing to the same set of
/// cells: (id string).
pub struct SymbolTable {
    // pointers to SymEntry
    id_to_nm: *mut *mut u8,
    nm_to_id: *mut *mut u8,

    // these could be u32, considering the symbol namespace
    map_len: usize,
    // maintain low load ratio
    load: usize,

    // start of entry space
    entries: *mut u8,
    // size of entry space, in bytes
    espace: usize,
    // bump allocator head
    btop: *mut u8,

    next_id: u32,
    ins_lock: u8,
    rsz_lock: u8,
}

unsafe impl Send for SymbolTable {}
unsafe impl Sync for SymbolTable {}

struct _SymEntry {
    id: u32,
    len: u16,
    name: [u8],
}

// multithread protections

// only one insertion can happen at a time, but they do not interfere
// with any other memory, so lookups should continue without issue

// a resize changes all the addresses and associations, so any
// positions calculated before (or during) a resize are invalidated

// normal conditions: no restrictions, lookup anytime

// active insertion: no other insertions, may lookup

// resize generate: no insertions, may still lookup

// resize final: no operations

impl Drop for SymbolTable {
    fn drop(&mut self) {
        eprintln!("DROP Symbol Table");
        unsafe {
            let tab_layout = alloc::Layout::from_size_align_unchecked(self.map_len * USA, USA);
            let zone_layout = alloc::Layout::from_size_align_unchecked(self.espace, USA);

            alloc::dealloc(self.id_to_nm as *mut u8, tab_layout);
            alloc::dealloc(self.nm_to_id as *mut u8, tab_layout);
            alloc::dealloc(self.entries, zone_layout);
        }
    }
}

const USA: usize = mem::align_of::<usize>();
const U32S: usize = mem::size_of::<u32>();
const U16S: usize = mem::size_of::<u16>();

impl SymbolTable {
    pub fn new(cap: usize) -> Self {
        // TODO: always use a prime table size
        let tab_byte_size = cap * USA;

        let zone_byte_size = {
            let approx = cap * 11;
            approx - (approx % USA)
        };

        assert_eq!(tab_byte_size % USA, 0);
        assert_eq!(zone_byte_size % USA, 0);
        let tab_mem_layout = alloc::Layout::from_size_align(tab_byte_size, USA).unwrap();
        let zone_mem_layout = alloc::Layout::from_size_align(zone_byte_size, USA).unwrap();

        let (tab_a_ptr, tab_b_ptr, entry_zone_ptr) = unsafe {
            (
                alloc::alloc_zeroed(tab_mem_layout),
                alloc::alloc_zeroed(tab_mem_layout),
                alloc::alloc(zone_mem_layout),
            )
        };

        Self {
            id_to_nm: tab_a_ptr as _,
            nm_to_id: tab_b_ptr as _,

            map_len: tab_byte_size / USA,
            load: 0,

            entries: entry_zone_ptr,
            espace: zone_byte_size,
            btop: entry_zone_ptr,

            next_id: 0,
            ins_lock: false as u8,
            rsz_lock: false as u8,
        }
    }

    // TODO: resize according to a progression of known primes!
    // NOTE: hashing is FAR slower with sizes not known at compile
    fn resize(self: Hdl<Self>, factor: usize) {
        // acquire insertion lock
        // unsafe {
        //     while !std::intrinsics::atomic_cxchg::<_, { AO::Acquire }, { AO::Acquire }>(
        //         &raw mut (*self.raw()).ins_lock,
        //         false as u8,
        //         true as u8,
        //     )
        //     .1
        //     {}
        // }

        let (old_tab_size, old_zone_size, old_ta_ptr, old_tb_ptr, old_ez_ptr) = unsafe {
            (
                (*self.raw()).map_len * USA,
                (*self.raw()).espace,
                (*self.raw()).id_to_nm,
                (*self.raw()).nm_to_id,
                (*self.raw()).entries,
            )
        };

        // create tables of new size
        let new_tab_size = old_tab_size * factor;
        let new_tab_len = new_tab_size / USA;

        let new_zone_size = {
            let approx = new_tab_len * 11;
            approx - (approx % USA)
        };

        let new_tab_layout = alloc::Layout::from_size_align(new_tab_size, USA).unwrap();
        let new_zone_layout = alloc::Layout::from_size_align(new_zone_size, USA).unwrap();

        let (ntapt, ntbpt, nezpt) = unsafe {
            (
                alloc::alloc_zeroed(new_tab_layout),
                alloc::alloc_zeroed(new_tab_layout),
                alloc::alloc(new_zone_layout),
            )
        };

        // write current entries into entry zone
        let entry_byte_ct;
        unsafe {
            entry_byte_ct = (*self.raw()).btop as usize - (*self.raw()).entries as usize;
            ptr::copy_nonoverlapping((*self.raw()).entries, nezpt, entry_byte_ct);
        }

        // re-hash ids and names; place pointers in tables
        unsafe {
            let mut zcur = (*self.raw()).entries;
            let mut offset = 0;
            while zcur < (*self.raw()).btop {
                // step through entries from start to end
                let cur_id = ptr::read_unaligned(zcur as *const u32);
                let cur_name = Self::e_name_slice(zcur);

                assert_eq!(
                    cur_id,
                    ptr::read_unaligned(nezpt.byte_add(offset) as *const u32)
                );

                // hash id; place pointer in id . name map
                let id_nm_tgt = Self::hash_id(cur_id) % new_tab_len;
                let id_nm_wp = Self::probe_gen(ntapt as _, new_tab_len, id_nm_tgt);
                ptr::write(id_nm_wp, nezpt.byte_add(offset));

                // hash name; place pointer in name . id map
                let nm_id_tgt = Self::hash_name(cur_name) % new_tab_len;
                let nm_id_wp = Self::probe_gen(ntbpt as _, new_tab_len, nm_id_tgt);
                ptr::write(nm_id_wp, nezpt.byte_add(offset));

                let elen = U32S + U16S + cur_name.len();

                zcur = zcur.byte_add(elen);
                offset += elen;
            }

            assert_eq!(zcur, (*self.raw()).btop);
            assert_eq!(offset, entry_byte_ct);
        }

        unsafe {
            // acquire resize lock
            while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                &raw mut (*self.raw()).rsz_lock,
                false as u8,
                true as u8,
            )
            .1
            {}

            // replace table pointers, entry zone pointers, and map len
            (*self.raw()).id_to_nm = ntapt as _;
            (*self.raw()).nm_to_id = ntbpt as _;

            (*self.raw()).entries = nezpt;
            (*self.raw()).espace = new_zone_size;
            (*self.raw()).btop = nezpt.byte_add(entry_byte_ct);

            (*self.raw()).map_len = new_tab_len;

            // release resize lock
            std::intrinsics::atomic_store::<_, { AO::Release }, false>(
                &raw mut (*self.raw()).rsz_lock,
                false as u8,
            )
        };

        // release insertion lock
        unsafe {
            // std::intrinsics::atomic_store::<_, { AO::Release }, false>(
            //     &raw mut (*self.raw()).ins_lock,
            //     false as u8,
            // );

            let old_tab_layout = alloc::Layout::from_size_align_unchecked(old_tab_size, USA);
            let old_zone_layout = alloc::Layout::from_size_align_unchecked(old_zone_size, USA);

            alloc::dealloc(old_ta_ptr as *mut u8, old_tab_layout);
            alloc::dealloc(old_tb_ptr as *mut u8, old_tab_layout);
            alloc::dealloc(old_ez_ptr, old_zone_layout);
        };
    }

    pub fn get_id(self: Hdl<Self>, name: &[u8]) -> u32 {
        // look up name, then insert with next available ID if not found

        if let Some(id) = self.lookup_by_name(name) {
            return id;
        }

        // TODO: give up after some iteration count (deadlock)

        // acquire lock
        unsafe {
            while !std::intrinsics::atomic_cxchg::<_, { AO::Acquire }, { AO::Acquire }>(
                &raw mut (*self.raw()).ins_lock,
                false as u8,
                true as u8,
            )
            .1
            {}
        }

        let out_id = if let Some(cur_id) = self.lookup_by_name(name) {
            cur_id
        } else {
            unsafe {
                // resize if load excessive
                if (*self.raw()).load > ((*self.raw()).map_len * 2) / 3
                    || (*self.raw()).btop as usize + name.len()
                        > (*self.raw()).entries as usize + (*self.raw()).espace
                {
                    self.resize(2);
                }

                // direct_insert using next_id
                let ins_id = (*self.raw()).next_id;
                self.direct_insert(ins_id, name);

                // increment next_id
                (*self.raw()).next_id += 1;

                ins_id
            }
        };

        // release lock
        unsafe {
            assert!((*self.raw()).next_id <= !(0b11_u32.reverse_bits()));

            std::intrinsics::atomic_store::<_, { AO::Release }, false>(
                &raw mut (*self.raw()).ins_lock,
                false as u8,
            );
        };

        out_id
    }

    // TODO: check that tables are the same size after lookup as before

    pub fn lookup_by_name(self: Hdl<Self>, name: &[u8]) -> Option<u32> {
        // hash input, then search appropriate table until either
        // entry or zero found

        // NOTE: watch for issues caused by resize during lookup
        // TODO: actually take the resize lock here?
        while unsafe {
            std::intrinsics::atomic_load::<_, { AO::Acquire }, false>(
                &raw const (*self.raw()).rsz_lock,
            )
        } != 0
        {}

        unsafe {
            let tgt = Self::hash_name(name) % (*self.raw()).map_len;

            self.find_slot(false, tgt, <*mut u8>::is_null, |p: *mut u8| {
                Self::e_name_slice(p) == name
            })
            .map(|slot| ptr::read_unaligned(slot as *const u32))
        }
    }

    // TODO: version of Vec with no allocations if under a certain length? (like 12)
    pub fn lookup_by_id(self: Hdl<Self>, id: u32) -> Option<&'static [u8]> {
        // hash input, then search appropriate table until either
        // entry or zero found

        // NOTE: would like to keep as an immutable reference though
        while unsafe {
            std::intrinsics::atomic_load::<_, { AO::Acquire }, false>(
                &raw const (*self.raw()).rsz_lock,
            )
        } != 0
        {}

        unsafe {
            let tgt = Self::hash_id(id) % (*self.raw()).map_len;

            self.find_slot(true, tgt, <*mut u8>::is_null, |p| {
                ptr::read_unaligned(p as *mut u32) == id
            })
            .map(|slot| Self::e_name_slice(slot))
        }
    }

    /// Assign a name to an identifier in the table; **the insert lock must be held**
    fn direct_insert(self: Hdl<Self>, id: u32, name: &[u8]) {
        // reserve space in the entry zone for this entry
        let nlen = name.len();
        let elen = nlen + U32S + U16S;

        assert!(nlen <= u16::MAX.into());

        let eptr;
        unsafe {
            eptr = (*self.raw()).btop;

            // no buffer overflow
            assert!(eptr as usize + elen < (*self.raw()).entries as usize + (*self.raw()).espace);

            (*self.raw()).btop = eptr.add(elen);
        }

        // fill entry with id and name
        unsafe {
            ptr::write_unaligned(eptr as *mut u32, id);
            ptr::write_unaligned(eptr.add(U32S) as *mut u16, nlen as u16);
            slice::from_raw_parts_mut(eptr.add(U32S + U16S) as _, nlen).copy_from_slice(name);
        }

        unsafe {
            // hash id; place pointer in id . name map
            let id_nm_tgt = Self::hash_id(id) % (*self.raw()).map_len;
            let id_nm_wp = self.probe(true, id_nm_tgt);
            std::intrinsics::atomic_store::<_, { AO::Release }, false>(id_nm_wp, eptr);

            // hash name; place pointer in name . id map
            let nm_id_tgt = Self::hash_name(name) % (*self.raw()).map_len;
            let nm_id_wp = self.probe(false, nm_id_tgt);
            std::intrinsics::atomic_store::<_, { AO::Release }, false>(nm_id_wp, eptr);

            (*self.raw()).load += 1;
        }
    }

    // hiln - high: id map; low: name map
    fn find_slot(
        self: Hdl<Self>,
        hiln: bool,
        tgt: usize,
        failp: impl Fn(*mut u8) -> bool,
        foundp: impl Fn(*mut u8) -> bool,
    ) -> Option<*mut u8> {
        let (map, len) = unsafe {
            (
                if hiln {
                    (*self.raw()).id_to_nm
                } else {
                    (*self.raw()).nm_to_id
                },
                (*self.raw()).map_len,
            )
        };

        assert!(tgt < len);

        let mut csn = tgt;

        loop {
            // let cur = unsafe { ptr::read(map.add(csn)) };
            let cur =
                unsafe { std::intrinsics::atomic_load::<_, { AO::Acquire }, false>(map.add(csn)) };

            if failp(cur) {
                return None;
            } else if foundp(cur) {
                return Some(cur);
            }

            csn += 1;
            csn %= len;

            if csn == tgt {
                panic!("rollover");
            }
        }
    }

    fn probe(self: Hdl<Self>, hiln: bool, tgt: usize) -> *mut *mut u8 {
        let (map, len) = unsafe {
            (
                if hiln {
                    (*self.raw()).id_to_nm
                } else {
                    (*self.raw()).nm_to_id
                },
                (*self.raw()).map_len,
            )
        };

        // assuming a pre-zeroed map, check slots until an empty one
        // is found (wrapping)
        Self::probe_gen(map, len, tgt)
    }

    fn probe_gen(map: *mut *mut u8, len: usize, mut tgt: usize) -> *mut *mut u8 {
        assert!(tgt < len);

        while unsafe { ptr::read(map.add(tgt)) } != ptr::null_mut() {
            tgt += 1;
            tgt %= len;
        }

        unsafe { map.add(tgt) }
    }

    fn e_name_slice(entry: *const u8) -> &'static [u8] {
        // NOTE: we're using direct offsets for a reason
        unsafe {
            slice::from_raw_parts(
                entry.add(U32S + U16S) as _,
                ptr::read_unaligned(entry.add(U32S) as *mut u16) as usize,
            )
        }
    }

    fn hash_id(id: u32) -> usize {
        id as usize
    }

    fn hash_name(name: &[u8]) -> usize {
        let mut acc: usize = 1;
        for b in name {
            acc = acc.wrapping_add(acc << 5).wrapping_add(*b as _)
        }
        acc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_test() {
        let mut tab = SymbolTable::new(100);
        let tref = Hdl::new(&raw mut tab);

        let id = tref.get_id(b"testsym");

        assert!(tref.lookup_by_id(42).is_none());
        assert!(tref.lookup_by_name(b"nothere").is_none());

        assert_eq!(tref.lookup_by_id(id).unwrap(), b"testsym");
        assert_eq!(tref.lookup_by_name(b"testsym").unwrap(), id);
    }

    #[test]
    fn manysym() {
        let mut tab = SymbolTable::new(8000);
        let tref = Hdl::new(&raw mut tab);

        // generates symbols a00 - z99
        let mut acc = vec![];
        for i in 0..2600 {
            acc.push((i / 100 + 97) as u8);
            acc.push(((i % 100) / 10 + 48) as u8);
            acc.push(((i % 10) + 48) as u8);

            let id = tref.get_id(&acc);

            acc.clear();

            assert_eq!(id, i);
        }

        // find specific ids (of random syms?)
        assert_eq!(b"m13", tref.lookup_by_id(1213).unwrap());
        assert_eq!(692, tref.lookup_by_name(b"g92").unwrap());

        // add more symbols

        // look for prior ids
    }

    #[test]
    fn multisym() {
        let mut tab = SymbolTable::new(100000);

        fn r#gen(tgt: usize) {
            let tref = Hdl::new(tgt as *mut SymbolTable);

            // symbols aA00 - zZ99
            let mut acc = vec![];
            for i in 0..67600 {
                acc.push((i / 2600 + 97) as u8);
                acc.push(((i % 2600) / 100 + 65) as u8);
                acc.push(((i % 100) / 10 + 48) as u8);
                acc.push(((i % 10) + 48) as u8);

                let id = tref.get_id(&acc);

                // println!("{}", String::from_utf8(acc.clone()).unwrap());

                acc.clear();

                assert_eq!(id, i);
            }
        }

        let hdl = (&raw mut tab) as usize;

        let mut th = vec![];
        for _ in 0..5 {
            th.push(std::thread::spawn(move || r#gen(hdl)))
        }

        for j in th {
            j.join().unwrap();
        }

        assert_eq!(tab.load, 67600);
    }

    #[test]
    fn resize() {
        let mut tab = SymbolTable::new(50);
        let tref = Hdl::new(&raw mut tab);

        assert_eq!(tab.map_len, 50);

        let id1 = tref.get_id(b"sea");
        let id2 = tref.get_id(b"mesa");
        let id3 = tref.get_id(b"droll");

        tref.resize(4);

        assert_eq!(tab.map_len, 200);

        let id4 = tref.get_id(b"crutch");
        let id5 = tref.get_id(b"quarter");

        assert_eq!(id1, tref.get_id(b"sea"));
        assert_eq!(id2, tref.get_id(b"mesa"));
        assert_eq!(id3, tref.get_id(b"droll"));
        assert_eq!(id4, tref.get_id(b"crutch"));
        assert_eq!(id5, tref.get_id(b"quarter"));

        assert_eq!(b"sea", tref.lookup_by_id(id1).unwrap());
        assert_eq!(b"mesa", tref.lookup_by_id(id2).unwrap());
        assert_eq!(b"droll", tref.lookup_by_id(id3).unwrap());
        assert_eq!(b"crutch", tref.lookup_by_id(id4).unwrap());
        assert_eq!(b"quarter", tref.lookup_by_id(id5).unwrap());
    }
}

// STARK, a system for computer augmented design.
// Copyright (C) 2024 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License, version 3. See the top level LICENSE file for the license
// text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/sail/symtab.rs

// A table to associate symbol names with efficient internal IDs.

// <>

use std::{alloc, mem, ptr, slice};

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
    // bump allocator head
    btop: *mut u8,

    next_id: u32,
    ins_lock: u8,
    rsz_lock: u8,
}

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

impl SymbolTable {
    const U32S: usize = mem::size_of::<u32>();
    const U16S: usize = mem::size_of::<u16>();

    fn new(approx_cap: usize) -> Self {
        let byte_size = {
            let min = approx_cap * 11;
            min + (min % 8)
        };

        let tab_mem_layout = alloc::Layout::from_size_align(byte_size, 8).unwrap();

        let (taptr, tbptr, ezptr) = unsafe {
            (
                alloc::alloc_zeroed(tab_mem_layout),
                alloc::alloc_zeroed(tab_mem_layout),
                alloc::alloc(tab_mem_layout),
            )
        };

        Self {
            id_to_nm: taptr as _,
            nm_to_id: tbptr as _,

            map_len: byte_size / 8,
            load: 0,

            entries: ezptr,
            btop: ezptr,

            next_id: 0,
            ins_lock: false as u8,
            rsz_lock: false as u8,
        }
    }

    fn resize(&mut self, factor: usize) {
        // acquire insertion lock
        unsafe {
            while !std::intrinsics::atomic_cxchg_acquire_acquire(
                &mut self.ins_lock,
                false as u8,
                true as u8,
            )
            .1
            {}
        }

        // create tables of new size
        let new_len = self.map_len * factor;
        let new_size = new_len * 8;
        let new_tab_layout = alloc::Layout::from_size_align(new_size, 8).unwrap();
        let (ntapt, ntbpt, nezpt) = unsafe {
            (
                alloc::alloc_zeroed(new_tab_layout),
                alloc::alloc_zeroed(new_tab_layout),
                alloc::alloc(new_tab_layout),
            )
        };

        // write current entries into entry zone
        let ebct = self.btop as usize - self.entries as usize;
        unsafe { ptr::copy_nonoverlapping(&self.entries, nezpt as _, ebct) }

        // re-hash ids and names; place pointers in tables
        let mut zcur = self.entries;
        while zcur < self.btop {
            // step through entries from start to end
            let cur_id = unsafe { ptr::read(zcur as *const u32) };
            let cur_name = Self::e_name_slice(unsafe { &*zcur });

            // hash id; place pointer in id . name map
            let id_nm_tgt = Self::hash_id(cur_id) % new_len;
            let id_nm_wp = Self::probe_gen(ntapt as _, new_len, id_nm_tgt);
            unsafe { ptr::write(id_nm_wp, zcur) };

            // hash name; place pointer in name . id map
            let nm_id_tgt = Self::hash_name(cur_name) % new_len;
            let nm_id_wp = Self::probe_gen(ntbpt as _, new_len, nm_id_tgt);
            unsafe { ptr::write(nm_id_wp, zcur) };

            let elen = Self::U32S + Self::U16S + cur_name.len();

            zcur = unsafe { zcur.add(elen) }
        }

        assert_eq!(zcur, self.btop);

        // acquire resize lock
        unsafe {
            while !std::intrinsics::atomic_cxchg_acquire_acquire(
                &mut self.rsz_lock,
                false as u8,
                true as u8,
            )
            .1
            {}
        }

        // replace table pointers, entry zone pointers, and map len
        self.id_to_nm = ntapt as _;
        self.nm_to_id = ntbpt as _;

        self.entries = nezpt;
        self.btop = zcur;

        self.map_len = new_len;

        // release resize lock
        unsafe { std::intrinsics::atomic_store_release(&mut self.rsz_lock, false as u8) };

        // release insertion lock
        unsafe { std::intrinsics::atomic_store_release(&mut self.ins_lock, false as u8) };
    }

    fn get_id(&mut self, name: &[u8]) -> u32 {
        // look up name, then insert with next available ID if not found

        if let Some(id) = self.lookup_by_name(name) {
            return id;
        }

        // acquire lock
        unsafe {
            while !std::intrinsics::atomic_cxchg_acquire_acquire(
                &mut self.ins_lock,
                false as u8,
                true as u8,
            )
            .1
            {}
        }

        let out_id = if let Some(cur_id) = self.lookup_by_name(name) {
            cur_id
        } else {
            // direct_insert using next_id
            let ins_id = self.next_id;
            self.direct_insert(ins_id, name);

            // increment next_id
            self.next_id += 1;

            ins_id
        };

        // release lock
        unsafe { std::intrinsics::atomic_store_release(&mut self.ins_lock, false as u8) };

        assert!(self.next_id < (1u32 << 31));

        out_id
    }

    fn lookup_by_name(&self, name: &[u8]) -> Option<u32> {
        // hash input, then search appropriate table until either
        // entry or zero found

        // TODO: should I actually lock these? test I suppose
        while unsafe { std::intrinsics::atomic_load_acquire(&self.rsz_lock) } != 0 {}

        let tgt = Self::hash_name(name) % self.map_len;

        self.find_slot(false, tgt, <*mut u8>::is_null, |p: *mut u8| {
            Self::e_name_slice(unsafe { &*p }) == name
        })
        .map(|slot| unsafe { ptr::read(slot as *const u32) })
    }

    fn lookup_by_id(&self, id: u32) -> Option<&[u8]> {
        // hash input, then search appropriate table until either
        // entry or zero found

        // NOTE: would like to keep as an immutable reference though
        while unsafe { std::intrinsics::atomic_load_acquire(&self.rsz_lock) } != 0 {}

        let tgt = Self::hash_id(id) % self.map_len;

        self.find_slot(true, tgt, <*mut u8>::is_null, |p| unsafe {
            ptr::read(p as *mut u32) == id
        })
        .map(|slot| Self::e_name_slice(unsafe { &*slot }))
    }

    fn direct_insert(&mut self, id: u32, name: &[u8]) {
        // reserve space in the entry zone for this entry
        let nlen = name.len();
        let elen = nlen + Self::U32S + Self::U16S;

        assert!(nlen <= u16::MAX.into());

        let eptr = self.btop;
        self.btop = unsafe { self.btop.add(elen) };

        // fill entry with id and name
        unsafe {
            ptr::write(eptr as *mut u32, id);
            ptr::write(eptr.add(Self::U32S) as *mut u16, nlen as u16);
            slice::from_raw_parts_mut(eptr.add(Self::U32S + Self::U16S) as _, nlen)
                .copy_from_slice(name);
        }

        // hash id; place pointer in id . name map
        let id_nm_tgt = Self::hash_id(id) % self.map_len;
        let id_nm_wp = self.probe(true, id_nm_tgt);
        unsafe { ptr::write(id_nm_wp, eptr) };

        // hash name; place pointer in name . id map
        let nm_id_tgt = Self::hash_name(name) % self.map_len;
        let nm_id_wp = self.probe(false, nm_id_tgt);
        unsafe { ptr::write(nm_id_wp, eptr) };

        // TODO: some mechanism to resize table upon excess load (1/3?)
        self.load += 1;
    }

    // hiln - high: id map; low: name map
    fn find_slot(
        &self,
        hiln: bool,
        tgt: usize,
        failp: impl Fn(*mut u8) -> bool,
        foundp: impl Fn(*mut u8) -> bool,
    ) -> Option<*mut u8> {
        let map = if hiln { self.id_to_nm } else { self.nm_to_id };
        let len = self.map_len;

        assert!(tgt < len);

        let mut csn = tgt;

        loop {
            let cur = unsafe { ptr::read(map.add(csn)) };

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

    fn probe(&self, hiln: bool, tgt: usize) -> *mut *mut u8 {
        // assuming a pre-zeroed map, check slots until an empty one
        // is found (wrapping)
        Self::probe_gen(
            if hiln { self.id_to_nm } else { self.nm_to_id },
            self.map_len,
            tgt,
        )
    }

    fn probe_gen(map: *mut *mut u8, len: usize, mut tgt: usize) -> *mut *mut u8 {
        assert!(tgt < len);

        while unsafe { ptr::read(map.add(tgt)) } != ptr::null_mut() {
            tgt += 1;

            if tgt >= len {
                tgt = 0;
            }
        }

        unsafe { map.add(tgt) }
    }

    fn e_name_slice(entry: &u8) -> &[u8] {
        let eptr = entry as *const u8;
        unsafe {
            slice::from_raw_parts(
                eptr.add(Self::U32S + Self::U16S) as _,
                ptr::read(eptr.add(Self::U32S) as *mut u16) as usize,
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

// TODO: add a Drop implementation to clean everything up

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_test() {
        let mut tab = SymbolTable::new(100);

        let id = tab.get_id(b"testsym");

        assert!(tab.lookup_by_id(42).is_none());
        assert!(tab.lookup_by_name(b"nothere").is_none());

        assert_eq!(tab.lookup_by_id(id).unwrap(), b"testsym");
        assert_eq!(tab.lookup_by_name(b"testsym").unwrap(), id);
    }

    #[test]
    fn manysym() {
        let mut tab = SymbolTable::new(8000);

        // generates symbols a00 - z99
        let mut acc = vec![];
        for i in 0..2600 {
            acc.push((i / 100 + 97) as u8);
            acc.push(((i % 100) / 10 + 48) as u8);
            acc.push(((i % 10) + 48) as u8);

            let id = tab.get_id(&acc);

            acc.clear();

            assert_eq!(id, i);
        }

        // find specific ids (of random syms?)
        assert_eq!(b"m13", tab.lookup_by_id(1213).unwrap());
        assert_eq!(692, tab.lookup_by_name(b"g92").unwrap());

        // add more symbols

        // look for prior ids
    }

    #[test]
    fn multisym() {
        let mut tab = SymbolTable::new(100000);

        fn gen(tgt: usize) {
            let tab = unsafe { mem::transmute::<usize, &mut SymbolTable>(tgt) };

            // symbols aA00 - zZ99
            let mut acc = vec![];
            for i in 0..67600 {
                acc.push((i / 2600 + 97) as u8);
                acc.push(((i % 2600) / 100 + 65) as u8);
                acc.push(((i % 100) / 10 + 48) as u8);
                acc.push(((i % 10) + 48) as u8);

                let id = tab.get_id(&acc);

                // println!("{}", String::from_utf8(acc.clone()).unwrap());

                acc.clear();

                assert_eq!(id, i);
            }
        }

        let hdl = unsafe { mem::transmute::<&mut SymbolTable, usize>(&mut tab) };

        let mut th = vec![];
        for _ in 0..5 {
            th.push(std::thread::spawn(move || gen(hdl)))
        }

        for j in th {
            j.join().unwrap();
        }

        assert_eq!(tab.load, 67600);
    }

    #[test]
    fn resize() {
        let mut tab = SymbolTable::new(50);

        assert_eq!(tab.map_len, 69);

        let id1 = tab.get_id(b"sea");
        let id2 = tab.get_id(b"mesa");
        let id3 = tab.get_id(b"droll");

        tab.resize(4);

        assert_eq!(tab.map_len, 276);

        assert_eq!(id1, tab.get_id(b"sea"));
        assert_eq!(id2, tab.get_id(b"mesa"));
        assert_eq!(id3, tab.get_id(b"droll"));

        assert_eq!(b"sea", tab.lookup_by_id(id1).unwrap());
        assert_eq!(b"mesa", tab.lookup_by_id(id2).unwrap());
        assert_eq!(b"droll", tab.lookup_by_id(id3).unwrap());
    }
}

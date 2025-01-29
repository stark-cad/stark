// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is free software: you can redistribute it and / or modify it
// under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, version 3 of the License
// only.

// STARK is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public
// License along with STARK (in the top-level LICENSES directory). If
// not, see <https://www.gnu.org/licenses/>.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/mod.rs

// Sail is a Lisp dialect built specifically for STARK. There is a
// parser, interpreter, REPL, and small "standard library". Much of
// STARK's function will be described in Sail.

// <>

//! The Structured Augmentation Interchange Language
//!
//! A custom Lisp dialect for writing STARK

use std::fmt;
use std::mem;
use std::ptr;

#[macro_use]
pub mod core;
pub use self::core::*;

pub mod eval;
pub mod memmgt;
pub mod parser;
pub mod queue;
pub mod stdenv;
pub mod symtab;
pub mod thread;

pub use symtab::SymbolTable as Stab;

// monotonic counter for type layout identifiers
pub struct Styc {
    inner: u32,
}

impl Styc {
    pub fn new() -> Self {
        Self { inner: 0 }
    }

    fn get_id(&mut self) -> u32 {
        unsafe { std::intrinsics::atomic_xadd_acqrel(&mut self.inner, 1) }
    }

    unsafe fn set_next_id(&mut self, id: u32) {
        std::intrinsics::atomic_store_release(&mut self.inner, id)
    }
}

// the below, to make a type manifest for a sized value, takes the
// region, the type ID counter, and information on all the fields;
// field info includes length, type symbol, and keyword name

// TODO: check whether type symbols are valid?
fn make_val_type_mfst(
    reg: *mut memmgt::Region,
    ctr: &mut Styc,
    fields: Vec<(u32, u32, u32)>,
) -> SlHndl {
    // TODO: define and impose proper object size maximums
    assert!(12 + fields.len() * 16 <= u32::MAX as usize);

    let fieldct = fields.len() as u32;
    let mfst = unsafe {
        SlHndl::from_raw_unchecked(memmgt::alloc(
            reg,
            ty_manifest_size(fieldct),
            memmgt::cap(Cfg::TyMfst),
        ))
    };

    write_field(mfst.clone(), 0, ctr.get_id());
    write_field(mfst.clone(), 4, fieldct);

    let mut val_ofst_acc: u32 = 0;

    for (i, f) in fields.iter().enumerate() {
        let ent_ofst = 16 * i as u32 + 12;
        write_field(mfst.clone(), ent_ofst + 0, val_ofst_acc);
        write_field(mfst.clone(), ent_ofst + 4, f.0);
        write_field(mfst.clone(), ent_ofst + 8, f.1);
        write_field(mfst.clone(), ent_ofst + 12, f.2);
        val_ofst_acc += f.0;
    }

    write_field(mfst.clone(), 8, val_ofst_acc);
    mfst
}

// Existing Sized Types

// U8
// I8
// U16
// I16
// U32
// I32
// U64
// I64
// U128
// I128
// F32
// F64
// Bool
// Symbol
// Ref
// TyDsc
// ErrCode
// ProcNative
// WarpHdl

// <frame handle object>
// <graphics engine object>

// These should have global type IDs, type symbol bindings in the
// environment tree, and manifests.

// TODO: probably track how many have handles to a queue
pub fn warp_hdl_init(reg: *mut memmgt::Region, tgt: *mut queue::Inlet) -> SlHndl {
    assert!(!tgt.is_null());
    unsafe {
        let out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, 8, memmgt::cap(Cfg::B8WarpHdl)));
        write_field_unchecked(out.clone(), 0, tgt as u64);
        out
    }
}

fn warp_hdl_send(mut loc: SlHndl, msg: SlHndl, from: usize) {
    coretypck!(loc ; WarpHdl);

    let addrs: u64 = read_field(loc, 0);

    unsafe {
        (addrs as *mut queue::Inlet)
            .as_mut()
            .expect("null warp handle")
    }
    .transmit(from, msg)
}

fn thread_ref_init(reg: *mut memmgt::Region, tgt: *mut thread::ThreadHull) -> SlHndl {
    assert!(!tgt.is_null());
    unsafe {
        let out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, 8, T_THR_REF_ID.0));
        write_field_unchecked(out.clone(), 0, tgt as u64);
        out
    }
}

fn thread_ref_join(reg: *mut memmgt::Region, mut loc: SlHndl) -> SlHndl {
    assert_eq!(loc.type_id(), T_THR_REF_ID.0);

    let addrs: u64 = read_field(loc, 0);

    let th_ref = unsafe {
        (addrs as *mut thread::ThreadHull)
            .as_mut()
            .expect("null thread reference")
    };

    // TODO: real thread management without spinloops
    while !th_ref.done_p() {}

    unsafe { SlHndl::from_raw_unchecked(structure_copy(reg, th_ref.result().unwrap())) }
}

/// Basic error codes for Sail faults
#[derive(Debug)]
#[repr(u16)]
pub enum SlErrCode {
    ErrorUnknown = 0,
    ParseUnexpectedEnd,
    ParseBadSpecial,
    ParseInvalidChar,
    ParseInvalidString,
    ParseInvalidNum,
    FileCouldNotRead,
}

impl TryFrom<u16> for SlErrCode {
    type Error = ();

    #[inline(always)]
    fn try_from(v: u16) -> Result<Self, Self::Error> {
        use SlErrCode::*;
        match v {
            x if x == ErrorUnknown as u16 => Ok(ErrorUnknown),
            x if x == ParseUnexpectedEnd as u16 => Ok(ParseUnexpectedEnd),
            x if x == ParseBadSpecial as u16 => Ok(ParseBadSpecial),
            x if x == ParseInvalidChar as u16 => Ok(ParseInvalidChar),
            x if x == ParseInvalidString as u16 => Ok(ParseInvalidString),
            x if x == ParseInvalidNum as u16 => Ok(ParseInvalidNum),
            x if x == FileCouldNotRead as u16 => Ok(FileCouldNotRead),
            _ => Err(()),
        }
    }
}

#[inline(always)]
fn errcode_make(reg: *mut memmgt::Region) -> SlHndl {
    unsafe {
        let out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, 2, memmgt::cap(Cfg::B2Err)));
        // write_field_unchecked::<u16>(ptr, 0, 0);
        out
    }
}

#[inline(always)]
fn errcode_init(reg: *mut memmgt::Region, err: SlErrCode) -> SlHndl {
    unsafe {
        let out = errcode_make(reg);
        write_field_unchecked::<u16>(out.clone(), 0, err as u16);
        out
    }
}

#[inline(always)]
fn errcode_set(mut loc: SlHndl, err: SlErrCode) {
    coretypck!(loc ; ErrCode);
    write_field(loc, 0, err as u16);
}

#[inline(always)]
fn errcode_get(mut loc: SlHndl) -> SlErrCode {
    coretypck!(loc ; ErrCode);
    SlErrCode::try_from(read_field::<u16>(loc, 0)).unwrap()
}

fn arrvec_make<T: SizedBase + Copy>(
    reg: *mut memmgt::Region,
    typ: u32,
    len: u32,
    fill: T,
) -> SlHndl {
    assert_eq!(temp_get_size(typ) as usize, mem::size_of::<T>());
    unsafe {
        let size = vec_size(8, temp_get_size(typ), len);
        let out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, size, memmgt::cap(Cfg::VecArr)));

        write_field_unchecked::<u32>(out.clone(), 0, typ);
        write_field_unchecked::<u32>(out.clone(), 4, len);

        for i in 0..len {
            write_field_unchecked(out.clone(), 8 + (temp_get_size(typ) * i), fill)
        }

        out
    }
}

pub fn arrvec_init<T: SizedBase + Copy>(
    reg: *mut memmgt::Region,
    typ: u32,
    len: u32,
    val: &[T],
) -> SlHndl {
    assert_eq!(len as usize, val.len());
    assert_eq!(temp_get_size(typ) as usize, mem::size_of::<T>());

    unsafe {
        let size = vec_size(8, temp_get_size(typ), len);
        let out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, size, memmgt::cap(Cfg::VecArr)));

        write_field_unchecked::<u32>(out.clone(), 0, typ);
        write_field_unchecked::<u32>(out.clone(), 4, len);

        for (i, p) in val.iter().enumerate() {
            write_field_unchecked(out.clone(), 8 + (temp_get_size(typ) * i as u32), *p)
        }

        out
    }
}

fn arrvec_get_typ(mut loc: SlHndl) -> u32 {
    coretypck!(loc ; VecArr);
    read_field(loc, 0)
}

fn arrvec_get_len(mut loc: SlHndl) -> u32 {
    coretypck!(loc ; VecArr);
    read_field(loc, 4)
}

pub fn arrvec_rplc<T: SizedBase + Copy>(loc: SlHndl, val: &[T]) {
    let (len, typ) = (arrvec_get_len(loc.clone()), arrvec_get_typ(loc.clone()));

    assert_eq!(len, val.len() as u32);
    assert_eq!(temp_get_size(typ), mem::size_of::<T>() as u32);

    for (i, p) in val.iter().enumerate() {
        unsafe { write_field_unchecked(loc.clone(), 8 + (temp_get_size(typ) * i as u32), *p) }
    }
}

/// Symbols that must be added to the symbol table while the runtime is being set up
///
/// TODO: remember types that are parents / children of others
/// TODO: remember to have a bitvec type
/// TODO: use a script to automatically generate a Rust "env" file
macro_rules! incl_symbols {
    ( $array:ident : $( $name:ident $strng:literal $mode:ident );+ ) => {
        $(
            pub const $name: (u32, &str) = (modeize_sym(${index()}, SymbolMode::$mode), $strng);
        )+
            const $array: [&str; ${count($name)}] = [$($strng),+];
    };
}

incl_symbols! {
    SYM_ARRAY:
    T_T           "t"       Type;
    T_NIL         "nil"     Type;
    T_BOOL        "bool"    Type;
    T_U8          "u8"      Type;
    T_I8          "i8"      Type;
    T_U16         "u16"     Type;
    T_I16         "i16"     Type;
    T_U32         "u32"     Type;
    T_I32         "i32"     Type;
    T_U64         "u64"     Type;
    T_I64         "i64"     Type;
    T_U128        "u128"    Type;
    T_I128        "i128"    Type;
    T_F32         "f32"     Type;
    T_F64         "f64"     Type;
    T_SYMBOL      "symbol"  Type;
    T_REF         "ref"     Type;
    T_VECTOR      "vector"  Type;
    T_STDVEC      "stdvec"  Type;
    T_STRING      "string"  Type;
    T_HASHVEC     "hashvec" Type;
    T_ANYVEC      "anyvec"  Type;
    T_MAP         "map"     Type;
    T_ALISMAP     "alismap" Type;
    T_HASHMAP     "hashmap" Type;
    T_PROC        "proc"    Type;
    T_PROC_LAMBDA "sail-fn" Type;
    T_PROC_NATIVE "rust-fn" Type;
    T_ERR         "err"     Type;
    T_FRM_HDL     "frm-hdl" Type;
    T_ENG_HDL     "eng-hdl" Type;
    T_ENV         "env"     Type;
    T_ENV_LYR     "env-lyr" Type;
    T_THR_REF     "thr-hdl" Type;
    SP_DEF        "def"     Basic;
    SP_DO         "do"      Basic;
    SP_EVAL       "eval"    Basic;
    SP_FN         "fn"      Basic;
    SP_IF         "if"      Basic;
    SP_QUOTE      "quote"   Basic;
    SP_SET        "set"     Basic;
    SP_WHILE      "while"   Basic;
    S_MAIN        "main"    Basic;
    S_FRAME       "frame"   Basic;
    S_RNDR        "rndr"    Basic;
    S_RDR_TGT     "rdr-tgt" Basic;
    S_ENGINE      "engine"  Basic;
    S_T_INTERN    "%true"   Basic;
    S_F_INTERN    "%false"  Basic;
    S_FR_DIMS     "fr-dims" Basic;
    S_CUR_POS     "cur-pos" Basic;
    K_CX_DESTR    "cx-dstr" Keyword;
    K_CX_RESIZ    "cx-resz" Keyword;
    K_CX_RECRD    "cx-rcrd" Keyword;
    K_CX_REDRW    "cx-rdrw" Keyword;
    K_CX_CURMV    "cx-crmv" Keyword;
    K_CX_SHELL    "cx-shel" Keyword;
    K_CX_KEY_U    "cx-kb-u" Keyword;
    K_CX_KEY_D    "cx-kb-d" Keyword;
    K_CX_KEY_F    "cx-kb-f" Keyword;
    K_CX_KEY_B    "cx-kb-b" Keyword;
    K_CX_KEY_L    "cx-kb-l" Keyword;
    K_CX_KEY_S    "cx-kb-s" Keyword;
    K_CX_KEY_E    "cx-kb-e" Keyword;
    K_CX_KEY_K    "cx-kb-k" Keyword;
    K_CX_KEY_M    "cx-kb-m" Keyword;
    K_FD_GNMAP    "gensyms" Keyword;
    K_FD_TYMAP    "types"   Keyword;
    K_FD_MDMAP    "modules" Keyword;
    K_FD_PTR      "target"  Keyword
}

macro_rules! incl_types {
    ( $ctvar:ident : $( $name:ident $sym:ident );+ ) => {
        $(
            pub const $name: (u32, u32) = (${index()}, $sym.0);
        )+
            const $ctvar: usize = ${count($name)};
    }
}

incl_types! {
    TID_COUNT:
    T_FRM_HDL_ID  T_FRM_HDL;
    T_ENG_HDL_ID  T_ENG_HDL;
    T_THR_REF_ID  T_THR_REF
}

pub fn structure_copy(tgt: *mut memmgt::Region, root: SlHndl) -> *mut SlHead {
    // collect handles to all objects referenced by msg, with
    // appropriate reference counts (1 for msg, 1+ for all others
    // according to the structure) (panic if too many layers)

    fn index_of(vec: &Vec<*mut SlHead>, qry: *mut SlHead) -> Option<usize> {
        vec.iter()
            .enumerate()
            .find_map(|(i, x)| if *x == qry { Some(i) } else { None })
    }

    let mut src_ptrs: Vec<*mut SlHead> = vec![unsafe { root.get_raw() }];
    let mut last_cycle_ptr_count = 0;
    let mut total_ptr_count = 1;

    let mut src_nxt_elts: Vec<Option<u32>> = vec![];
    let mut src_int_refs: Vec<Vec<(u32, u32)>> = vec![];

    let mut new_rcs: Vec<u8> = vec![1];

    let mut ptrs_to_push: Vec<*mut SlHead> = Vec::new();

    loop {
        for this_src_ptr in &src_ptrs[last_cycle_ptr_count..] {
            let nxt_elt =
                (unsafe { ptr::read_unaligned(*this_src_ptr as *mut usize) } >> 16) as *mut SlHead;

            src_nxt_elts.push(if nil_p(nxt_elt) {
                None
            } else {
                // condition: pointer found in existing list or in
                // current (to push) list

                // increment appropriate reference count slot
                // (index for src_ptrs, index + last_end for ptp)

                if let Some(cycle_idx) = index_of(&ptrs_to_push, nxt_elt) {
                    new_rcs[cycle_idx + last_cycle_ptr_count] += 1;
                    Some((cycle_idx + last_cycle_ptr_count) as u32)
                } else if let Some(existing_id) = index_of(&src_ptrs, nxt_elt) {
                    new_rcs[existing_id] += 1;
                    Some(existing_id as u32)
                } else {
                    ptrs_to_push.push(nxt_elt);
                    new_rcs.push(1);
                    total_ptr_count += 1;
                    Some(total_ptr_count - 1)
                }
            });

            // we must also track the offsets of every reference within
            // every object, and identify which object each reference
            // pointed to, so that we can reconstitute the structure

            let mut ofs_tgt = Vec::new();
            // TODO: permit arbitrary objects, beyond core objects
            for ofs in discern_ref_offsets_core(*this_src_ptr) {
                let this_int_ptr = unsafe {
                    ptr::read_unaligned(
                        raw_val_ptr(*this_src_ptr).add(ofs as usize) as *mut *mut SlHead
                    )
                };
                if !nil_p(this_int_ptr) {
                    let tgt_id = if let Some(cycle_idx) = index_of(&ptrs_to_push, this_int_ptr) {
                        new_rcs[cycle_idx + last_cycle_ptr_count] += 1;
                        (cycle_idx + last_cycle_ptr_count) as u32
                    } else if let Some(existing_id) = index_of(&src_ptrs, this_int_ptr) {
                        new_rcs[existing_id] += 1;
                        existing_id as u32
                    } else {
                        ptrs_to_push.push(this_int_ptr);
                        new_rcs.push(1);
                        total_ptr_count += 1;
                        total_ptr_count - 1
                    };

                    ofs_tgt.push((ofs, tgt_id));
                }
            }

            src_int_refs.push(ofs_tgt);
        }

        assert_eq!(src_ptrs.len(), src_nxt_elts.len());
        assert_eq!(src_ptrs.len(), src_int_refs.len());

        if total_ptr_count as usize == last_cycle_ptr_count {
            // TODO: check for excess passes
            assert!(ptrs_to_push.is_empty());
            break;
        }

        last_cycle_ptr_count = src_ptrs.len();

        src_ptrs.append(&mut ptrs_to_push);

        assert_eq!(src_ptrs.len(), total_ptr_count as usize);
        assert_eq!(src_ptrs.len(), new_rcs.len());
    }

    // copy all objects to the new region, in reverse reference
    // dependency order, tracking the new pointers and populating
    // them into the correct reference slots as we go

    // we can insert them in reverse order of the source pointer
    // vector, because it was constructed by probing stepwise from
    // the root object

    let mut new_ptrs: Vec<*mut SlHead> = vec![ptr::null_mut(); src_ptrs.len()];

    // for p in &src_ptrs {
    //     println!("{:?}", raw_core_type(*p).unwrap());
    // }

    // println!("{src_ptrs:?}");
    // println!("{new_rcs:?}");
    // println!("{src_nxt_elts:?}");
    // println!("{src_int_refs:?}");

    for (i, p) in src_ptrs.into_iter().enumerate().rev() {
        let this_new_obj = memmgt::copy_direct(p, tgt, new_rcs[i]);
        new_ptrs[i] = this_new_obj;

        // insert correct new addresses in next element slot and
        // at reference offset points (the first item processed
        // here is guaranteed not to have any references out)

        unsafe {
            if let Some(nxt_id) = src_nxt_elts[i] {
                // set next list element raw
                ptr::write_unaligned(
                    this_new_obj as *mut u64,
                    ((new_ptrs[nxt_id as usize] as u64) << 16)
                        + ptr::read_unaligned(this_new_obj as *mut u16) as u64,
                );
            }

            for (ofs, id) in &src_int_refs[i] {
                // write internal pointer raw
                ptr::write_unaligned(
                    raw_val_ptr(this_new_obj).add(*ofs as usize) as *mut _,
                    new_ptrs[*id as usize],
                );
            }
        }
    }

    new_ptrs[0]
}

/// Bundles together an object and associated symbol table for display
pub struct SlContextVal<'a> {
    tbl: &'a Stab,
    obj: SlHndl,
}

/// Create a SlContextVal for display
pub fn context(tbl: &Stab, obj: SlHndl) -> SlContextVal {
    SlContextVal { tbl, obj }
}

// TODO: this is a mess; write Sail native display functions
// TODO: just push characters into a byte vector (string) for display
impl fmt::Display for SlContextVal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut value = self.obj.clone();

        use CoreType::*;
        match value.core_type() {
            Some(t) => match t {
                Nil => write!(f, "()"),
                Bool => write!(f, "{}", if bool_get(value) { "#T" } else { "#F" }),
                I64 => write!(f, "{}", i64_get(value)),
                U32 => write!(f, "{}", u32_get(value)),
                F64 => write!(f, "{}", f64_get(value)),
                F32 => write!(f, "{}", f32_get(value)),
                ErrCode => write!(f, "<err: {:?}>", errcode_get(value)),
                Symbol => {
                    let full_id = sym_get_id(value);
                    match mode_of_sym(full_id) {
                        SymbolMode::Basic => {
                            write!(
                                f,
                                "{}",
                                match self.tbl.lookup_by_id(full_id) {
                                    Some(n) => unsafe { std::str::from_utf8_unchecked(n) },
                                    None => "<none>",
                                }
                            )
                        }
                        SymbolMode::Keyword => {
                            write!(
                                f,
                                ":{}",
                                match self.tbl.lookup_by_id(demodes_sym(full_id)) {
                                    Some(n) => unsafe { std::str::from_utf8_unchecked(n) },
                                    None => "<none>",
                                }
                            )
                        }
                        SymbolMode::Module => {
                            write!(
                                f,
                                "@{}",
                                match self.tbl.lookup_by_id(demodes_sym(full_id)) {
                                    Some(n) => unsafe { std::str::from_utf8_unchecked(n) },
                                    None => "<none>",
                                }
                            )
                        }
                        SymbolMode::Type => {
                            write!(
                                f,
                                "${}",
                                match self.tbl.lookup_by_id(demodes_sym(full_id)) {
                                    Some(n) => unsafe { std::str::from_utf8_unchecked(n) },
                                    None => "<none>",
                                }
                            )
                        }
                    }
                }
                Ref => {
                    write!(f, "(").unwrap();
                    let mut elt = ref_get(value);
                    while let Some(next) = elt {
                        write!(f, "{}", context(self.tbl, next.clone()).to_string()).unwrap();
                        elt = get_next_list_elt(next);
                        if elt.is_some() {
                            write!(f, " ").unwrap();
                        }
                    }
                    write!(f, ")")
                }
                VecStd => {
                    write!(f, "[").unwrap();
                    let len = stdvec_get_len(value.clone());
                    for idx in 0..len {
                        write!(
                            f,
                            "{}",
                            context(self.tbl, stdvec_idx(value.clone(), idx)).to_string()
                        )
                        .unwrap();
                        if idx < len - 1 {
                            write!(f, " ").unwrap();
                        }
                    }
                    write!(f, "]")
                }
                VecStr => write!(f, "\"{}\"", string_get(value)),
                VecHash => {
                    // TODO: function to access all map pairs somehow
                    write!(f, "{{").unwrap();
                    // visit every position and traverse each entry list as below
                    let size = hashvec_get_size(value.clone());
                    let mut fst = true;
                    for idx in 0..size {
                        let mut pos = read_ptr(value.clone(), 4 + 4 + (PTR_LEN * idx));
                        while let Some(next) = pos {
                            if !fst {
                                write!(f, " ").unwrap()
                                // TODO: keep commas / parse them in maps?
                                // write!(f, ", ").unwrap()
                            }
                            write!(
                                f,
                                "{} ",
                                context(self.tbl, ref_get(next.clone()).unwrap()).to_string()
                            )
                            .unwrap();
                            write!(
                                f,
                                "{}",
                                context(
                                    self.tbl,
                                    get_next_list_elt(ref_get(next.clone()).unwrap()).unwrap()
                                )
                                .to_string()
                            )
                            .unwrap();
                            pos = get_next_list_elt(next);
                            fst = false;
                        }
                    }
                    write!(f, "}}")
                }
                ProcLambda | ProcNative => write!(f, "<$proc>"),
                _ => write!(f, "<@core/$other>"),
            },
            None => write!(f, "<$other>"),
        }
    }
}

/// Accepts an input stream and runs a read - evaluate - print loop perpetually
pub fn repl(stream_in: std::io::Stdin) {
    // TODO: consider stack-like environment per function
    // TODO: design module / namespace mechanisms

    let tact = thread::Tact::create(251);
    let mut weft = thread::Weft::create(tact);

    global_ctx_setup(weft.ctx_mut());

    let thr = thread::ThreadHull::summon(&mut weft, 10000, 100000, None);

    thread_env_setup(thr);

    // TODO: some kind of thread handle?
    let thread_ref = unsafe { &mut *thr };

    weft.assign_special(thread_ref.id);
    weft.add_worker();

    let region = thread_ref.region();

    loop {
        let mut input = String::new();
        stream_in.read_line(&mut input).expect("Failure");

        let expr = match parser::parse(region, weft.ctx_mut().symtab(), &input, false) {
            Ok(out) => out,
            Err(err) => {
                println!("{:?}\n", err);
                continue;
            }
        };

        thread_ref.load_direct(expr);

        while thread_ref.advance() {}

        println!(
            "{}\n",
            context(weft.ctx_mut().symtab(), thread_ref.result().unwrap())
        );
    }
}

/// Runs a Sail file in its own context
pub fn run_file(filename: &str) -> Result<String, SlErrCode> {
    let file = match std::fs::read_to_string(filename) {
        Ok(s) => s,
        Err(_) => return Err(SlErrCode::FileCouldNotRead),
    };

    interpret(&file, true)
}

/// Interprets a Sail expression, returning the formatted result
pub fn interpret(code: &str, dolist: bool) -> Result<String, SlErrCode> {
    let mut ctx = thread::Tact::create(251);
    global_ctx_setup(&mut ctx);

    let mut weft = thread::Weft::create(ctx);
    let thr = thread::ThreadHull::summon(&mut weft, 10000, 100000, None);
    thread_env_setup(thr);

    let thread_ref = unsafe { &mut *thr };

    weft.assign_special(thread_ref.id);
    weft.add_worker();

    thread_ref.load_from_text(code, dolist)?;

    while thread_ref.advance() {}

    let result = thread_ref.result().unwrap();

    Ok(context(weft.ctx_mut().symtab(), result).to_string())
}

pub fn global_ctx_setup(tact: &mut thread::Tact) {
    for (n, s) in SYM_ARRAY.into_iter().enumerate() {
        assert_eq!(tact.symtab().get_id(s.as_bytes()) as usize, n);
    }

    unsafe { tact.typctr().set_next_id(TID_COUNT as u32) };
}

pub fn thread_env_setup(thr: *mut thread::ThreadHull) {
    let thr_ref = unsafe { thr.as_mut().unwrap() };

    let reg = thr_ref.region();
    let env = thr_ref.top_env();

    // TODO: interning is temporary, will crash in lists

    let true_intern = bool_init(reg, true);
    env_scope_ins_by_id(reg, env.clone(), S_T_INTERN.0, true_intern);

    let false_intern = bool_init(reg, false);
    env_scope_ins_by_id(reg, env.clone(), S_F_INTERN.0, false_intern);

    insert_native_procs(reg, thr_ref.context().symtab(), env, stdenv::ENVFNS);
}

/// Insert a slice of native procedures into the symbol table and environment
pub fn insert_native_procs(
    reg: *mut memmgt::Region,
    tbl: &mut Stab,
    env: SlHndl,
    fns: &[(&str, NativeFn, u16)],
) {
    for entry in fns {
        let proc_id = tbl.get_id(entry.0.as_bytes());

        let proc_fn = proc_native_init(reg, entry.2, entry.1);

        env_scope_ins_by_id(reg, env.clone(), proc_id, proc_fn);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn returns() {
        let exp = String::from("42");
        assert_eq!(exp, interpret(&exp, false).unwrap());
    }

    #[test]
    fn adds() {
        let exp = String::from("(+ 2 2)");
        assert_eq!("4", interpret(&exp, false).unwrap());
    }

    #[test]
    fn lambda() {
        let exp = String::from("((fn [a b] (- a (- 0 b))) 3 3)");
        assert_eq!("6", interpret(&exp, false).unwrap());
    }

    #[test]
    fn parses() {
        let (reg, mut tbl) = (memmgt::Region::acq(1000), Stab::new(51));

        let exp = String::from("(+ (() 42 (e) #T) #F 2.1 e)");
        let val = parser::parse(reg, &mut tbl, &exp, false).unwrap();
        let out = context(&tbl, val).to_string();
        assert_eq!(exp, out);

        let exp = String::from("(() (()) ((((() ())))))");
        let val = parser::parse(reg, &mut tbl, &exp, false).unwrap();
        let out = context(&tbl, val).to_string();
        assert_eq!(exp, out);

        let exp = String::from("((1 2 3 4) ;Comment\n5)");
        let gnd = String::from("((1 2 3 4) 5)");
        let val = parser::parse(reg, &mut tbl, &exp, false).unwrap();
        let out = context(&tbl, val).to_string();
        assert_eq!(gnd, out);
    }

    #[test]
    fn mult_while_direct() {
        let input = include_str!("../../examples/mult-while.sl");
        assert_eq!("233168", interpret(&input, true).unwrap());
    }
}

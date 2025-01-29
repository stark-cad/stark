// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/core.rs

// Sail types and many, many important utility functions for building
// list structures and evaluating Sail code. Contains the necessary
// definitions for constructing a useful Sail environment.

// <>

//! TODO: Gradual typing; more extensible type system; subtypes

use std::mem;
use std::ptr;

use super::thread::ThreadHull;

use super::memmgt::{self, Region};

/// Core type assertion
#[macro_export]
macro_rules! coretypck {
    ( $var:ident ; $typ:ident ) => {
        assert_eq!($var.core_type().unwrap(), crate::sail::core::CoreType::$typ);
    };
}

/// Core type predicate
#[macro_export]
macro_rules! coretypp {
    ( $var:ident ; $typ:ident ) => {
        match $var.core_type() {
            Some(t) => t == crate::sail::core::CoreType::$typ,
            None => false,
        }
    };
}

/// Trait for base types that are always the same size
pub trait SizedBase {}

/// Implements SizedBase trait for basic Rust types
macro_rules! sized_base {
    ( $( $typ:ty, )+ ) => {
        $(
            impl SizedBase for $typ {}
        )+
    };
}

sized_base! {
    u8, i8, u16, i16,
    u32, i32, u64, i64,
    u128, i128,
    f32, f64,
    // *mut SlHead,
}

/// Head includes pointer to next list element
#[cfg(not(feature = "memdbg"))]
pub const HEAD_LEN: u32 = 8;
#[cfg(feature = "memdbg")]
pub const HEAD_LEN: u32 = 12;

pub const PTR_LEN: u32 = 8;
pub const SYMBOL_LEN: u32 = 4;
pub const NUM_8_LEN: u32 = 1;
pub const NUM_16_LEN: u32 = 2;
pub const NUM_32_LEN: u32 = 4;
pub const NUM_64_LEN: u32 = 8;
pub const NUM_128_LEN: u32 = 16;

/// Header for all Sail objects in memory
///
/// **Handle ONLY using methods that operate on pointers to SlHead**
/// Should only store information that every referenced Sail object needs
#[repr(C)]
pub struct SlHead {
    pub cfg: u8,
    pub rc: u8,
}

// TODO: potential change: add a "shared" bit to indicate that an object
// may be read and / or written by other threads

// TODO: add an "interned" bit to indicate that the object must be
// copied before any list structure manipulation; also could mean
// "loner" or "read-only"

// TODO / NOTE: There are now two spare, unused bits in the Sail head:
// the lowest two bits

/// ALL Sail objects that may be independently referenced, begin with bytes of this format
///
/// size: 3 bits - base type: 3 bits - UNUSED: 2 bits - rc: 8 bits
/// The first eight bits determine the subsequent memory layout
const _MIN_HEAD: u16 = 0b0001110011111111;

/// Pointer to the next element of a linked list;
/// tagged with the SlHead (upper 2 unused bytes)
struct _SlListPtr {
    ptr: *mut SlHead,
}

#[derive(Debug, PartialEq)]
#[repr(C)]
pub struct SlHndl {
    raw: *mut SlHead,
}

impl Clone for SlHndl {
    fn clone(&self) -> Self {
        // log::debug!("Cloning object handle");
        inc_refc(self.raw);
        SlHndl { raw: self.raw }
    }
}

impl Drop for SlHndl {
    fn drop(&mut self) {
        // log::debug!("Dropping object handle");
        // __dbg_head_info(self.raw);
        if cfg!(feature = "memdbg") {
            println!("O {} DROP", self.memdbg_obj_id())
        }
        if dec_refc(self.raw) {
            destroy_obj_core(self.raw)
        }
    }
}

unsafe impl Send for SlHndl {}
unsafe impl Sync for SlHndl {}

// TODO: unsafe extractor alts for when loc is known up-to-date

impl SlHndl {
    unsafe fn correct_pos(&mut self) {
        let orig = self.raw;
        let mut loc = orig;

        while raw_cfg_byte(loc) == Cfg::B0Redir as u8 {
            // TODO: rewrite addresses in memory to avoid indirection!
            println!("-- redir hit --");
            loc = (ptr::read_unaligned(loc as *mut usize) >> 16) as *mut SlHead;
        }

        if loc != orig {
            inc_refc(loc);
            ptr::write_unaligned(&mut self.raw, loc);

            // TODO: perhaps should have a destroy_redir function
            // NOTE: in these cases we know ONLY the redir chain may
            // be destroyed

            if dec_refc(orig) {
                destroy_redir(orig);
            }
        }
    }

    pub unsafe fn get_raw(&self) -> *mut SlHead {
        self.raw
    }

    pub unsafe fn from_raw(loc: *mut SlHead) -> Option<Self> {
        if nil_p(loc) {
            None
        } else {
            // log::debug!("Creating object handle");
            inc_refc(loc);
            Some(SlHndl { raw: loc })
        }
    }

    pub unsafe fn from_raw_unchecked(loc: *mut SlHead) -> Self {
        debug_assert!(!nil_p(loc));
        // log::debug!("Creating object handle");
        SlHndl { raw: loc }
    }

    #[inline(always)]
    pub fn nnil_ref_p(&mut self) -> bool {
        unsafe { self.correct_pos() };

        match self.core_type() {
            Some(t) if t == CoreType::Ref => ref_get(self.clone()).is_some(),
            _ => false,
        }
    }

    #[inline(always)]
    pub fn basic_sym_p(&mut self) -> bool {
        unsafe { self.correct_pos() };

        match self.core_type() {
            Some(t) if t == CoreType::Symbol => {
                mode_of_sym(sym_get_id(self.clone())) == SymbolMode::Basic
            }
            _ => false,
        }
    }

    #[inline(always)]
    pub fn proc_p(&mut self) -> bool {
        unsafe { self.correct_pos() };

        match self.core_type() {
            Some(t) if t == CoreType::ProcLambda || t == CoreType::ProcNative => true,
            _ => false,
        }
    }

    #[inline(always)]
    pub fn type_fld_p(&mut self) -> bool {
        unsafe { self.correct_pos() };

        let head = self.cfg_byte();
        (head & 0b00011100) >> 2 == 7
    }

    #[inline(always)]
    pub fn size_fld_p(&mut self) -> bool {
        unsafe { self.correct_pos() };

        let head = self.cfg_byte();
        head >> 5 > 5
    }

    pub fn memdbg_obj_id(&mut self) -> u32 {
        unsafe { self.correct_pos() };

        if cfg!(feature = "memdbg") {
            unsafe { ptr::read_unaligned((self.raw as *mut u32).add(2)) }
        } else {
            panic!("Memory debug mode not active")
        }
    }

    #[inline(always)]
    pub fn type_id(&mut self) -> u32 {
        unsafe { self.correct_pos() };

        assert!(self.type_fld_p());
        unsafe { ptr::read_unaligned((self.raw as *mut u8).add(HEAD_LEN as usize) as *const u32) }
    }

    #[inline(always)]
    pub fn size(&mut self) -> u32 {
        unsafe { self.correct_pos() };

        let code = self.cfg_byte() >> 5;
        if code <= 5 {
            0x80000000_u32.rotate_left(code as u32) & 31
        } else {
            assert!(self.size_fld_p());
            unsafe {
                ptr::read_unaligned(
                    (self.raw as *mut u8)
                        .add((HEAD_LEN + (NUM_32_LEN * self.type_fld_p() as u32)) as usize)
                        as *const u32,
                )
            }
        }
    }

    #[inline(always)]
    fn __dbg_head_info(&mut self) {
        unsafe { self.correct_pos() };

        println!(
            "type field?: {}; size field?: {}; size: {}",
            self.type_fld_p(),
            self.size_fld_p(),
            self.size()
        )
    }

    #[inline(always)]
    pub fn truthy(&mut self) -> bool {
        unsafe { self.correct_pos() };

        !((coretypp!(self ; Bool) && !bool_get(self.clone()))
            || (coretypp!(self ; Ref) && ref_get(self.clone()).is_none()))
    }

    #[inline(always)]
    fn cfg_byte(&mut self) -> u8 {
        unsafe { self.correct_pos() };

        unsafe { ptr::read_unaligned(self.raw as *const u8) }
    }

    #[inline(always)]
    pub fn refc_byte(&mut self) -> u8 {
        unsafe { self.correct_pos() };

        unsafe { ptr::read_unaligned((self.raw as *const u8).add(1)) }
    }

    #[inline(always)]
    pub fn cfg_spec(&mut self) -> Cfg {
        unsafe { self.correct_pos() };

        let top_byte = self.cfg_byte();
        match Cfg::try_from(top_byte & 0b11111100) {
            Ok(out) => out,
            Err(_) => panic!("invalid cfg specifier: {:08b}", top_byte),
        }
    }

    #[inline(always)]
    pub fn base_size(&mut self) -> BaseSize {
        unsafe { self.correct_pos() };

        match BaseSize::try_from(self.cfg_byte() >> 5) {
            Ok(out) => out,
            Err(_) => unreachable!(),
        }
    }

    #[inline(always)]
    pub fn value_ptr(&mut self) -> *mut u8 {
        unsafe { self.correct_pos() };

        let offset = (HEAD_LEN
            + (self.type_fld_p() as u32 * NUM_32_LEN)
            + (self.size_fld_p() as u32 * NUM_32_LEN)) as usize;
        unsafe { (self.raw as *mut u8).add(offset) }
    }

    #[inline(always)]
    pub fn core_type(&mut self) -> Option<CoreType> {
        unsafe { self.correct_pos() };

        match CoreType::try_from(self.cfg_spec()) {
            Ok(out) => Some(out),
            Err(_) => None,
        }
    }
}

// do we require that SlHndl is always non-null? could use the same
// guaranteed non-null optimization for Option as certain standard
// library types do

/// Generates TryFrom implementations for important enums
macro_rules! enum_and_tryfrom {
    ($(#[$meta:meta])* $vis:vis enum $name:ident {
        $($(#[$vmeta:meta])* $vname:ident $(= $val:expr)?,)*
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $($(#[$vmeta])* $vname $(= $val)?,)*
        }

        impl std::convert::TryFrom<u8> for $name {
            type Error = ();

            #[inline(always)]
            fn try_from(v: u8) -> Result<Self, Self::Error> {
                match v {
                    $(x if x == $name::$vname as u8 => Ok($name::$vname),)*
                    _ => Err(()),
                }
            }
        }
    }
}

// a "Redir" is not a real object, but an immediate redirect to an
// object at another address

enum_and_tryfrom! {
    /// Contains all valid values for the high six bits of an SlHead
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    #[repr(u8)]
    pub enum Cfg {
        B0BoolF = 0b00000000,
        B0BoolT = 0b00000100,
        B0Redir = 0b00001000,
        B0Other = 0b00011100,
        B1U8 = 0b00100000,
        B1I8 = 0b00100100,
        B1Other = 0b00111100,
        B2U16 = 0b01000000,
        B2I16 = 0b01000100,
        B2Err = 0b01001000,
        B2Other = 0b01011100,
        B4U32 = 0b01100000,
        B4I32 = 0b01100100,
        B4F32 = 0b01101000,
        B4Sym = 0b01101100,
        B4Other = 0b01111100,
        B8U64 = 0b10000000,
        B8I64 = 0b10000100,
        B8F64 = 0b10001000,
        B8Ptr = 0b10001100,
        B8WarpHdl = 0b10010000,
        // B8FileHdl = 0b10010100,
        // B8SockHdl = 0b10011000,
        B8Other = 0b10011100,
        B16U128 = 0b10100000,
        B16I128 = 0b10100100,
        B16TyDsc = 0b10101000,
        B16Other = 0b10111100,
        VecStd = 0b11000000,
        VecStr = 0b11000100,
        VecArr = 0b11001000,
        VecAny = 0b11001100,
        VecHash = 0b11010000,
        VecOther = 0b11011100,
        ProcLambda = 0b11100000,
        ProcNative = 0b11100100,
        // ProcLbdaCk = 0b11101000,
        // ProcNatvCk = 0b11101100,
        TyMfst = 0b11110000,
        EnvScope = 0b11110100,
        EnvLayer = 0b11111000,
        Other = 0b11111100,
    }
}

enum_and_tryfrom! {
    /// All type sizes that may be specified in the head
    #[derive(Debug, PartialEq, Eq)]
    #[repr(u8)]
    pub enum BaseSize {
        // don't mess around with the enum variants
        B0 = 0,
        B1 = 1,
        B2 = 2,
        B4 = 3,
        B8 = 4,
        B16 = 5,
        Vec = 6,
        Other = 7,
    }
}

enum_and_tryfrom! {
    /// The four "modes" a symbol can have
    #[derive(Debug, PartialEq, Eq)]
    #[repr(u8)]
    pub enum SymbolMode {
        Basic = 0,
        Module = 1,
        Keyword = 2,
        Type = 3,
    }
}

/// Core types that must be known in order to assemble the runtime
///
/// They can be represented in the object head, without an additional type specifier.
/// Null pointers "point to" Nil objects; the concept is like interning.
/// The next 18 types have statically known size, and correspond to Rust types.
/// The last 7 types have variable size, and must be inspected to get a size.
#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum CoreType {
    Nil,
    Bool,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    U128,
    I128,
    F32,
    F64,
    Symbol,
    Ref,
    WarpHdl,
    TyDsc,
    TyMfst,
    ErrCode,
    VecStd,
    VecStr,
    VecArr,
    VecAny,
    VecHash,
    ProcLambda,
    ProcNative,
    EnvScope,
    EnvLayer,
}

impl TryFrom<Cfg> for CoreType {
    type Error = ();

    #[inline(always)]
    fn try_from(v: Cfg) -> Result<Self, Self::Error> {
        match v {
            Cfg::B0BoolF | Cfg::B0BoolT => Ok(Self::Bool),
            Cfg::B1U8 => Ok(Self::U8),
            Cfg::B1I8 => Ok(Self::I8),
            Cfg::B2U16 => Ok(Self::U16),
            Cfg::B2I16 => Ok(Self::I16),
            Cfg::B2Err => Ok(Self::ErrCode),
            Cfg::B4U32 => Ok(Self::U32),
            Cfg::B4I32 => Ok(Self::I32),
            Cfg::B4F32 => Ok(Self::F32),
            Cfg::B4Sym => Ok(Self::Symbol),
            Cfg::B8U64 => Ok(Self::U64),
            Cfg::B8I64 => Ok(Self::I64),
            Cfg::B8F64 => Ok(Self::F64),
            Cfg::B8Ptr => Ok(Self::Ref),
            Cfg::B8WarpHdl => Ok(Self::WarpHdl),
            Cfg::B16U128 => Ok(Self::U128),
            Cfg::B16I128 => Ok(Self::I128),
            Cfg::B16TyDsc => Ok(Self::TyDsc),
            Cfg::VecStd => Ok(Self::VecStd),
            Cfg::VecStr => Ok(Self::VecStr),
            Cfg::VecArr => Ok(Self::VecArr),
            Cfg::VecAny => Ok(Self::VecAny),
            Cfg::VecHash => Ok(Self::VecHash),
            Cfg::ProcLambda => Ok(Self::ProcLambda),
            Cfg::ProcNative => Ok(Self::ProcNative),
            Cfg::TyMfst => Ok(Self::TyMfst),
            Cfg::EnvScope => Ok(Self::EnvScope),
            Cfg::EnvLayer => Ok(Self::EnvLayer),
            _ => Err(()),
        }
    }
}

/// Signature for Sail functions implemented in Rust
///
/// Arguments:
/// - Structure for local thread
/// - Current environment
/// - Slice containing all Sail arguments
pub type NativeFn = fn(*mut ThreadHull, SlHndl, &[SlHndl]) -> SlHndl;

// TODO: should the env be provided as part of the argument slice?

/// Creates a nil Sail object
#[inline(always)]
pub fn nil() -> *mut SlHead {
    ptr::null_mut()
}

/// Checks whether a pointer, ostensibly to a Sail object, is null
#[inline(always)]
pub fn nil_p(loc: *mut SlHead) -> bool {
    loc.is_null()
}

// With current value representation, nothing is really an atom
// pub fn atom_p(loc: *mut SlHead) -> bool {
//     match core_type(loc) {
//         Some(t) if t != CoreType::Ref => true,
//         _ => false,
//     }
// }

/// Set a symbol to one of the four symbol modes
pub const fn modeize_sym(sym: u32, mode: SymbolMode) -> u32 {
    (sym & 0x3FFFFFFF) + ((mode as u32) << 30)
}

/// Returns a symbol set to the default, basic mode
pub const fn demodes_sym(sym: u32) -> u32 {
    sym & 0x3FFFFFFF
}

/// Get the mode of a symbol
pub const fn mode_of_sym(sym: u32) -> SymbolMode {
    unsafe { mem::transmute::<u8, SymbolMode>((sym >> 30) as u8) }
}

/// Checks whether a valid Sail object contains the 4-byte type ID
/// field
pub fn raw_typ_fld_p(loc: *mut SlHead) -> bool {
    let head = raw_cfg_byte(loc);
    (head & 0b00011100) >> 2 == 7
}

/// Checks whether a valid Sail object contains the 4-byte size field
pub fn raw_siz_fld_p(loc: *mut SlHead) -> bool {
    let head = raw_cfg_byte(loc);
    head >> 5 > 5
}

#[deprecated]
pub fn raw_type_id(loc: *mut SlHead) -> u32 {
    assert!(raw_typ_fld_p(loc));
    unsafe { ptr::read_unaligned((loc as *mut u8).add(HEAD_LEN as usize) as *const u32) }
}

/// Return the size in bytes of any Sail object's payload (the space
/// for a value following the head)
pub fn raw_size(loc: *mut SlHead) -> u32 {
    let code = raw_cfg_byte(loc) >> 5;
    if code <= 5 {
        0x80000000_u32.rotate_left(code as u32) & 31
    } else {
        assert!(raw_siz_fld_p(loc));
        unsafe {
            ptr::read_unaligned(
                (loc as *mut u8).add((HEAD_LEN + (NUM_32_LEN * raw_typ_fld_p(loc) as u32)) as usize)
                    as *const u32,
            )
        }
    }
}

#[deprecated]
fn __dbg_head_info(loc: *mut SlHead) {
    println!(
        "type field?: {}; size field?: {}; size: {}",
        raw_typ_fld_p(loc),
        raw_siz_fld_p(loc),
        raw_size(loc)
    )
}

/// Gets the full configuration byte from a Sail object
fn raw_cfg_byte(loc: *mut SlHead) -> u8 {
    unsafe { ptr::read_unaligned(loc as *const u8) }
}

/// Gets the reference count byte from a Sail object
fn raw_refc_byte(loc: *mut SlHead) -> u8 {
    unsafe { ptr::read_unaligned((loc as *const u8).add(1)) }
}

/// Gets the size / type configuration from a Sail object
pub fn raw_cfg_spec(loc: *mut SlHead) -> Cfg {
    let top_byte = raw_cfg_byte(loc);
    match Cfg::try_from(top_byte & 0b11111100) {
        Ok(out) => out,
        Err(_) => panic!("invalid cfg specifier: {:08b}", top_byte),
    }
}

/// From a valid Sail object, returns a pointer to the start of the value proper
///
/// (After the header and type specifiers, if they exist)
pub fn raw_val_ptr(loc: *mut SlHead) -> *mut u8 {
    let offset = (HEAD_LEN
        + (raw_typ_fld_p(loc) as u32 * NUM_32_LEN)
        + (raw_siz_fld_p(loc) as u32 * NUM_32_LEN)) as usize;
    unsafe { (loc as *mut u8).add(offset) }
}

/// Returns None if the object is not of a core type, or its type if it is
pub fn raw_core_type(loc: *mut SlHead) -> Option<CoreType> {
    if nil_p(loc) {
        Some(CoreType::Nil)
    } else {
        match CoreType::try_from(raw_cfg_spec(loc)) {
            Ok(out) => Some(out),
            Err(_) => None,
        }
    }
}

pub fn raw_memdbg_obj_id(loc: *mut SlHead) -> u32 {
    if cfg!(feature = "memdbg") {
        unsafe { ptr::read_unaligned((loc as *mut u32).add(2)) }
    } else {
        panic!("Memory debug mode not active")
    }
}

// a VecArr has head of type, length
// a VecAny has head of type, capacity, length

// TODO: rectify the lack of a real type system

/// Identifies whether a given type ID refers to a base sized type
pub fn temp_base_sized_p(typ: u32) -> bool {
    match typ {
        t if t == super::T_U8.0
            || t == super::T_I8.0
            || t == super::T_U16.0
            || t == super::T_I16.0
            || t == super::T_U32.0
            || t == super::T_I32.0
            || t == super::T_U64.0
            || t == super::T_I64.0
            || t == super::T_U128.0
            || t == super::T_I128.0
            || t == super::T_F32.0
            || t == super::T_F64.0
            || t == super::T_SYMBOL.0
            || t == super::T_REF.0
            || t == super::T_ERR.0 =>
        {
            true
        }
        _t => false,
    }
}

/// Gives the size of a limited range of types (base sized) by type
/// symbol
pub fn temp_get_size(typ: u32) -> u32 {
    match typ {
        t if t == super::T_U8.0 => 1,
        t if t == super::T_I8.0 => 1,
        t if t == super::T_U16.0 => 2,
        t if t == super::T_I16.0 => 2,
        t if t == super::T_U32.0 => 4,
        t if t == super::T_I32.0 => 4,
        t if t == super::T_U64.0 => 8,
        t if t == super::T_I64.0 => 8,
        t if t == super::T_U128.0 => 16,
        t if t == super::T_I128.0 => 16,
        t if t == super::T_F32.0 => 4,
        t if t == super::T_F64.0 => 8,
        t if t == super::T_SYMBOL.0 => 4,
        t if t == super::T_REF.0 => 8,
        t if t == super::T_ERR.0 => 2,
        _t => {
            panic!("type not allowed")
        }
    }
}

/// Initializes a Sail object from a base sized type and a pointer
pub fn temp_init_from(reg: *mut Region, typ: u32, ptr: *const u8) -> SlHndl {
    assert!(temp_base_sized_p(typ));
    unsafe {
        match typ {
            t if t == super::T_U8.0 => u8_init(reg, ptr::read_unaligned(ptr)),
            t if t == super::T_I8.0 => i8_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_U16.0 => u16_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_I16.0 => i16_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_U32.0 => u32_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_I32.0 => i32_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_U64.0 => u64_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_I64.0 => i64_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_U128.0 => u128_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_I128.0 => i128_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_F32.0 => f32_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_F64.0 => f64_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_SYMBOL.0 => sym_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_REF.0 => ref_init(reg, ptr::read_unaligned(ptr as *const _)),
            t if t == super::T_ERR.0 => {
                super::errcode_init(reg, ptr::read_unaligned(ptr as *const _))
            }
            _ => unreachable!(),
        }
    }
}

/// Gives the overall size of a Vec with certain parameters
#[inline(always)]
pub fn vec_size(head_size: u32, elt_size: u32, capacity: u32) -> u32 {
    head_size + (elt_size * capacity)
}

#[inline(always)]
pub fn ty_manifest_size(fldct: u32) -> u32 {
    (NUM_32_LEN + NUM_32_LEN + NUM_32_LEN)
        + ((NUM_32_LEN + NUM_32_LEN + SYMBOL_LEN + SYMBOL_LEN) * fldct)
}

/// Gives the overall size of a lambda procedure by argument count
#[inline(always)]
fn proc_lambda_size(argct: u16) -> u32 {
    (NUM_16_LEN + PTR_LEN) + (SYMBOL_LEN * argct as u32)
}

/// Gives the overall size of a native procedure
#[inline(always)]
fn proc_native_size() -> u32 {
    NUM_16_LEN + PTR_LEN
}

// TODO: could avoid multithreaded / reentrance-safe reference count
// handlers by requiring that all objects are known to only one
// evaluation unit; queue objects would be the main concern

// TODO: to handle objects from a parent thread, check the address of
// each object to find whether it sits in the appropriate region? all
// objects addressable from a child thread are guaranteed to outlive
// it because the child will take a reference

// NOTE: it seems that this would require a concept of which region /
// thread we are "working from" for each reference count operation

/// Increment the reference count stored in a Sail object, up to a
/// maximum of 255; if the count is at 255, return true
pub fn inc_refc(loc: *mut SlHead) -> bool {
    assert!(!nil_p(loc));

    let rc_pos = unsafe { (loc as *mut u8).add(1) };
    let mut cur = unsafe { std::intrinsics::atomic_load_acquire(rc_pos) };

    loop {
        match cur {
            0 => panic!("attempted reference to dead object"),
            255 => {
                if cfg!(feature = "memdbg") {
                    println!("O {} SAT", unsafe {
                        ptr::read_unaligned((loc as *const u32).add(2))
                    })
                }
                return true;
            }
            _ => (),
        }

        let sp;
        (cur, sp) = unsafe { std::intrinsics::atomic_cxchg_acqrel_acquire(rc_pos, cur, cur + 1) };

        if sp {
            break;
        }
    }

    if cfg!(feature = "memdbg") {
        println!(
            "O {} INC (c {})",
            unsafe { ptr::read_unaligned((loc as *const u32).add(2)) },
            cur + 1
        )
    }

    false
}

/// Decrement the reference count stored in a Sail object, down to a
/// minimum of 0; if the count reaches 0, return true
pub fn dec_refc(loc: *mut SlHead) -> bool {
    assert!(!nil_p(loc));
    let mut out;

    let rc_pos = unsafe { (loc as *mut u8).add(1) };
    let mut cur = unsafe { std::intrinsics::atomic_load_acquire(rc_pos) };

    loop {
        match cur {
            0 => panic!("too many reference count decrements"),
            1 => out = true,
            255 => {
                if cfg!(feature = "memdbg") {
                    println!("O {} ESC", unsafe {
                        ptr::read_unaligned((loc as *const u32).add(2))
                    })
                }
                return false;
            }
            _ => out = false,
        }

        let sp;
        (cur, sp) = unsafe { std::intrinsics::atomic_cxchg_acqrel_acquire(rc_pos, cur, cur - 1) };

        if sp {
            break;
        }
    }

    if cfg!(feature = "memdbg") {
        println!(
            "O {} DEC (c {})",
            unsafe { ptr::read_unaligned((loc as *const u32).add(2)) },
            cur - 1
        )
    }

    out
}

pub fn discern_ref_offsets_core(loc: *mut SlHead) -> Vec<u32> {
    let mut acc = Vec::new();

    unsafe {
        match raw_core_type(loc) {
            Some(CoreType::Ref) => acc.push(0),
            Some(CoreType::ProcLambda) => acc.push(2),
            Some(CoreType::TyDsc) => acc.push(8),
            Some(CoreType::EnvScope) => {
                for l in 0..3 {
                    acc.push(l * PTR_LEN)
                }
            }
            Some(CoreType::EnvLayer) => {
                for ety in 0..ENV_LAYER_SLOTS {
                    acc.push(ety * (PTR_LEN + SYMBOL_LEN) + SYMBOL_LEN)
                }
            }
            Some(CoreType::VecStd) => {
                for i in 1..=ptr::read_unaligned(raw_val_ptr(loc).add(4) as *mut u32) {
                    acc.push(PTR_LEN * i)
                }
            }
            Some(CoreType::VecHash) => {
                for i in 1..=ptr::read_unaligned(raw_val_ptr(loc) as *mut u32) {
                    acc.push(PTR_LEN * i)
                }
            }
            // TODO: Handle VecArr?
            _ => (),
        }
    }

    acc
}

/// Return pointers to every object referenced by a given object,
/// such as can be known from core type information
fn discern_refs_core(loc: *mut SlHead) -> Vec<*mut SlHead> {
    let mut acc = Vec::new();

    let next = (unsafe { ptr::read_unaligned(loc as *mut usize) } >> 16) as _;
    if !nil_p(next) {
        acc.push(next)
    }

    for ofs in discern_ref_offsets_core(loc) {
        // combine argument object address with offsets to read
        // pointers to each referenced object

        let nest = unsafe { ptr::read_unaligned(raw_val_ptr(loc).add(ofs as usize) as _) };
        if !nil_p(nest) {
            acc.push(nest)
        }
    }

    acc
}

/// Return pointers to every object referenced by a given object
fn discern_refs(env: SlHndl, loc: *mut SlHead) -> Vec<*mut SlHead> {
    let acc = discern_refs_core(loc);

    if raw_core_type(loc).is_none() {
        let tid = raw_type_id(loc);

        // TODO: adapt to work with custom types in time

        // first, search the type section of the environment for type
        // descriptions with the first bit set, then follow the
        // pointer to the manifest.

        // here, compare the first four-byte segment to the type
        // ID. if it matches, check the static type section of every
        // field entry in the manifest; note the offset if this
        // indicates a reference (maybe check the length against that
        // too just in case).

        // with the offsets of all references in the object body now
        // recorded, take the words from those addresses, check that
        // they are not nil (another check could be whether the
        // address lies within a region table span, but that's
        // usually-unnecessary debugging), and append them to the
        // reference accumulator.
    }

    acc
}

fn destroy_redir(loc: *mut SlHead) {
    assert!(!nil_p(loc));
    assert_eq!(raw_cfg_byte(loc), Cfg::B0Redir as u8);

    // TODO: eliminate recursion

    unsafe {
        let next = (ptr::read_unaligned(loc as *mut usize) >> 16) as *mut SlHead;

        if raw_cfg_byte(next) == Cfg::B0Redir as u8 {
            if dec_refc(next) {
                destroy_redir(next)
            }
        } else {
            // Invariant: this will NEVER drop a true object's
            // reference count to zero itself
            assert!(!dec_refc(next))
        }

        memmgt::dealloc(loc);
    }
}

// TODO: if the object points to non-core objects, this causes leakage

// TODO: ALL types used in core logic (setup, evaluation, teardown)
// must be properly handled by this function
pub fn destroy_obj_core(loc: *mut SlHead) {
    assert!(!nil_p(loc));

    // log::debug!("Destroying object at {:x}", loc as usize);

    let refs = discern_refs_core(loc);

    for r in refs {
        if !nil_p(r) && dec_refc(r) {
            // TODO: eliminate recursion
            destroy_obj_core(r)
        }
    }

    unsafe { memmgt::dealloc(loc) }
}

pub fn destroy_obj(env: SlHndl, loc: *mut SlHead) {
    assert!(!nil_p(loc));

    let refs = discern_refs(env.clone(), loc);

    for r in refs {
        if !nil_p(r) && dec_refc(r) {
            // TODO: eliminate recursion
            destroy_obj(env.clone(), r)
        }
    }

    unsafe { memmgt::dealloc(loc) }
}

#[cfg(test)]
mod refc_tests {
    use super::*;

    #[test]
    fn up_down() {
        unsafe {
            let reg = memmgt::Region::acq(100);
            let item = memmgt::alloc(reg, 0, memmgt::cap(Cfg::B0BoolF));

            let adrs = item as usize;

            assert_eq!(raw_refc_byte(item), 1);

            inc_refc(item);
            inc_refc(item);

            assert_eq!(raw_refc_byte(item), 3);

            dec_refc(item);
            dec_refc(item);

            assert_eq!(raw_refc_byte(item), 1);

            if dec_refc(item) {
                println!("count reached 0");
                destroy_obj_core(item)
            }

            let new = memmgt::alloc(reg, 0, memmgt::cap(Cfg::B0BoolT));

            assert_eq!(adrs, new as usize);
        }
    }

    #[test]
    fn stack_refs() {
        unsafe {
            let reg = memmgt::Region::acq(100);

            let dme = env_create(reg, None);

            let hdl_a_1 = u64_make(reg);
            let ptr_a = hdl_a_1.get_raw();
            println!("Object at {:x}", ptr_a as usize);

            assert_eq!(raw_refc_byte(ptr_a), 1);

            let hdl_a_2 = hdl_a_1.clone();
            let hdl_a_3 = hdl_a_1.clone();

            assert_eq!(raw_refc_byte(ptr_a), 3);

            let hdl_b_1 = ref_init(reg, hdl_a_1);

            assert_eq!(raw_refc_byte(ptr_a), 3);

            let hdl_c_1 = bool_make(reg);
            let hdl_c_2 = hdl_c_1.clone();

            println!("All objects created");

            set_next_list_elt(dme, hdl_c_1, hdl_a_2);

            println!("Bool next elt set to u64");

            assert_eq!(raw_refc_byte(ptr_a), 3);

            drop(hdl_b_1);

            println!("Ref to u64 dropped");

            assert_eq!(raw_refc_byte(ptr_a), 2);

            drop(hdl_c_2);

            println!("Bool handle dropped");

            assert_eq!(raw_refc_byte(ptr_a), 1);

            drop(hdl_a_3);

            println!("u64 handle dropped");

            assert_eq!(raw_refc_byte(ptr_a), 0);
        }
    }
}

pub fn write_ptr(env: SlHndl, mut loc: SlHndl, offset: u32, pto: SlHndl) {
    assert!(offset + PTR_LEN <= loc.size());

    unsafe {
        let dst = loc.value_ptr().add(offset as usize) as *mut _;
        let cur = ptr::read_unaligned(dst);

        let tgt = pto.get_raw();
        ptr::write_unaligned(dst, tgt);

        if !nil_p(tgt) {
            inc_refc(tgt);
        }

        if !nil_p(cur) && dec_refc(cur) {
            destroy_obj(env, cur)
        }
    }
}

fn write_ptr_atomic(env: SlHndl, mut loc: SlHndl, offset: u32, pto: SlHndl) {
    assert!(offset + PTR_LEN <= loc.size());

    unsafe {
        let dst = loc.value_ptr().add(offset as usize) as *mut _;

        let tgt = pto.get_raw();
        let cur = std::intrinsics::atomic_xchg_acqrel(dst, tgt);

        if !nil_p(tgt) {
            inc_refc(tgt);
        }

        if !nil_p(cur) && dec_refc(cur) {
            destroy_obj(env, cur)
        }
    }
}

pub fn write_ptr_cmpxcg(
    env: SlHndl,
    mut loc: SlHndl,
    offset: u32,
    old: Option<SlHndl>,
    pto: SlHndl,
) -> bool {
    assert!(offset + PTR_LEN <= loc.size());

    unsafe {
        let dst = loc.value_ptr().add(offset as usize) as *mut _;

        let old = match old {
            Some(o) => o.get_raw(),
            None => nil(),
        };

        let tgt = pto.get_raw();
        let p = std::intrinsics::atomic_cxchg_acqrel_acquire(dst, old, tgt).1;

        if p {
            // if !nil_p(tgt) {
            inc_refc(tgt);
            // }

            if !nil_p(old) && dec_refc(old) {
                destroy_obj(env, old)
            }
        }

        p
    }
}

pub fn write_ptr_cmpxcg_may_clr(
    env: SlHndl,
    mut loc: SlHndl,
    offset: u32,
    old: Option<SlHndl>,
    pto: Option<SlHndl>,
) -> bool {
    assert!(offset + PTR_LEN <= loc.size());

    unsafe {
        let dst = loc.value_ptr().add(offset as usize) as *mut _;

        let olp = match old {
            Some(o) => o.get_raw(),
            None => nil(),
        };

        let tgt = match pto {
            Some(p) => p.get_raw(),
            None => nil(),
        };

        let p = std::intrinsics::atomic_cxchg_acqrel_acquire(dst, olp, tgt).1;

        if p {
            if !nil_p(tgt) {
                inc_refc(tgt);
            }

            if !nil_p(olp) && dec_refc(olp) {
                destroy_obj(env, olp)
            }
        }

        p
    }
}

/// Write a Sail object reference to a Sail object without any memory
/// accounting; will NOT handle reference counts
pub unsafe fn write_ptr_unsafe_unchecked(mut loc: SlHndl, offset: u32, pto: SlHndl) {
    debug_assert!(offset + PTR_LEN <= loc.size());
    unsafe {
        let dst = loc.value_ptr().add(offset as usize) as *mut _;
        ptr::write_unaligned(dst, pto.get_raw());
    }
}

pub fn read_ptr(mut loc: SlHndl, offset: u32) -> Option<SlHndl> {
    assert!(offset + PTR_LEN <= loc.size());
    let out = unsafe {
        let slot = get_ptr_ptr_unchecked(loc.clone(), offset);
        write_correct_pos_slot(slot);
        SlHndl::from_raw(ptr::read_unaligned(slot))
    };
    drop(loc);
    out
}

pub fn read_ptr_atomic(mut loc: SlHndl, offset: u32) -> Option<SlHndl> {
    assert!(offset + PTR_LEN <= loc.size());
    let out = unsafe {
        // TODO: is it atomic with below? is an atomic ver needed?
        let slot = get_ptr_ptr_unchecked(loc.clone(), offset);
        write_correct_pos_slot(slot);
        SlHndl::from_raw(std::intrinsics::atomic_load_acquire(slot))
    };
    drop(loc);
    out
}

pub unsafe fn read_ptr_unchecked(mut loc: SlHndl, offset: u32) -> Option<SlHndl> {
    debug_assert!(offset + PTR_LEN <= loc.size());
    unsafe {
        SlHndl::from_raw(ptr::read_unaligned(
            loc.value_ptr().add(offset as usize) as _
        ))
    }
}

#[inline(always)]
unsafe fn get_ptr_ptr_unchecked(mut loc: SlHndl, offset: u32) -> *mut *mut SlHead {
    debug_assert!(offset + PTR_LEN <= loc.size());
    loc.value_ptr().add(offset as usize) as _
}

// TODO: check that the given offset matches a valid field offset in the object?

/// Write to a field of a Sail object of a core type
#[inline(always)]
pub fn write_field<T: SizedBase>(mut loc: SlHndl, offset: u32, src: T) {
    assert!(offset + mem::size_of::<T>() as u32 <= loc.size());
    unsafe {
        let dst = loc.value_ptr().add(offset as usize) as *mut T;
        ptr::write_unaligned(dst, src)
    }
}

/// Write to a field of a Sail object without any checks
#[inline(always)]
pub unsafe fn write_field_unchecked<T: SizedBase>(mut loc: SlHndl, offset: u32, src: T) {
    debug_assert!(offset + mem::size_of::<T>() as u32 <= loc.size());
    let dst = loc.value_ptr().add(offset as usize) as *mut T;
    ptr::write_unaligned(dst, src)
}

/// Write to a field of a Sail object atomically without any checks
#[inline(always)]
pub unsafe fn write_field_atomic_unchecked<T: SizedBase + Copy>(
    mut loc: SlHndl,
    offset: u32,
    src: T,
) {
    debug_assert!(offset + mem::size_of::<T>() as u32 <= loc.size());
    let dst = loc.value_ptr().add(offset as usize) as *mut T;
    std::intrinsics::atomic_store_release(dst, src);
}

/// Write to a field of a Sail object only if the current value is the
/// same as `old`, without any checks
///
/// Returns true if the write was successful, or false if nothing was
/// written
#[inline(always)]
pub unsafe fn write_field_cmpxcg_unchecked<T: SizedBase + Copy>(
    mut loc: SlHndl,
    offset: u32,
    old: T,
    src: T,
) -> bool {
    debug_assert!(offset + mem::size_of::<T>() as u32 <= loc.size());
    let dst = loc.value_ptr().add(offset as usize) as *mut T;
    std::intrinsics::atomic_cxchg_acqrel_acquire(dst, old, src).1
}

/// Read from a field of a Sail object of a core type
#[inline(always)]
pub fn read_field<T: SizedBase>(mut loc: SlHndl, offset: u32) -> T {
    assert!(offset + mem::size_of::<T>() as u32 <= loc.size());
    unsafe {
        let src = loc.value_ptr().add(offset as usize) as *mut T;
        ptr::read_unaligned(src)
    }
}

/// Get a pointer into a Sail object without any checks
#[inline(always)]
unsafe fn get_field_ptr_unchecked<T: SizedBase>(mut loc: SlHndl, offset: u32) -> *mut T {
    debug_assert!(offset + mem::size_of::<T>() as u32 <= loc.size());
    loc.value_ptr().add(offset as usize) as *mut T
}

/// Read from a field of a Sail object without any checks
#[inline(always)]
pub unsafe fn read_field_unchecked<T: SizedBase>(mut loc: SlHndl, offset: u32) -> T {
    ptr::read_unaligned(get_field_ptr_unchecked(loc, offset))
}

/// Read from a field of a Sail object atomically without any checks
#[inline(always)]
pub unsafe fn read_field_atomic_unchecked<T: SizedBase + Copy>(mut loc: SlHndl, offset: u32) -> T {
    std::intrinsics::atomic_load_acquire(get_field_ptr_unchecked(loc, offset))
}

/// Set the pointer to a list element's next element
#[inline(always)]
pub fn set_next_list_elt(env: SlHndl, mut loc: SlHndl, next: SlHndl) {
    // TODO: what happens if the reference count changes between the
    // read and the write? (answer: wrong count ends up written)

    // NOTE: current intent is for the Sail execution model to make
    // the above impossible via separate single Sail execution threads

    unsafe {
        loc.correct_pos();

        let (cfg, prev_ptr) = {
            let head = ptr::read_unaligned(loc.get_raw() as *mut u64);
            (head & u16::MAX as u64, (head >> 16) as *mut SlHead)
        };

        let tgt = next.get_raw();

        ptr::write_unaligned(loc.get_raw() as *mut u64, ((tgt as u64) << 16) + cfg);

        inc_refc(tgt);

        if !nil_p(prev_ptr) && dec_refc(prev_ptr) {
            destroy_obj(env, prev_ptr)
        }
    }
}

#[inline(always)]
pub fn clr_next_list_elt(env: SlHndl, mut loc: SlHndl) {
    // TODO: what happens if the reference count changes between the
    // read and the write? (answer: wrong count ends up written)

    unsafe {
        loc.correct_pos();

        let (cfg, prev_ptr) = {
            let head = ptr::read_unaligned(loc.get_raw() as *mut u64);
            (head & u16::MAX as u64, (head >> 16) as *mut SlHead)
        };

        ptr::write_unaligned(loc.get_raw() as *mut u64, cfg);

        if !nil_p(prev_ptr) && dec_refc(prev_ptr) {
            destroy_obj(env, prev_ptr)
        }
    }
}

// TODO: set_next_list_elt_atomic (?)

/// Set the pointer to a list element's next element only if the
/// current pointer is equivalent to `old`
#[inline(always)]
pub fn set_next_list_elt_cmpxcg(
    env: SlHndl,
    mut loc: SlHndl,
    old: Option<SlHndl>,
    new: SlHndl,
) -> bool {
    unsafe {
        loc.correct_pos();

        let lcp = loc.get_raw();
        let olp = match old {
            Some(o) => o.get_raw(),
            None => nil(),
        };
        let nwp = new.get_raw();

        let mut cfg = std::intrinsics::atomic_load_acquire(lcp as *mut u16) as u64;

        let p = loop {
            let (cur, p) = std::intrinsics::atomic_cxchg_acqrel_acquire(
                lcp as *mut u64,
                ((olp as u64) << 16) + cfg,
                ((nwp as u64) << 16) + cfg,
            );

            let cur_cfg = cur & u16::MAX as u64;

            if p || cur_cfg == cfg {
                break p;
            }

            cfg = cur_cfg
        };

        if p {
            inc_refc(nwp);

            if !nil_p(olp) && dec_refc(olp) {
                destroy_obj(env, olp)
            }
        }

        p
    }
}

#[inline(always)]
pub unsafe fn set_next_list_elt_unsafe_unchecked(loc: SlHndl, next: SlHndl) {
    let head = ptr::read_unaligned(loc.get_raw() as *mut u16);
    ptr::write_unaligned(
        loc.get_raw() as *mut u64,
        ((next.get_raw() as u64) << 16) + head as u64,
    );
}

/// Gets the pointer to the next element from a list element
#[inline(always)]
pub fn get_next_list_elt(loc: SlHndl) -> Option<SlHndl> {
    unsafe {
        let rptr = loc.get_raw();
        write_correct_pos_next(rptr);
        SlHndl::from_raw((ptr::read_unaligned(rptr as *mut usize) >> 16) as _)
    }
}

#[inline(always)]
pub fn ref_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, PTR_LEN, memmgt::cap(Cfg::B8Ptr))) }
}

#[inline(always)]
pub fn ref_init(reg: *mut Region, val: SlHndl) -> SlHndl {
    unsafe {
        let out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, PTR_LEN, memmgt::cap(Cfg::B8Ptr)));

        inc_refc(val.get_raw());
        write_ptr_unsafe_unchecked(out.clone(), 0, val);

        out
    }
}

#[inline(always)]
pub fn sym_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, SYMBOL_LEN, memmgt::cap(Cfg::B4Sym))) }
}

#[inline(always)]
pub fn sym_init(reg: *mut Region, val: u32) -> SlHndl {
    let out = sym_make(reg);
    unsafe { write_field_unchecked(out.clone(), 0, val) };
    out
}

#[inline(always)]
pub fn stdvec_make(reg: *mut Region, cap: u32) -> SlHndl {
    // cap, len, (pointer * cap)
    unsafe {
        let size = vec_size(NUM_32_LEN * 2, PTR_LEN, cap);
        let out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, size, memmgt::cap(Cfg::VecStd)));

        write_field_unchecked::<u32>(out.clone(), 0, cap);
        // write_field_unchecked::<u32>(ptr, 4, 0);

        out
    }
}

#[inline(always)]
pub fn stdvec_init(reg: *mut Region, val: &[SlHndl]) -> SlHndl {
    let len = val.len() as u32;
    let out = stdvec_make(reg, len);
    unsafe { write_field_unchecked(out.clone(), 4, len) }

    for (i, p) in val.into_iter().enumerate() {
        unsafe {
            inc_refc(p.get_raw());
            write_ptr_unsafe_unchecked(out.clone(), 8 + (8 * i as u32), p.clone());
        }
    }

    out
}

#[inline(always)]
pub fn string_make(reg: *mut Region, cap: u32) -> SlHndl {
    // cap, len, (byte * cap)
    unsafe {
        let size = vec_size(NUM_32_LEN * 2, NUM_8_LEN, cap);
        let out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, size, memmgt::cap(Cfg::VecStr)));

        write_field_unchecked::<u32>(out.clone(), 0, cap);
        // write_field_unchecked::<u32>(ptr, 4, 0);

        out
    }
}

#[inline(always)]
pub fn string_init(reg: *mut Region, val: &str) -> SlHndl {
    let len = val.len();
    let mut out = string_make(reg, len.try_into().unwrap());

    unsafe {
        write_field_unchecked(out.clone(), 4, len as u32);

        // TODO: make `out` immutable again by adding an unsafe
        // `value_ptr_known` method for such cases

        let local = std::slice::from_raw_parts_mut(out.value_ptr().add(8), len);
        local.copy_from_slice(val.as_bytes());
    }

    out
}

#[inline(always)]
pub fn hashvec_make(reg: *mut Region, size: u32) -> SlHndl {
    // size, fill, (pointer * size)
    unsafe {
        let top_size = vec_size(NUM_32_LEN * 2, PTR_LEN, size);
        let out =
            SlHndl::from_raw_unchecked(memmgt::alloc(reg, top_size, memmgt::cap(Cfg::VecHash)));

        write_field_unchecked::<u32>(out.clone(), 0, size); // size

        // write_field_unchecked::<u32>(ptr, 4, 0); // fill

        // for i in 0..size {
        //     write_ptr_unsafe_unchecked(out, 4 + 4 + (i * 8), ptr::null_mut());
        // }

        out
    }
}

#[inline(always)]
pub fn proc_lambda_make(reg: *mut Region, argct: u16) -> SlHndl {
    // argct, pointer, (symbol * argct)
    unsafe {
        let size = proc_lambda_size(argct);
        let out =
            SlHndl::from_raw_unchecked(memmgt::alloc(reg, size, memmgt::cap(Cfg::ProcLambda)));

        write_field_unchecked::<u16>(out.clone(), 0, argct);
        // write_ptr_unsafe_unchecked(out, 2, ptr::null_mut());

        out
    }
}

#[inline(always)]
pub fn proc_native_make(reg: *mut Region, argct: u16) -> SlHndl {
    // argct, pointer
    unsafe {
        let size = proc_native_size();
        let out =
            SlHndl::from_raw_unchecked(memmgt::alloc(reg, size, memmgt::cap(Cfg::ProcNative)));

        write_field_unchecked::<u16>(out.clone(), 0, argct);
        // write_ptr_unsafe_unchecked(ptr, 2, ptr::null_mut());

        out
    }
}

pub fn proc_native_init(reg: *mut Region, argct: u16, fun: NativeFn) -> SlHndl {
    let out = proc_native_make(reg, argct);
    unsafe { write_field_unchecked(out.clone(), 2, fun as u64) };
    out
}

#[inline(always)]
pub fn ref_set(env: SlHndl, mut loc: SlHndl, dst: SlHndl) {
    coretypck!(loc ; Ref);
    write_ptr(env, loc, 0, dst)
}

#[inline(always)]
pub fn ref_get(mut loc: SlHndl) -> Option<SlHndl> {
    coretypck!(loc ; Ref);
    read_ptr(loc, 0)
}

#[inline(always)]
pub fn sym_set_id(mut loc: SlHndl, id: u32) {
    coretypck!(loc ; Symbol);
    write_field(loc, 0, id)
}

#[inline(always)]
pub fn sym_get_id(mut loc: SlHndl) -> u32 {
    coretypck!(loc ; Symbol);
    read_field(loc, 0)
}

#[inline(always)]
fn stdvec_set_len(mut loc: SlHndl, len: u32) {
    coretypck!(loc ; VecStd);
    write_field(loc, 4, len)
}

#[inline(always)]
pub fn stdvec_get_len(mut loc: SlHndl) -> u32 {
    coretypck!(loc ; VecStd);
    read_field(loc, 4)
}

#[inline(always)]
fn stdvec_get_cap(mut loc: SlHndl) -> u32 {
    coretypck!(loc ; VecStd);
    read_field(loc, 0)
}

#[inline(always)]
pub fn stdvec_idx(mut loc: SlHndl, idx: u32) -> SlHndl {
    coretypck!(loc ; VecStd);
    // all slots up to length must be filled
    read_ptr(loc, 4 + 4 + (idx * 8)).unwrap()
}

// NOTE: interim position and name
fn true_obj(mut loc: SlHndl) -> SlHndl {
    while loc.cfg_byte() == Cfg::B0Redir as u8 {
        loc = get_next_list_elt(loc).unwrap()
    }
    loc
}

// TODO: Insert check everywhere we attempt to read from a new object
fn write_correct_pos_slot(slot: *mut *mut SlHead) {
    unsafe {
        let orig = ptr::read_unaligned(slot);
        let mut loc = orig;

        if nil_p(orig) {
            return;
        }

        while raw_cfg_byte(loc) == Cfg::B0Redir as u8 {
            loc = (ptr::read_unaligned(loc as *mut usize) >> 16) as *mut SlHead;
        }

        if loc != orig {
            inc_refc(loc);
            ptr::write_unaligned(slot, loc);

            if dec_refc(orig) {
                destroy_redir(orig);
            }
        }
    }
}

fn write_correct_pos_next(loc: *mut SlHead) {
    unsafe {
        let orig = (ptr::read_unaligned(loc as *mut usize) >> 16) as *mut SlHead;
        let mut tgt = orig;

        if nil_p(orig) {
            return;
        }

        while raw_cfg_byte(tgt) == Cfg::B0Redir as u8 {
            tgt = (ptr::read_unaligned(tgt as *mut usize) >> 16) as *mut SlHead;
        }

        if tgt != orig {
            inc_refc(tgt);
            ptr::write_unaligned(
                loc as *mut usize,
                ((tgt as usize) << 16) + ptr::read_unaligned(loc as *mut u16) as usize,
            );

            if dec_refc(orig) {
                destroy_redir(orig);
            }
        }
    }
}

#[cfg(test)]
mod redir_test {
    use super::*;

    fn redir_gen(reg: *mut memmgt::Region, tgt: SlHndl) -> SlHndl {
        unsafe {
            let out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, 0, memmgt::cap(Cfg::B0Redir)));
            inc_refc(tgt.get_raw());
            set_next_list_elt_unsafe_unchecked(out.clone(), tgt);
            out
        }
    }

    #[test]
    fn chain_follow() {
        let region = memmgt::Region::acq(1000);
        let gt = bool_init(region, true);

        let r1 = redir_gen(region, gt.clone());
        let r2 = redir_gen(region, r1.clone());
        let r3 = redir_gen(region, r2.clone());

        assert_eq!(true_obj(gt.clone()).raw, gt.clone().raw);
        assert_eq!(true_obj(r1).raw, gt.clone().raw);
        assert_eq!(true_obj(r2).raw, gt.clone().raw);
        assert_eq!(true_obj(r3).raw, gt.raw);
    }

    #[test]
    fn ow_norm() {
        let region = memmgt::Region::acq(1000);
        let gt = bool_init(region, true);

        let re = redir_gen(region, gt.clone());

        let mut slot: *mut SlHead = re.raw;
        inc_refc(slot);

        assert_ne!(slot, gt.raw);

        write_correct_pos_slot(&mut slot);

        assert_eq!(slot, gt.raw);
    }

    #[test]
    fn ow_next() {
        let region = memmgt::Region::acq(1000);
        let gt = bool_init(region, true);

        let r1 = redir_gen(region, gt.clone());
        let r2 = redir_gen(region, r1.clone());

        let fr = bool_init(region, false);

        inc_refc(r2.raw);
        unsafe { set_next_list_elt_unsafe_unchecked(fr.clone(), r2) };

        write_correct_pos_next(fr.raw);

        assert_eq!(get_next_list_elt(fr).unwrap().raw, gt.raw);
    }
}

#[inline(always)]
pub fn stdvec_push(mut loc: SlHndl, item: SlHndl) {
    // ensure that we're pointing at the true object
    unsafe { loc.correct_pos() };

    let (len, cap) = (stdvec_get_len(loc.clone()), stdvec_get_cap(loc.clone()));

    let hdl = if len >= cap {
        unsafe {
            let new_cap = (cap * 2).max(2);
            let new_ptr = memmgt::realloc(loc.get_raw(), vec_size(8, 8, new_cap));
            ptr::write_unaligned(raw_val_ptr(new_ptr) as *mut u32, new_cap);
            let new_hdl = SlHndl::from_raw_unchecked(new_ptr);
            write_field(new_hdl.clone(), 0, new_cap);
            new_hdl
        }
    } else {
        loc
    };

    unsafe {
        inc_refc(item.get_raw());
        write_ptr_unsafe_unchecked(hdl.clone(), 4 + 4 + (len * 8), item);
    }

    stdvec_set_len(hdl, len + 1);
}

#[inline(always)]
fn string_get_len(mut loc: SlHndl) -> u32 {
    coretypck!(loc ; VecStr);
    read_field(loc, NUM_32_LEN)
}

#[inline(always)]
fn string_set_len(mut loc: SlHndl, len: u32) {
    coretypck!(loc ; VecStr);
    write_field(loc, NUM_32_LEN, len)
}

#[inline(always)]
fn string_get_cap(mut loc: SlHndl) -> u32 {
    coretypck!(loc ; VecStr);
    read_field(loc, 0)
}

#[inline(always)]
pub fn string_set(mut loc: SlHndl, val: &str) {
    assert!(val.len() <= u32::MAX as usize - 8);

    // NOTE: if loc is a redirect, there will be two handle rewrites
    // TODO: add a way to assert first that any rewrites are done

    let cap = string_get_cap(loc.clone());
    let len = val.len() as u32;

    if len <= cap {
        unsafe {
            std::slice::from_raw_parts_mut(loc.value_ptr().add(2 * NUM_32_LEN as usize), len as _)
                .copy_from_slice(val.as_bytes())
        }

        string_set_len(loc, len);
    } else {
        panic!("not enough space in string");
    }
}

#[inline(always)]
pub fn string_get(mut loc: SlHndl) -> &'static str {
    let len = string_get_len(loc.clone());

    unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
            loc.value_ptr().offset(4 + 4) as *mut u8,
            len as usize,
        ))
    }
}

#[inline(always)]
pub fn hashvec_get_size(mut loc: SlHndl) -> u32 {
    coretypck!(loc ; VecHash);
    read_field(loc, 0)
}

/// Returns true if both arguments are the same Sail object
///
/// TODO: symbol handling etc
#[inline(always)]
fn id(fst: SlHndl, lst: SlHndl) -> bool {
    fst == lst
}

/// Returns true if both arguments' values are equal
///
/// TODO: make eq and hash actually function for all types
#[inline(always)]
pub fn core_eq(mut fst: SlHndl, mut lst: SlHndl) -> bool {
    if id(fst.clone(), lst.clone()) {
        true
    } else {
        match fst.core_type() {
            Some(CoreType::Ref) => {
                if ref_get(fst).is_none() {
                    coretypp!(lst ; Ref) && ref_get(lst).is_none()
                } else {
                    false
                }
            }
            Some(CoreType::Nil) => coretypp!(lst ; Ref) && ref_get(lst).is_none(),
            Some(typ) if typ != lst.core_type().unwrap() => false,
            Some(CoreType::Bool) => bool_get(fst) == bool_get(lst),
            Some(CoreType::Symbol) => sym_get_id(fst) == sym_get_id(lst),
            Some(CoreType::VecStr) => string_get(fst) == string_get(lst),
            _ => false,
        }
    }
}

/// Computes a hash value for the provided object
#[inline(always)]
fn core_hash(mut loc: SlHndl) -> u32 {
    match loc.core_type().expect("not a core type") {
        CoreType::Symbol => sym_get_id(loc),
        CoreType::VecStr => str_hash(string_get(loc)),
        _ => 0,
    }
}

/// Provides a simple hash function for string slices
#[inline(always)]
fn str_hash(slice: &str) -> u32 {
    let mut out: u32 = 1;
    for c in slice.bytes() {
        out = out.wrapping_add(out << 5).wrapping_add(c as u32);
    }
    out
}

// TODO: automatically resize as needed (probably as an option) (need "fill" field in subhead)
// TODO: clean up shadowed entries sometime
pub fn hash_map_insert(reg: *mut Region, loc: SlHndl, key: SlHndl, val: SlHndl) {
    let entry = core_cons_copy(reg, key.clone(), val);

    let size = hashvec_get_size(loc.clone());
    let hash = core_hash(key) % size;
    let idx = 4 + 4 + (hash * PTR_LEN);

    let next = read_ptr(loc.clone(), idx);

    if let Some(nx) = next {
        // total references to next entry unchanged by insertion
        unsafe { set_next_list_elt_unsafe_unchecked(entry.clone(), nx) };
    }

    unsafe {
        inc_refc(entry.get_raw());
        write_ptr_unsafe_unchecked(loc, idx, entry);
    }
}

// fn alist_map_insert(reg: *mut Region, loc: *mut SlHead, key: *mut SlHead, val: *mut SlHead) {
//     let entry = core_cons_copy(reg, key, val);
//     let next = core_read_field(loc, 0);
//     if !nil_p(next) {
//         set_next_list_elt(entry, next);
//     }
//     core_write_field(loc, 0, entry)
// }

// fn hash_map_lookup(loc: *mut SlHead, key: *mut SlHead) -> *mut SlHead {
//     let size = hashvec_get_size(loc);
//     let hash = core_hash(key) % size;
//     let entry = core_read_field(loc, 4 + 4 + (hash as usize * PTR_LEN as usize));
//     alist_search(entry, key)
// }

// fn alist_map_lookup(loc: *mut SlHead, key: *mut SlHead) -> *mut SlHead {
//     let entry = core_read_field(loc, 0);
//     alist_search(entry, key)
// }

// fn alist_search(head: *mut SlHead, target: *mut SlHead) -> *mut SlHead {
//     let mut pos = head;
//     loop {
//         if nil_p(pos) {
//             return nil();
//         }
//         if core_eq(ref_get(pos), target) {
//             return pos;
//         }
//         pos = get_next_list_elt(pos);
//     }
// }

#[inline(always)]
pub fn proc_get_argct(mut loc: SlHndl) -> u16 {
    assert!(loc.proc_p());
    read_field(loc, 0)
}

#[inline(always)]
pub fn proc_lambda_set_arg(mut loc: SlHndl, idx: u16, arg: u32) {
    coretypck!(loc ; ProcLambda);
    write_field(loc, (NUM_16_LEN + PTR_LEN) + (idx as u32 * SYMBOL_LEN), arg)
}

#[inline(always)]
pub fn proc_lambda_get_arg(reg: *mut Region, loc: SlHndl, idx: u16) -> SlHndl {
    sym_init(reg, proc_lambda_get_arg_id(loc, idx))
}

#[inline(always)]
pub fn proc_lambda_get_arg_id(mut loc: SlHndl, idx: u16) -> u32 {
    coretypck!(loc ; ProcLambda);
    read_field(loc, (NUM_16_LEN + PTR_LEN) + (idx as u32 * SYMBOL_LEN))
}

#[inline(always)]
pub fn proc_lambda_set_body(mut loc: SlHndl, body: SlHndl) {
    coretypck!(loc ; ProcLambda);
    unsafe {
        inc_refc(body.get_raw());
        write_ptr_unsafe_unchecked(loc, NUM_16_LEN, body);
    }
}

#[inline(always)]
pub fn proc_lambda_get_body(mut loc: SlHndl) -> Option<SlHndl> {
    coretypck!(loc ; ProcLambda);
    read_ptr(loc, NUM_16_LEN)
}

#[inline(always)]
pub fn proc_native_set_body(mut loc: SlHndl, fun: NativeFn) {
    coretypck!(loc ; ProcNative);
    write_field(loc, NUM_16_LEN, fun as u64)
}

#[inline(always)]
pub fn proc_native_get_body(mut loc: SlHndl) -> NativeFn {
    coretypck!(loc ; ProcNative);
    let ptr = read_field::<u64>(loc, NUM_16_LEN) as *const ();
    unsafe { mem::transmute::<_, NativeFn>(ptr) }
}

// TODO: do the below in a better way, maybe in memmgt

/// Copies the value from a Sail object of a core type into a newly
/// allocated object
#[inline(always)]
pub fn core_copy_val(reg: *mut Region, mut src: SlHndl) -> SlHndl {
    let (siz, cfg) = (src.size(), src.cfg_byte());

    unsafe {
        // NOTE: another example; dst created here so can't be a redir
        let mut dst = SlHndl::from_raw_unchecked(memmgt::alloc(
            reg,
            siz,
            memmgt::cap(cfg.try_into().unwrap()),
        ));

        ptr::copy_nonoverlapping(src.value_ptr(), dst.value_ptr(), siz as usize);
        dst
    }
}

/// Copies the values from a pair of Sail objects of core types into a
/// two element list structure
#[inline(always)]
fn core_cons_copy(reg: *mut Region, car: SlHndl, cdr: SlHndl) -> SlHndl {
    let new_cdr = core_copy_val(reg, cdr);
    let new_car = core_copy_val(reg, car);

    unsafe {
        inc_refc(new_cdr.get_raw());
        set_next_list_elt_unsafe_unchecked(new_car.clone(), new_cdr);
    }

    ref_init(reg, new_car)
}

// **********************************************************
// * `car` and `cdr` CANNOT be provided in the Sail internals
// * they do not fit well with the implementation details
// * use `ref_get` and `get_next_list_elt` instead of these
// **********************************************************

// /// Returns the first element of the provided list
// pub fn car(loc: *mut SlHead) -> *mut SlHead {
//     if nil_p(loc) {
//         nil()
//     } else if coretypp!(loc ; Ref) {
//         ref_get(loc)
//     } else {
//         loc
//     }
// }

// /// Returns list of elements following the first element of the provided list
// pub fn cdr(loc: *mut SlHead) -> *mut SlHead {
//     if coretypp!(loc ; Ref) {
//         get_next_list_elt(ref_get(loc))
//     } else {
//         get_next_list_elt(loc)
//     }
// }

// TODO: deal somewhere with dynamic bindings, lexical bindings, argument bindings
// TODO: give env and symtab their own predicate types
// TODO: the core does not use maps except for env and symtab, so consolidate the code
// TODO: improve the env data structure (this is where most evaluation time is spent)


// TODO: should there be a small version (maybe an array with length 6
// to 12) for procedure arguments? note need for modules and types

// TODO: assess changing slot "value" for module and type dicts

// unused, tentative, might aid type checking
type SlSym = u32;

// elements of an environment slot
struct _EnvSlot {
    sym: SlSym,
    obj: *mut SlHead,
}

const ENV_LAYER_SLOTS: u32 = 41;
const ENV_MAX_PROBES: u32 = 5;

/// Creates an environment, which is a list of structures containing
/// distinct hash dictionaries for binding objects, modules, and types
/// to symbols
pub fn env_create(reg: *mut Region, parent: Option<SlHndl>) -> SlHndl {
    unsafe {
        let env =
            SlHndl::from_raw_unchecked(memmgt::alloc(reg, 3 * PTR_LEN, memmgt::cap(Cfg::EnvScope)));

        if let Some(par) = parent {
            inc_refc(par.get_raw());
            set_next_list_elt_unsafe_unchecked(env.clone(), par);
        }

        // println!("<> environment created");

        env
    }
}

pub fn env_layer_make(reg: *mut Region) -> SlHndl {
    // TODO: prevent the technically unnecessary `mut`
    let mut layer = unsafe {
        SlHndl::from_raw_unchecked(memmgt::alloc(
            reg,
            ENV_LAYER_SLOTS * (SYMBOL_LEN + PTR_LEN),
            memmgt::cap(Cfg::EnvLayer),
        ))
    };

    let start = layer.value_ptr() as *mut u32;

    for i in 0..ENV_LAYER_SLOTS {
        unsafe {
            ptr::write_unaligned(start.add(3 * i as usize), 0x80000000);
        }
    }

    layer
}

/// Looks up the given symbol in the given environment, returning the
/// object it refers to
#[inline(always)]
pub fn env_lookup(env: SlHndl, mut sym: SlHndl) -> Option<SlHndl> {
    coretypck!(sym ; Symbol);
    env_lookup_by_id(env, sym_get_id(sym))
}

/// Looks up the given symbol ID in the given environment, returning
/// the object it refers to
#[inline(always)]
pub fn env_lookup_by_id(env: SlHndl, sym_id: u32) -> Option<SlHndl> {
    env_get_binding_loc(env, sym_id).map(|l| unsafe {
        write_correct_pos_slot(l);

        let obj = ptr::read_unaligned(l);
        inc_refc(obj);
        SlHndl::from_raw_unchecked(obj)
    })
}

/// Changes the object pointed to by the given symbol's ID in the
/// environment, if the entry already exists
///
/// Returns false if no mutation was performed, or true if it was
#[inline(always)]
pub fn env_scope_mut(env: SlHndl, mut sym: SlHndl, obj: SlHndl) -> bool {
    coretypck!(sym ; Symbol);
    env_scope_mut_by_id(env, sym_get_id(sym), obj)
}

/// Changes the object pointed to by the given ID in the environment,
/// if the entry already exists
///
/// Returns false if no mutation was performed, or true if it was
#[inline(always)]
pub fn env_scope_mut_by_id(env: SlHndl, sym_id: u32, obj: SlHndl) -> bool {
    match env_get_binding_loc(env.clone(), sym_id) {
        Some(loc) => {
            unsafe {
                let old = ptr::read_unaligned(loc);
                if !nil_p(old) && dec_refc(old) {
                    destroy_obj(env, old)
                }

                let new = obj.get_raw();
                ptr::write_unaligned(loc, new);
                inc_refc(new);
            };
            true
        }
        None => false,
    }
}

/// Inserts the given symbol into the environment, bound to the given
/// object
#[inline(always)]
pub fn env_scope_ins(reg: *mut Region, env: SlHndl, mut sym: SlHndl, obj: SlHndl) {
    coretypck!(sym ; Symbol);
    env_scope_ins_by_id(reg, env, sym_get_id(sym), obj)
}

// TODO: shadowing behavior within a scope is complete nonsense;
// characterize and fix

/// Inserts a symbol with the given ID into the environment, bound to
/// the given object
pub fn env_scope_ins_by_id(reg: *mut Region, mut env: SlHndl, sym_id: u32, obj: SlHndl) {
    coretypck!(env ; EnvScope);

    let layer_offset = match mode_of_sym(sym_id) {
        SymbolMode::Basic => 0,
        SymbolMode::Module => 8,
        SymbolMode::Type => 16,
        SymbolMode::Keyword => return,
    };

    let entry_offset = sym_id % ENV_LAYER_SLOTS;

    let top_layer = unsafe { read_ptr_unchecked(env.clone(), layer_offset) };
    let mut layer_ptr = top_layer.clone();

    'layer: loop {
        if layer_ptr.is_none() {
            let new_layer = env_layer_make(reg);
            // TODO: r!() macro instead of clone?
            match top_layer.clone() {
                Some(tl) => set_next_list_elt(env.clone(), new_layer.clone(), tl),
                None => clr_next_list_elt(env.clone(), new_layer.clone()),
            }
            write_ptr(env.clone(), env.clone(), layer_offset, new_layer.clone());
            layer_ptr = Some(new_layer.clone());
        }

        let lytt = layer_ptr.as_mut().unwrap();
        coretypck!(lytt ; EnvLayer);

        'entry: for o in 0..ENV_MAX_PROBES {
            let slot_offset = (entry_offset + o) % ENV_LAYER_SLOTS;
            let byte_offset = slot_offset * (SYMBOL_LEN + PTR_LEN);

            // println!("inserting: entry is {}, o is {}", entry_offset, o);

            let slot_id: u32 =
                unsafe { read_field_unchecked(layer_ptr.clone().unwrap(), byte_offset) };

            // there needs to be a way to discern an empty slot in all
            // three environment sections
            if slot_id >> 30 != SymbolMode::Keyword as u32 {
                continue 'entry;
            }

            unsafe {
                write_field_unchecked(layer_ptr.clone().unwrap(), byte_offset, sym_id);
                write_ptr(env, layer_ptr.unwrap(), byte_offset + SYMBOL_LEN, obj);
            }

            break 'layer;
        }

        layer_ptr = get_next_list_elt(layer_ptr.unwrap());
    }

    ()
}

/// Looks up the given symbol ID in the given environment, returning
/// the location in the environment of the object it refers to; None
/// if no binding
fn env_get_binding_loc(mut env: SlHndl, sym_id: u32) -> Option<*mut *mut SlHead> {
    coretypck!(env ; EnvScope);

    let layer_offset = match mode_of_sym(sym_id) {
        SymbolMode::Basic => 0,
        SymbolMode::Module => 8,
        SymbolMode::Type => 16,
        SymbolMode::Keyword => return None,
    };

    let entry_offset = sym_id % ENV_LAYER_SLOTS;

    let mut scope_ptr: Option<SlHndl> = Some(env);
    let mut layer_ptr: Option<SlHndl>;

    'scope: loop {
        if scope_ptr.is_none() {
            break 'scope;
        }

        let sctt = scope_ptr.as_mut().unwrap();
        coretypck!(sctt ; EnvScope);

        layer_ptr = unsafe { read_ptr_unchecked(scope_ptr.clone().unwrap(), layer_offset) };

        'layer: loop {
            if layer_ptr.is_none() {
                break 'layer;
            }

            let lytt = layer_ptr.as_mut().unwrap();
            coretypck!(lytt ; EnvLayer);

            'entry: for o in 0..ENV_MAX_PROBES {
                let slot_offset = (entry_offset + o) % ENV_LAYER_SLOTS;
                let byte_offset = slot_offset * (SYMBOL_LEN + PTR_LEN);

                let slot_id: u32 =
                    unsafe { read_field_unchecked(layer_ptr.clone().unwrap(), byte_offset) };

                if slot_id != sym_id {
                    if slot_id >> 30 == SymbolMode::Keyword as u32 {
                        break 'entry;
                    }
                    continue 'entry;
                }

                return Some(unsafe {
                    get_ptr_ptr_unchecked(layer_ptr.unwrap(), byte_offset + SYMBOL_LEN)
                });
            }

            layer_ptr = get_next_list_elt(layer_ptr.unwrap());
        }

        scope_ptr = get_next_list_elt(scope_ptr.unwrap());
    }

    None
}

#[inline(always)]
fn u8_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_8_LEN, memmgt::cap(Cfg::B1U8))) }
}

#[inline(always)]
fn u8_init(reg: *mut Region, val: u8) -> SlHndl {
    let ptr = u8_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
fn u8_set(mut loc: SlHndl, val: u8) {
    coretypck!(loc ; U8);
    write_field(loc, 0, val)
}

#[inline(always)]
fn u8_get(mut loc: SlHndl) -> u8 {
    coretypck!(loc ; U8);
    read_field(loc, 0)
}

#[inline(always)]
fn u16_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_16_LEN, memmgt::cap(Cfg::B2U16))) }
}

#[inline(always)]
fn u16_init(reg: *mut Region, val: u16) -> SlHndl {
    let ptr = u16_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
fn u16_set(mut loc: SlHndl, val: u16) {
    coretypck!(loc ; U16);
    write_field(loc, 0, val)
}

#[inline(always)]
fn u16_get(mut loc: SlHndl) -> u16 {
    coretypck!(loc ; U16);
    read_field(loc, 0)
}

#[inline(always)]
pub fn u32_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_32_LEN, memmgt::cap(Cfg::B4U32))) }
}

#[inline(always)]
pub fn u32_init(reg: *mut Region, val: u32) -> SlHndl {
    let ptr = u32_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
pub fn u32_set(mut loc: SlHndl, val: u32) {
    coretypck!(loc ; U32);
    write_field(loc, 0, val)
}

#[inline(always)]
pub fn u32_get(mut loc: SlHndl) -> u32 {
    coretypck!(loc ; U32);
    read_field(loc, 0)
}

#[inline(always)]
fn u64_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_64_LEN, memmgt::cap(Cfg::B8U64))) }
}

#[inline(always)]
fn u64_init(reg: *mut Region, val: u64) -> SlHndl {
    let ptr = u64_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
fn u64_set(mut loc: SlHndl, val: u64) {
    coretypck!(loc ; U64);
    write_field(loc, 0, val)
}

#[inline(always)]
pub fn u64_get(mut loc: SlHndl) -> u64 {
    coretypck!(loc ; U64);
    read_field(loc, 0)
}

#[inline(always)]
fn u128_make(reg: *mut Region) -> SlHndl {
    unsafe {
        SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_128_LEN, memmgt::cap(Cfg::B16U128)))
    }
}

#[inline(always)]
fn u128_init(reg: *mut Region, val: u128) -> SlHndl {
    let ptr = u128_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
fn u128_set(mut loc: SlHndl, val: u128) {
    coretypck!(loc ; U128);
    write_field(loc, 0, val)
}

#[inline(always)]
fn u128_get(mut loc: SlHndl) -> u128 {
    coretypck!(loc ; U128);
    read_field(loc, 0)
}

#[inline(always)]
fn i8_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_8_LEN, memmgt::cap(Cfg::B1I8))) }
}

#[inline(always)]
fn i8_init(reg: *mut Region, val: i8) -> SlHndl {
    let ptr = i8_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
fn i8_set(mut loc: SlHndl, val: i8) {
    coretypck!(loc ; I8);
    write_field(loc, 0, val)
}

#[inline(always)]
fn i8_get(mut loc: SlHndl) -> i8 {
    coretypck!(loc ; I8);
    read_field(loc, 0)
}

#[inline(always)]
fn i16_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_16_LEN, memmgt::cap(Cfg::B2I16))) }
}

#[inline(always)]
fn i16_init(reg: *mut Region, val: i16) -> SlHndl {
    let ptr = i16_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
fn i16_set(mut loc: SlHndl, val: i16) {
    coretypck!(loc ; I16);
    write_field(loc, 0, val)
}

#[inline(always)]
fn i16_get(mut loc: SlHndl) -> i16 {
    coretypck!(loc ; I16);
    read_field(loc, 0)
}

#[inline(always)]
fn i32_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_32_LEN, memmgt::cap(Cfg::B4I32))) }
}

#[inline(always)]
fn i32_init(reg: *mut Region, val: i32) -> SlHndl {
    let ptr = i32_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
fn i32_set(mut loc: SlHndl, val: i32) {
    coretypck!(loc ; I32);
    write_field(loc, 0, val)
}

#[inline(always)]
fn i32_get(mut loc: SlHndl) -> i32 {
    coretypck!(loc ; I32);
    read_field(loc, 0)
}

#[inline(always)]
pub fn i64_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_64_LEN, memmgt::cap(Cfg::B8I64))) }
}

#[inline(always)]
pub fn i64_init(reg: *mut Region, val: i64) -> SlHndl {
    let ptr = i64_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
pub fn i64_set(mut loc: SlHndl, val: i64) {
    coretypck!(loc ; I64);
    write_field(loc, 0, val)
}

#[inline(always)]
pub fn i64_get(mut loc: SlHndl) -> i64 {
    coretypck!(loc ; I64);
    read_field(loc, 0)
}

#[inline(always)]
fn i128_make(reg: *mut Region) -> SlHndl {
    unsafe {
        SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_128_LEN, memmgt::cap(Cfg::B16I128)))
    }
}

#[inline(always)]
fn i128_init(reg: *mut Region, val: i128) -> SlHndl {
    let ptr = i128_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
fn i128_set(mut loc: SlHndl, val: i128) {
    coretypck!(loc ; I128);
    write_field(loc, 0, val)
}

#[inline(always)]
fn i128_get(mut loc: SlHndl) -> i128 {
    coretypck!(loc ; I128);
    read_field(loc, 0)
}

#[inline(always)]
pub fn f32_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_32_LEN, memmgt::cap(Cfg::B4F32))) }
}

#[inline(always)]
pub fn f32_init(reg: *mut Region, val: f32) -> SlHndl {
    let ptr = f32_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
pub fn f32_set(mut loc: SlHndl, val: f32) {
    coretypck!(loc ; F32);
    write_field(loc, 0, val)
}

#[inline(always)]
pub fn f32_get(mut loc: SlHndl) -> f32 {
    coretypck!(loc ; F32);
    read_field(loc, 0)
}

#[inline(always)]
pub fn f64_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, NUM_64_LEN, memmgt::cap(Cfg::B8F64))) }
}

#[inline(always)]
pub fn f64_init(reg: *mut Region, val: f64) -> SlHndl {
    let ptr = f64_make(reg);
    unsafe { write_field_unchecked(ptr.clone(), 0, val) };
    ptr
}

#[inline(always)]
pub fn f64_set(mut loc: SlHndl, val: f64) {
    coretypck!(loc ; F64);
    write_field(loc, 0, val)
}

#[inline(always)]
pub fn f64_get(mut loc: SlHndl) -> f64 {
    coretypck!(loc ; F64);
    read_field(loc, 0)
}

#[inline(always)]
pub fn bool_make(reg: *mut Region) -> SlHndl {
    unsafe { SlHndl::from_raw_unchecked(memmgt::alloc(reg, 0, memmgt::cap(Cfg::B0BoolF))) }
}

#[inline(always)]
pub fn bool_init(reg: *mut Region, val: bool) -> SlHndl {
    unsafe {
        SlHndl::from_raw_unchecked(memmgt::alloc(
            reg,
            0,
            memmgt::cap(if val { Cfg::B0BoolT } else { Cfg::B0BoolF }),
        ))
    }
}

// TODO: these change the config byte, could mess with head handling

#[inline(always)]
pub fn bool_set(mut loc: SlHndl, val: bool) {
    coretypck!(loc ; Bool);
    unsafe {
        if val {
            ptr::write_unaligned(
                loc.get_raw() as *mut u8,
                ptr::read_unaligned(loc.get_raw() as *mut u8) | 0b00000100,
            )
        } else {
            ptr::write_unaligned(
                loc.get_raw() as *mut u8,
                ptr::read_unaligned(loc.get_raw() as *mut u8) & 0b11111011,
            )
        }
    }
}

#[inline(always)]
pub fn bool_get(mut loc: SlHndl) -> bool {
    coretypck!(loc ; Bool);
    unsafe { ptr::read_unaligned(loc.get_raw() as *mut u8) & 0b00000100 != 0 }
}

// TODO: need a sort of "NsPath" type which stores an arbitrary-length
// path of symbol IDs, used for resolution of "path symbols" (?)

struct _TypeDesc {
    type_sort_and_sym: u32, // type symbol of the described type (first bit HIGH denotes manifest or LOW predicate)
    parent_type: u32,       // type symbol of parent
    predicate_or_manifest: *mut SlHead, // pointer to description data
}

// TODO: establish uses / dependencies for type symbol and type ID
// objects like this are immutable once created
struct _TypeManifest {
    id: u32,    // global ID of this object type
    fldct: u32, // number of fields
    size: u32,  // size of a value of this type

    // FIELD ENTRY -- REPEAT THE BELOW AS NECESSARY
    offset: u32, // offset of this field from value start, in bytes
    length: u32, // length of the value in this field, in bytes
    stype: u32,  // type (STATIC SIZED) of this field (symbol)
    name: u32,   // internally unique keyword symbol: field name
}

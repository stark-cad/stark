// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License. See the top level LICENSE file for the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/sail/core.rs

// Sail types and many, many important utility functions for building
// list structures and evaluating Sail code. Contains the necessary
// definitions for constructing a useful Sail environment.

// <>

//! TODO: Gradual typing; more extensible type system; subtypes

use std::convert::TryFrom;
use std::mem;
use std::ptr;

use super::memmgt::{self, Region};

/// Core type assertion
macro_rules! coretypck {
    ( $var:ident ; $typ:ident ) => {
        assert_eq!(core_type($var).unwrap(), CoreType::$typ);
    };
}

/// Core type predicate
macro_rules! coretypp {
    ( $var:ident ; $typ:ident ) => {
        match core_type($var) {
            Some(t) => t == CoreType::$typ,
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
    *mut SlHead,
}

/// Head includes pointer to next list element
pub const HEAD_LEN: u32 = 8;
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
/// TODO: Think about references, memory management, and associative table support
#[repr(C)]
pub struct SlHead {
    pub cfg: u8,
    pub rc: u8,
}

// TODO: potential change: add a "shared" bit to indicate that an object
// may be read and / or written by other threads

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

enum_and_tryfrom! {
    /// Contains all valid values for the high six bits of an SlHead
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    #[repr(u8)]
    pub enum Cfg {
        B0BoolF = 0b00000000,
        B0BoolT = 0b00000100,
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
            _ => Err(()),
        }
    }
}

/// Signature for Sail functions implemented in Rust
///
/// Arguments:
/// - Memory region in which to place return value
/// - Symbol table
/// - Current environment
/// - Slice containing all Sail arguments
pub type NativeFn = fn(*mut Region, *mut SlHead, *mut SlHead, &[*mut SlHead]) -> *mut SlHead;

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

/// Checks whether a Sail object is a reference to another non-nil object
#[inline(always)]
pub fn nnil_ref_p(loc: *mut SlHead) -> bool {
    match core_type(loc) {
        Some(t) if t == CoreType::Ref => !ref_empty_p(loc),
        _ => false,
    }
}

/// Checks whether a Sail object is a basic symbol
#[inline(always)]
pub fn basic_sym_p(loc: *mut SlHead) -> bool {
    match core_type(loc) {
        Some(t) if t == CoreType::Symbol => mode_of_sym(sym_get_id(loc)) == SymbolMode::Basic,
        _ => false,
    }
}

/// Checks whether a Sail object is an executable procedure
#[inline(always)]
pub fn proc_p(loc: *mut SlHead) -> bool {
    match core_type(loc) {
        Some(t) if t == CoreType::ProcLambda || t == CoreType::ProcNative => true,
        _ => false,
    }
}

// no longer relevant
// pub fn list_elt_p(loc: *mut SlHead) -> bool {
//     (get_cfg_all(loc) & 0b00000010) != 0
// }

/// Checks whether a valid Sail object contains the 4-byte type ID
/// field
#[inline(always)]
pub fn type_fld_p(loc: *mut SlHead) -> bool {
    let head = get_cfg_all(loc);
    (head & 0b00011100) >> 2 == 7
}

/// Checks whether a valid Sail object contains the 4-byte size field
#[inline(always)]
pub fn size_fld_p(loc: *mut SlHead) -> bool {
    let head = get_cfg_all(loc);
    head >> 5 > 5
}

pub fn get_type_id(loc: *mut SlHead) -> u32 {
    assert!(type_fld_p(loc));
    unsafe { ptr::read_unaligned((loc as *mut u8).add(HEAD_LEN as usize) as *const u32) }
}

pub fn get_size(loc: *mut SlHead) -> u32 {
    let code = get_cfg_all(loc) >> 5;
    if code <= 5 {
        0x80000000_u32.rotate_left(code as u32) & 31
    } else {
        assert!(size_fld_p(loc));
        unsafe {
            ptr::read_unaligned(
                (loc as *mut u8).add((HEAD_LEN + (NUM_32_LEN * type_fld_p(loc) as u32)) as usize)
                    as *const u32,
            )
        }
    }
}

fn __dbg_head_info(loc: *mut SlHead) {
    println!(
        "type field?: {}; size field?: {}; size: {}",
        type_fld_p(loc),
        size_fld_p(loc),
        get_size(loc)
    )
}

/// Returns the truthiness of a valid Sail object
#[inline(always)]
pub fn truthy(loc: *mut SlHead) -> bool {
    !(nil_p(loc)
        || (coretypp!(loc ; Bool) && !bool_get(loc))
        || (coretypp!(loc ; Ref) && ref_empty_p(loc)))
}

/// Gets the full configuration byte from a Sail object
#[inline(always)]
fn get_cfg_all(loc: *mut SlHead) -> u8 {
    unsafe { ptr::read_unaligned(loc as *const u8) }
}

/// Gets the size / type configuration from a Sail object
#[inline(always)]
pub fn get_cfg_spec(loc: *mut SlHead) -> Cfg {
    let top_byte = get_cfg_all(loc);
    match Cfg::try_from(top_byte & 0b11111100) {
        Ok(out) => out,
        Err(_) => panic!("invalid cfg specifier: {:08b}", top_byte),
    }
}

/// Gets the base size of a Sail object
#[inline(always)]
pub fn get_base_size(loc: *mut SlHead) -> BaseSize {
    match BaseSize::try_from(get_cfg_all(loc) >> 5) {
        Ok(out) => out,
        Err(_) => unreachable!(),
    }
}

/// Gets base type specifier from a Sail object (its meaning differs with size)
#[inline(always)]
fn get_base_spec(loc: *mut SlHead) -> u8 {
    (get_cfg_all(loc) & 0b00011100) >> 2
}

/// From a valid Sail object, returns a pointer to the start of the value proper
///
/// (After the header and type specifiers, if they exist)
#[inline(always)]
pub fn value_ptr(loc: *mut SlHead) -> *mut u8 {
    let offset = (HEAD_LEN
        + (type_fld_p(loc) as u32 * NUM_32_LEN)
        + (size_fld_p(loc) as u32 * NUM_32_LEN)) as usize;
    unsafe { (loc as *mut u8).add(offset) }
}

/// Returns None if the object is not of a core type, or its type if it is
#[inline(always)]
pub fn core_type(loc: *mut SlHead) -> Option<CoreType> {
    if nil_p(loc) {
        Some(CoreType::Nil)
    } else {
        match CoreType::try_from(get_cfg_spec(loc)) {
            Ok(out) => Some(out),
            Err(_) => None,
        }
    }
}

/// Returns the size of an object, which must be of a core type
#[deprecated]
pub fn core_size(loc: *mut SlHead) -> u32 {
    use CoreType::*;
    match core_type(loc).expect("not a core type") {
        Nil | Bool => 0,
        U8 | I8 => 1,
        U16 | I16 | ErrCode => 2,
        U32 | I32 | F32 | Symbol => 4,
        U64 | I64 | F64 | Ref => 8,
        U128 | I128 | TyDsc => 16,
        VecStd => vec_size(8, 8, unsafe { read_field_unchecked::<u32>(loc, 0) }),
        VecStr => vec_size(8, 1, unsafe { read_field_unchecked::<u32>(loc, 0) }),
        VecHash => vec_size(8, 8, unsafe { read_field_unchecked::<u32>(loc, 0) }),
        VecArr => vec_size(
            8,
            temp_get_size(unsafe { read_field_unchecked::<u32>(loc, 0) }),
            unsafe { read_field_unchecked::<u32>(loc, 4) },
        ),
        VecAny => vec_size(
            12,
            temp_get_size(unsafe { read_field_unchecked::<u32>(loc, 0) }),
            unsafe { read_field_unchecked::<u32>(loc, 4) },
        ),
        ProcLambda => proc_lambda_size(unsafe { read_field_unchecked::<u16>(loc, 0) }),
        ProcNative => proc_native_size(),
        TyMfst => ty_manifest_size(unsafe { read_field_unchecked::<u32>(loc, 4) }),
    }
}

// a VecArr has head of type, length
// a VecAny has head of type, capacity, length

// TODO: the lack of a real type system needs to be rectified

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
pub fn temp_init_from(reg: *mut Region, typ: u32, ptr: *const u8) -> *mut SlHead {
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

// TODO: separate out pointer writes for garbage collection

/// Write to a field of a Sail object of a core type
#[inline(always)]
pub fn core_write_field<T: SizedBase>(loc: *mut SlHead, offset: u32, src: T) {
    unsafe {
        let dst = value_ptr(loc).add(offset as usize) as *mut T;
        assert!(offset + mem::size_of::<T>() as u32 <= core_size(loc));
        ptr::write_unaligned(dst, src)
    }
}

/// Write to a field of a Sail object without any checks
#[inline(always)]
pub unsafe fn write_field_unchecked<T: SizedBase>(loc: *mut SlHead, offset: u32, src: T) {
    debug_assert!(offset as usize + mem::size_of::<T>() <= get_size(loc) as usize);
    let dst = value_ptr(loc).add(offset as usize) as *mut T;
    ptr::write_unaligned(dst, src)
}

/// Write to a field of a Sail object atomically without any checks
#[inline(always)]
pub unsafe fn write_field_atomic_unchecked<T: SizedBase + Copy>(
    loc: *mut SlHead,
    offset: u32,
    src: T,
) {
    debug_assert!(offset as usize + mem::size_of::<T>() <= get_size(loc) as usize);
    let dst = value_ptr(loc).add(offset as usize) as *mut T;
    std::intrinsics::atomic_store_release(dst, src);
}

/// Write to a field of a Sail object only if the current value is the
/// same as `old`, without any checks
///
/// Returns true if the write was successful, or false if nothing was
/// written
#[inline(always)]
pub unsafe fn write_field_cmpxcg_unchecked<T: SizedBase + Copy>(
    loc: *mut SlHead,
    offset: u32,
    old: T,
    src: T,
) -> bool {
    debug_assert!(offset as usize + mem::size_of::<T>() <= get_size(loc) as usize);
    let dst = value_ptr(loc).add(offset as usize) as *mut T;
    std::intrinsics::atomic_cxchg_acqrel_acquire(dst, old, src).1
}

/// Read from a field of a Sail object of a core type
#[inline(always)]
pub fn core_read_field<T: SizedBase>(loc: *mut SlHead, offset: u32) -> T {
    unsafe {
        let src = value_ptr(loc).add(offset as usize) as *mut T;
        assert!(offset + mem::size_of::<T>() as u32 <= core_size(loc));
        ptr::read_unaligned(src)
    }
}

/// Get a pointer into a Sail object without any checks
#[inline(always)]
unsafe fn get_field_ptr_unchecked<T: SizedBase>(loc: *mut SlHead, offset: u32) -> *mut T {
    debug_assert!(offset as usize + mem::size_of::<T>() <= get_size(loc) as usize);
    value_ptr(loc).add(offset as usize) as *mut T
}

/// Read from a field of a Sail object without any checks
#[inline(always)]
pub unsafe fn read_field_unchecked<T: SizedBase>(loc: *mut SlHead, offset: u32) -> T {
    debug_assert!(offset as usize + mem::size_of::<T>() <= get_size(loc) as usize);
    ptr::read_unaligned(get_field_ptr_unchecked(loc, offset))
}

/// Read from a field of a Sail object atomically without any checks
#[inline(always)]
pub unsafe fn read_field_atomic_unchecked<T: SizedBase + Copy>(loc: *mut SlHead, offset: u32) -> T {
    debug_assert!(offset as usize + mem::size_of::<T>() <= get_size(loc) as usize);
    let src = value_ptr(loc).add(offset as usize) as *mut T;
    std::intrinsics::atomic_load_acquire(src)
}

/// Set the pointer to a list element's next element
#[inline(always)]
pub fn set_next_list_elt(loc: *mut SlHead, next: *mut SlHead) {
    unsafe {
        let head = ptr::read_unaligned(loc as *mut u16);
        ptr::write_unaligned(loc as *mut u64, ((next as u64) << 16) + head as u64);
    }
}

/// Set the pointer to a list element's next element only if the
/// current pointer is equivalent to `old`
#[inline(always)]
pub fn set_next_list_elt_cmpxcg(loc: *mut SlHead, old: *mut SlHead, new: *mut SlHead) -> bool {
    unsafe {
        let head = ptr::read_unaligned(loc as *mut u16);
        std::intrinsics::atomic_cxchg_acqrel_acquire(
            loc as *mut u64,
            ((old as u64) << 16) + head as u64,
            ((new as u64) << 16) + head as u64,
        )
        .1
    }
}

/// Gets the pointer to the next element from a list element
///
/// TODO: in future, handle redirects?
#[inline(always)]
pub fn get_next_list_elt(loc: *mut SlHead) -> *mut SlHead {
    assert!(!nil_p(loc));
    unsafe { (ptr::read_unaligned(loc as *mut usize) >> 16) as *mut SlHead }
}

#[inline(always)]
pub fn ref_make(reg: *mut Region) -> *mut SlHead {
    unsafe {
        let ptr = memmgt::alloc(reg, PTR_LEN, memmgt::cap(Cfg::B8Ptr));
        write_field_unchecked(ptr, 0, nil());
        ptr
    }
}

// TODO: disallow null pointers to make sure that nil and the empty list are the same?
#[inline(always)]
pub fn ref_init(reg: *mut Region, val: *mut SlHead) -> *mut SlHead {
    unsafe {
        let ptr = memmgt::alloc(reg, PTR_LEN, memmgt::cap(Cfg::B8Ptr));
        write_field_unchecked(ptr, 0, val);
        ptr
    }
}

#[inline(always)]
pub fn sym_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, SYMBOL_LEN, memmgt::cap(Cfg::B4Sym)) }
}

#[inline(always)]
pub fn sym_init(reg: *mut Region, val: u32) -> *mut SlHead {
    let ptr = sym_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
pub fn stdvec_make(reg: *mut Region, cap: u32) -> *mut SlHead {
    // cap, len, (pointer * cap)
    unsafe {
        let size = vec_size(NUM_32_LEN * 2, PTR_LEN, cap);
        let ptr = memmgt::alloc(reg, size, memmgt::cap(Cfg::VecStd));

        write_field_unchecked::<u32>(ptr, 0, cap);
        write_field_unchecked::<u32>(ptr, 4, 0);

        ptr
    }
}

#[inline(always)]
pub fn stdvec_init(reg: *mut Region, val: &[*mut SlHead]) -> *mut SlHead {
    let len = val.len() as u32;
    let ptr = stdvec_make(reg, len);
    unsafe { write_field_unchecked(ptr, 4, len) }

    for (i, p) in val.iter().enumerate() {
        unsafe { write_field_unchecked(ptr, 8 + (8 * i as u32), *p) }
    }

    ptr
}

#[inline(always)]
pub fn string_make(reg: *mut Region, cap: u32) -> *mut SlHead {
    // cap, len, (byte * cap)
    unsafe {
        let size = vec_size(NUM_32_LEN * 2, NUM_8_LEN, cap);
        let ptr = memmgt::alloc(reg, size, memmgt::cap(Cfg::VecStr));

        write_field_unchecked::<u32>(ptr, 0, cap);
        write_field_unchecked::<u32>(ptr, 4, 0);

        ptr
    }
}

#[inline(always)]
pub fn string_init(reg: *mut Region, val: &str) -> *mut SlHead {
    let len = val.len();
    let ptr = string_make(reg, len.try_into().unwrap());

    unsafe {
        write_field_unchecked(ptr, 4, len as u32);

        let local = std::slice::from_raw_parts_mut(value_ptr(ptr).add(8), len);
        local.copy_from_slice(val.as_bytes());
    }

    ptr
}

#[inline(always)]
pub fn hashvec_make(reg: *mut Region, size: u32) -> *mut SlHead {
    // size, fill, (pointer * size)
    unsafe {
        let top_size = vec_size(NUM_32_LEN * 2, PTR_LEN, size);
        let ptr = memmgt::alloc(reg, top_size, memmgt::cap(Cfg::VecHash));

        write_field_unchecked::<u32>(ptr, 0, size); // size
        write_field_unchecked::<u32>(ptr, 4, 0); // fill

        for i in 0..size {
            write_field_unchecked(ptr, 4 + 4 + (i * 8), ptr::null_mut());
        }

        ptr
    }
}

#[inline(always)]
pub fn proc_lambda_make(reg: *mut Region, argct: u16) -> *mut SlHead {
    // argct, pointer, (symbol * argct)
    unsafe {
        let size = proc_lambda_size(argct);
        let ptr = memmgt::alloc(reg, size, memmgt::cap(Cfg::ProcLambda));

        write_field_unchecked::<u16>(ptr, 0, argct);
        write_field_unchecked(ptr, 2, ptr::null_mut());

        ptr
    }
}

#[inline(always)]
pub fn proc_native_make(reg: *mut Region, argct: u16) -> *mut SlHead {
    // argct, pointer
    unsafe {
        let size = proc_native_size();
        let ptr = memmgt::alloc(reg, size, memmgt::cap(Cfg::ProcNative));

        write_field_unchecked::<u16>(ptr, 0, argct);
        write_field_unchecked(ptr, 2, ptr::null_mut());

        ptr
    }
}

pub fn proc_native_init(reg: *mut Region, argct: u16, fun: NativeFn) -> *mut SlHead {
    // argct, pointer
    unsafe {
        let size = proc_native_size();
        let ptr = memmgt::alloc(reg, size, memmgt::cap(Cfg::ProcNative));

        write_field_unchecked::<u16>(ptr, 0, argct);
        write_field_unchecked(ptr, 2, fun as u64);

        ptr
    }
}

// TODO: this implementation will **easily** cause memory leaks
// TODO: decrement the reference count of previous object
#[inline(always)]
pub fn ref_set(loc: *mut SlHead, dst: *mut SlHead) {
    coretypck!(loc ; Ref);
    core_write_field(loc, 0, dst)
}

#[inline(always)]
pub fn ref_get(loc: *mut SlHead) -> *mut SlHead {
    coretypck!(loc ; Ref);
    core_read_field(loc, 0)
}

#[inline(always)]
fn ref_empty_p(loc: *mut SlHead) -> bool {
    nil_p(ref_get(loc))
}

#[inline(always)]
pub fn sym_set_id(loc: *mut SlHead, id: u32) {
    coretypck!(loc ; Symbol);
    core_write_field(loc, 0, id)
}

#[inline(always)]
pub fn sym_get_id(loc: *mut SlHead) -> u32 {
    coretypck!(loc ; Symbol);
    core_read_field(loc, 0)
}

#[inline(always)]
fn stdvec_set_len(loc: *mut SlHead, len: u32) {
    coretypck!(loc ; VecStd);
    core_write_field(loc, 4, len)
}

#[inline(always)]
pub fn stdvec_get_len(loc: *mut SlHead) -> u32 {
    coretypck!(loc ; VecStd);
    core_read_field(loc, 4)
}

#[inline(always)]
fn stdvec_get_cap(loc: *mut SlHead) -> u32 {
    coretypck!(loc ; VecStd);
    core_read_field(loc, 0)
}

#[inline(always)]
pub fn stdvec_idx(loc: *mut SlHead, idx: u32) -> *mut SlHead {
    coretypck!(loc ; VecStd);
    core_read_field(loc, 4 + 4 + (idx * 8))
}

#[inline(always)]
pub fn stdvec_push(loc: *mut SlHead, item: *mut SlHead) {
    let (len, cap) = (stdvec_get_len(loc), stdvec_get_cap(loc));

    if len < cap {
        core_write_field(loc, 4 + 4 + (len * 8), item);
        stdvec_set_len(loc, len + 1);
    } else {
        panic!("not enough space in vec");
    }
}

#[inline(always)]
fn string_get_len(loc: *mut SlHead) -> u32 {
    coretypck!(loc ; VecStr);
    core_read_field(loc, NUM_32_LEN)
}

#[inline(always)]
fn string_set_len(loc: *mut SlHead, len: u32) {
    coretypck!(loc ; VecStr);
    core_write_field(loc, NUM_32_LEN, len)
}

#[inline(always)]
fn string_get_cap(loc: *mut SlHead) -> u32 {
    coretypck!(loc ; VecStr);
    core_read_field(loc, 0)
}

#[inline(always)]
pub fn string_set(loc: *mut SlHead, val: &str) {
    assert!(val.len() <= u32::MAX as usize - 8);

    let cap = string_get_cap(loc);
    let len = val.len() as u32;

    // TODO: copy using a purpose-designed function
    if len <= cap {
        for (count, c) in val.bytes().enumerate() {
            core_write_field(loc, 4 + 4 + count as u32, c);
        }
        string_set_len(loc, len);
    } else {
        panic!("not enough space in string");
    }
}

#[inline(always)]
pub fn string_get(loc: *mut SlHead) -> &'static str {
    let len = string_get_len(loc);

    unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
            value_ptr(loc).offset(4 + 4) as *mut u8,
            len as usize,
        ))
    }
}

#[inline(always)]
pub fn hashvec_get_size(loc: *mut SlHead) -> u32 {
    coretypck!(loc ; VecHash);
    core_read_field(loc, 0)
}

/// Returns true if both arguments are the same Sail object
///
/// TODO: symbol handling etc
#[inline(always)]
fn id(fst: *mut SlHead, lst: *mut SlHead) -> bool {
    fst == lst
}

/// Returns true if both arguments' values are equal
///
/// TODO: make eq and hash actually function for all types
#[inline(always)]
pub fn core_eq(fst: *mut SlHead, lst: *mut SlHead) -> bool {
    if id(fst, lst) {
        true
    } else {
        match core_type(fst) {
            Some(CoreType::Ref) => {
                if ref_empty_p(fst) {
                    nil_p(lst) || (core_type(lst).unwrap() == CoreType::Ref && ref_empty_p(lst))
                } else {
                    false
                }
            }
            Some(CoreType::Nil) => {
                nil_p(lst) || (core_type(lst).unwrap() == CoreType::Ref && ref_empty_p(lst))
            }
            Some(typ) if typ != core_type(lst).unwrap() => false,
            Some(CoreType::Symbol) => sym_get_id(fst) == sym_get_id(lst),
            Some(CoreType::VecStr) => string_get(fst) == string_get(lst),
            _ => false,
        }
    }
}

/// Computes a hash value for the provided object
#[inline(always)]
fn core_hash(loc: *mut SlHead) -> u32 {
    match core_type(loc).expect("not a core type") {
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
pub fn hash_map_insert(reg: *mut Region, loc: *mut SlHead, key: *mut SlHead, val: *mut SlHead) {
    let entry = core_cons_copy(reg, key, val);

    let size = hashvec_get_size(loc);
    let hash = core_hash(key) % size;
    let idx = 4 + 4 + (hash * PTR_LEN);

    let next = core_read_field(loc, idx);

    if !nil_p(next) {
        set_next_list_elt(entry, next);
    }

    core_write_field(loc, idx, entry)
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
pub fn proc_get_argct(loc: *mut SlHead) -> u16 {
    assert!({
        let typ = core_type(loc).unwrap();
        typ == CoreType::ProcLambda || typ == CoreType::ProcNative
    });
    core_read_field(loc, 0)
}

#[inline(always)]
pub fn proc_lambda_set_arg(loc: *mut SlHead, idx: u16, arg: u32) {
    coretypck!(loc ; ProcLambda);
    core_write_field(loc, (NUM_16_LEN + PTR_LEN) + (idx as u32 * SYMBOL_LEN), arg)
}

#[inline(always)]
pub fn proc_lambda_get_arg(reg: *mut Region, loc: *mut SlHead, idx: u16) -> *mut SlHead {
    sym_init(reg, proc_lambda_get_arg_id(loc, idx))
}

#[inline(always)]
pub fn proc_lambda_get_arg_id(loc: *mut SlHead, idx: u16) -> u32 {
    coretypck!(loc ; ProcLambda);
    core_read_field(loc, (NUM_16_LEN + PTR_LEN) + (idx as u32 * SYMBOL_LEN))
}

#[inline(always)]
pub fn proc_lambda_set_body(loc: *mut SlHead, body: *mut SlHead) {
    coretypck!(loc ; ProcLambda);
    core_write_field(loc, NUM_16_LEN, body)
}

#[inline(always)]
pub fn proc_lambda_get_body(loc: *mut SlHead) -> *mut SlHead {
    coretypck!(loc ; ProcLambda);
    core_read_field(loc, NUM_16_LEN)
}

#[inline(always)]
pub fn proc_native_set_body(loc: *mut SlHead, fun: NativeFn) {
    coretypck!(loc ; ProcNative);
    core_write_field(loc, NUM_16_LEN, fun as u64)
}

#[inline(always)]
pub fn proc_native_get_body(loc: *mut SlHead) -> NativeFn {
    coretypck!(loc ; ProcNative);
    let ptr = core_read_field::<u64>(loc, NUM_16_LEN) as *const ();
    unsafe { mem::transmute::<_, NativeFn>(ptr) }
}

// TODO: there are better ways to do the below, maybe in memmgt

/// Copies the value from a Sail object of a core type into a newly
/// allocated object
#[inline(always)]
pub fn core_copy_val(reg: *mut Region, src: *mut SlHead) -> *mut SlHead {
    let (siz, cfg) = (core_size(src), get_cfg_all(src));

    unsafe {
        let dst = memmgt::alloc(reg, siz, memmgt::cap(cfg.try_into().unwrap()));
        ptr::copy_nonoverlapping(value_ptr(src), value_ptr(dst), siz as usize);
        dst
    }
}

/// Copies the values from a pair of Sail objects of core types into a
/// two element list structure
#[inline(always)]
fn core_cons_copy(reg: *mut Region, car: *mut SlHead, cdr: *mut SlHead) -> *mut SlHead {
    let new_cdr = core_copy_val(reg, cdr);
    let new_car = core_copy_val(reg, car);
    set_next_list_elt(new_car, new_cdr);

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

/// Creates an environment, which is a list of structures containing
/// distinct hash dictionaries for binding objects, modules, and types
/// to symbols
pub fn env_create(reg: *mut Region, parent: *mut SlHead) -> *mut SlHead {
    let env = unsafe { memmgt::alloc(reg, 3 * PTR_LEN, super::T_ENV_ID.0) };
    set_next_list_elt(env, parent);

    unsafe {
        write_field_unchecked(env, 0, nil());
        write_field_unchecked(env, 8, nil());
        write_field_unchecked(env, 16, nil());
    }

    // println!("<> environment created");

    env
}

pub fn env_layer_make(reg: *mut Region) -> *mut SlHead {
    let layer = unsafe {
        memmgt::alloc(
            reg,
            ENV_LAYER_SLOTS * (SYMBOL_LEN + PTR_LEN),
            super::T_ENV_LYR_ID.0,
        )
    };

    let start = value_ptr(layer) as *mut u32;

    for i in 0..ENV_LAYER_SLOTS {
        unsafe {
            ptr::write_unaligned(start.add(3 * i as usize), 0x80000000);
        }
    }

    // println!("<> env layer created");

    layer
}

/// Looks up the given symbol in the given environment, returning the
/// object it refers to
#[inline(always)]
pub fn env_lookup(env: *mut SlHead, sym: *mut SlHead) -> Option<*mut SlHead> {
    coretypck!(sym ; Symbol);
    env_lookup_by_id(env, sym_get_id(sym))
}

/// Looks up the given symbol ID in the given environment, returning
/// the object it refers to
#[inline(always)]
pub fn env_lookup_by_id(env: *mut SlHead, sym_id: u32) -> Option<*mut SlHead> {
    env_get_binding_loc(env, sym_id).map(|i| match i {
        Some(loc) => unsafe { ptr::read_unaligned(loc) },
        None => nil(),
    })
}

/// Changes the object pointed to by the given symbol's ID in the
/// environment, if the entry already exists
///
/// Returns false if no mutation was performed, or true if it was
#[inline(always)]
pub fn env_scope_mut(env: *mut SlHead, sym: *mut SlHead, obj: *mut SlHead) -> bool {
    coretypck!(sym ; Symbol);
    env_scope_mut_by_id(env, sym_get_id(sym), obj)
}

/// Changes the object pointed to by the given ID in the environment,
/// if the entry already exists
///
/// Returns false if no mutation was performed, or true if it was
#[inline(always)]
pub fn env_scope_mut_by_id(env: *mut SlHead, sym_id: u32, obj: *mut SlHead) -> bool {
    match env_get_binding_loc(env, sym_id).unwrap_or(None) {
        Some(loc) => {
            unsafe { ptr::write_unaligned(loc, obj) };
            true
        }
        None => false,
    }
}

/// Inserts the given symbol into the environment, bound to the given
/// object
#[inline(always)]
pub fn env_scope_ins(reg: *mut Region, env: *mut SlHead, sym: *mut SlHead, obj: *mut SlHead) {
    coretypck!(sym ; Symbol);
    env_scope_ins_by_id(reg, env, sym_get_id(sym), obj)
}

// TODO: shadowing behavior within a scope is complete nonsense; fix

/// Inserts a symbol with the given ID into the environment, bound to
/// the given object
pub fn env_scope_ins_by_id(reg: *mut Region, env: *mut SlHead, sym_id: u32, obj: *mut SlHead) {
    assert_eq!(get_type_id(env), super::T_ENV_ID.0);

    // println!("inserting sym {}", sym_id);

    let layer_offset = match mode_of_sym(sym_id) {
        SymbolMode::Basic => 0,
        SymbolMode::Module => 8,
        SymbolMode::Type => 16,
        SymbolMode::Keyword => return,
    };

    let entry_offset = sym_id % ENV_LAYER_SLOTS;

    let top_layer: *mut SlHead = unsafe { read_field_unchecked(env, layer_offset) };
    let mut layer_ptr = top_layer;

    'layer: loop {
        if nil_p(layer_ptr) {
            let new_layer = env_layer_make(reg);
            set_next_list_elt(new_layer, top_layer);
            unsafe { write_field_unchecked(env, layer_offset, new_layer) };
            layer_ptr = new_layer;
        }

        assert_eq!(get_type_id(layer_ptr), super::T_ENV_LYR_ID.0);

        'entry: for o in 0..5 {
            let slot_offset = (entry_offset + o) % ENV_LAYER_SLOTS;
            let byte_offset = slot_offset * (SYMBOL_LEN + PTR_LEN);

            // println!("inserting: entry is {}, o is {}", entry_offset, o);

            let slot_id: u32 = unsafe { read_field_unchecked(layer_ptr, byte_offset) };

            if slot_id >> 30 != SymbolMode::Keyword as u32 {
                continue 'entry;
            }

            unsafe {
                write_field_unchecked(layer_ptr, byte_offset, sym_id);
                write_field_unchecked(layer_ptr, byte_offset + SYMBOL_LEN, obj);
            }

            break 'layer;
        }

        layer_ptr = get_next_list_elt(layer_ptr);
    }

    ()
}

// TODO: simpler return type here?

/// Looks up the given symbol ID in the given environment, returning
/// the location in the environment of the object it refers to; None
/// if no binding; Some(None) if no binding location
fn env_get_binding_loc(env: *mut SlHead, sym_id: u32) -> Option<Option<*mut *mut SlHead>> {
    assert_eq!(get_type_id(env), super::T_ENV_ID.0);

    // println!("seeking sym {}", sym_id);

    let layer_offset = match mode_of_sym(sym_id) {
        SymbolMode::Basic => 0,
        SymbolMode::Module => 8,
        SymbolMode::Type => 16,
        SymbolMode::Keyword => return Some(None),
    };

    let entry_offset = sym_id % ENV_LAYER_SLOTS;

    let mut scope_ptr: *mut SlHead = env;
    let mut layer_ptr: *mut SlHead;

    'scope: loop {
        if nil_p(scope_ptr) {
            break 'scope;
        }

        assert_eq!(get_type_id(scope_ptr), super::T_ENV_ID.0);

        layer_ptr = unsafe { read_field_unchecked(scope_ptr, layer_offset) };

        'layer: loop {
            if nil_p(layer_ptr) {
                break 'layer;
            }

            assert_eq!(get_type_id(layer_ptr), super::T_ENV_LYR_ID.0);

            'entry: for o in 0..5 {
                let slot_offset = (entry_offset + o) % ENV_LAYER_SLOTS;
                let byte_offset = slot_offset * (SYMBOL_LEN + PTR_LEN);

                let slot_id: u32 = unsafe { read_field_unchecked(layer_ptr, byte_offset) };

                if slot_id != sym_id {
                    if slot_id >> 30 == SymbolMode::Keyword as u32 {
                        break 'entry;
                    }
                    continue 'entry;
                }

                return Some(Some(unsafe {
                    get_field_ptr_unchecked(layer_ptr, byte_offset + SYMBOL_LEN)
                }));
            }

            layer_ptr = get_next_list_elt(layer_ptr);
        }

        scope_ptr = get_next_list_elt(scope_ptr);
    }

    None
}

/// Creates a symbol table, which maps symbol strings to symbol IDs
/// and vice versa
///
/// This should take the form of a bimap, a 1 to 1 association between strings and IDs.
/// Two maps, one for each direction, pointing to the same set of cells: (id string).
/// Must keep track of id to assign (counter) and reclaim unused slots if counter reaches max.
fn sym_tab_create(reg: *mut Region, size: u32) -> *mut SlHead {
    let tbl = stdvec_make(reg, 3);

    let id_to_str = hashvec_make(reg, size);
    let str_to_id = hashvec_make(reg, size);

    let id_count = sym_init(reg, 0xC0000000);

    stdvec_push(tbl, id_to_str);
    stdvec_push(tbl, str_to_id);
    stdvec_push(tbl, id_count);

    tbl
}

// TODO: change the symtab structure to use purpose-specific types


// TODO: I probably need to switch the system to lexical binding,
// which prevents "spooky action from within", or procedures changing
// variables in the calling scope; procedures should always either be
// pure (no environment besides arguments), or closures (carrying
// their creation environment with them)

pub fn sym_tab_set_next_id(tbl: *mut SlHead, id: u32) {
    let id_slot = stdvec_idx(tbl, 2);
    sym_set_id(id_slot, id | 0xC0000000);
}

/// Inserts a string object into the symbol table with a particular
/// ID; initialization and helper function only
fn sym_tab_direct_insert(reg: *mut Region, tbl: *mut SlHead, sym: *mut SlHead, idn: u32) {
    let entry = {
        let id = sym_init(reg, idn);
        set_next_list_elt(id, sym);
        id
    };

    let id_to_str = stdvec_idx(tbl, 0);
    let str_to_id = stdvec_idx(tbl, 1);

    let id_size = hashvec_get_size(id_to_str);
    let str_size = hashvec_get_size(str_to_id);

    let id_hash = idn % id_size;
    let str_hash = core_hash(sym) % str_size;

    let id_idx = (NUM_32_LEN + NUM_32_LEN) + (id_hash * PTR_LEN);
    let str_idx = (NUM_32_LEN + NUM_32_LEN) + (str_hash * PTR_LEN);

    let mut id_pos = core_read_field(id_to_str, id_idx);
    let mut str_pos = core_read_field(str_to_id, str_idx);

    let id_entry = ref_init(reg, entry);
    let str_entry = ref_init(reg, entry);

    if nil_p(id_pos) {
        core_write_field(id_to_str, id_idx, id_entry)
    } else {
        while !nil_p(get_next_list_elt(id_pos)) {
            id_pos = get_next_list_elt(id_pos);
        }

        set_next_list_elt(id_pos, id_entry)
    }

    if nil_p(str_pos) {
        core_write_field(str_to_id, str_idx, str_entry)
    } else {
        while !nil_p(get_next_list_elt(str_pos)) {
            str_pos = get_next_list_elt(str_pos);
        }

        set_next_list_elt(str_pos, str_entry)
    }
}

/// Takes the symbol table and a string object to insert, returning
/// the symbol's unique ID
///
/// Uses a lock to ensure IDs are globally unique, even when called by
/// multiple threads
fn sym_tab_insert(reg: *mut Region, tbl: *mut SlHead, sym: *mut SlHead) -> u32 {
    let id_slot = stdvec_idx(tbl, 2);
    let lock_id = value_ptr(id_slot) as *mut u32;

    // we set the top two bits of the ID slot low; if they were not
    // high before this (locked), we try again until they are

    // TODO: could this be implemented with a single compare and exchange?

    // TODO: give up after a certain number of iterations
    unsafe { while std::intrinsics::atomic_and_acquire(lock_id, 0x3FFFFFFF) >> 30 != 3 {} }

    // since the ID slot holds only correct IDs while locked, we treat
    // it normally
    let id_num = sym_get_id(id_slot);
    assert!(id_num < 0x3FFFFFFF);
    sym_tab_direct_insert(reg, tbl, sym, id_num);
    sym_set_id(id_slot, id_num + 1);

    // we set the top two bits of the ID slot high, releasing the lock
    unsafe {
        assert!(std::intrinsics::atomic_or_release(lock_id, 0xC0000000) >> 30 == 0);
    }

    id_num
}

/// Retrieves and returns the string representation of the given symbol
///
/// Looks up normally, by car, and returns cdr
pub fn sym_tab_lookup_by_id(tbl: *mut SlHead, qry: *mut SlHead) -> *mut SlHead {
    sym_tab_lookup_id_num(tbl, sym_get_id(qry))
}

/// Retrieves and returns the symbol referring to the given string
///
/// Must look up by cdr, and return car
fn sym_tab_lookup_by_str(tbl: *mut SlHead, qry: *mut SlHead) -> *mut SlHead {
    let map = stdvec_idx(tbl, 1);
    let size = hashvec_get_size(map);
    let hash = core_hash(qry) % size as u32;

    let mut entry = core_read_field(map, 4 + 4 + (hash * PTR_LEN));

    loop {
        if nil_p(entry) {
            return nil();
        }

        if core_eq(get_next_list_elt(ref_get(entry)), qry) {
            return ref_get(entry);
        }

        entry = get_next_list_elt(entry);
    }
}

/// Retrieves and returns the string representation corresponding to
/// the given symbol ID
///
/// Looks up by ID number directly
pub fn sym_tab_lookup_id_num(tbl: *mut SlHead, id: u32) -> *mut SlHead {
    let map = stdvec_idx(tbl, 0);
    let size = hashvec_get_size(map);
    let hash = id % size as u32;

    let mut entry = core_read_field(map, 4 + 4 + (hash * PTR_LEN));

    loop {
        if nil_p(entry) {
            return nil();
        }

        if sym_get_id(ref_get(entry)) == id {
            return get_next_list_elt(ref_get(entry));
        }

        entry = get_next_list_elt(entry);
    }
}

/// Returns a unique ID for any symbol string; inserts symbol into the
/// table if not already present
pub fn sym_tab_get_id(reg: *mut Region, tbl: *mut SlHead, sym: &str) -> u32 {
    let map = stdvec_idx(tbl, 1);
    let size = hashvec_get_size(map);
    let hash = str_hash(sym) % size;

    let mut entry = core_read_field(map, 4 + 4 + (hash * PTR_LEN));

    while !nil_p(entry) {
        if sym == string_get(get_next_list_elt(ref_get(entry))) {
            return sym_get_id(ref_get(entry));
        }
        entry = get_next_list_elt(entry);
    }

    let record = string_init(reg, sym);

    sym_tab_insert(reg, tbl, record)
}

pub fn sym_tab_add_with_id(reg: *mut Region, tbl: *mut SlHead, sym: &str, id: u32) {
    let record = string_init(reg, sym);
    sym_tab_direct_insert(reg, tbl, record, id);
}

/// Creates a counter for global type IDs
fn typ_ctr_create(reg: *mut Region) -> *mut SlHead {
    u32_init(reg, 0x80000000)
}

pub fn typ_ctr_set_next_id(ctr: *mut SlHead, id: u32) {
    u32_set(ctr, id | 0x80000000)
}

/// Returns the next globally unique object type ID
pub fn typ_ctr_get_id(ctr: *mut SlHead) -> u32 {
    let lock_id = value_ptr(ctr) as *mut u32;

    // the lock is the first bit of the value: high when available,
    // low when locked

    unsafe { while std::intrinsics::atomic_and_acquire(lock_id, 0x7FFFFFFF) >> 31 != 1 {} }

    let id = u32_get(ctr);
    assert!(id < 0x7FFFFFFF);
    u32_set(ctr, id + 1);

    unsafe {
        assert!(std::intrinsics::atomic_or_release(lock_id, 0x80000000) >> 31 == 0);
    }

    id
}

/// Prepares a complete Sail runtime environment, including symbol
/// table and env
///
/// TODO: should regions know about their Sail environment?
/// TODO: insert all the core type symbols
pub fn prep_environment(reg: *mut Region) -> (*mut SlHead, *mut SlHead, *mut SlHead) {
    (
        sym_tab_create(reg, 251),
        typ_ctr_create(reg),
        env_create(reg, nil()),
    )
}

#[inline(always)]
fn u8_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_8_LEN, memmgt::cap(Cfg::B1U8)) }
}

#[inline(always)]
fn u8_init(reg: *mut Region, val: u8) -> *mut SlHead {
    let ptr = u8_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
fn u8_set(loc: *mut SlHead, val: u8) {
    coretypck!(loc ; U8);
    core_write_field(loc, 0, val)
}

#[inline(always)]
fn u8_get(loc: *mut SlHead) -> u8 {
    coretypck!(loc ; U8);
    core_read_field(loc, 0)
}

#[inline(always)]
fn u16_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_16_LEN, memmgt::cap(Cfg::B2U16)) }
}

#[inline(always)]
fn u16_init(reg: *mut Region, val: u16) -> *mut SlHead {
    let ptr = u16_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
fn u16_set(loc: *mut SlHead, val: u16) {
    coretypck!(loc ; U16);
    core_write_field(loc, 0, val)
}

#[inline(always)]
fn u16_get(loc: *mut SlHead) -> u16 {
    coretypck!(loc ; U16);
    core_read_field(loc, 0)
}

#[inline(always)]
pub fn u32_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_32_LEN, memmgt::cap(Cfg::B4U32)) }
}

#[inline(always)]
pub fn u32_init(reg: *mut Region, val: u32) -> *mut SlHead {
    let ptr = u32_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
pub fn u32_set(loc: *mut SlHead, val: u32) {
    coretypck!(loc ; U32);
    core_write_field(loc, 0, val)
}

#[inline(always)]
pub fn u32_get(loc: *mut SlHead) -> u32 {
    coretypck!(loc ; U32);
    core_read_field(loc, 0)
}

#[inline(always)]
fn u64_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_64_LEN, memmgt::cap(Cfg::B8U64)) }
}

#[inline(always)]
fn u64_init(reg: *mut Region, val: u64) -> *mut SlHead {
    let ptr = u64_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
fn u64_set(loc: *mut SlHead, val: u64) {
    coretypck!(loc ; U64);
    core_write_field(loc, 0, val)
}

#[inline(always)]
pub fn u64_get(loc: *mut SlHead) -> u64 {
    coretypck!(loc ; U64);
    core_read_field(loc, 0)
}

#[inline(always)]
fn u128_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_128_LEN, memmgt::cap(Cfg::B16U128)) }
}

#[inline(always)]
fn u128_init(reg: *mut Region, val: u128) -> *mut SlHead {
    let ptr = u128_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
fn u128_set(loc: *mut SlHead, val: u128) {
    coretypck!(loc ; U128);
    core_write_field(loc, 0, val)
}

#[inline(always)]
fn u128_get(loc: *mut SlHead) -> u128 {
    coretypck!(loc ; U128);
    core_read_field(loc, 0)
}

#[inline(always)]
fn i8_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_8_LEN, memmgt::cap(Cfg::B1I8)) }
}

#[inline(always)]
fn i8_init(reg: *mut Region, val: i8) -> *mut SlHead {
    let ptr = i8_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
fn i8_set(loc: *mut SlHead, val: i8) {
    coretypck!(loc ; I8);
    core_write_field(loc, 0, val)
}

#[inline(always)]
fn i8_get(loc: *mut SlHead) -> i8 {
    coretypck!(loc ; I8);
    core_read_field(loc, 0)
}

#[inline(always)]
fn i16_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_16_LEN, memmgt::cap(Cfg::B2I16)) }
}

#[inline(always)]
fn i16_init(reg: *mut Region, val: i16) -> *mut SlHead {
    let ptr = i16_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
fn i16_set(loc: *mut SlHead, val: i16) {
    coretypck!(loc ; I16);
    core_write_field(loc, 0, val)
}

#[inline(always)]
fn i16_get(loc: *mut SlHead) -> i16 {
    coretypck!(loc ; I16);
    core_read_field(loc, 0)
}

#[inline(always)]
fn i32_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_32_LEN, memmgt::cap(Cfg::B4I32)) }
}

#[inline(always)]
fn i32_init(reg: *mut Region, val: i32) -> *mut SlHead {
    let ptr = i32_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
fn i32_set(loc: *mut SlHead, val: i32) {
    coretypck!(loc ; I32);
    core_write_field(loc, 0, val)
}

#[inline(always)]
fn i32_get(loc: *mut SlHead) -> i32 {
    coretypck!(loc ; I32);
    core_read_field(loc, 0)
}

#[inline(always)]
pub fn i64_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_64_LEN, memmgt::cap(Cfg::B8I64)) }
}

#[inline(always)]
pub fn i64_init(reg: *mut Region, val: i64) -> *mut SlHead {
    let ptr = i64_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
pub fn i64_set(loc: *mut SlHead, val: i64) {
    coretypck!(loc ; I64);
    core_write_field(loc, 0, val)
}

#[inline(always)]
pub fn i64_get(loc: *mut SlHead) -> i64 {
    coretypck!(loc ; I64);
    core_read_field(loc, 0)
}

#[inline(always)]
fn i128_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_128_LEN, memmgt::cap(Cfg::B16I128)) }
}

#[inline(always)]
fn i128_init(reg: *mut Region, val: i128) -> *mut SlHead {
    let ptr = i128_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
fn i128_set(loc: *mut SlHead, val: i128) {
    coretypck!(loc ; I128);
    core_write_field(loc, 0, val)
}

#[inline(always)]
fn i128_get(loc: *mut SlHead) -> i128 {
    coretypck!(loc ; I128);
    core_read_field(loc, 0)
}

#[inline(always)]
pub fn f32_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_32_LEN, memmgt::cap(Cfg::B4F32)) }
}

#[inline(always)]
pub fn f32_init(reg: *mut Region, val: f32) -> *mut SlHead {
    let ptr = f32_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
pub fn f32_set(loc: *mut SlHead, val: f32) {
    coretypck!(loc ; F32);
    core_write_field(loc, 0, val)
}

#[inline(always)]
pub fn f32_get(loc: *mut SlHead) -> f32 {
    coretypck!(loc ; F32);
    core_read_field(loc, 0)
}

#[inline(always)]
pub fn f64_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_64_LEN, memmgt::cap(Cfg::B8F64)) }
}

#[inline(always)]
pub fn f64_init(reg: *mut Region, val: f64) -> *mut SlHead {
    let ptr = f64_make(reg);
    unsafe { write_field_unchecked(ptr, 0, val) };
    ptr
}

#[inline(always)]
pub fn f64_set(loc: *mut SlHead, val: f64) {
    coretypck!(loc ; F64);
    core_write_field(loc, 0, val)
}

#[inline(always)]
pub fn f64_get(loc: *mut SlHead) -> f64 {
    coretypck!(loc ; F64);
    core_read_field(loc, 0)
}

#[inline(always)]
pub fn bool_make(reg: *mut Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, 0, memmgt::cap(Cfg::B0BoolF)) }
}

#[inline(always)]
pub fn bool_init(reg: *mut Region, val: bool) -> *mut SlHead {
    unsafe {
        memmgt::alloc(
            reg,
            0,
            memmgt::cap(if val { Cfg::B0BoolT } else { Cfg::B0BoolF }),
        )
    }
}

#[inline(always)]
pub fn bool_set(loc: *mut SlHead, val: bool) {
    coretypck!(loc ; Bool);
    unsafe {
        if val {
            ptr::write_unaligned(
                loc as *mut u8,
                ptr::read_unaligned(loc as *mut u8) | 0b00000100,
            )
        } else {
            ptr::write_unaligned(
                loc as *mut u8,
                ptr::read_unaligned(loc as *mut u8) & 0b11111011,
            )
        }
    }
}

#[inline(always)]
pub fn bool_get(loc: *mut SlHead) -> bool {
    coretypck!(loc ; Bool);
    unsafe { ptr::read_unaligned(loc as *mut u8) & 0b00000100 != 0 }
}

// TODO: need a sort of "NsPath" type which stores an arbitrary-length
// path of symbol IDs, used for resolution of "path symbols" (?)

struct _TypeDesc {
    type_sort_and_sym: u32, // type symbol of the described type (first bit denotes manifest or predicate)
    parent_type: u32,       // type symbol of parent
    manifest_or_predicate: *mut SlHead, // pointer to description data
}
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

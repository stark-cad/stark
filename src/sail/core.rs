//! TODO: Gradual typing; more extensible type system; subtypes

use std::convert::TryFrom;
use std::mem;
use std::ptr;

use super::memmgt;

/// Core type assertion
macro_rules! coretypck {
    ( $var:ident ; $typ:ident ) => {
        assert_eq!(core_type($var).unwrap(), CoreType::$typ);
    };
}

/// Core type predicate
macro_rules! coretypp {
    ( $var:ident ; $typ:ident ) => {
        core_type($var).unwrap() == CoreType::$typ
    };
}

/// Trait for base types that are always the same size
pub trait SizedBase {}

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

/// head includes pointer to next list element
pub const HEAD_LEN: u8 = 8;
pub const PTR_LEN: u8 = 8;
pub const SYMBOL_LEN: u8 = 4;
pub const NUM_8_LEN: u8 = 1;
pub const NUM_16_LEN: u8 = 2;
pub const NUM_32_LEN: u8 = 4;
pub const NUM_64_LEN: u8 = 8;
pub const NUM_128_LEN: u8 = 16;

/// Header for all Sail values in memory (optimizations later)
/// **Handle ONLY using methods such as sail::alloc and etc**
/// Should only store information that every referenced Sail value needs
/// TODO: Think about references, memory management, and associative table support
#[repr(C)]
pub struct SlHead {
    pub cfg: u8,
    pub rc: u8,
}

// TODO: potential change: add a "shared" bit to indicate that a value
// may be read and / or written by other threads

/// ALL Sail values that may be independently referenced, begin with bytes of this format
/// size: 4 bits - base type: 3 bit - list elt: 1 bit - type pred: 1 bit - rc: 8 bits
/// The first eight bits determine the subsequent memory layout
const _MIN_HEAD: u16 = 0b1110001011111111;

/// Pointer to the next element of a linked list;
/// is tagged with the SlHead (upper 2 unused bytes)
struct _SlListPtr {
    ptr: *mut SlHead,
}

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
    #[derive(Debug, PartialEq, Eq)]
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
        B16Other = 0b10111100,
        VecStd = 0b11000000,
        VecStr = 0b11000100,
        VecHash = 0b11001000,
        VecAny = 0b11001100,
        VecOther = 0b11011100,
        ProcLambda = 0b11100000,
        ProcNative = 0b11100100,
        Other = 0b11111100,
    }
}

enum_and_tryfrom! {
    /// All type sizes that may be specified in the head
    #[derive(Debug, PartialEq, Eq)]
    #[repr(u8)]
    pub enum BaseSize {
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

/// **Core types that must be known in order to assemble the runtime**
/// They can be represented in the value head, without an additional type specifier
/// Null pointers "point to" Nil values; the concept is like interning
/// The next 15 types have statically known size, and correspond to Rust types
/// The last 5 types have variable size, and must be inspected to get a size
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
    VecStd,
    VecStr,
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
            Cfg::VecStd => Ok(Self::VecStd),
            Cfg::VecStr => Ok(Self::VecStr),
            Cfg::VecHash => Ok(Self::VecHash),
            Cfg::ProcLambda => Ok(Self::ProcLambda),
            Cfg::ProcNative => Ok(Self::ProcNative),
            _ => Err(()),
        }
    }
}

pub type NativeFn =
    fn(*mut memmgt::Region, *mut SlHead, *mut SlHead, &[*mut SlHead]) -> *mut SlHead;

// a malformed list (single cons cell included) can simply be a list that ends in a value that is not a list element!
// two element list: List -> value (list elt) -> value (list elt) -> [null]
// cons cell: List -> value (list elt) -> value (NOT list elt)

// What can follow a list head value:
// - A sequence of list elements ending in a list element (list)
// - A sequence of list elements ending in a non element (malformed list or cons cell)
// - A single non element (???)

/// Creates a nil Sail value
#[inline(always)]
pub fn nil() -> *mut SlHead {
    ptr::null_mut()
}

/// Checks whether a pointer, ostensibly to a Sail value, is null
#[inline(always)]
pub fn nil_p(loc: *mut SlHead) -> bool {
    loc.is_null()
}

// #[inline(always)]
// pub fn atom_p(loc: *mut SlHead) -> bool {
//     match core_type(loc) {
//         Some(t) if t != CoreType::Ref => true,
//         _ => false,
//     }
// }

#[inline(always)]
pub fn ref_p(loc: *mut SlHead) -> bool {
    match core_type(loc) {
        Some(t) if t == CoreType::Ref && !nil_p(ref_get(loc)) => true,
        _ => false,
    }
}

#[inline(always)]
pub fn symbol_p(loc: *mut SlHead) -> bool {
    match core_type(loc) {
        Some(t) if t == CoreType::Symbol => true,
        _ => false,
    }
}

#[inline(always)]
pub fn proc_p(loc: *mut SlHead) -> bool {
    match core_type(loc) {
        Some(t) if t == CoreType::ProcLambda || t == CoreType::ProcNative => true,
        _ => false,
    }
}

// /// Checks a valid Sail value to determine whether it is a list element
// #[inline(always)]
// pub fn list_elt_p(loc: *mut SlHead) -> bool {
//     (get_cfg_all(loc) & 0b00000010) != 0
// }

/// Checks whether a valid Sail value has a type specifier with a predicate
#[inline(always)]
pub fn pred_type_p(loc: *mut SlHead) -> bool {
    (get_cfg_all(loc) & 0b00000001) != 0
}

/// Checks whether a valid Sail value has a type specifier for itself alone
#[inline(always)]
pub fn self_type_p(loc: *mut SlHead) -> bool {
    let head = get_cfg_all(loc);
    if head >> 5 == 7 || (head & 0b00011100) >> 2 == 7 {
        true
    } else {
        false
    }
}

/// Gets the full configuration byte from a Sail value
#[inline(always)]
fn get_cfg_all(loc: *mut SlHead) -> u8 {
    unsafe { ptr::read_unaligned(loc as *const u8) }
}

/// Gets the size / type configuration from a Sail value
#[inline(always)]
pub fn get_cfg_spec(loc: *mut SlHead) -> Cfg {
    match Cfg::try_from(get_cfg_all(loc) & 0b11111100) {
        Ok(out) => out,
        Err(_) => panic!("invalid cfg specifier"),
    }
}

/// Gets the base size of a Sail value
#[inline(always)]
pub fn get_base_size(loc: *mut SlHead) -> BaseSize {
    match BaseSize::try_from(get_cfg_all(loc) >> 5) {
        Ok(out) => out,
        Err(_) => unreachable!(),
    }
}

/// Gets base type specifier from a Sail value (its meaning differs with size)
#[inline(always)]
fn get_base_spec(loc: *mut SlHead) -> u8 {
    (get_cfg_all(loc) & 0b00011100) >> 2
}

/// From a valid Sail value, returns a pointer to the start of the value proper
/// (After the header and type specifiers, if they exist)
#[inline(always)]
pub fn value_ptr(loc: *mut SlHead) -> *mut u8 {
    let offset = if self_type_p(loc) && pred_type_p(loc) {
        HEAD_LEN + SYMBOL_LEN + SYMBOL_LEN
    } else if self_type_p(loc) || pred_type_p(loc) {
        HEAD_LEN + SYMBOL_LEN
    } else {
        HEAD_LEN
    } as usize;

    unsafe { (loc as *mut u8).add(offset) }
}

/// Returns None if the value is not a core type, or its type if it is
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

/// Returns the size of a value, which must be of a core type
#[inline(always)]
pub fn core_size(loc: *mut SlHead) -> usize {
    use CoreType::*;
    match core_type(loc).expect("not a core type") {
        Nil | Bool => 0,
        U8 | I8 => 1,
        U16 | I16 => 2,
        U32 | I32 | F32 | Symbol => 4,
        U64 | I64 | F64 | Ref => 8,
        U128 | I128 => 16,
        VecStd => vec_size(4, 8, unsafe { read_field_unchecked::<u32>(loc, 0) }
            as usize),
        VecStr => vec_size(4, 1, unsafe { read_field_unchecked::<u32>(loc, 0) }
            as usize),
        VecHash => vec_size(4, 8, unsafe { read_field_unchecked::<u32>(loc, 0) }
            as usize),
        ProcLambda => proc_lambda_size(unsafe { read_field_unchecked::<u16>(loc, 0) }),
        ProcNative => proc_native_size(),
    }
}

/// Gives the overall size of a Vec with certain parameters
#[inline(always)]
fn vec_size(head_size: usize, elt_size: usize, capacity: usize) -> usize {
    head_size + head_size + (elt_size * capacity)
}

#[inline(always)]
fn proc_lambda_size(argct: u16) -> usize {
    (NUM_16_LEN + PTR_LEN) as usize + (SYMBOL_LEN as usize * argct as usize)
}

#[inline(always)]
fn proc_native_size() -> usize {
    (NUM_16_LEN + PTR_LEN) as usize
}

/// Write to a field of a core type
#[inline(always)]
fn core_write_field<T: SizedBase>(loc: *mut SlHead, offset: usize, src: T) {
    unsafe {
        let dst = value_ptr(loc).add(offset) as *mut T;
        assert!(offset + mem::size_of::<T>() <= core_size(loc));
        ptr::write_unaligned(dst, src)
    }
}

unsafe fn write_field_unchecked<T: SizedBase>(loc: *mut SlHead, offset: usize, src: T) {
    let dst = value_ptr(loc).add(offset) as *mut T;
    ptr::write_unaligned(dst, src)
}

/// Read from a field of a core type
#[inline(always)]
pub fn core_read_field<T: SizedBase>(loc: *mut SlHead, offset: usize) -> T {
    unsafe {
        let src = value_ptr(loc).add(offset) as *mut T;
        assert!(offset + mem::size_of::<T>() <= core_size(loc));
        ptr::read_unaligned(src)
    }
}

unsafe fn read_field_unchecked<T: SizedBase>(loc: *mut SlHead, offset: usize) -> T {
    let src = value_ptr(loc).add(offset) as *mut T;
    ptr::read_unaligned(src)
}

// /// Set a Sail value's list element bit to true or false
// #[inline(always)]
// fn set_list_elt_bit(loc: *mut SlHead, elt: bool) {
//     let old = get_cfg_all(loc);
//     let new = if elt {
//         old | 0b00000010
//     } else {
//         old & 0b11111101
//     };
//     unsafe { ptr::write_unaligned(loc as *mut u8, new) }
// }

/// Set the pointer to a list element's next element
#[inline(always)]
pub fn set_next_list_elt(loc: *mut SlHead, next: *mut SlHead) {
    // if !list_elt_p(loc) {
    //     set_list_elt_bit(loc, true);
    // }
    unsafe {
        let head = ptr::read_unaligned(loc as *mut u16);
        ptr::write_unaligned(
            loc as *mut *mut SlHead,
            ((next as usize) << 16) as *mut SlHead,
        );
        ptr::write_unaligned(loc as *mut u16, head);
    }
}

/// TODO: in future, handle redirects?
/// Gets the pointer to the next element from a list element
#[inline(always)]
pub fn get_next_list_elt(loc: *mut SlHead) -> *mut SlHead {
    // assert!(list_elt_p(loc));
    unsafe { (ptr::read_unaligned(loc as *mut usize) >> 16) as *mut SlHead }
}

/// TODO: disallow null pointers to make sure that nil and the empty list are the same?
#[inline(always)]
pub fn init_ref(reg: *mut memmgt::Region) -> *mut SlHead {
    let ptr = unsafe { memmgt::alloc(reg, PTR_LEN as usize, Cfg::B8Ptr as u8) };
    core_write_field(ptr, 0, nil());
    ptr
}

#[inline(always)]
pub fn init_symbol(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, SYMBOL_LEN as usize, Cfg::B4Sym as u8) }
}

#[inline(always)]
pub fn init_stdvec(reg: *mut memmgt::Region, cap: u32) -> *mut SlHead {
    // cap, len, (pointer * cap)
    let size = vec_size(NUM_32_LEN as usize, PTR_LEN as usize, cap as usize);
    let ptr = unsafe { memmgt::alloc(reg, size, Cfg::VecStd as u8) };

    core_write_field::<u32>(ptr, 0, cap);
    core_write_field::<u32>(ptr, 4, 0);

    ptr
}

#[inline(always)]
pub fn init_string(reg: *mut memmgt::Region, cap: u32) -> *mut SlHead {
    // cap, len, (byte * cap)
    let size = vec_size(NUM_32_LEN as usize, NUM_8_LEN as usize, cap as usize);
    let ptr = unsafe { memmgt::alloc(reg, size, Cfg::VecStr as u8) };

    core_write_field::<u32>(ptr, 0, cap);
    core_write_field::<u32>(ptr, 4, 0);

    ptr
}

#[inline(always)]
pub fn init_hash_map(reg: *mut memmgt::Region, size: u32) -> *mut SlHead {
    // size, fill, (pointer * size)
    let top_size = vec_size(NUM_32_LEN as usize, PTR_LEN as usize, size as usize);
    let ptr = unsafe { memmgt::alloc(reg, top_size, Cfg::VecHash as u8) };

    core_write_field::<u32>(ptr, 0, size); // size
    core_write_field::<u32>(ptr, 4, 0); // fill

    for i in 0..size as usize {
        core_write_field(ptr, 4 + 4 + (i * 8), ptr::null_mut());
    }

    ptr
}

#[inline(always)]
fn init_alist_map(reg: *mut memmgt::Region) -> *mut SlHead {
    let ptr = unsafe { memmgt::alloc(reg, PTR_LEN as usize, Cfg::B8Ptr as u8) };
    core_write_field(ptr, 0, ptr::null_mut());
    ptr
}

#[inline(always)]
pub fn init_proc_lambda(reg: *mut memmgt::Region, argct: u16) -> *mut SlHead {
    // argct, pointer, (symbol * argct)
    let size = proc_lambda_size(argct);
    let ptr = unsafe { memmgt::alloc(reg, size, Cfg::ProcLambda as u8) };

    core_write_field::<u16>(ptr, 0, argct);
    core_write_field(ptr, 2, ptr::null_mut());

    ptr
}

#[inline(always)]
pub fn init_proc_native(reg: *mut memmgt::Region, argct: u16) -> *mut SlHead {
    // argct, pointer
    let size = proc_native_size();
    let ptr = unsafe { memmgt::alloc(reg, size, Cfg::ProcNative as u8) };

    core_write_field::<u16>(ptr, 0, argct);
    core_write_field(ptr, 2, ptr::null_mut());

    ptr
}

// TODO: should functions like this check for correct type?
/// TODO: this implementation will **easily** cause memory leaks
/// TODO: decrement the reference count of previously held value
#[inline(always)]
pub fn ref_set(loc: *mut SlHead, dst: *mut SlHead) {
    coretypck!(loc ; Ref);
    core_write_field(loc, 0, dst)
}

/// Gets the pointer contained within a list head
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
    core_read_field(loc, 4 + 4 + (idx as usize * 8))
}

#[inline(always)]
pub fn stdvec_push(loc: *mut SlHead, item: *mut SlHead) {
    let (len, cap) = (stdvec_get_len(loc), stdvec_get_cap(loc));

    if len < cap {
        core_write_field(loc, 4 + 4 + (len as usize * 8), item);
        stdvec_set_len(loc, len + 1);
    } else {
        panic!("not enough space in vec");
    }
}

#[inline(always)]
fn string_get_len(loc: *mut SlHead) -> u32 {
    coretypck!(loc ; VecStr);
    core_read_field(loc, NUM_32_LEN as usize)
}

#[inline(always)]
fn string_set_len(loc: *mut SlHead, len: u32) {
    coretypck!(loc ; VecStr);
    core_write_field(loc, NUM_32_LEN as usize, len)
}

#[inline(always)]
fn string_get_cap(loc: *mut SlHead) -> u32 {
    coretypck!(loc ; VecStr);
    core_read_field(loc, 0)
}

#[inline(always)]
pub fn string_set(loc: *mut SlHead, val: &str) {
    let cap = string_get_cap(loc);
    let len = val.len() as u32;

    // TODO: copy using a purpose-designed function
    if len <= cap {
        let mut count = 0;
        for c in val.bytes() {
            core_write_field(loc, 4 + 4 + count as usize, c);
            count += 1;
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

/// Returns true if both arguments are the same Sail value
/// TODO: symbol handling etc
#[inline(always)]
fn id(fst: *mut SlHead, lst: *mut SlHead) -> bool {
    if fst == lst {
        true
    } else {
        false
    }
}

/// Returns true if both arguments match in structure
/// TODO: make eq and hash actually function for all types
#[inline(always)]
fn core_eq(fst: *mut SlHead, lst: *mut SlHead) -> bool {
    if id(fst, lst) {
        true
    } else if core_type(fst).unwrap() != core_type(lst).unwrap() {
        false
    } else {
        match core_type(fst).unwrap() {
            CoreType::Symbol => sym_get_id(fst) == sym_get_id(lst),
            CoreType::VecStr => string_get(fst).eq(string_get(lst)),
            _ => false,
        }
    }
}

/// Returns a unique hash value (based on the provided value's content?)
#[inline(always)]
fn core_hash(loc: *mut SlHead) -> u32 {
    match core_type(loc).expect("not a core type") {
        CoreType::Symbol => sym_get_id(loc),
        CoreType::VecStr => str_hash(string_get(loc)),
        _ => 0,
    }
}

#[inline(always)]
fn str_hash(slice: &str) -> u32 {
    let mut out: u32 = 1;
    for c in slice.bytes() {
        out = out.wrapping_add(out << 5).wrapping_add(c as u32);
    }
    out
}

/// TODO: automatically resize as needed (probably as an option) (need "fill" field in subhead)
/// TODO: clean up shadowed entries sometime
/// TODO: option to provide region if known?
pub fn hash_map_insert(
    reg: *mut memmgt::Region,
    loc: *mut SlHead,
    key: *mut SlHead,
    val: *mut SlHead,
) {
    let entry = core_cons_copy(reg, key, val);

    let size = hashvec_get_size(loc);
    let hash = core_hash(key) % size;
    let idx = 4 + 4 + (hash as usize * PTR_LEN as usize);

    let next = core_read_field(loc, idx);

    if !nil_p(next) {
        set_next_list_elt(entry, next);
    }

    core_write_field(loc, idx, entry)
}

fn alist_map_insert(
    reg: *mut memmgt::Region,
    loc: *mut SlHead,
    key: *mut SlHead,
    val: *mut SlHead,
) {
    let entry = core_cons_copy(reg, key, val);
    let next = core_read_field(loc, 0);

    if !nil_p(next) {
        set_next_list_elt(entry, next);
    }

    core_write_field(loc, 0, entry)
}

/// Looks up a key in a map, returning the key-value pair
fn hash_map_lookup(loc: *mut SlHead, key: *mut SlHead) -> *mut SlHead {
    let size = hashvec_get_size(loc);
    let hash = core_hash(key) % size;
    let entry = core_read_field(loc, 4 + 4 + (hash as usize * PTR_LEN as usize));
    alist_search(entry, key)
}

fn alist_map_lookup(loc: *mut SlHead, key: *mut SlHead) -> *mut SlHead {
    let entry = core_read_field(loc, 0);
    alist_search(entry, key)
}

fn alist_search(head: *mut SlHead, target: *mut SlHead) -> *mut SlHead {
    let mut pos = head;

    loop {
        if nil_p(pos) {
            return nil();
        }
        if core_eq(ref_get(pos), target) {
            return pos;
        }
        pos = get_next_list_elt(pos);
    }
}

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
    core_write_field(
        loc,
        (NUM_16_LEN + PTR_LEN) as usize + (idx as usize * SYMBOL_LEN as usize),
        arg,
    )
}

#[inline(always)]
pub fn proc_lambda_get_arg(reg: *mut memmgt::Region, loc: *mut SlHead, idx: u16) -> *mut SlHead {
    let out = init_symbol(reg);
    sym_set_id(out, proc_lambda_get_arg_id(loc, idx));
    out
}

#[inline(always)]
fn proc_lambda_get_arg_id(loc: *mut SlHead, idx: u16) -> u32 {
    coretypck!(loc ; ProcLambda);
    core_read_field(
        loc,
        (NUM_16_LEN + PTR_LEN) as usize + (idx as usize * SYMBOL_LEN as usize),
    )
}

#[inline(always)]
pub fn proc_lambda_set_body(loc: *mut SlHead, body: *mut SlHead) {
    coretypck!(loc ; ProcLambda);
    core_write_field(loc, NUM_16_LEN as usize, body)
}

#[inline(always)]
pub fn proc_lambda_get_body(loc: *mut SlHead) -> *mut SlHead {
    coretypck!(loc ; ProcLambda);
    core_read_field(loc, NUM_16_LEN as usize)
}

#[inline(always)]
pub fn proc_native_set_body(loc: *mut SlHead, fun: NativeFn) {
    coretypck!(loc ; ProcNative);
    let ptr = unsafe { mem::transmute::<NativeFn, u64>(fun) };
    core_write_field(loc, NUM_16_LEN as usize, ptr)
}

#[inline(always)]
pub fn proc_native_get_body(loc: *mut SlHead) -> NativeFn {
    coretypck!(loc ; ProcNative);
    let ptr = core_read_field(loc, NUM_16_LEN as usize);
    unsafe { mem::transmute::<u64, NativeFn>(ptr) }
}

#[inline(always)]
fn core_copy_val(reg: *mut memmgt::Region, src: *mut SlHead) -> *mut SlHead {
    let (siz, cfg) = (core_size(src), get_cfg_all(src));

    unsafe {
        let dst = memmgt::alloc(reg, siz, cfg);
        ptr::copy_nonoverlapping(value_ptr(src), value_ptr(dst), siz);
        dst
    }
}

#[inline(always)]
fn core_cons_copy(reg: *mut memmgt::Region, car: *mut SlHead, cdr: *mut SlHead) -> *mut SlHead {
    let new_cdr = core_copy_val(reg, cdr);
    let new_car = core_copy_val(reg, car);
    let head = init_ref(reg);

    set_next_list_elt(new_car, new_cdr);
    ref_set(head, new_car);

    head
}

// **********************************************************
// * `car` and `cdr` CANNOT be provided in the Sail internals
// * they do not fit well with the implementation details
// * use `ref_get` and `get_next_list_elt` instead of these
// **********************************************************

// /// Returns the first element of the provided list
// #[inline(always)]
// pub fn car(loc: *mut SlHead) -> *mut SlHead {
//     if nil_p(loc) {
//         nil()
//     } else if coretypp!(loc ; Ref) {
//         ref_get(loc)
//     } else {
//         loc
//     }
// }

// /// Returns the list of elements following the first element of the provided list
// #[inline(always)]
// pub fn cdr(loc: *mut SlHead) -> *mut SlHead {
//     if coretypp!(loc ; Ref) {
//         get_next_list_elt(ref_get(loc))
//         // if nil_p(cadr) {
//         //     cadr
//         // } else /*if list_elt_p(cadr)*/ {
//         //     let ptr = init_ref(reg);
//         //     ref_set(ptr, cadr);
//         //     ptr
//         // } //else {
//         //     // cadr
//         // // }
//     } else {
//         get_next_list_elt(loc)
//     }
// }

/// An environment is a list of maps: should function as a LIFO stack
/// TODO: deal somewhere with dynamic bindings, lexical bindings, argument bindings
/// TODO: give env and symtab their own predicate types
/// TODO: the core does not use maps except for env and symtab, so consolidate the code
fn env_create(reg: *mut memmgt::Region) -> *mut SlHead {
    init_hash_map(reg, 255)
}

#[inline(always)]
pub fn env_lookup(env: *mut SlHead, sym: *mut SlHead) -> *mut SlHead {
    env_lookup_by_id(env, sym_get_id(sym))
}

pub fn env_lookup_by_id(mut env: *mut SlHead, sym_id: u32) -> *mut SlHead {
    while !nil_p(env) {
        // A layer can be a hash table or an alist
        let entry = if coretypp!(env ; VecHash) {
            let size = hashvec_get_size(env);
            let hash = sym_id % size;
            core_read_field(env, 4 + 4 + (hash as usize * PTR_LEN as usize))
        } else if coretypp!(env ; Ref) {
            core_read_field(env, 0)
        } else {
            println!("{:?}", core_type(env).unwrap());
            panic!("incorrect layer in env")
        };

        let mut pos = entry;
        loop {
            if nil_p(pos) {
                break;
            }
            if sym_get_id(ref_get(pos)) == sym_id {
                return get_next_list_elt(ref_get(pos));
            }
            pos = get_next_list_elt(pos);
        }

        env = get_next_list_elt(env);
    }

    nil()
}

// TODO: use dynamic map mode
fn env_new_layer(reg: *mut memmgt::Region, min_size: u32) -> *mut SlHead {
    init_hash_map(reg, min_size * 2)
}

pub fn env_layer_ins_entry(
    reg: *mut memmgt::Region,
    layer: *mut SlHead,
    key: *mut SlHead,
    val: *mut SlHead,
) {
    if coretypp!(layer ; VecHash) {
        hash_map_insert(reg, layer, key, val)
    } else if coretypp!(layer ; Ref) {
        alist_map_insert(reg, layer, key, val)
    } else {
        panic!("incorrect layer in env")
    }
}

/// Uses Alist every time
/// TODO: many improvements / optimizations possible throughout env system
pub fn env_new_arg_layer(reg: *mut memmgt::Region) -> *mut SlHead {
    init_alist_map(reg)
}

/// TODO: this should be a vector or something else more sensible, especially for natives
pub fn env_arg_layer_get(layer: *mut SlHead, idx: u16) -> *mut SlHead {
    let mut left = idx;
    let mut pos = core_read_field(layer, 0);
    while left > 0 {
        pos = get_next_list_elt(pos);
        left -= 1;
    }
    get_next_list_elt(ref_get(pos))
}

pub fn env_arg_layer_ins(
    reg: *mut memmgt::Region,
    layer: *mut SlHead,
    key: *mut SlHead,
    val: *mut SlHead,
) {
    let entry = core_cons_copy(reg, key, val);

    let mut next = core_read_field(layer, 0);
    if nil_p(next) {
        core_write_field(layer, 0, entry);
    } else {
        while !get_next_list_elt(next).is_null() {
            next = get_next_list_elt(next);
        }

        set_next_list_elt(next, entry);
    }
}

pub fn env_push_layer(env: *mut SlHead, layer: *mut SlHead) {
    let next = ref_get(env);
    set_next_list_elt(layer, next);
    ref_set(env, layer);
}

// TODO: MEMORY LEAK
pub fn env_pop_layer(env: *mut SlHead) {
    let next = get_next_list_elt(ref_get(env));
    ref_set(env, next);
}

/// This should take the form of a bimap, a 1 to 1 association between strings and IDs
/// Two maps, one for each direction, pointing to the same set of cons cells (id . string)
/// Must keep track of id to assign (counter) and reclaim unused slots if counter reaches max
fn sym_tab_create(reg: *mut memmgt::Region) -> *mut SlHead {
    let tbl = init_stdvec(reg, 3);

    let id_to_str = init_hash_map(reg, 255);
    let str_to_id = init_hash_map(reg, 255);

    let id_count = init_symbol(reg);
    sym_set_id(id_count, 0);

    stdvec_push(tbl, id_to_str);
    stdvec_push(tbl, str_to_id);
    stdvec_push(tbl, id_count);

    tbl
}

/// Takes the symbol table and a String to insert, returning the symbol's unique ID
fn sym_tab_insert(reg: *mut memmgt::Region, tbl: *mut SlHead, sym: *mut SlHead) -> u32 {
    let next_id = stdvec_idx(tbl, 2);
    let id_num = sym_get_id(next_id);

    let entry = {
        let id = core_copy_val(reg, next_id);
        set_next_list_elt(id, sym);
        id
    };

    let id_to_str = stdvec_idx(tbl, 0);
    let str_to_id = stdvec_idx(tbl, 1);

    let id_size = hashvec_get_size(id_to_str);
    let str_size = hashvec_get_size(str_to_id);

    let id_hash = id_num % id_size;
    let str_hash = core_hash(sym) % str_size;

    let id_idx = 4 + 4 + (id_hash as usize * PTR_LEN as usize);
    let str_idx = 4 + 4 + (str_hash as usize * PTR_LEN as usize);

    let mut id_pos = core_read_field(id_to_str, id_idx);
    let mut str_pos = core_read_field(str_to_id, str_idx);

    let id_entry = {
        let ptr = init_ref(reg);
        ref_set(ptr, entry);
        ptr
    };

    let str_entry = {
        let ptr = init_ref(reg);
        ref_set(ptr, entry);
        ptr
    };

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

    sym_set_id(next_id, id_num + 1);

    id_num
}

/// Looks up normally, by car, and returns cdr
pub fn sym_tab_lookup_by_id(tbl: *mut SlHead, qry: *mut SlHead) -> *mut SlHead {
    let map = stdvec_idx(tbl, 0);
    let size = hashvec_get_size(map);
    let hash = core_hash(qry) % size as u32;

    let mut entry = core_read_field(map, 4 + 4 + (hash as usize * PTR_LEN as usize));

    loop {
        if nil_p(entry) {
            return nil();
        }

        if core_eq(ref_get(entry), qry) {
            return get_next_list_elt(ref_get(entry));
        }

        entry = get_next_list_elt(entry);
    }
}

/// Must look up by cdr, and return car
fn sym_tab_lookup_by_str(tbl: *mut SlHead, qry: *mut SlHead) -> *mut SlHead {
    let map = stdvec_idx(tbl, 1);
    let size = hashvec_get_size(map);
    let hash = core_hash(qry) % size as u32;

    let mut entry = core_read_field(map, 4 + 4 + (hash as usize * PTR_LEN as usize));

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

/// Looks up by ID number directly
fn sym_tab_lookup_id_num(tbl: *mut SlHead, id: u32) -> *mut SlHead {
    let map = stdvec_idx(tbl, 0);
    let size = hashvec_get_size(map);
    let hash = id % size as u32;

    let mut entry = core_read_field(map, 4 + 4 + (hash as usize * PTR_LEN as usize));

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

/// Returns ID for any symbol string
/// Inserts symbol into the table if not already present
pub fn sym_tab_get_id(reg: *mut memmgt::Region, tbl: *mut SlHead, sym: &str) -> u32 {
    // println!("need id for: {}", sym);

    let map = stdvec_idx(tbl, 1);
    let size = hashvec_get_size(map);
    let hash = str_hash(sym) % size;

    let mut entry = core_read_field(map, 4 + 4 + (hash as usize * PTR_LEN as usize));

    while !nil_p(entry) {
        if sym == string_get(get_next_list_elt(ref_get(entry))) {
            return sym_get_id(ref_get(entry));
        }
        entry = get_next_list_elt(entry);
    }

    let record = init_string(reg, sym.len() as u32);
    string_set(record, sym);

    sym_tab_insert(reg, tbl, record)
}

/// TODO: should regions know about their Sail environment?
/// TODO: insert all the core type symbols
pub fn prep_environment(reg: *mut memmgt::Region) -> (*mut SlHead, *mut SlHead) {
    (sym_tab_create(reg), env_create(reg))
}

#[inline(always)]
fn init_u8(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_8_LEN as usize, Cfg::B1U8 as u8) }
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
fn init_u16(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_16_LEN as usize, Cfg::B2U16 as u8) }
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
fn init_u32(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_32_LEN as usize, Cfg::B4U32 as u8) }
}

#[inline(always)]
fn u32_set(loc: *mut SlHead, val: u32) {
    coretypck!(loc ; U32);
    core_write_field(loc, 0, val)
}

#[inline(always)]
fn u32_get(loc: *mut SlHead) -> u32 {
    coretypck!(loc ; U32);
    core_read_field(loc, 0)
}

#[inline(always)]
fn init_u64(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_64_LEN as usize, Cfg::B8U64 as u8) }
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
fn init_u128(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_128_LEN as usize, Cfg::B16U128 as u8) }
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
fn init_i8(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_8_LEN as usize, Cfg::B1I8 as u8) }
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
fn init_i16(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_16_LEN as usize, Cfg::B2I16 as u8) }
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
fn init_i32(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_32_LEN as usize, Cfg::B4I32 as u8) }
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
pub fn init_i64(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_64_LEN as usize, Cfg::B8I64 as u8) }
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
fn init_i128(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_128_LEN as usize, Cfg::B16I128 as u8) }
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
fn init_f32(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_32_LEN as usize, Cfg::B4F32 as u8) }
}

#[inline(always)]
fn f32_set(loc: *mut SlHead, val: f32) {
    coretypck!(loc ; F32);
    core_write_field(loc, 0, val)
}

#[inline(always)]
fn f32_get(loc: *mut SlHead) -> f32 {
    coretypck!(loc ; F32);
    core_read_field(loc, 0)
}

#[inline(always)]
pub fn init_f64(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, NUM_64_LEN as usize, Cfg::B8F64 as u8) }
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
pub fn init_bool(reg: *mut memmgt::Region) -> *mut SlHead {
    unsafe { memmgt::alloc(reg, 0, Cfg::B0BoolF as u8) }
}

/// Set a boolean value
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

/// Get a boolean value
#[inline(always)]
pub fn bool_get(loc: *mut SlHead) -> bool {
    coretypck!(loc ; Bool);
    unsafe { ptr::read_unaligned(loc as *mut u8) & 0b00000100 != 0 }
}

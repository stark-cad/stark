//! The Structured Augmentation Interchange Language
//!
//! A custom Lisp dialect for writing STARK

use std::fmt;
use std::mem;
use std::ptr;

use rug;

mod memmgt;
mod parser;

pub enum SailErr {
    Error,
}

impl fmt::Display for SailErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error")
    }
}

impl fmt::Debug for SailErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error")
    }
}

/// Could replace "*mut SlHead" in future
/// Is there any reason to do this?
#[repr(transparent)]
struct SlCursor {
    loc: Option<ptr::NonNull<SlHead>>,
}

const HEAD_LEN: u8 = 2;
/// Header for all Sail values in memory (optimizations later)
/// **Handle ONLY using methods such as sail::alloc and etc**
/// Should only store information that every referenced Sail value needs
/// TODO: Think about references, memory management, and associative table support
#[repr(C)]
pub struct SlHead {
    conf: u8,
    rc: u8,
}

/// ALL Sail values that may be independently referenced, begin with bytes of this format
/// type: 4 bits - list: 1 bit - config: 3 bits - rc: 8 bits
/// The first eight bits determine the subsequent memory layout
const _MIN_HEAD: u16 = 0b1111011100000000;

const PTR_LEN: u8 = 8;
/// Pointer to the next element of a linked list;
/// appears immediately after the SlHead.
struct SlListPtr {
    ptr: *mut SlHead,
}

// TODO: MINIMIZE the use of *unsafe* functions

/// Returns the type of a valid Sail value
unsafe fn get_type(loc: *mut SlHead) -> SlType {
    let typ = ptr::read_unaligned(loc as *mut u8) >> 4;
    mem::transmute::<u8, SlType>(typ)
}

/// Returns the size of a valid Sail value
unsafe fn get_size(loc: *mut SlHead) -> usize {
    match get_type(loc) {
        SlType::List => PTR_LEN as usize,
        SlType::Vec => VEC_SH_LEN as usize + (vec_get_cap(loc) as usize * PTR_LEN as usize),
        SlType::Map => MAP_SH_LEN as usize + (map_get_size(loc) as usize * PTR_LEN as usize),
        SlType::Proc => match proc_mode(loc) {
            SlProcMode::Lambda => {
                PROC_SH_LEN as usize
                    + PTR_LEN as usize
                    + ((ptr::read_unaligned(value_ptr(loc) as *mut u16) as usize)
                        * SYMBOL_ID_LEN as usize)
            }
            SlProcMode::Native => PROC_SH_LEN as usize + PTR_LEN as usize,
        },
        SlType::Symbol => match sym_mode(loc) {
            SlSymbolMode::ById => SYMBOL_ID_LEN as usize,
            SlSymbolMode::ByStr => SYMBOL_SH_LEN as usize + sym_get_len(loc) as usize,
        },
        SlType::Keyword => {
            KEYWORD_SH_LEN as usize
                + ptr::read_unaligned(value_ptr(loc).offset(2) as *mut u16) as usize
        }
        SlType::String => {
            STRING_SH_LEN as usize
                + ptr::read_unaligned(value_ptr(loc).offset(2) as *mut u16) as usize
        }
        SlType::FixInt => FIX_NUM_LEN as usize,
        SlType::FixFloat => FIX_NUM_LEN as usize,
        SlType::MpInt => MP_INT_LEN as usize,
        SlType::MpFloat => MP_FLOAT_LEN as usize,
        SlType::Rational => RATIONAL_LEN as usize,
        SlType::Complex => COMPLEX_LEN as usize,
        SlType::Bool => 0,
        SlType::Err => 0,
        SlType::Other => 0,
    }
}

unsafe fn get_cfg_bits(loc: *mut SlHead) -> u8 {
    ptr::read_unaligned(loc as *const u8) & 0b00000111
}

unsafe fn set_cfg_bits(loc: *mut SlHead, cfg: u8) {
    let head = ptr::read_unaligned(loc as *const u8) & 0b11111000;
    ptr::write_unaligned(loc as *mut u8, head | cfg)
}

/// Checks a valid Sail value to determine whether it is a list element
unsafe fn list_elt_p(loc: *mut SlHead) -> bool {
    (ptr::read_unaligned(loc as *const u8) & 0b00001000) != 0
}

unsafe fn nil_p(loc: *mut SlHead) -> bool {
    loc == ptr::null_mut::<SlHead>()
}

/// From a valid Sail value, returns a pointer to the start of the value proper
/// (After the header and list element pointer, if it exists)
/// TODO: explicitly inline functions like this if necessary
unsafe fn value_ptr(loc: *mut SlHead) -> *mut u8 {
    (loc as *mut u8).offset(if list_elt_p(loc) {
        HEAD_LEN + PTR_LEN
    } else {
        HEAD_LEN
    } as isize)
}

unsafe fn init_list(list_elt: bool) -> *mut SlHead {
    let ptr = memmgt::alloc(PTR_LEN as usize, list_elt, SlType::List);

    ptr::write_unaligned(
        value_ptr(ptr) as *mut *mut SlHead,
        ptr::null_mut::<SlHead>(),
    );

    ptr
}

// For now, vectors will only store references to their contained values
unsafe fn init_vec(list_elt: bool, cap: u16) -> *mut SlHead {
    let ptr = memmgt::alloc(
        VEC_SH_LEN as usize + (PTR_LEN as usize * cap as usize),
        list_elt,
        SlType::Vec,
    );

    ptr::write_unaligned(value_ptr(ptr) as *mut SlVecSH, SlVecSH { len: 0, cap });

    ptr
}

// Maps will probably all use the same mode for now (list out of each hash value entry)
unsafe fn init_map(list_elt: bool, mode: SlMapMode, size: u16) -> *mut SlHead {
    let ptr;
    match mode {
        SlMapMode::Assoc => {
            ptr = memmgt::alloc(
                MAP_SH_LEN as usize + (PTR_LEN as usize * size as usize),
                list_elt,
                SlType::Map,
            );
            ptr::write_unaligned(value_ptr(ptr) as *mut SlMapSH, SlMapSH { size });
            for i in 0..size {
                ptr::write_unaligned(
                    value_ptr(ptr)
                        .offset(MAP_SH_LEN as isize)
                        .offset(i as isize * PTR_LEN as isize)
                        as *mut *mut SlHead,
                    ptr::null_mut(),
                );
            }
        }
        SlMapMode::Alist => {
            ptr = memmgt::alloc(PTR_LEN as usize, list_elt, SlType::Map);
            set_cfg_bits(ptr, mode as u8);
            ptr::write_unaligned(value_ptr(ptr) as *mut *mut SlHead, ptr::null_mut());
        }
    }

    ptr
}

unsafe fn init_proc(list_elt: bool, mode: SlProcMode, argct: u16) -> *mut SlHead {
    let ptr;
    match mode {
        SlProcMode::Lambda => {
            ptr = memmgt::alloc(
                (PROC_SH_LEN + PTR_LEN) as usize + (SYMBOL_ID_LEN as usize * argct as usize),
                list_elt,
                SlType::Proc,
            );
        }
        SlProcMode::Native => {
            ptr = memmgt::alloc((PROC_SH_LEN + PTR_LEN) as usize, list_elt, SlType::Proc);
            set_cfg_bits(ptr, mode as u8);
        }
    }

    let start = value_ptr(ptr);
    ptr::write_unaligned(start as *mut SlProcSH, SlProcSH { argct });
    ptr::write_unaligned(
        start.offset(PROC_SH_LEN as isize) as *mut *mut SlHead,
        ptr::null_mut::<SlHead>(),
    );

    ptr
}

unsafe fn init_symbol(list_elt: bool, mode: SlSymbolMode, len: u16) -> *mut SlHead {
    let ptr;

    match mode {
        SlSymbolMode::ById => {
            ptr = memmgt::alloc(SYMBOL_ID_LEN as usize, list_elt, SlType::Symbol);
        }
        SlSymbolMode::ByStr => {
            ptr = memmgt::alloc(
                SYMBOL_SH_LEN as usize + len as usize,
                list_elt,
                SlType::Symbol,
            );
            set_cfg_bits(ptr, mode as u8);
            ptr::write_unaligned(value_ptr(ptr) as *mut SlSymbolSH, SlSymbolSH { len });
        }
    }

    ptr
}

unsafe fn init_keyword(list_elt: bool, cap: u16) -> *mut SlHead {
    let ptr = memmgt::alloc(
        KEYWORD_SH_LEN as usize + cap as usize,
        list_elt,
        SlType::Keyword,
    );

    ptr::write_unaligned(value_ptr(ptr) as *mut SlKeyword, SlKeyword { len: 0, cap });

    ptr
}

unsafe fn init_string(list_elt: bool, cap: u16) -> *mut SlHead {
    let ptr = memmgt::alloc(
        STRING_SH_LEN as usize + cap as usize,
        list_elt,
        SlType::String,
    );

    ptr::write_unaligned(value_ptr(ptr) as *mut SlString, SlString { len: 0, cap });

    ptr
}

unsafe fn init_fixint(list_elt: bool) -> *mut SlHead {
    let ptr = memmgt::alloc(FIX_NUM_LEN as usize, list_elt, SlType::FixInt);

    ptr
}

unsafe fn init_fixfloat(list_elt: bool) -> *mut SlHead {
    let ptr = memmgt::alloc(FIX_NUM_LEN as usize, list_elt, SlType::FixFloat);

    ptr
}

unsafe fn init_mpint(list_elt: bool) -> *mut SlHead {
    let ptr = memmgt::alloc(MP_INT_LEN as usize, list_elt, SlType::MpInt);

    ptr::write_unaligned(value_ptr(ptr) as *mut rug::Integer, rug::Integer::new());

    ptr
}

unsafe fn init_mpfloat(list_elt: bool, prec: u32) -> *mut SlHead {
    let ptr = memmgt::alloc(MP_FLOAT_LEN as usize, list_elt, SlType::MpFloat);

    ptr::write_unaligned(value_ptr(ptr) as *mut rug::Float, rug::Float::new(prec));

    ptr
}

unsafe fn init_rational(list_elt: bool) -> *mut SlHead {
    let ptr = memmgt::alloc(RATIONAL_LEN as usize, list_elt, SlType::Rational);

    ptr::write_unaligned(value_ptr(ptr) as *mut rug::Rational, rug::Rational::new());

    ptr
}

unsafe fn init_complex(list_elt: bool, prec: u32) -> *mut SlHead {
    let ptr = memmgt::alloc(COMPLEX_LEN as usize, list_elt, SlType::Complex);

    ptr::write_unaligned(value_ptr(ptr) as *mut rug::Complex, rug::Complex::new(prec));

    ptr
}

unsafe fn init_bool(list_elt: bool) -> *mut SlHead {
    let ptr = memmgt::alloc(0, list_elt, SlType::Bool);

    ptr
}

/// Still causing memory leaks...
unsafe fn set_list_elt(loc: *mut SlHead, next: *mut SlHead) {
    ptr::write_unaligned(
        (loc as *mut u8).offset(HEAD_LEN as isize) as *mut *mut SlHead,
        next,
    )
}

/// TODO: check for types and handle errors?
/// Gets the pointer to the next element from a list element
unsafe fn next_list_elt(loc: *mut SlHead) -> *mut SlHead {
    ptr::read_unaligned((loc as *mut u8).offset(HEAD_LEN as isize) as *mut *mut SlHead)
}

/// Returns true if both arguments are the same Sail value
/// TODO: symbol handling etc
unsafe fn id(fst: *mut SlHead, lst: *mut SlHead) -> bool {
    if fst == lst {
        true
    } else {
        false
    }
}

/// Returns true if both arguments match in structure
/// TODO: make eq and hash actually function for all types
unsafe fn eq(fst: *mut SlHead, lst: *mut SlHead) -> bool {
    if id(fst, lst) {
        true
    } else if get_type(fst) != get_type(lst) {
        false
    } else {
        match get_type(fst) {
            SlType::Symbol => {
                if sym_mode(fst) != sym_mode(lst) {
                    false
                } else if sym_mode(fst) == SlSymbolMode::ById {
                    sym_get_id(fst) == sym_get_id(lst)
                } else {
                    sym_get_str(fst).eq(sym_get_str(lst))
                }
            }
            _ => false,
        }
    }
}

/// Returns a unique hash value (based on the provided value's content?)
unsafe fn hash(loc: *mut SlHead) -> u32 {
    match get_type(loc) {
        SlType::Symbol => {
            if sym_mode(loc) == SlSymbolMode::ById {
                sym_get_id(loc)
            } else {
                let slice = sym_get_str(loc);

                let mut out: u32 = 1;
                for c in slice.bytes() {
                    out = out.wrapping_add(out << 5).wrapping_add(c as u32);
                }

                out
            }
        }
        _ => 0,
    }
}

// TODO: should functions like this check for correct type?
/// TODO: this implementation will **easily** cause memory leaks
unsafe fn list_set(loc: *mut SlHead, next: *mut SlHead) {
    ptr::write_unaligned(value_ptr(loc) as *mut *mut SlHead, next)
}

/// Gets the pointer contained within a list head
unsafe fn list_get(loc: *mut SlHead) -> *mut SlHead {
    ptr::read_unaligned(value_ptr(loc) as *mut *mut SlHead)
}

unsafe fn list_empty_p(loc: *mut SlHead) -> bool {
    nil_p(list_get(loc))
}

unsafe fn vec_set_len(loc: *mut SlHead, len: u16) {
    assert_eq!(SlType::Vec, get_type(loc));
    ptr::write_unaligned(value_ptr(loc) as *mut u16, len)
}

unsafe fn vec_get_len(loc: *mut SlHead) -> u16 {
    assert_eq!(SlType::Vec, get_type(loc));
    ptr::read_unaligned(value_ptr(loc) as *mut u16)
}

unsafe fn vec_get_cap(loc: *mut SlHead) -> u16 {
    assert_eq!(SlType::Vec, get_type(loc));
    ptr::read_unaligned(value_ptr(loc).offset(2) as *mut u16)
}

unsafe fn vec_idx(loc: *mut SlHead, idx: u16) -> *mut SlHead {
    assert_eq!(SlType::Vec, get_type(loc));

    ptr::read_unaligned(
        value_ptr(loc)
            .offset(VEC_SH_LEN as isize)
            .offset(idx as isize * PTR_LEN as isize) as *mut *mut SlHead,
    )
}

unsafe fn vec_push(loc: *mut SlHead, item: *mut SlHead) {
    assert_eq!(SlType::Vec, get_type(loc));

    let (len, cap) = (vec_get_len(loc), vec_get_cap(loc));

    if len < cap {
        ptr::write_unaligned(
            value_ptr(loc).offset(VEC_SH_LEN as isize + (len as isize * PTR_LEN as isize))
                as *mut *mut SlHead,
            item,
        );
        vec_set_len(loc, len + 1);
    } else {
        panic!("not enough space in vec");
    }
}

unsafe fn map_mode(loc: *mut SlHead) -> SlMapMode {
    mem::transmute::<u8, SlMapMode>(get_cfg_bits(loc))
}

unsafe fn map_get_size(loc: *mut SlHead) -> u16 {
    assert_eq!(SlType::Map, get_type(loc));

    ptr::read_unaligned(value_ptr(loc) as *mut u16)
}

/// TODO: automatically resize as needed (probably as an option) (need "fill" field in subhead)
/// TODO: clean up shadowed entries sometime
unsafe fn map_insert(loc: *mut SlHead, key: *mut SlHead, val: *mut SlHead) {
    assert_eq!(SlType::Map, get_type(loc));

    let entry = cons_copy(true, key, val);
    let pos;

    match map_mode(loc) {
        SlMapMode::Assoc => {
            let size = map_get_size(loc);
            let hash = hash(key) % size as u32;
            let idx = MAP_SH_LEN as isize + (hash * PTR_LEN as u32) as isize;
            pos = value_ptr(loc).offset(idx) as *mut *mut SlHead;
        }
        SlMapMode::Alist => {
            pos = value_ptr(loc) as *mut *mut SlHead;
        }
    }

    let next = ptr::read_unaligned(pos);

    if !next.is_null() {
        set_list_elt(entry, next);
    }

    ptr::write_unaligned(pos, entry)
}

/// Looks up a key in a map, returning the key-value pair
unsafe fn map_lookup(loc: *mut SlHead, key: *mut SlHead) -> *mut SlHead {
    assert_eq!(SlType::Map, get_type(loc));

    let mut entry;

    match map_mode(loc) {
        SlMapMode::Assoc => {
            let size = map_get_size(loc);
            let hash = hash(key) % size as u32;

            entry = ptr::read_unaligned(
                value_ptr(loc).offset(MAP_SH_LEN as isize + (hash * PTR_LEN as u32) as isize)
                    as *mut *mut SlHead,
            );
        }
        SlMapMode::Alist => {
            entry = ptr::read_unaligned(value_ptr(loc) as *mut *mut SlHead);
        }
    }

    loop {
        if entry == ptr::null_mut() {
            let out = init_bool(false);
            bool_set(out, false);
            return out;
        }

        if eq(list_get(entry), key) {
            return entry;
        }

        entry = next_list_elt(entry);
    }
}

unsafe fn proc_mode(loc: *mut SlHead) -> SlProcMode {
    mem::transmute::<u8, SlProcMode>(get_cfg_bits(loc))
}

unsafe fn proc_get_argct(loc: *mut SlHead) -> u16 {
    ptr::read_unaligned(value_ptr(loc) as *mut u16)
}

unsafe fn proc_set_arg(loc: *mut SlHead, ind: u16, arg: u32) {
    assert_eq!(SlProcMode::Lambda, proc_mode(loc));

    ptr::write_unaligned(
        value_ptr(loc)
            .offset((PROC_SH_LEN + PTR_LEN) as isize)
            .offset(ind as isize * SYMBOL_ID_LEN as isize) as *mut u32,
        arg,
    );
}

unsafe fn proc_get_arg(loc: *mut SlHead, ind: u16) -> *mut SlHead {
    assert_eq!(SlProcMode::Lambda, proc_mode(loc));

    let id = ptr::read_unaligned(
        value_ptr(loc)
            .offset((PROC_SH_LEN + PTR_LEN) as isize)
            .offset(ind as isize * SYMBOL_ID_LEN as isize) as *mut u32,
    );

    let sym = init_symbol(false, SlSymbolMode::ById, 0);

    sym_set_id(sym, id);

    sym
}

unsafe fn proc_lambda_set(loc: *mut SlHead, body: *mut SlHead) {
    ptr::write_unaligned(
        value_ptr(loc).offset(PROC_SH_LEN as isize) as *mut *mut SlHead,
        body,
    )
}

unsafe fn proc_lambda_body(loc: *mut SlHead) -> *mut SlHead {
    // let body_car =
    ptr::read_unaligned(value_ptr(loc).offset(PROC_SH_LEN as isize) as *mut *mut SlHead)
    // let ptr = init_list(false);
    // list_set(ptr, body_car);
    // ptr
}

unsafe fn proc_native_set(
    loc: *mut SlHead,
    fun: unsafe fn(*mut SlHead, *mut SlHead) -> *mut SlHead,
) {
    let ptr = mem::transmute::<unsafe fn(*mut SlHead, *mut SlHead) -> *mut SlHead, u64>(fun);
    ptr::write_unaligned(value_ptr(loc).offset(PROC_SH_LEN as isize) as *mut u64, ptr);
}

unsafe fn proc_native_body(loc: *mut SlHead) -> unsafe fn(*mut SlHead, *mut SlHead) -> *mut SlHead {
    let ptr = ptr::read_unaligned(value_ptr(loc).offset(PROC_SH_LEN as isize) as *mut u64);
    mem::transmute::<u64, unsafe fn(*mut SlHead, *mut SlHead) -> *mut SlHead>(ptr)
}

unsafe fn sym_mode(loc: *mut SlHead) -> SlSymbolMode {
    mem::transmute::<u8, SlSymbolMode>(get_cfg_bits(loc))
}

unsafe fn sym_set_id(loc: *mut SlHead, id: u32) {
    ptr::write_unaligned(value_ptr(loc) as *mut u32, id)
}

unsafe fn sym_get_id(loc: *mut SlHead) -> u32 {
    ptr::read_unaligned(value_ptr(loc) as *mut u32)
}

unsafe fn sym_get_len(loc: *mut SlHead) -> u16 {
    ptr::read_unaligned(value_ptr(loc) as *mut u16)
}

unsafe fn sym_set_str(loc: *mut SlHead, val: &[u8]) {
    let len = sym_get_len(loc);
    let here = std::slice::from_raw_parts_mut(value_ptr(loc).offset(2) as *mut u8, len as usize);

    here.copy_from_slice(val)
}

unsafe fn sym_get_str(loc: *mut SlHead) -> &'static str {
    let len = sym_get_len(loc);

    std::str::from_utf8_unchecked(std::slice::from_raw_parts(
        value_ptr(loc).offset(2) as *mut u8,
        len as usize,
    ))
}

unsafe fn string_get_cap(loc: *mut SlHead) -> u16 {
    ptr::read_unaligned(value_ptr(loc).offset(2) as *mut u16)
}

unsafe fn string_set(loc: *mut SlHead, val: &str) {
    let cap = string_get_cap(loc);

    let count = 0;
    for c in val.bytes() {
        if count <= cap {
            ptr::write_unaligned(
                value_ptr(loc).offset(STRING_SH_LEN as isize + count as isize),
                c,
            )
        }
    }
}

unsafe fn fixint_set(loc: *mut SlHead, val: i64) {
    ptr::write_unaligned(value_ptr(loc) as *mut i64, val)
}

unsafe fn fixint_get(loc: *mut SlHead) -> i64 {
    ptr::read_unaligned(value_ptr(loc) as *mut i64)
}

unsafe fn fixfloat_set(loc: *mut SlHead, val: f64) {
    ptr::write_unaligned(value_ptr(loc) as *mut f64, val)
}

unsafe fn fixfloat_get(loc: *mut SlHead) -> f64 {
    ptr::read_unaligned(value_ptr(loc) as *mut f64)
}

/// Set a boolean value
unsafe fn bool_set(loc: *mut SlHead, val: bool) {
    if val {
        ptr::write_unaligned(
            loc as *mut u8,
            ptr::read_unaligned(loc as *mut u8) | 0b00000001,
        )
    } else {
        ptr::write_unaligned(
            loc as *mut u8,
            ptr::read_unaligned(loc as *mut u8) & 0b11111110,
        )
    }
}

/// Get a boolean value
unsafe fn bool_get(loc: *mut SlHead) -> bool {
    ptr::read_unaligned(loc as *mut u8) & 0b00000001 != 0
}

unsafe fn copy_val(src: *mut SlHead, list_elt: bool) -> *mut SlHead {
    let (typ, siz, cfg) = (get_type(src), get_size(src), get_cfg_bits(src));

    let dst = memmgt::alloc(siz, list_elt, typ);
    set_cfg_bits(dst, cfg);
    ptr::copy_nonoverlapping(value_ptr(src), value_ptr(dst), siz);

    dst
}

unsafe fn cons_copy(list_elt: bool, car: *mut SlHead, cdr: *mut SlHead) -> *mut SlHead {
    let new_cdr = copy_val(cdr, false);
    let new_car = copy_val(car, true);
    let head = init_list(list_elt);

    set_list_elt(new_car, new_cdr);
    list_set(head, new_car);

    head
}

/// Returns the first element of the provided list
unsafe fn car(loc: *mut SlHead) -> *mut SlHead {
    // Code for car that just accesses the value
    list_get(loc)

    // Code for car that returns a newly created copy
    // let car = list_get(loc);
    // let (typ, siz) = (get_type(car), get_size(car));

    // let ptr = alloc(siz, false, typ);
    // ptr::copy_nonoverlapping(value_ptr(car), value_ptr(ptr), siz);

    // ptr
}

/// Returns the list of elements following the first element of the provided list
unsafe fn cdr(loc: *mut SlHead) -> *mut SlHead {
    let cadr = next_list_elt(list_get(loc));

    // if the cdr is nil, just return it
    // if the cdr is still a list, return it as such
    // if the cdr is the final item in a malformed list, return it
    if nil_p(cadr) {
        cadr
    } else if list_elt_p(cadr) {
        let ptr = init_list(false);
        list_set(ptr, cadr);
        ptr
    } else {
        cadr
    }
}

// a malformed list (single cons cell included) can simply be a list that ends in a value that is not a list element!
// two element list: List -> value (list elt) -> value (list elt) -> [null]
// cons cell: List -> value (list elt) -> value (NOT list elt)

// What can follow a list head value:
// - A sequence of list elements ending in a list element (list)
// - A sequence of list elements ending in a non element (malformed list or cons cell)
// - A single non element (???)

// TODO: How to keep track of which memory has been allocated
// TODO: Consider whether bool is necessary
// TODO: Think about storing Keyword: use more interning?
// TODO: Collapse Lambda and Native into Proc and add Err
#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum SlType {
    List = 0,
    Vec = 1,
    Map = 2,
    Proc = 3,
    Symbol = 4,
    Keyword = 5,
    String = 6,
    FixInt = 7,
    FixFloat = 8,
    MpInt = 9,
    MpFloat = 10,
    Rational = 11,
    Complex = 12,
    Bool = 13,
    Err = 14,
    Other = 15,
}

struct SlList {
    ptr: *mut SlHead,
}

// TODO: Vecs and Maps should have various modes with various performance characteristics in the future
const VEC_SH_LEN: u8 = 4;
#[repr(C)]
struct SlVecSH {
    len: u16,
    cap: u16,
}

const MAP_SH_LEN: u8 = 2;
#[repr(C)]
struct SlMapSH {
    size: u16,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
enum SlMapMode {
    Assoc = 0,
    Alist = 1,
}

const PROC_SH_LEN: u8 = 2;
struct SlProcSH {
    argct: u16,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
enum SlProcMode {
    Lambda = 0,
    Native = 1,
}

const SYMBOL_SH_LEN: u8 = 2;
struct SlSymbolSH {
    len: u16,
}

const SYMBOL_ID_LEN: u8 = 4;
struct SlSymbolId {
    id: u32,
}

#[derive(PartialEq, Eq)]
#[repr(u8)]
enum SlSymbolMode {
    ById = 0,
    ByStr = 1,
}

const KEYWORD_SH_LEN: u8 = 4;
#[repr(C)]
struct SlKeyword {
    len: u16,
    cap: u16,
}

const STRING_SH_LEN: u8 = 4;
#[repr(C)]
struct SlString {
    len: u16,
    cap: u16,
}

/// Currently just an i64 or an f64
/// TODO: Use modes to permit u16 - u128 and i16 - i128 for FixInt
const FIX_NUM_LEN: u8 = 8;

#[derive(PartialEq, Eq)]
#[repr(u8)]
enum SlFixIntMode {
    I16,
    I32,
    I64,
    I128,
    U16,
    U32,
    U64,
    U128,
}

const MP_INT_LEN: u8 = 16;
struct SlMpInt {
    val: rug::Integer,
}

const MP_FLOAT_LEN: u8 = 32;
struct SlMpFloat {
    val: rug::Float,
}

const RATIONAL_LEN: u8 = 32;
struct SlRational {
    val: rug::Rational,
}

const COMPLEX_LEN: u8 = 64;
struct SlComplex {
    val: rug::Complex,
}

//TODO: Not just complexes, but also quaternions

#[repr(u8)]
enum SlBoolMode {
    False = 0,
    True = 1,
}

// TODO: not sure if this should exist, but may be useful
struct SlRef {
    ptr: *mut SlHead,
}

/// An environment is a list of maps: should function as a FIFO stack
/// TODO: deal somewhere with dynamic bindings, lexical bindings, argument bindings
unsafe fn env_create() -> *mut SlHead {
    let head = init_list(false);
    let base = init_map(true, SlMapMode::Assoc, 255);

    list_set(head, base);

    head
}

unsafe fn env_lookup(env: *mut SlHead, sym: *mut SlHead) -> *mut SlHead {
    assert_eq!(SlType::List, get_type(env));
    assert_eq!(SlType::Symbol, get_type(sym));

    let result = map_lookup(car(env), sym);
    if get_type(result) == SlType::List {
        return cdr(result);
    }

    let next = cdr(env);
    if nil_p(next) {
        let out = init_bool(false);
        bool_set(out, false);
        out
    } else {
        env_lookup(next, sym)
    }
}

// TODO: use dynamic map mode
unsafe fn env_new_layer(min_size: u16) -> *mut SlHead {
    init_map(true, SlMapMode::Alist, min_size * 2)
}

unsafe fn env_layer_ins_entry(layer: *mut SlHead, key: *mut SlHead, val: *mut SlHead) {
    map_insert(layer, key, val)
}

/// Uses Alist every time
/// TODO: many improvements / optimizations possible throughout env system
unsafe fn env_new_arg_layer() -> *mut SlHead {
    init_map(true, SlMapMode::Alist, 0)
}

unsafe fn env_arg_layer_get(layer: *mut SlHead, idx: u16) -> *mut SlHead {
    assert_eq!(SlType::Map, get_type(layer));
    assert_eq!(SlMapMode::Alist, map_mode(layer));

    let mut left = idx;
    let mut pos = ptr::read_unaligned(value_ptr(layer) as *mut *mut SlHead);
    while left > 0 {
        pos = next_list_elt(pos);
        left -= 1;
    }
    cdr(pos)
}

unsafe fn env_arg_layer_ins(layer: *mut SlHead, key: *mut SlHead, val: *mut SlHead) {
    assert_eq!(SlType::Map, get_type(layer));
    assert_eq!(SlMapMode::Alist, map_mode(layer));

    let entry = cons_copy(true, key, val);
    let pos = value_ptr(layer) as *mut *mut SlHead;

    let mut next = ptr::read_unaligned(pos);
    if next.is_null() {
        ptr::write_unaligned(pos, entry);
    } else {
        while !next_list_elt(next).is_null() {
            next = next_list_elt(next);
        }

        set_list_elt(next, entry);
    }
}

unsafe fn env_push_layer(env: *mut SlHead, layer: *mut SlHead) {
    let next = list_get(env);
    set_list_elt(layer, next);
    list_set(env, layer);
}

// TODO: MEMORY LEAK
unsafe fn env_pop_layer(env: *mut SlHead) {
    let next = next_list_elt(list_get(env));
    list_set(env, next);
}

/// This should take the form of a bimap, a 1 to 1 association between strings and IDs
/// Two maps, one for each direction, pointing to the same set of cons cells (id . string)
/// Must keep track of id to assign (counter) and reclaim unused slots if counter reaches max
unsafe fn sym_tab_create() -> *mut SlHead {
    let tbl = init_vec(false, 3);

    let id_to_str = init_map(false, SlMapMode::Assoc, 255);
    let str_to_id = init_map(false, SlMapMode::Assoc, 255);

    let id_count = init_symbol(false, SlSymbolMode::ById, 0);
    sym_set_id(id_count, 0);

    vec_push(tbl, id_to_str);
    vec_push(tbl, str_to_id);
    vec_push(tbl, id_count);

    tbl
}

/// Takes the symbol table and a Symbol ByStr to insert, returning the symbol's unique ID
unsafe fn sym_tab_insert(tbl: *mut SlHead, sym: *mut SlHead) -> u32 {
    assert_eq!(SlType::Vec, get_type(tbl));
    assert_eq!(SlType::Symbol, get_type(sym));

    let next_id = vec_idx(tbl, 2);
    let id_num = sym_get_id(next_id);

    let entry = {
        let id = copy_val(next_id, true);
        set_list_elt(id, sym);
        id
    };

    let id_to_str = vec_idx(tbl, 0);
    let str_to_id = vec_idx(tbl, 1);

    let id_size = map_get_size(id_to_str);
    let str_size = map_get_size(str_to_id);

    let id_hash = id_num % id_size as u32;
    let str_hash = hash(sym) % str_size as u32;

    let id_idx = MAP_SH_LEN as isize + (id_hash * PTR_LEN as u32) as isize;
    let str_idx = MAP_SH_LEN as isize + (str_hash * PTR_LEN as u32) as isize;

    let mut id_pos = ptr::read_unaligned(value_ptr(id_to_str).offset(id_idx) as *mut *mut SlHead);
    let mut str_pos = ptr::read_unaligned(value_ptr(str_to_id).offset(str_idx) as *mut *mut SlHead);

    let id_entry = {
        let ptr = init_list(true);
        list_set(ptr, entry);
        ptr
    };

    let str_entry = {
        let ptr = init_list(true);
        list_set(ptr, entry);
        ptr
    };

    if id_pos == ptr::null_mut() {
        ptr::write_unaligned(
            value_ptr(id_to_str).offset(id_idx) as *mut *mut SlHead,
            id_entry,
        )
    } else {
        while next_list_elt(id_pos) != ptr::null_mut() {
            id_pos = next_list_elt(id_pos);
        }

        set_list_elt(id_pos, id_entry)
    }

    if str_pos == ptr::null_mut() {
        ptr::write_unaligned(
            value_ptr(str_to_id).offset(str_idx) as *mut *mut SlHead,
            str_entry,
        )
    } else {
        while next_list_elt(str_pos) != ptr::null_mut() {
            str_pos = next_list_elt(str_pos);
        }

        set_list_elt(str_pos, str_entry)
    }

    sym_set_id(next_id, id_num + 1);

    id_num
}

/// Looks up normally, by car, and returns cdr
unsafe fn sym_tab_lookup_by_id(tbl: *mut SlHead, qry: *mut SlHead) -> *mut SlHead {
    let map = vec_idx(tbl, 0);

    let size = map_get_size(map);
    let hash = hash(qry) % size as u32;

    let mut entry = ptr::read_unaligned(
        value_ptr(map).offset(MAP_SH_LEN as isize + (hash * PTR_LEN as u32) as isize)
            as *mut *mut SlHead,
    );

    loop {
        if entry == ptr::null_mut() {
            let out = init_bool(false);
            bool_set(out, false);
            return out;
        }

        if eq(list_get(entry), qry) {
            return next_list_elt(list_get(entry));
        }

        entry = next_list_elt(entry);
    }
}

/// Must look up by cdr, and return car
unsafe fn sym_tab_lookup_by_str(tbl: *mut SlHead, qry: *mut SlHead) -> *mut SlHead {
    let map = vec_idx(tbl, 1);

    let size = map_get_size(map);
    let hash = hash(qry) % size as u32;

    let mut entry = ptr::read_unaligned(
        value_ptr(map).offset(MAP_SH_LEN as isize + (hash * PTR_LEN as u32) as isize)
            as *mut *mut SlHead,
    );

    loop {
        if entry == ptr::null_mut() {
            let out = init_bool(false);
            bool_set(out, false);
            return out;
        }

        if eq(next_list_elt(list_get(entry)), qry) {
            return list_get(entry);
        }

        entry = next_list_elt(entry);
    }
}

/// Bundles together a value and associated symbol table for display
struct SlContextVal {
    tbl: *mut SlHead,
    val: *mut SlHead,
}

fn context(tbl: *mut SlHead, val: *mut SlHead) -> SlContextVal {
    SlContextVal { tbl, val }
}

// TODO: Figure out how to manage / dispose of memory
impl fmt::Display for SlContextVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let table = self.tbl;
        let value = self.val;

        // TODO: Implement special cons cell display
        unsafe {
            match get_type(value) {
                SlType::List => {
                    write!(f, "(").unwrap();
                    let mut elt = list_get(value);
                    while elt != ptr::null_mut() {
                        write!(f, "{}", context(table, elt).to_string()).unwrap();
                        elt = next_list_elt(elt);
                        if elt != ptr::null_mut() {
                            write!(f, " ").unwrap();
                        }
                    }
                    write!(f, ")")
                }
                SlType::Vec => {
                    write!(f, "[").unwrap();
                    let len = vec_get_len(value);
                    for idx in 0..len {
                        write!(f, "{}", context(table, vec_idx(value, idx)).to_string()).unwrap();

                        if idx < len - 1 {
                            write!(f, " ").unwrap();
                        }
                    }
                    write!(f, "]")
                }
                // TODO: function to access all map pairs somehow
                SlType::Map => write!(f, "map"),
                SlType::Proc => write!(f, "<procedure>"),
                SlType::Symbol => write!(f, "{}", sym_get_str(sym_tab_lookup_by_id(table, value))),
                SlType::Keyword => write!(f, "keyword"),
                SlType::String => write!(f, "string"),
                SlType::FixInt => write!(f, "{}", fixint_get(value)),
                SlType::FixFloat => write!(f, "{}", fixfloat_get(value)),
                SlType::MpInt => write!(f, "mpint"),
                SlType::MpFloat => write!(f, "mpfloat"),
                SlType::Rational => write!(f, "rational"),
                SlType::Complex => write!(f, "complex"),
                SlType::Bool => write!(f, "{}", if bool_get(value) { "#T" } else { "#F" }),
                SlType::Err => write!(f, "<error>"),
                SlType::Other => write!(f, "other"),
            }
        }
    }
}

// SLATED FOR DELETION
// impl fmt::Display for Cons {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let car = self.car.to_string();
//         let cdr = self.cdr.to_string();
//         write!(f, "({} . {})", car, cdr)
//     }
// }
// impl fmt::Display for Value {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Value::Map(x) => write!(
//                 f,
//                 "{{{}}}",
//                 x.iter().fold(String::new(), |acc, new| acc
//                     + &new.0.to_string()
//                     + " "
//                     + &new.1.to_string()
//                     + " ")
//             ),
//             Value::Lambda { args, body } => write!(
//                 f,
//                 "<lambda fn: [{}] ({})>",
//                 args.iter().fold(String::new(), |acc, new| acc + new + " "),
//                 unsafe {
//                     match body.as_ref() {
//                         Some(x) => x.to_string(),
//                         None => String::new(),
//                     }
//                 }
//             ),
//             Value::Keyword(x) => write!(f, ":{}", x),
//             Value::String(x) => write!(f, "\"{}\"", x),
//         }
//     }
// }

/// Accepts an input stream and runs a read - evaluate - print loop perpetually
pub fn repl(stream_in: std::io::Stdin) {
    // TODO: Consider stack-like environment per function
    // TODO: Start to think about namespaces etc

    // Create persistent environment and symbol table
    let sym = unsafe { sym_tab_create() };
    let env = unsafe { env_create() };

    // Load standard / base definitions into environment and symbol table
    environment_setup(sym, env);

    loop {
        let mut input = String::new();
        stream_in.read_line(&mut input).expect("Failure");

        let expr = match parser::parse(sym, &input) {
            Ok(out) => out,
            Err(_) => {
                println!("Parse Error");
                continue;
            }
        };

        let result = match unsafe { eval(sym, env, expr) } {
            Ok(out) => out,
            Err(_) => {
                println!("Evaluation Error");
                continue;
            }
        };

        println!("{}", context(sym, result).to_string())
    }
}

/// Interprets a Sail expression, returning the result
pub fn interpret(code: &String) -> Result<String, SailErr> {
    let sym = unsafe { sym_tab_create() };
    let env = unsafe { env_create() };

    environment_setup(sym, env);

    // TODO: fix functions so such insanity isn't required to get them in place
    let expr = parser::parse(sym, code)?;
    let result = unsafe { eval(sym, env, expr) }?;

    Ok(context(sym, result).to_string())
}

/// TODO: fix functions so such insanity isn't required to get them in place
fn environment_setup(sym: *mut SlHead, env: *mut SlHead) {
    // Special form symbols
    unsafe {
        let def_str = init_symbol(false, SlSymbolMode::ByStr, 3);
        sym_set_str(def_str, b"def");
        let def_id = init_symbol(false, SlSymbolMode::ById, 0);
        sym_set_id(def_id, sym_tab_insert(sym, def_str));

        env_layer_ins_entry(car(env), def_id, def_str);

        let fn_str = init_symbol(false, SlSymbolMode::ByStr, 2);
        sym_set_str(fn_str, b"fn");
        let fn_id = init_symbol(false, SlSymbolMode::ById, 0);
        sym_set_id(fn_id, sym_tab_insert(sym, fn_str));

        env_layer_ins_entry(car(env), fn_id, fn_str);
    }

    // Addition function (prototype)
    unsafe {
        let add_str = init_symbol(false, SlSymbolMode::ByStr, 1);
        sym_set_str(add_str, b"+");
        let add_id = init_symbol(false, SlSymbolMode::ById, 0);
        sym_set_id(add_id, sym_tab_insert(sym, add_str));

        let add_fn = init_proc(false, SlProcMode::Native, 2);
        proc_native_set(add_fn, add);
        env_layer_ins_entry(car(env), add_id, add_fn);
    }

    unsafe {
        let sub_str = init_symbol(false, SlSymbolMode::ByStr, 1);
        sym_set_str(sub_str, b"-");
        let sub_id = init_symbol(false, SlSymbolMode::ById, 0);
        sym_set_id(sub_id, sym_tab_insert(sym, sub_str));

        let sub_fn = init_proc(false, SlProcMode::Native, 2);
        proc_native_set(sub_fn, sub);
        env_layer_ins_entry(car(env), sub_id, sub_fn);
    }
}

/// Evaluates a Sail value, returning the result
unsafe fn eval(
    sym: *mut SlHead,
    env: *mut SlHead,
    expr: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    match get_type(expr) {
        SlType::Symbol => return Ok(env_lookup(env, expr)),
        SlType::List => {
            if list_empty_p(expr) {
                return Ok(expr);
            }

            let operator = eval(sym, env, car(expr))?;
            // TODO: replace with next_list_elt(list_get(expr)) to avoid allocation
            let args = cdr(expr);
            match get_type(operator) {
                SlType::Proc => {
                    return apply(sym, env, operator, args);
                }
                SlType::Symbol => {
                    // TODO: special form handling
                    // TODO: is there a need for special forms? why not just make these native functions?
                    // TODO: these may be good examples for creating / using native functions cleanly
                    match sym_get_str(operator) {
                        "def" => {
                            env_layer_ins_entry(
                                car(env),
                                car(args),
                                eval(sym, env, car(cdr(args)))?,
                            );
                            return Ok(car(args));
                        }
                        "fn" => {
                            let argvec = car(args);
                            let argct = vec_get_len(argvec);
                            let out = init_proc(false, SlProcMode::Lambda, argct);
                            for i in 0..argct {
                                proc_set_arg(out, i, sym_get_id(vec_idx(argvec, i)));
                            }

                            proc_lambda_set(out, car(cdr(args)));
                            return Ok(out);
                        }
                        _ => {
                            eprintln!("error on special form symbol");
                            return Err(SailErr::Error);
                        }
                    }
                }
                _ => {
                    eprintln!("error on operator type");
                    Err(SailErr::Error)
                }
            }
        }
        _ => return Ok(expr),
    }
}

/// Applies a Sail procedure to its arguments, returning the result
/// TODO: execute multiple expressions in a lambda sequentially?
/// TODO: use an ordered, alist based env layer for procedures
unsafe fn apply(
    sym: *mut SlHead,
    env: *mut SlHead,
    proc: *mut SlHead,
    args: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    let argct = proc_get_argct(proc);
    let mode = proc_mode(proc);

    let proc_env = env_new_arg_layer();

    let mut arglist = args;

    for i in 0..argct {
        if nil_p(arglist) || list_empty_p(arglist) {
            eprintln!("error on empty arglist");
            return Err(SailErr::Error);
        }

        let curarg = eval(sym, env, car(arglist))?;

        match mode {
            SlProcMode::Lambda => {
                // TODO: just get and look up arg by id itself, without allocating
                env_arg_layer_ins(proc_env, proc_get_arg(proc, i), curarg);
            }
            SlProcMode::Native => {
                // special symbols "@0", "@1", "@2", etc for native arguments
                let mut spec_str = String::from("@");
                spec_str.push_str(&(i.to_string()));

                let spec_sym_str = init_symbol(false, SlSymbolMode::ByStr, spec_str.len() as u16);
                sym_set_str(spec_sym_str, &spec_str.into_bytes());

                let mut spec_sym_id = sym_tab_lookup_by_str(sym, spec_sym_str);
                while get_type(spec_sym_id) != SlType::Symbol {
                    sym_tab_insert(sym, spec_sym_str);
                    spec_sym_id = sym_tab_lookup_by_str(sym, spec_sym_str);
                }

                env_arg_layer_ins(proc_env, spec_sym_id, curarg);
            }
        }

        arglist = cdr(arglist)
    }

    env_push_layer(env, proc_env);

    let result = match mode {
        SlProcMode::Lambda => eval(sym, env, proc_lambda_body(proc)),
        SlProcMode::Native => Ok(proc_native_body(proc)(sym, env)),
    };

    env_pop_layer(env);

    result
}

macro_rules! sail_fn {
    ( $fn_name:tt [ $($args:ident),* ] { $($body:stmt)* } ) => {};
}

sail_fn!(
    + [add1, add2] {
        fixint_get(add1) + fixint_get(add2);
        return;
    }
);

/// TODO: need a macro for creating a native Sail function / adding to environment
/// TODO: native functions MUST be fully safe to use
unsafe fn add(sym: *mut SlHead, env: *mut SlHead) -> *mut SlHead {
    let fst = env_arg_layer_get(car(env), 0);
    let snd = env_arg_layer_get(car(env), 1);

    let out = init_fixint(false);
    fixint_set(out, fixint_get(fst) + fixint_get(snd));
    out
}

unsafe fn sub(sym: *mut SlHead, env: *mut SlHead) -> *mut SlHead {
    let fst = env_arg_layer_get(car(env), 0);
    let snd = env_arg_layer_get(car(env), 1);

    let out = init_fixint(false);
    fixint_set(out, fixint_get(fst) - fixint_get(snd));
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn returns() {
        let exp = String::from("42");
        assert_eq!(exp, interpret(&exp).unwrap());
    }

    #[test]
    fn adds() {
        let exp = String::from("(+ 2 2)");
        assert_eq!("4", interpret(&exp).unwrap());
    }

    #[test]
    fn parses() {
        let tbl = unsafe { sym_tab_create() };

        let exp = String::from("(+ (() 42 (e) #T) #F 2.1 e)");
        let val = parser::parse(tbl, &exp).unwrap();
        let out = context(tbl, val).to_string();
        assert_eq!(exp, out);

        let exp = String::from("(() (()) ((((() ())))))");
        let val = parser::parse(tbl, &exp).unwrap();
        let out = context(tbl, val).to_string();
        assert_eq!(exp, out);

        let exp = String::from("((1 2 3 4) ;Comment\n5)");
        let gnd = String::from("((1 2 3 4) 5)");
        let val = parser::parse(tbl, &exp).unwrap();
        let out = context(tbl, val).to_string();
        assert_eq!(gnd, out);
    }
}

macro_rules! typechk {
    ( $var:ident == $typ:ident ) => {
        assert_eq!($crate::sail::get_type($var), $crate::sail::types::SlType::$typ);
    };

    ( Ref $($mode:ident)? ; $var:ident ) => {
        assert_eq!($crate::sail::get_type($var), $crate::sail::types::SlType::Ref);
        $(assert_eq!($crate::sail::ref_mode($var), $crate::sail::types::SlRefMode::$mode);)?
    };

    ( Vec $($mode:ident)? ; $var:ident ) => {
        assert_eq!($crate::sail::get_type($var), $crate::sail::types::SlType::Vec);
        $(assert_eq!($crate::sail::vec_mode($var), $crate::sail::types::SlVecMode::$mode);)?
    };

    ( Map $($mode:ident)? ; $var:ident ) => {
        assert_eq!($crate::sail::get_type($var), $crate::sail::types::SlType::Map);
        $(assert_eq!($crate::sail::map_mode($var), $crate::sail::types::SlMapMode::$mode);)?
    };

    ( Proc $($mode:ident)? ; $var:ident ) => {
        assert_eq!($crate::sail::get_type($var), $crate::sail::types::SlType::Proc);
        $(assert_eq!($crate::sail::proc_mode($var), $crate::sail::types::SlProcMode::$mode);)?
    };

    ( Symbol $($mode:ident)? ; $var:ident ) => {
        assert_eq!($crate::sail::get_type($var), $crate::sail::types::SlType::Symbol);
        $(assert_eq!($crate::sail::sym_mode($var), $crate::sail::types::SlSymbolMode::$mode);)?
    };

    ( ; $typ:ident ; $var:ident ) => {
        assert_eq!($crate::sail::get_type($var), $crate::sail::types::SlType::$typ);
    };
}

/// Primary Sail types; each Sail value must be one of these
#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum SlType {
    Ref = 0,
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

// TODO: potential change: halve the number of top level types (or
// reduce the refct size) and add a "shared" bit to indicate that a
// value may be read and / or written by other threads

pub const HEAD_LEN: u8 = 2;
pub const PTR_LEN: u8 = 8;
pub const VEC_SH_LEN: u8 = 4;
pub const MAP_SH_LEN: u8 = 2;
pub const PROC_SH_LEN: u8 = 2;
pub const SYMBOL_SH_LEN: u8 = 2;
pub const SYMBOL_ID_LEN: u8 = 4;
pub const STRING_SH_LEN: u8 = 4;
pub const FIXNUM_32_LEN: u8 = 4;
pub const FIXNUM_64_LEN: u8 = 8;

/// Header for all Sail values in memory (optimizations later)
/// **Handle ONLY using methods such as sail::alloc and etc**
/// Should only store information that every referenced Sail value needs
/// TODO: Think about references, memory management, and associative table support
#[repr(C)]
pub struct SlHead {
    pub conf: u8,
    pub rc: u8,
}

/// ALL Sail values that may be independently referenced, begin with bytes of this format
/// type: 4 bits - list: 1 bit - config: 3 bits - rc: 8 bits
/// The first eight bits determine the subsequent memory layout
const _MIN_HEAD: u16 = 0b1111011100000000;

/// Pointer to the next element of a linked list;
/// appears immediately after the SlHead.
struct SlListPtr {
    ptr: *mut SlHead,
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
// TODO: Make List a multimodal Ref type (references, lists, queues)

struct SlRef {
    ptr: *mut SlHead,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum SlRefMode {
    List = 0,
    Redir = 1,
    QSend = 2,
    QReceive = 3,
}

// TODO: Vecs and Maps should have various modes with various performance characteristics in the future
#[repr(C)]
pub struct SlVecSH {
    pub len: u16,
    pub cap: u16,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum SlVecMode {
    Default = 0,
    FlatF32 = 1,
    FlatF64 = 2,
    FlatI32 = 3,
    FlatI64 = 4,
    FlatU32 = 5,
    FlatU64 = 6,
    FlatBit = 7,
}

#[repr(C)]
pub struct SlMapSH {
    pub size: u16,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum SlMapMode {
    Assoc = 0,
    Alist = 1,
}

pub struct SlProcSH {
    pub argct: u16,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum SlProcMode {
    Lambda = 0,
    Native = 1,
}

pub struct SlSymbolSH {
    pub len: u16,
}

// for now, keywords and symbols byid use the same representation
pub struct SlSymbolId {
    pub id: u32,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum SlSymbolMode {
    ById = 0,
    ByStr = 1,
}

#[repr(C)]
pub struct SlStringSH {
    pub len: u16,
    pub cap: u16,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
enum SlFixNumMode {
    I32,
    I64,
    I128,
    U32,
    U64,
    U128,
    F32,
    F64,
}

pub const MP_INT_LEN: u8 = 16;
struct SlMpInt {
    val: rug::Integer,
}

pub const MP_FLOAT_LEN: u8 = 32;
struct SlMpFloat {
    val: rug::Float,
}

pub const RATIONAL_LEN: u8 = 32;
struct SlRational {
    val: rug::Rational,
}

pub const COMPLEX_LEN: u8 = 64;
struct SlComplex {
    val: rug::Complex,
}

//TODO: Not just complexes, but also quaternions

#[repr(u8)]
enum SlBoolMode {
    False = 0,
    True = 1,
}

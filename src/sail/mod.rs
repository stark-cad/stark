//! The Structured Augmentation Interchange Language
//!
//! A custom Lisp dialect for writing STARK

use std::fmt;
use std::mem;
use std::ptr;

#[macro_use]
mod core;
pub use self::core::*;

pub mod memmgt;
pub mod parser;
pub mod queue;

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

/// TODO: remember types that are parents / children of others
/// TODO: types with children cannot be a self type
/// TODO: these items must be added to the symtab and env on every run
/// TODO: remember to have a bitvec type
/// TODO: use a script to automatically generate a Rust "env" file
/// TODO: separate all core types from other types a little bit more
macro_rules! incl_symbols {
    ( $array:ident : $( $id:literal $name:ident $strng:literal $mode:ident; )+ ; $size:literal ) => {
        $(
            const $name: (u32, &str) = (modeize_sym($id, SymbolMode::$mode), $strng);
        )+
            const $array: [&str; $size] = [$($strng),+];
    };
}

incl_symbols! {
    SYM_ARRAY:
    0  T_T           "t"       Type;
    1  T_NIL         "nil"     Type;
    2  T_BOOL        "bool"    Type;
    3  T_U8          "u8"      Type;
    4  T_I8          "i8"      Type;
    5  T_U16         "u16"     Type;
    6  T_I16         "i16"     Type;
    7  T_U32         "u32"     Type;
    8  T_I32         "i32"     Type;
    9  T_U64         "u64"     Type;
    10 T_I64         "i64"     Type;
    11 T_U128        "u128"    Type;
    12 T_I128        "i128"    Type;
    13 T_F32         "f32"     Type;
    14 T_F64         "f64"     Type;
    15 T_SYMBOL      "symbol"  Type;
    16 T_REF         "ref"     Type;
    17 T_VECTOR      "vector"  Type;
    18 T_STDVEC      "stdvec"  Type;
    19 T_STRING      "string"  Type;
    20 T_HASHVEC     "hashvec" Type;
    21 T_ANYVEC      "anyvec"  Type;
    22 T_MAP         "map"     Type;
    23 T_ALISMAP     "alismap" Type;
    24 T_HASHMAP     "hashmap" Type;
    25 T_PROC        "proc"    Type;
    26 T_PROC_LAMBDA "sail-fn" Type;
    27 T_PROC_NATIVE "rust-fn" Type;
    28 T_ERR         "err"     Type;
    29 SP_DEF        "def"     Basic;
    30 SP_DO         "do"      Basic;
    31 SP_FN         "fn"      Basic;
    32 SP_IF         "if"      Basic;
    33 SP_QUOTE      "quote"   Basic;;
    34
}

/// Set a symbol to one of the four symbol modes
const fn modeize_sym(sym: u32, mode: SymbolMode) -> u32 {
    (sym & 0x3FFFFFFF) + ((mode as u32) << 30)
}

/// Returns a symbol set to the default, basic mode
const fn demodes_sym(sym: u32) -> u32 {
    sym & 0x3FFFFFFF
}

/// Get the mode of a symbol
const fn mode_of_sym(sym: u32) -> SymbolMode {
    unsafe { mem::transmute::<u8, SymbolMode>((sym >> 30) as u8) }
}

// TODO: MINIMIZE the use of *pub* and *unsafe* functions

fn get_self_type(loc: *mut SlHead) -> u32 {
    if nil_p(loc) {
        return T_NIL.0;
    }
    use Cfg::*;
    match get_cfg_spec(loc) {
        B0BoolF | B0BoolT => T_BOOL.0,
        B1U8 => T_U8.0,
        B1I8 => T_I8.0,
        B2U16 => T_U16.0,
        B2I16 => T_I16.0,
        B4U32 => T_U32.0,
        B4I32 => T_I32.0,
        B4F32 => T_F32.0,
        B4Sym => T_SYMBOL.0,
        B8U64 => T_U64.0,
        B8I64 => T_I64.0,
        B8F64 => T_F64.0,
        B8Ptr => T_REF.0,
        B16U128 => T_U128.0,
        B16I128 => T_I128.0,
        VecStd => T_STDVEC.0,
        VecStr => T_STRING.0,
        VecHash => T_HASHVEC.0,
        VecAny => T_ANYVEC.0,
        ProcLambda => T_PROC_LAMBDA.0,
        ProcNative => T_PROC_NATIVE.0,
        _ => {
            assert!(self_type_p(loc));
            unsafe { ptr::read_unaligned((loc as *mut u8).add(HEAD_LEN as usize) as *const u32) }
        }
    }
}

fn get_pred_type(loc: *mut SlHead) -> u32 {
    if nil_p(loc) {
        T_NIL.0
    } else if pred_type_p(loc) {
        unsafe {
            ptr::read_unaligned((loc as *const u8).add(if !self_type_p(loc) {
                HEAD_LEN
            } else {
                HEAD_LEN + SYMBOL_LEN
            } as usize) as *const u32)
        }
    } else {
        // return $t, true for all types
        T_T.0
    }
}

/// Returns the size of a valid Sail value
fn get_size(
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
    env: *mut SlHead,
    loc: *mut SlHead,
) -> usize {
    use BaseSize::*;
    match get_base_size(loc) {
        B0 => 0,
        B1 => 1,
        B2 => 2,
        B4 => 4,
        B8 => 8,
        B16 => 16,
        _ => match core_type(loc) {
            Some(_) => core_size(loc),
            None => {
                let entry = env_lookup_by_id(reg, env, get_self_type(loc));
                let entry_type = get_self_type(entry);

                if entry_type == T_U64.0 {
                    return u64_get(entry) as usize;
                } else if entry_type == T_PROC_LAMBDA.0 || entry_type == T_PROC_NATIVE.0 {
                    u64_get(apply(reg, tbl, env, entry, loc).unwrap()) as usize
                } else {
                    panic!("wrong type in type entry")
                }
            }
        },
    }
}

fn set_self_type(loc: *mut SlHead, typ: u32) {
    assert!(self_type_p(loc));
    unsafe { ptr::write_unaligned(loc.add(HEAD_LEN as usize) as *mut u32, typ) }
}

fn set_pred_type(loc: *mut SlHead, typ: u32) {
    assert!(pred_type_p(loc));
    unsafe {
        ptr::write_unaligned(
            loc.add(if self_type_p(loc) {
                HEAD_LEN + SYMBOL_LEN
            } else {
                HEAD_LEN
            } as usize) as *mut u32,
            typ,
        )
    }
}

/// TODO: eliminate as much write_unaligned as possible
fn write_field<T: SizedBase>(
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
    env: *mut SlHead,
    loc: *mut SlHead,
    offset: usize,
    src: T,
) {
    unsafe {
        let dst = value_ptr(loc).add(offset) as *mut T;
        assert!(offset + std::mem::size_of::<T>() <= get_size(reg, tbl, env, loc));
        ptr::write_unaligned(dst, src)
    }
}

/// TODO: eliminate as much read_unaligned as possible
fn read_field<T: SizedBase>(
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
    env: *mut SlHead,
    loc: *mut SlHead,
    offset: usize,
) -> T {
    unsafe {
        let src = value_ptr(loc).add(offset) as *mut T;
        assert!(offset + std::mem::size_of::<T>() <= get_size(reg, tbl, env, loc));
        ptr::read_unaligned(src)
    }
}

// unsafe fn ref_qsend_set_target(loc: *mut SlHead, target: *mut memmgt::MemSector) {
//     ptr::write_unaligned(
//         value_ptr(loc).offset(PTR_LEN as isize) as *mut *mut memmgt::MemSector,
//         target,
//     )
// }

// unsafe fn ref_qsend_get_target(loc: *mut SlHead) -> *mut memmgt::MemSector {
//     ptr::read_unaligned(value_ptr(loc).offset(PTR_LEN as isize) as *mut *mut memmgt::MemSector)
// }

// unsafe fn vec_idx_f32(loc: *mut SlHead, idx: u16) -> f32 {
//     ptr::read_unaligned(
//         value_ptr(loc)
//             .offset(VEC_SH_LEN as isize)
//             .offset(idx as isize * FIXNUM_32_LEN as isize) as *mut f32,
//     )
// }

// unsafe fn vec_push_f32(loc: *mut SlHead, item: f32) {
//     let (len, cap) = (vec_get_len(loc), vec_get_cap(loc));
//     if len < cap {
//         ptr::write_unaligned(
//             value_ptr(loc).offset(VEC_SH_LEN as isize + (len as isize * FIXNUM_32_LEN as isize))
//                 as *mut f32,
//             item,
//         );
//         vec_set_len(loc, len + 1);
//     } else {
//         panic!("not enough space in vec");
//     }
// }

pub union SlSend {
    ptr: *mut SlHead,
    num: usize,
}

/// Bundles together a value and associated symbol table for display
pub struct SlContextVal {
    tbl: *mut SlHead,
    val: *mut SlHead,
}

pub fn context(tbl: *mut SlHead, val: *mut SlHead) -> SlContextVal {
    SlContextVal { tbl, val }
}

// TODO: just push characters into a byte vector (string) for display
impl fmt::Display for SlContextVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let table = self.tbl;
        let value = self.val;

        use CoreType::*;
        match core_type(value) {
            Some(t) => match t {
                Nil => write!(f, "()"),
                Bool => write!(f, "{}", if bool_get(value) { "#T" } else { "#F" }),
                I64 => write!(f, "{}", i64_get(value)),
                F64 => write!(f, "{}", f64_get(value)),
                Symbol => write!(f, "{}", string_get(sym_tab_lookup_by_id(table, value))),
                Ref => {
                    write!(f, "(").unwrap();
                    let mut elt = ref_get(value);
                    while !nil_p(elt) {
                        if !list_elt_p(elt) {
                            write!(f, ". ").unwrap();
                            write!(f, "{}", context(table, elt).to_string()).unwrap();
                            break;
                        }
                        write!(f, "{}", context(table, elt).to_string()).unwrap();
                        elt = get_next_list_elt(elt);
                        if !nil_p(elt) {
                            write!(f, " ").unwrap();
                        }
                    }
                    write!(f, ")")
                }
                VecStd => {
                    write!(f, "[").unwrap();
                    let len = stdvec_get_len(value);
                    for idx in 0..len {
                        write!(f, "{}", context(table, stdvec_idx(value, idx)).to_string())
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
                    let size = hashvec_get_size(value);
                    let mut fst = true;
                    for idx in 0..size {
                        let mut pos =
                            core_read_field(value, 4 + 4 + (PTR_LEN as usize * idx as usize));
                        while !nil_p(pos) {
                            if !fst {
                                write!(f, " ").unwrap()
                            }
                            write!(f, "{} ", context(table, car(pos)).to_string()).unwrap();
                            write!(
                                f,
                                "{}",
                                context(
                                    table,
                                    cdr(unsafe { memmgt::which_mem_region(value) }, pos)
                                )
                                .to_string()
                            )
                            .unwrap();
                            pos = get_next_list_elt(pos);
                            fst = false;
                        }
                    }
                    write!(f, "}}")
                }
                ProcLambda | ProcNative => write!(f, "<$procedure>"),
                _ => write!(f, "<@core/$other>"),
            },
            None => write!(f, "<$other>"),
        }
    }
}

/// Accepts an input stream and runs a read - evaluate - print loop perpetually
pub fn repl(stream_in: std::io::Stdin) {
    // TODO: Consider stack-like environment per function
    // TODO: Start to think about namespaces etc

    let region = unsafe { memmgt::acquire_mem_region(1000000) };

    // Create persistent environment and symbol table
    let (tbl, env) = prep_environment(region);

    // Load standard / base definitions into environment and symbol table
    environment_setup(region, tbl, env);

    loop {
        let mut input = String::new();
        stream_in.read_line(&mut input).expect("Failure");

        let expr = match parser::parse(region, tbl, &input) {
            Ok(out) => out,
            Err(_) => {
                println!("Parse Error");
                continue;
            }
        };

        let result = match eval(region, tbl, env, expr) {
            Ok(out) => out,
            Err(_) => {
                println!("Evaluation Error");
                continue;
            }
        };

        println!("{}\n", context(tbl, result).to_string())
    }
}

pub fn run_file(filename: &str) -> Result<String, SailErr> {
    let file = std::fs::read_to_string(filename).unwrap();
    interpret(&file)
}

/// Interprets a Sail expression, returning the result
pub fn interpret(code: &str) -> Result<String, SailErr> {
    let region = unsafe { memmgt::acquire_mem_region(100000) };

    let (tbl, env) = prep_environment(region);

    environment_setup(region, tbl, env);

    let expr = parser::parse(region, tbl, code)?;
    let result = eval(region, tbl, env, expr)?;

    Ok(context(tbl, result).to_string())
}

/// TODO: fix functions so such insanity isn't required to get them in place
pub fn environment_setup(reg: *mut memmgt::Region, tbl: *mut SlHead, env: *mut SlHead) {
    for s in SYM_ARRAY.iter() {
        sym_tab_get_id(reg, tbl, s);
    }

    // Native functions
    insert_native_proc(reg, tbl, env, "+", add, 2);
    insert_native_proc(reg, tbl, env, "-", sub, 2);
    insert_native_proc(reg, tbl, env, "mod", modulus, 2);
    insert_native_proc(reg, tbl, env, "=", equal, 2);
    insert_native_proc(reg, tbl, env, "print", print, 1);
    insert_native_proc(reg, tbl, env, "printenv", printenv, 0);
    // insert_native_proc(reg, tbl, env, "color", color, 4);
    // insert_native_proc(reg, tbl, env, "qtx", qtx, 2);
}

/// TODO: intended to be temporary; still relies on some "magic values"
fn insert_native_proc(
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
    env: *mut SlHead,
    name: &str,
    func: fn(*mut SlHead, *mut SlHead) -> *mut SlHead,
    argct: u16,
) {
    let proc_id = init_symbol(reg);
    sym_set_id(proc_id, sym_tab_get_id(reg, tbl, name));

    let proc_fn = init_proc_native(reg, argct);
    proc_native_set_body(proc_fn, func);
    env_layer_ins_entry(reg, car(env), proc_id, proc_fn);
}

/// TODO: improve macro to allow adding functions to environment?
/// TODO: type checks and variable length arglists for native functions
/// TODO: add memory sector to function signature
/// TODO: generate these functions somehow else if macros won't cut it
macro_rules! sail_fn {
    ( $reg:ident; $( $fn_name:ident [ $($args:ident),* ] $body:block )+ ) => {
        $(
            fn $fn_name (_tbl: *mut SlHead, env: *mut SlHead) -> *mut SlHead {
                let $reg = unsafe { memmgt::which_mem_region(_tbl) };

                let mut _ind = 0;
                $(
                    let $args = env_arg_layer_get($reg, car(env), _ind);
                    _ind += 1;
                )*

                $body
            }
        )+
    };
}

// TODO: native functions MUST be fully safe to use
sail_fn! {
    reg;

    add [fst, snd] {
        let out = init_i64(reg);
        let result = i64_get(fst) + i64_get(snd);
        i64_set(out, result);
        return out;
    }

    sub [fst, snd] {
        let out = init_i64(reg);
        let result = i64_get(fst) - i64_get(snd);
        i64_set(out, result);
        return out;
    }

    modulus [fst, snd] {
        let out = init_i64(reg);
        let result = i64_get(fst) % i64_get(snd);
        i64_set(out, result);
        return out;
    }

    equal [fst, snd] {
        let out = init_bool(reg);
        let result = i64_get(fst) == i64_get(snd);
        bool_set(out, result);
        return out;
    }

    // qtx [sender, item] {
    //     queue::queue_tx(sender, item);

    //     let out = init_bool(reg);
    //     bool_set(out, true);
    //     return out;
    // }
}

fn print(_tbl: *mut SlHead, env: *mut SlHead) -> *mut SlHead {
    let reg = unsafe { memmgt::which_mem_region(_tbl) };
    let arg = env_arg_layer_get(reg, car(env), 0);
    println!("{}", context(_tbl, arg).to_string());
    return nil();
}

// TODO: sail_fn macro does not work for functions that access higher environment levels
fn printenv(_tbl: *mut SlHead, env: *mut SlHead) -> *mut SlHead {
    let reg = unsafe { memmgt::which_mem_region(_tbl) };
    println!("{}", context(_tbl, env).to_string());
    // TODO: from_bool function or similar
    let out = init_bool(reg);
    bool_set(out, true);
    return out;
}

/// TODO: replace with Sail-defined function
// fn color(_tbl: *mut SlHead, env: *mut SlHead) -> *mut SlHead {
//     let region = memmgt::which_mem_region(_tbl);

//     let r = env_arg_layer_get(car(env), 0);
//     let g = env_arg_layer_get(car(env), 1);
//     let b = env_arg_layer_get(car(env), 2);
//     let a = env_arg_layer_get(car(env), 3);

//     let qstr = init_symbol(sector, false, SlSymbolMode::ByStr, 7);
//     sym_set_str(qstr, b"g_queue");

//     let queue = env_lookup(env, sym_tab_lookup_by_str(_tbl, qstr));

//     let vec = init_vec(sector, false, SlVecMode::FlatF32, 4);

//     vec_push_f32(vec, fixfloat_get(r) as f32);
//     vec_push_f32(vec, fixfloat_get(g) as f32);
//     vec_push_f32(vec, fixfloat_get(b) as f32);
//     vec_push_f32(vec, fixfloat_get(a) as f32);

//     queue::queue_tx(queue, vec);

//     let out = init_bool(region);
//     bool_set(out, true);
//     return out;
// }

/// Evaluates a Sail value, returning the result
/// TODO: **Macros**, closures, continuations
pub fn eval(
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
    env: *mut SlHead,
    expr: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    if symbol_p(expr) {
        return Ok(env_lookup(reg, env, expr));
    } else if atom_p(expr) {
        return Ok(expr);
    } else {
        assert!(list_p(expr));
        let lcar = car(expr);
        let args = cdr(reg, expr);

        if symbol_p(lcar) {
            // TODO: what other special forms are needed?
            // TODO: is there a need for special forms? why not just make these native functions?
            // TODO: these may be good examples for creating / using native functions cleanly
            // TODO: just like native functions, these special forms should check for type
            let id = sym_get_id(lcar);
            if id == SP_DEF.0 {
                env_layer_ins_entry(
                    reg,
                    car(env),
                    car(args),
                    eval(reg, tbl, env, car(cdr(reg, args)))?,
                );
                return Ok(car(args));
            } else if id == SP_DO.0 {
                let mut remain = args;
                let mut result = nil();
                while !nil_p(remain) {
                    result = eval(reg, tbl, env, car(remain))?;
                    remain = cdr(reg, remain)
                }
                return Ok(result);
            } else if id == SP_FN.0 {
                let argvec = car(args);
                let argct = stdvec_get_len(argvec) as u16;
                let out = init_proc_lambda(reg, argct);
                for i in 0..argct {
                    proc_lambda_set_arg(out, i, sym_get_id(stdvec_idx(argvec, i as u32)));
                }
                proc_lambda_set_body(out, car(cdr(reg, args)));
                return Ok(out);
            } else if id == SP_IF.0 {
                let test = car(args);
                let fst = car(cdr(reg, args));
                let snd = car(cdr(reg, cdr(reg, args)));
                if bool_get(eval(reg, tbl, env, test)?) {
                    return eval(reg, tbl, env, fst);
                } else {
                    return eval(reg, tbl, env, snd);
                }
            } else if id == SP_QUOTE.0 {
                return Ok(car(args));
            }
        }
        let operator = eval(reg, tbl, env, lcar)?;
        // TODO: replace with next_list_elt(list_get(expr)) to avoid allocation
        if proc_p(operator) {
            return apply(reg, tbl, env, operator, args);
        } else {
            eprintln!("operator type error");
            return Err(SailErr::Error);
        }
    }
}

/// Applies a Sail procedure to its arguments, returning the result
/// TODO: execute multiple expressions in a lambda sequentially?
/// TODO: match the argument structure to the number of arguments needed
/// TODO: tail call optimization
fn apply(
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
    env: *mut SlHead,
    proc: *mut SlHead,
    args: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    let typ = match core_type(proc) {
        Some(t) if t == CoreType::ProcLambda => true,
        Some(t) if t == CoreType::ProcNative => false,
        _ => return Err(SailErr::Error),
    };

    let argct = proc_get_argct(proc);
    let proc_env = env_new_arg_layer(reg);

    let mut arglist = args;
    for i in 0..argct {
        if nil_p(arglist) {
            return Err(SailErr::Error);
        }

        let curarg = eval(reg, tbl, env, car(arglist))?;

        if typ {
            env_arg_layer_ins(reg, proc_env, proc_lambda_get_arg(reg, proc, i), curarg);
        } else {
            // TODO: need better call system for natives and maybe lambdas too
            // special symbols "%0", "%1", "%2", etc for native arguments
            let mut spec_str = String::from("%");
            spec_str.push_str(&(i.to_string()));

            let spec_sym_id = init_symbol(reg);
            sym_set_id(spec_sym_id, sym_tab_get_id(reg, tbl, &spec_str));

            env_arg_layer_ins(reg, proc_env, spec_sym_id, curarg);
        }

        arglist = cdr(reg, arglist)
    }

    env_push_layer(env, proc_env);

    let result = if typ {
        eval(reg, tbl, env, proc_lambda_get_body(proc))
    } else {
        Ok(proc_native_get_body(proc)(tbl, env))
    };

    env_pop_layer(env);

    result
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
        let (reg, tbl) = unsafe {
            let reg = memmgt::acquire_mem_region(10000);
            (reg, prep_environment(reg).0)
        };

        let exp = String::from("(+ (() 42 (e) #T) #F 2.1 e)");
        let val = parser::parse(reg, tbl, &exp).unwrap();
        let out = context(tbl, val).to_string();
        assert_eq!(exp, out);

        let exp = String::from("(() (()) ((((() ())))))");
        let val = parser::parse(reg, tbl, &exp).unwrap();
        let out = context(tbl, val).to_string();
        assert_eq!(exp, out);

        let exp = String::from("((1 2 3 4) ;Comment\n5)");
        let gnd = String::from("((1 2 3 4) 5)");
        let val = parser::parse(reg, tbl, &exp).unwrap();
        let out = context(tbl, val).to_string();
        assert_eq!(gnd, out);
    }
}

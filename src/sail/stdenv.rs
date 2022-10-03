// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License. See the top level LICENSE file for the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/sail/stdenv.rs

// Items, particularly native procedures, which are part of the
// standard Sail environment and should be automatically loaded.

// <>

use super::{core::*, memmgt};

/// Generates a slice of native Sail function pointers along with
/// names and argument counts
///
/// This may be a constant or may be local to a Rust scope. The syntax
/// used is quite similar to that of regular functions but eases
/// access to arguments in the body. All native functions must return
/// a valid Sail object.
///
/// TODO: type checks and variable length arglists for native functions
/// TODO: generate these functions somehow else if macros won't cut it
#[macro_export]
macro_rules! sail_fn {
    ( const $array:ident; $reg:ident $tbl:ident $env:ident;
      $( $name:literal $argct:literal [ $($args:ident),* ] $body:block )+
    ) => {
        pub const $array: &[(&str, crate::sail::core::NativeFn, u16)] =
            &[$(($name, |
                _reg: *mut crate::sail::memmgt::Region,
                _tbl: *mut crate::sail::SlHead,
                _env: *mut crate::sail::SlHead,
                _args: &[*mut crate::sail::SlHead]
                | {
                    let $reg = _reg;
                    let $tbl = _tbl;
                    let $env = _env;

                    let mut _ind = 0;
                    $(
                        let $args = _args[_ind];
                        _ind += 1;
                    )*

                        $body
                },
                $argct)),+];
    };

    ( let $array:ident; $reg:ident $tbl:ident $env:ident;
      $( $name:literal $argct:literal [ $($args:ident),* ] $body:block )+
    ) => {
        let $array: &[(&str, crate::sail::core::NativeFn, u16)] =
            &[$(($name, |
                _reg: *mut crate::sail::memmgt::Region,
                _tbl: *mut crate::sail::SlHead,
                _env: *mut crate::sail::SlHead,
                _args: &[*mut crate::sail::SlHead]
                | {
                    let $reg = _reg;
                    let $tbl = _tbl;
                    let $env = _env;

                    let mut _ind = 0;
                    $(
                        let $args = _args[_ind];
                        _ind += 1;
                    )*

                        $body
                },
                $argct)),+];
    };
}

// TODO: native functions MUST be fully safe to use
// TODO: sensible type checking & operator overloading
sail_fn! {
    const ENVFNS;
    _reg _tbl _env;

    // TODO: use fixed point at times to avoid floating point errors?

    "+" 2 [fst, snd] {
        let typ = core_type(fst);
        assert_eq!(typ, core_type(snd));

        match typ.expect("type invalid") {
            CoreType::I64 => {
                let out = i64_make(_reg);
                let result = i64_get(fst) + i64_get(snd);
                i64_set(out, result);
                return out;
            }
            CoreType::F32 => {
                let out = f32_make(_reg);
                let result = f32_get(fst) + f32_get(snd);
                f32_set(out, result);
                return out;
            }
            _ => panic!("type invalid for add"),
        }
    }

    "-" 2 [fst, snd] {
        let typ = core_type(fst);
        assert_eq!(typ, core_type(snd));

        match typ.expect("type invalid") {
            CoreType::I64 => {
                let out = i64_make(_reg);
                let result = i64_get(fst) - i64_get(snd);
                i64_set(out, result);
                return out;
            }
            CoreType::F32 => {
                let out = f32_make(_reg);
                let result = f32_get(fst) - f32_get(snd);
                f32_set(out, result);
                return out;
            }
            _ => panic!("type invalid for sub"),
        }
    }

    "*" 2 [fst, snd] {
        let typ = core_type(fst);
        assert_eq!(typ, core_type(snd));

        match typ.expect("type invalid") {
            CoreType::I64 => {
                let out = i64_make(_reg);
                let result = i64_get(fst) * i64_get(snd);
                i64_set(out, result);
                return out;
            }
            CoreType::F32 => {
                let out = f32_make(_reg);
                let result = f32_get(fst) * f32_get(snd);
                f32_set(out, result);
                return out;
            }
            _ => panic!("type invalid for mul"),
        }
    }

    "/" 2 [fst, snd] {
        let typ = core_type(fst);
        assert_eq!(typ, core_type(snd));

        match typ.expect("type invalid") {
            CoreType::I64 => {
                let out = i64_make(_reg);
                let result = i64_get(fst) / i64_get(snd);
                i64_set(out, result);
                return out;
            }
            CoreType::F32 => {
                let out = f32_make(_reg);
                let result = f32_get(fst) / f32_get(snd);
                f32_set(out, result);
                return out;
            }
            _ => panic!("type invalid for div"),
        }
    }

    "mod" 2 [fst, snd] {
        let out = i64_make(_reg);
        let result = i64_get(fst) % i64_get(snd);
        i64_set(out, result);
        return out;
    }

    "neg" 1 [val] {
        match core_type(val).expect("type invalid") {
            CoreType::I64 => {
                return i64_init(_reg, -i64_get(val));
            }
            CoreType::F32 => {
                return f32_init(_reg, -f32_get(val));
            }
            _ => panic!("type invalid for div"),
        }
    }

    "=" 2 [fst, snd] {
        // let out = init_bool(reg);
        let result = i64_get(fst) == i64_get(snd);
        if result {
            env_lookup_by_id(_env, super::S_T_INTERN.0)
        } else {
            nil()
        }
        // bool_set(out, result);
        // return out;
    }

    "eq" 2 [fst, snd] {
        // let out = init_bool(reg);
        let result = core_eq(fst, snd);
        if result {
            env_lookup_by_id(_env, super::S_T_INTERN.0)
        } else {
            nil()
        }
        // bool_set(out, result);
        // return out;
    }

    "not" 1 [val] {
        // let out = init_bool(reg);
        if !truthy(val) {
            // bool_set(out, false)
            env_lookup_by_id(_env, super::S_T_INTERN.0)
        } else {
            // bool_set(out, true)
            nil()
        }
        // return out;
    }

    "qtx" 2 [sender, item] {
        super::queue::queue_tx(sender, item);

        // let out = init_bool(reg);
        // bool_set(out, true);
        // return out;

        return nil();
    }

    "qrx" 2 [receiver] {
        return super::queue::queue_rx(receiver);
    }

    "as-f32" 1 [val] {
        return f32_init(_reg, f64_get(val) as f32);
    }

    "arr-vec-make" 3 [typ, len, init] {
        coretypck!(typ ; Symbol);
        coretypck!(len ; I64);

        let typ = sym_get_id(typ);
        let len = i64_get(len) as u32;

        assert!(temp_base_sized_p(typ));
        // assert_eq!(typ, super::get_self_type(init));

        unsafe {
            let size = vec_size(8, temp_get_size(typ), len);
            let ptr = memmgt::alloc(_reg, size, memmgt::cap(Cfg::VecArr));

            write_field_unchecked::<u32>(ptr, 0, typ);
            write_field_unchecked::<u32>(ptr, 4, len);

            for i in 0..len {
                std::ptr::copy_nonoverlapping(
                    value_ptr(init),
                    value_ptr(ptr).add(8 + (temp_get_size(typ) * i) as usize),
                    temp_get_size(typ) as usize,
                )
            }

            return ptr;
        }

    }

    "arr-vec-get" 2 [target, idx] {
        coretypck!(target ; VecArr);
        coretypck!(idx ; I64);
        let typ = super::arrvec_get_typ(target);
        assert!(temp_base_sized_p(typ));

        let idx = i64_get(idx) as u32;
        assert!(idx < super::arrvec_get_len(target));

        return temp_init_from(_reg, typ, unsafe {
            value_ptr(target).add(8 + (temp_get_size(typ) * idx) as usize)
        });
    }

    "arr-vec-set" 3 [target, idx, val] {
        coretypck!(target ; VecArr);
        coretypck!(idx ; I64);
        let typ = super::arrvec_get_typ(target);
        assert!(temp_base_sized_p(typ));
        // assert_eq!(typ, super::get_self_type(val));

        let idx = i64_get(idx) as u32;
        assert!(idx < super::arrvec_get_len(target));

        unsafe {
            std::ptr::copy_nonoverlapping(
                value_ptr(val),
                value_ptr(target).add(8 + (temp_get_size(typ) * idx) as usize),
                temp_get_size(typ) as usize,
            )
        }

        return target;
    }

    "print" 1 [arg] {
        println!("{}", super::context(_tbl, arg).to_string());
        return nil();
    }

    "dbg" 1 [arg] {
        println!("{}", super::context(_tbl, arg).to_string());
        return arg;
    }

    "printenv" 0 [] {
        println!("{}", super::context(_tbl, _env).to_string());
        return nil();
    }

    "parse" 1 [strin] {
        coretypck!(strin ; VecStr);
        let strsl = string_get(strin);

        return match super::parser::parse(_reg, _tbl, strsl) {
            Ok(head) => head,
            Err(err) => super::errcode_init(_reg, err),
        };
    }
}

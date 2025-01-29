// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/stdenv.rs

// Items, particularly native procedures, which are part of the
// standard Sail environment and should be automatically loaded.

// <>

use super::{core::*, memmgt};

// TODO: type checks and variable length arglists for native functions
// TODO: generate these functions somehow else if macros won't cut it

/// Generates a slice of native Sail function pointers along with
/// names and argument counts
///
/// This may be a constant or may be local to a Rust scope. The syntax
/// used is quite similar to that of regular functions but eases
/// access to arguments in the body. All native functions must return
/// a valid Sail object.
#[macro_export]
macro_rules! sail_fn {
    ( const $array:ident; $thr:ident $env:ident;
      $( $name:literal [ $($args:ident),* ] $body:block )+
    ) => {
        pub const $array: &[(&str, crate::sail::core::NativeFn, u16)] =
            &[$(($name, |
                _thr: *mut crate::sail::thread::ThreadHull,
                _env: crate::sail::SlHndl,
                _args: &[crate::sail::SlHndl],
              | {
                    let $thr = _thr;
                    let $env = _env;

                    $(
                        let mut $args = _args[${index()}].clone();
                    )*

                        $body
              },
              ${count($args)})),+];
    };

    ( let $array:ident; $thr:ident $env:ident;
      $( $name:literal [ $($args:ident),* ] $body:block )+
    ) => {
        let $array: &[(&str, crate::sail::core::NativeFn, u16)] =
            &[$(($name, |
                _thr: *mut crate::sail::thread::ThreadHull,
                _env: crate::sail::SlHndl,
                _args: &[crate::sail::SlHndl],
                | {
                    let $thr = _thr;
                    let $env = _env;

                    $(
                        let mut $args = _args[${index()}].clone();
                    )*

                        $body
                },
                ${count($args)})),+];
    };
}

// TODO: native functions MUST be fully safe to use
// TODO: sensible type checking & operator overloading
sail_fn! {
    const ENVFNS;
    _thr _env;

    // TODO: use fixed point at times to avoid floating point errors?

    "+" [fst, snd] {
        let typ = fst.core_type();
        assert_eq!(typ, snd.core_type());

        let reg = unsafe { (*_thr).region() };

        match typ.expect("type invalid") {
            CoreType::I64 => {
                let out = i64_make(reg);
                let result = i64_get(fst) + i64_get(snd);
                i64_set(out.clone(), result);
                return out;
            }
            CoreType::F32 => {
                let out = f32_make(reg);
                let result = f32_get(fst) + f32_get(snd);
                f32_set(out.clone(), result);
                return out;
            }
            _ => panic!("type invalid for add"),
        }
    }

    "-" [fst, snd] {
        let typ = fst.core_type();
        assert_eq!(typ, snd.core_type());

        let reg = unsafe { (*_thr).region() };

        match typ.expect("type invalid") {
            CoreType::I64 => {
                let out = i64_make(reg);
                let result = i64_get(fst) - i64_get(snd);
                i64_set(out.clone(), result);
                return out;
            }
            CoreType::F32 => {
                let out = f32_make(reg);
                let result = f32_get(fst) - f32_get(snd);
                f32_set(out.clone(), result);
                return out;
            }
            _ => panic!("type invalid for sub"),
        }
    }

    "*" [fst, snd] {
        let typ = fst.core_type();
        assert_eq!(typ, snd.core_type());

        let reg = unsafe { (*_thr).region() };

        match typ.expect("type invalid") {
            CoreType::I64 => {
                let out = i64_make(reg);
                let result = i64_get(fst) * i64_get(snd);
                i64_set(out.clone(), result);
                return out;
            }
            CoreType::F32 => {
                let out = f32_make(reg);
                let result = f32_get(fst) * f32_get(snd);
                f32_set(out.clone(), result);
                return out;
            }
            _ => panic!("type invalid for mul"),
        }
    }

    "/" [fst, snd] {
        let typ = fst.core_type();
        assert_eq!(typ, snd.core_type());

        let reg = unsafe { (*_thr).region() };

        match typ.expect("type invalid") {
            CoreType::I64 => {
                let out = i64_make(reg);
                let result = i64_get(fst) / i64_get(snd);
                i64_set(out.clone(), result);
                return out;
            }
            CoreType::F32 => {
                let out = f32_make(reg);
                let result = f32_get(fst) / f32_get(snd);
                f32_set(out.clone(), result);
                return out;
            }
            _ => panic!("type invalid for div"),
        }
    }

    "mod" [fst, snd] {
        let reg = unsafe { (*_thr).region() };

        let out = i64_make(reg);
        let result = i64_get(fst) % i64_get(snd);
        i64_set(out.clone(), result);
        return out;
    }

    "neg" [val] {
        let reg = unsafe { (*_thr).region() };
        match val.core_type().expect("type invalid") {
            CoreType::I64 => {
                return i64_init(reg, -i64_get(val));
            }
            CoreType::F32 => {
                return f32_init(reg, -f32_get(val));
            }
            _ => panic!("type invalid for div"),
        }
    }

    "=" [fst, snd] {
        let result = i64_get(fst) == i64_get(snd);
        if result {
            env_lookup_by_id(_env, super::S_T_INTERN.0).unwrap()
        } else {
            env_lookup_by_id(_env, super::S_F_INTERN.0).unwrap()
        }
        // return bool_init(_reg, result);
    }

    "eq" [fst, snd] {
        let result = core_eq(fst, snd);
        if result {
            env_lookup_by_id(_env, super::S_T_INTERN.0).unwrap()
        } else {
            env_lookup_by_id(_env, super::S_F_INTERN.0).unwrap()
        }
        // return bool_init(_reg, result);
    }

    "not" [val] {
        // let out = bool_make(_reg);
        if val.truthy() {
            // bool_set(out.clone(), false)
            env_lookup_by_id(_env, super::S_F_INTERN.0).unwrap()
        } else {
            // bool_set(out.clone(), true)
            env_lookup_by_id(_env, super::S_T_INTERN.0).unwrap()
        }
        // return out;
    }

    // TODO: and and or as macros or, more easily, special forms
    "and" [lhs, rhs] {
        if lhs.truthy() && rhs.truthy() {
            env_lookup_by_id(_env, super::S_T_INTERN.0).unwrap()
        } else {
            env_lookup_by_id(_env, super::S_F_INTERN.0).unwrap()
        }
    }

    "own-tx-hdl" [] {
        let reg = unsafe { (*_thr).region() };

        let tq = unsafe { (*_thr).queue_inlet() };

        super::warp_hdl_init(reg, tq)
    }

    "th-spawn" [fun] {
        let reg = unsafe { (*_thr).region() };
        let new = unsafe { (*_thr).spawn(None, None) };

        let nr = unsafe { (*new).region() };
        let to_apply = unsafe {
            SlHndl::from_raw_unchecked(super::structure_copy(nr, fun))
        };

        unsafe { (*new).load_proc_immed(to_apply) };

        if !unsafe { (*new).attempt_start() } {
            panic!()
        }

        let nq = unsafe { (*new).queue_inlet() };

        let send_tgt = super::warp_hdl_init(reg, nq);
        let new_hdl = super::thread_ref_init(reg, new);

        set_next_list_elt(_env, new_hdl.clone(), send_tgt);

        new_hdl
    }

    "th-join" [thref] {
        let reg = unsafe { (*_thr).region() };
        super::thread_ref_join(reg, thref)
    }

    "th-id" [thref] {
        assert_eq!(thref.type_id(), super::T_THR_REF_ID.0);

        let addrs: u64 = read_field(thref, 0);

        let th_ref = unsafe {
            (addrs as *mut super::thread::ThreadHull)
                .as_mut()
                .expect("null thread reference")
        };

        let reg = unsafe { (*_thr).region() };

        super::i64_init(reg, th_ref.id as _)
    }

    "qtx" [sender, item] {
        let id = unsafe { (*_thr).id };

        super::warp_hdl_send(sender, item, id);

        env_lookup_by_id(_env, super::S_T_INTERN.0).unwrap()
    }

    "qrx" [] {
        let reg = unsafe { (*_thr).region() };
        let inlet = unsafe { (*_thr).queue_inlet() };

        let msg = unsafe { (&mut *inlet).receive() };

        match msg.1 {
            Some(r) => {
                let id = i64_init(reg, msg.0 as _);
                set_next_list_elt(_env, id.clone(), r);
                id
            }
            None => env_lookup_by_id(_env, super::S_F_INTERN.0).unwrap(),
        }
    }

    "rest" [loc] {
        match get_next_list_elt(loc) {
            Some(h) => h,
            None => env_lookup_by_id(_env, super::S_F_INTERN.0).unwrap(),
        }
    }

    "link" [fst, nxt] {
        set_next_list_elt(_env, fst.clone(), nxt);
        fst
    }

    "as-f32" [val] {
        let reg = unsafe { (*_thr).region() };
        return f32_init(reg, f64_get(val) as f32);
    }

    "arr-vec-make" [typ, len, init] {
        coretypck!(typ ; Symbol);
        coretypck!(len ; I64);

        let typ = sym_get_id(typ);
        let len = i64_get(len) as u32;

        assert!(temp_base_sized_p(typ));
        // assert_eq!(typ, super::get_self_type(init));

        let reg = unsafe { (*_thr).region() };

        unsafe {
            let size = vec_size(8, temp_get_size(typ), len);
            let mut out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, size, memmgt::cap(Cfg::VecArr)));

            write_field_unchecked::<u32>(out.clone(), 0, typ);
            write_field_unchecked::<u32>(out.clone(), 4, len);

            for i in 0..len {
                std::ptr::copy_nonoverlapping(
                    init.value_ptr(),
                    out.value_ptr().add(8 + (temp_get_size(typ) * i) as usize),
                    temp_get_size(typ) as usize,
                )
            }

            return out;
        }

    }

    "arr-vec-get" [target, idx] {
        coretypck!(target ; VecArr);
        coretypck!(idx ; I64);
        let typ = super::arrvec_get_typ(target.clone());
        assert!(temp_base_sized_p(typ));

        let idx = i64_get(idx) as u32;
        assert!(idx < super::arrvec_get_len(target.clone()));

        let reg = unsafe { (*_thr).region() };

        return temp_init_from(reg, typ, unsafe {
            target.value_ptr().add(8 + (temp_get_size(typ) * idx) as usize)
        });
    }

    "arr-vec-set" [target, idx, val] {
        coretypck!(target ; VecArr);
        coretypck!(idx ; I64);
        let typ = super::arrvec_get_typ(target.clone());
        assert!(temp_base_sized_p(typ));
        // assert_eq!(typ, super::get_self_type(val));

        let idx = i64_get(idx) as u32;
        assert!(idx < super::arrvec_get_len(target.clone()));

        unsafe {
            std::ptr::copy_nonoverlapping(
                val.value_ptr(),
                target.value_ptr().add(8 + (temp_get_size(typ) * idx) as usize),
                temp_get_size(typ) as usize,
            )
        }

        return target;
    }

    "print" [arg] {
        let tbl = unsafe { ((*_thr).context()).symtab() };

        println!("{}", super::context(tbl, arg.clone()).to_string());
        return arg;
    }

    "dbg" [arg] {
        let tbl = unsafe { ((*_thr).context()).symtab() };

        println!("{}", super::context(tbl, arg.clone()).to_string());
        return arg;
    }

    // "printenv" 0 [] {
    //     println!("{}", super::context(_tbl, _env).to_string());
    //     return bool_init(_reg, false);
    // }

    "parse" [strin] {
        coretypck!(strin ; VecStr);
        let strsl = string_get(strin);

        let reg = unsafe { (*_thr).region() };
        let tbl = unsafe { ((*_thr).context()).symtab() };

        return match super::parser::parse(reg, tbl, strsl, false) {
            Ok(head) => head,
            Err(err) => super::errcode_init(reg, err),
        };
    }

    "_itsp_mdbg_id" [obj] {
        let id = obj.memdbg_obj_id();

        let reg = unsafe { (*_thr).region() };

        let out = super::i64_init(reg, id as _);

        out
    }

    "vec-push" [target, item] {
        coretypck!(target ; VecStd);

        super::stdvec_push(target.clone(), item);

        return target;
    }

    "vec-len" [target] {
        let reg = unsafe { (*_thr).region() };
        super::i64_init(reg, super::stdvec_get_len(target) as _)
    }

    "vec-get" [target, idx] {
        super::stdvec_idx(target, super::i64_get(idx) as _)
    }
}

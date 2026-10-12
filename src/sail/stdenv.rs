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

use super::{core::*, debug, memmgt, parser};

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
    ( const $array:ident; $vsl:ident $env:ident;
      $( $name:literal [ $($args:ident),* ] $body:block )+
    ) => {
        // compiler incorrectly warns on required 'mut $args'
        #[allow(unused_mut)]
        pub const $array: &[(&str, crate::sail::NativeFn, u16)] =
        &[$(($name, |
            _vsl: crate::sail::util::Hdl<crate::sail::thread::Vessel>,
            _env: crate::sail::SlHndl,
            _args: &[crate::sail::SlHndl],
            | {
                let $vsl = _vsl;
                let $env = _env;

                $(
                let mut $args = _args[${index()}].clone();
                )*

                $body
              },
              ${count($args)})),+];
    };

    ( let $array:ident; $vsl:ident $env:ident;
      $( $name:literal [ $($args:ident),* ] $body:block )+
    ) => {
        // compiler incorrectly warns on required 'mut $args'
        #[allow(unused_mut)]
        let $array: &[(&str, crate::sail::NativeFn, u16)] =
        &[$(($name, |
            _vsl: crate::sail::util::Hdl<crate::sail::thread::Vessel>,
            _env: crate::sail::SlHndl,
            _args: &[crate::sail::SlHndl],
            | {
                let $vsl = _vsl;
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
    _vsl _env;

    // TODO: use fixed point at times to avoid floating point errors?

    "+" [fst, snd] {
        let typ = fst.core_type();
        assert_eq!(typ, snd.core_type());

        let reg = _vsl.region();

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

        let reg = _vsl.region();

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

        let reg = _vsl.region();

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

        let reg = _vsl.region();

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
        let reg = _vsl.region();

        let out = i64_make(reg);
        let result = i64_get(fst) % i64_get(snd);
        i64_set(out.clone(), result);
        return out;
    }

    "neg" [val] {
        let reg = _vsl.region();

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
            env_lookup_by_id(_vsl.top_env(), super::S_T_INTERN.0).unwrap()
        } else {
            env_lookup_by_id(_vsl.top_env(), super::S_F_INTERN.0).unwrap()
        }
        // let reg = _vsl.region();
        // return bool_init(reg, result);
    }

    ">=" [fst, snd] {
        let result = i64_get(fst) >= i64_get(snd);

        if result {
            env_lookup_by_id(_vsl.top_env(), super::S_T_INTERN.0).unwrap()
        } else {
            env_lookup_by_id(_vsl.top_env(), super::S_F_INTERN.0).unwrap()
        }
        // let reg = _vsl.region();
        // return bool_init(reg, result);
    }

    "<=" [fst, snd] {
        let result = i64_get(fst) <= i64_get(snd);

        if result {
            env_lookup_by_id(_vsl.top_env(), super::S_T_INTERN.0).unwrap()
        } else {
            env_lookup_by_id(_vsl.top_env(), super::S_F_INTERN.0).unwrap()
        }
        // let reg = _vsl.region();
        // return bool_init(reg, result);
    }

    "eq" [fst, snd] {
        let result = core_eq(fst, snd);

        if result {
            env_lookup_by_id(_vsl.top_env(), super::S_T_INTERN.0).unwrap()
        } else {
            env_lookup_by_id(_vsl.top_env(), super::S_F_INTERN.0).unwrap()
        }
        // let reg = _vsl.region();
        // return bool_init(reg, result);
    }

    "not" [val] {
        if val.truthy() {
        env_lookup_by_id(_vsl.top_env(), super::S_F_INTERN.0).unwrap()
        } else {
        env_lookup_by_id(_vsl.top_env(), super::S_T_INTERN.0).unwrap()
        }
        // let reg = _vsl.region();
        // return bool_init(reg, !val.truthy());
    }

    // TODO: and and or as macros or, special forms for short-circuit
    "and" [lhs, rhs] {
        if lhs.truthy() && rhs.truthy() {
            env_lookup_by_id(_vsl.top_env(), super::S_T_INTERN.0).unwrap()
        } else {
            env_lookup_by_id(_vsl.top_env(), super::S_F_INTERN.0).unwrap()
        }
        // let reg = _vsl.region();
        // return bool_init(reg, lhs.truthy() && rhs.truthy());
    }

    "or" [lhs, rhs] {
        if lhs.truthy() || rhs.truthy() {
            env_lookup_by_id(_vsl.top_env(), super::S_T_INTERN.0).unwrap()
        } else {
            env_lookup_by_id(_vsl.top_env(), super::S_F_INTERN.0).unwrap()
        }
        // let reg = _vsl.region();
        // return bool_init(reg, lhs.truthy() || rhs.truthy());
    }

    "own-hdl" [] {
        let reg = _vsl.region();
        super::vsl_ref_init(reg, _vsl)
    }

    "vsl-spawn" [fun, args] {
        let reg = _vsl.region();
        let mut new = _vsl.spawn(None, None) ;

        let nr = new.region();

        let mbase = env_lookup_by_id(_vsl.top_env(), super::M_BASE.0) ;
        env_ins_by_id(nr, new.top_env(), super::M_BASE.0, mbase.unwrap());

        let to_apply = unsafe {
            SlHndl::from_raw_unchecked(super::structure_copy(nr, fun))
        };

        let mut av = vec![];
        let mut next = ref_get(args);
        while let Some(arg) = next {
            av.push(unsafe { SlHndl::from_raw_unchecked(super::structure_copy(nr, arg.clone())) });
            next = get_next_list_elt(arg);
        }

        new.load_proc_immed(to_apply, &av);

        if !new.attempt_start()  {
            panic!()
        }

        let new_hdl = super::vsl_ref_init(reg, new);

        new_hdl
    }

    "vsl-join" [vsref] {
        let reg = _vsl.region();
        super::vsl_ref_join(reg, vsref)
    }

    "vsl-id" [vsref] {
        let reg = _vsl.region();

        let vs_ref = super::vsl_ref_get(vsref);
        super::i64_init(reg, vs_ref.id() as _)
    }

    "qtx" [vsref, item] {
        let id = _vsl.id();

        super::vsl_ref_send(vsref, id, item);
        env_lookup_by_id(_vsl.top_env(), super::S_T_INTERN.0).unwrap()
        // let reg = _vsl.region();
        // return bool_init(reg, true);
    }

    "qrx" [] {
        let reg = _vsl.region();
        let msg = _vsl.msg_recv();

        match msg.1 {
            Some(r) => {
                let id = i64_init(reg, msg.0 as _);
                set_next_list_elt(_env, id.clone(), r);
                id
            }
            None => {
                env_lookup_by_id(_vsl.top_env(), super::S_F_INTERN.0).unwrap()
                // return bool_init(reg, false);
            }
        }
    }

    // TODO: adjust memmgt so we can efficiently get new bool objects
    // NOTE: 'empty list predicate'
    "elp" [loc] {
        if coretypp!(loc ; Ref) && !loc.nnil_ref_p() {
            env_lookup_by_id(_vsl.top_env(), super::S_T_INTERN.0).unwrap()
        } else {
            env_lookup_by_id(_vsl.top_env(), super::S_F_INTERN.0).unwrap()
        }
        // let reg = _vsl.region();
        // return bool_init(reg, coretypp!(loc ; Ref) && !loc.nnil_ref_p());
    }

    "push" [loc, item] {
        if coretypp!(loc ; Ref) {
            if let Some(tgt) = ref_get(loc.clone()) {
                set_next_list_elt(_env.clone(), item.clone(), tgt)
            }
            ref_set(_env, loc.clone(), item);
            loc
        } else {
            set_next_list_elt(_env, item.clone(), loc);
            item
        }
    }

    // TODO: automatic dereference? avoid handling first elt differently
    "fst" [loc] {
        if coretypp!(loc ; Ref) {
            ref_get(loc).expect("null ref!")
        } else {
            loc
        }
    }

    "rest" [loc] {
        let reg = _vsl.region();
        let loc = if coretypp!(loc ; Ref) {
            ref_get(loc).expect("null ref!")
        } else { loc };

        match get_next_list_elt(loc) {
            Some(h) => h,
            None => bool_init(reg, false),
        }
    }

    // TODO: variadic procedures
    "list" [arg] {
        ref_init(_vsl.region(), arg)
    }

    // TODO: automatically link following item
    "link" [fst, nxt] {
        set_next_list_elt(_env, fst.clone(), nxt);
        fst
    }

    "as-f32" [val] {
        let reg = _vsl.region();
        match val.core_type().expect("type invalid") {
            CoreType::F64 => return f32_init(reg, f64_get(val) as f32),
            CoreType::I64 => return f32_init(reg, i64_get(val) as f32),
            _ => panic!("type invalid for cast"),
        }
    }

    "alist-get" [loc, tgt] {
        let reg = _vsl.region();
        let lst = if coretypp!(loc ; Ref) {
            ref_get(loc).expect("null ref!")
        } else {
            loc
        };

        match alist_search(lst, tgt) {
            Some(c) => get_next_list_elt(c).unwrap(),
            None => bool_init(reg, false),
        }
    }

    "arr-vec-make" [typ, len, init] {
        coretypck!(typ ; Symbol);
        coretypck!(len ; I64);

        let typ = sym_get_id(typ);
        let len = i64_get(len) as u32;

        assert!(temp_base_sized_p(typ));
        // assert_eq!(typ, super::get_self_type(init));

        let reg = _vsl.region();

        unsafe {
            let size = vec_size(8, temp_get_size(typ), len);

            crate::report_site!("arr-vec-make alloc");
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

        let reg = _vsl.region();

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
        let tbl = (_vsl.context()).symtab() ;

        println!("{}", debug::bundle(tbl, arg.clone()).to_string());
        return arg;
    }

    "dbg" [arg] {
        let tbl = (_vsl.context()).symtab() ;

        println!("{}", debug::bundle(tbl, arg.clone()).to_string());
        return arg;
    }

    "parse" [strin] {
        coretypck!(strin ; VecStr);
        let strsl = string_get(strin);

        let reg = _vsl.region();
        let tbl =  (_vsl.context()).symtab() ;

        return match parser::parse_text(reg, tbl, Vec::from(strsl), None) {
            Ok(head) => head,
            Err(err) => debug::errcode_init(reg, err),
        };
    }

    "parse-prog" [strin] {
        coretypck!(strin ; VecStr);

        let reg = _vsl.region();
        let tbl = (_vsl.context()).symtab() ;
        let dbd = _vsl.src_dbg_info() ;

        return match parser::parse_obj(reg, tbl, strin, dbd) {
            Ok(head) => head,
            Err(err) => debug::errcode_init(reg, err),
        };
    }

    "parse-file" [path] {
        coretypck!(path ; VecStr);
        let pth = std::path::PathBuf::from(string_get(path));

        let reg = _vsl.region();
        let tbl = (_vsl.context()).symtab() ;
        let dbd = _vsl.src_dbg_info() ;

        return match parser::parse_file(reg, tbl, pth, dbd) {
            Ok(head) => head,
            Err(err) => debug::errcode_init(reg, err),
        };
    }

    "tmp-read-file" [path] {
        coretypck!(path ; VecStr);
        let pth = string_get(path);

        let reg = _vsl.region();

        let out = match std::fs::read_to_string(pth) {
            Ok(s) => super::string_init(reg, &s),
            Err(_) => debug::errcode_init(reg, super::debug::SlErrCode::FileCouldNotRead)
        };

        out
    }

    "_itsp_mdbg_id" [obj] {
        let id = obj.memdbg_obj_id();

        let reg = _vsl.region();

        let out = super::i64_init(reg, id as _);

        out
    }

    "str-new" [] {
        let reg = _vsl.region();
        super::string_make(reg, 0)
    }

    "vec-new" [] {
        let reg = _vsl.region();
        super::stdvec_make(reg, 0)
    }

    // TODO: implement type dispatch (separate logic from dispatch)
    // NOTE: already happening in some of these - is this the right way?
    "vec-push" [target, item] {
        if coretypp!(target ; VecStd) {
            super::stdvec_push(target.clone(), item);
        } else if coretypp!(target ; VecStr) {
            super::string_push(target.clone(), u8_get(item));
        }

        return target;
    }

    "vec-len" [target] {
        let reg = _vsl.region();
        i64_init(reg, stdvec_get_len(target) as _)
    }

    "vec-get" [target, idx] {
        stdvec_idx(target, i64_get(idx) as _)
    }

    "vec-set" [target, idx, val] {
        let idx = i64_get(idx) as u32;
        assert!(idx < stdvec_get_len(target.clone()));
        write_ptr(_env, target.clone(), 8 + (idx * 8), val);
        target
    }

    "tmp-vec-match" [target, item] {
        coretypck!(target ; VecStd);

        for i in 0..super::stdvec_get_len(target.clone()) {
            let this = super::stdvec_idx(target.clone(), i);
            if super::i64_get(this.clone()) == super::i64_get(item.clone()) {
                return match get_next_list_elt(this.clone()) {
                    Some(t) => t,
                    None => break,
                }
            }
        }
        env_lookup_by_id(_vsl.top_env(), super::S_F_INTERN.0).unwrap()

        // let reg = _vsl.region();
        // bool_init(reg, false)
    }

    "tmp-coord-log" [tgt] {
        coretypck!(tgt ; VecArr);

        let x = super::read_field::<f32>(tgt.clone(), 8);
        let y = super::read_field::<f32>(tgt.clone(), 12);

        log::debug!("sail | x: {x}, y: {y}");

        tgt
    }
}

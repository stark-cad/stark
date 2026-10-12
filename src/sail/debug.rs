// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2026 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/debug.rs

// Home of reflection, condition, error, logging, and miscellaneous
// debugging facilities for Sail.

// <>

use std::fmt;

use super::{Hdl, SlHndl, Stab, Styc};
use super::{core::*, memmgt};

// NOTE: at the moment we ONLY get parsed objects! (so no procs etc)
pub struct SrcInfo {
    // TODO: better specify & use Sail hash table
    table: std::collections::HashMap<u32, (u32, u32)>,
    sources: Vec<Source>,
}

impl SrcInfo {
    pub fn fresh() -> Self {
        Self {
            table: std::collections::HashMap::new(),
            sources: Vec::new(),
        }
    }

    pub fn add_src(&mut self, s: Source) -> u32 {
        self.sources.push(s);
        self.sources.len() as u32 - 1
    }

    pub fn ins_entry(&mut self, id: u32, from: u32, line: u32) {
        // NOTE: collisions shouldn't be possible in memdbg mode
        if let Some(oldval) = self.table.insert(id, (from, line)) {
            if cfg!(feature = "memdbg") {
                panic!("object IDs should never collide: {}", id)
            } else {
                eprintln!("M src dbg info: key collision: {oldval:?} replaced for {id}")
            }
        }
    }

    pub fn lookup(&self, id: u32) -> Option<(Source, u32)> {
        if let Some((sid, ln)) = self.table.get(&id) {
            Some((self.sources[*sid as usize].clone(), *ln))
        } else {
            None
        }
    }
}

// NOTE: might this enum have uses beyond debugging?
// NOTE: should some sources include a hash to notice changes?
#[derive(Clone)]
pub enum Source {
    File(std::path::PathBuf),
    Object(SlHndl),
    // TODO: identify codex and record id
    Record(()),
    // TODO: point to a particular REPL input stream?
    Stream(()),
    // TODO: write this text to a temporary file?
    Text(Vec<u8>),
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
fn errcode_make(reg: Hdl<memmgt::Region>) -> SlHndl {
    unsafe {
        crate::report_site!("error code obj alloc");
        let out = SlHndl::from_raw_unchecked(memmgt::alloc(reg, 2, memmgt::cap(Cfg::B2Err)));
        // write_field_unchecked::<u16>(ptr, 0, 0);
        out
    }
}

#[inline(always)]
pub fn errcode_init(reg: Hdl<memmgt::Region>, err: SlErrCode) -> SlHndl {
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

/// Bundles together an object and associated symbol table for display
/// DEPRECATED
pub struct SlWithCtx {
    tbl: Hdl<Stab>,
    obj: SlHndl,
}

/// Join a Sail value to its context for display
pub fn bundle(tbl: Hdl<Stab>, obj: SlHndl) -> SlWithCtx {
    SlWithCtx { tbl, obj }
}

// TODO: this is a mess; write Sail native display functions
// TODO: just push characters into a byte vector (string) for display
impl fmt::Display for SlWithCtx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut value = self.obj.clone();

        use CoreType::*;
        match value.core_type() {
            Some(t) => match t {
                Nil => write!(f, "()"),
                Bool => write!(f, "{}", if bool_get(value) { "#T" } else { "#F" }),
                U8 => write!(f, "#'{}", u8_get(value)),
                I64 => write!(f, "{}", i64_get(value)),
                U32 => write!(f, "{}", u32_get(value)),
                F64 => write!(f, "{}", f64_get(value)),
                F32 => write!(f, "{}", f32_get(value)),
                ErrCode => write!(f, "<err: {:?}>", errcode_get(value)),
                Symbol => {
                    let full_id = sym_get_id(value);
                    match mode_of_sym(full_id) {
                        SymbolMode::Basic => {}
                        SymbolMode::Keyword => write!(f, ":",).unwrap(),
                        SymbolMode::Module => write!(f, "@",).unwrap(),
                        SymbolMode::Type => write!(f, "$",).unwrap(),
                    }
                    write!(
                        f,
                        "{}",
                        match self.tbl.lookup_by_id(demodes_sym(full_id)) {
                            Some(n) => unsafe { std::str::from_utf8_unchecked(n) },
                            None => "<none>",
                        }
                    )
                }
                // TODO: procedures to output any type as string from
                // the body data or from the object itself
                VecPath => {
                    let len = sympath_get_len(value.clone());
                    for idx in 0..len {
                        let full_id = sympath_idx(value.clone(), idx);
                        match mode_of_sym(full_id) {
                            SymbolMode::Basic => {}
                            SymbolMode::Keyword => write!(f, ":",).unwrap(),
                            SymbolMode::Module => write!(f, "@",).unwrap(),
                            SymbolMode::Type => write!(f, "$",).unwrap(),
                        }
                        write!(
                            f,
                            "{}",
                            match self.tbl.lookup_by_id(demodes_sym(full_id)) {
                                Some(n) => unsafe { std::str::from_utf8_unchecked(n) },
                                None => "<none>",
                            }
                        )
                        .unwrap();
                        if idx < len - 1 {
                            write!(f, "/").unwrap()
                        }
                    }
                    Ok(())
                }
                Ref => {
                    write!(f, "(").unwrap();
                    let mut elt = ref_get(value);
                    while let Some(next) = elt {
                        write!(f, "{}", bundle(self.tbl, next.clone()).to_string()).unwrap();
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
                            bundle(self.tbl, stdvec_idx(value.clone(), idx)).to_string()
                        )
                        .unwrap();
                        if idx < len - 1 {
                            write!(f, " ").unwrap();
                        }
                    }
                    write!(f, "]")
                }
                VecStr => write!(f, "\"{}\"", string_get(value)),
                // TODO: this fails when called on nontrivial maps
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
                                bundle(self.tbl, ref_get(next.clone()).unwrap()).to_string()
                            )
                            .unwrap();
                            write!(
                                f,
                                "{}",
                                bundle(
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
                VecArr => {
                    let typ = super::arrvec_get_typ(value.clone());
                    if typ == super::T_F32.0 {
                        write!(f, "#[").unwrap();
                        let len = super::arrvec_get_len(value.clone());
                        for idx in 0..len {
                            let val = read_field::<f32>(value.clone(), 8 + (4 * idx));
                            write!(f, "{}", val).unwrap();
                            if idx < len - 1 {
                                write!(f, " ").unwrap();
                            }
                        }
                        write!(f, "]")
                    } else {
                        write!(f, "<$arr-vec>")
                    }
                }
                ProcLambda | ProcNative => write!(f, "<$proc>"),
                _ => write!(f, "<@core/$other>"),
            },
            None => {
                println!("{:?}", value.cfg_spec());
                write!(f, "<$other>")
            }
        }
    }
}

pub fn env_print(tbl: Hdl<Stab>, top: SlHndl) {
    let mut acc: Vec<(u32, SlHndl)> = vec![];
    let mut layer_ptr = Some(top);

    'layer: loop {
        if layer_ptr.is_none() {
            break 'layer;
        }

        let layer = layer_ptr.as_mut().unwrap();
        coretypck!(layer ; EnvLayer);

        'entry: for o in 0..ENV_LAYER_SLOTS {
            let byte_offset = o * (SYMBOL_LEN + PTR_LEN);

            let slot_id: u32 =
                unsafe { read_field_unchecked(layer_ptr.clone().unwrap(), byte_offset) };

            if slot_id >> SYM_ID_BIT_WIDTH == SymbolMode::Keyword as u32 {
                continue 'entry;
            }

            acc.push((slot_id, unsafe {
                read_ptr_unchecked(layer_ptr.clone().unwrap(), byte_offset + SYMBOL_LEN).unwrap()
            }));
        }

        layer_ptr = get_next_list_elt(layer_ptr.unwrap());
    }

    for entry in acc {
        // TODO: allow for type & mod symbols / reapply sigils
        let symn = unsafe { std::str::from_utf8_unchecked(tbl.lookup_by_id(entry.0).unwrap()) };
        let spac = if symn.len() > 6 { "\t" } else { "\t\t" };
        let valt = bundle(tbl, entry.1.clone()).to_string();
        println!("{symn} {spac}| {valt}");
    }
}

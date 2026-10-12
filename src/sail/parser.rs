// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/parser.rs

// Recursive descent parser which converts string slices into Sail
// objects, usually for evaluation.

// <>

use super::{Hdl, Stab, core::*, debug, memmgt};

use std::iter;
use std::str;

struct ParseState<'a> {
    acc: Vec<u8>,
    reg: Hdl<memmgt::Region>,
    tbl: Hdl<Stab>,
    src: u32,
    ln: u32,
    dbg: Option<&'a mut debug::SrcInfo>,
    cursor: iter::Peekable<iter::Copied<std::slice::Iter<'a, u8>>>,
}

pub fn wrap_prog(code: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(code.len() + 6);
    out.extend(b"(do\n");
    out.extend(code);
    out.extend(b"\n)");
    out
}

fn parse_inner(
    reg: Hdl<memmgt::Region>,
    tbl: Hdl<Stab>,
    src: Option<debug::Source>,
    mut dbd: Option<&mut debug::SrcInfo>,
    text: Vec<u8>,
) -> Result<SlHndl, debug::SlErrCode> {
    let src = if let Some(d) = dbd.as_deref_mut() {
        d.add_src(src.expect("all or nothing"))
    } else {
        u32::MAX
    };

    let mut init = ParseState {
        acc: Vec::new(),
        reg,
        tbl,
        src,
        ln: 0,
        dbg: dbd,
        cursor: text.iter().copied().peekable(),
    };

    let val = read_value(&mut init)?;

    Ok(val)
}

// TODO: should the actual source handling happen elsewhere?
pub fn parse_text(
    reg: Hdl<memmgt::Region>,
    tbl: Hdl<Stab>,
    code: Vec<u8>,
    dbg: Option<&mut debug::SrcInfo>,
) -> Result<SlHndl, debug::SlErrCode> {
    parse_inner(reg, tbl, Some(debug::Source::Text(code.clone())), dbg, code)
}

pub fn parse_obj(
    reg: Hdl<memmgt::Region>,
    tbl: Hdl<Stab>,
    mut loc: SlHndl,
    dbg: Option<&mut debug::SrcInfo>,
) -> Result<SlHndl, debug::SlErrCode> {
    coretypck!(loc ; VecStr);
    let code = string_get(loc.clone());
    parse_inner(
        reg,
        tbl,
        Some(debug::Source::Object(loc)),
        dbg,
        wrap_prog(code.as_bytes()),
    )
}

pub fn parse_file(
    reg: Hdl<memmgt::Region>,
    tbl: Hdl<Stab>,
    fpa: std::path::PathBuf,
    dbg: Option<&mut debug::SrcInfo>,
) -> Result<SlHndl, debug::SlErrCode> {
    let code = std::fs::read(&fpa).map_err(|_| debug::SlErrCode::FileCouldNotRead)?;
    parse_inner(
        reg,
        tbl,
        Some(debug::Source::File(fpa)),
        dbg,
        wrap_prog(&code),
    )
}

// NOTE: keep in mind =next_if_map= and =next_if_map_mut= on Peekable

/// Returns the head of a Sail object structure representing a single
/// item parsed from the input stream
///
/// This is a recursive descent parser; the appropriate reader can
/// almost always be deduced from the first character
fn read_value(s: &mut ParseState) -> Result<SlHndl, debug::SlErrCode> {
    let value;

    let mut in_comment = false;
    loop {
        match *(s
            .cursor
            .peek()
            .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?)
        {
            b';' => {
                in_comment = true;
                s.cursor.next();
            }
            b'\n' => {
                in_comment = false;
                s.ln += 1;
                s.cursor.next();
            }
            c if in_comment || c.is_ascii_whitespace() => {
                s.cursor.next();
            }
            _ => break,
        }
    }

    // let mut c = **(chars.peek().ok_or(debug::SlErrCode::ParseUnexpectedEnd)?);
    // while c.is_ascii_whitespace() || c == b';' {
    //     if c == b';' {
    //         while **(chars.peek().ok_or(debug::SlErrCode::ParseUnexpectedEnd)?) != b'\n' {
    //             chars.next();
    //         }
    //     }
    //     chars.next();
    //     c = **(chars.peek().ok_or(debug::SlErrCode::ParseUnexpectedEnd)?);
    // }

    match *(s
        .cursor
        .peek()
        .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?)
    {
        b'\'' => {
            s.cursor.next();
            value = read_quote(s)?;
        }
        b'(' => {
            s.cursor.next();
            value = read_list(s)?;
        }
        b'[' => {
            s.cursor.next();
            value = read_vec(s)?;
        }
        b'{' => {
            s.cursor.next();
            value = read_map(s)?;
        }
        b'"' => {
            s.cursor.next();
            value = read_string(s)?;
            s.acc.clear();
        }
        b'#' => {
            s.cursor.next();
            value = read_special(s)?;
            s.acc.clear();
        }
        b'+' | b'-' => {
            s.acc.push(s.cursor.next().unwrap());
            if s.cursor
                .peek()
                .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?
                .is_ascii_digit()
            {
                value = read_number(s)?;
            } else {
                value = read_sym(s)?;
            }
            s.acc.clear();
        }
        b':' | b'$' | b'@' | b'*' | b'/' | b'<' | b'=' | b'>' | b'_' => {
            value = read_sym(s)?;
            s.acc.clear();
        }
        c if c.is_ascii_alphabetic() => {
            value = read_sym(s)?;
            s.acc.clear();
        }
        c if c.is_ascii_digit() => {
            value = read_number(s)?;
            s.acc.clear();
        }
        _ => {
            return Err(debug::SlErrCode::ParseInvalidChar);
        }
    }

    if let Some(d) = s.dbg.as_mut() {
        let did = if cfg!(feature = "memdbg") {
            unsafe { value.memdbg_obj_id_immed() }
        } else {
            (unsafe { value.get_raw() } as usize & u32::MAX as usize) as u32
        };

        d.ins_entry(did, s.src, s.ln)
    }

    Ok(value)
}

/// Reads a quoted expression off the input stream, into the
/// appropriate object structure
fn read_quote(s: &mut ParseState) -> Result<SlHndl, debug::SlErrCode> {
    let start = sym_init(s.reg, super::SP_QUOTE.0);
    let head = ref_init(s.reg, start.clone());

    let end = read_value(s)?;
    unsafe {
        inc_refc(end.get_raw());
        set_next_list_elt_unsafe_unchecked(start, end);
    }

    Ok(head)
}

/// Reads a list of values from the input stream and creates a
/// corresponding list of Sail objects
fn read_list(s: &mut ParseState) -> Result<SlHndl, debug::SlErrCode> {
    let head = ref_make(s.reg);

    let mut c = *(s
        .cursor
        .peek()
        .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?);

    let mut count = 0;
    let mut tail = head.clone();

    while c != b')' {
        match c {
            b';' => {
                while s.cursor.next().unwrap_or(b'\n') != b'\n' {}
                s.ln += 1;
            }
            b'\n' => {
                s.cursor.next();
                s.ln += 1;
            }
            _ if c.is_ascii_whitespace() => {
                s.cursor.next();
            }
            _ => {
                // append to the list tail
                tail = {
                    let next = read_value(s)?;
                    unsafe {
                        inc_refc(next.get_raw());
                        if count < 1 {
                            write_ptr_unsafe_unchecked(tail, 0, next.clone())
                        } else {
                            set_next_list_elt_unsafe_unchecked(tail, next.clone())
                        }
                    }
                    next
                };

                count += 1;
            }
        }

        c = *(s
            .cursor
            .peek()
            .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?);
    }

    s.cursor.next();
    Ok(head)
}

// TODO: lists may need to be evaluated even if they appear in a vec or map
// TODO: tighter parser-evaluator integration likely necessary for this & symbols

/// Reads a vector from the input stream and creates the corresponding
/// Sail object
fn read_vec(s: &mut ParseState) -> Result<SlHndl, debug::SlErrCode> {
    let mut tvc = vec![];
    let mut c = *(s
        .cursor
        .peek()
        .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?);
    while c != b']' {
        match c {
            b';' => {
                while s.cursor.next().unwrap_or(b'\n') != b'\n' {}
                s.ln += 1;
            }
            b'\n' => {
                s.cursor.next();
                s.ln += 1;
            }
            _ if c.is_ascii_whitespace() => {
                s.cursor.next();
            }
            _ => tvc.push(read_value(s)?),
        }
        c = *(s
            .cursor
            .peek()
            .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?);
    }
    let vec = stdvec_init(s.reg, &tvc.into_boxed_slice());
    s.cursor.next();
    Ok(vec)
}

/// Reads an associative map from the input stream and creates the
/// corresponding Sail object
fn read_map(s: &mut ParseState) -> Result<SlHndl, debug::SlErrCode> {
    let map = hashvec_make(s.reg, 16);
    let mut c = *(s
        .cursor
        .peek()
        .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?);
    while c != b'}' {
        match c {
            b';' => {
                while s.cursor.next().unwrap_or(b'\n') != b'\n' {}
                s.ln += 1;
            }
            b'\n' => {
                s.cursor.next();
                s.ln += 1;
            }
            _ if c.is_ascii_whitespace() => {
                s.cursor.next();
            }
            _ => hash_map_insert(s.reg, map.clone(), read_value(s)?, read_value(s)?),
        }
        c = *(s
            .cursor
            .peek()
            .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?);
    }
    s.cursor.next();
    Ok(map)
}

/// Reads any symbol or sympath from the input stream and creates the
/// indicated Sail object
fn read_sym(s: &mut ParseState) -> Result<SlHndl, debug::SlErrCode> {
    let mut stack: Vec<u32> = vec![];
    let mut variety = SymbolMode::Basic;

    while {
        let peek = s.cursor.peek().unwrap_or(&b' ');
        match peek {
            b')' | b']' | b'}' => false,
            _ if peek.is_ascii_whitespace() => false,
            _ => true,
        }
    } {
        // if character is '/', push id to stack, clear acc, reset
        // variety, and accept sigil if next character
        let next = s.cursor.next().unwrap();
        match next {
            b'/' if s.acc.is_empty() => s.acc.push(next),
            b'/' => {
                stack.push(super::modeize_sym(s.tbl.get_id(s.acc.as_slice()), variety));
                s.acc.clear();
                variety = SymbolMode::Basic;
            }
            b':' if s.acc.is_empty() && variety == SymbolMode::Basic => {
                variety = SymbolMode::Keyword
            }
            b'@' if s.acc.is_empty() && variety == SymbolMode::Basic => {
                variety = SymbolMode::Module
            }
            b'$' if s.acc.is_empty() && variety == SymbolMode::Basic => variety = SymbolMode::Type,
            // sigils are not allowed within symbols
            b':' | b'@' | b'$' => return Err(debug::SlErrCode::ParseInvalidChar),
            // symbols cannot start with a digit
            b'0'..=b'9' if s.acc.is_empty() => return Err(debug::SlErrCode::ParseInvalidChar),
            b'*' | b'+' | b'-' | b'<' | b'=' | b'>' | b'?' => s.acc.push(next),
            _ if next.is_ascii_alphanumeric() => s.acc.push(next),
            _ => return Err(debug::SlErrCode::ParseInvalidChar),
        }
    }

    // no empty symbols or trailing slashes permitted
    if s.acc.is_empty() {
        return Err(debug::SlErrCode::ParseUnexpectedEnd);
    }

    stack.push(super::modeize_sym(s.tbl.get_id(s.acc.as_slice()), variety));

    let out = if stack.len() == 1 {
        sym_init(s.reg, stack[0])
    } else {
        sympath_init(s.reg, &stack)
    };

    Ok(out)
}

/// Reads a string from the input stream and creates its Sail object
fn read_string(s: &mut ParseState) -> Result<SlHndl, debug::SlErrCode> {
    let mut next = *(s
        .cursor
        .peek()
        .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?);
    while next != b'"' {
        s.acc.push(s.cursor.next().unwrap());
        next = *(s
            .cursor
            .peek()
            .ok_or(debug::SlErrCode::ParseUnexpectedEnd)?);
    }

    s.cursor.next();

    let string = string_init(
        s.reg,
        match str::from_utf8(&s.acc) {
            Ok(s) => s,
            _ => return Err(debug::SlErrCode::ParseInvalidString),
        },
    );

    Ok(string)
}

/// Reads a number from the input stream and creates its Sail object
fn read_number(s: &mut ParseState) -> Result<SlHndl, debug::SlErrCode> {
    while {
        let peek = s.cursor.peek().unwrap_or(&&b' ');
        match peek {
            b')' | b']' | b'}' => false,
            _ if peek.is_ascii_whitespace() => false,
            _ => true,
        }
    } {
        let next = s.cursor.next().unwrap();
        match next {
            b'+' | b'-' | b'_' | b'.' => s.acc.push(next),
            _ if next.is_ascii_alphanumeric() => s.acc.push(next),
            _ => {
                return Err(debug::SlErrCode::ParseInvalidChar);
            }
        }
    }
    process_num(unsafe { str::from_utf8_unchecked(&s.acc) }, s.reg)
}

/// Reads a special item from the input stream and creates a Sail
/// object if appropriate
fn read_special(s: &mut ParseState) -> Result<SlHndl, debug::SlErrCode> {
    while {
        let peek = s.cursor.peek().unwrap_or(&b' ');
        match peek {
            b')' | b']' | b'}' => false,
            _ if peek.is_ascii_whitespace() => false,
            _ => true,
        }
    } {
        let next = s.cursor.next().unwrap();
        match next {
            b'_' | b'\'' => s.acc.push(next),
            _ if next.is_ascii_alphanumeric() => s.acc.push(next),
            _ => {
                return Err(debug::SlErrCode::ParseInvalidChar);
            }
        }
    }

    if s.acc.is_empty() {
        return Err(debug::SlErrCode::ParseUnexpectedEnd);
    }

    if s.acc[0].eq_ignore_ascii_case(&b't') && s.acc.len() == 1 {
        Ok(bool_init(s.reg, true))
    } else if s.acc[0].eq_ignore_ascii_case(&b'f') && s.acc.len() == 1 {
        Ok(bool_init(s.reg, false))
    } else if s.acc[0].eq(&b'\'') && s.acc.len() == 2 {
        Ok(u8_init(s.reg, s.acc[1]))
    } else {
        Err(debug::SlErrCode::ParseBadSpecial)
    }
}

/// Parses a number and creates an object according to its textual
/// representation
fn process_num(slice: &str, reg: Hdl<memmgt::Region>) -> Result<SlHndl, debug::SlErrCode> {
    if let Ok(n) = slice.parse::<i64>() {
        Ok(i64_init(reg, n))
    } else if let Ok(n) = slice.parse::<f64>() {
        Ok(f64_init(reg, n))
    } else {
        Err(debug::SlErrCode::ParseInvalidNum)
    }
}

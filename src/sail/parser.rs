// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License. See the top level LICENSE file for the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/sail/parser.rs

// Recursive descent parser which converts string slices into Sail
// values, usually for evaluation.

// <>

use super::{core::*, memmgt, SlErrCode, SlHead};

use std::iter;
use std::str;

// struct Parser {
//     chars: iter::Peekable<str::Bytes<'static>>,
//     acc: Vec<u8>,
// }

/// Parses a Sail expression into the internal representation
pub fn parse(
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
    code: &str,
) -> Result<*mut SlHead, SlErrCode> {
    // Accumulator for collecting string values
    let mut acc: Vec<u8> = Vec::new();
    let mut chars = code.bytes().peekable();

    let val = read_value(&mut chars, &mut acc, reg, tbl)?;

    Ok(val)
}

// pub fn parse_bytes(tbl: *mut SlHead, code: &[u8]) -> Result<*mut SlHead, SlErrCode> {
//     let mut acc: Vec<u8> = Vec::new();
//     let mut chars = code.iter().peekable();

//     read_value(&mut chars, &mut acc, tbl, false)
// }

/// Returns a contiguous value parsed from the input stream
/// The appropriate reader can almost always be deduced from the first character
fn read_value(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SlErrCode> {
    let value;

    let mut c = *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?);
    while c.is_ascii_whitespace() || c == b';' {
        if c == b';' {
            while *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?) != b'\n' {
                chars.next();
            }
        }
        chars.next();
        c = *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?);
    }

    match c {
        b'\'' => {
            chars.next();
            value = read_quote(chars, acc, reg, tbl)?;
        }
        b'(' => {
            chars.next();
            value = read_list(chars, acc, reg, tbl)?;
        }
        b'[' => {
            chars.next();
            value = read_vec(chars, acc, reg, tbl)?;
        }
        b'{' => {
            chars.next();
            value = read_map(chars, acc, reg, tbl)?;
        }
        b':' => {
            chars.next();
            value = read_spec_sym(chars, acc, reg, tbl, SymbolMode::Keyword)?;
            acc.clear();
        }
        b'$' => {
            chars.next();
            value = read_spec_sym(chars, acc, reg, tbl, SymbolMode::Type)?;
            acc.clear();
        }
        b'"' => {
            chars.next();
            value = read_string(chars, acc, reg, tbl)?;
            acc.clear();
        }
        b'#' => {
            chars.next();
            value = read_special(chars, acc, reg, tbl)?;
            acc.clear();
        }
        b'+' | b'-' => {
            acc.push(chars.next().unwrap());
            if chars
                .peek()
                .ok_or(SlErrCode::ParseUnexpectedEnd)?
                .is_ascii_digit()
            {
                value = read_number(chars, acc, reg, tbl)?;
            } else {
                value = read_symbol(chars, acc, reg, tbl)?;
            }
            acc.clear();
        }
        b'*' | b'/' | b'<' | b'=' | b'>' | b'_' => {
            value = read_symbol(chars, acc, reg, tbl)?;
            acc.clear();
        }
        _ if c.is_ascii_alphabetic() => {
            value = read_symbol(chars, acc, reg, tbl)?;
            acc.clear();
        }
        _ if c.is_ascii_digit() => {
            value = read_number(chars, acc, reg, tbl)?;
            acc.clear();
        }
        _ => {
            return Err(SlErrCode::ParseInvalidChar);
        }
    }
    Ok(value)
}

fn read_quote(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SlErrCode> {
    let start = sym_init(reg, super::SP_QUOTE.0);
    let head = ref_init(reg, start);

    let end = read_value(chars, acc, reg, tbl)?;
    set_next_list_elt(start, end);

    Ok(head)
}

fn read_list(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SlErrCode> {
    let head = ref_make(reg);

    let mut c = *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?);
    if c == b')' {
        chars.next();
        return Ok(head);
    }

    let mut count = 0;
    let mut tail = head;

    while c != b')' {
        match c {
            b';' => while chars.next().unwrap_or(b'\n') != b'\n' {},
            _ if c.is_ascii_whitespace() => {
                chars.next();
            }
            _ => {
                // append to the list tail
                tail = {
                    let next = read_value(chars, acc, reg, tbl)?;
                    if count < 1 {
                        ref_set(tail, next)
                    } else {
                        set_next_list_elt(tail, next)
                    }
                    next
                };

                count += 1;
            }
        }

        c = *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?);
    }

    chars.next();
    Ok(head)
}

// TODO: What about lists, which need to be evaluated even if they appear in a vec or map
// TODO: Tighter integration between parser and evaluator likely necessary for this & symbols
fn read_vec(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SlErrCode> {
    // TODO: allow vectors of arbitrary length
    let vec = stdvec_make(reg, 8);
    let mut c = *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?);
    while c != b']' {
        match c {
            b';' => while chars.next().unwrap_or(b'\n') != b'\n' {},
            _ if c.is_ascii_whitespace() => {
                chars.next();
            }
            _ => stdvec_push(vec, read_value(chars, acc, reg, tbl)?),
        }
        c = *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?);
    }
    chars.next();
    Ok(vec)
}

fn read_map(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SlErrCode> {
    let map = hashvec_make(reg, 16);
    let mut c = *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?);
    while c != b'}' {
        match c {
            b';' => while chars.next().unwrap_or(b'\n') != b'\n' {},
            _ if c.is_ascii_whitespace() => {
                chars.next();
            }
            _ => hash_map_insert(
                reg,
                map,
                read_value(chars, acc, reg, tbl)?,
                read_value(chars, acc, reg, tbl)?,
            ),
        }
        c = *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?);
    }
    chars.next();
    Ok(map)
}

fn read_symbol(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SlErrCode> {
    while {
        let peek = chars.peek().unwrap_or(&b' ');
        match peek {
            b')' | b']' | b'}' => false,
            _ if peek.is_ascii_whitespace() => false,
            _ => true,
        }
    } {
        let next = chars.next().unwrap();
        match next {
            b'!' | b'*' | b'+' | b'-' | b'/' | b'<' | b'=' | b'>' | b'?' | b'_' => acc.push(next),
            _ if next.is_ascii_alphanumeric() => acc.push(next),
            _ => {
                return Err(SlErrCode::ParseInvalidChar);
            }
        }
    }

    let sym = sym_init(
        reg,
        super::sym_tab_get_id(reg, tbl, unsafe {
            str::from_utf8_unchecked(acc.as_slice())
        }),
    );

    Ok(sym)
}

fn read_spec_sym(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
    mode: SymbolMode,
) -> Result<*mut SlHead, SlErrCode> {
    while {
        let peek = chars.peek().unwrap_or(&b' ');
        match peek {
            b')' | b']' | b'}' => false,
            _ if peek.is_ascii_whitespace() => false,
            _ => true,
        }
    } {
        let next = chars.next().unwrap();
        match next {
            b'-' | b'_' => acc.push(next),
            _ if next.is_ascii_alphanumeric() => acc.push(next),
            _ => {
                return Err(SlErrCode::ParseInvalidChar);
            }
        }
    }

    if acc.len() == 0 {
        return Err(SlErrCode::ParseUnexpectedEnd);
    }

    let sym = unsafe {
        sym_init(
            reg,
            super::modeize_sym(
                sym_tab_get_id(reg, tbl, str::from_utf8_unchecked(acc.as_slice())),
                mode,
            ),
        )
    };

    Ok(sym)
}

fn read_string(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SlErrCode> {
    let mut next = *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?);
    while next != b'"' {
        acc.push(chars.next().unwrap());
        next = *(chars.peek().ok_or(SlErrCode::ParseUnexpectedEnd)?);
    }

    chars.next();

    let string = string_init(
        reg,
        match str::from_utf8(&acc) {
            Ok(s) => s,
            _ => return Err(SlErrCode::ParseInvalidString),
        },
    );

    Ok(string)
}

fn read_number(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SlErrCode> {
    while {
        let peek = chars.peek().unwrap_or(&b' ');
        match peek {
            b')' | b']' | b'}' => false,
            _ if peek.is_ascii_whitespace() => false,
            _ => true,
        }
    } {
        let next = chars.next().unwrap();
        match next {
            b'+' | b'-' | b'_' | b'.' => acc.push(next),
            _ if next.is_ascii_alphanumeric() => acc.push(next),
            _ => {
                return Err(SlErrCode::ParseInvalidChar);
            }
        }
    }
    process_num(unsafe { str::from_utf8_unchecked(acc) }, reg, tbl)
}

fn read_special(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SlErrCode> {
    while {
        let peek = chars.peek().unwrap_or(&b' ');
        match peek {
            b')' | b']' | b'}' => false,
            _ if peek.is_ascii_whitespace() => false,
            _ => true,
        }
    } {
        let next = chars.next().unwrap();
        match next {
            b'_' => acc.push(next),
            _ if next.is_ascii_alphanumeric() => acc.push(next),
            _ => {
                return Err(SlErrCode::ParseInvalidChar);
            }
        }
    }

    if acc.len() == 0 {
        return Err(SlErrCode::ParseUnexpectedEnd);
    }

    if acc[0].eq_ignore_ascii_case(&b't') && acc.len() == 1 {
        Ok(bool_init(reg, true))
    } else if acc[0].eq_ignore_ascii_case(&b'f') && acc.len() == 1 {
        Ok(bool_init(reg, false))
    } else {
        Err(SlErrCode::ParseBadSpecial)
    }
}

fn process_num(
    slice: &str,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SlErrCode> {
    if let Ok(n) = slice.parse::<i64>() {
        Ok(i64_init(reg, n))
    } else if let Ok(n) = slice.parse::<f64>() {
        Ok(f64_init(reg, n))
    } else {
        Err(SlErrCode::ParseInvalidNum)
    }
}

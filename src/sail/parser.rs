use super::{sym_set_id, sym_tab_get_id, SailErr, SlHead};

use std::iter;
use std::ptr;
use std::str;

struct Parser {
    chars: iter::Peekable<str::Bytes<'static>>,
    acc: Vec<u8>,
}

/// Parses a Sail expression into the internal representation
/// TODO: Parse Keyword
pub fn parse(tbl: *mut SlHead, code: &str) -> Result<*mut SlHead, SailErr> {
    // Accumulator for collecting string values
    let mut acc: Vec<u8> = Vec::new();
    let mut chars = code.bytes().peekable();

    let val = read_value(&mut chars, &mut acc, tbl, false)?;

    Ok(val)
}

/// Returns a contiguous value parsed from the input stream
/// The appropriate reader can almost always be deduced from the first character
fn read_value(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    tbl: *mut SlHead,
    elt: bool,
) -> Result<*mut SlHead, SailErr> {
    let value;

    let mut c = *(chars.peek().unwrap());
    while c.is_ascii_whitespace() {
        chars.next();
        c = *(chars.peek().ok_or(SailErr::Error)?);
    }

    match c {
        b'\'' => {
            chars.next();
            value = read_quote(chars, acc, tbl, elt)?;
        }
        b'(' => {
            chars.next();
            value = read_list(chars, acc, tbl, elt)?;
        }
        b'[' => {
            chars.next();
            value = read_vec(chars, acc, tbl, elt)?;
        }
        b'{' => {
            chars.next();
            value = read_map(chars, acc, tbl, elt)?;
        }
        b':' => {
            chars.next();
            value = read_keyword(chars, acc, tbl, elt)?;
            acc.clear();
        }
        b'"' => {
            chars.next();
            value = read_string(chars, acc, elt)?;
            acc.clear();
        }
        b'#' => {
            chars.next();
            value = read_special(chars, acc, elt)?;
            acc.clear();
        }
        b'+' | b'-' => {
            acc.push(chars.next().unwrap());
            if chars.peek().unwrap().is_ascii_digit() {
                value = read_number(chars, acc, elt)?;
            } else {
                value = read_symbol(chars, acc, tbl, elt)?;
            }
            acc.clear();
        }
        b'*' | b'/' | b'<' | b'=' | b'>' | b'_' => {
            value = read_symbol(chars, acc, tbl, elt)?;
            acc.clear();
        }
        _ if c.is_ascii_alphabetic() => {
            value = read_symbol(chars, acc, tbl, elt)?;
            acc.clear();
        }
        _ if c.is_ascii_digit() => {
            value = read_number(chars, acc, elt)?;
            acc.clear();
        }
        _ => {
            return Err(SailErr::Error);
        }
    }
    Ok(value)
}

fn read_quote(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    tbl: *mut SlHead,
    elt: bool,
) -> Result<*mut SlHead, SailErr> {
    unsafe {
        let head = super::init_list(elt);
        let start = super::init_symbol(true, super::SlSymbolMode::ById, 0);
        super::sym_set_id(start, super::sym_tab_get_id(tbl, "quote"));
        super::list_set(head, start);

        let end = read_value(chars, acc, tbl, true)?;
        super::set_list_elt(start, end);

        Ok(head)
    }
}

fn read_list(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    tbl: *mut SlHead,
    elt: bool,
) -> Result<*mut SlHead, SailErr> {
    let head = unsafe { super::init_list(elt) };

    let mut c = *(chars.peek().unwrap());

    if c == b')' {
        unsafe { super::list_set(head, ptr::null_mut()) }
        chars.next();
        return Ok(head);
    }

    let mut count = 0;
    let mut tail = head;

    while c != b')' {
        match c {
            b';' => while chars.next().unwrap() != b'\n' {},
            b'.' => {
                // may only appear immediately before the final element
                if count < 1 {
                    return Err(SailErr::Error);
                }

                // finish the malformed list
                chars.next().unwrap();
                let last = read_value(chars, acc, tbl, false)?;
                unsafe { super::set_list_elt(tail, last) }

                // make sure no illegal elements appear afterwards
                loop {
                    let nc = chars.next().unwrap();
                    match nc {
                        b')' => break,
                        _ if nc.is_ascii_whitespace() => (),
                        _ => return Err(SailErr::Error),
                    }
                }

                return Ok(head);
            }
            _ if c.is_ascii_whitespace() => {
                chars.next();
            }
            _ => {
                // append to the list tail
                tail = unsafe {
                    let next = read_value(chars, acc, tbl, true)?;
                    if count < 1 {
                        super::list_set(tail, next)
                    } else {
                        super::set_list_elt(tail, next)
                    }
                    next
                };

                count += 1;
            }
        }

        c = *(chars.peek().ok_or(SailErr::Error)?);
    }

    chars.next();
    Ok(head)
}

// TODO: What about lists, which need to be evaluated even if they appear in a vec or map
// TODO: Tighter integration between parser and evaluator likely necessary for this & symbols
fn read_vec(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    tbl: *mut SlHead,
    elt: bool,
) -> Result<*mut SlHead, SailErr> {
    let vec = unsafe { super::init_vec(elt, 8) };
    let mut c = *(chars.peek().unwrap());
    while c != b']' {
        match c {
            b';' => while chars.next().unwrap() != b'\n' {},
            _ if c.is_ascii_whitespace() => {
                chars.next();
            }
            _ => unsafe { super::vec_push(vec, read_value(chars, acc, tbl, false)?) },
        }
        c = *(chars.peek().unwrap());
    }
    chars.next();
    Ok(vec)
}

fn read_map(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    tbl: *mut SlHead,
    elt: bool,
) -> Result<*mut SlHead, SailErr> {
    let map = unsafe { super::init_map(elt, super::SlMapMode::Assoc, 16) };
    let mut c = *(chars.peek().unwrap());
    while c != b'}' {
        match c {
            b';' => while chars.next().unwrap() != b'\n' {},
            _ if c.is_ascii_whitespace() => {
                chars.next();
            }
            _ => {
                unsafe {
                    super::map_insert(
                        map,
                        read_value(chars, acc, tbl, false)?,
                        read_value(chars, acc, tbl, false)?,
                    )
                };
            }
        }
        c = *(chars.peek().unwrap());
    }
    chars.next();
    Ok(map)
}

fn read_symbol(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    tbl: *mut SlHead,
    elt: bool,
) -> Result<*mut SlHead, SailErr> {
    let sym = unsafe { super::init_symbol(elt, super::SlSymbolMode::ById, 0) };
    while {
        let peek = chars.peek().unwrap();
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
            _ => return Err(SailErr::Error),
        }
    }

    unsafe { super::sym_set_id(sym, sym_tab_get_id(tbl, str::from_utf8_unchecked(acc.as_slice()))) }

    Ok(sym)
}

fn read_keyword(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    tbl: *mut SlHead,
    elt: bool,
) -> Result<*mut SlHead, SailErr> {
    // TODO: figure out how keywords work and then fix this
    let key = unsafe { super::init_keyword(elt, 8) };
    while {
        let peek = chars.peek().unwrap();
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
            _ => return Err(SailErr::Error),
        }
    }
    Ok(key)
}

fn read_string(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    elt: bool,
) -> Result<*mut SlHead, SailErr> {
    let string = unsafe { super::init_string(elt, 32) };
    let mut next = *(chars.peek().unwrap());
    while next != b'"' {
        acc.push(chars.next().unwrap());
        next = *(chars.peek().unwrap());
    }

    unsafe {
        super::string_set(
            string,
            match str::from_utf8(&acc) {
                Ok(s) => s,
                _ => return Err(SailErr::Error),
            },
        )
    }

    Ok(string)
}

fn read_number(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    elt: bool,
) -> Result<*mut SlHead, SailErr> {
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
            _ => return Err(SailErr::Error),
        }
    }
    process_num(unsafe { str::from_utf8_unchecked(acc) }, elt)
}

fn read_special(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    elt: bool,
) -> Result<*mut SlHead, SailErr> {
    while {
        let peek = chars.peek().unwrap();
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
            _ => return Err(SailErr::Error),
        }
    }

    if acc.len() == 0 {
        return Err(SailErr::Error);
    }

    let val = unsafe { super::init_bool(elt) };
    if acc[0].eq_ignore_ascii_case(&b't') && acc.len() == 1 {
        unsafe { super::bool_set(val, true) }
        Ok(val)
    } else if acc[0].eq_ignore_ascii_case(&b'f') && acc.len() == 1 {
        unsafe { super::bool_set(val, false) }
        Ok(val)
    } else {
        return Err(SailErr::Error);
    }
}

fn process_num(slice: &str, elt: bool) -> Result<*mut SlHead, SailErr> {
    let val;
    if let Ok(n) = slice.parse::<i64>() {
        unsafe {
            val = super::init_fixint(elt);
            super::fixint_set(val, n);
        }
        return Ok(val);
    } else if let Ok(n) = slice.parse::<f64>() {
        unsafe {
            val = super::init_fixfloat(elt);
            super::fixfloat_set(val, n);
        }
        return Ok(val);
    } else {
        Err(SailErr::Error)
    }
}

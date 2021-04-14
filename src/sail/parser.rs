use super::{memmgt, SailErr, SlHead};

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
) -> Result<*mut SlHead, SailErr> {
    // Accumulator for collecting string values
    let mut acc: Vec<u8> = Vec::new();
    let mut chars = code.bytes().peekable();

    let val = read_value(&mut chars, &mut acc, reg, tbl)?;

    Ok(val)
}

// pub fn parse_bytes(tbl: *mut SlHead, code: &[u8]) -> Result<*mut SlHead, SailErr> {
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
) -> Result<*mut SlHead, SailErr> {
    let value;

    let mut c = *(chars.peek().unwrap());
    while c.is_ascii_whitespace() || c == b';' {
        if c == b';' {
            while *(chars.peek().unwrap()) != b'\n' {
                chars.next();
            }
        }
        chars.next();
        c = *(chars.peek().ok_or(SailErr::Error)?);
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
        // b':' => {
        //     chars.next();
        //     value = read_keyword(chars, acc, reg, tbl, elt)?;
        //     acc.clear();
        // }
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
            if chars.peek().unwrap().is_ascii_digit() {
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
            return Err(SailErr::Error);
        }
    }
    Ok(value)
}

fn read_quote(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    let head = super::init_ref(reg);
    let start = super::init_symbol(reg);
    super::sym_set_id(start, super::sym_tab_get_id(reg, tbl, "quote"));
    super::ref_set(head, start);

    let end = read_value(chars, acc, reg, tbl)?;
    super::set_next_list_elt(start, end);

    Ok(head)
}

fn read_list(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    let head = super::init_ref(reg);

    let mut c = *(chars.peek().unwrap());
    if c == b')' {
        chars.next();
        super::ref_set(head, super::nil());
        return Ok(head);
    }

    let mut count = 0;
    let mut tail = head;

    while c != b')' {
        match c {
            b';' => while chars.next().unwrap() != b'\n' {},
            // b'.' => {
            //     // may only appear immediately before the final element
            //     if count < 1 {
            //         return Err(SailErr::Error);
            //     }

            //     // finish the malformed list
            //     chars.next().unwrap();
            //     let last = read_value(chars, acc, reg, tbl)?;
            //     super::set_next_list_elt(tail, last);

            //     // make sure no illegal elements appear afterwards
            //     loop {
            //         let nc = chars.next().unwrap();
            //         match nc {
            //             b')' => break,
            //             _ if nc.is_ascii_whitespace() => (),
            //             _ => return Err(SailErr::Error),
            //         }
            //     }

            //     return Ok(head);
            // }
            _ if c.is_ascii_whitespace() => {
                chars.next();
            }
            _ => {
                // append to the list tail
                tail = {
                    let next = read_value(chars, acc, reg, tbl)?;
                    if count < 1 {
                        super::ref_set(tail, next)
                    } else {
                        super::set_next_list_elt(tail, next)
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
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    let vec = super::init_stdvec(reg, 8);
    let mut c = *(chars.peek().unwrap());
    while c != b']' {
        match c {
            b';' => while chars.next().unwrap() != b'\n' {},
            _ if c.is_ascii_whitespace() => {
                chars.next();
            }
            _ => super::stdvec_push(vec, read_value(chars, acc, reg, tbl)?),
        }
        c = *(chars.peek().unwrap());
    }
    chars.next();
    Ok(vec)
}

fn read_map(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    let map = super::init_hash_map(reg, 16);
    let mut c = *(chars.peek().unwrap());
    while c != b'}' {
        match c {
            b';' => while chars.next().unwrap() != b'\n' {},
            _ if c.is_ascii_whitespace() => {
                chars.next();
            }
            _ => super::hash_map_insert(
                reg,
                map,
                read_value(chars, acc, reg, tbl)?,
                read_value(chars, acc, reg, tbl)?,
            ),
        }
        c = *(chars.peek().unwrap());
    }
    chars.next();
    Ok(map)
}

fn read_symbol(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    let sym = super::init_symbol(reg);
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

    super::sym_set_id(
        sym,
        super::sym_tab_get_id(reg, tbl, unsafe {
            str::from_utf8_unchecked(acc.as_slice())
        }),
    );

    Ok(sym)
}

// TODO: make keywords work properly again
// fn read_keyword(
//     chars: &mut iter::Peekable<str::Bytes>,
//     acc: &mut Vec<u8>,
//     reg: *mut memmgt::Region,
//     tbl: *mut SlHead,
//     elt: bool,
// ) -> Result<*mut SlHead, SailErr> {
//     let key = unsafe { super::init_keyword(memmgt::which_mem_sector(tbl), elt) };
//     while {
//         let peek = chars.peek().unwrap();
//         match peek {
//             b')' | b']' | b'}' => false,
//             _ if peek.is_ascii_whitespace() => false,
//             _ => true,
//         }
//     } {
//         let next = chars.next().unwrap();
//         match next {
//             b'_' => acc.push(next),
//             _ if next.is_ascii_alphanumeric() => acc.push(next),
//             _ => return Err(SailErr::Error),
//         }
//     }

//     if acc.len() == 0 {
//         return Err(SailErr::Error);
//     }

//     unsafe {
//         super::key_set_id(
//             key,
//             super::sym_tab_get_id(tbl, str::from_utf8_unchecked(acc.as_slice())),
//         )
//     }

//     Ok(key)
// }

fn read_string(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    let string = super::init_string(reg, 32);
    let mut next = *(chars.peek().unwrap());
    while next != b'"' {
        acc.push(chars.next().unwrap());
        next = *(chars.peek().unwrap());
    }

    chars.next();

    super::string_set(
        string,
        match str::from_utf8(&acc) {
            Ok(s) => s,
            _ => return Err(SailErr::Error),
        },
    );

    Ok(string)
}

fn read_number(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
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
    process_num(unsafe { str::from_utf8_unchecked(acc) }, reg, tbl)
}

fn read_special(
    chars: &mut iter::Peekable<str::Bytes>,
    acc: &mut Vec<u8>,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
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
            b'_' => acc.push(next),
            _ if next.is_ascii_alphanumeric() => acc.push(next),
            _ => return Err(SailErr::Error),
        }
    }

    if acc.len() == 0 {
        return Err(SailErr::Error);
    }

    let val = super::init_bool(reg);
    if acc[0].eq_ignore_ascii_case(&b't') && acc.len() == 1 {
        super::bool_set(val, true);
        Ok(val)
    } else if acc[0].eq_ignore_ascii_case(&b'f') && acc.len() == 1 {
        super::bool_set(val, false);
        Ok(val)
    } else {
        return Err(SailErr::Error);
    }
}

fn process_num(
    slice: &str,
    reg: *mut memmgt::Region,
    tbl: *mut SlHead,
) -> Result<*mut SlHead, SailErr> {
    let val;
    if let Ok(n) = slice.parse::<i64>() {
        val = super::init_i64(reg);
        super::i64_set(val, n);

        return Ok(val);
    } else if let Ok(n) = slice.parse::<f64>() {
        val = super::init_f64(reg);
        super::f64_set(val, n);

        return Ok(val);
    } else {
        Err(SailErr::Error)
    }
}

//! The Structured Augmentation Interchange Language
//!
//! A custom Lisp dialect for writing STARK

use std::fmt;

struct _Cons {
    car: Value,
    cdr: Value,
}

// TODO: Figure out how to manage / dispose of memory
pub struct List {
    car: Value,
    cdr: *mut List,
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let car = self.car.to_string();
        let cdr = unsafe {
            match self.cdr.as_ref() {
                Some(x) => String::from(" ") + &x.to_string(),
                None => String::new(),
            }
        };
        write!(f, "{}{}", car, cdr)
    }
}

/// Container type for many possible value types
/// TODO: add support for arbitrary precision numbers (`rug` crate)
pub enum Value {
    // None == nil == empty list ()
    List(*mut List),

    True,
    False,

    Symbol(String),

    String(String),
    Integer(i32),
    Float(f32),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::List(p) => unsafe {
                match p.as_ref() {
                    Some(x) => write!(f, "({})", x),
                    None => write!(f, "()"),
                }
            },
            Value::True => write!(f, "#T"),
            Value::False => write!(f, "#F"),
            Value::Symbol(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::Integer(x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
        }
    }
}

pub fn repl() {}

/// Interprets a Sail expression, returning the result
pub fn interpret(code: &String) -> String {
    eval(parser::parse(code).unwrap()).to_string()
}

mod parser {
    use super::{List, Value};

    use std::ptr;
    use std::str;

    enum State {
        Neutral,
        Comment,
        Special,
        Symbol,
        String,
        Number,
    }

    /// Parses a Sail expression into the internal representation
    pub fn parse(code: &str) -> Result<Value, &str> {
        // Return value; mutated into an atom or list head
        let mut val = Value::False;

        // The parser is a state machine that tracks current context
        let mut state = State::Neutral;
        // Accumulator for collecting string values
        let mut acc: Vec<u8> = Vec::new();

        // Nesting level (number of subsequent `(` before a value)
        let mut nest: u32 = 0;
        // Stack of list tails; enables nesting linked lists
        let mut tails: Vec<*mut List> = vec![];

        // Adds a value to the list according to the current parse state
        macro_rules! add_value {
            ($value:expr) => {
                if nest > 0 {
                    // If the value is nested, a stack of pointers to lists must be built
                    let mut ptrs = vec![new_list($value)];
                    for _ in 1..nest {
                        ptrs.push(new_list(Value::List(*ptrs.last().unwrap())));
                    }

                    // Adds a node to the current list for the base of the nest
                    // If not inside a list already, sets the return value
                    if let Some(tail) = tails.pop() {
                        tails.push(add_node(tail, Value::List(*ptrs.last().unwrap())));
                    } else {
                        val = Value::List(*ptrs.last().unwrap());
                    }

                    // Pushes the nested tails to the tail stack in reverse
                    while let Some(ptr) = ptrs.pop() {
                        tails.push(ptr);
                    }

                    // Sets built up nesting level back to 0
                    nest = 0;
                } else {
                    // If the value is not nested, simply pushes it onto current list
                    if let Some(tail) = tails.pop() {
                        tails.push(add_node(tail, $value));
                    } else {
                        val = $value;
                    }
                }
            };
        }

        // Tracks whether the parser is finished consuming its input
        let mut finished = false;
        let mut chars = code.bytes();

        while !finished {
            // Adds whitespace to the end to ensure everything gets closed out
            let c = match chars.next() {
                Some(c) => c,
                None => {
                    finished = true;
                    b' '
                }
            };

            // Current state determines how new characters are handled
            match state {
                State::Neutral => match c {
                    b'(' => nest += 1,
                    b')' => {
                        if nest > 0 {
                            nest -= 1;
                            add_value!(Value::List(ptr::null_mut()));
                        } else {
                            tails.pop();
                        }
                    }
                    b';' => state = State::Comment,
                    b'"' => state = State::String,
                    b'#' => state = State::Special,
                    _ if c.is_ascii_alphabetic() || c.is_ascii_punctuation() => {
                        acc.push(c);
                        state = State::Symbol;
                    }
                    _ if c.is_ascii_digit() => {
                        acc.push(c);
                        state = State::Number;
                    }
                    _ if c.is_ascii_whitespace() => (),
                    _ => {
                        panic!();
                        // return Err("Unacceptable character");
                    }
                },
                State::Comment => match c {
                    b'\n' => state = State::Neutral,
                    _ => (),
                },
                State::Special => match c {
                    b')' => {
                        if acc[0].eq_ignore_ascii_case(&b't') && acc.len() == 1 {
                            add_value!(Value::True);
                        } else if acc[0].eq_ignore_ascii_case(&b'f') && acc.len() == 1 {
                            add_value!(Value::False);
                        } else {
                            return Err("Non-boolean specials not yet supported");
                        }

                        tails.pop();
                        acc.clear();
                        state = State::Neutral;
                    }
                    _ if c.is_ascii_whitespace() => {
                        if acc[0].eq_ignore_ascii_case(&b't') && acc.len() == 1 {
                            add_value!(Value::True);
                        } else if acc[0].eq_ignore_ascii_case(&b'f') && acc.len() == 1 {
                            add_value!(Value::False);
                        } else {
                            return Err("Non-boolean specials not yet supported");
                        }

                        acc.clear();
                        state = State::Neutral;
                    }
                    _ if c.is_ascii_alphanumeric() => acc.push(c),
                    _ => {
                        panic!();
                        // return Err("Unacceptable character");
                    }
                },
                State::Symbol => match c {
                    b')' => {
                        add_value!(Value::Symbol(String::from(str::from_utf8(&acc).unwrap())));

                        tails.pop();
                        acc.clear();
                        state = State::Neutral;
                    }
                    _ if c.is_ascii_whitespace() => {
                        add_value!(Value::Symbol(String::from(str::from_utf8(&acc).unwrap())));

                        acc.clear();
                        state = State::Neutral;
                    }
                    _ if c.is_ascii_alphanumeric() => acc.push(c),
                    _ => {
                        panic!();
                        // return Err("Unacceptable character");
                    }
                },
                State::String => match c {
                    b'"' => {
                        add_value!(Value::String(String::from(str::from_utf8(&acc).unwrap())));

                        acc.clear();
                        state = State::Neutral;
                    }
                    _ => acc.push(c),
                },
                // TODO: Don't examine any character more than once: parse inline
                State::Number => match c {
                    b')' => {
                        let result = process_num(str::from_utf8(&acc).unwrap());
                        if let Ok(value) = result {
                            add_value!(value);
                        } else {
                            return result;
                        }

                        tails.pop();
                        acc.clear();
                        state = State::Neutral;
                    }
                    _ if c.is_ascii_whitespace() => {
                        let result = process_num(str::from_utf8(&acc).unwrap());
                        if let Ok(value) = result {
                            add_value!(value);
                        } else {
                            return result;
                        }

                        acc.clear();
                        state = State::Neutral;
                    }
                    _ if c.is_ascii_alphanumeric() => acc.push(c),
                    _ if c.is_ascii_punctuation() => acc.push(c),
                    _ => {
                        panic!();
                        // return Err("Unacceptable character");
                    }
                },
            }
        }

        Ok(val)
    }

    fn process_num<'a, 'b>(slice: &'a str) -> Result<Value, &'b str> {
        if let Ok(n) = slice.parse::<i32>() {
            return Ok(Value::Integer(n));
        } else if let Ok(n) = slice.parse::<f32>() {
            return Ok(Value::Float(n));
        } else {
            Err("Invalid number")
        }
    }

    fn add_node(tail: *mut List, value: Value) -> *mut List {
        let ptr = new_list(value);
        unsafe { (*tail).cdr = ptr };
        ptr
    }

    fn new_list(value: Value) -> *mut List {
        Box::into_raw(Box::new(List {
            car: value,
            cdr: ptr::null_mut(),
        }))
    }
}

/// Evaluates a Sail value, returning the result
fn eval(value: Value) -> Value {
    value
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;

    #[test]
    fn returns() {
        let exp = String::from("42");
        assert_eq!(exp, interpret(&exp));
    }

    #[test]
    fn adds() {
        let exp = String::from("(+ 2 2)");
        assert_eq!("4", interpret(&exp));
    }

    #[test]
    fn parses() {
        let exp = String::from("(+ (() 42 (e) #T) #F 2.1)");
        let out = parser::parse(&exp).unwrap().to_string();
        assert_eq!(exp, out);

        let exp = String::from("(() (()) ((((() ())))))");
        let out = parser::parse(&exp).unwrap().to_string();
        assert_eq!(exp, out);

        let exp = String::from("((1 2 3 4) ;Comment\n5)");
        let gnd = String::from("((1 2 3 4) 5)");
        let out = parser::parse(&exp).unwrap().to_string();
        assert_eq!(gnd, out);
    }

    #[test]
    fn displays() {
        let list = Value::List(Box::into_raw(Box::new(List {
            car: Value::List(Box::into_raw(Box::new(List {
                car: Value::Integer(42),
                cdr: Box::into_raw(Box::new(List {
                    car: Value::True,
                    cdr: ptr::null_mut(),
                })),
            }))),
            cdr: Box::into_raw(Box::new(List {
                car: Value::String(String::from("the answer")),
                cdr: Box::into_raw(Box::new(List {
                    car: Value::List(ptr::null_mut()),
                    cdr: ptr::null_mut(),
                })),
            })),
        })));
        assert_eq!("((42 #T) \"the answer\" ())", list.to_string());
    }
}

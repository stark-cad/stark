//! The Structured Augmentation Interchange Language
//!
//! A custom Lisp dialect for writing STARK

use std::collections::HashMap;
use std::fmt;
use std::ptr;

mod parser;

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
    // TODO: Combine into a `Num` enum for future additions
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

pub struct Env {
    entries: HashMap<String, Value>,
    parent: *mut Env,
}

impl Default for Env {
    fn default() -> Env {
        Env {
            entries: HashMap::new(),
            parent: ptr::null_mut(),
        }
    }
}

pub fn repl() {}

/// Interprets a Sail expression, returning the result
pub fn interpret(code: &String) -> String {
    let env = Env::default();

    eval(&Env::default(), &parser::parse(code).unwrap()).unwrap().to_string()
}

/// Evaluates a Sail value, returning the result
fn eval<'a, 'b>(env: &'a Env, value: &'a Value) -> Result<&'a Value, &'b str> {
    Ok(match value {
        Value::List(p) => value,
        Value::Symbol(s) => value,
        Value::True => value,
        Value::False => value,
        Value::String(_) => value,
        Value::Integer(_) => value,
        Value::Float(_) => value,
    })
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

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

/// Interprets a Sail expression, returning the result
pub fn interpret(code: &String) -> String {

}

/// Parses a Sail expression into the internal list representation
fn parse(code: &String) -> Box<List> {

}

/// Evaluates a Sail list, returning the result
fn eval(list: Box<List>) -> Value {

}

#[cfg(test)]
mod tests {
    use super::*;

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
}

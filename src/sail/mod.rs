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

impl List {
    fn car(&mut self) -> &mut Value {
        &mut self.car
    }
    fn cdr(&mut self) -> &mut Value {
        unsafe { self.cdr.as_mut() }
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let car = self.car.to_string();
        let cdr = match unsafe { self.cdr.as_ref() } {
            Some(x) => String::from(" ") + &x.to_string(),
            None => String::new(),
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

impl Value {
    pub fn atom_p(&self) -> bool {
        if let Self::List(p) = self {
            p.is_null()
        } else {
            true
        }
    }
    pub fn list_p(&self) -> bool {
        if let Self::List(_) = self {
            true
        } else {
            false
        }
    }
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

pub struct Procedure {}

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

    eval(&Env::default(), &parser::parse(code).unwrap())
        .unwrap()
        .to_string()
}

/// Evaluates a Sail value, returning the result
fn eval<'a, 'b>(env: &'a Env, value: &'a Value) -> Result<&'a Value, &'b str> {
    if value.atom_p() {
        if let Value::Symbol(s) = value {
            return Ok(env.entries.get(s).unwrap());
        }
        return Ok(value);
    } else if let Value::List(p) = value {
        let car = unsafe { (**p).car() };

        if let Value::Symbol(s) = car {
            match s.as_str() {
                "def" => {}
                "fun" => {}
                "if" => {}
                "quote" => {}
                _ => {}
            }
        }

        Ok(&Value::False)
    } else {
        unreachable!();
    }
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

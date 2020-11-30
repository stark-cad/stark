//! The Structured Augmentation Interchange Language
//!
//! A custom Lisp dialect for writing STARK

use std::collections::HashMap;
use std::fmt;
use std::ptr;

mod parser;

pub enum SailErr {}

impl fmt::Display for SailErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error")
    }
}

impl fmt::Debug for SailErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error")
    }
}

pub trait SailVal {}

// TODO: Figure out how to manage / dispose of memory
pub struct List {
    car: Value,
    cdr: *mut List,
}

impl List {
    fn car(&self) -> Value {
        self.car.clone()
    }
    fn cdr(&self) -> Value {
        Value::List(self.cdr)
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

pub struct Cons {
    car: Value,
    cdr: Value,
}

impl fmt::Display for Cons {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let car = self.car.to_string();
        let cdr = self.cdr.to_string();
        write!(f, "({} . {})", car, cdr)
    }
}

/// Container type for many possible Sail value types
/// TODO: Switch to mostly SailVal trait bound to avoid excessive size
#[derive(Clone)]
pub enum Value {
    // None == nil == empty list ()
    List(*mut List),
    Cons(*mut Cons),

    Vec(Vec<Value>),
    Map(HashMap<Value, Value>),

    Lambda {
        args: Vec<String>,
        body: *mut List,
    },
    Native(fn(*mut Env, *mut List) -> Result<Value, SailErr>),

    // TODO: What are symbols and keywords, really?
    Symbol(String),
    Keyword(String),

    String(String),

    /// TODO: add support for arbitrary precision numbers (`rug` crate)
    FixInt(i64),
    FixFloat(f64),

    Bool(bool),
}

impl Value {
    pub fn atom_p(&self) -> bool {
        match self {
            Self::List(p) => p.is_null(),
            Self::Cons(p) => p.is_null(),
            _ => true,
        }
    }
    pub fn proc_p(&self) -> bool {
        match self {
            Self::Lambda { args: _, body: _ } => true,
            Self::Native(_) => true,
            _ => false,
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
            Value::Cons(p) => unsafe {
                match p.as_ref() {
                    Some(x) => write!(f, "{}", x),
                    None => write!(f, "()"),
                }
            },
            // TODO: The next three will have an extra space at the end; fix
            Value::Vec(x) => write!(
                f,
                "[{}]",
                x.iter()
                    .fold(String::new(), |acc, new| acc + &new.to_string() + " ")
            ),
            Value::Map(x) => write!(
                f,
                "{{{}}}",
                x.iter().fold(String::new(), |acc, new| acc
                    + &new.0.to_string()
                    + " "
                    + &new.1.to_string()
                    + " ")
            ),
            Value::Lambda { args, body } => write!(
                f,
                "<lambda fn: [{}] ({})>",
                args.iter().fold(String::new(), |acc, new| acc + new + " "),
                unsafe {
                    match body.as_ref() {
                        Some(x) => x.to_string(),
                        None => String::new(),
                    }
                }
            ),
            Value::Native(_) => write!(f, "<native fn>"),
            Value::Symbol(x) => write!(f, "{}", x),
            Value::Keyword(x) => write!(f, ":{}", x),
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::FixInt(x) => write!(f, "{}", x),
            Value::FixFloat(x) => write!(f, "{}", x),
            Value::Bool(true) => write!(f, "#T"),
            Value::Bool(false) => write!(f, "#F"),
        }
    }
}

/// TODO: How is this best implemented (based on symbols and keywords)
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
    let mut env = Env::default();

    eval(&mut Env::default(), parser::parse(code).unwrap())
        .unwrap()
        .to_string()
}

/// Evaluates a Sail value, returning the result
fn eval(env: &mut Env, value: Value) -> Result<Value, SailErr> {
    if value.atom_p() {
        if let Value::Symbol(s) = value {
            return Ok(env.entries.get(&s).unwrap().clone());
        }
        return Ok(value);
    } else if let Value::List(p) = value {
        let car = unsafe { (*p).car() };

        if let Value::Symbol(s) = car {
            match s.as_str() {
                "def" => {}
                "fn" => {}
                "if" => {}
                "quote" => {}
                _ => {}
            }
        }

        Ok(Value::Bool(false))
    } else {
        unreachable!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;
    use std::ptr;

    #[test]
    fn returns() {
        let exp = String::from("42");
        assert_eq!(exp, interpret(&exp));
    }

    #[test]
    fn adds() {
        println!("{}", mem::size_of::<Value>());

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
                car: Value::FixInt(42),
                cdr: Box::into_raw(Box::new(List {
                    car: Value::Bool(true),
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

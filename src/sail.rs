//! The Structured Augmentation Interchange Language
//!
//! A custom Lisp dialect for writing STARK

struct List {
    car: Value,
    cdr: Option<Box<List>>,
}

/// Container type for many possible value types
/// TODO: add support for arbitrary precision numbers (`rug` crate)
enum Value {
    True,
    False,

    Symbol(String),
    List(Box<List>),

    String(String),
    Integer(i32),
    Float(f32),
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

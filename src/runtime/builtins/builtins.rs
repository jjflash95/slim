use std::{rc::Rc, collections::HashMap};
use crate::runtime::{EResult, RuntimeError};
use crate::runtime::{Value, context::RuntimeContext};

use crate::runtime::builtins::io;
use crate::runtime::builtins::net;
use crate::runtime::builtins::stdlib;
use crate::runtime::builtins::filesystem;
use crate::runtime::builtins::time;



pub type BuiltinFunc = Rc<Box<dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult>>;

fn wrap_builtin(b: &'static dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult) -> Value {
    Value::Builtin(Rc::new(Box::new(b)))
}

pub fn defaults() -> HashMap<String, Value> {
    let builtins = [
        ("clear".to_string(), wrap_builtin(&io::clear)),
        ("print".to_string(), wrap_builtin(&io::print)),
        ("dbg".to_string(), wrap_builtin(&io::debug)),
        ("input".to_string(), wrap_builtin(&io::input)),
        ("socket".to_string(), wrap_builtin(&net::socket)),
        ("pop".to_string(), wrap_builtin(&stdlib::pop)),
        ("concat".to_string(), wrap_builtin(&stdlib::concat)),
        ("to_string".to_string(), wrap_builtin(&stdlib::to_string)),
        ("slice".to_string(), wrap_builtin(&stdlib::slice)),
        ("len".to_string(), wrap_builtin(&stdlib::len)),
        ("trim".to_string(), wrap_builtin(&stdlib::trim)),
        ("range".to_string(), wrap_builtin(&stdlib::range)),
        ("replace".to_string(), wrap_builtin(&stdlib::replace)),
        ("lowercase".to_string(), wrap_builtin(&stdlib::lowercase)),
        ("random".to_string(), wrap_builtin(&stdlib::random)),
        ("fopen".to_string(), wrap_builtin(&filesystem::open)),
        ("instant".to_string(), wrap_builtin(&time::instant)),
    ];

    HashMap::from(builtins)
}

pub fn next_arg(args: &mut Vec<Value>, name: &str) -> EResult {
    if args.is_empty() {
        Err(RuntimeError(format!("Missing argument <{}>", name)))
    } else {
        Ok(args.remove(0))
    }
}
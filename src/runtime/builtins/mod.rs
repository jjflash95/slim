mod filesystem;
mod net;
mod requests;

use std::cell::RefCell;
use std::time::SystemTime;
use std::{collections::HashMap, io::Write, rc::Rc};

use crate::parser::Span;
use crate::runtime::object::BuiltinClosure;
use crate::{nil, rt_err, runtime::RuntimeError};

use super::object::ToObject;
use super::{
    object::{BuiltinFunc, Object, ObjectRef},
    scope::Scope,
    EResult,
};

pub fn split(_: &mut Scope, span: &Span, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let arg = next_arg(&mut args, "string").map_err(|s| rt_err!(span, "{}", s))?;
    let sep: String = next_arg(&mut args, "separator")
        .unwrap_or_else(|_| Rc::new(RefCell::new(Object::Str("".to_string()))))
        .object()
        .try_into()
        .map_err(|s| rt_err!(span, "{}", s))?;

    Ok(match arg.object() {
        Object::Str(s) => match sep.as_str() {
            "" => Object::Sequence(
                s.chars()
                    .map(|c| Object::Str(c.to_string()).into())
                    .collect(),
            )
            .into(),
            sep => Object::Sequence(
                s.split(sep)
                    .map(|s| Object::Str(s.to_string()).into())
                    .collect(),
            )
            .into(),
        },
        _ => Err(rt_err!(span, "Expected string but got <{:?}> instead", arg,))?,
    })
}

fn pop(_: &mut Scope, span: &Span, args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    if args.len() != 1 {
        return Err(rt_err!(span, "pop takes 1 argument"));
    }
    let mut arg = args[0].borrow_mut();
    if let Object::Sequence(s) = &mut *arg {
        return s
            .pop()
            .ok_or(rt_err!(span, "Cannot pop from empty sequence",));
    }
    Err(rt_err!(span, "pop takes a sequence"))
}

fn clear(_: &mut Scope, _span: &Span, _: Vec<ObjectRef>) -> EResult<ObjectRef> {
    print!("\x1B[2J\x1B[1;1H"); // ANSI escape code to clear the screen
    std::io::stdout().flush().unwrap();
    nil!()
}

fn dbg(_: &mut Scope, _span: &Span, args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    for arg in args {
        // remove 1 from ref_count, since arg passed to dbg is a new Rc
        println!("Refs: {} -> {:?}", Rc::strong_count(&arg) - 1, arg);
    }
    Ok(Object::Nil.into())
}

fn print(_: &mut Scope, _span: &Span, args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    for arg in args {
        print!("{}", arg.borrow().clone());
    }
    println!();
    Ok(Object::Nil.into())
}

fn input(_: &mut Scope, _span: &Span, args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    if !args.is_empty() {
        let prompt = args[0].borrow().clone();
        print!("{}", prompt);
        std::io::stdout().flush().unwrap();
    }
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    Ok(Object::Str(input.trim_end().into()).into())
}

pub fn slice(_: &mut Scope, span: &Span, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    if args.len() < 3 {
        return Err(rt_err!(span, "Slice needs 3 arguments"));
    }

    let iterable = next_arg(&mut args, "sequence")
        .map_err(|s| rt_err!(span, "{}", s))?
        .object();
    let start: i128 = next_arg(&mut args, "start")
        .map_err(|s| rt_err!(span, "{}", s))?
        .object()
        .try_into()
        .map_err(|s| rt_err!(span, "{}", s))?;
    let end: i128 = next_arg(&mut args, "end")
        .map_err(|s| rt_err!(span, "{}", s))?
        .object()
        .try_into()
        .map_err(|s| rt_err!(span, "{}", s))?;
    match iterable {
        Object::Sequence(seq) => {
            Ok(Object::Sequence(seq[start as usize..end as usize].into()).into())
        }
        Object::Str(s) => Ok(Object::Str(s[start as usize..end as usize].to_string()).into()),
        _ => Err(rt_err!(
            span,
            "Slice needs a sequence or string as first argument"
        )),
    }
}

fn rand(_: &mut Scope, span: &Span, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let lower: i128 = next_arg(&mut args, "lower")
        .map_err(|s| rt_err!(span, "{}", s))?
        .object()
        .try_into()
        .map_err(|s| rt_err!(span, "{}", s))?;
    let upper: i128 = next_arg(&mut args, "upper")
        .map_err(|s| rt_err!(span, "{}", s))?
        .object()
        .try_into()
        .map_err(|s| rt_err!(span, "{}", s))?;
    let mut seed = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    seed ^= seed << 21;
    seed ^= seed >> 35;
    seed ^= seed << 4;
    let range = upper - lower + 1;
    Ok(Object::Int(lower + (seed as i128 % range as i128)).into())
}

fn len(_: &mut Scope, span: &Span, args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    if args.len() != 1 {
        return Err(rt_err!(span, "len takes 1 argument"));
    }
    let arg = args[0].borrow();
    match &*arg {
        Object::Sequence(s) => Ok(Object::Int(s.len() as i128).into()),
        Object::Str(s) => Ok(Object::Int(s.len() as i128).into()),
        _ => Err(rt_err!(span, "len takes a sequence or string")),
    }
}

fn concat(_: &mut Scope, _span: &Span, args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let mut result = String::new();
    for arg in args {
        result.push_str(&arg.borrow().to_string());
    }
    Ok(Object::Str(result).into())
}

fn uppercase(_: &mut Scope, span: &Span, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let arg = next_arg(&mut args, "string").map_err(|s| rt_err!(span, "{}", s))?;
    Ok(match arg.object() {
        Object::Str(s) => Object::Str(s.to_uppercase()).into(),
        _ => Err(rt_err!(span, "Expected string but got `{:?}` instead", arg,))?,
    })
}

fn lowercase(_: &mut Scope, span: &Span, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let arg = next_arg(&mut args, "string").map_err(|s| rt_err!(span, "{}", s))?;
    Ok(match arg.object() {
        Object::Str(s) => Object::Str(s.to_lowercase()).into(),
        _ => Err(rt_err!(span, "Expected string but got `{:?}` instead", arg,))?,
    })
}

fn to_string(_: &mut Scope, span: &Span, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let arg = next_arg(&mut args, "string").map_err(|s| rt_err!(span, "{}", s))?;
    Ok(Object::Str(arg.object().to_string()).into())
}

fn _type(_: &mut Scope, span: &Span, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let arg = next_arg(&mut args, "type").map_err(|s| rt_err!(span, "{}", s))?;
    Ok(Object::Str(arg.object()._type().to_string()).into())
}

fn _panic(_: &mut Scope, span: &Span, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let arg = next_arg(&mut args, "panic").map_err(|s| rt_err!(span, "{}", s))?;
    Err(rt_err!(span, "Panic: {}", arg.object()))
}

fn next_arg(args: &mut Vec<Rc<RefCell<Object>>>, arg: &str) -> Result<ObjectRef, String> {
    if args.is_empty() {
        Err(format!("Missing argument <{}>", arg))
    } else {
        Ok(args.remove(0))
    }
}

fn get_builtins() -> Vec<(&'static str, BuiltinClosure)> {
    vec![
        ("fread", wrap(filesystem::read)),
        ("http_get", wrap(requests::get)),
        ("listen", wrap(net::listen)),
        ("to_string", wrap(to_string)),
        ("lowercase", wrap(lowercase)),
        ("uppercase", wrap(uppercase)),
        ("split", wrap(split)),
        ("concat", wrap(concat)),
        ("len", wrap(len)),
        ("slice", wrap(slice)),
        ("rand", wrap(rand)),
        ("input", wrap(input)),
        ("clear", wrap(clear)),
        ("dbg", wrap(dbg)),
        ("print", wrap(print)),
        ("pop", wrap(pop)),
        ("type", wrap(_type)),
        ("panic", wrap(_panic)),
    ]
}

pub fn wrap(f: fn(&mut Scope, &Span, Vec<ObjectRef>) -> EResult<ObjectRef>) -> BuiltinClosure {
    Rc::new(Box::new(f))
}

pub fn default() -> HashMap<&'static str, ObjectRef> {
    let mut builtins = HashMap::new();
    for (name, builtin) in get_builtins() {
        builtins.insert(name, Object::Builtin(BuiltinFunc(builtin)).into());
    }
    builtins
}

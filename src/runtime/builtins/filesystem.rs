use crate::parser::Span;
use crate::rt_err;
use crate::runtime::builtins::next_arg;
use crate::runtime::object::Object;
use crate::runtime::object::ObjectRef;
use crate::runtime::object::ToObject;
use crate::runtime::scope::Scope;
use crate::runtime::EResult;
use crate::runtime::RuntimeError;
use std::{fs::File, io::Read, path::Path};

pub fn read(_: &mut Scope, span: &Span, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let path: String = next_arg(&mut args, "path")
        .map_err(|s| rt_err!(span, "{}", s))?
        .object()
        .try_into()
        .map_err(|s| rt_err!(span, "{}", s))?;
    let mut file = if Path::new(&path).exists() {
        File::open(&path).map_err(|_| rt_err!(span, "Failed to open file"))?
    } else {
        Err(rt_err!(span, "No file named: {}", path))?
    };

    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap_or(0);

    Ok(Object::Str(contents).into())
}

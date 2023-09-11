use std::borrow::BorrowMut;
use std::rc::Rc;

use crate::nil;
use crate::runtime::builtins::next_arg;
use crate::runtime::builtins::BuiltinFunc;
use crate::runtime::object::{Object, ObjectRef, ToObject};
use crate::runtime::scope::Scope;
use crate::runtime::{EResult, RuntimeError};
use std::cell::RefCell;
use std::{
    collections::HashMap,
    io::{Read, Write},
    net::TcpListener,
};

pub fn listen(_: &mut Scope, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let host: String = next_arg(&mut args, "host")?.object().try_into()?;
    let port: i128 = next_arg(&mut args, "port")?.object().try_into()?;
    let listener = TcpListener::bind((host.as_str(), port as u16)).unwrap();

    if let Ok((stream, _)) = listener.accept() {
        let sm = Rc::new(RefCell::new(stream));
        let reader = Rc::clone(&sm);
        let writer = Rc::clone(&sm);
        let closer = Rc::clone(&sm);
        let read = move |_: &mut Scope, _| {
            let buffer = &mut [0; 1024];
            (*reader).borrow_mut().read(buffer).unwrap();
            return Ok(Object::Str(String::from_utf8_lossy(&buffer[..]).into()).into());
        };

        let write = move |_: &mut Scope, mut args: Vec<ObjectRef>| {
            let t: String = args.remove(0).object().try_into()?;
            let mut buf = t.as_bytes();
            while !buf.is_empty() {
                let res = (*writer).borrow_mut().write(buf);
                match res {
                    Ok(0) => break,
                    Ok(n) => buf = &buf[n..],
                    Err(_) => return Err(RuntimeError("Failed socket.write".to_owned())),
                };
            }
            nil!()
        };

        let close = move |_: &mut Scope, _| {
            (*closer)
                .borrow_mut()
                .shutdown(std::net::Shutdown::Both)
                .unwrap();
            nil!()
        };

        let fields = HashMap::from([
            (
                "read".to_string(),
                Object::Builtin(BuiltinFunc(Rc::new(Box::new(read)))).into(),
            ),
            (
                "write".to_string(),
                Object::Builtin(BuiltinFunc(Rc::new(Box::new(write)))).into(),
            ),
            (
                "close".to_string(),
                Object::Builtin(BuiltinFunc(Rc::new(Box::new(close)))).into(),
            ),
        ]);
        Ok(Object::Collection(fields).into())
    } else {
        nil!()
    }
}

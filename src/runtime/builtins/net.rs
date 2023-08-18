
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::{
    collections::HashMap,
    io::{Read, Write},
    net::TcpListener,
};

use crate::runtime::{Value, EResult, RuntimeError};
use crate::runtime::context::RuntimeContext;
use crate::runtime::builtins::next_arg;


pub fn socket(args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let host: String = next_arg(args, "host")?.try_into()?;
    let port: i128 = next_arg(args, "port")?.try_into()?;
    let listener = TcpListener::bind((host.as_str(), port as u16)).unwrap();

    let accept: Box<dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult> = Box::new(move |_, _| {
        if let Ok((stream, _)) = listener.accept() {
            let sm = Arc::new(Mutex::new(stream));
            let reader = Arc::clone(&sm);
            let writer = Arc::clone(&sm);
            let closer = Arc::clone(&sm);
            let listen: Box<dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult> =
                Box::new(move |_, _| {
                    let mut buffer = [0; 1024];
                    reader.lock().unwrap().read(&mut buffer).unwrap();
                    return Ok(Value::StringLiteral(String::from_utf8_lossy(&buffer[..]).into()));
                });

            let write: Box<dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult> =
                Box::new(move |args: &mut Vec<Value>, _: RuntimeContext| {
                    let t: String = args.remove(0).try_into()?;
                    let mut buf = t.as_bytes();
                    while buf != &[] {
                        let res = writer.lock().unwrap().write(buf);
                        match res {
                            Ok(0) => break,
                            Ok(n) => buf = &buf[n..],
                            Err(_) => {
                                return Err(RuntimeError("Failed socket.write".to_owned()))
                            }
                        };
                    }
                    Ok(Value::Nil)
                });

                let close: Box<dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult> =
                Box::new(move |_: &mut Vec<Value>, _: RuntimeContext| {
                    closer.lock().unwrap().shutdown(std::net::Shutdown::Both).unwrap();
                    Ok(Value::Nil)
                });

            let fields = HashMap::from([
                (
                    "listen".to_string(),
                    Value::Builtin(Rc::new(listen)),
                ),
                (
                    "write".to_string(),
                    Value::Builtin(Rc::new(write)),
                ),
                (
                    "close".to_string(),
                    Value::Builtin(Rc::new(close)),
                ),
            ]);
            return Ok(Value::Collection(fields));
        };

        Ok(Value::Nil)
    });

    let fields = HashMap::from([
        (
            "accept".to_string(),
            Value::Builtin(Rc::new(accept)),
        ),
    ]);

    Ok(Value::Collection(fields))
}

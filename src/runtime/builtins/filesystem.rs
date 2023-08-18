
use std::{fs::File, io::{Read, SeekFrom, Write, Seek}, path::Path, sync::{Arc, Mutex}, collections::HashMap, rc::Rc};
use crate::runtime::builtins::next_arg;
use crate::runtime::{Value, context::RuntimeContext, EResult};




pub fn open(args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let path: String = next_arg(args, "path")?.try_into()?;
    let file = if Path::new(&path).exists() {
        File::open(&path).expect("Failed to open file")
    } else {
        File::create(&path).expect("Failed to create file")
    };

    let file = Arc::new(Mutex::new(file));
    let reader = Arc::clone(&file);
    let writer = Arc::clone(&file);
    let seeker = Arc::clone(&file);

    let read: Box<dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult> = Box::new(move |_, _| {
        let mut contents = String::new();
        reader
            .lock()
            .unwrap()
            .read_to_string(&mut contents)
            .unwrap_or(0);
        Ok(Value::StringLiteral(contents))
    });

    let seek: Box<dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult> = Box::new(move |args, _| {
        let bytes: i128 = if let Ok(a) = next_arg(args, "seek") {
            a.try_into()?
        } else {
            0
        };
        seeker.lock().unwrap().seek(SeekFrom::Start(bytes as u64)).unwrap();
        Ok(Value::Nil)
    });

    let write: Box<dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult> = Box::new(move |args, _| {
        let contents: String = next_arg(args, "Content")?.try_into()?;
        match writer
            .lock()
            .unwrap()
            .write_all(contents.as_bytes())
        {
            Ok(_) => Ok(Value::Bool(true)),
            Err(_) => Ok(Value::Bool(false)),
        }
    });

    Ok(Value::Collection(
        HashMap::from([
            (
                "read".to_string(),
                Value::Builtin(Rc::new(read))
            ),
            (
                "seek".to_string(),
                Value::Builtin(Rc::new(seek))
            ),
            (
                "write".to_string(),
                Value::Builtin(Rc::new(write))
            ),
        ]),
    ))
}
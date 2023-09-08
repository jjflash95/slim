use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::time::SystemTime;

use crate::runtime::builtins::BuiltinFunc;
use crate::runtime::scope::RuntimeScope;
use crate::runtime::{EResult, RuntimeError, Value};

pub fn instant(_: &mut Vec<Value>, _: RuntimeScope) -> EResult {
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();
    let shared = Arc::new(Mutex::new(now));
    let millis_ref = Arc::clone(&shared);
    let secs_ref = Arc::clone(&shared);

    let millis: BuiltinFunc = Box::new(move |_, _| {
        Ok(Value::Int(
            millis_ref
                .lock()
                .map_err(|_| RuntimeError("XD".to_string()))?
                .as_millis() as i128,
        ))
    });

    let secs: BuiltinFunc = Box::new(move |_, _| {
        Ok(Value::Int(
            secs_ref
                .lock()
                .map_err(|_| RuntimeError("XD".to_string()))?
                .as_secs()
                .into(),
        ))
    });

    let fields = HashMap::from([
        ("millis".to_string(), Value::Builtin(Rc::new(millis))),
        ("secs".to_string(), Value::Builtin(Rc::new(secs))),
    ]);

    Ok(Value::Collection(fields))
}

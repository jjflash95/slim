use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::time::SystemTime;

use crate::runtime::{Value, EResult, RuntimeError};
use crate::runtime::context::RuntimeContext;

pub fn instant(_: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();
    let shared = Arc::new(Mutex::new(now));
    let millis_ref = Arc::clone(&shared);
    let secs_ref = Arc::clone(&shared);

    let millis: Box<dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult> =
        Box::new(move |_, _| Ok(Value::Int(millis_ref.lock().map_err(|_| RuntimeError(format!("XD")))?.as_millis() as i128)));

    let secs: Box<dyn Fn(&mut Vec<Value>, RuntimeContext) -> EResult> =
        Box::new(move |_, _| Ok(Value::Int(secs_ref.lock().map_err(|_| RuntimeError(format!("XD")))?.as_secs().into())));

    let fields = HashMap::from([
        (
            "millis".to_string(),
            Value::Builtin(Rc::new(millis)),
        ),
        (
            "secs".to_string(),
            Value::Builtin(Rc::new(secs)),
        ),
    ]);

    Ok(Value::Collection(fields))
}

extern crate reqwest;
use crate::parser::Span;
use crate::rt_err;
use crate::runtime::object::{Object, ObjectRef, ToObject};
use crate::runtime::scope::Scope;
use crate::runtime::{EResult, RuntimeError};
use reqwest::header::HeaderMap;
use serde_json::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Read;
use std::rc::Rc;

fn json_to_obj(json_value: Value) -> ObjectRef {
    match json_value {
        Value::Null => Rc::new(RefCell::new(Object::Nil)),
        Value::Bool(b) => Rc::new(RefCell::new(Object::Bool(b))),
        Value::Number(n) => Rc::new(RefCell::new(Object::Float(n.as_f64().unwrap()))),
        Value::String(s) => Rc::new(RefCell::new(Object::Str(s))),
        Value::Array(a) => {
            let mut vec = Vec::new();
            for v in a {
                vec.push(json_to_obj(v));
            }
            Rc::new(RefCell::new(Object::Sequence(vec)))
        }
        Value::Object(o) => {
            let mut map = HashMap::new();
            for (k, v) in o {
                map.insert(k, json_to_obj(v));
            }
            Rc::new(RefCell::new(Object::Collection(map)))
        }
    }
}

impl From<HeaderMap> for Object {
    fn from(val: HeaderMap) -> Self {
        let mut map = HashMap::new();
        for (k, v) in val {
            let key = if k.is_some() {
                k.unwrap().as_str().to_string()
            } else {
                "".to_string()
            };
            map.insert(
                key,
                Object::Str(v.to_str().unwrap_or_default().to_string()).into(),
            );
        }
        Object::Collection(map)
    }
}

impl From<HashMap<String, Value>> for Object {
    fn from(val: HashMap<String, Value>) -> Self {
        let mut map = HashMap::new();
        for (k, v) in val {
            map.insert(k, json_to_obj(v));
        }
        Object::Collection(map)
    }
}

pub fn get(_: &mut Scope, span: &Span, mut args: Vec<ObjectRef>) -> EResult<ObjectRef> {
    let url: String = args
        .remove(0)
        .object()
        .try_into()
        .map_err(|s| rt_err!(span, "{}", s))?;
    let mut res = reqwest::blocking::get(url).map_err(|_| rt_err!(span, "Failed to get"))?;
    let status = res.status().as_u16() as i128;
    let headers = res.headers().to_owned().into();
    let mut body = String::new();
    let _ = res.read_to_string(&mut body);
    let json = res.json::<HashMap<String, Value>>().unwrap_or_default();
    let json: Object = json.into();
    let map = HashMap::from([
        (
            "status".to_string(),
            Rc::new(RefCell::new(Object::Int(status))),
        ),
        ("json".to_string(), Rc::new(RefCell::new(json))),
        ("body".to_string(), Rc::new(RefCell::new(Object::Str(body)))),
        ("headers".to_string(), Rc::new(RefCell::new(headers))),
    ]);

    Ok(Object::Collection(map).into())
}

use std::cell::RefCell;

use std::collections::HashMap;
use std::rc::Rc;

use crate::runtime::Value;

use crate::runtime::builtins;

use super::{InnerReference, RuntimeError};

pub type RuntimeContext = Rc<RefCell<Context>>;

pub static INSTANCE: &str = "self";

pub trait MutRef {
    fn mut_ref(&self) -> Self;
}

impl MutRef for RuntimeContext {
    fn mut_ref(&self) -> Self {
        Rc::clone(self)
    }
}

impl Default for Value {
    fn default() -> Value {
        Value::Nil
    }
}

#[derive(Default)]
pub struct Context {
    pub instance_name: Option<String>,
    pub parent: Option<RuntimeContext>,
    pub scope: HashMap<String, Value>,
    pub eval: Option<Value>,
}

impl Context {
    pub fn get_cloned(&self, key: &str) -> Option<Value> {
        if let Some(value) = self.scope.get(key).cloned() {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_cloned(key)
        } else {
            None
        }
    }

    pub fn peek_ref(&self, key: &str) -> Option<InnerReference> {
        if let Some(value) = self.scope.get(key) {
            match value {
                Value::Ref(inner) => Some(inner.clone()),
                _ => None,
            }
        } else if let Some(parent) = &self.parent {
            parent.borrow().peek_ref(key)
        } else {
            None
        }
    }

    pub fn set(&mut self, key: String, value: Value) -> Option<Value> {
        self.scope.insert(key, value)
    }

    pub fn set_eval(&mut self, value: Value) {
        self.eval = Some(value)
    }

    pub fn type_hint(&self, key: &str, ctx: &RuntimeContext) -> Option<String> {
        self.get_cloned(key)
            .map(|value| value.resolve(ctx).unwrap_or(Value::Nil).type_hint())
    }

    pub fn get_locals(&self) -> HashMap<String, Value> {
        self.scope.clone()
    }

    pub fn set_locals(&mut self, locals: HashMap<String, Value>) {
        self.scope.extend(locals)
    }

    pub fn has_eval(&self) -> bool {
        self.eval.is_some()
    }

    pub fn consume(self) -> Value {
        self.eval.unwrap_or(Value::Nil)
    }

    pub fn set_parent(&mut self, parent: RuntimeContext) {
        self.parent = Some(parent)
    }

    pub fn set_builtins(&mut self) {
        for (name, value) in builtins::defaults() {
            self.set(name, value);
        }
    }
}

pub trait Mutate {
    fn mutate(
        &mut self,
        name: String,
        path: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError>;
}

impl Mutate for Context {
    fn mutate(
        &mut self,
        mut name: String,
        mut path: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let mut _ref = None;

        while let Some(next) = self.peek_ref(&name) {
            _ref = Some(next.clone());
            let (n, p) = next.into();
            name = n;
            path.append(&mut p.clone().into_iter().rev().collect());
        }

        if path.is_empty() {
            return if let Some(src) = self.scope.get_mut(&name) {
                *src = value;
                Ok(Value::Nil)
            } else if let Some(parent) = &mut self.parent {
                return parent.borrow_mut().mutate(name, path, value);
            } else {
                Err(RuntimeError(format!("Cannot mutate: {:?}", name)))?
            };
        };

        if let Some(mut src) = self.scope.get_mut(&name) {
            loop {
                match path
                    .pop()
                    .ok_or_else(|| RuntimeError(format!("No path to access: {:?}", name)))?
                {
                    Value::Int(i) => {
                        if let Value::Ref(inner) = src {
                            let (name, mut np) = inner.to_owned().into();
                            path.push(Value::Int(i));
                            path.append(&mut np);
                            return self.mutate(name, path, value);
                        };
                        let items = src.as_sequence_mut()?;
                        src = items.get_mut(i as usize).ok_or_else(|| {
                            RuntimeError(format!("Index {:?} out of bounds for sequence", i))
                        })?;
                        if path.is_empty() {
                            *src = value;
                            return Ok(Value::Nil);
                        }
                    }
                    Value::StringLiteral(s) => {
                        if let Value::Ref(inner) = src {
                            let (name, mut np) = inner.to_owned().into();
                            path.push(Value::StringLiteral(s));
                            path.append(&mut np);
                            return self.mutate(name, path, value);
                        };

                        let fields = src.as_collection_mut()?;
                        src = fields.entry(s).or_insert(Value::Collection(HashMap::new()));
                        if path.is_empty() {
                            *src = value;
                            return Ok(Value::Nil);
                        }
                    }
                    Value::Nil => {
                        if let Value::Ref(inner) = src {
                            let (name, mut np) = inner.to_owned().into();
                            path.push(Value::Nil);
                            path.append(&mut np);
                            return self.mutate(name, path, value);
                        };

                        let items = src.as_sequence_mut()?;
                        if path.is_empty() {
                            items.push(value);
                            return Ok(Value::Nil);
                        }
                    }
                    v => Err(RuntimeError(format!(
                        "[mutate] Cannot access path {} on: {:?}",
                        v, src
                    )))?,
                }
            }
        } else if let Some(parent) = &mut self.parent {
            parent.borrow_mut().mutate(name, path, value)
        } else {
            Err(RuntimeError(format!("Cannot mutate: {:?}", name)))
        }
    }
}

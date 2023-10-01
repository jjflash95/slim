use core::fmt;
use std::fmt::Write;
use std::hash::Hash;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::lexer::TokenValue;
use crate::parser::{Expr, Token, Block, Span};
use crate::rt_err;
use crate::runtime::scope::Scope;
use crate::runtime::EResult;
use crate::runtime::RuntimeError;

pub type BuiltinClosure = Rc<Box<dyn Fn(&mut Scope, &Span, Vec<ObjectRef>) -> EResult<ObjectRef>>>;

#[derive(Clone)]
pub struct BuiltinFunc(pub BuiltinClosure);

pub type ObjectRef = Rc<RefCell<Object>>;

pub type Ord = std::cmp::Ordering;

pub type TraitImpl = HashMap<String, ObjectRef>;
pub type StructProps = Vec<String>;

#[derive(Clone, Debug)]
pub struct TraitDef {
    pub methods: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Bool,
    Int,
    Float,
    Str,
    Collection,
    Sequence,
    Builtin,
    Func,
    Struct(String),
}

#[derive(Clone, Debug)]
pub enum Object {
    Nil,
    Bool(bool),
    Int(i128),
    Float(f64),
    Str(String),
    Collection(HashMap<String, ObjectRef>),
    Sequence(Vec<ObjectRef>),
    Builtin(BuiltinFunc),
    Func {
        name: Option<String>,
        params: Vec<String>,
        locals: Option<HashMap<String, ObjectRef>>,
        body: Vec<Block>,
    },
    Struct {
        name: String,
        rules: Vec<String>,
        props: HashMap<String, ObjectRef>,
    },
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Nil, Object::Nil) => true,
            (Object::Bool(a), Object::Bool(b)) => a == b,
            (Object::Int(a), Object::Int(b)) => a == b,
            (Object::Float(a), Object::Float(b)) => a == b,
            (Object::Str(a), Object::Str(b)) => a == b,
            (Object::Collection(a), Object::Collection(b)) => a == b,
            (Object::Sequence(a), Object::Sequence(b)) => a == b,
            (Object::Builtin(a), Object::Builtin(b)) => {
                std::ptr::addr_of!(a) == std::ptr::addr_of!(b)
            }
            (Object::Func { .. }, Object::Func { .. }) => self == other,
            _ => false,
        }
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Object::Nil, Object::Nil) => Some(std::cmp::Ordering::Equal),
            (Object::Bool(a), Object::Bool(b)) => {
                if a == b {
                    Some(Ord::Equal)
                } else if a == &true {
                    Some(Ord::Greater)
                } else {
                    Some(Ord::Less)
                }
            }
            (Object::Int(a), Object::Int(b)) => {
                if a == b {
                    Some(Ord::Equal)
                } else if a > b {
                    Some(Ord::Greater)
                } else {
                    Some(Ord::Less)
                }
            }
            (Object::Float(a), Object::Float(b)) => {
                if a == b {
                    Some(Ord::Equal)
                } else if a > b {
                    Some(Ord::Greater)
                } else {
                    Some(Ord::Less)
                }
            }
            (Object::Str(a), Object::Str(b)) => {
                if a == b {
                    Some(Ord::Equal)
                } else if a > b {
                    Some(Ord::Greater)
                } else {
                    Some(Ord::Less)
                }
            }
            (Object::Sequence(a), Object::Sequence(b)) => {
                if a == b {
                    Some(Ord::Equal)
                } else if a > b {
                    Some(Ord::Greater)
                } else {
                    Some(Ord::Less)
                }
            }
            _ => None,
        }
    }
}

impl fmt::Debug for BuiltinFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", std::ptr::addr_of!(self.0))
    }
}

impl Into<Type> for String {
    fn into(self) -> Type {
        match self.as_str() {
            "nil" => Type::Nil,
            "bool" => Type::Bool,
            "int" => Type::Int,
            "float" => Type::Float,
            "str" => Type::Str,
            "collection" => Type::Collection,
            "sequence" => Type::Sequence,
            "builtin" => Type::Builtin,
            "func" => Type::Func,
            _ => Type::Struct(self),
        }
    }
}

impl Object {
    pub fn _type(&self) -> Type {
        match self {
            Object::Nil => Type::Nil,
            Object::Bool(..) => Type::Bool,
            Object::Int(..) => Type::Int,
            Object::Float(..) => Type::Float,
            Object::Str(..) => Type::Str,
            Object::Collection(..) => Type::Collection,
            Object::Sequence(..) => Type::Sequence,
            Object::Builtin(..) => Type::Builtin,
            Object::Func { .. } => Type::Func,
            Object::Struct { name, .. } => Type::Struct(name.to_string()),
        }
    }

    pub fn get_trait(&mut self, scope: &mut Scope, field: ObjectRef) -> Result<ObjectRef, String> {
        if let Object::Str(name) = field.borrow().clone() {
            let traits = scope
                .get_trait_impl(self._type())
                .ok_or(format!(
                    "type {:?} has no traits",
                    self._type()
                ))?;
            traits
                .get(&name)
                .cloned()
                .ok_or(format!("did not found trait for {:?}", field))
        } else {
            Err(format!("type {:?} has no field {:?}", self._type(), field))
        }
    }

    pub fn into_vec(self) -> Result<Vec<ObjectRef>, String> {
        match self {
            Object::Sequence(items) => Ok(items),
            Object::Str(s) => Ok(s
                .chars()
                .map(|c| Object::Str(c.to_string()).into())
                .collect()),
            _ => Err(format!("Cannot convert {:?} to vec", self)),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Nil => false,
            Object::Bool(b) => *b,
            Object::Int(n) => *n != 0,
            Object::Float(n) => *n != 0.0,
            Object::Str(s) => !s.is_empty(),
            Object::Collection(fields) => !fields.is_empty(),
            Object::Sequence(items) => !items.is_empty(),
            Object::Func { .. } => true,
            Object::Builtin(..) => true,
            Object::Struct { .. } => true,
        }
    }

    pub fn access(&mut self, field: Object) -> Result<ObjectRef, String> {
        match self {
            Object::Sequence(items) => match field {
                Object::Int(index) => {
                    let item = items
                        .get(index as usize)
                        .ok_or_else(|| format!("No index: {}", index))?;
                    return Ok(Rc::clone(item));
                }
                Object::Nil => {
                    let next = Rc::new(RefCell::new(Object::Nil));
                    items.push(next.clone());
                    return Ok(next);
                }
                _ => {}
            },
            Object::Collection(fields) => match field {
                Object::Str(key) => {
                    let item = fields
                        .entry(key)
                        .or_insert_with(|| Object::Collection(HashMap::default()).into());
                    return Ok(Rc::clone(item));
                }
                _ => {}
            },
            Object::Struct { name, rules, props } => match field {
                Object::Str(key) => {
                    if !rules.contains(&key) {
                        Err(format!("Struct {} has no field {}", name, key))?
                    };

                    let item = props
                        .get(&key)
                        .ok_or_else(|| format!("No field named: {}", key))?;
                    return Ok(Rc::clone(item));
                }
                _ => {}
            },
            _ => {}
        };

        Err(format!("cannot access : {:?} with {:?}", self, field))
    }

    fn format(&self, indent: u8) -> String {
        match self {
            Object::Collection(fields) => {
                let mut s = String::new();
                let _ = writeln!(s, "{}{{", "".repeat(indent as usize));
                for (key, value) in fields {
                    let _ = writeln!(
                        s,
                        "{}  {}: {}",
                        "  ".repeat(indent as usize),
                        key,
                        value.borrow().format(indent + 1)
                    );
                }
                let _ = write!(s, "{}}}", "  ".repeat(indent as usize));
                s
            }
            Object::Func { name, params, .. } => {
                format!(
                    "[func]{}({})",
                    &name
                        .as_ref()
                        .map(|s| format!(" {} ", s))
                        .unwrap_or("".to_string()),
                    params.join(",")
                )
            }
            Object::Sequence(values) => {
                let mut s = String::new();
                let values = &values
                    .iter()
                    .map(|v| v.borrow().format(indent + 1))
                    .collect::<Vec<String>>()
                    .join(", ");

                let _ = write!(s, "{}[{}]", "".repeat(indent as usize), values);
                s
            }
            Object::Bool(b) => format!("{}", b),
            Object::Str(s) => s.to_string(),
            Object::Int(n) => n.to_string(),
            Object::Float(n) => n.to_string(),
            Object::Builtin { .. } => "[builtin]()".to_string(),
            Object::Nil => "nil".into(),
            Object::Struct { name, props, .. } => {
                let mut s = String::new();
                let _ = write!(s, "{}{} ", "".repeat(indent as usize), name);
                let _ = writeln!(s, "{}{{", "".repeat(indent as usize));
                for (key, value) in props {
                    let _ = writeln!(
                        s,
                        "{}  {}: {}",
                        "  ".repeat(indent as usize),
                        key,
                        value.borrow().format(indent + 1)
                    );
                }
                let _ = write!(s, "{}}}", "  ".repeat(indent as usize));
                s
            }
            _ => "".into(),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.format(0))
    }
}

pub trait ToObject {
    fn object(&self) -> Object;
}

impl ToObject for Rc<RefCell<Object>> {
    fn object(&self) -> Object {
        self.borrow().clone()
    }
}

impl TryInto<Vec<ObjectRef>> for Object {
    type Error = String;

    fn try_into(self) -> Result<Vec<ObjectRef>, String> {
        match self {
            Object::Sequence(items) => Ok(items),
            _ => Err(format!("Cannot convert {:?} to sequence", self)),
        }
    }
}

impl TryInto<HashMap<String, ObjectRef>> for Object {
    type Error = String;

    fn try_into(self) -> Result<HashMap<String, ObjectRef>, Self::Error> {
        match self {
            Object::Collection(fields) => Ok(fields),
            _ => Err(format!("Cannot convert {:?} to collection", self)),
        }
    }
}

impl TryInto<i128> for Object {
    type Error = String;

    fn try_into(self) -> Result<i128, Self::Error> {
        match self {
            Object::Int(n) => Ok(n),
            _ => Err(format!("Cannot convert {:?} to integer", self)),
        }
    }
}

impl TryInto<f64> for Object {
    type Error = String;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            Object::Float(n) => Ok(n),
            _ => Err(format!("Cannot convert {:?} to float", self)),
        }
    }
}

impl TryInto<usize> for Object {
    type Error = String;

    fn try_into(self) -> Result<usize, Self::Error> {
        match self {
            Object::Int(n) => Ok(n as usize),
            _ => Err(format!("Cannot convert {:?} to integer", self)),
        }
    }
}

impl TryInto<String> for Object {
    type Error = String;

    fn try_into(self) -> Result<String, Self::Error> {
        match self {
            Object::Str(s) => Ok(s),
            _ => Err(format!("Cannot convert {:?} to string", self)),
        }
    }
}

impl Into<ObjectRef> for Object {
    fn into(self) -> ObjectRef {
        Rc::new(RefCell::new(self))
    }
}

impl Into<bool> for Object {
    fn into(self) -> bool {
        match self {
            Object::Nil => false,
            Object::Bool(b) => b,
            Object::Int(n) => n != 0,
            Object::Float(n) => n != 0.0,
            Object::Str(s) => !s.is_empty(),
            Object::Collection(fields) => !fields.is_empty(),
            Object::Func { .. } => true,
            _ => panic!("Should never come to this"),
        }
    }
}

impl TryFrom<Token> for Object {
    type Error = String;
    fn try_from(token: Token) -> Result<Object, String> {
        match token.value {
            TokenValue::Int(n) => Ok(Object::Int(n)),
            TokenValue::Float(n) => Ok(Object::Float(n)),
            TokenValue::Str(s) => Ok(Object::Str(s)),
            TokenValue::True => Ok(Object::Bool(true)),
            TokenValue::False => Ok(Object::Bool(false)),
            TokenValue::Nil => Ok(Object::Nil),
            _ => Err(format!(
                "Cannot directly convert token `{:?}` to value",
                token
            )),
        }
    }
}

impl TryFrom<Token> for ObjectRef {
    type Error = String;
    fn try_from(token: Token) -> Result<ObjectRef, String> {
        match token.value {
            TokenValue::Int(n) => Ok(Object::Int(n).into()),
            TokenValue::Float(n) => Ok(Object::Float(n).into()),
            TokenValue::Str(s) => Ok(Object::Str(s).into()),
            TokenValue::True => Ok(Object::Bool(true).into()),
            TokenValue::False => Ok(Object::Bool(false).into()),
            TokenValue::Nil => Ok(Object::Nil.into()),
            _ => Err(format!(
                "Cannot directly convert token `{:?}` to value",
                token
            )),
        }
    }
}

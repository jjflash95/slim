use crate::runtime::{RuntimeError, Value};
use std::fmt::Write as _;
use core::fmt;

impl TryInto<i128> for Value {
    type Error = RuntimeError;
    fn try_into(self) -> Result<i128, Self::Error> {
        match self {
            Value::Int(n) => Ok(n),
            _ => Err(RuntimeError(format!(
                "Cannot convert {} to int",
                self.type_hint()
            ))),
        }
    }
}

impl TryInto<String> for Value {
    type Error = RuntimeError;
    fn try_into(self) -> Result<String, Self::Error> {
        match self {
            Value::StringLiteral(s) => Ok(s),
            Value::Int(i) => Ok(i.to_string()),
            _ => Err(RuntimeError(format!(
                "Cannot convert {} to int",
                self.type_hint()
            ))),
        }
    }
}

impl TryInto<Vec<Value>> for Value {
    type Error = RuntimeError;
    fn try_into(self) -> Result<Vec<Value>, Self::Error> {
        match self {
            Value::Sequence(seq) => Ok(seq),
            _ => Err(RuntimeError(format!(
                "Cannot convert {} to int",
                self.type_hint()
            ))),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::StringLiteral(a), Self::StringLiteral(b)) => a == b,
            (Self::Sequence(a), Self::Sequence(b)) => a == b,
            (Self::Collection(a), Self::Collection(b)) => a == b,
            (Self::Ref(a), Self::Ref(b)) => a == b,
            (
                Self::Func {
                    name,
                    body,
                    locals,
                    params,
                },
                Self::Func {
                    name: nb,
                    body: bb,
                    locals: lb,
                    params: pb,
                },
            ) => name == nb && body == bb && params == pb && locals == lb,
            _ => false,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Bool(b) => write!(f, "Bool({})", b),
            Value::Float(num) => write!(f, "Float({})", num),
            Value::Int(num) => write!(f, "Int({})", num),
            Value::StringLiteral(s) => write!(f, "Str(\"{}\")", s),
            Value::Sequence(values) => write!(f, "Sequence({:#?})", values),
            Value::Collection(fields) => write!(f, "Collection({:#?})", fields),
            Value::Builtin { .. } => write!(f, "BuiltinFunc"),
            Value::Func {
                name,
                params,
                body,
                locals,
            } => write!(
                f,
                "Func {{ name: {:?}, params: {:?}, locals: {:#?}, body: {:#?}}}",
                name, params, locals, body
            ),
            Value::Ref(inner) => write!(f, "Ref({:?})", inner),
            _ => write!(f, ""),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format(self, 0))
    }
}

fn format(value: &Value, indent: u8) -> String {
    match value {
        Value::Collection(fields) => {
            let mut s = String::new();
            let _ = writeln!(s, "{}{{", "".repeat(indent as usize));
            for (key, value) in fields {
                let _ = writeln!(
                    s,
                    "{}  {}: {}",
                    "  ".repeat(indent as usize),
                    key,
                    format(value, indent + 1)
                );
            };
            let _ = write!(s, "{}}}", "  ".repeat(indent as usize));
            s
        }
        Value::Func { name, params, .. } => {
            format!(
                "func<{}>({})",
                &name.as_ref().unwrap_or(&"anonymous".to_string()),
                params.join(",")
            )
        }
        Value::Sequence(values) => {
            let mut s = String::new();
            let values = &values
                .iter()
                .map(|v| format(v, indent + 1))
                .collect::<Vec<String>>()
                .join(", ");

            let _ = write!(s, "{}[{}]", "".repeat(indent as usize), values);
            s
        }
        Value::Bool(b) => format!("{}", b),
        Value::StringLiteral(s) => s.to_string(),
        Value::Int(n) => n.to_string(),
        Value::Float(n) => n.to_string(),
        Value::Builtin { .. } => "func<builtin>()".to_string(),
        Value::Nil => "nil".into(),
        _ => "".into(),
    }
}

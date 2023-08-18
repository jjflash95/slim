use crate::parser::Token;
use crate::runtime::EResult;
use crate::runtime::RuntimeError;
use crate::runtime::Value;

pub trait BinOps {
    fn binary(self, op: Token, other: Self) -> EResult;
    fn add(self, other: Self) -> EResult;
    fn sub(self, other: Self) -> EResult;
    fn mult(self, other: Self) -> EResult;
    fn div(self, other: Self) -> EResult;
    fn coef(self, other: Self) -> EResult;
    fn eq(self, other: Self) -> EResult;
    fn ne(self, other: Self) -> EResult;
    fn gt(self, other: Self) -> EResult;
    fn gte(self, other: Self) -> EResult;
    fn lt(self, other: Self) -> EResult;
    fn lte(self, other: Self) -> EResult;
    fn and(self, other: Self) -> EResult;
    fn or(self, other: Self) -> EResult;
}

pub trait UnaryOps {
    fn unary(self, token: Token) -> EResult;
    fn negative(self) -> EResult;
    fn not(self) -> EResult;
}

impl UnaryOps for Value {
    fn unary(self, op: Token) -> EResult {
        match op {
            Token::Negative => self.negative(),
            Token::Not => self.not(),
            _ => Err(RuntimeError(format!(
                "Cannot perform unary operation with token {:?}",
                op
            ))),
        }
    }

    fn negative(self) -> EResult {
        match self {
            Value::Int(n) => Ok(Value::Int(-n)),
            Value::Float(f) => Ok(Value::Float(-f)),
            _ => Err(RuntimeError(format!(
                "Cannot get negative value of {:?}",
                self
            ))),
        }
    }

    fn not(self) -> EResult {
        Ok(Value::Bool(!self.to_bool()))
    }
}

impl BinOps for Value {
    fn binary(self, op: Token, other: Self) -> EResult {
        match op {
            Token::Add => self.add(other),
            Token::Sub => self.sub(other),
            Token::Multiply => self.mult(other),
            Token::Divide => self.div(other),
            Token::Coef => self.coef(other),
            Token::Eq => self.eq(other),
            Token::Ne => self.ne(other),
            Token::Gt => self.gt(other),
            Token::Gte => self.gte(other),
            Token::Lt => self.lt(other),
            Token::Lte => self.lte(other),
            Token::And => self.and(other),
            Token::Or => self.or(other),
            _ => Err(RuntimeError(format!(
                "Cannot perform binary operation with token {:?}",
                op
            ))),
        }
    }

    fn add(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l + r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 + r)),
            (l, r) => Err(RuntimeError(format!("Cannot add {:?} to {:?}", l, r))),
        }
    }

    fn sub(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l - r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 - r)),
            (l, r) => Err(RuntimeError(format!("Cannot sub {:?} to {:?}", l, r))),
        }
    }

    fn mult(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l * r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 * r)),
            (l, r) => Err(RuntimeError(format!("Cannot mult {:?} to {:?}", l, r))),
        }
    }

    fn div(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Float(l as f64 / r as f64)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l / r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 / r)),
            (l, r) => Err(RuntimeError(format!("Cannot div {:?} to {:?}", l, r))),
        }
    }

    fn coef(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l % r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l % r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l % r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Float(l as f64 % r)),
            (l, r) => Err(RuntimeError(format!("Cannot mod {:?} to {:?}", l, r))),
        }
    }

    fn eq(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l == r)),
            (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l == r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l == r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l == r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) == r)),
            (Value::StringLiteral(l), Value::StringLiteral(r)) => Ok(Value::Bool(l == r)),
            (Value::Sequence(l), Value::Sequence(r)) => Ok(Value::Bool(l == r)),
            (Value::Collection(l), Value::Collection(r)) => Ok(Value::Bool(l == r)),
            (l, r) => Err(RuntimeError(format!(
                "Cannot compare eq {:?} to {:?}",
                l, r
            ))),
        }
    }

    fn ne(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l == r)),
            (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l != r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l != r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l != r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) != r)),
            (Value::StringLiteral(l), Value::StringLiteral(r)) => Ok(Value::Bool(l != r)),
            (Value::Sequence(l), Value::Sequence(r)) => Ok(Value::Bool(l != r)),
            (Value::Collection(l), Value::Collection(r)) => Ok(Value::Bool(l != r)),
            (l, r) => Err(RuntimeError(format!(
                "Cannot compare ne {:?} to {:?}",
                l, r
            ))),
        }
    }

    fn gt(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l > r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l > r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l > r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) > r)),
            (Value::StringLiteral(l), Value::StringLiteral(r)) => {
                Ok(Value::Bool(l.len() > r.len()))
            }
            (Value::Sequence(l), Value::Sequence(r)) => Ok(Value::Bool(l.len() > r.len())),
            // (Value::Collection(l), Value::Collection(r)) => Ok(Value::Bool(l.len() > r.len())), // should I?
            (l, r) => Err(RuntimeError(format!(
                "Cannot compare gt {:?} to {:?}",
                l, r
            ))),
        }
    }

    fn gte(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l >= r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l >= r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l >= r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) >= r)),
            (Value::StringLiteral(l), Value::StringLiteral(r)) => {
                Ok(Value::Bool(l.len() >= r.len()))
            }
            (Value::Sequence(l), Value::Sequence(r)) => Ok(Value::Bool(l.len() >= r.len())),
            // (Value::Collection(l), Value::Collection(r)) => Ok(Value::Bool(l.len() >= r.len())), // should I?
            (l, r) => Err(RuntimeError(format!(
                "Cannot compare gte {:?} to {:?}",
                l, r
            ))),
        }
    }

    fn lt(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l < r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l < r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l < r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) < r)),
            (Value::StringLiteral(l), Value::StringLiteral(r)) => {
                Ok(Value::Bool(l.len() < r.len()))
            }
            (Value::Sequence(l), Value::Sequence(r)) => Ok(Value::Bool(l.len() < r.len())),
            // (Value::Collection(l), Value::Collection(r)) => Ok(Value::Bool(l.len() < r.len())),
            (l, r) => Err(RuntimeError(format!(
                "Cannot compare lt {:?} to {:?}",
                l, r
            ))),
        }
    }

    fn lte(self, other: Self) -> EResult {
        match (self, other) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l <= r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l <= r)),
            (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l <= r as f64)),
            (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) <= r)),
            (Value::StringLiteral(l), Value::StringLiteral(r)) => {
                Ok(Value::Bool(l.len() <= r.len()))
            }
            (Value::Sequence(l), Value::Sequence(r)) => Ok(Value::Bool(l.len() <= r.len())),
            // (Value::Collection(l), Value::Collection(r)) => Ok(Value::Bool(l.len() <= r.len())),
            (l, r) => Err(RuntimeError(format!(
                "Cannot compare lt {:?} to {:?}",
                l, r
            ))),
        }
    }

    fn and(self, other: Self) -> EResult {
        match (&self, other) {
            (&Value::Int(l), other) => Ok(if l == 0 { self } else { other }),
            (&Value::Float(l), other) => Ok(if l == 0.0 { self } else { other }),
            (&Value::Bool(l), other) => Ok(if !l { self } else { other }),
            (&Value::StringLiteral(ref l), other) => Ok(if l.is_empty() { self } else { other }),
            (&Value::Nil, ..) => Ok(self),
            (&Value::Collection(ref fields), other) => {
                Ok(if fields.is_empty() { self } else { other })
            }
            (&Value::Func { .. }, other) => Ok(other),
            (l, r) => Err(RuntimeError(format!(
                "Cannot compare && {:?} to {:?}",
                l, r
            ))),
        }
    }

    fn or(self, other: Self) -> EResult {
        match (&self, other) {
            (&Value::Int(l), other) => Ok(if l == 0 { other } else { self }),
            (&Value::Float(l), other) => Ok(if l == 0.0 { other } else { self }),
            (&Value::Bool(l), other) => Ok(if !l { other } else { self }),
            (&Value::StringLiteral(ref l), other) => Ok(if l.is_empty() { other } else { self }),
            (&Value::Nil, other) => Ok(other),
            (&Value::Collection(ref fields), other) => {
                Ok(if fields.is_empty() { other } else { self })
            }
            (&Value::Func { .. }, ..) => Ok(self),
            (l, r) => Err(RuntimeError(format!(
                "Cannot compare || {:?} to {:?}",
                l, r
            ))),
        }
    }
}

pub trait Helpers {
    fn concat(self, other: Self) -> EResult;
    fn len(self) -> EResult;
}

impl Helpers for Value {
    fn concat(self, other: Value) -> EResult {
        match self {
            Value::StringLiteral(a) => Ok(Value::StringLiteral(
                a + TryInto::<String>::try_into(other)?.as_ref(),
            )),
            Value::Sequence(values) => Ok(Value::Sequence(
                values
                    .into_iter()
                    .chain(TryInto::<Vec<Value>>::try_into(other)?)
                    .collect(),
            )),
            _ => Err(RuntimeError(format!(
                "Cannot merge {} to {}",
                self.type_hint(),
                other.type_hint()
            ))),
        }
    }

    fn len(self) -> EResult {
        match self {
            Value::StringLiteral(s) => Ok(Value::Int(s.len() as i128)),
            Value::Sequence(values) => Ok(Value::Int(values.len() as i128)),
            Value::Collection(fields) => Ok(Value::Int(fields.len() as i128)),
            v => Err(RuntimeError(format!(
                "Cannot get len of: <{}>",
                v.type_hint()
            ))),
        }
    }
}

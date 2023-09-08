use crate::parser::Token;
use crate::rt_err;
use crate::runtime::EResult;
use crate::runtime::Object;
use crate::runtime::RuntimeError;

use super::object::ObjectRef;
use super::object::Ord;

pub trait BinOps {
    fn binary(self, op: Token, other: Self) -> EResult<Object>;
    fn add(self, other: Self) -> EResult<Object>;
    fn sub(self, other: Self) -> EResult<Object>;
    fn mult(self, other: Self) -> EResult<Object>;
    fn div(self, other: Self) -> EResult<Object>;
    fn coef(self, other: Self) -> EResult<Object>;
    fn eq(self, other: Self) -> EResult<Object>;
    fn ne(self, other: Self) -> EResult<Object>;
    fn gt(self, other: Self) -> EResult<Object>;
    fn gte(self, other: Self) -> EResult<Object>;
    fn lt(self, other: Self) -> EResult<Object>;
    fn lte(self, other: Self) -> EResult<Object>;
    fn contains(self, ohter: Self) -> EResult<Object>;
    fn and(self, other: Self) -> EResult<Object>;
    fn or(self, other: Self) -> EResult<Object>;
}

pub trait UnaryOps {
    fn unary(self, token: Token) -> EResult<Object>;
    fn negative(self) -> EResult<Object>;
    fn not(self) -> EResult<Object>;
}

impl UnaryOps for Object {
    fn unary(self, op: Token) -> EResult<Object> {
        match op {
            Token::Negative => self.negative(),
            Token::Not => self.not(),
            _ => Err(RuntimeError(format!(
                "Cannot perform unary operation with token {:?}",
                op
            ))),
        }
    }

    fn negative(self) -> EResult<Object> {
        match self {
            Object::Int(n) => Ok(Object::Int(-n)),
            Object::Float(f) => Ok(Object::Float(-f)),
            _ => Err(RuntimeError(format!(
                "Cannot get negative value of {:?}",
                self
            ))),
        }
    }

    fn not(self) -> EResult<Object> {
        Ok(Object::Bool(true)) //(!self.to_bool()))
    }
}

impl BinOps for Object {
    fn binary(self, op: Token, other: Self) -> EResult<Object> {
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
            Token::In => self.contains(other),
            Token::And => self.and(other),
            Token::Or => self.or(other),
            _ => Err(RuntimeError(format!(
                "Cannot perform binary operation with token `{:?}`",
                op
            ))),
        }
    }

    fn add(self, other: Self) -> EResult<Object> {
        match (self, other) {
            (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l + r)),
            (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l + r)),
            (Object::Float(l), Object::Int(r)) => Ok(Object::Float(l + r as f64)),
            (Object::Int(l), Object::Float(r)) => Ok(Object::Float(l as f64 + r)),
            (l, r) => Err(RuntimeError(format!("Cannot add {:?} to {:?}", l, r))),
        }
    }

    fn sub(self, other: Self) -> EResult<Object> {
        match (self, other) {
            (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l - r)),
            (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l - r)),
            (Object::Float(l), Object::Int(r)) => Ok(Object::Float(l - r as f64)),
            (Object::Int(l), Object::Float(r)) => Ok(Object::Float(l as f64 - r)),
            (l, r) => Err(RuntimeError(format!("Cannot sub {:?} to {:?}", l, r))),
        }
    }

    fn mult(self, other: Self) -> EResult<Object> {
        match (self, other) {
            (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l * r)),
            (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l * r)),
            (Object::Float(l), Object::Int(r)) => Ok(Object::Float(l * r as f64)),
            (Object::Int(l), Object::Float(r)) => Ok(Object::Float(l as f64 * r)),
            (l, r) => Err(RuntimeError(format!("Cannot mult {:?} to {:?}", l, r))),
        }
    }

    fn div(self, other: Self) -> EResult<Object> {
        match (self, other) {
            (Object::Int(l), Object::Int(r)) => Ok(Object::Float(l as f64 / r as f64)),
            (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l / r)),
            (Object::Float(l), Object::Int(r)) => Ok(Object::Float(l / r as f64)),
            (Object::Int(l), Object::Float(r)) => Ok(Object::Float(l as f64 / r)),
            (l, r) => Err(RuntimeError(format!("Cannot div {:?} to {:?}", l, r))),
        }
    }

    fn coef(self, other: Self) -> EResult<Object> {
        match (self, other) {
            (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l % r)),
            (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l % r)),
            (Object::Float(l), Object::Int(r)) => Ok(Object::Float(l % r as f64)),
            (Object::Int(l), Object::Float(r)) => Ok(Object::Float(l as f64 % r)),
            (l, r) => Err(RuntimeError(format!("Cannot mod {:?} to {:?}", l, r))),
        }
    }

    fn eq(self, other: Self) -> EResult<Object> {
        Ok(Object::Bool(self == other))
    }

    fn ne(self, other: Self) -> EResult<Object> {
        Ok(Object::Bool(self != other))
    }

    fn gt(self, other: Self) -> EResult<Object> {
        match self.partial_cmp(&other) {
            Some(Ord::Greater) => Ok(Object::Bool(true)),
            Some(_) => Ok(Object::Bool(false)),
            None => rt_err!("Cannot compare gt {} on {}", self, other),
        }
    }

    fn gte(self, other: Self) -> EResult<Object> {
        match self.partial_cmp(&other) {
            Some(Ord::Greater) => Ok(Object::Bool(true)),
            Some(Ord::Equal) => Ok(Object::Bool(true)),
            Some(_) => Ok(Object::Bool(false)),
            None => rt_err!("Cannot compare gte {} on {}", self, other),
        }
    }

    fn lt(self, other: Self) -> EResult<Object> {
        match self.partial_cmp(&other) {
            Some(Ord::Less) => Ok(Object::Bool(true)),
            Some(_) => Ok(Object::Bool(false)),
            None => rt_err!("Cannot compare lt {} on {}", self, other),
        }
    }

    fn lte(self, other: Self) -> EResult<Object> {
        match self.partial_cmp(&other) {
            Some(Ord::Less) => Ok(Object::Bool(true)),
            Some(Ord::Equal) => Ok(Object::Bool(true)),
            Some(_) => Ok(Object::Bool(false)),
            None => rt_err!("Cannot compare lte {} on {}", self, other),
        }
    }

    fn contains(self, other: Self) -> EResult<Object> {
        match other {
            Object::Sequence(s) => Ok(Object::Bool(s.contains(&self.into()))),
            Object::Collection(f) => Ok(Object::Bool(
                f.contains_key(TryInto::<String>::try_into(self)?.as_str()),
            )),
            Object::Str(s) => Ok(Object::Bool(
                s.contains(TryInto::<String>::try_into(self)?.as_str()),
            )),
            _ => Err(RuntimeError(format!(
                "Cannot check if <{:?}> contains <{:?}>",
                self, other
            ))),
        }
    }

    fn and(self, other: Self) -> EResult<Object> {
        match (&self, other) {
            (&Object::Int(l), other) => Ok(if l == 0 { self } else { other }),
            (&Object::Float(l), other) => Ok(if l == 0.0 { self } else { other }),
            (&Object::Bool(l), other) => Ok(if !l { self } else { other }),
            (&Object::Str(ref l), other) => Ok(if l.is_empty() { self } else { other }),
            (&Object::Nil, ..) => Ok(self),
            (&Object::Collection(ref fields), other) => {
                Ok(if fields.is_empty() { self } else { other })
            }
            (&Object::Func { .. }, other) => Ok(other),
            (l, r) => Err(RuntimeError(format!(
                "Cannot compare && {:?} to {:?}",
                l, r
            ))),
        }
    }

    fn or(self, other: Self) -> EResult<Object> {
        match (&self, other) {
            (&Object::Int(l), other) => Ok(if l == 0 { other } else { self }),
            (&Object::Float(l), other) => Ok(if l == 0.0 { other } else { self }),
            (&Object::Bool(l), other) => Ok(if !l { other } else { self }),
            (&Object::Str(ref l), other) => Ok(if l.is_empty() { other } else { self }),
            (&Object::Nil, other) => Ok(other),
            (&Object::Collection(ref fields), other) => {
                Ok(if fields.is_empty() { other } else { self })
            }
            (&Object::Func { .. }, ..) => Ok(self),
            (l, r) => Err(RuntimeError(format!(
                "Cannot compare || {:?} to {:?}",
                l, r
            ))),
        }
    }
}

pub trait Helpers {
    fn concat(self, other: Self) -> EResult<Object>;
    fn len(self) -> EResult<Object>;
}

impl Helpers for Object {
    fn concat(self, other: Object) -> EResult<Object> {
        match self {
            Object::Str(a) => Ok(Object::Str(
                a + TryInto::<String>::try_into(other)?.as_ref(),
            )),
            Object::Sequence(values) => Ok(Object::Sequence(
                values
                    .into_iter()
                    .chain(TryInto::<Vec<ObjectRef>>::try_into(other)?)
                    .collect(),
            )),
            _ => Err(RuntimeError(format!(
                "Cannot merge {:?} to {:?}",
                self, other
            ))),
        }
    }

    fn len(self) -> EResult<Object> {
        match self {
            Object::Str(s) => Ok(Object::Int(s.len() as i128)),
            Object::Sequence(values) => Ok(Object::Int(values.len() as i128)),
            Object::Collection(fields) => Ok(Object::Int(fields.len() as i128)),
            v => Err(RuntimeError(format!("Cannot get len of: <{:?}>", v))),
        }
    }
}

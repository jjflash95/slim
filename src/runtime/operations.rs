use crate::parser::TokenValue;

use crate::runtime::Object;

use super::object::ObjectRef;
use super::object::Ord;

type Token = TokenValue;

pub trait BinOps {
    fn binary(self, op: &Token, other: Self) -> Result<Object, String>;
    fn add(self, other: Self) -> Result<Object, String>;
    fn sub(self, other: Self) -> Result<Object, String>;
    fn mult(self, other: Self) -> Result<Object, String>;
    fn div(self, other: Self) -> Result<Object, String>;
    fn coef(self, other: Self) -> Result<Object, String>;
    fn eq(self, other: Self) -> Result<Object, String>;
    fn ne(self, other: Self) -> Result<Object, String>;
    fn gt(self, other: Self) -> Result<Object, String>;
    fn gte(self, other: Self) -> Result<Object, String>;
    fn lt(self, other: Self) -> Result<Object, String>;
    fn lte(self, other: Self) -> Result<Object, String>;
    fn contains(self, ohter: Self) -> Result<Object, String>;
    fn and(self, other: Self) -> Result<Object, String>;
    fn or(self, other: Self) -> Result<Object, String>;
}

pub trait UnaryOps {
    fn unary(self, token: &Token) -> Result<Object, String>;
    fn negative(self) -> Result<Object, String>;
    fn not(self) -> Result<Object, String>;
}

impl UnaryOps for Object {
    fn unary(self, op: &Token) -> Result<Object, String> {
        match op {
            Token::Negative => self.negative(),
            Token::Not => self.not(),
            _ => Err(format!(
                "Cannot perform unary operation with token {:?}",
                op
            )),
        }
    }

    fn negative(self) -> Result<Object, String> {
        match self {
            Object::Int(n) => Ok(Object::Int(-n)),
            Object::Float(f) => Ok(Object::Float(-f)),
            _ => Err(format!("Cannot get negative value of {:?}", self)),
        }
    }

    fn not(self) -> Result<Object, String> {
        Ok(Object::Bool(true)) //(!self.to_bool()))
    }
}

impl BinOps for Object {
    fn binary(self, op: &Token, other: Self) -> Result<Object, String> {
        match op {
            Token::Plus => self.add(other),
            Token::Minus => self.sub(other),
            Token::Star => self.mult(other),
            Token::Slash => self.div(other),
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
            _ => Err(format!(
                "Cannot perform binary operation with token `{:?}`",
                op
            )),
        }
    }

    fn add(self, other: Self) -> Result<Object, String> {
        match (self, other) {
            (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l + r)),
            (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l + r)),
            (Object::Float(l), Object::Int(r)) => Ok(Object::Float(l + r as f64)),
            (Object::Int(l), Object::Float(r)) => Ok(Object::Float(l as f64 + r)),
            (l, r) => Err(format!("Cannot add {:?} to {:?}", l, r)),
        }
    }

    fn sub(self, other: Self) -> Result<Object, String> {
        match (self, other) {
            (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l - r)),
            (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l - r)),
            (Object::Float(l), Object::Int(r)) => Ok(Object::Float(l - r as f64)),
            (Object::Int(l), Object::Float(r)) => Ok(Object::Float(l as f64 - r)),
            (l, r) => Err(format!("Cannot sub {:?} to {:?}", l, r)),
        }
    }

    fn mult(self, other: Self) -> Result<Object, String> {
        match (self, other) {
            (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l * r)),
            (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l * r)),
            (Object::Float(l), Object::Int(r)) => Ok(Object::Float(l * r as f64)),
            (Object::Int(l), Object::Float(r)) => Ok(Object::Float(l as f64 * r)),
            (l, r) => Err(format!("Cannot mult {:?} to {:?}", l, r)),
        }
    }

    fn div(self, other: Self) -> Result<Object, String> {
        match (self, other) {
            (Object::Int(l), Object::Int(r)) => Ok(Object::Float(l as f64 / r as f64)),
            (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l / r)),
            (Object::Float(l), Object::Int(r)) => Ok(Object::Float(l / r as f64)),
            (Object::Int(l), Object::Float(r)) => Ok(Object::Float(l as f64 / r)),
            (l, r) => Err(format!("Cannot div {:?} to {:?}", l, r)),
        }
    }

    fn coef(self, other: Self) -> Result<Object, String> {
        match (self, other) {
            (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l % r)),
            (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l % r)),
            (Object::Float(l), Object::Int(r)) => Ok(Object::Float(l % r as f64)),
            (Object::Int(l), Object::Float(r)) => Ok(Object::Float(l as f64 % r)),
            (l, r) => Err(format!("Cannot mod {:?} to {:?}", l, r)),
        }
    }

    fn eq(self, other: Self) -> Result<Object, String> {
        Ok(Object::Bool(self == other))
    }

    fn ne(self, other: Self) -> Result<Object, String> {
        Ok(Object::Bool(self != other))
    }

    fn gt(self, other: Self) -> Result<Object, String> {
        match self.partial_cmp(&other) {
            Some(Ord::Greater) => Ok(Object::Bool(true)),
            Some(_) => Ok(Object::Bool(false)),
            None => Err(format!("Cannot compare gt {} on {}", self, other)),
        }
    }

    fn gte(self, other: Self) -> Result<Object, String> {
        match self.partial_cmp(&other) {
            Some(Ord::Greater) => Ok(Object::Bool(true)),
            Some(Ord::Equal) => Ok(Object::Bool(true)),
            Some(_) => Ok(Object::Bool(false)),
            None => Err(format!("Cannot compare gte {} on {}", self, other)),
        }
    }

    fn lt(self, other: Self) -> Result<Object, String> {
        match self.partial_cmp(&other) {
            Some(Ord::Less) => Ok(Object::Bool(true)),
            Some(_) => Ok(Object::Bool(false)),
            None => Err(format!("Cannot compare lt {} on {}", self, other)),
        }
    }

    fn lte(self, other: Self) -> Result<Object, String> {
        match self.partial_cmp(&other) {
            Some(Ord::Less) => Ok(Object::Bool(true)),
            Some(Ord::Equal) => Ok(Object::Bool(true)),
            Some(_) => Ok(Object::Bool(false)),
            None => Err(format!("Cannot compare lte {} on {}", self, other)),
        }
    }

    fn contains(self, other: Self) -> Result<Object, String> {
        match other {
            Object::Sequence(s) => Ok(Object::Bool(s.contains(&self.into()))),
            Object::Collection(f) => Ok(Object::Bool(
                f.contains_key(TryInto::<String>::try_into(self)?.as_str()),
            )),
            Object::Str(s) => Ok(Object::Bool(
                s.contains(TryInto::<String>::try_into(self)?.as_str()),
            )),
            _ => Err(format!(
                "Cannot check if <{:?}> contains <{:?}>",
                self, other
            )),
        }
    }

    fn and(self, other: Self) -> Result<Object, String> {
        match (&self, other) {
            (&Object::Int(l), other) => Ok(if l == 0 { self } else { other }),
            (&Object::Float(l), other) => Ok(if l == 0.0 { self } else { other }),
            (&Object::Bool(l), other) => Ok(if !l { self } else { other }),
            (Object::Str(l), other) => Ok(if l.is_empty() { self } else { other }),
            (&Object::Nil, ..) => Ok(self),
            (Object::Collection(fields), other) => Ok(if fields.is_empty() { self } else { other }),
            (&Object::Func { .. }, other) => Ok(other),
            (l, r) => Err(format!("Cannot compare && {:?} to {:?}", l, r)),
        }
    }

    fn or(self, other: Self) -> Result<Object, String> {
        match (&self, other) {
            (&Object::Int(l), other) => Ok(if l == 0 { other } else { self }),
            (&Object::Float(l), other) => Ok(if l == 0.0 { other } else { self }),
            (&Object::Bool(l), other) => Ok(if !l { other } else { self }),
            (Object::Str(l), other) => Ok(if l.is_empty() { other } else { self }),
            (&Object::Nil, other) => Ok(other),
            (Object::Collection(fields), other) => Ok(if fields.is_empty() { other } else { self }),
            (&Object::Func { .. }, ..) => Ok(self),
            (l, r) => Err(format!("Cannot compare || {:?} to {:?}", l, r)),
        }
    }
}

pub trait Helpers {
    fn concat(self, other: Self) -> Result<Object, String>;
    fn len(self) -> Result<Object, String>;
}

impl Helpers for Object {
    fn concat(self, other: Object) -> Result<Object, String> {
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
            _ => Err(format!("Cannot merge {:?} to {:?}", self, other)),
        }
    }

    fn len(self) -> Result<Object, String> {
        match self {
            Object::Str(s) => Ok(Object::Int(s.len() as i128)),
            Object::Sequence(values) => Ok(Object::Int(values.len() as i128)),
            Object::Collection(fields) => Ok(Object::Int(fields.len() as i128)),
            v => Err(format!("Cannot get len of: <{:?}>", v)),
        }
    }
}

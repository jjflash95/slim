mod builtins;
pub mod context;
mod helpers;
mod operations;

use crate::parser::Expr;
use crate::parser::Token;

use std::collections::HashMap;

use context::MutRef;
use context::Mutate;
use context::RuntimeContext;
use context::INSTANCE;
use operations::BinOps;
use operations::UnaryOps;

use builtins::BuiltinFunc;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Clone)]
pub enum InnerReference {
    Access(String, Vec<Value>),
    Identifier(String),
}

impl Into<(String, Vec<Value>)> for InnerReference {
    fn into(self) -> (String, Vec<Value>) {
        match self {
            InnerReference::Access(name, path) => (name, path),
            InnerReference::Identifier(name) => (name, vec![]),
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i128),
    Float(f64),
    Ref(InnerReference),
    StringLiteral(String),
    Collection(HashMap<String, Value>),
    Sequence(Vec<Value>),
    Builtin(BuiltinFunc),
    Func {
        name: Option<String>,
        params: Vec<String>,
        locals: Option<HashMap<String, Value>>,
        body: Vec<Expr>,
    },
    Break(Box<Value>),
    Continue,
}

impl Value {
    pub fn resolve_inner(self, ctx: &RuntimeContext) -> EResult {
        match self {
            Value::Sequence(seq) => {
                let mut items = vec![];
                for item in seq.into_iter() {
                    items.push(item.resolve(ctx)?.resolve_inner(ctx)?);
                }
                Ok(Value::Sequence(items))
            }
            Value::Collection(fields) => {
                let mut items = HashMap::new();
                for (key, value) in fields.into_iter() {
                    items.insert(key, value.resolve(ctx)?.resolve_inner(ctx)?);
                }
                Ok(Value::Collection(items))
            }
            v => Ok(v),
        }
    }

    pub fn resolve(mut self, ctx: &RuntimeContext) -> EResult {
        while let Value::Ref(inner) = &self {
            match inner {
                InnerReference::Identifier(name) => {
                    self = ctx
                        .borrow()
                        .get_cloned(name)
                        .ok_or(RuntimeError(format!("No variable named: {:?}", name)))?;
                }
                InnerReference::Access(name, path) => {
                    self = eval_flattened_access(name.to_owned(), path.to_owned(), ctx)?;
                }
            }
        }
        Ok(self)
    }

    pub fn type_hint(&self) -> String {
        match self {
            Value::Nil => "nil",
            Value::Bool(..) => "bool",
            Value::Int(..) => "int",
            Value::Float(..) => "float",
            Value::StringLiteral(..) => "string",
            Value::Collection(..) => "collection",
            Value::Sequence(..) => "sequence",
            Value::Func { .. } => "function",
            Value::Break(..) => "break",
            Value::Ref(..) => "ref",
            Value::Continue => "continue",
            Value::Builtin(..) => "builtin",
        }
        .to_owned()
    }
    pub fn to_bool(self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => b,
            Value::Int(n) => n != 0,
            Value::Float(n) => n != 0.0,
            Value::StringLiteral(s) => !s.is_empty(),
            Value::Collection(fields) => !fields.is_empty(),
            Value::Func { .. } => true,
            _ => panic!("Should never come to this"),
        }
    }

    pub fn as_collection_mut(&mut self) -> RtResult<&mut HashMap<String, Value>> {
        if let Value::Collection(fields) = self {
            Ok(fields)
        } else {
            Err(RuntimeError(format!(
                "Error accessing `{}` as a collection",
                self.type_hint()
            )))
        }
    }

    pub fn as_sequence_mut(&mut self) -> RtResult<&mut Vec<Value>> {
        if let Value::Sequence(items) = self {
            Ok(items)
        } else {
            Err(RuntimeError(format!(
                "Error accessing `{}` as a sequence",
                self.type_hint()
            )))
        }
    }

    pub fn into_vec(&self) -> RtResult<Vec<Value>> {
        match self {
            Value::Sequence(items) => Ok(items.clone()),
            Value::Collection(fields) => {
                let mut items = vec![];
                for (key, v) in fields.iter() {
                    let v = Value::Collection(HashMap::from([
                        ("key".to_string(), Value::StringLiteral(key.clone())),
                        ("value".to_string(), v.clone()),
                    ]));
                    items.push(v);
                }
                Ok(items)
            }
            Value::StringLiteral(s) => Ok(s
                .clone()
                .chars()
                .map(|c| Value::StringLiteral(c.to_string()))
                .collect()),
            _ => Err(RuntimeError(format!(
                "Error trying to iterate on `{}`",
                self.type_hint()
            ))),
        }
    }
}

pub type EResult = Result<Value, RuntimeError>;
pub type RtResult<T> = Result<T, RuntimeError>;

#[derive(Debug, PartialEq, Eq)]
pub struct RuntimeError(pub String);

pub trait PeekInstance {
    /*
    for a given recursive
    Access {
        Access {
            Access {
                instance,
                field1
            }
            field2,
        }
        field3
    }
    return the instance name_value
    */
    fn peek_instance(&self) -> Option<String>;
}

impl PeekInstance for Expr {
    fn peek_instance(&self) -> Option<String> {
        let mut src = self;

        loop {
            match src {
                Expr::Access { target, .. } => src = target,
                Expr::Call { target, .. } => src = target,
                _ => break,
            }
        }

        if let Expr::Term(Token::Identifier(n)) = src {
            Some(n.to_string())
        } else {
            None
        }
    }
}

impl TryFrom<Token> for Value {
    type Error = RuntimeError;
    fn try_from(token: Token) -> EResult {
        match token {
            Token::Int(n) => Ok(Value::Int(n)),
            Token::Float(n) => Ok(Value::Float(n)),
            Token::StringLiteral(s) => Ok(Value::StringLiteral(s)),
            Token::True => Ok(Value::Bool(true)),
            Token::False => Ok(Value::Bool(false)),
            Token::Nil => Ok(Value::Nil),
            _ => Err(RuntimeError(format!(
                "Cannot directly convert token `{:?}` to value",
                token
            ))),
        }
    }
}

pub fn eval_materialized_term(token: Token, ctx: RuntimeContext) -> EResult {
    match token {
        Token::Identifier(name) => match ctx.borrow().get_cloned(name.as_ref()) {
            Some(inner) => Ok(inner),
            _ => Err(RuntimeError(format!("No variable named: {:?}", name))),
        },
        Token::_Self => match ctx.borrow().get_cloned(INSTANCE) {
            Some(inner) => Ok(inner),
            None => Err(RuntimeError("No instance in scope".to_string())),
        },
        _ => token.try_into(),
    }
}

pub fn eval(expr: Expr, ctx: RuntimeContext) -> EResult {
    match expr {
        Expr::Deref(r) => eval_deref(*r, ctx),
        Expr::Ref(r) => eval_ref(*r, ctx),
        Expr::Term(t) => eval_term(t, ctx),
        Expr::Return(expr) => eval_return(*expr, ctx),
        Expr::Break(expr) => eval_break(*expr, ctx),
        Expr::Sequence(items) => eval_sequence(items, ctx),
        Expr::Collection(fields) => eval_collection(fields, ctx),
        Expr::Call { target, args } => eval_call(*target, args, ctx),
        Expr::Pipe { parent, child } => eval_pipe(*parent, *child, ctx),
        Expr::Func { name, args, body } => eval_func(name, args, body, ctx),
        Expr::Assign { target, value } => eval_assign(*target, *value, ctx),
        Expr::Binary { op, left, right } => eval_binary(op, *left, *right, ctx),
        Expr::Unary { op, value } => eval_unary(op, *value, ctx),
        Expr::Access { target, field } => eval_access(*target, *field, ctx),
        Expr::Conditional { branches } => eval_branches(branches, ctx),
        Expr::Loop { body } => eval_loop(body, ctx),
        Expr::For { pin, value, body } => eval_for(*pin, *value, body, ctx),
        Expr::While { pin, body } => eval_while(*pin, body, ctx),
        Expr::Continue => Ok(Value::Continue),
        Expr::Nil => Ok(Value::Nil),
    }
}

fn eval_pipe(parent: Expr, child: Expr, ctx: RuntimeContext) -> EResult {
    let mut _args = vec![child];
    let f = match parent {
        Expr::Term(..) => parent,
        Expr::Call { target, args } => {
            _args.extend(args.into_iter());
            *target
        }
        _ => Err(RuntimeError("XFD".into()))?,
    };

    eval_call(f, _args, ctx)
}

fn eval_deref(_ref: Expr, ctx: RuntimeContext) -> EResult {
    eval(_ref, ctx.mut_ref())?
        .resolve(&ctx)?
        .resolve_inner(&ctx)
}

fn eval_ref(mut _ref: Expr, ctx: RuntimeContext) -> EResult {
    while let Expr::Ref(inner) = _ref {
        _ref = *inner;
    }
    match _ref {
        Expr::Term(Token::Identifier(inner)) => Ok(Value::Ref(InnerReference::Identifier(inner))),
        Expr::Access { .. } => {
            let (name, path) = get_access_path(_ref, ctx.mut_ref())?;
            Ok(Value::Ref(InnerReference::Access(name, path)))
        }
        _ => Err(RuntimeError(format!(
            "Cannot reference immutable data of type <{:?}>",
            _ref
        ))),
    }
}

fn eval_while(pin: Expr, body: Vec<Expr>, ctx: RuntimeContext) -> EResult {
    let mut last = Value::Nil;

    while eval(pin.clone(), ctx.mut_ref())?.to_bool() {
        last = eval_body(body.clone(), ctx.mut_ref())?;
        if ctx.borrow().has_eval() {
            return Ok(last);
        }

        match last {
            Value::Break(v) => return Ok(*v),
            _ => {}
        };
    }
    Ok(last)
}

fn eval_for(pin: Expr, value: Expr, body: Vec<Expr>, ctx: RuntimeContext) -> EResult {
    let mut last = Value::Nil;
    let pin = if let Expr::Term(Token::Identifier(name)) = pin {
        Some(name)
    } else {
        None
    };
    let sequence = eval(value, ctx.mut_ref())?.resolve(&ctx)?;
    for item in sequence.into_vec()?.into_iter() {
        if let Some(ref pin) = pin {
            ctx.borrow_mut().set(pin.to_string(), item.clone());
        }

        last = eval_body(body.clone(), ctx.mut_ref())?;
        if ctx.borrow().has_eval() {
            return Ok(last);
        }
        match last {
            Value::Break(v) => return Ok(*v),
            _ => {}
        };
    }
    Ok(last)
}

fn eval_loop(body: Vec<Expr>, ctx: RuntimeContext) -> EResult {
    loop {
        let res = eval_body(body.clone(), ctx.mut_ref())?;
        if ctx.borrow().has_eval() {
            return Ok(Value::Nil);
        }
        match res {
            Value::Break(v) => break Ok(*v),
            _ => {}
        }
    }
}

fn eval_body(body: Vec<Expr>, ctx: RuntimeContext) -> EResult {
    let exprs = body.into_iter();
    let mut last = Value::Nil;
    for e in exprs {
        last = eval(e, ctx.mut_ref())?;
        match last {
            Value::Break(..) | Value::Continue => return Ok(last),
            _ => {}
        };
    }
    Ok(last)
}

fn eval_branches(branches: Vec<(Expr, Vec<Expr>)>, ctx: RuntimeContext) -> EResult {
    for (cond, body) in branches.into_iter() {
        if eval(cond, ctx.mut_ref())?.to_bool() {
            let res = eval_body(body, ctx.mut_ref())?;
            if ctx.borrow().has_eval() {
                return Ok(last);
            }
            return Ok(res);
        }
    }
    Ok(Value::Nil)
}

fn eval_call(target: Expr, args: Vec<Expr>, ctx: RuntimeContext) -> EResult {
    /*
    for a given
    my_seq: [1,2,3]
    func mutate_first_element(inner_seq) {
        inner_seq[0]: 'mutated_element'
    }
    mutate_first_element(my_seq)

    we want to be able to mutate the outer sequence directly
    so we store a reference to the outer sequence in the inner context
    so when we try to evaluate `inner_seq` we end up fetching `my_seq`
    from the parent context mutably, allowing to mutate the data structure
    directly without having to allocate extra memory or keep track of
    mutated elements, this just happens in mutable elements like
    Collection and Sequence, since the other types do not allow for mutations
    and are instead copied by value if reassigned
    */

    let instance = target.peek_instance();
    let value = eval(target, ctx.mut_ref())?.resolve(&ctx)?;
    if let Value::Func {
        name,
        params,
        locals,
        body,
    } = value
    {
        if params.len() != args.len() {
            Err(RuntimeError(format!(
                "Error calling function {:?}, expected {:?} params but got {:?} instead",
                name.unwrap_or("<anonymous>".to_owned()),
                params.len(),
                args.len()
            )))?;
        }

        let inner_ctx = RuntimeContext::default();
        inner_ctx.borrow_mut().set_parent(ctx.mut_ref());
        let mut params = params.into_iter();
        if let Some(locals) = locals {
            inner_ctx.borrow_mut().set_locals(locals);
        };
        for arg in args.into_iter() {
            let param = params.next().unwrap();
            if let Expr::Term(Token::Identifier(n)) = arg {
                if param != n {
                    // avoid circular resolve
                    inner_ctx
                        .borrow_mut()
                        .set(param, Value::Ref(InnerReference::Identifier(n)));
                }
            } else {
                inner_ctx.borrow_mut().set(param, eval(arg, ctx.mut_ref())?);
            }
        }

        if let Some(instance_name) = instance {
            inner_ctx.borrow_mut().set(
                INSTANCE.to_owned(),
                Value::Ref(InnerReference::Identifier(instance_name)),
            );
        }

        let exprs = body.into_iter();

        for e in exprs {
            eval(e, inner_ctx.mut_ref())?;
            if inner_ctx.borrow().has_eval() {
                break;
            }
        }

        Ok(inner_ctx.take().consume())
    } else if let Value::Builtin(builtin) = value {
        let mut args = args
            .into_iter()
            .map(|a| eval(a, ctx.mut_ref())?.resolve(&ctx))
            .collect::<RtResult<Vec<Value>>>()?;
        Ok(builtin(&mut args, ctx.mut_ref())?)
    } else {
        Err(RuntimeError(format!(
            "Cannot call as function -> {:?}",
            value
        )))
    }
}

fn eval_flattened_access(target: String, path: Vec<Value>, ctx: &RuntimeContext) -> EResult {
    let mut src = ctx.borrow().get_cloned(&target).unwrap().resolve(ctx)?;
    for item in path.into_iter() {
        match item {
            Value::Int(i) => {
                let items = src.as_sequence_mut()?;
                src = items.get(i as usize).cloned().ok_or_else(|| {
                    RuntimeError(format!("Index {:?} out of bounds for sequence", i))
                })?;
            }
            Value::StringLiteral(s) => {
                let fields = src.as_collection_mut()?;
                src = fields
                    .entry(s)
                    .or_insert(Value::Collection(HashMap::new()))
                    .clone();
            }
            _ => Err(RuntimeError(format!("Cannot access path on: {:?}", src)))?,
        }
    }
    Ok(src)
}

fn eval_access(target: Expr, field: Expr, ctx: RuntimeContext) -> EResult {
    let src = eval(target, ctx.mut_ref())?.resolve(&ctx)?;
    let key = eval(field, ctx.mut_ref())?.resolve(&ctx)?;
    match key {
        Value::Int(i) => {
            if let Value::Sequence(items) = src {
                match items.get(i as usize).cloned() {
                    Some(item) => Ok(item),
                    None => Err(RuntimeError(format!(
                        "Sequence index: {} is out of bounds",
                        i
                    ))),
                }
            } else {
                Err(RuntimeError(format!(
                    "Cannot index `{:?}` on `{:?}`",
                    i, src
                )))
            }
        }
        Value::StringLiteral(name) => {
            if let Value::Collection(fields) = src {
                match fields.get(&name).cloned() {
                    Some(val) => Ok(val),
                    None => Err(RuntimeError(format!("Collection has no field: {:?}", name))),
                }
            } else {
                Err(RuntimeError(format!(
                    "Cannot access field `{}` on `{}`",
                    name,
                    src.type_hint()
                )))
            }
        }
        v => Err(RuntimeError(format!(
            "Cannot use `{}` to access {}",
            v,
            src.type_hint()
        )))?,
    }
}

fn eval_sequence(items: Vec<Expr>, ctx: RuntimeContext) -> EResult {
    let items = items
        .into_iter()
        .map(|i| eval(i, ctx.mut_ref()))
        .collect::<RtResult<Vec<Value>>>()?;

    let mut v = Vec::with_capacity(10000);
    for item in items {
        v.push(item)
    }
    Ok(Value::Sequence(v))
}

fn eval_break(expr: Expr, ctx: RuntimeContext) -> EResult {
    let value = eval(expr, ctx.mut_ref())?.resolve(&ctx)?;
    Ok(Value::Break(value.into()))
}

fn eval_return(expr: Expr, ctx: RuntimeContext) -> EResult {
    if ctx.borrow().parent.is_none() {
        return Err(RuntimeError(
            "Cannot call return outside a function".to_owned(),
        ));
    }
    let value = match expr {
        Expr::Term(Token::_Self) => eval(expr, ctx.mut_ref())?,
        expr => eval(expr, ctx.mut_ref())?.resolve(&ctx)?,
    };

    ctx.borrow_mut().set_eval(value);
    Ok(Value::Nil)
}

fn eval_unary(op: Token, value: Expr, ctx: RuntimeContext) -> EResult {
    let value = eval(value, ctx.mut_ref())?.resolve(&ctx)?;
    value.unary(op)
}

fn eval_collection(fields: Vec<Expr>, ctx: RuntimeContext) -> EResult {
    let fields = fields
        .into_iter()
        .map(|e| {
            if let Expr::Assign { target, value } = e {
                if let Expr::Term(Token::Identifier(name)) = *target {
                    return Ok((name, eval(*value, ctx.mut_ref())?));
                }
                Err(RuntimeError(format!(
                    "Expected term inside collection but found: {:?}",
                    target
                )))
            } else {
                Err(RuntimeError(format!(
                    "Expected assign inside collection, found: {:?}",
                    e
                )))
            }
        })
        .collect::<RtResult<HashMap<String, Value>>>()?;
    Ok(Value::Collection(fields))
}

fn eval_func(
    name: Option<String>,
    params: Vec<Expr>,
    body: Vec<Expr>,
    ctx: RuntimeContext,
) -> EResult {
    let params = params
        .into_iter()
        .map(|a| {
            if let Expr::Term(Token::Identifier(arg)) = a {
                Ok(arg)
            } else {
                Err(RuntimeError(format!("Expected arg but found: {:?}", a)))
            }
        })
        .collect::<RtResult<Vec<String>>>()?;

    let locals = if let Some(_) = ctx.borrow().parent {
        Some(ctx.borrow().get_locals())
    } else {
        None
    };

    let func = Value::Func {
        name: name.clone(),
        params,
        locals,
        body,
    };

    if let Some(name) = name {
        ctx.borrow_mut().set(name, func.clone());
    };

    Ok(func)
}

fn eval_assign(target: Expr, value: Expr, ctx: RuntimeContext) -> EResult {
    if let Expr::Term(Token::Identifier(name)) = target {
        let value = eval(value, ctx.mut_ref())?;
        ctx.borrow_mut().set(name, value);
        Ok(Value::Nil)
    } else if let Expr::Access { .. } = target {
        let (name, path) = get_access_path(target, ctx.mut_ref())?;
        let value = eval(value, ctx.mut_ref())?.resolve(&ctx)?;
        ctx.borrow_mut().mutate(name, path, value)?;
        Ok(Value::Nil)
    } else if let Expr::Deref(mut _ref) = target {
        while let Expr::Deref(i) = *_ref {
            _ref = i;
        }
        let (name, path) = match *_ref {
            Expr::Term(Token::Identifier(inner)) => (inner, vec![]),
            Expr::Access { .. } => get_access_path(*_ref, ctx.mut_ref())?,
            _ => Err(RuntimeError(format!(
                "Cannot reference immutable data of type <{:?}>",
                _ref
            )))?,
        };
        let value = eval(value, ctx.mut_ref())?.resolve(&ctx)?;
        ctx.borrow_mut().mutate(name, path, value)?;
        Ok(Value::Bool(true))
    } else {
        Err(RuntimeError(format!(
            "Tried to assign to non identifier: {:?}",
            target
        )))
    }
}

fn eval_term(token: Token, ctx: RuntimeContext) -> EResult {
    match token {
        Token::Identifier(name) => ctx
            .borrow()
            .get_cloned(name.as_ref())
            .ok_or_else(|| RuntimeError(format!("No variable named: {:?}", name))),
        Token::_Self => match ctx.borrow().get_cloned(INSTANCE) {
            Some(inner) => Ok(inner),
            None => Err(RuntimeError("No instance in scope".to_string())),
        },
        _ => token.try_into(),
    }
}

fn eval_binary(op: Token, left: Expr, right: Expr, ctx: RuntimeContext) -> EResult {
    let left = eval(left, ctx.mut_ref())?.resolve(&ctx)?;
    let right = eval(right, ctx.mut_ref())?.resolve(&ctx)?;
    left.binary(op, right)
}

fn get_access_path(mut src: Expr, ctx: RuntimeContext) -> RtResult<(String, Vec<Value>)> {
    let mut path = vec![];
    while let Expr::Access { target, field } = src {
        match *field {
            Expr::Term(Token::Identifier(..)) | Expr::Binary { .. } | Expr::Access { .. } => {
                path.push(eval(*field, ctx.mut_ref())?)
            }
            Expr::Term(Token::Nil) => path.push(Value::Nil),
            Expr::Term(Token::Int(i)) => path.push(Value::Int(i)),
            Expr::Term(Token::StringLiteral(s)) => path.push(Value::StringLiteral(s)),
            _ => Err(RuntimeError(format!(
                "Expected identifier, found: {:?}",
                field
            )))?,
        }

        match *target {
            Expr::Term(Token::Identifier(name)) => {
                let path = match ctx.borrow().type_hint(&name, &ctx) {
                    Some(t) if t == "collection" => Ok(path),
                    Some(t) if t == "sequence" => Ok(path),
                    Some(t) => Err(RuntimeError(format!(
                        "Cannot access path on <{}> of type <{}>",
                        name, t
                    )))?,
                    None => Err(RuntimeError(format!(
                        "Cannot access path on <{}> of unknown type",
                        name
                    )))?,
                }?;
                return Ok((name, path));
            }
            Expr::Term(Token::_Self) => {
                match ctx.borrow().type_hint(INSTANCE, &ctx) {
                    Some(t) if t == "collection" => {}
                    Some(t) => Err(RuntimeError(format!(
                        "Cannot access path on {:?} of type {:?}",
                        INSTANCE, t
                    )))?,
                    None => Err(RuntimeError(format!(
                        "Cannot access path on {:?} of unknown type",
                        INSTANCE
                    )))?,
                }
                return Ok((INSTANCE.to_string(), path));
            }
            _ => {}
        }
        src = *target;
    }
    Err(RuntimeError(format!(
        "Expected access path, found: {:?}",
        src
    )))
}

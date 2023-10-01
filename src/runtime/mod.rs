pub mod builtins;
pub mod object;
mod operations;
pub mod scope;

use crate::slim::{get_parse_err_msg, execute_tree};
use std::fs;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::{Expr, Statement, Token, Block, TokenValue, Span, parse_ast, ParseError};
use crate::runtime::object::{ObjectRef, TraitDef};
use crate::runtime::scope::Scope;

use self::object::{BuiltinFunc, Object, ToObject};
use self::operations::{BinOps, UnaryOps};

#[derive(Debug)]
pub struct RuntimeError(pub Span, pub String);

pub type EResult<T> = Result<T, RuntimeError>;

#[macro_export]
macro_rules! nil {
    () => {
        Ok(Rc::new(RefCell::new(Object::Nil)))
    };
}

#[macro_export]
macro_rules! rt_err {
    ($span: ident, $template:literal $(,)?) => {
        Err(RuntimeError($span.to_owned(), String::from($template)))
    };
    ($span: ident, $template:literal, $($expr:expr),+ $(,)?) => {
        Err(RuntimeError($span.to_owned(), format!($template, $($expr),+)))
    };
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Interrupts {
    Return,
    Break,
    Continue,
}

#[derive(PartialEq, Debug)]
pub enum Bubble {
    Return(ObjectRef),
    Break(ObjectRef),
    Eval(ObjectRef),
    Continue,
}

#[cfg(test)]
mod tests;

pub fn evaluate_stmt(scope: &mut Scope, stm: &Statement) -> EResult<()> {
    match stm {
        Statement::Import { span, path, name } => eval_import(scope, span, path, name),
        Statement::ImportNames { span, path, names } => eval_import_names(scope, span, path, names),
        Statement::DefStruct { span, name, props } => eval_def_struct(scope, span, name, props),
        Statement::ImplFor { span, name, target } => eval_impl_for(scope, span, name, target),
        Statement::Trait { span, name, methods } => eval_declare_trait(scope, span, name, methods),
        Statement::Impl { span, target, methods } => eval_impl(scope, span, target, methods),
    }
}

pub fn evaluate(scope: &mut Scope, expr: &Expr) -> EResult<ObjectRef> {
    match expr {
        Expr::Nil(_) => nil!(),
        Expr::Struct { span, name, props } => eval_struct(scope, span, name, props),
        Expr::Pipe { span, parent, child } => eval_pipe(scope, span, parent, child),
        Expr::Call { span, target, args } => eval_call(scope, span, target, args),
        Expr::Func { span, name, params, body } => eval_func(scope, span, name.as_deref(), params, body),
        Expr::Loop { span, body } => {
            match eval_while_with(scope, span, &Expr::Term(span.to_owned(), TokenValue::True), body, &mut vec![])? {
                Bubble::Eval(v) | Bubble::Break(v) => Ok(v),
                bubble => rt_err!(span, "Unexpected {:?}", bubble),
            }
        }
        Expr::For {
            span,
            pin,
            iterable,
            body,
        } => match eval_for_with(scope, span, pin, iterable, body, &mut vec![])? {
            Bubble::Break(v) | Bubble::Eval(v) => Ok(v),
            bubble => rt_err!(span, "Unexpected {:?}", bubble),
        },
        Expr::Conditional { span, branches } => match eval_branches_with(scope, span, branches, &mut vec![])? {
            Bubble::Eval(v) => Ok(v),
            bubble => rt_err!(span, "Cannot call {:?} outside a loop/function", bubble),
        },
        Expr::While { span, pin, body } => match eval_while_with(scope, span, pin, body, &mut vec![])? {
            Bubble::Eval(v) | Bubble::Break(v) => Ok(v),
            bubble => rt_err!(span, "Unexpected {:?}", bubble),
        },
        Expr::Scope(span, body) => eval_scope(scope, span, body),
        Expr::Binary { span, op, left, right } => eval_binary(scope, span, op, left, right),
        Expr::Unary { span, op, value } => eval_unary(scope, span, op, value),
        Expr::Deref(_span, ptr) => Ok(evaluate(scope, ptr)?.object().into()),
        Expr::Collection(span, fields) => eval_collection(scope, span, fields),
        Expr::Access { span, target, field } => eval_access(scope, span, target, field),
        Expr::Sequence(_span, items) => eval_sequence(scope, items),
        Expr::Term(span, token) => eval_term(scope, span, token),
        Expr::Assign { span, target, value } => eval_assign(scope, span, target, value),
        _e => todo!(),
    }
}

fn eval_term(scope: &mut Scope, span: &Span, token: &TokenValue) -> EResult<ObjectRef> {
    match token {
        TokenValue::Identifier(ref i) => eval_identifier(scope, span, i),
        TokenValue::_Self => eval_self(scope, span),
        _t => Ok(ObjectRef::try_from(Token { span: span.to_owned(), value: token.to_owned() }).map_err(|s| RuntimeError(span.to_owned(), s))?),
    }
}

fn eval_self(scope: &mut Scope, span: &Span) -> EResult<ObjectRef> {
    scope
        .get("self")
        .ok_or(RuntimeError(span.to_owned(), "No self in scope".into()))
}

fn eval_struct(scope: &mut Scope, span: &Span, name: &str, props: &[Expr]) -> EResult<ObjectRef> {
    let rules = scope
        .get_struct_def(name)
        .ok_or_else(|| RuntimeError(span.to_owned(), format!("Struct {} does not exist", name)))?;
    let props = props
        .iter()
        .map(|e| {
            if let Expr::Assign { span, target, value } = e {
                let Expr::Term(ref span, ref t) = **target else {
                    rt_err!(span, "XD")?
                };
                if let TokenValue::Identifier(name) = t {
                    return Ok((name.to_owned(), evaluate(scope, value)?));
                }
                rt_err!(span, "Expected term inside collection but found: {:?}", target)?
            } else {
                rt_err!(span, "Expected assign inside collection, found: {:?}", e)?
            }
        })
        .collect::<EResult<HashMap<String, ObjectRef>>>()?;
    for prop in &rules {
        if !props.contains_key(prop) {
            rt_err!(span, "Missing field {} for struct {}", prop, name)?
        }
    }
    Ok(Object::Struct { name: name.to_string(), rules, props }.into())
}

fn eval_def_struct(scope: &mut Scope, _: &Span, name: &str, props: &[String]) -> EResult<()> {
    scope.add_struct(name, props.into());
    Ok(())
}

fn eval_impl_for(scope: &mut Scope, span: &Span, _name: &Token, _target: &Token) -> EResult<()> {
    match (&_name.value, &_target.value) {
        (TokenValue::Identifier(name), TokenValue::Identifier(target)) => {
            let def = scope
                .trait_defs
                .get(name)
                .cloned()
                .ok_or_else(|| RuntimeError(_target.span.to_owned(), format!(
                    "impl {}: trait {} does not exist",
                    target, name
                )))?;
            let mut methods = HashMap::new();
            for f in def.methods {
                if let Expr::Func {
                    name, params, body, ..
                } = f
                {
                    let name = name.clone().ok_or_else(||RuntimeError(
                        _target.span.to_owned(),
                        "Cannot implement anonymous trait functions".into(),
                    ))?;
                    methods.insert(name, _eval_func(scope, span, None, &params, &body)?.1);
                } else {
                    rt_err!(span, "Found non-function in trait impl")?
                }
            }
            scope.add_trait(target.clone().into(), methods);
            Ok(())
        }
        (n, t) => rt_err!(span, "Cannot impl {:?} for {:?}", n, t),
    }
}

fn eval_impl(scope: &mut Scope, span: &Span, target: &Token, _methods: &[Expr]) -> EResult<()> {
    let TokenValue::Identifier(target) = &target.value else {
        return rt_err!(span, "Cannot eval impl");
    };
    let mut methods = HashMap::new();
    for f in _methods {
        if let Expr::Func {
            name, params, body, ..
        } = f
        {
            let name = name.clone().ok_or(RuntimeError(
                span.to_owned(),
                "Cannot implement anonymous trait functions".into(),
            ))?;
            methods.insert(name, _eval_func(scope, span, None, params, body)?.1);
        } else {
            rt_err!(span, "Found non-function in trait impl")?
        }
    }
    scope.add_trait(target.clone().into(), methods);
    Ok(())
}

fn eval_declare_trait(scope: &mut Scope, span: &Span, name: &Token, methods: &[Expr]) -> EResult<()> {
    let TokenValue::Identifier(name) = &name.value else {
        return rt_err!(span, "Cannot eval trait");
    };
    for m in methods {
        if !matches!(m, Expr::Func { .. }) {
            rt_err!(span, "Trait methods must be functions")?
        }
    }
    scope.add_trait_def(name, TraitDef { methods: methods.into() });
    Ok(())
}

fn eval_pipe(scope: &mut Scope, span: &Span, parent: &Expr, child: &Expr) -> EResult<ObjectRef> {
    let mut arguments = vec![child];
    let f = match parent {
        Expr::Term(..) => parent,
        Expr::Call { span: _, target, args } => {
            arguments.extend(args);
            target
        }
        e => rt_err!(span, "Cannot use: {:?} as callable", e)?,
    };
    let arguments = arguments.into_iter().cloned().collect::<Vec<Expr>>();
    eval_call(scope, span, f, &arguments)
}

fn eval_while_with(
    scope: &mut Scope,
    _span: &Span,
    pin: &Expr,
    body: &[Block],
    interrupts: &mut Vec<Interrupts>,
) -> EResult<Bubble> {
    let mut last: ObjectRef = nil!()?;

    while evaluate(scope, pin)?.borrow().is_truthy() {
        for expr in body {
            match expr {
                Block::Expression(Expr::Return(span, e)) => {
                    if interrupts.contains(&Interrupts::Return) {
                        return Ok(Bubble::Return(evaluate(scope, e)?));
                    }
                    rt_err!(span, "Cannot return from outside a function")?
                }
                Block::Expression(Expr::Break(_span, v)) => return Ok(Bubble::Break(evaluate(scope, v)?)),
                Block::Expression(Expr::Continue(_span)) => break,
                Block::Expression(Expr::Loop { span, body }) => {
                    match eval_while_with(scope, span, &Expr::Term(span.to_owned(), TokenValue::True), body, interrupts)? {
                        Bubble::Return(v) => {
                            if interrupts.contains(&Interrupts::Return) {
                                return Ok(Bubble::Return(v));
                            }
                        }
                        Bubble::Break(v) => return Ok(Bubble::Eval(v)),
                        Bubble::Eval(v) => last = v,
                        Bubble::Continue => break,
                    };
                }
                Block::Expression(Expr::For {
                    span,
                    pin,
                    iterable,
                    body,
                }) => {
                    match eval_for_with(scope, span, pin, iterable, body, interrupts)? {
                        Bubble::Return(v) => {
                            if interrupts.contains(&Interrupts::Return) {
                                return Ok(Bubble::Return(v));
                            }
                        }
                        Bubble::Break(v) => return Ok(Bubble::Eval(v)),
                        Bubble::Eval(v) => last = v,
                        Bubble::Continue => break,
                    };
                }
                Block::Expression(Expr::While { span, pin, body }) => {
                    match eval_while_with(scope, span, pin, body, interrupts)? {
                        Bubble::Return(v) => {
                            if interrupts.contains(&Interrupts::Return) {
                                return Ok(Bubble::Return(v));
                            }
                        }
                        Bubble::Break(v) => return Ok(Bubble::Eval(v)),
                        Bubble::Eval(v) => last = v,
                        Bubble::Continue => break,
                    };
                }
                Block::Expression(Expr::Conditional { span, branches }) => {
                    interrupts.push(Interrupts::Break);
                    interrupts.push(Interrupts::Continue);
                    match eval_branches_with(scope, span, branches, interrupts)? {
                        Bubble::Return(v) => {
                            if interrupts.contains(&Interrupts::Return) {
                                return Ok(Bubble::Return(v));
                            }
                        }
                        Bubble::Break(v) => return Ok(Bubble::Eval(v)),
                        Bubble::Eval(v) => last = v,
                        Bubble::Continue => break,
                    };
                }
                Block::Expression(e) => last = evaluate(scope, e)?,
                Block::Statement(stm) => {
                    evaluate_stmt(scope, stm)?;
                }
            }
        }
    }

    Ok(Bubble::Eval(last))
}

fn eval_for_with(
    scope: &mut Scope,
    span: &Span,
    pin: &Expr,
    iterable: &Expr,
    body: &[Block],
    interrupts: &mut Vec<Interrupts>,
) -> EResult<Bubble> {
    let mut last = nil!()?;

    let sequence = evaluate(scope, iterable)?;
    for item in sequence.object().into_vec().map_err(|s| RuntimeError(span.to_owned(), s))?.into_iter() {
        if let Expr::Term(_, TokenValue::Identifier(ref name)) = pin {
            scope.set(name, item);
        }

        for expr in body.clone() {
            match expr {
                Block::Expression(Expr::Return(span, e)) => {
                    if interrupts.contains(&Interrupts::Return) {
                        return Ok(Bubble::Return(evaluate(scope, e)?));
                    }
                    rt_err!(span, "Cannot return from outside a function")?
                }
                Block::Expression(Expr::Break(_span, v)) => return Ok(Bubble::Break(evaluate(scope, v)?)),
                Block::Expression(Expr::Continue(_span)) => break,
                Block::Expression(Expr::Loop { span, body }) => {
                    match eval_while_with(scope, span, &Expr::Term(span.to_owned(), TokenValue::True), body, interrupts)? {
                        Bubble::Return(v) => {
                            if interrupts.contains(&Interrupts::Return) {
                                return Ok(Bubble::Return(v));
                            }
                        }
                        Bubble::Break(v) => return Ok(Bubble::Eval(v)),
                        Bubble::Eval(v) => last = v,
                        Bubble::Continue => break,
                    };
                }
                Block::Expression(Expr::For {
                    span,
                    pin,
                    iterable,
                    body,
                }) => {
                    match eval_for_with(scope, span, pin, iterable, body, interrupts)? {
                        Bubble::Return(v) => {
                            if interrupts.contains(&Interrupts::Return) {
                                return Ok(Bubble::Return(v));
                            }
                        }
                        Bubble::Break(v) => return Ok(Bubble::Eval(v)),
                        Bubble::Eval(v) => last = v,
                        Bubble::Continue => break,
                    };
                }
                Block::Expression(Expr::While { span, pin, body }) => {
                    match eval_while_with(scope, span, pin, body, interrupts)? {
                        Bubble::Return(v) => {
                            if interrupts.contains(&Interrupts::Return) {
                                return Ok(Bubble::Return(v));
                            }
                        }
                        Bubble::Break(v) => return Ok(Bubble::Eval(v)),
                        Bubble::Eval(v) => last = v,
                        Bubble::Continue => break,
                    };
                }
                Block::Expression(Expr::Conditional { span, branches }) => {
                    interrupts.push(Interrupts::Break);
                    interrupts.push(Interrupts::Continue);
                    match eval_branches_with(scope, span, branches, interrupts)? {
                        Bubble::Return(v) => {
                            if interrupts.contains(&Interrupts::Return) {
                                return Ok(Bubble::Return(v));
                            }
                        }
                        Bubble::Break(v) => return Ok(Bubble::Eval(v)),
                        Bubble::Eval(v) => last = v,
                        Bubble::Continue => break,
                    };
                }
                Block::Expression(e) => last = evaluate(scope, e)?,
                Block::Statement(stm) => {
                    evaluate_stmt(scope, stm)?;
                }
            }
        }
    }
    Ok(Bubble::Eval(last))
}

fn eval_interruptable_expr(
    scope: &mut Scope,
    _span: &Span,
    expr: &Expr,
    interrupts: &mut Vec<Interrupts>,
) -> EResult<Bubble> {
    match expr {
        Expr::Return(span, e) => {
            if interrupts.contains(&Interrupts::Return) {
                return Ok(Bubble::Return(evaluate(scope, e)?));
            }
            rt_err!(span, "Cannot return from outside a function")?
        }
        Expr::Break(span, v) => {
            if interrupts.contains(&Interrupts::Break) {
                return Ok(Bubble::Break(evaluate(scope, v)?));
            }
            rt_err!(span, "Cannot call break from outside a loop")?
        }
        Expr::Continue(span) => {
            if interrupts.contains(&Interrupts::Continue) {
                return Ok(Bubble::Continue);
            }
            rt_err!(span, "Cannot call continue from outside a loop")?
        }
        _ => panic!("Should never come to this"),
    }
}

fn eval_branches_with(
    scope: &mut Scope,
    span: &Span,
    branches: &[(Expr, Vec<Block>)],
    interrupts: &mut Vec<Interrupts>,
) -> EResult<Bubble> {
    for (cond, body) in branches.iter() {
        if evaluate(scope, cond)?.borrow().is_truthy() {
            let mut last = nil!()?;
            for expr in body {
                match expr {
                    Block::Expression(Expr::Return(..)) | Block::Expression(Expr::Break(..)) | Block::Expression(Expr::Continue(..)) => {
                        return eval_interruptable_expr(scope, span, &expr.clone().into_expr_checked(), interrupts)
                    }
                    Block::Expression(Expr::Conditional { span, branches }) => {
                        match eval_branches_with(scope, span, branches, interrupts)? {
                            Bubble::Eval(v) => last = v,
                            bubble => return Ok(bubble),
                        }
                    }
                    Block::Expression(Expr::Loop { span, body }) => {
                        match eval_while_with(scope, span, &Expr::Term(span.to_owned(), TokenValue::True), body, interrupts)? {
                            Bubble::Eval(v) => last = v,
                            bubble => return Ok(bubble),
                        };
                    }
                    Block::Expression(Expr::For {
                        span,
                        pin,
                        iterable,
                        body,
                    }) => match eval_for_with(scope, span, pin, iterable, body, interrupts)? {
                        Bubble::Eval(v) => last = v,
                        bubble => return Ok(bubble),
                    },
                    Block::Expression(e) => {
                        last = evaluate(scope, e)?;
                    },
                    Block::Statement(stm) => {
                        evaluate_stmt(scope, stm)?;
                    }
                }
            }
            return Ok(Bubble::Eval(last));
        }
    }
    Ok(Bubble::Eval(nil!()?))
}

fn eval_binary(scope: &mut Scope, span: &Span, op: &TokenValue, left: &Expr, right: &Expr) -> EResult<ObjectRef> {
    let left = evaluate(scope, left)?;
    let right = evaluate(scope, right)?;
    let left = left.object();
    let right = right.object();
    Ok(left.binary(op, right).map_err(|s| RuntimeError(span.to_owned(), s))?.into())
}

fn eval_scope(scope: &mut Scope, _span: &Span, body: &[Block]) -> EResult<ObjectRef> {
    let mut inner = Scope::default();
    inner.parent = Some(scope.get_ref());

    let mut last = nil!()?;
    for expr in body {
        match expr {
            Block::Expression(Expr::Return(_span, e)) => return evaluate(&mut inner, e),
            Block::Expression(Expr::Loop { span, body }) => match eval_while_with(
                scope,
                span,
                &Expr::Term(span.to_owned(), TokenValue::True),
                body,
                &mut vec![Interrupts::Return],
            )? {
                Bubble::Return(v) => return Ok(v),
                Bubble::Continue => rt_err!(span, "Cannot call continue outside loop")?,
                Bubble::Eval(v) => last = v,
                _ => {} // eval & break get ignored
            },
            Block::Expression(Expr::For {
                span,
                pin,
                iterable,
                body,
            }) => match eval_for_with(
                &mut inner,
                span,
                pin,
                iterable,
                body,
                &mut vec![Interrupts::Return],
            )? {
                Bubble::Return(v) => return Ok(v),
                Bubble::Continue => rt_err!(span, "Cannot call continue outside loop")?,
                Bubble::Eval(v) => last = v,
                _ => {} // eval & break get ignored
            },
            Block::Expression(Expr::Conditional { span, branches }) => {
                match eval_branches_with(&mut inner, span, branches, &mut vec![Interrupts::Return])? {
                    Bubble::Return(v) => return Ok(v),
                    Bubble::Continue => rt_err!(span, "Cannot call continue outside loop")?,
                    Bubble::Break(_) => rt_err!(span, "Cannot call break outside loop")?,
                    Bubble::Eval(v) => last = v,
                    _ => {} // eval & break
                }
            }
            Block::Expression(e) => {
                last = evaluate(&mut inner, e)?;
            },
            Block::Statement(stm) => {
                evaluate_stmt(&mut inner, stm)?;
            }
        }
    }
    Ok(last)
}

fn eval_call_func(
    scope: &mut Scope,
    _: Option<String>,
    params: &[String],
    locals: Option<HashMap<String, ObjectRef>>,
    body: &[Block],
    args: Vec<ObjectRef>,
) -> EResult<ObjectRef> {
    let mut inner = Scope::default();
    inner.parent = Some(scope.get_ref());

    if let Some(locals) = locals {
        inner.store = locals;
    }

    for (param, arg) in params.iter().zip(args) {
        inner.store.insert(param.clone(), arg);
    }

    for expr in body {
        match expr {
            Block::Expression(Expr::Return(_span, e)) => return evaluate(&mut inner, e),
            Block::Expression(Expr::Loop { span, body }) => match eval_while_with(
                &mut inner,
                span,
                &Expr::Term(span.to_owned(), TokenValue::True),
                body,
                &mut vec![Interrupts::Return],
            )? {
                Bubble::Return(v) => return Ok(v),
                Bubble::Continue => rt_err!(span, "Cannot call continue outside loop")?,
                _ => {} // eval & break get ignored
            },
            Block::Expression(Expr::While { span, pin, body }) => {
                match eval_while_with(&mut inner, span, pin, body, &mut vec![Interrupts::Return])? {
                    Bubble::Return(v) => return Ok(v),
                    Bubble::Continue => rt_err!(span, "Cannot call continue outside loop")?,
                    _ => {} // eval & break get ignored
                }
            }
            Block::Expression(Expr::For {
                span,
                pin,
                iterable,
                body,
            }) => match eval_for_with(
                &mut inner,
                span,
                pin,
                iterable,
                body,
                &mut vec![Interrupts::Return],
            )? {
                Bubble::Return(v) => return Ok(v),
                Bubble::Continue => rt_err!(span, "Cannot call continue outside loop")?,
                _ => {} // eval & break get ignored
            },
            Block::Expression(Expr::Conditional { span, branches }) => {
                match eval_branches_with(&mut inner, span, branches, &mut vec![Interrupts::Return])? {
                    Bubble::Return(v) => return Ok(v),
                    Bubble::Continue => rt_err!(span, "Cannot call continue outside loop")?,
                    Bubble::Break(_) => rt_err!(span, "Cannot call break outside loop")?,
                    _ => {} // eval & break
                }
            }
            Block::Expression(e) => {
                evaluate(&mut inner, e)?;
            },
            Block::Statement(stm) => {
                evaluate_stmt(&mut inner, stm)?;
            }
        }
    }

    nil!()
}

fn eval_call(scope: &mut Scope, span: &Span, f: &Expr, args: &[Expr]) -> EResult<ObjectRef> {
    let args = args
        .iter()
        .map(|a| evaluate(scope, a))
        .collect::<EResult<Vec<ObjectRef>>>()?;
    match evaluate(scope, f)?.object() {
        Object::Func {
            name,
            params,
            locals,
            body,
        } => eval_call_func(scope, name, &params, locals, &body, args),
        Object::Builtin(BuiltinFunc(b)) => b(scope, span, args),
        v => rt_err!(span, "Cannot call: {:?} as a function", v),
    }
}

fn _eval_func(
    scope: &mut Scope,
    span: &Span,
    name: Option<&str>,
    params: &[Expr],
    body: &[Block],
) -> EResult<(Option<String>, ObjectRef)> {
    let params = params.iter().map(|a| {
        if let Expr::Term(_span, TokenValue::Identifier(arg)) = a {
            return Ok(arg.clone());
        }
        rt_err!(span, "Expected arg but found: {:?}", a)
    });

    Ok((
        name.map(str::to_string),
        Rc::new(RefCell::new(Object::Func {
            name: name.map(str::to_string),
            params: params.collect::<EResult<Vec<String>>>()?,
            locals: scope.locals(),
            body: body.to_vec(),
        })),
    ))
}

fn eval_func(
    scope: &mut Scope,
    span: &Span,
    name: Option<&str>,
    params: &[Expr],
    body: &[Block],
) -> EResult<ObjectRef> {
    let (name, func) = _eval_func(scope, span, name, params, body)?;

    if let Some(name) = name {
        scope.set(&name, Rc::clone(&func));
    }

    Ok(func)
}

fn eval_unary(scope: &mut Scope, span: &Span, op: &TokenValue, value: &Expr) -> EResult<ObjectRef> {
    let object = evaluate(scope, value)?.object();
    Ok(object.unary(op).map_err(|s| RuntimeError(span.to_owned(), s))?.into())
}

fn eval_identifier(scope: &mut Scope, span: &Span, i: &str) -> EResult<ObjectRef> {
    if let Some(object) = scope.get(i) {
        Ok(object)
    } else {
        rt_err!(span, "No variable named: {}", i)
    }
}

fn eval_assign(scope: &mut Scope, span: &Span, target: &Expr, value: &Expr) -> EResult<ObjectRef> {
    let oref = evaluate(scope, value)?;
    match target {
        Expr::Term(_span, TokenValue::Identifier(name)) => {
            scope.set(name, oref);
            nil!()
        }
        Expr::Access { span, target, field } => {
            let src = eval_access(scope, span, target, field)?;
            src.replace(oref.object());
            nil!()
        }
        Expr::Deref(span, ref inner) => match &**inner {
            Expr::Term(span, TokenValue::Identifier(name)) => {
                let prev = scope
                    .get(name)
                    .ok_or_else(|| RuntimeError(span.to_owned(), format!("No variable named: {}", name)))?;
                prev.replace(oref.object());
                nil!()
            }
            Expr::Term(span, TokenValue::_Self) => {
                let prev = scope
                    .get("self")
                    .ok_or_else(|| RuntimeError(span.to_owned(), "No self in scope".to_string()))?;
                prev.replace(oref.object());
                nil!()
            }
            Expr::Access { span, target, field } => eval_access(scope, span, target, field),
            inner => rt_err!(span, "Cannot deref expression: {:?}", inner),
        },
        _ => rt_err!(span, "Cannot assign to expression: {:?}", target),
    }
}

fn eval_sequence(scope: &mut Scope, items: &[Expr]) -> EResult<ObjectRef> {
    Ok(Object::Sequence(
        items
            .iter()
            .map(|i| evaluate(scope, i))
            .collect::<Result<Vec<ObjectRef>, RuntimeError>>()?,
    )
    .into())
}

fn eval_collection(scope: &mut Scope, span: &Span, items: &[Expr]) -> EResult<ObjectRef> {
    let items = items
        .iter()
        .map(|e| {
            if let Expr::Assign { span, target, value } = e {
                if let Expr::Term(ref _span, TokenValue::Identifier(ref name)) = **target {
                    return Ok((name.to_owned(), evaluate(scope, value)?));
                }
                rt_err!(span, "Expected term inside collection but found: {:?}", target)?
            } else {
                rt_err!(span, "Expected assign inside collection, found: {:?}", e)?
            }
        })
        .collect::<EResult<HashMap<String, ObjectRef>>>()?;
    Ok(Object::Collection(items).into())
}

fn eval_access(scope: &mut Scope, span: &Span, target: &Expr, field: &Expr) -> EResult<ObjectRef> {
    let target = evaluate(scope, target)?;
    let field = evaluate(scope, field)?;
    let src = &mut *target.borrow_mut();
    let index = field.object();
    if let Ok(inner) = src.access(index) {
        Ok(inner)
    } else {
        let t = src.get_trait(scope, field).map_err(|s| RuntimeError(span.to_owned(), s))?;
        if let Object::Func {
            name,
            params,
            locals,
            body,
        } = &*t.borrow()
        {
            let mut locals = locals.clone().unwrap_or_default();
            locals.insert("self".into(), Rc::clone(&target));
            return Ok(Object::Func {
                name: name.clone(),
                params: params.clone(),
                locals: Some(locals),
                body: body.clone(),
            }
            .into());
        };
        Ok(t)
    }
}

fn eval_import_names(scope: &mut Scope, span: &Span, path: &str, names: &[String]) -> EResult<()> {
    let mut temp = Scope::default();
    let program = fs::read_to_string(path).map_err(|e| RuntimeError(span.to_owned(), e.to_string()))?;
    let ast = match parse_ast(&program) {
        Ok(ast) => ast,
        Err(ParseError::Interrupt(e, t)) => {
            Err(RuntimeError(span.to_owned(), get_parse_err_msg(t, e, &program)))?
        }
        Err(ParseError::Continue) => {
            panic!("bug in parser")
        }
    };

    execute_tree(&mut temp, &ast)?;
    for name in names {
        if let Some(object) = temp.get(name) {
            scope.set(name, object);
        } else {
            rt_err!(span, "No variable named: {}", name)?
        }
    };

    scope.trait_defs.extend(temp.trait_defs);
    scope.trait_impls.extend(temp.trait_impls);
    Ok(())
}

fn eval_import(scope: &mut Scope, span: &Span, path: &str, name: &str) -> EResult<()> {
    let mut temp = Scope::default();
    let program = fs::read_to_string(path).map_err(|e| RuntimeError(span.to_owned(), e.to_string()))?;
    let ast = match parse_ast(&program) {
        Ok(ast) => ast,
        Err(ParseError::Interrupt(e, t)) => {
            Err(RuntimeError(span.to_owned(), get_parse_err_msg(t, e, &program)))?
        }
        Err(ParseError::Continue) => {
            panic!("bug in parser")
        }
    };

    execute_tree(&mut temp, &ast)?;
    scope.set(name, Object::Collection(temp.store).into());
    scope.trait_defs.extend(temp.trait_defs);
    scope.trait_impls.extend(temp.trait_impls);
    Ok(())
}
pub mod builtins;
pub mod object;
mod operations;
pub mod scope;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::{Expr, Token, Statement};
use crate::runtime::object::{ObjectRef, TraitDef};
use crate::runtime::scope::Scope;

use self::object::{BuiltinFunc, Object, ToObject, StructProps};
use self::operations::{BinOps, UnaryOps};

#[derive(Debug, PartialEq, Eq)]
pub struct RuntimeError(pub String);

pub type EResult<T> = Result<T, RuntimeError>;

#[macro_export]
macro_rules! nil {
    () => {
        Ok(Rc::new(RefCell::new(Object::Nil)))
    };
}

#[macro_export]
macro_rules! rt_err {
    ($template:literal $(,)?) => {
        Err(RuntimeError(String::from($template)))
    };
    ($template:literal, $($expr:expr),+ $(,)?) => {
        Err(RuntimeError(format!($template, $($expr),+)))
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

pub fn evaluate_stmt(scope: &mut Scope, stm: Statement) -> EResult<()> {
    match stm {
        Statement::DefStruct { name, props } => eval_def_struct(scope, name, props),
        Statement::ImplFor { name, target } => eval_impl_for(scope, name, target),
        Statement::Trait { name, methods } => eval_declare_trait(scope, name, methods),
        Statement::Impl { target, methods } => eval_impl(scope, target, methods),
    }
}

pub fn evaluate(scope: &mut Scope, expr: Expr) -> EResult<ObjectRef> {
    match expr {
        Expr::Nil => nil!(),
        Expr::Struct { name, props } => eval_struct(scope, name, props),
        Expr::Pipe { parent, child } => eval_pipe(scope, *parent, *child),
        Expr::Call { target, args } => eval_call(scope, *target, args),
        Expr::Func { name, params, body } => eval_func(scope, name, params, body),
        Expr::Loop { body } => {
            match eval_while_with(scope, Expr::Term(Token::True), body, &mut vec![])? {
                Bubble::Eval(v) | Bubble::Break(v) => Ok(v),
                bubble => rt_err!("Unexpected {:?}", bubble),
            }
        }
        Expr::For {
            pin,
            iterable,
            body,
        } => match eval_for_with(scope, *pin, *iterable, body, &mut vec![])? {
            Bubble::Break(v) | Bubble::Eval(v) => Ok(v),
            bubble => rt_err!("Unexpected {:?}", bubble),
        },
        Expr::Conditional { branches } => match eval_branches_with(scope, branches, &mut vec![])? {
            Bubble::Eval(v) => Ok(v),
            bubble => rt_err!("Cannot call {:?} outside a loop/function", bubble),
        },
        Expr::While { pin, body } => match eval_while_with(scope, *pin, body, &mut vec![])? {
            Bubble::Eval(v) | Bubble::Break(v) => Ok(v),
            bubble => rt_err!("Unexpected {:?}", bubble),
        },
        Expr::Scope(body) => eval_scope(scope, body),
        Expr::Binary { op, left, right } => eval_binary(scope, op, *left, *right),
        Expr::Unary { op, value } => eval_unary(scope, op, *value),
        Expr::Deref(ptr) => Ok(evaluate(scope, *ptr)?.object().into()),
        Expr::Collection(fields) => eval_collection(scope, fields),
        Expr::Access { target, field } => eval_access(scope, *target, *field),
        Expr::Sequence(items) => eval_sequence(scope, items),
        Expr::Term(Token::Identifier(i)) => eval_identifier(scope, i),
        Expr::Term(Token::_Self) => eval_self(scope),
        Expr::Term(token) => Ok(TryInto::try_into(token)?),
        Expr::Assign { target, value } => eval_assign(scope, *target, *value),
        _ => todo!(),
    }
}

fn eval_self(scope: &mut Scope) -> EResult<ObjectRef> {
    scope
        .get("self")
        .ok_or(RuntimeError("No self in scope".into()))
}

fn eval_struct(scope: &mut Scope, name: String, props: Vec<Expr>) -> EResult<ObjectRef> {
    let rules = scope.get_struct_def(&name).ok_or(RuntimeError(format!("Struct {} does not exist", name)))?;
    let props = props
    .into_iter()
    .map(|e| {
        if let Expr::Assign { target, value } = e {
            if let Expr::Term(Token::Identifier(name)) = *target {
                return Ok((name, evaluate(scope, *value)?));
            }
            rt_err!("Expected term inside collection but found: {:?}", target)?
        } else {
            rt_err!("Expected assign inside collection, found: {:?}", e)?
        }
    })
    .collect::<EResult<HashMap<String, ObjectRef>>>()?;
    for prop in &rules {
        if !props.contains_key(prop) {
            rt_err!("Missing field {} for struct {}", prop, name)?
        }
    }
    Ok(Object::Struct {
        name,
        rules,
        props,
    }.into())
}

fn eval_def_struct(scope: &mut Scope, name: String, props: StructProps) -> EResult<()> {
    scope.add_struct(name, props);
    Ok(())
}

fn eval_impl_for(scope: &mut Scope, name: Token, target: Token) -> EResult<()> {
    match (name, target) {
        (Token::Identifier(name), Token::Identifier(target)) => {
            let def = scope
                .trait_defs
                .get(&name)
                .cloned()
                .ok_or(RuntimeError(format!("impl {}: trait {} does not exist", target, name)))?;
            let mut methods = HashMap::new();
            for f in def.methods {
                if let Expr::Func {
                    name, params, body, ..
                } = f
                {
                    let name = name.ok_or(RuntimeError(
                        "Cannot implement anonymous trait functions".into(),
                    ))?;
                    methods.insert(name, _eval_func(scope, None, params, body)?.1);
                } else {
                    rt_err!("Found non-function in trait impl")?
                }
            }
            scope.add_trait(target.into(), methods);
            Ok(())
        }
        (n, t) => rt_err!("Cannot impl {:?} for {:?}", n, t),
    }
}

fn eval_impl(scope: &mut Scope, target: Token, _methods: Vec<Expr>) -> EResult<()> {
    if let Token::Identifier(target) = target {
        let mut methods = HashMap::new();
        for f in _methods {
            if let Expr::Func {
                name, params, body, ..
            } = f
            {
                let name = name.ok_or(RuntimeError(
                    "Cannot implement anonymous trait functions".into(),
                ))?;
                methods.insert(name, _eval_func(scope, None, params, body)?.1);
            } else {
                rt_err!("Found non-function in trait impl")?
            }
        }
        scope.add_trait(target.into(), methods);
        return Ok(());
    }
    rt_err!("Cannot eval direct impl")
}

fn eval_declare_trait(scope: &mut Scope, name: Token, methods: Vec<Expr>) -> EResult<()> {
    if let Token::Identifier(name) = name {
        for m in &methods {
            if !matches!(m, Expr::Func { .. }) {
                rt_err!("Trait methods must be functions")?
            }
        }
        scope.trait_defs.insert(name, TraitDef { methods });
        return Ok(());
    }
    rt_err!("Cannot eval trait")
}

fn eval_pipe(scope: &mut Scope, parent: Expr, child: Expr) -> EResult<ObjectRef> {
    let mut arguments = vec![child];
    let f = match parent {
        Expr::Term(..) => parent,
        Expr::Call { target, args } => {
            arguments.extend(args.into_iter());
            *target
        }
        e => rt_err!("Cannot use: {:?} as callable", e)?,
    };
    eval_call(scope, f, arguments)
}

fn eval_while_with(
    scope: &mut Scope,
    pin: Expr,
    body: Vec<Expr>,
    interrupts: &mut Vec<Interrupts>,
) -> EResult<Bubble> {
    let mut last: ObjectRef = nil!()?;

    while evaluate(scope, pin.clone())?.borrow().is_truthy() {
        for expr in body.clone() {
            match expr {
                Expr::Return(e) => {
                    if interrupts.contains(&Interrupts::Return) {
                        return Ok(Bubble::Return(evaluate(scope, *e)?));
                    }
                    rt_err!("Cannot return from outside a function")?
                }
                Expr::Break(v) => return Ok(Bubble::Break(evaluate(scope, *v)?)),
                Expr::Continue => break,
                Expr::Loop { body } => {
                    match eval_while_with(scope, Expr::Term(Token::True), body, interrupts)? {
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
                Expr::For {
                    pin,
                    iterable,
                    body,
                } => {
                    match eval_for_with(scope, *pin, *iterable, body, interrupts)? {
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
                Expr::While { pin, body } => {
                    match eval_while_with(scope, *pin, body, interrupts)? {
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
                Expr::Conditional { branches } => {
                    interrupts.push(Interrupts::Break);
                    interrupts.push(Interrupts::Continue);
                    match eval_branches_with(scope, branches, interrupts)? {
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
                e => last = evaluate(scope, e)?,
            }
        }
    }

    Ok(Bubble::Eval(last))
}

fn eval_for_with(
    scope: &mut Scope,
    pin: Expr,
    iterable: Expr,
    body: Vec<Expr>,
    interrupts: &mut Vec<Interrupts>,
) -> EResult<Bubble> {
    let mut last = nil!()?;

    let sequence = evaluate(scope, iterable)?;
    for item in sequence.object().into_vec()?.into_iter() {
        if let Expr::Term(Token::Identifier(ref name)) = pin {
            scope.set(name, item);
        }

        for expr in body.clone() {
            match expr {
                Expr::Return(e) => {
                    if interrupts.contains(&Interrupts::Return) {
                        return Ok(Bubble::Return(evaluate(scope, *e)?));
                    }
                    rt_err!("Cannot return from outside a function")?
                }
                Expr::Break(v) => return Ok(Bubble::Break(evaluate(scope, *v)?)),
                Expr::Continue => break,
                Expr::Loop { body } => {
                    match eval_while_with(scope, Expr::Term(Token::True), body, interrupts)? {
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
                Expr::For {
                    pin,
                    iterable,
                    body,
                } => {
                    match eval_for_with(scope, *pin, *iterable, body, interrupts)? {
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
                Expr::While { pin, body } => {
                    match eval_while_with(scope, *pin, body, interrupts)? {
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
                Expr::Conditional { branches } => {
                    interrupts.push(Interrupts::Break);
                    interrupts.push(Interrupts::Continue);
                    match eval_branches_with(scope, branches, interrupts)? {
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
                e => last = evaluate(scope, e)?,
            }
        }
    }
    Ok(Bubble::Eval(last))
}

fn eval_interruptable_expr(
    scope: &mut Scope,
    expr: Expr,
    interrupts: &mut Vec<Interrupts>,
) -> EResult<Bubble> {
    match expr {
        Expr::Return(e) => {
            if interrupts.contains(&Interrupts::Return) {
                return Ok(Bubble::Return(evaluate(scope, *e)?));
            }
            rt_err!("Cannot return from outside a function")?
        }
        Expr::Break(v) => {
            if interrupts.contains(&Interrupts::Break) {
                return Ok(Bubble::Break(evaluate(scope, *v)?));
            }
            rt_err!("Cannot call break from outside a loop")?
        }
        Expr::Continue => {
            if interrupts.contains(&Interrupts::Continue) {
                return Ok(Bubble::Continue);
            }
            rt_err!("Cannot call continue from outside a loop")?
        }
        _ => panic!("Should never come to this"),
    }
}

fn eval_branches_with(
    scope: &mut Scope,
    branches: Vec<(Expr, Vec<Expr>)>,
    interrupts: &mut Vec<Interrupts>,
) -> EResult<Bubble> {
    for (cond, body) in branches.into_iter() {
        if evaluate(scope, cond)?.borrow().is_truthy() {
            let mut last = nil!()?;
            for expr in body {
                match expr {
                    Expr::Return(..) | Expr::Break(..) | Expr::Continue => {
                        return eval_interruptable_expr(scope, expr, interrupts)
                    }
                    Expr::Conditional { branches } => {
                        match eval_branches_with(scope, branches, interrupts)? {
                            Bubble::Eval(v) => last = v,
                            bubble => return Ok(bubble),
                        }
                    }
                    Expr::Loop { body } => {
                        match eval_while_with(scope, Expr::Term(Token::True), body, interrupts)? {
                            Bubble::Eval(v) => last = v,
                            bubble => return Ok(bubble),
                        };
                    }
                    Expr::For {
                        pin,
                        iterable,
                        body,
                    } => match eval_for_with(scope, *pin, *iterable, body, interrupts)? {
                        Bubble::Eval(v) => last = v,
                        bubble => return Ok(bubble),
                    },
                    e => {
                        last = evaluate(scope, e)?;
                    }
                }
            }
            return Ok(Bubble::Eval(last));
        }
    }
    Ok(Bubble::Eval(nil!()?))
}

fn eval_binary(scope: &mut Scope, op: Token, left: Expr, right: Expr) -> EResult<ObjectRef> {
    let left = evaluate(scope, left)?;
    let right = evaluate(scope, right)?;
    let left = left.object();
    let right = right.object();
    Ok(left.binary(op, right)?.into())
}

fn eval_scope(scope: &mut Scope, body: Vec<Expr>) -> EResult<ObjectRef> {
    let mut inner = Scope::default();
    inner.parent = Some(scope.get_ref());

    let mut last = nil!()?;
    for expr in body {
        match expr {
            Expr::Return(e) => return evaluate(&mut inner, *e),
            Expr::Loop { body } => match eval_while_with(
                scope,
                Expr::Term(Token::True),
                body,
                &mut vec![Interrupts::Return],
            )? {
                Bubble::Return(v) => return Ok(v),
                Bubble::Continue => rt_err!("Cannot call continue outside loop")?,
                Bubble::Eval(v) => last = v,
                _ => {} // eval & break get ignored
            },
            Expr::For {
                pin,
                iterable,
                body,
            } => match eval_for_with(
                &mut inner,
                *pin,
                *iterable,
                body,
                &mut vec![Interrupts::Return],
            )? {
                Bubble::Return(v) => return Ok(v),
                Bubble::Continue => rt_err!("Cannot call continue outside loop")?,
                Bubble::Eval(v) => last = v,
                _ => {} // eval & break get ignored
            },
            Expr::Conditional { branches } => {
                match eval_branches_with(&mut inner, branches, &mut vec![Interrupts::Return])? {
                    Bubble::Return(v) => return Ok(v),
                    Bubble::Continue => rt_err!("Cannot call continue outside loop")?,
                    Bubble::Break(_) => rt_err!("Cannot call break outside loop")?,
                    Bubble::Eval(v) => last = v,
                    _ => {} // eval & break
                }
            }
            e => {
                last = evaluate(&mut inner, e)?;
            }
        }
    }
    Ok(last)
}

fn eval_call_func(
    scope: &mut Scope,
    _: Option<String>,
    params: Vec<String>,
    locals: Option<HashMap<String, ObjectRef>>,
    body: Vec<Expr>,
    args: Vec<ObjectRef>,
) -> EResult<ObjectRef> {
    let mut inner = Scope::default();
    inner.parent = Some(scope.get_ref());

    if let Some(locals) = locals {
        inner.store = locals;
    }

    for (param, arg) in params.into_iter().zip(args) {
        inner.store.insert(param, arg);
    }

    for expr in body {
        match expr {
            Expr::Return(e) => return evaluate(&mut inner, *e),
            Expr::Loop { body } => match eval_while_with(
                &mut inner,
                Expr::Term(Token::True),
                body,
                &mut vec![Interrupts::Return],
            )? {
                Bubble::Return(v) => return Ok(v),
                Bubble::Continue => rt_err!("Cannot call continue outside loop")?,
                _ => {} // eval & break get ignored
            },
            Expr::For {
                pin,
                iterable,
                body,
            } => match eval_for_with(
                &mut inner,
                *pin,
                *iterable,
                body,
                &mut vec![Interrupts::Return],
            )? {
                Bubble::Return(v) => return Ok(v),
                Bubble::Continue => rt_err!("Cannot call continue outside loop")?,
                _ => {} // eval & break get ignored
            },
            Expr::Conditional { branches } => {
                match eval_branches_with(&mut inner, branches, &mut vec![Interrupts::Return])? {
                    Bubble::Return(v) => return Ok(v),
                    Bubble::Continue => rt_err!("Cannot call continue outside loop")?,
                    Bubble::Break(_) => rt_err!("Cannot call break outside loop")?,
                    _ => {} // eval & break
                }
            }
            e => {
                evaluate(&mut inner, e)?;
            }
        }
    }

    nil!()
}

fn eval_call(scope: &mut Scope, f: Expr, args: Vec<Expr>) -> EResult<ObjectRef> {
    let args = args
        .into_iter()
        .map(|a| evaluate(scope, a))
        .collect::<EResult<Vec<ObjectRef>>>()?;
    match evaluate(scope, f)?.object() {
        Object::Func {
            name,
            params,
            locals,
            body,
        } => eval_call_func(scope, name, params, locals, body, args),
        Object::Builtin(BuiltinFunc(b)) => b(scope, args),
        v => rt_err!("Cannot call: {:?} as a function", v),
    }
}

fn _eval_func(
    scope: &mut Scope,
    name: Option<String>,
    params: Vec<Expr>,
    body: Vec<Expr>,
) -> EResult<(Option<String>, ObjectRef)> {
    let params = params.into_iter().map(|a| {
        if let Expr::Term(Token::Identifier(arg)) = a {
            return Ok(arg);
        }
        rt_err!("Expected arg but found: {:?}", a)
    });

    Ok((
        name.clone(),
        Rc::new(RefCell::new(Object::Func {
            name,
            params: params.collect::<EResult<Vec<String>>>()?,
            locals: scope.locals(),
            body,
        })),
    ))
}

fn eval_func(
    scope: &mut Scope,
    name: Option<String>,
    params: Vec<Expr>,
    body: Vec<Expr>,
) -> EResult<ObjectRef> {
    let (name, func) = _eval_func(scope, name, params, body)?;

    if let Some(name) = name {
        scope.set(&name, Rc::clone(&func));
    }

    Ok(func)
}

fn eval_unary(scope: &mut Scope, op: Token, value: Expr) -> EResult<ObjectRef> {
    let object = evaluate(scope, value)?.object();
    Ok(object.unary(op)?.into())
}

fn eval_identifier(scope: &mut Scope, i: String) -> EResult<ObjectRef> {
    if let Some(object) = scope.get(&i) {
        Ok(object)
    } else {
        rt_err!("No variable named: {}", i)
    }
}

fn eval_assign(scope: &mut Scope, target: Expr, value: Expr) -> EResult<ObjectRef> {
    let oref = evaluate(scope, value)?;
    match target {
        Expr::Term(Token::Identifier(name)) => {
            scope.set(&name, oref);
            nil!()
        }
        Expr::Access { target, field } => {
            let src = eval_access(scope, *target, *field)?;
            src.replace(oref.object());
            nil!()
        }
        Expr::Deref(inner) => match *inner {
            Expr::Term(Token::Identifier(name)) => {
                let prev = scope
                    .get(&name)
                    .ok_or_else(|| RuntimeError(format!("No variable named: {}", name)))?;
                prev.replace(oref.object());
                nil!()
            }
            Expr::Term(Token::_Self) => {
                let prev = scope
                    .get("self")
                    .ok_or_else(|| RuntimeError("No self in scope".to_string()))?;
                prev.replace(oref.object());
                nil!()
            }
            Expr::Access { target, field } => eval_access(scope, *target, *field),
            inner => rt_err!("Cannot deref expression: {:?}", inner),
        },
        _ => rt_err!("Cannot assign to expression: {:?}", target),
    }
}

fn eval_sequence(scope: &mut Scope, items: Vec<Expr>) -> EResult<ObjectRef> {
    Ok(Object::Sequence(
        items
            .into_iter()
            .map(|i| evaluate(scope, i))
            .collect::<Result<Vec<ObjectRef>, RuntimeError>>()?,
    )
    .into())
}

fn eval_collection(scope: &mut Scope, items: Vec<Expr>) -> EResult<ObjectRef> {
    let items = items
        .into_iter()
        .map(|e| {
            if let Expr::Assign { target, value } = e {
                if let Expr::Term(Token::Identifier(name)) = *target {
                    return Ok((name, evaluate(scope, *value)?));
                }
                rt_err!("Expected term inside collection but found: {:?}", target)?
            } else {
                rt_err!("Expected assign inside collection, found: {:?}", e)?
            }
        })
        .collect::<EResult<HashMap<String, ObjectRef>>>()?;
    Ok(Object::Collection(items).into())
}

fn eval_access(scope: &mut Scope, target: Expr, field: Expr) -> EResult<ObjectRef> {
    let target = evaluate(scope, target)?;
    let field = evaluate(scope, field)?;
    let src = &mut *target.borrow_mut();
    let index = field.object();
    if let Ok(inner) = src.access(index) {
        Ok(inner)
    } else {
        let t = src.get_trait(scope, field)?;
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

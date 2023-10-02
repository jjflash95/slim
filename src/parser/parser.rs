use std::{iter::Peekable, vec::IntoIter};

use crate::parser::lexer::Span;
use crate::parser::lexer::Token;
use crate::parser::lexer::TokenValue;

pub type PResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    Interrupt(&'static str, Token),
    Continue,
}

#[derive(Debug)]
pub struct TokenStream {
    tokens: Peekable<IntoIter<Token>>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Block {
    Statement(Statement),
    Expression(Expr),
}

impl Block {
    pub fn into_expr_checked(self) -> Expr {
        match self {
            Block::Expression(expr) => expr,
            _ => panic!("Expected expression"),
        }
    }

    pub fn into_stm(self) -> Statement {
        match self {
            Block::Statement(stm) => stm,
            _ => panic!("Expected statement"),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Statement {
    ImportNames {
        span: Span,
        path: String,
        names: Vec<String>,
    },
    Import {
        span: Span,
        path: String,
        name: String,
    },
    Trait {
        span: Span,
        name: Token,
        methods: Vec<Expr>,
    },
    ImplFor {
        span: Span,
        name: Token,
        target: Token,
    },
    Impl {
        span: Span,
        target: Token,
        methods: Vec<Expr>,
    },
    DefStruct {
        span: Span,
        name: String,
        props: Vec<String>,
    },
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Expr {
    Optional {
        span: Span,
        inner: Box<Expr>,
    },
    Assign {
        span: Span,
        target: Box<Expr>,
        value: Box<Expr>,
    },
    Binary {
        span: Span,
        op: TokenValue,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        span: Span,
        op: TokenValue,
        value: Box<Expr>,
    },
    Func {
        span: Span,
        name: Option<String>,
        params: Vec<Expr>,
        body: Vec<Block>,
    },
    Call {
        span: Span,
        target: Box<Expr>,
        args: Vec<Expr>,
    },
    Access {
        span: Span,
        target: Box<Expr>,
        field: Box<Expr>,
    },
    Pipe {
        span: Span,
        parent: Box<Expr>,
        child: Box<Expr>,
    },
    For {
        span: Span,
        pin: Box<Expr>,
        iterable: Box<Expr>,
        body: Vec<Block>,
    },
    While {
        span: Span,
        pin: Box<Expr>,
        body: Vec<Block>,
    },
    Loop {
        span: Span,
        body: Vec<Block>,
    },
    Conditional {
        span: Span,
        branches: Vec<(Expr, Vec<Block>)>,
    },
    Struct {
        span: Span,
        name: String,
        props: Vec<Expr>,
    },
    Term(Span, TokenValue),
    Return(Span, Box<Expr>),
    Break(Span, Box<Expr>),
    Deref(Span, Box<Expr>),
    Ref(Span, Box<Expr>),
    Scope(Span, Vec<Block>),
    Sequence(Span, Vec<Expr>),
    Collection(Span, Vec<Expr>),
    Continue(Span),
    Nil(Span),
}

impl Expr {
    pub fn get_span(&self) -> &Span {
        match self {
            Expr::Optional { span, .. } => span,
            Expr::Assign { span, .. } => span,
            Expr::Binary { span, .. } => span,
            Expr::Unary { span, .. } => span,
            Expr::Func { span, .. } => span,
            Expr::Call { span, .. } => span,
            Expr::Access { span, .. } => span,
            Expr::Pipe { span, .. } => span,
            Expr::For { span, .. } => span,
            Expr::While { span, .. } => span,
            Expr::Loop { span, .. } => span,
            Expr::Conditional { span, .. } => span,
            Expr::Struct { span, .. } => span,
            Expr::Term(span, ..) => span,
            Expr::Return(span, ..) => span,
            Expr::Break(span, ..) => span,
            Expr::Deref(span, ..) => span,
            Expr::Ref(span, ..) => span,
            Expr::Scope(span, ..) => span,
            Expr::Sequence(span, ..) => span,
            Expr::Collection(span, ..) => span,
            Expr::Continue(span) => span,
            Expr::Nil(span) => span,
        }
    }
}

trait Chainable<T> {
    fn run(self, tokens: &mut TokenStream) -> PResult<T>;
}

impl<T> Chainable<T> for &[fn(&mut TokenStream) -> PResult<T>] {
    fn run(self, tokens: &mut TokenStream) -> PResult<T> {
        for f in self {
            match f(tokens) {
                Ok(result) => return Ok(result),
                Err(ParseError::Continue) => continue,
                Err(err) => return Err(err),
            }
        }
        Err(ParseError::Continue)
    }
}

impl TokenStream {
    pub fn is_empty(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    pub fn from_tokens(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    fn expect_identifier(&mut self) -> PResult<Token> {
        self.expect(|t| matches!(t, TokenValue::Identifier(_)))
    }

    fn expect(&mut self, test: impl Fn(&TokenValue) -> bool) -> PResult<Token> {
        match self.peek() {
            Some(token) if test(&token.value) => Ok(self.next().expect("Checked")),
            _ => Err(ParseError::Interrupt("Unexpected token", Token::default())),
        }
    }

    fn next_if(&mut self, test: impl Fn(&TokenValue) -> bool) -> Option<Token> {
        match self.peek() {
            Some(token) if test(&token.value) => self.next(),
            _ => None,
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    pub fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }
}

pub fn parse(tokens: &mut TokenStream) -> PResult<Block> {
    if let Ok(stmt) = parse_statement(tokens) {
        Ok(Block::Statement(stmt))
    } else {
        Ok(Block::Expression(parse_optional(tokens)?))
    }
}

fn parse_statement(tokens: &mut TokenStream) -> PResult<Statement> {
    [
        parse_from_import,
        parse_import,
        parse_impl,
        parse_struct_def,
        parse_trait,
    ]
    .run(tokens)
}

fn parse_from_import(tokens: &mut TokenStream) -> PResult<Statement> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::From))
        .map_err(|_| ParseError::Continue)?
        .span;
    let path = tokens
        .expect(|t| matches!(t, TokenValue::Str(_)))?
        .value
        .str();

    let _ = tokens.expect(|t| matches!(t, TokenValue::Import))?;
    let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?;
    let names = comma_separated(parse_identifier)(tokens)
        .into_iter()
        .map(|e| {
            if let Expr::Term(_, TokenValue::Identifier(name)) = e {
                name
            } else {
                panic!("Expected identifier")
            }
        })
        .collect();
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;

    Ok(Statement::ImportNames { span, path, names })
}

fn parse_import(tokens: &mut TokenStream) -> PResult<Statement> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::Import))
        .map_err(|_| ParseError::Continue)?
        .span;
    let path = tokens
        .expect(|t| matches!(t, TokenValue::Str(_)))?
        .value
        .str();
    let _ = tokens.expect(|t| matches!(t, TokenValue::As))?;
    let name = tokens.expect_identifier()?.value.identifier();
    Ok(Statement::Import { span, path, name })
}

fn parse_optional(tokens: &mut TokenStream) -> PResult<Expr> {
    let inner = parse_expression(tokens)?;
    if let Ok(o) = tokens.expect(|t| matches!(t, TokenValue::QuestionMark)) {
        Ok(Expr::Optional {
            span: o.span,
            inner: Box::new(inner),
        })
    } else {
        Ok(inner)
    }
}

fn parse_expression(tokens: &mut TokenStream) -> PResult<Expr> {
    [parse_for, parse_if, parse_while, parse_loop, parse_pipe].run(tokens)
}

fn parse_while(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::While))
        .map_err(|_| ParseError::Continue)?
        .span;
    let pin = Box::new(parse_expression(tokens)?);
    let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?;
    let body = parse_body(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;

    Ok(Expr::While { span, pin, body })
}

fn parse_loop(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::Loop))
        .map_err(|_| ParseError::Continue)?
        .span;
    let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?;
    let body = parse_body(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;

    Ok(Expr::Loop { span, body })
}

fn parse_pipe(tokens: &mut TokenStream) -> PResult<Expr> {
    let mut left = parse_andor(tokens)?;
    while let Ok(p) = tokens.expect(|t| matches!(t, TokenValue::Pipe)) {
        let span = p.span;
        let right = parse_andor(tokens)?;
        left = Expr::Pipe {
            span,
            parent: left.into(),
            child: right.into(),
        };
    }
    Ok(left)
}

fn parse_andor(tokens: &mut TokenStream) -> PResult<Expr> {
    let mut left = parse_comparison(tokens)?;
    while let Ok(op) = tokens.expect(|t| matches!(t, TokenValue::And | TokenValue::Or)) {
        let span = op.span;
        let right = parse_comparison(tokens)?;
        left = Expr::Binary {
            span,
            op: op.value,
            left: left.into(),
            right: right.into(),
        };
    }
    Ok(left)
}

fn parse_comparison(tokens: &mut TokenStream) -> PResult<Expr> {
    let mut left = parse_addsub(tokens)?;
    while let Ok(op) = tokens.expect(|t| {
        matches!(
            t,
            TokenValue::Eq
                | TokenValue::Ne
                | TokenValue::Gt
                | TokenValue::Lt
                | TokenValue::Gte
                | TokenValue::Lte
        )
    }) {
        let span = op.span;
        let right = parse_addsub(tokens)?;
        left = Expr::Binary {
            span,
            op: op.value,
            left: left.into(),
            right: right.into(),
        };
    }
    Ok(left)
}

fn parse_addsub(tokens: &mut TokenStream) -> PResult<Expr> {
    let mut left = parse_muldiv(tokens)?;
    while let Ok(op) = tokens.expect(|t| matches!(t, TokenValue::Plus | TokenValue::Minus)) {
        let span = op.span;
        let right = parse_muldiv(tokens)?;
        left = Expr::Binary {
            span,
            op: op.value,
            left: left.into(),
            right: right.into(),
        };
    }
    Ok(left)
}

fn parse_muldiv(tokens: &mut TokenStream) -> PResult<Expr> {
    let mut left = parse_assignment(tokens)?;
    while let Ok(op) =
        tokens.expect(|t| matches!(t, TokenValue::Star | TokenValue::Slash | TokenValue::Coef))
    {
        let span = op.span;
        let right = parse_assignment(tokens)?;
        left = Expr::Binary {
            span,
            op: op.value,
            left: left.into(),
            right: right.into(),
        };
    }
    Ok(left)
}

fn parse_assignment(tokens: &mut TokenStream) -> PResult<Expr> {
    let mut left = parse_access(tokens)?;
    while let Ok(bind) = tokens.expect(|t| {
        matches!(
            t,
            TokenValue::Assign | TokenValue::Semicolon | TokenValue::Mutate
        )
    }) {
        let span = bind.span;
        let right = parse_expression(tokens)?;
        left = match &bind.value {
            TokenValue::Assign | TokenValue::Semicolon => Expr::Assign {
                span,
                target: left.into(),
                value: right.into(),
            },
            TokenValue::Mutate => Expr::Assign {
                span,
                target: Expr::Deref(left.get_span().to_owned(), left.into()).into(),
                value: right.into(),
            },
            _ => unreachable!(),
        }
    }
    Ok(left)
}

fn parse_self(tokens: &mut TokenStream) -> PResult<Expr> {
    let token = tokens
        .expect(|t| matches!(t, TokenValue::_Self))
        .map_err(|_| ParseError::Continue)?;
    Ok(Expr::Term(token.span, token.value))
}

fn parse_identifier(tokens: &mut TokenStream) -> PResult<Expr> {
    let token = tokens
        .expect_identifier()
        .map_err(|_| ParseError::Continue)?;
    Ok(Expr::Term(token.span, token.value))
}

fn parse_if(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::If))
        .map_err(|_| ParseError::Continue)?
        .span;
    let cond = parse_expression(tokens)?;
    let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?;
    let body = parse_body(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;
    let mut branches = vec![(cond, body)];
    while let Ok(t) = tokens.expect(|t| matches!(t, TokenValue::Else)) {
        let condition = if tokens.expect(|t| matches!(t, TokenValue::If)).is_ok() {
            parse_expression(tokens)?
        } else {
            Expr::Term(t.span, TokenValue::True)
        };
        let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?;
        let body = parse_body(tokens);
        let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;
        branches.push((condition, body));
    }

    if let Ok(t) = tokens.expect(|t| matches!(t, TokenValue::Else)) {
        return Err(ParseError::Interrupt("Unexpected else", t));
    }
    Ok(Expr::Conditional { span, branches })
}

fn parse_for(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::For))
        .map_err(|_| ParseError::Continue)?
        .span;
    let pin = Box::new(parse_identifier(tokens)?);
    let _ = tokens.expect(|t| matches!(t, TokenValue::In))?;
    let iterable = Box::new(parse_access(tokens)?);
    let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?;
    let body = parse_body(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;

    Ok(Expr::For {
        span,
        pin,
        iterable,
        body,
    })
}

fn parse_access(tokens: &mut TokenStream) -> PResult<Expr> {
    let mut target = parse_call(tokens).map_err(|_| ParseError::Continue)?;
    loop {
        match tokens.peek() {
            Some(token) if matches!(token.value, TokenValue::Dot) => {
                let span = tokens.next().expect("checked").span;
                let next = tokens.expect_identifier()?;
                let field = Expr::Term(next.span, TokenValue::Str(next.value.identifier()));
                target = Expr::Access {
                    span,
                    target: target.into(),
                    field: field.into(),
                };
            }
            Some(token) if matches!(token.value, TokenValue::LParen) => {
                let span = tokens.next().expect("checked").span;
                let args = parse_args(tokens)?;
                let _ = tokens.expect(|t| matches!(t, TokenValue::RParen))?;
                target = Expr::Call {
                    span,
                    target: target.into(),
                    args,
                };
            }
            Some(token) if matches!(token.value, TokenValue::LBracket) => {
                let span = tokens.next().expect("checked").span;
                let index = parse_expression(tokens).unwrap_or(Expr::Nil(span.clone()));
                let _ = tokens.expect(|t| matches!(t, TokenValue::RBracket))?;
                target = Expr::Access {
                    span,
                    target: target.into(),
                    field: index.into(),
                };
            }
            _ => break,
        }
    }

    Ok(target)
}

fn parse_args(tokens: &mut TokenStream) -> PResult<Vec<Expr>> {
    let mut args = Vec::new();
    loop {
        match parse_expression(tokens) {
            Ok(expr) => {
                args.push(expr);
                if let Err(_) = tokens.expect(|t| matches!(t, TokenValue::Comma)) {
                    break;
                }
            }
            Err(ParseError::Continue) => break,
            Err(e) => return Err(e),
        }
    }

    tokens.next_if(|t| matches!(t, TokenValue::Comma));
    Ok(args)
}

fn parse_grouped(tokens: &mut TokenStream) -> PResult<Expr> {
    let _ = tokens
        .expect(|t| matches!(t, TokenValue::LParen))
        .map_err(|_| ParseError::Continue)?
        .span;
    let expr = parse_expression(tokens)?;
    let _ = tokens.expect(|t| matches!(t, TokenValue::RParen))?;
    Ok(expr)
}

fn parse_term(tokens: &mut TokenStream) -> PResult<Expr> {
    [
        parse_grouped,
        parse_struct_create,
        parse_continue,
        parse_func,
        parse_return,
        parse_break,
        parse_self,
        parse_nil,
        parse_unary,
        parse_identifier,
        parse_string_literal,
        parse_float,
        parse_int,
        parse_sequence,
        parse_collection,
        parse_bool,
    ]
    .run(tokens)
}

fn parse_nil(tokens: &mut TokenStream) -> PResult<Expr> {
    let token = tokens
        .expect(|t| matches!(t, TokenValue::Nil))
        .map_err(|_| ParseError::Continue)?;
    Ok(Expr::Term(token.span, token.value))
}

fn parse_collection(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::LBrace))
        .map_err(|_| ParseError::Continue)?
        .span;
    let items = comma_separated(parse_assignment)(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;
    Ok(Expr::Collection(span, items))
}

fn parse_bool(tokens: &mut TokenStream) -> PResult<Expr> {
    let token = tokens
        .expect(|t| matches!(t, TokenValue::True | TokenValue::False))
        .map_err(|_| ParseError::Continue)?;
    Ok(Expr::Term(token.span, token.value))
}

fn parse_return(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::Return))
        .map_err(|_| ParseError::Continue)?
        .span;
    let value = Box::new(parse_expression(tokens)?);
    Ok(Expr::Return(span, value))
}

fn parse_break(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::Break))
        .map_err(|_| ParseError::Continue)?
        .span;
    let value = Box::new(parse_expression(tokens)?);
    Ok(Expr::Break(span, value))
}

fn parse_unary(tokens: &mut TokenStream) -> PResult<Expr> {
    let op = tokens
        .expect(|t| matches!(t, TokenValue::Minus | TokenValue::Not | TokenValue::Star))
        .map_err(|_| ParseError::Continue)?;
    let span = op.span;
    let value = Box::new(parse_term(tokens)?);
    Ok(Expr::Unary {
        span,
        op: op.value,
        value,
    })
}

fn parse_continue(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::Continue))
        .map_err(|_| ParseError::Continue)?
        .span;
    Ok(Expr::Continue(span))
}

fn parse_struct_create(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::New))
        .map_err(|_| ParseError::Continue)?
        .span;
    let name = tokens.expect_identifier()?;
    let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?.span;
    let props = comma_separated(parse_assignment)(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;

    Ok(Expr::Struct {
        span,
        name: name.into(),
        props,
    })
}

fn comma_separated<T>(
    mut f: impl FnMut(&mut TokenStream) -> PResult<T>,
) -> impl FnMut(&mut TokenStream) -> Vec<T> {
    move |tokens| {
        let mut items = Vec::new();
        while let Ok(item) = f(tokens) {
            items.push(item);
            if tokens.expect(|t| matches!(t, TokenValue::Comma)).is_err() {
                break;
            }
        }
        tokens.next_if(|t| matches!(t, TokenValue::Comma));
        items
    }
}

fn parse_sequence(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::LBracket))
        .map_err(|_| ParseError::Continue)?
        .span;
    let items = comma_separated(parse_expression)(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBracket))?;
    Ok(Expr::Sequence(span, items))
}

fn parse_float(tokens: &mut TokenStream) -> PResult<Expr> {
    let token = tokens
        .expect(|t| matches!(t, TokenValue::Float(_)))
        .map_err(|_| ParseError::Continue)?;
    Ok(Expr::Term(token.span, token.value))
}

fn parse_int(tokens: &mut TokenStream) -> PResult<Expr> {
    let token = tokens
        .expect(|t| matches!(t, TokenValue::Int(_)))
        .map_err(|_| ParseError::Continue)?;
    Ok(Expr::Term(token.span, token.value))
}

fn parse_string_literal(tokens: &mut TokenStream) -> PResult<Expr> {
    let token = tokens
        .expect(|t| matches!(t, TokenValue::Str(_)))
        .map_err(|_| ParseError::Continue)?;
    Ok(Expr::Term(token.span, token.value))
}

fn parse_call(tokens: &mut TokenStream) -> PResult<Expr> {
    let mut left = parse_term(tokens)?;
    while let Ok(t) = tokens.expect(|t| matches!(t, TokenValue::LParen)) {
        let span = t.span;
        let args = parse_args(tokens)?;
        let _ = tokens.expect(|t| matches!(t, TokenValue::RParen))?;
        left = Expr::Call {
            span,
            target: left.into(),
            args,
        };
    }
    Ok(left)
}

fn parse_props(tokens: &mut TokenStream) -> Vec<String> {
    let mut props = Vec::new();
    while let Ok(prop) = tokens.expect_identifier() {
        props.push(prop.into());
        if tokens.expect(|t| matches!(t, TokenValue::Comma)).is_err() {
            break;
        }
    }
    props
}

fn parse_methods(tokens: &mut TokenStream) -> Vec<Expr> {
    let mut methods = Vec::new();
    while let Ok(method) = parse_method(tokens) {
        methods.push(method);
    }
    methods
}

fn parse_params(tokens: &mut TokenStream) -> Vec<Expr> {
    let mut args = Vec::new();
    while let Ok(token) = tokens.expect_identifier() {
        args.push(Expr::Term(token.span, token.value));
        if tokens.expect(|t| matches!(t, TokenValue::Comma)).is_err() {
            break;
        }
    }
    tokens.next_if(|t| matches!(t, TokenValue::Comma));
    args
}

fn parse_body(tokens: &mut TokenStream) -> Vec<Block> {
    let mut body = Vec::new();
    while let Ok(expr) = parse(tokens) {
        body.push(expr);
    }
    body
}

fn parse_method(tokens: &mut TokenStream) -> PResult<Expr> {
    let name_or_kw = tokens
        .expect(|t| matches!(t, TokenValue::Fn | TokenValue::Identifier(_)))
        .map_err(|_| ParseError::Continue)?;
    let span = name_or_kw.span.clone();
    let name = match name_or_kw.value {
        TokenValue::Fn => match tokens.expect_identifier()?.value {
            TokenValue::Identifier(name) => name,
            _ => {
                return Err(ParseError::Interrupt(
                    "Expected identifier or fn token",
                    name_or_kw,
                ))
            }
        },
        TokenValue::Identifier(name) => name,
        _ => unreachable!(),
    };
    let _ = tokens.expect(|t| matches!(t, TokenValue::LParen))?;
    let params = parse_params(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RParen))?;
    let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?;
    let body = parse_body(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;

    Ok(Expr::Func {
        span,
        name: Some(name),
        params,
        body,
    })
}

fn parse_func(tokens: &mut TokenStream) -> PResult<Expr> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::Fn))
        .map_err(|_| ParseError::Continue)?
        .span;
    let name = match tokens.expect_identifier() {
        Ok(name) => Some(name.into()),
        Err(_) => None,
    };
    let _ = tokens.expect(|t| matches!(t, TokenValue::LParen))?;
    let params = parse_params(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RParen))?;
    let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?;
    let body = parse_body(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;

    Ok(Expr::Func {
        span,
        name,
        params,
        body,
    })
}

fn parse_struct_def(tokens: &mut TokenStream) -> PResult<Statement> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::Struct))
        .map_err(|_| ParseError::Continue)?
        .span;
    let name: String = tokens.expect_identifier()?.into();
    let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?;
    let props = parse_props(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;

    Ok(Statement::DefStruct { span, name, props })
}

fn parse_trait(tokens: &mut TokenStream) -> PResult<Statement> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::Trait))
        .map_err(|_| ParseError::Continue)?
        .span;
    let name = tokens.expect_identifier()?;
    let _ = tokens.expect(|t| matches!(t, TokenValue::LBrace))?;
    let methods = parse_methods(tokens);
    let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;

    Ok(Statement::Trait {
        span,
        name,
        methods,
    })
}

fn parse_impl(tokens: &mut TokenStream) -> PResult<Statement> {
    let span = tokens
        .expect(|t| matches!(t, TokenValue::Impl))
        .map_err(|_| ParseError::Continue)?
        .span;
    let name = tokens.expect_identifier()?;
    match tokens.expect(|t| matches!(t, TokenValue::For | TokenValue::LBrace)) {
        Ok(Token {
            value: TokenValue::For,
            span,
        }) => {
            let target =
                tokens.expect(|t| matches!(t, TokenValue::Identifier(_) | TokenValue::Star))?;
            Ok(Statement::ImplFor { span, name, target })
        }
        Ok(Token {
            value: TokenValue::LBrace,
            ..
        }) => {
            let methods = parse_methods(tokens);
            let _ = tokens.expect(|t| matches!(t, TokenValue::RBrace))?;

            Ok(Statement::Impl {
                span,
                target: name,
                methods,
            })
        }
        Ok(_) => unreachable!(),
        Err(e) => Err(e),
    }
}

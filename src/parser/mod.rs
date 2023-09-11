use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::alpha1;
use nom::combinator::{not, opt, peek};
use nom::multi::{fold_many0, many0, many1, separated_list0};
use nom::number::complete::recognize_float_parts;
use nom::sequence::{delimited, tuple};
use nom::IResult;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Token {
    Eq,
    Ne,
    Gt,
    Gte,
    Lt,
    Lte,
    In,
    And,
    Or,
    Add,
    Sub,
    Set,
    False,
    True,
    Nil,
    Not,
    Negative,
    Multiply,
    Divide,
    Coef,
    Identifier(String),
    StringLiteral(String),
    Float(f64),
    Int(i128),
    TypeInt,
    TypeStr,
    TypeSeq,
    TypeColl,
    TypeBool,
    TypeStruct,
    _Self,
}

#[derive(Debug, PartialEq)]
pub enum ParseResult {
    Statement(Statement),
    Expression(Expr),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Statement {
    Trait { name: Token, methods: Vec<Expr> },
    ImplFor { name: Token, target: Token },
    Impl { target: Token, methods: Vec<Expr> },
    DefStruct { name: String, props: Vec<String> },
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Expr {
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    Binary {
        op: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: Token,
        value: Box<Expr>,
    },
    Func {
        name: Option<String>,
        params: Vec<Expr>,
        body: Vec<Expr>,
    },
    Call {
        target: Box<Expr>,
        args: Vec<Expr>,
    },
    Access {
        target: Box<Expr>,
        field: Box<Expr>,
    },
    Pipe {
        parent: Box<Expr>,
        child: Box<Expr>,
    },
    For {
        pin: Box<Expr>,
        iterable: Box<Expr>,
        body: Vec<Expr>,
    },
    While {
        pin: Box<Expr>,
        body: Vec<Expr>,
    },
    Loop {
        body: Vec<Expr>,
    },
    Conditional {
        branches: Vec<(Expr, Vec<Expr>)>,
    },
    Struct {
        name: String,
        props: Vec<Expr>,
    },
    Term(Token),
    Return(Box<Expr>),
    Break(Box<Expr>),
    Deref(Box<Expr>),
    Ref(Box<Expr>),
    Scope(Vec<Expr>),
    Sequence(Vec<Expr>),
    Collection(Vec<Expr>),
    Continue,
    Nil,
}

impl From<Expr> for Token {
    fn from(i: Expr) -> Self {
        match i {
            Expr::Term(t) => t,
            _ => panic!("Expected term"),
        }
    }
}

impl From<&str> for Token {
    fn from(i: &str) -> Self {
        match i {
            "false" => Token::False,
            "true" => Token::True,
            "self" => Token::_Self,
            "nil" => Token::Nil,
            "==" => Token::Eq,
            "!=" => Token::Ne,
            ">" => Token::Gt,
            "<" => Token::Lt,
            ">=" => Token::Gte,
            "<=" => Token::Lte,
            "in " => Token::In,
            "&&" => Token::And,
            "||" => Token::Or,
            ":" => Token::Set,
            "+" => Token::Add,
            "-" => Token::Sub,
            "*" => Token::Multiply,
            "/" => Token::Divide,
            "%" => Token::Coef,
            i => Token::Identifier(i.to_owned()),
        }
    }
}

fn parse_statement(input: &str) -> IResult<&str, ParseResult> {
    alt((parse_trait, parse_struct_def, parse_impl_for, parse_impl))(input)
        .map(|(i, o)| (i, ParseResult::Statement(o)))
}

pub fn parse(input: &str) -> IResult<&str, ParseResult> {
    alt((parse_statement, parse_expr))(input.trim_start())
}

fn parse_expr(input: &str) -> IResult<&str, ParseResult> {
    _parse_expr(input).map(|(i, o)| (i, ParseResult::Expression(o)))
}

fn _parse_expr(input: &str) -> IResult<&str, Expr> {
    alt((
        parse_for,
        parse_if,
        parse_while,
        parse_loop,
        // parse_scope,
        parse_pipe,
    ))(input)
}

fn body() -> impl Fn(&str) -> IResult<&str, Vec<Expr>> {
    |i| {
        fold_many0(
            tuple((_parse_expr, skippables)),
            Vec::new,
            |mut acc: Vec<_>, (item, _)| {
                acc.push(item);
                acc
            },
        )(i)
    }
}

fn parse_struct_create(input: &str) -> IResult<&str, Expr> {
    tuple((
        wrap("new"),
        _parse_identifier,
        wrap("{"),
        separated_list0(tuple((wrap(","),)), parse_collection_assignment),
        opt(wrap(",")),
        wrap("}"),
    ))(input)
    .map(|(i, (_, name, _, props, _, _))| (i, Expr::Struct { name, props }))
}

fn parse_struct_def(input: &str) -> IResult<&str, Statement> {
    tuple((
        wrap("struct"),
        _parse_identifier,
        wrap("{"),
        separated_list0(tuple((wrap(","),)), _parse_identifier),
        opt(wrap(",")),
        wrap("}"),
    ))(input)
    .map(|(i, (_, name, _, props, _, _))| {
        (
            i,
            Statement::DefStruct {
                name,
                props: props.into_iter().collect(),
            },
        )
    })
}

fn parse_impl(input: &str) -> IResult<&str, Statement> {
    tuple((
        wrap("impl"),
        _parse_identifier,
        wrap("{"),
        many0(parse_func),
        opt(wrap(",")),
        wrap("}"),
    ))(input)
    .map(|(i, (_, target, _, methods, _, _))| {
        (
            i,
            Statement::Impl {
                target: Token::Identifier(target),
                methods,
            },
        )
    })
}

fn parse_impl_for(input: &str) -> IResult<&str, Statement> {
    tuple((
        wrap("impl"),
        _parse_identifier,
        wrap("for"),
        _parse_identifier,
    ))(input)
    .map(|(i, (_, t, _, s))| {
        (
            i,
            Statement::ImplFor {
                name: Token::Identifier(t),
                target: Token::Identifier(s),
            },
        )
    })
}

fn parse_trait(input: &str) -> IResult<&str, Statement> {
    tuple((
        wrap("trait"),
        _parse_identifier,
        wrap("{"),
        many0(parse_func),
        opt(wrap(",")),
        wrap("}"),
    ))(input)
    .map(|(i, (_, name, _, methods, _, _))| {
        (
            i,
            Statement::Trait {
                name: Token::Identifier(name),
                methods,
            },
        )
    })
}

fn parse_scope(input: &str) -> IResult<&str, Expr> {
    tuple((wrap("{"), body(), wrap("}")))(input).map(|(i, (_, body, _))| (i, Expr::Scope(body)))
}

fn parse_if(input: &str) -> IResult<&str, Expr> {
    let mut branches: Vec<(Expr, Vec<Expr>)> = vec![];

    let (mut next, main) = tuple((wrap("if"), _parse_expr, wrap("{"), body(), wrap("}")))(input)
        .map(|(i, (_, cond, _, b, _))| (i, (cond, b)))?;
    branches.push(main);

    while let Ok((more, elif)) = tuple((
        wrap("else"),
        wrap("if"),
        _parse_expr,
        wrap("{"),
        body(),
        wrap("}"),
    ))(next)
    .map(|(i, (_, _, cond, _, b, _))| (i, (cond, b)))
    {
        next = more;
        branches.push(elif);
    }

    if let Ok((more, last)) = tuple((wrap("else"), wrap("{"), body(), wrap("}")))(next)
        .map(|(i, (_, _, b, _))| (i, (Expr::Term(Token::True), b)))
    {
        next = more;
        branches.push(last);
    };

    Ok((next, Expr::Conditional { branches }))
}

fn parse_loop(input: &str) -> IResult<&str, Expr> {
    tuple((wrap("loop"), wrap("{"), body(), wrap("}")))(input)
        .map(|(i, (_, _, body, _))| (i, Expr::Loop { body }))
}

fn parse_while(input: &str) -> IResult<&str, Expr> {
    tuple((wrap("while"), _parse_expr, wrap("{"), body(), wrap("}")))(input).map(
        |(i, (_, value, _, body, _))| {
            (
                i,
                Expr::While {
                    pin: Box::new(value),
                    body,
                },
            )
        },
    )
}

fn parse_func(input: &str) -> IResult<&str, Expr> {
    tuple((
        wrap("fn"),
        opt(parse_identifier),
        wrap("("),
        parse_args,
        wrap(")"),
        wrap("{"),
        body(),
        wrap("}"),
    ))(input)
    .map(|(i, (_, name, _, params, _, _, body, _))| {
        let name = if let Some(Expr::Term(Token::Identifier(name))) = name {
            Some(name)
        } else {
            None
        };
        (i, Expr::Func { name, params, body })
    })
}

fn parse_assignment(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = alt((parse_kw_deref, parse_access))(input)?; //access
    while let Ok((next, _)) = tuple((char('='), peek(not(char('=')))))(input) {
        let (i, right) = _parse_expr(next)?;
        input = i;
        left = Expr::Assign {
            target: Box::new(left),
            value: Box::new(right),
        }
    }
    Ok((input, left))
}

fn parse_collection_assignment(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = alt((parse_kw_deref, parse_access))(input)?; //access
    while let Ok((next, _)) = char(':')(input) {
        let (i, right) = _parse_expr(next)?;
        input = i;
        left = Expr::Assign {
            target: Box::new(left),
            value: Box::new(right),
        }
    }
    Ok((input, left))
}

fn parse_kw_deref(input: &str) -> IResult<&str, Expr> {
    tuple((wrap("follow "), alt((parse_access, parse_term))))(input)
        .map(|(i, (_, o))| (i, Expr::Deref(o.into())))
}

fn parse_andor(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = parse_compare(input)?;
    while let Ok((next, op)) =
        alt((wrap("&&"), wrap("||")))(input).map(|(i, o)| (i, Token::from(o)))
    {
        let (i, right) = parse_compare(next)?;
        input = i;
        left = Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        };
    }
    Ok((input, left))
}

fn parse_compare(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = parse_add(input)?;
    while let Ok((next, op)) = alt((
        wrap("<="),
        wrap(">="),
        wrap("=="),
        wrap("!="),
        wrap("<"),
        wrap(">"),
        wrap("in "), // space needed, otherwise x = 1 index = 2 matches (x = 1 in ... breaks)
    ))(input)
    .map(|(i, o)| (i, Token::from(o)))
    {
        let (i, right) = parse_add(next)?;
        input = i;
        left = Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        };
    }
    Ok((input, left))
}

fn parse_add(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = parse_mult(input)?;
    while let Ok((next, op)) = alt((wrap("+"), wrap("-")))(input).map(|(i, o)| (i, Token::from(o)))
    {
        let (i, right) = parse_mult(next)?;
        input = i;
        left = Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        };
    }
    Ok((input, left))
}

fn parse_mult(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = parse_assignment(input)?;
    while let Ok((next, op)) =
        alt((wrap("*"), wrap("/"), wrap("%")))(input).map(|(i, o)| (i, Token::from(o)))
    {
        let (i, right) = parse_assignment(next)?;
        input = i;
        left = Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
    Ok((input, left))
}

fn parse_args(input: &str) -> IResult<&str, Vec<Expr>> {
    separated_list0(tuple((wrap(","),)), _parse_expr)(input)
}

fn parse_call(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = parse_term(input)?;
    while let Ok((next, _)) = char('(')(input) {
        let (i, args) = parse_args(next)?;
        input = char(')')(i)?.0;
        left = Expr::Call {
            target: Box::new(left),
            args,
        }
    }
    Ok((input, left))
}

fn parse_pipe(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = parse_andor(input)?;
    while let Ok(next) = wrap("&")(input).map(|(i, _)| i) {
        let (i, right) = parse_andor(next)?;
        input = i;
        left = Expr::Pipe {
            parent: Box::new(right),
            child: Box::new(left),
        }
    }
    Ok((input, left))
}

fn parse_access(input: &str) -> IResult<&str, Expr> {
    let (mut input, mut left) = delimited(skippables, parse_call, skippables)(input)?;
    loop {
        match input.chars().next() {
            Some('.') => {
                let (i, mut right) = delimited(skippables, parse_term, skippables)(&input[1..])?;
                if let Expr::Term(Token::Identifier(inner)) = right {
                    right = Expr::Term(Token::StringLiteral(inner));
                }
                input = i;
                left = Expr::Access {
                    target: Box::new(left),
                    field: Box::new(right),
                };
            }
            Some('(') => {
                let (i, args) = delimited(skippables, parse_args, skippables)(&input[1..])?;
                input = char(')')(i)?.0;
                left = Expr::Call {
                    target: Box::new(left),
                    args,
                }
            }
            Some('[') => {
                let (i, right) = delimited(skippables, _parse_expr, skippables)(&input[1..])
                    .unwrap_or((&input[1..], Expr::Term(Token::Nil)));
                input = char(']')(i)?.0;
                left = Expr::Access {
                    target: Box::new(left),
                    field: Box::new(right),
                };
            }
            _ => break,
        };
    }
    Ok((input, left))
}

fn parse_inc_dec(input: &str) -> IResult<&str, Expr> {
    tuple((parse_identifier, alt((wrap("++"), wrap("--")))))(input).map(|(i, (id, o))| {
        (
            i,
            Expr::Assign {
                target: id.clone().into(),
                value: Expr::Binary {
                    op: if o == "++" { Token::Add } else { Token::Sub },
                    left: id.into(),
                    right: Expr::Term(Token::Int(1)).into(),
                }
                .into(),
            },
        )
    })
}

fn parse_term(input: &str) -> IResult<&str, Expr> {
    alt((
        parse_struct_create,
        parse_inc_dec,
        parse_continue,
        parse_func,
        parse_return,
        parse_break,
        parse_unary,
        // parse_ref,
        parse_deref,
        parse_identifier,
        parse_string_literal,
        parse_num,
        parse_sequence,
        parse_collection,
        parse_grouped,
    ))(input)
}

fn parse_continue(input: &str) -> IResult<&str, Expr> {
    wrap("continue")(input).map(|(i, _)| (i, Expr::Continue))
}

fn parse_unary(input: &str) -> IResult<&str, Expr> {
    tuple((alt((wrap("!"), wrap("-"))), _parse_expr))(input).map(|(i, (op, value))| {
        (
            i,
            Expr::Unary {
                op: match op {
                    "!" => Token::Not,
                    "-" => Token::Negative,
                    _ => panic!("Expected unary operator"),
                },
                value: value.into(),
            },
        )
    })
}

fn parse_grouped(input: &str) -> IResult<&str, Expr> {
    tuple((wrap("("), _parse_expr, wrap(")")))(input).map(|(i, (_, o, _))| (i, o))
}

fn parse_break(input: &str) -> IResult<&str, Expr> {
    tuple((wrap("break"), opt(_parse_expr)))(input)
        .map(|(i, (_, o))| (i, Expr::Break(o.unwrap_or(Expr::Nil).into())))
}

fn parse_return(input: &str) -> IResult<&str, Expr> {
    tuple((wrap("return"), opt(_parse_expr)))(input)
        .map(|(i, (_, o))| (i, Expr::Return(o.unwrap_or(Expr::Nil).into())))
}

// fn parse_ref(input: &str) -> IResult<&str, Expr> {
//     tuple((wrap("&"), parse))(input).map(|(i, (_, o))| (i, Expr::Ref(o.into())))
// }

fn parse_deref(input: &str) -> IResult<&str, Expr> {
    tuple((wrap("*"), _parse_expr))(input).map(|(i, (_, o))| (i, Expr::Deref(o.into())))
}

fn _parse_identifier(input: &str) -> IResult<&str, String> {
    let (o, parsed) = many1(alt((alpha1, tag("_"))))(input)?;
    Ok((o, parsed.join("")))
}

fn parse_identifier(input: &str) -> IResult<&str, Expr> {
    _parse_identifier(input).map(|(i, o)| (i, Expr::Term((o.as_str()).into())))
}

fn parse_sequence(input: &str) -> IResult<&str, Expr> {
    tuple((
        char('['),
        separated_list0(tuple((skippables, char(','), skippables)), parse_access),
        opt(wrap(",")),
        char(']'),
    ))(input)
    .map(|(i, (_, values, _, _))| (i, Expr::Sequence(values)))
}

fn parse_collection(input: &str) -> IResult<&str, Expr> {
    tuple((
        wrap("{"),
        separated_list0(tuple((wrap(","),)), parse_collection_assignment),
        opt(wrap(",")),
        wrap("}"),
    ))(input)
    .map(|(i, (_, values, _, _))| (i, Expr::Collection(values)))
}

fn parse_string_literal(input: &str) -> IResult<&str, Expr> {
    alt((
        tuple((lwrap("\""), take_while(|c| c != '"'), rwrap("\""))),
        tuple((lwrap("'"), take_while(|c| c != '\''), rwrap("'"))),
    ))(input)
    .map(|(i, (_, s, _))| {
        (
            i,
            Expr::Term(Token::StringLiteral(s.to_owned().replace("\\n", "\n"))),
        )
    })
}

fn parse_num(input: &str) -> IResult<&str, Expr> {
    tuple((recognize_float_parts, skippables))(input).map(|(i, (o, _))| {
        let (sign, integer, fraction, exponent) = o;
        if exponent != 0 || !fraction.is_empty() {
            let mut num = (format!("{}.{}", integer, fraction)
                .parse::<f64>()
                .expect("number too big for f64"))
                * if sign { 1. } else { -1. };
            if exponent != 0 {
                num *= 10.0_f64.powi(exponent)
            }
            (i, Expr::Term(Token::Float(num)))
        } else {
            let n = format!("{}{}", if sign { "" } else { "-" }, integer)
                .parse::<i128>()
                .expect("Too big for i128");
            (i, Expr::Term(Token::Int(n)))
        }
    })
}

fn parse_for(input: &str) -> IResult<&str, Expr> {
    tuple((
        wrap("for"),
        parse_identifier,
        wrap("in"),
        parse_access,
        wrap("{"),
        body(),
        wrap("}"),
    ))(input)
    .map(|(i, (_, pin, _, iterable, _, body, _))| {
        (
            i,
            Expr::For {
                pin: Box::new(pin),
                iterable: Box::new(iterable),
                body,
            },
        )
    })
}

fn skippables(input: &str) -> IResult<&str, &str> {
    take_while(|c| [' ', '\t', '\n', '\r'].contains(&c))(input)
}

fn char(ch: char) -> impl Fn(&str) -> IResult<&str, char> {
    move |i| delimited(skippables, nom::character::complete::char(ch), skippables)(i)
}

fn wrap(t: &'static str) -> impl Fn(&str) -> IResult<&str, &str> {
    move |i| delimited(skippables, nom::bytes::complete::tag(t), skippables)(i)
}

fn lwrap(t: &'static str) -> impl Fn(&str) -> IResult<&str, &str> {
    move |i| tuple((skippables, nom::bytes::complete::tag(t)))(i).map(|(i, (_, o))| (i, o))
}

fn rwrap(t: &'static str) -> impl Fn(&str) -> IResult<&str, &str> {
    move |i| tuple((nom::bytes::complete::tag(t), skippables))(i).map(|(i, (o, _))| (i, o))
}

use core::panic;
use std::{str::Chars, iter::Peekable};


#[derive(Clone, Debug, Default)]
pub struct Span {
    pub col: usize,
    pub row: usize,
    pub filename: String
}

impl PartialEq for Span {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal)
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Default)]
pub struct Token {
    pub value: TokenValue,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CharArray<'a> {
    chars: Peekable<Chars<'a>>,
    span: Span,
}

impl<'a> CharArray<'a> {
    fn new(text: &'a str, span: Span) -> Self {
        CharArray { chars: text.chars().peekable(), span }
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn next(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                self.span.col += 1;
                if c == '\n' {
                    self.span.row += 1;
                    self.span.col = 0;
                }
                Some(c)
            },
            None => None
        }
    }

    fn current_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Default)]
pub enum TokenValue {
    #[default]
    EOF,
    As,
    Import,
    From,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Dot,
    Comma,
    Assign,
    Semicolon,
    Mutate,

    New,
    Trait,
    Impl,
    While,
    For,
    Loop,
    Struct,
    If,
    ElseIf,
    Else,
    Return,
    Break,
    Continue,
    Fn,

    Pipe,
    Eq,
    Ne,
    Gt,
    Gte,
    Lt,
    Lte,
    In,
    And,
    Or,
    Plus,
    Minus,
    Set,
    False,
    True,
    Nil,
    Not,
    Negative,
    Star,
    Slash,
    Coef,
    Identifier(String),
    Str(String),
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

impl TokenValue {
    pub fn identifier(self) -> String {
        if let TokenValue::Identifier(value) = self {
            value
        } else {
            panic!("Not an identifier")
        }
    }

    pub fn str(self) -> String {
        if let TokenValue::Str(value) = self {
            value
        } else {
            panic!("Not a string")
        }
    }
}

fn is_emoji(ch: char) -> bool {
    match ch as u32 {
        0x1F300..=0x1F5FF => true,
        0x1F600..=0x1F64F => true,
        0x1F910..=0x1F96C => true,
        _ => false,
    }
}

fn is_alpha(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_numeric(ch: char) -> bool {
    ch.is_ascii_digit() || ch == '_'
}

pub fn tokenize(text: &str, file: Option<String>) -> Vec<Token> {
    let mut tokens = Vec::new();
    let filename = file.unwrap_or(String::from("<stdin>"));
    let span = Span {
        col: 0,
        row: 0,
        filename
    };

    let mut chars = CharArray::new(text, span);
    while let Some(token) = next_token(&mut chars) {
        tokens.push(token);
    };
    tokens
}

impl TryInto<TokenValue> for char {
    type Error = &'static str;

    fn try_into(self) -> Result<TokenValue, Self::Error> {
        match self {
            ':' => Ok(TokenValue::Semicolon),
            '*' => Ok(TokenValue::Star),
            '/' => Ok(TokenValue::Slash),
            '+' => Ok(TokenValue::Plus),
            '-' => Ok(TokenValue::Minus),
            '%' => Ok(TokenValue::Coef),
            '!' => Ok(TokenValue::Not),
            '=' => Ok(TokenValue::Assign),
            _ => Err("Invalid token"),
        }
    }
}

impl From<String> for TokenValue {
    fn from(value: String) -> Self {
        match value.as_str() {
            "trait" => TokenValue::Trait,
            "impl" => TokenValue::Impl,
            "struct" => TokenValue::Struct,
            "if" => TokenValue::If,
            "in" => TokenValue::In,
            "else" => TokenValue::Else,
            "for" => TokenValue::For,
            "new" => TokenValue::New,
            "while" => TokenValue::While,
            "loop" => TokenValue::Loop,
            "return" => TokenValue::Return,
            "fn" => TokenValue::Fn,
            "break" => TokenValue::Break,
            "continue" => TokenValue::Continue,
            "true" => TokenValue::True,
            "false" => TokenValue::False,
            "nil" => TokenValue::Nil,
            "self" => TokenValue::_Self,
            _ => TokenValue::Identifier(value),
        }
    }
}

impl From<Token> for String {
    fn from(token: Token) -> Self {
        match token.value {
            TokenValue::Identifier(value) => value,
            TokenValue::Str(value) => value,
            _ => panic!("XDSDSDSDD")
        }
    }
}

fn parse_str(chars: &mut CharArray, span: Span, ch: char) -> Option<Token> {
    let mut acc = String::new();
    while let Some(c) = chars.next() {
        if c == ch {
            break;
        } else if c == '\\' {
            match chars.next() {
                Some('n') => acc.push('\n'),
                Some('t') => acc.push('\t'),
                Some('r') => acc.push('\r'),
                Some(c) => acc.push(c),
                None => return None,
            }
            continue;
        } else {
            acc.push(c);
        };
    };

    Some(
        Token {
            value: TokenValue::Str(acc),
            span
    })
}

fn parse_identifier(leading: char, chars: &mut CharArray, span: Span) -> Token {
    let mut acc = leading.to_string();
    while let Some(c) = chars.peek() {
        if is_alpha(*c) || is_emoji(*c) || is_numeric(*c) {
            acc.push(*c);
            chars.next();
        } else {
            break;
        }
    };

    Token {
        value: acc.into(),
        span
    }
}

fn parse_num(leading: char, chars: &mut CharArray, span: Span) -> Token {
    let mut acc = leading.to_string();
    while let Some(c) = chars.peek() {
        if *c == '_' {
            chars.next();
        } else if is_numeric(*c) || (*c == '.' && !acc.contains('.')) {
            acc.push(chars.next().expect("Checked"));
        } else {
            break;
        }
    };

    if let Ok(int) = acc.parse::<i128>() {
        return Token {
            value: TokenValue::Int(int),
            span
        }
    } else if let Ok(float) = acc.parse::<f64>() {
        return Token {
            value: TokenValue::Float(float),
            span
        }
    };
    panic!("XDparsenum")
}

fn next_token(chars: &mut CharArray) -> Option<Token> {
    let span = chars.current_span();
    let Some(char) = chars.next() else {
        return None
    };

    match char {
        ' ' | '\t' | '\n' => next_token(chars),
        '(' => Some(
            Token {
                value: TokenValue::LParen,
                span
            }
        ),
        ')' => Some(
            Token {
                value: TokenValue::RParen,
                span
            }
        ),
        '{' => Some(
            Token {
                value: TokenValue::LBrace,
                span
            },
        ),
        '}' => Some(
            Token {
                value: TokenValue::RBrace,
                span
            },
        ),
        '[' => Some(
            Token {
                value: TokenValue::LBracket,
                span
            }
        ),
        ']' => Some(
            Token {
                value: TokenValue::RBracket,
                span
            }
        ),
        '.' => Some(
            Token {
                value: TokenValue::Dot,
                span
            }
        ),
        ',' => Some(
            Token {
                value: TokenValue::Comma,
                span
            }
        ),
        '=' => {
            let next = chars.peek();
            match next {
                Some('=') => {
                    chars.next();
                    Some(
                        Token {
                            value: TokenValue::Eq,
                            span
                        }
                    )
                }
                _ => Some(
                    Token {
                        value: TokenValue::Assign,
                        span
                    }
                )
            }
        },
        '!' => {
            let next = chars.peek();
            match next {
                Some('=') => {
                    chars.next();
                    Some(
                        Token {
                            value: TokenValue::Ne,
                            span
                        }
                    )
                }
                _ => Some(
                    Token {
                        value: TokenValue::Not,
                        span
                    }
                )
            }
        },
        '>' => {
            let next = chars.peek();
            match next {
                Some('=') => {
                    chars.next();
                    Some(
                        Token {
                            value: TokenValue::Gte,
                            span
                        }
                    )
                }
                _ => Some(
                    Token {
                        value: TokenValue::Gt,
                        span
                    }
                )
            }
        },
        '<' => {
            let next = chars.peek();
            match next {
                Some('=') => {
                    chars.next();
                    Some(
                        Token {
                            value: TokenValue::Lte,
                            span
                        }
                    )
                }
                Some('<') => {
                    chars.next();
                    Some(
                        Token {
                            value: TokenValue::Mutate,
                            span
                        }
                    )
                }
                _ => Some(
                    Token {
                        value: TokenValue::Lt,
                        span
                    }
                )
            }
        },
        '&' => {
            let next = chars.peek();
            if let Some('&') = next {
                chars.next();
                Some(
                    Token {
                        value: TokenValue::And,
                        span
                    }
                )
            } else {
                Some(
                    Token {
                        value: TokenValue::Pipe,
                        span
                    }
                )
            }
        },
        '|' => {
            if let Some('|') = chars.peek() {
                chars.next();
                Some(
                    Token {
                        value: TokenValue::Or,
                        span
                    }
                )
            } else {
                Some(
                    Token {
                        value: TokenValue::Pipe,
                        span
                    }
                )
            }
        }
        '/' => {
            match chars.peek() {
                Some('/') => {
                    while let Some(c) = chars.next() {
                        if c == '\n' {
                            break;
                        }
                    };
                    next_token(chars)
                }
                _ => Some(
                    Token {
                        value: TokenValue::Slash,
                        span
                    }
                )
            }
        }
        '*' | '+' | '-' | '%' | ':' => Some(
            char.try_into().map(|value| {
                Token {
                    value,
                    span
                }
            }
        ).unwrap()),
        '"' => parse_str(chars, span, '"'),
        '\'' => parse_str(chars, span, '\''),
        c => {
            if is_alpha(char) || is_emoji(char) {
                Some(parse_identifier(c, chars, span))
            } else if is_numeric(char) {
                Some(parse_num(c, chars, span))
            } else {
                dbg!(c, c,c);
                dbg!(chars.clone().chars.collect::<String>());
                panic!("XcharD")
            }
        }
    }
}

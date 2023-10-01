use crate::parser;
use crate::parser::ParseError;
use crate::parser::TokenStream;
use crate::parser::lexer;
use crate::parser::PResult;
use crate::parser::Block;
use crate::parser::lexer::Span;
use crate::parser::lexer::Token;
use crate::runtime;
use crate::runtime::builtins;
use crate::runtime::scope::Scope;
use crate::runtime::RuntimeError;
use std::fs;
use std::io::{self, Write};

pub fn interactive() -> Result<(), i32> {
    let mut scope = get_scope();
    loop {
        print!("> ");
        let mut input = String::new();
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read input");

        {
            if &input.trim().to_lowercase() == "exit" {
                break;
            }

            let mut stream = TokenStream::from_tokens(lexer::tokenize(&input, None));

            match parser::parse(&mut stream) {
                Ok(ast) => {
                    match ast {
                        Block::Statement(stm) => {
                            if let Err(RuntimeError(span, e)) = dbg!(runtime::evaluate_stmt(&mut scope, &stm)) {
                                handle_runtime_err(span, e, &input);
                                return Err(1)
                            }
                        }
                        Block::Expression(e) => {
                            if let Err(RuntimeError(span, e)) = runtime::evaluate(&mut scope, &e) {
                                handle_runtime_err(span, e, &input);
                                return Err(1)
                            }
                        }
                    };
                }
                Err(ParseError::Interrupt(e, t)) => {
                    handle_parse_err(t, e, &input);
                    return Err(2);
                },
                Err(ParseError::Continue) => {
                    panic!("bug in parser")
                }
            };
        }
    }

    Ok(())
}

pub fn run_program(args: &[String]) -> Result<(), i32> {
    let path = &args[0];
    let program = fs::read_to_string(path).unwrap();
    let mut stream = TokenStream::from_tokens(lexer::tokenize(&program, Some(path.to_owned())));
    let mut scope = get_scope();

    loop {
        if stream.is_empty() {
            break;
        };

        match parser::parse(&mut stream) {
            Ok(ast) => {
                match ast {
                    Block::Statement(stm) => {
                        if let Err(RuntimeError(span, e)) = runtime::evaluate_stmt(&mut scope, &stm) {
                            handle_runtime_err(span, e, &program);
                            return Err(1)
                        }
                    }
                    Block::Expression(e) => {
                        if let Err(RuntimeError(span, e)) = runtime::evaluate(&mut scope, &e) {
                            handle_runtime_err(span, e, &program);
                            return Err(1)
                        }
                    }
                };
            }
            Err(ParseError::Interrupt(e, t)) => {
                handle_parse_err(t, e, &program);
                return Err(2);
            },
            Err(ParseError::Continue) => {
                panic!("bug in parser")
            }
        };
    }

    Ok(())
}

fn get_scope() -> Scope {
    let mut s = Scope::root();
    for (name, f) in builtins::default() {
        s.set(name, f)
    }
    s
}

fn find_error_on_string(main_text: &str, substring: &str) -> Option<(usize, usize)> {
    if let Some(start_index) = main_text.find(substring) {
        let (line, column) = main_text[..start_index]
            .chars()
            .fold((1, 1), |(line, column), c| {
                if c == '\n' {
                    (line + 1, 1)
                } else {
                    (line, column + 1)
                }
            });

        return Some((line, column));
    }

    None
}

fn handle_runtime_err(span: Span, e: String, program: &str) {
    let (line, col) = (span.row, span.col);
    let fail_line = program.lines().collect::<Vec<&str>>()[line];
    let indicator = fail_line[..col].chars().map(|c| if c == '\t' { '\t' } else { ' ' }).collect::<String>() + "^";
    eprintln!("[RuntimeError]: {}\n{}\n{}", e, fail_line, indicator);
}

fn handle_parse_err(token: Token, e: &str, p: &str) {
    let (line, col) = (token.span.row, token.span.col);
    let t = token.value;
    let fail_line = p.lines().collect::<Vec<&str>>()[line];
    let indicator = fail_line[..col].chars().map(|c| if c == '\t' { '\t' } else { ' ' }).collect::<String>() + "^";
    eprintln!("[ParseError]: {}\n{}\n{}\n\tfound: {:?}", e, fail_line, indicator, t);
}

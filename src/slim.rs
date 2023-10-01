use nom::Err;

use crate::prelude;
use crate::parser;
use crate::parser::ParseError;
use crate::parser::TokenStream;
use crate::parser::lexer;
use crate::parser::PResult;
use crate::parser::Block;
use crate::parser::lexer::Span;
use crate::parser::lexer::Token;
use crate::parser::parse_ast;
use crate::runtime;
use crate::runtime::builtins;
use crate::runtime::scope::Scope;
use crate::runtime::RuntimeError;
use std::fs;
use std::io::{self, Write};
use std::vec;

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
                                eprintln!("{}", get_runtime_err_msg(span, e, &input));
                                return Err(1)
                            }
                        }
                        Block::Expression(e) => {
                            if let Err(RuntimeError(span, e)) = runtime::evaluate(&mut scope, &e) {
                                eprintln!("{}", get_runtime_err_msg(span, e, &input));
                                return Err(1)
                            }
                        }
                    };
                }
                Err(ParseError::Interrupt(e, t)) => {
                    eprintln!("{}", get_parse_err_msg(t, e, &input));
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
    let mut scope = get_scope();
    let ast = match parse_ast(&program) {
        Ok(ast) => ast,
        Err(ParseError::Interrupt(e, t)) => {
            eprintln!("{}", get_parse_err_msg(t, e, &program));
            return Err(2);
        },
        Err(ParseError::Continue) => {
            panic!("bug in parser")
        }
    };

    execute_tree(&mut scope, &ast).map_err(|e| {
        eprintln!("{}", get_runtime_err_msg(e.0, e.1, &program));
        1
    })
}

pub fn execute_tree(scope: &mut Scope, ast: &[Block]) -> Result<(), RuntimeError> {
    for block in ast {
        match block {
            Block::Statement(stm) => {
                let _ = runtime::evaluate_stmt(scope, &stm)?;
            }
            Block::Expression(e) => {
                let _ = runtime::evaluate(scope, &e)?;
            }
        };
    };

    Ok(())
}

fn get_scope() -> Scope {
    let mut scope = Scope::root();
    for (name, f) in builtins::default() {
        scope.set(name, f)
    }
    let ast = parse_ast(&prelude::PRELUDE).expect("Failed to parse prelude");
    let _ = execute_tree(&mut scope, &ast).expect("Failed to execute prelude");
    scope
}

pub fn get_runtime_err_msg(span: Span, e: String, program: &str) -> String {
    let (line, col) = (span.row, span.col);
    let fail_line = program.lines().collect::<Vec<&str>>()[line];
    let indicator = fail_line[..col].chars().map(|c| if c == '\t' { '\t' } else { ' ' }).collect::<String>() + "^";
    format!("[RuntimeError]: {}\n{}\n{}", e, fail_line, indicator)
}

pub fn get_parse_err_msg(token: Token, e: &str, p: &str) -> String {
    let (line, col) = (token.span.row, token.span.col);
    let t = token.value;
    let fail_line = p.lines().collect::<Vec<&str>>()[line];
    let indicator = fail_line[..col].chars().map(|c| if c == '\t' { '\t' } else { ' ' }).collect::<String>() + "^";
    format!("[ParseError]: {}\n{}\n{}\n\tfound: {:?}", e, fail_line, indicator, t)
}

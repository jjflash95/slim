use crate::parser::lexer::Span;
use crate::parser::lexer::Token;
use crate::parser::lexer::TokenValue;
use crate::parser::parse_ast;
use crate::parser::Block;
use crate::parser::ParseError;
use crate::runtime;
use crate::runtime::builtins;
use crate::runtime::scope::Scope;
use crate::runtime::RuntimeError;
use std::fs;
use std::io::Read;
use std::io::{self, Write};

pub fn interactive() -> Result<(), i32> {
    let mut scope = get_scope();
    let mut program = String::new();
    loop {
        print!("> ");
        let mut input = String::new();
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read input");
        program.push_str(&input);

        {
            if &input.trim().to_lowercase() == "exit" {
                break;
            }

            match parse_ast(&input, None) {
                Ok(ast) => {
                    for b in ast {
                        match b {
                            Block::Statement(stm) => {
                                if let Err(RuntimeError(span, e)) =
                                    runtime::evaluate_stmt(&mut scope, &stm)
                                {
                                    eprintln!("{}", get_runtime_err_msg(&span, e, Some(&program)));
                                }
                            }
                            Block::Expression(e) => {
                                if let Err(RuntimeError(span, e)) =
                                    runtime::evaluate(&mut scope, &e)
                                {
                                    eprintln!("{}", get_runtime_err_msg(&span, e, Some(&program)));
                                }
                            }
                        };
                    }
                }
                Err(ParseError::Interrupt(e, t)) => {
                    eprintln!("{}", get_parse_err_msg(t, e, Some(&program)));
                }
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
    let ast = match parse_ast(&program, Some(path.to_owned())) {
        Ok(ast) => ast,
        Err(ParseError::Interrupt(e, t)) => {
            eprintln!("{}", get_parse_err_msg(t, e, None));
            return Err(2);
        }
        Err(ParseError::Continue) => {
            panic!("bug in parser")
        }
    };

    execute_tree(&mut scope, &ast).map_err(|e| {
        eprintln!("{}", get_runtime_err_msg(&e.0, e.1, None));
        1
    })
}

pub fn execute_tree(scope: &mut Scope, ast: &[Block]) -> Result<(), RuntimeError> {
    for block in ast {
        match block {
            Block::Statement(stm) => {
                runtime::evaluate_stmt(scope, stm)?;
            }
            Block::Expression(e) => {
                let _ = runtime::evaluate(scope, e)?;
            }
        };
    }

    Ok(())
}

fn get_scope() -> Scope {
    let mut scope = Scope::root();
    for (name, f) in builtins::default() {
        scope.add_builtin(name, f)
    }
    let prelude = include_bytes!("./prelude");
    let ast = parse_ast(
        String::from_utf8_lossy(prelude).as_ref(),
        Some("./prelude".to_string()),
    )
    .expect("Failed to parse prelude");
    execute_tree(&mut scope, &ast).expect("Failed to execute prelude");
    scope
}

pub fn get_file_contents(span: &Span, program: Option<&str>) -> String {
    let mut c = String::new();
    match span.filename.as_str() {
        "<stdin>" => {
            c = program.unwrap().to_string();
        }
        "./prelude" => {
            c = String::from_utf8_lossy(include_bytes!("./prelude")).to_string();
        }
        path => {
            fs::File::open(path)
                .unwrap()
                .read_to_string(&mut c)
                .unwrap();
        }
    };
    c
}

pub fn get_runtime_err_msg(spans: &[Span], e: String, program: Option<&str>) -> String {
    let fail_lines = spans
        .iter()
        .rev()
        .map(|s| {
            let contents = get_file_contents(s, program);
            contents
                .lines()
                .map(|l| {
                    (
                        format!("File: \"{}\", line {}", s.filename, s.row + 1),
                        l.trim_end().to_owned(),
                        s.to_owned(),
                    )
                })
                .collect::<Vec<(String, String, Span)>>()[s.row]
                .clone()
        })
        .collect::<Vec<(String, String, Span)>>();

    eprintln!("Traceback (most recent call last):");
    for (n, (header, line, span)) in fail_lines.iter().enumerate() {
        let _line = if n == 0 && span.filename == "<stdin>" {
            let ls: Vec<&str> = program.unwrap().trim_end().lines().collect();
            ls[ls.len() - 1]
        } else {
            line
        };
        let indicator = line[..span.col]
            .chars()
            .map(|c| if c == '\t' { '\t' } else { ' ' })
            .collect::<String>()
            + "^";
        eprint!("{}\n{}\n{}\n\n", header, _line, indicator);
    }
    e.to_string()
}

pub fn get_parse_err_msg(token: Token, e: &str, program: Option<&str>) -> String {
    let span = token.span.clone();
    let row = token.span.row;
    let mut p = String::new();
    let fail_line = if program.is_none() {
        let _ = fs::File::open(span.filename)
            .unwrap()
            .read_to_string(&mut p)
            .unwrap();
        p.lines().collect::<Vec<&str>>()[row].trim_end()
    } else {
        program
            .unwrap()
            .lines()
            .collect::<Vec<&str>>()
            .last()
            .unwrap()
            .trim_end()
    };

    let col = match &token.value {
        TokenValue::EOF => fail_line.len() - 1,
        _ => token.span.col,
    };
    let indicator = fail_line[..col]
        .chars()
        .map(|c| if c == '\t' { '\t' } else { ' ' })
        .collect::<String>()
        + "^";
    format!(
        "[ParseError]: {} {:?}\n{}\n{}\n",
        e, token.value, fail_line, indicator
    )
}

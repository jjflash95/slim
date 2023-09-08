use crate::parser;
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

            match parser::parse(&input) {
                Ok((_, ast)) => {
                    let r = runtime::evaluate(&mut scope, ast);

                    if let Err(RuntimeError(e)) = r {
                        handle_runtime_err("".to_string(), " ", e)
                    }
                }
                Err(e) => {
                    eprintln!("[ParseError] {:?}", e);
                }
            };
        }
    }

    Ok(())
}

pub fn run_program(args: &[String]) -> Result<(), i32> {
    let path = &args[0];
    let program = fs::read_to_string(path).unwrap();
    let p = program.clone();
    let mut input = program.as_str();
    let mut scope = get_scope();

    loop {
        if [""].contains(&input.trim()) {
            break;
        };

        match parser::parse(input) {
            Ok((i, ast)) => {
                input = i;
                if let Err(RuntimeError(e)) = runtime::evaluate(&mut scope, ast) {
                    handle_runtime_err(p, i, e);
                    return Err(1);
                }
            }
            Err(nom::Err::Error(e)) => {
                handle_parse_err(p, e);
                return Err(1);
            }
            e => {
                eprintln!("[ParseError]: {:?}", e);
                return Err(2);
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

fn handle_runtime_err(p: String, remaining: &str, e: String) {
    if let Some((line, col)) = find_error_on_string(&p, remaining) {
        let fail_line = p.lines().collect::<Vec<&str>>()[line - 1];
        let mut indicator: String = " ".repeat(col - 1);

        indicator.push('^');
        eprintln!(
            "{}",
            &format!("line: {}\n\t{}\n\t{}", line, fail_line, indicator)
        )
    };
    eprintln!("[RuntimeError]: {}", e);
}

fn handle_parse_err(p: String, e: nom::error::Error<&str>) {
    if let Some((line, col)) = find_error_on_string(&p, e.input) {
        let fail_line = p.lines().collect::<Vec<&str>>()[line - 1];
        let mut indicator: String = " ".repeat(col - 1);

        indicator.push('^');
        eprintln!(
            "{}",
            &format!("line: {}\n\t{}\n\t{}", line, fail_line, indicator)
        )
    };
    eprintln!("[ParseError]: Invalid syntax");
}

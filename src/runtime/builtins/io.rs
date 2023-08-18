use std::io::Write;

use crate::runtime::{context::RuntimeContext, EResult, Value};

pub fn print(vals: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    for val in vals {
        println!("{}", val);
    }
    Ok(Value::Nil)
}

pub fn debug(vals: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    for val in vals {
        println!("{:?}", val);
    }
    Ok(Value::Nil)
}

pub fn input(vals: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    if !vals.is_empty() {
        let prompt: String = vals.remove(0).try_into()?;
        println!("{}", &prompt);
    }
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    Ok(Value::StringLiteral(input))
}

pub fn clear(_: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    print!("\x1B[2J\x1B[1;1H"); // ANSI escape code to clear the screen
    std::io::stdout().flush().unwrap();
    Ok(Value::Nil)
}

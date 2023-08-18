use std::time::SystemTime;

use crate::runtime::builtins::next_arg;
use crate::runtime::{context::RuntimeContext, EResult, Value, operations::Helpers, RuntimeError};



pub fn concat(mut args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let mut first = next_arg(&mut args, "string")?;
    for arg in args {
        first = first.concat(arg.clone())?
    }

    Ok(first)   
}

pub fn len(mut args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let arg = next_arg(&mut args, "string|sequence|collection")?;
    Ok(match arg {
        Value::StringLiteral(s) => Value::Int(s.len() as i128),
        Value::Sequence(values) => Value::Int(values.len() as i128),
        _ => Value::Nil,
    })
}

pub fn lowercase(mut args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let arg = next_arg(&mut args, "string")?;
    Ok(match arg {
        Value::StringLiteral(s) => Value::StringLiteral(s.to_lowercase()),
        _ => Err(RuntimeError(format!("Expected string but got <{}> instead", arg)))?
    })
}

pub fn trim(mut args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let arg = next_arg(&mut args, "string")?;
    Ok(match arg {
        Value::StringLiteral(s) => Value::StringLiteral(s.trim().to_string()),
        _ => Err(RuntimeError(format!("Expected string but got <{}> instead", arg)))?
    })
}

pub fn replace(mut args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let arg = next_arg(&mut args, "string")?;
    let from: String = next_arg(&mut args, "from")?.try_into()?;
    let to: String = next_arg(&mut args, "to")?.try_into()?;

    Ok(match arg {
        Value::StringLiteral(s) => Value::StringLiteral(s.replace(&from, &to)),
        _ => Err(RuntimeError(format!("Expected string but got <{}> instead", arg)))?
    })
}

pub fn range(mut args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let start = next_arg(&mut args, "start")?.try_into()?;
    let end: i128 = next_arg(&mut args, "end")?.try_into()?;
    let step: i128 = if let Ok(arg) = next_arg(&mut args, "step") {
        arg.try_into()?
    } else {
        1
    };

    Ok(Value::Sequence((start..end).step_by(step as usize).map(|i| Value::Int(i)).collect()))
}

pub fn pop(args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let seq = &mut args[0];
    if let Value::Sequence(values) = seq {
        Ok(values.pop().unwrap_or(Value::Nil))
    } else {
        Ok(Value::Nil)
    }
}

pub fn slice(mut args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    if args.len() < 3 {
        return Err(RuntimeError(format!("Slice needs 3 arguments")))?
    }

    let iterable = next_arg(&mut args, "sequence")?;
    let start: i128 = next_arg(&mut args, "start")?.try_into()?;
    let end: i128 = next_arg(&mut args, "end")?.try_into()?;
    match iterable {
        Value::Sequence(seq) => Ok(Value::Sequence(seq[start as usize..end as usize].to_vec())),
        Value::StringLiteral(s) => Ok(Value::StringLiteral(s[start as usize..end as usize].to_string())),
        _ => Err(RuntimeError(format!("Slice needs a sequence or string as first argument, got: {}", iterable.type_hint())))
    }
}

pub fn to_string(mut args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let value = next_arg(&mut args, "string")?;
    Ok(Value::StringLiteral(value.to_string()))
}

pub fn random(mut args: &mut Vec<Value>, _: RuntimeContext) -> EResult {
    let lower: i128 = next_arg(&mut args, "lower")?.try_into()?;
    let upper: i128 = next_arg(&mut args, "upper")?.try_into()?;
    let mut seed = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    seed ^= seed << 21;
    seed ^= seed >> 35;
    seed ^= seed << 4;
    let range = upper - lower + 1;
    Ok(Value::Int(lower + (seed as i128 % range as i128)))
}

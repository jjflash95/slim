mod parser;
mod runtime;
mod slim;

use std::env;

fn main() -> Result<(), i32> {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        slim::run_program(&args[1..])
    } else {
        slim::interactive()
    }
}

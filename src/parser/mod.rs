pub mod lexer;
pub mod parser;

pub use lexer::*;
pub use parser::*;

pub fn parse_ast(program: &str, file: Option<String>) -> Result<Vec<Block>, ParseError> {
    let mut stream = TokenStream::from_tokens(lexer::tokenize(program, file));
    let mut ast = vec![];

    while !stream.is_empty() {
        match parser::parse(&mut stream) {
            Ok(a) => ast.push(a),
            Err(e) => match e {
                ParseError::Interrupt(e, t) => {
                    return Err(ParseError::Interrupt(e, t));
                }
                ParseError::Continue => {
                    return Err(ParseError::Interrupt(
                        "Unexpected token",
                        stream.next().unwrap_or_default(),
                    ))
                }
            },
        };
    }

    Ok(ast)
}

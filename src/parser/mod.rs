pub mod lexer;
pub mod parser;

pub use lexer::*;
pub use parser::*;

pub fn parse_ast(program: &str) -> Result<Vec<Block>, ParseError> {
    let mut stream = TokenStream::from_tokens(lexer::tokenize(program, None));
    let mut ast = vec![];

    while !stream.is_empty() {
        match parser::parse(&mut stream) {
            Ok(a) => ast.push(a),
            Err(e) => return Err(e),
        };
    }

    Ok(ast)
}

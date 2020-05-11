mod lexer;
pub mod ast;

use lexer::{ 
    SourcePos, 
    TokenKind, 
    Token, 
    Lexer, 
    LexerError 
};
use ast::{ TypeExpression };
use crate::IdCounter;
use crate::tiny_string::TinyString;

/// Common arguments to parse functions,
/// in one, unified place!
struct Parser<'a> {
    lexer: Lexer<'a>,
    ids: &'a IdCounter,
}

impl Parser<'_> {
    fn file(&self) -> TinyString {
        self.lexer.file
    }

    // TODO: Find a better name for this?
    fn expect_peek_token(
        &mut self, 
        n_forward: usize,
    ) -> Result<Token, ParseError> {
        Ok(
            self.lexer
                .peek_token(n_forward)?
                .ok_or_else(
                    || ParseError::EndOfFile(self.file())
                )?
        )
    }
}

#[derive(Debug)]
pub enum CompilationUnit {

}

/// Like parsing a namespace, but we parse it in a file.
/// The difference is that a file can import other files,
/// while a namespace cannot.
pub fn parse_file(
    path: &str,
    ids: &IdCounter,
    mut comp_units: impl FnMut(CompilationUnit),
) -> Result<u32, ParseError> {
    // Create the lexer
    let file = std::fs::read_to_string(path)?;
    let mut parser = Parser {
        lexer: Lexer::new(path.into(), &file),
        ids,
    };

    let namespace_id = ids.create_id();

    let type_expr = parse_type(&mut parser, namespace_id)?;
    println!("{:?}", type_expr);

    Ok(namespace_id)
}

fn parse_type(
    parser: &mut Parser<'_>,
    namespace_id: u32,
) -> Result<TypeExpression, ParseError> {
    let token = parser.expect_peek_token(0)?;

    use TokenKind::*;
    match token.kind {
        Identifier(name) => {
            // A named type(or a primitive)
            Ok(TypeExpression::NamedType {
                pos: token.pos(parser.file()),
                namespace_id,
                name,
            })
        }
        _ => Err(ParseError::InvalidToken {
            pos: token.pos(parser.file()),
            kind: token.kind,
            message: format!("Expected a type"),
        }),
    } 
}

#[derive(Debug)]
pub enum ParseError {
    Io(std::io::Error),
    Lexer(LexerError),
    InvalidToken {
        pos: SourcePos, 
        kind: TokenKind, 
        message: String,
    },
    EndOfFile(TinyString),
}

impl From<LexerError> for ParseError {
    fn from(other: LexerError) -> ParseError {
        ParseError::Lexer(other)
    }
}

impl From<std::io::Error> for ParseError {
    fn from(other: std::io::Error) -> ParseError {
        ParseError::Io(other)
    }
}

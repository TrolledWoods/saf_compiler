mod lexer;
mod keyword;
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
use keyword::Keyword as KeywordKind;
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

    fn expect_eat_token(
        &mut self,
    ) -> Result<Token, ParseError> {
        Ok(
            self.lexer
                .eat_token()?
                .ok_or_else(
                    || ParseError::EndOfFile(self.file())
                )?
        )
    }

    fn eat_token(
        &mut self,
    ) -> Result<Option<Token>, ParseError> {
        Ok(self.lexer.eat_token()?)
    }
}

#[derive(Debug)]
pub enum CompilationUnit {
    TypeDefinition {
        pos: SourcePos,
        namespace_id: u32,
        name: Identifier,
        definition: TypeExpression,
    },
}

/// Like parsing a namespace, but we parse it in a file.
/// The difference is that a file can import other files,
/// while a namespace cannot.
pub fn parse_file(
    path: &str,
    ids: &IdCounter,
    mut add_comp_unit: impl FnMut(CompilationUnit),
) -> Result<u32, ParseError> {
    // Create the lexer
    let file = std::fs::read_to_string(path)?;
    let mut parser = Parser {
        lexer: Lexer::new(path.into(), &file),
        ids,
    };

    let namespace_id = ids.create_id();

    while let Some(token) = parser.lexer.peek_token(0)? {
        use TokenKind::*;
        match token.kind {
            Keyword(KeywordKind::TypeDef) => add_comp_unit(parse_type_def(&mut parser, namespace_id)?),
            _ => return Err(ParseError::InvalidToken {
                pos: token.pos(parser.file()),
                kind: token.kind,
                message: format!(
                    "Expected a namespace item definition, \
                    'type Blah = f32;' or \
                    'const main = func () ( let x = 42; );"
                ),
            })
        }
    }

    Ok(namespace_id)
}

const TYPE_DEF_FORM: &str = r#"
Type definitions have the form
``type [identifier] = [type];``
"#;

/// Parse a type definition.
/// ``type [identifier] = [parse_type];``
fn parse_type_def(
    parser: &mut Parser<'_>,
    namespace_id: u32,
) -> Result<CompilationUnit, ParseError> {
    let pos = parse_kind(
        parser,
        KeywordKind::TypeDef,
        |_| unreachable!(
            "Nothing should be calling the \
            parse_type_def function without making sure \
            that the keyword ``type`` exists there first"
        ),
    )?;

    let name = parse_identifier(
        parser,
        |_| format!(
            "Expected an identifier in type definition.\n{}",
            TYPE_DEF_FORM
        ),
    )?;

    parse_kind(
        parser,
        TokenKind::AssignmentOperator(""),
        |_| format!(
            "Expected an '=' operator in type definition.\n{}",
            TYPE_DEF_FORM
        ),
    )?;

    let definition = parse_type(parser, namespace_id)?;

    parse_kind(
        parser,
        TokenKind::Terminator,
        |_| format!("Expected a ';' after type definition."),
    )?;

    Ok(CompilationUnit::TypeDefinition {
        pos,
        namespace_id,
        name,
        definition,
    })
}

/// Parses an identifier.
/// Every identifier is just a single token,
/// which contains only alphabetical characters,
/// digits(excepts the first character), and
/// underscores.
/// An identifier is also never a keyword.
/// If you want a list of all the keywords,
/// just take a looks at the ``lexer.rs`` file
fn parse_identifier(
    parser: &mut Parser<'_>,
    error_message: impl FnOnce(&Token) -> String,
) -> Result<Identifier, ParseError> {
    let token = parser.expect_eat_token()?;
    match token.kind {
        TokenKind::Identifier(name) => Ok(Identifier {
            pos: token.pos(parser.file()),
            name,
        }),
        _ => Err(ParseError::InvalidToken {
            pos: token.pos(parser.file()),
            message: error_message(&token),
            kind: token.kind,
        }),
    }
}

/// Parses any type.
fn parse_type(
    parser: &mut Parser<'_>,
    namespace_id: u32,
) -> Result<TypeExpression, ParseError> {
    let token = parser.expect_peek_token(0)?;

    use TokenKind::*;
    match token.kind {
        Identifier(name) => {
            // A named type(or a primitive)
            parser.eat_token()?;
            Ok(TypeExpression::NamedType {
                pos: token.pos(parser.file()),
                namespace_id,
                name,
            })
        }
        Keyword(KeywordKind::Null) | 
        Keyword(KeywordKind::Mutable) |
        Operator("*") => 
            parse_pointer(parser, namespace_id),
        _ => Err(ParseError::InvalidToken {
            pos: token.pos(parser.file()),
            kind: token.kind,
            message: format!("Expected a type"),
        }),
    } 
}

fn parse_pointer(
    parser: &mut Parser<'_>,
    namespace_id: u32,
) -> Result<TypeExpression, ParseError> {
    let token = parser.expect_eat_token()?;
    let start = token.start;
    match token.kind {
        Operator("*") => (),
        _ => return Err(ParseError::InvalidToken {
            pos: token.pos(parser.file()),
            kind: token.kind,
            message: format!(
                "'null' or 'mut' keywords in a \
                type are always before a pointer, \
                but there is no '*' here"),
        }),
    }

    use TokenKind::*;
    let token = parser.expect_peek_token(0)?;
    let start = token.start;

    let mut mutable = try_parse_kind(
        parser,
        Keyword(KeywordKind::Mutable),
    )?;

    let nullable = try_parse_kind(
        parser,
        Keyword(KeywordKind::Null),
    )?;

    // Make the keywords order independant
    if !mutable {
        mutable = try_parse_kind(
            parser,
            Keyword(KeywordKind::Mutable),
        )?;
    }

    let internal = parse_type(
        parser, 
        namespace_id
    )?;

    Ok(TypeExpression::Pointer {
        pos: SourcePos {
            file: parser.file(),
            start,
            end: token.end,
        },
        mutable,
        nullable,
        pointing_to: Box::new(internal),
    })
}

fn parse_kind(
    parser: &mut Parser,
    kind: impl PartialEq<TokenKind>,
    generate_error: impl FnOnce(&Token) -> String,
) -> Result<SourcePos, ParseError> {
    match parser.expect_eat_token()? {
        token if kind == token.kind => 
            Ok(token.pos(parser.file())),
        token => Err(ParseError::InvalidToken {
            pos: token.pos(parser.file()),
            message: generate_error(&token),
            kind: token.kind,
        })
    }
}

fn try_parse_kind(
    parser: &mut Parser, 
    kind: impl PartialEq<TokenKind>
) -> Result<bool, ParseError> {
    match parser.lexer.peek_token(0)? {
        Some(token) if kind == token.kind => {
            parser.expect_eat_token().unwrap();
            Ok(true)
        }
        _ => Ok(false)
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub pos: SourcePos,
    pub name: TinyString,
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

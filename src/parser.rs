mod lexer;
mod keyword;
pub mod ast;

pub use lexer::SourcePos;
pub use lexer::Literal;
use lexer::{ 
    TokenKind, 
    Token, 
    Lexer, 
    LexerError 
};
use ast::{ 
    PrimitiveKind, 
    TypeExpression, 
    Expression,
    Statement,
};
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
    ) -> ParseResult<Token> {
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
    ) -> ParseResult<Token> {
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
    ) -> ParseResult<Option<Token>> {
        Ok(self.lexer.eat_token()?)
    }

    fn peek_token(
        &mut self, 
        n_forward: usize,
    ) -> ParseResult<Option<Token>> {
        Ok(self.lexer.peek_token(n_forward)?)
    }
}

#[derive(Debug)]
pub enum CompilationUnit {
    Constant {
        pos: SourcePos,
        namespace_id: usize,
        name: Identifier,
        expression: Expression,
    },
    TypeDefinition {
        pos: SourcePos,
        namespace_id: usize,
        name: Identifier,
        definition: TypeExpression,
        is_unique: bool,
    },
}

/// Like parsing a namespace, but we parse it in a file.
/// The difference is that a file can import other files,
/// while a namespace cannot.
pub fn parse_file(
    path: &str,
    ids: &IdCounter,
    mut add_comp_unit: impl FnMut(CompilationUnit),
) -> ParseResult<usize> {
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
            Keyword(KeywordKind::TypeDef) |
            Keyword(KeywordKind::Alias) => 
                add_comp_unit(
                    parse_type_def(&mut parser, namespace_id)?
                ),
            Keyword(KeywordKind::Const) => 
                add_comp_unit(
                    parse_constant_def(&mut parser, namespace_id)?
                ),
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

fn parse_constant_def(
    parser: &mut Parser<'_>,
    namespace_id: usize,
) -> ParseResult<CompilationUnit> {
    let pos = parse_kind(
        parser,
        KeywordKind::Const,
        |_| unreachable!("Don't call the parse_constant_def function when you do not have the 'const' keyword, please"),
    )?;

    let name = parse_identifier(
        parser,
        |_| format!("Expected identifier, constant definitions come in the form ``const [name] = [expression];``"),
    )?;
    
    let const_type = if try_parse_kind(
        parser,
        TokenKind::Declaration,
    )? {
        Some(parse_type(
            parser,
            namespace_id,
        )?)
    } else {
        None
    };

    parse_kind(
        parser,
        TokenKind::AssignmentOperator(""),
        |_| format!("Expected '=' in constant"),
    )?;

    let expression = parse_expression(
        parser,
        namespace_id,
    )?;

    parse_kind(
        parser,
        TokenKind::Terminator,
        |_| format!("Expected ';' at the end of constant definition"),
    )?;

    Ok(CompilationUnit::Constant {
        pos,
        namespace_id,
        name,
        expression,
    })
}

const TYPE_DEF_FORM: &str = r#"
Type definitions have the form
``type [identifier] = [type];``
"#;

/// Parse a type definition.
/// ``type [identifier] = [parse_type];``
fn parse_type_def(
    parser: &mut Parser<'_>,
    namespace_id: usize,
) -> ParseResult<CompilationUnit> {
    let token = parser.expect_eat_token()?;
    let (is_unique, pos) = match token {
        Token {
            kind: TokenKind::Keyword(KeywordKind::TypeDef),
            ..
        } => {
            (true, token.pos(parser.file()))
        }
        Token {
            kind: TokenKind::Keyword(KeywordKind::Alias),
            ..
        } => {
            (false, token.pos(parser.file()))
        }
        _ => return Err(ParseError::InvalidToken {
            pos: token.pos(parser.file()),
            kind: token.kind,
            message: format!("Wanted 'type' or 'alias'"),
        })
    };

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
        is_unique,
    })
}

fn parse_collection(
    parser: &mut Parser<'_>,
    namespace_id: usize,
) -> ParseResult<Vec<(
    TinyString, 
    TypeExpression, 
    Option<Expression>
)>> {
    let pos = parse_kind(
        parser, 
        TokenKind::Bracket('{'),
        |_| unreachable!("Don't call the parse_collection when there is not a bracket token beforehand!"),
        )?;

    let mut collection = Vec::new();
    let mut unnamed_ctr = 0;
    loop {
        // Is the termination already here?
        if try_parse_kind(
            parser,
            TokenKind::Bracket('}'),
        )? {
            break;
        }

        // Get/Generate the name
        let token = parser.expect_peek_token(0)?;
        let (pos, name) = match (
            &token.kind,
            parser.peek_token(1)?.map(|v| v.kind),
        ) {
            (
                &TokenKind::Identifier(name),
                Some(TokenKind::Declaration),
            ) => {
                let pos = parser.expect_eat_token()?
                    .pos(parser.file());
                parser.expect_eat_token()?;

                (pos, name)
            }
            (_, _) => {
                unnamed_ctr += 1;
                (
                    token.pos(parser.file()), 
                    format!("{}", unnamed_ctr - 1).into()
                )
            }
        };

        let type_ = parse_type(parser, namespace_id)?;

        collection.push((name, type_, None));

        if try_parse_kind(
            parser,
            TokenKind::Bracket('}'),
        )? {
            break;
        } else {
            parse_kind(
                parser,
                TokenKind::Separator,
                |_| format!("Expected ',' or ';'"),
            )?;
        }
    }

    Ok(collection)
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
    namespace_id: usize,
) -> Result<TypeExpression, ParseError> {
    let token = parser.expect_peek_token(0)?;

    use TokenKind::*;
    match token.kind {
        Identifier(name) => {
            // A named type(or a primitive)
            parser.eat_token()?;

            let primitive = {
                use PrimitiveKind::*;
                let name = name.read();
                match &*name {
                    "f32" => Some(Float32),
                    "f64" => Some(Float64),
                    "i32" => Some(Int32),
                    "i64" => Some(Int64),
                    _ => None,
                }
            };

            if let Some(primitive) = primitive {
                Ok(TypeExpression::Primitive {
                    pos: token.pos(parser.file()),
                    kind: primitive,
                })
            } else {
                Ok(TypeExpression::NamedType {
                    pos: token.pos(parser.file()),
                    namespace_id,
                    name,
                })
            }
        }
        Operator("*") => 
            parse_pointer(parser, namespace_id),
        Bracket('[') | ArrayWindow | DynamicArray => {
            // This is an array of some kind.
            Ok(parse_array(parser, namespace_id)?)
        }
        Bracket('{') => {
            Ok(TypeExpression::NamedCollection(
                parse_collection(parser, namespace_id)?
            ))
        }
        _ => Err(ParseError::InvalidToken {
            pos: token.pos(parser.file()),
            kind: token.kind,
            message: format!("Expected a type"),
        }),
    } 
}

fn parse_array(
    parser: &mut Parser<'_>,
    namespace_id: usize,
) -> ParseResult<TypeExpression> {
    let pos = parser.expect_peek_token(0)?.pos(parser.file());

    // A window or mutable array
    if try_parse_kind(parser, TokenKind::ArrayWindow)? {
        let internal = parse_type(parser, namespace_id)?;
        Ok(TypeExpression::VariableArray {
            pos,
            mutable: false,
            members: Box::new(internal),
        })
    } else if try_parse_kind(parser, TokenKind::DynamicArray)? {
        let internal = parse_type(parser, namespace_id)?;
        Ok(TypeExpression::VariableArray {
            pos,
            mutable: true,
            members: Box::new(internal),
        })
    } else {
        let start = parse_kind(
            parser,
            TokenKind::Bracket('['),
            |_| unreachable!("Shouldn't call parse_array when not sure that there isn't a '[', '[-]' or [?] bracket"),
        )?.start;

        let size = parse_expression(parser, namespace_id)?;
        
        let end = parse_kind(
            parser,
            TokenKind::Bracket(']'),
            |_| format!("Expected closing ']'"),
        )?.end;

        let internal = parse_type(parser, namespace_id)?;

        Ok(TypeExpression::FixedArray {
            pos: SourcePos {
                start, end, file: parser.file(),
            },
            size: Box::new(size),
            members: Box::new(internal),
        })
    }
}

fn parse_expression(
    parser: &mut Parser<'_>,
    namespace_id: usize,
) -> ParseResult<Expression> {
    let e = parse_expression_req(parser, namespace_id, 0)?;
    Ok(e)
}

/// Returns the operator precendence of the given operator.
/// Higher means higher priority, i.e. that operator
/// gets done first.
///
/// ``0`` is a reserved priority, so this function should
/// never return 0.
///
/// # Panics
/// Panics if the given string is not an operator.
fn operator_precedence(op: &str) -> u8 {
    match op {
        "==" => 1,
        "+" | "-" => 2,
        "*" | "/" => 3,
        _ => panic!("Invalid operator {}", op),
    }
}

fn parse_expression_req(
    parser: &mut Parser<'_>,
    namespace_id: usize,
    priority: u8,
) -> ParseResult<Expression> {
    let mut expression = parse_expression_value(
        parser,
        namespace_id,
    )?;

    loop {
        match parser.peek_token(0)? {
            Some(Token {
                kind: TokenKind::Operator(op),
                ..
            }) => {
                let new_priority = operator_precedence(op);
                if new_priority > priority {
                    let token = parser.expect_eat_token().unwrap();

                    let new = parse_expression_req(
                        parser,
                        namespace_id,
                        new_priority,
                    )?;

                    expression = Expression::Operation {
                        pos: token.pos(parser.file()),
                        operator: op,
                        arguments: vec![
                            expression,
                            new,
                        ],
                    };
                } else {
                    break;
                }
            },
            _ => break,
        }
    }

    Ok(expression)
}

fn parse_expression_value(
    parser: &mut Parser,
    namespace_id: usize,
) -> ParseResult<Expression> {
    use Expression::*;
    let token = parser.expect_peek_token(0)?;
    let expression = match &token.kind {
        TokenKind::Identifier(name) => {
            parser.expect_eat_token().unwrap();
            NamedValue {
                pos: token.pos(parser.file()),
                namespace_id,
                name: *name,
            }
        }
        TokenKind::Bracket('(') => {
            let block = parse_code_block(
                parser,
                namespace_id,
            )?;
            println!("{:?}", block);
            match block {
                Expression::Block {
                    return_value: Some(return_value),
                    code,
                    pos,
                } if code.len() == 0 => {
                    *return_value
                }
                other => other,
            }
        }
        TokenKind::Literal(value) => {
            parser.expect_eat_token().unwrap();
            Literal {
                pos: token.pos(parser.file()),
                data: value.clone(),
            }
        }
        kind => return Err(ParseError::InvalidToken {
            pos: token.pos(parser.file()),
            kind: kind.clone(),
            message: format!("Expected expression value"),
        }),
    };

    while try_parse_kind(
        parser,
        TokenKind::Bracket('('),
    )? {
        // Parse an argument list
        unimplemented!("Function argument list");
    }

    Ok(expression)
}

fn parse_code_block(
    parser: &mut Parser<'_>,
    namespace_id: usize,
) -> ParseResult<Expression> {
    let pos = parse_kind(
        parser,
        TokenKind::Bracket('('),
        |_| unreachable!("Don't call parse_code_block on anything other than a '(', please"),
    )?;

    let mut code = Vec::new();
    let mut return_value = None;

    loop {
        use TokenKind::*;
        let token = parser.expect_peek_token(0)?;
        match &token.kind {
            Keyword(KeywordKind::Let) => {
                // This is a declaration.
                parser.expect_eat_token().unwrap(); 

                let name = parse_identifier(
                    parser, 
                    |_| format!("Expected an identifier")
                )?;

                let var_type = if try_parse_kind(
                    parser,
                    Declaration,
                )? {
                    // The type
                    Some(parse_type(parser, namespace_id)?)
                } else {
                    None
                };

                parse_kind(
                    parser,
                    AssignmentOperator(""),
                    |_| format!("Expected '=', ``let [name] (*optionally : [type]) = [expression];``"),
                )?;

                let value = parse_expression(parser, namespace_id)?;

                code.push(Statement::Declaration {
                    declaring: name,
                    var_type,
                    value,
                });
            }
            Bracket(')') => break,
            _ => {
                let first = parse_expression(parser, namespace_id)?;
                let token = parser.expect_peek_token(0)?;
                match &token.kind {
                    Bracket(')') => {
                        parser.expect_eat_token().unwrap();

                        // If the expression was a non expressino
                        // block, then we don't want to set our
                        // return value to it.
                        if let Expression::Block {
                            return_value: None,
                            ..
                        } = &first {
                            code.push(Statement::Expression(first));
                            break;
                        } else {
                            return_value = Some(first);
                            break;
                        }
                    },
                    Terminator => {
                        parser.expect_eat_token().unwrap();
                        code.push(Statement::Expression(first));
                    }
                    AssignmentOperator(operator) => {
                        parser.expect_eat_token().unwrap();

                        let value = parse_expression(
                            parser, 
                            namespace_id,
                        )?;

                        parse_kind(
                            parser,
                            Terminator,
                            |_| format!("Expected ';' at the end of assignment."),
                        )?;

                        code.push(Statement::Assignment {
                            assigning: first,
                            operator,
                            value,
                        });
                    }
                    _ => unimplemented!("TODO: Invalid token when parsing block thingy"),
                };
            }
        }
    }

    Ok(Expression::Block {
        pos,
        code,
        return_value: return_value.map(|v| Box::new(v)),
    })
}

fn parse_pointer(
    parser: &mut Parser<'_>,
    namespace_id: usize,
) -> ParseResult<TypeExpression> {
    use TokenKind::*;
    let token = parser.expect_eat_token()?;
    let start = token.start;
    match token.kind {
        Operator("*") => (),
        _ => unreachable!(
            "Shouldn't call parse_pointer \
            function without having a \
            pointer operator"
        ),
    }

    let token = parser.expect_peek_token(0)?;
    let start = token.start;

    let mut mutable = false;
    let mut nullable = false;
    loop {
        let token = parser.expect_peek_token(0)?;
        match (&token.kind, mutable, nullable) {
            (&Keyword(KeywordKind::Mutable), false, _) => {
                parser.expect_eat_token().unwrap();
                mutable = true;
            }
            (&Keyword(KeywordKind::Mutable), true, _) =>
                return Err(ParseError::InvalidToken {
                    pos: token.pos(parser.file()),
                    kind: token.kind,
                    message: format!(
                        "Got a second ``mut`` modifier \
                        for a pointer, but one is enough"
                    ),
                }),
            (&Keyword(KeywordKind::Null), _, false) => {
                parser.expect_eat_token().unwrap();
                nullable = true;
            }
            (&Keyword(KeywordKind::Null), _, true) =>
                return Err(ParseError::InvalidToken {
                    pos: token.pos(parser.file()),
                    kind: token.kind,
                    message: format!(
                        "Got a second ``null`` modifier \
                            for a pointer, but one is enough"
                    ),
                }),
            _ => break,
        }
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

#[derive(Debug, Clone)]
pub struct Identifier {
    pub pos: SourcePos,
    pub name: TinyString,
}

type ParseResult<T> = Result<T, ParseError>;

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

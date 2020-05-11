use super::lexer::{ SourcePos, Literal };
use crate::tiny_string::TinyString;

#[derive(Debug)]
pub enum Expression {
    NamedValue {
        pos: SourcePos,
        namespace_id: u32,
        name: TinyString,
    },
    Block {
        pos: SourcePos,
        // code_block: Vec<Statement>,
    },
    Literal {
        pos: SourcePos,
        data: Literal,
    },
    Operation {
        pos: SourcePos,
        operator: &'static str,
        arguments: Vec<Expression>,
    },
    FunctionCall {
        pos: SourcePos,
        calling: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug)]
pub enum TypeExpression {
    NamedCollection(Vec<(TinyString, TypeExpression, Option<Expression>)>),
    UnnamedCollection(Vec<(TypeExpression, Option<Expression>)>),
    Pointer {
        pos: SourcePos,
        mutable: bool,
        nullable: bool,
        pointing_to: Box<TypeExpression>,
    },
    FixedArray {
        size: Box<Expression>,
        members: Box<TypeExpression>,
    },
    VariableArray {
        pos: SourcePos,
        mutable: bool,
        members: Box<TypeExpression>,
    },
    NamedType {
        pos: SourcePos,
        namespace_id: u32, 
        name: TinyString,
    },
}

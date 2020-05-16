use super::lexer::{ SourcePos, Literal };
use crate::tiny_string::TinyString;

#[derive(Debug)]
pub enum Expression {
    NamedValue {
        pos: SourcePos,
        namespace_id: usize,
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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum PrimitiveKind {
    Float32,
    Float64,
    Int32,
    Int64,
}

#[derive(Debug)]
pub enum TypeExpression {
    NamedCollection(Vec<(TinyString, TypeExpression, Option<Expression>)>),
    Pointer {
        pos: SourcePos,
        mutable: bool,
        nullable: bool,
        pointing_to: Box<TypeExpression>,
    },
    FixedArray {
        pos: SourcePos,
        size: Box<Expression>,
        // TODO: Rename to internal
        members: Box<TypeExpression>,
    },
    VariableArray {
        pos: SourcePos,
        mutable: bool,
        // TODO: Rename to internal
        members: Box<TypeExpression>,
    },
    NamedType {
        pos: SourcePos,
        namespace_id: usize, 
        name: TinyString,
    },
    Primitive {
        pos: SourcePos,
        kind: PrimitiveKind,
    },
    /// A unique type is special.
    /// A unique type will become its own type,
    /// no matter what the contents are. This is
    /// needed to allow for named types that aren't
    /// aliases.
    UniqueType(Box<TypeExpression>),
}

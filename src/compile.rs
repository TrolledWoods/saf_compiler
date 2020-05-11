use std::sync::{ RwLock, RwLockReadGuard };
use std::collections::HashMap;
use crate::tiny_string::TinyString;
use crate::parser::Identifier;
use crate::parser::ast::{
    TypeExpression,
    PrimitiveKind,
};

pub struct Compiler {
    types: RwLock<Vec<TypeUnit>>,
    namespaces: Namespaces,
}

/// A ResolvedType is a type where we know that no
/// infinite "sizing loops" occur, and where we know
/// that all the types that the ResolvedType depends
/// on are also ResolvedType:s.
/// Also, all constant expressions are calculated
/// in resolved types.
#[derive(Debug)]
pub enum ResolvedType {
    Collection(Vec<(TinyString, u32)>),
    /// Just a pointer to some other type
    Pointer {
        nullable: bool,
        mutable: bool,
        pointing_to: u32,
    },
    VariableArray {
        mutable: bool,
        content_type: u32,
    },
    FixedArray {
        size: usize,
        content_type: u32,
    },
    /// A wrapped type has the same representation
    /// as the type it wraps, but isn't treated
    /// as the same time.
    WrappedType(u32),
    Primitive(PrimitiveKind),
}

#[derive(Debug)]
enum ResolvingError {
    CircularSize(Identifier),
    InvalidDependency(Identifier),
    DependencyNotReady(CompileMemberId),
    // Add when const expressions are computed
    // Expression(ExpressionError),
}

fn resolve_type(
    compiler: &Compiler,
    resolving_id: u32,
) -> Result<ResolvedType, ResolvingError> {
    unimplemented!();
}

struct Namespaces {
    pub members: RwLock<HashMap<TinyString, CompileMemberId>>,
}

impl Namespaces {
    fn find_value(
        &self, 
        namespace_id: u32, 
        name: TinyString,
    ) -> Option<CompileMemberId> {
        self.members
            .read()
            .unwrap()
            .get(&name)
            .copied()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum CompileMemberId {
    Type(u32),
    Constant(u32),
}

struct TypeUnit {
    definition: TypeExpression,
    resolved: RwLock<Option<ResolvedType>>,
}


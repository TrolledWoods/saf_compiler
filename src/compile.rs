use std::sync::{ RwLock, RwLockReadGuard };
use std::collections::HashMap;
use crate::tiny_string::TinyString;
use crate::parser::Identifier;
use crate::parser::ast::{
    TypeExpression,
    PrimitiveKind,
};

// We want to know the type id of primitives.
const FLOAT_32_ID: u32 = 0;
const FLOAT_64_ID: u32 = 1;
const INT_32_ID: u32   = 2;
const INT_64_ID: u32   = 3;

pub struct Compiler {
    named_types: RwLock<Vec<TypeUnit>>,
    resolved_types: RwLock<Vec<ResolvedType>>,

    namespaces: Namespaces,
}

impl Compiler {
    pub fn new() -> Compiler {
        let mut types = {
            use ResolvedType::*;
            use PrimitiveKind::*;
            vec![
                Primitive(Float32),
                Primitive(Float64),
                Primitive(Int32),
                Primitive(Int64),
            ]
        };

        Compiler {
            named_types: RwLock::new(Vec::new()),
            resolved_types: RwLock::new(types),
            namespaces: Namespaces::new(),
        }
    }

    /// Returns the type id of a resolved type.
    /// There is one and exactly one type id for 
    /// every kind of type(except unique types, 
    /// who have a unique identifier). If there is already
    /// an index, we return that, if not, we create
    /// a new type id and return that.
    pub fn type_id(
        &self,
        index_of: &ResolvedType,
    ) -> u32 {
        let mut resolved_types = 
            self.resolved_types.write().unwrap();

        for (
            i, 
            old_type,
        ) in resolved_types.iter().enumerate() {
            if old_type == index_of {
                return i as u32;
            }
        }

        let id = resolved_types.len() as u32;
        println!("Added type id {}: {:#?}", id, index_of);
        resolved_types.push(index_of.clone());

        id
    }
}

type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub enum CompileError {
    Resolving(ResolvingError),
}

impl From<ResolvingError> for CompileError {
    fn from(other: ResolvingError) -> CompileError {
        CompileError::Resolving(other)
    }
}

/// A ResolvedType is a type where we know that no
/// infinite "sizing loops" occur, and where we know
/// that all the types that the ResolvedType depends
/// on are also ResolvedType:s.
/// Also, all constant expressions are calculated
/// in resolved types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
pub enum ResolvingError {
    /// The error was caused by another error,
    /// so this is not the cause, so don't report.
    Poison,
    CircularSize(u32, Identifier),
    InvalidDependency(u32, Identifier),
    DependencyNotReady(CompileMemberId),
    // Add when const expressions are computed
    // Expression(ExpressionError),
}

pub fn resolve_type(
    compiler: &Compiler,
    resolving: &TypeExpression,
) -> Result<u32, ResolvingError> {
    resolve_type_req(compiler, resolving, &mut Vec::new())
}

fn resolve_type_req(
    compiler: &Compiler,
    resolving: &TypeExpression,
    reqursion_guard: &mut Vec<CompileMemberId>,
) -> Result<u32, ResolvingError> {
    use TypeExpression::*;
    match resolving {
        Primitive {
            kind: PrimitiveKind::Float32, ..
        } => Ok(FLOAT_32_ID),
        Primitive {
            kind: PrimitiveKind::Float64, ..
        } => Ok(FLOAT_64_ID),
        Primitive {
            kind: PrimitiveKind::Int32, ..
        } => Ok(INT_32_ID),
        Primitive {
            kind: PrimitiveKind::Int64, ..
        } => Ok(INT_64_ID),
        Pointer {
            mutable, nullable, pointing_to, ..
        } => {
            let pointing_to = resolve_type_req(
                compiler,
                pointing_to,
                reqursion_guard,
            )?;

            let resolved = ResolvedType::Pointer {
                // rustc: please make this easier! :pray:
                mutable: *mutable, 
                nullable: *nullable, 
                pointing_to
            };

            Ok(compiler.type_id(&resolved))
        }
        _ => unimplemented!(),
    }
}

struct Namespaces {
    pub members: RwLock<HashMap<TinyString, CompileMemberId>>,
}

impl Namespaces {
    fn new() -> Namespaces {
        Namespaces {
            members: RwLock::new(HashMap::new()),
        }
    }

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
pub enum CompileMemberId {
    Type(u32),
    Constant(u32),
}

struct TypeUnit {
    definition: TypeExpression,

    /// An id to the resolved type.
    /// This is because resolved types are intended
    /// to have the same id if they are the same type,
    /// and they recursively use the id:s to refer to
    /// subtypes and so on.
    /// The ONLY time the RwLock should be written to
    /// is when changing from None to Some(value),
    /// or changing Some(value) to Some(the_same_value).
    /// Modifying the value in the Some may invalidate
    /// things other parts of the compiler read before,
    /// so it's not a good idea to do so.
    resolved: RwLock<Option<u32>>,
}


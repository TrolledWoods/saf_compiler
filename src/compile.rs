use std::sync::{ Mutex, RwLock, RwLockReadGuard };
use std::collections::HashMap;
use std::mem::drop;
use crate::tiny_string::TinyString;
use crate::vec_top::VecTop;
use crate::parser::Identifier;
use crate::parser::ast::{
    TypeExpression,
    PrimitiveKind,
};

pub mod namespace;
use namespace::{ Namespaces, NamespaceError };

// We want to know the type id of primitives.
const FLOAT_32_ID: usize = 0;
const FLOAT_64_ID: usize = 1;
const INT_32_ID: usize   = 2;
const INT_64_ID: usize   = 3;

/// Wether or not debug information should be
/// printed.
pub const DEBUG: bool = true;

macro_rules! debug {
    ($($tokens:tt)*) => {{ 
        if crate::compile::DEBUG {
            println!($($tokens)*);
        }
    }}
} 

pub struct Compiler {
    named_types: RwLock<Vec<TypeUnit>>,
    resolved_types: RwLock<Vec<ResolvedType>>,

    ready_to_compile: Mutex<Vec<CompileMemberId>>,
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
            ready_to_compile: Mutex::new(Vec::new()),
        }
    }
}

/// Compiles things that are "ready for compilation"
/// until there are no more thing that are ready
pub fn compile_ready(
    compiler: &Compiler
) -> Result<(), CompileError> {
    while let Some(compile_member_id) 
        = compiler.ready_to_compile.lock().unwrap().pop() {

        use CompileMemberId::*;
        match compile_member_id {
            // Cannot compile it anyway
            Poison => (),
            NamedType(id) => {
                try_resolve_type_unit(compiler, id)?;
            }
            Constant(id) => 
                unimplemented!("TODO: Constants"),
        }
    }

    Ok(())
}

pub fn finish(
    compiler: Compiler
) -> Result<String, Vec<CompileError>> {
    Ok(format!("Yay!"))
}

/// Returns the type id of a resolved type.
/// There is one and exactly one type id for 
/// every kind of type(except unique types, 
/// who have a unique identifier). If there is already
/// an index, we return that, if not, we create
/// a new type id and return that.
pub fn type_id(
    resolved_types: &mut Vec<ResolvedType>,
    index_of: &ResolvedType,
) -> usize {
    for (
        i, 
        old_type,
    ) in resolved_types.iter().enumerate() {
        if old_type == index_of {
            return i as usize;
        }
    }

    let id = resolved_types.len() as usize;
    println!("Added type id {}: {:#?}", id, index_of);
    resolved_types.push(index_of.clone());

    id
}

pub fn add_named_type(
    compiler: &Compiler,
    namespace_id: usize,
    name: Identifier,
    definition: TypeExpression,
) -> Result<usize, CompileError> {
    let mut named_types = compiler.named_types.write().unwrap();
    let named_type_id = named_types.len();
    named_types.push(TypeUnit {
        definition,
        resolved: RwLock::new(None),
        resolved_deps: Mutex::new(Vec::new()),
    });
    drop(named_types);

    compiler.namespaces.insert_member(
        namespace_id,
        name,
        CompileMemberId::NamedType(named_type_id as usize),
    )?;

    let named_types = compiler.named_types.read().unwrap();
    match resolve_type(
        compiler,
        &named_types[named_type_id].definition,
    ) {
        Ok(resolved_type_id) => {
            let mut resolved = 
                named_types[named_type_id]
                .resolved.write().unwrap();
            *resolved = Some(resolved_type_id);
        }
        Err(CompileError::DependencyNotReady(id)) => {
            unimplemented!("TODO: add a 'on_done' thing");
        }
        Err(CompileError::Poison) => 
            return Err(CompileError::Poison),
        Err(err) => Err(err)?,
    }

    Ok(named_type_id as usize)
}


type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub enum CompileError {
    Poison,
    DependencyNotReady(Dependency),
    Namespace(NamespaceError),
}

impl From<NamespaceError> for CompileError {
    fn from(other: NamespaceError) -> CompileError {
        CompileError::Namespace(other)
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
    Collection(Vec<(TinyString, usize)>),
    /// Just a pointer to some other type
    Pointer {
        nullable: bool,
        mutable: bool,
        pointing_to: usize,
    },
    VariableArray {
        mutable: bool,
        content_type: usize,
    },
    FixedArray {
        size: usize,
        content_type: usize,
    },
    /// A wrapped type has the same representation
    /// as the type it wraps, but isn't treated
    /// as the same time.
    WrappedType(usize),
    Primitive(PrimitiveKind),
}

pub fn add_dependency(
    compiler: &Compiler,
    to: Dependency,
    dependant: CompileMemberId,
) {
    use Dependency::*;
    match to {
        _ => unimplemented!()
    }
}

/// Will try to resolve a type unit.
/// If it's not possible for some reason,
/// it will either add a dependency and try again
/// once resolved, or if not recoverable, it will
/// produce a CompileError.
pub fn try_resolve_type_unit(
    compiler: &Compiler,
    type_unit_id: usize,
) -> Result<Option<usize>, CompileError> {
    let type_units = compiler.named_types.read().unwrap();

    match resolve_type(
        compiler,
        &type_units[type_unit_id].definition,
    ) {
        Ok(resolved_id) => {
            // This may seem like a logical datarace,
            // but it's fine, because even if it is a
            // "data race", we should be setting the
            // value to the same thing in both cases,
            // so it should be fine anyway.
            let mut resolved = 
                type_units[type_unit_id]
                    .resolved
                    .write()
                    .unwrap();
            *resolved = Some(resolved_id);
            
            debug!(
                "Resolved type unit {:?} to {}", 
                type_unit_id,
                resolved_id
            );

            Ok(Some(resolved_id))
        }
        Err(
            CompileError::DependencyNotReady(
                depending_on_id
            )
        ) => {
            add_dependency(
                compiler,
                depending_on_id,
                CompileMemberId::NamedType(type_unit_id),
            );

            Ok(None)
        }
        Err(err) => Err(err)
    }
}

pub fn resolve_type(
    compiler: &Compiler,
    resolving: &TypeExpression,
) -> Result<usize, CompileError> {
    let mut resolved_types = compiler.resolved_types.write().unwrap();
    let namespaces = &compiler.namespaces;
    let mut reqursion_guard = Vec::new();
    resolve_type_req(&mut *resolved_types, namespaces, resolving, VecTop::at_top(&mut reqursion_guard))
}

/// ``reqursion_guard`` parameter:
/// The values inside the VecTop are types 
/// who will cause an infinite loop
/// if reqursed into, the other ones in the 
/// parent vector are behind pointers and do not cause
/// infinite sizing loops, but should still not
/// be recursed into.
fn resolve_type_req(
    resolved_types: &mut Vec<ResolvedType>,
    namespaces: &Namespaces,
    resolving: &TypeExpression,
    reqursion_guard: VecTop<'_, CompileMemberId>,
) -> Result<usize, CompileError> {
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
                resolved_types,
                namespaces,
                pointing_to,
                reqursion_guard,
            )?;

            let resolved = ResolvedType::Pointer {
                // rustc: please make this easier! :pray:
                mutable: *mutable,
                nullable: *nullable, 
                pointing_to
            };

            Ok(type_id(resolved_types, &resolved))
        }
        _ => unimplemented!(),
    }
}

#[derive(Debug)]
pub enum Dependency {
    SizedTypeUnit(usize),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum CompileMemberId {
    Poison,
    NamedType(usize),
    Constant(usize),
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
    resolved: RwLock<Option<usize>>,
    resolved_deps: Mutex<Vec<CompileMemberId>>,
}


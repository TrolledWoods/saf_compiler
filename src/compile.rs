use std::sync::{ Mutex, RwLock, RwLockReadGuard };
use std::collections::HashMap;
use std::mem::drop;
use crate::tiny_string::TinyString;
use crate::vec_top::{ VecTop, Index };
use crate::parser::{ SourcePos, Identifier };
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

    fn add_ready_to_compile(
        &self, ready: CompileMemberId,
    ) {
        self.ready_to_compile.lock().unwrap().push(ready);
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
                let mut vec = Vec::new();
                let mut resolved_types = 
                    compiler.resolved_types
                    .write().unwrap();
                match resolve_type_unit(
                    compiler, 
                    &mut *resolved_types,
                    id,
                    VecTop::at_top(&mut vec),
                ) {
                    Ok(value) => Ok(value),
                    Err(CompileError::NotDefined {
                        namespace_id,
                        name,
                        dependant_name,
                    }) => {
                        let deps 
                        = compiler.namespaces.add_dependency(
                            namespace_id,
                            name,
                            dependant_name.pos.clone(),
                            CompileMemberId::NamedType(id),
                        );
                        if let Some(value) = deps {
                            compiler.add_ready_to_compile(
                                value
                            );
                        }

                        // Just skip this one for now
                        continue;
                    }
                    Err(err) => Err(err),
                }?;
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

    let dependants = compiler.namespaces.insert_member(
        namespace_id,
        name,
        CompileMemberId::NamedType(named_type_id as usize),
    )?;

    for (_pos, dependant) in dependants {
        compiler.add_ready_to_compile(dependant);
    }

    // Try to resolve the type unit.
    // If any of the dependencies are not defined,
    // it will wait until(if) that dependency
    // is defined.
    let mut resolved_types = 
        compiler.resolved_types.write().unwrap();
    let mut req_guard = Vec::new();
    match resolve_type_unit(
        compiler,
        &mut *resolved_types,
        named_type_id,
        VecTop::at_top(&mut req_guard)
    ) {
        Ok(value) => (),
        Err(CompileError::NotDefined {
            namespace_id,
            name,
            dependant_name,
        }) => {
            let deps = compiler.namespaces.add_dependency(
                namespace_id,
                name,
                dependant_name.pos.clone(),
                CompileMemberId::NamedType(named_type_id),
            );
            if let Some(value) = deps {
                compiler.add_ready_to_compile(
                    value
                );
            }
        }
        Err(err) => return Err(err),
    }

    Ok(named_type_id as usize)
}


type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub enum CompileError {
    Poison,
    DependencyNotReady(Dependency),
    NotDefined {
        namespace_id: usize,
        name: TinyString,
        dependant_name: Identifier,
    },
    InvalidKind {
        at: SourcePos, 
        // TODO: Add where the invalid kind was
        // defined as well.
        expected_kind: String
    },
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
    Circular { 
        type_unit_id: usize,
    },
    Collection(Vec<(TinyString, Box<ResolvedType>)>),
    /// Just a pointer to some other type
    Pointer {
        nullable: bool,
        mutable: bool,
        pointing_to: Box<ResolvedType>,
    },
    VariableArray {
        mutable: bool,
        content_type: Box<ResolvedType>,
    },
    FixedArray {
        size: usize,
        content_type: Box<ResolvedType>,
    },
    UniqueType(usize, Box<ResolvedType>),
    Primitive(PrimitiveKind),
}

fn resolve_type_unit(
    compiler: &Compiler,
    resolved_types: &mut Vec<ResolvedType>,
    type_unit_id: usize,
    mut reqursion_guard: VecTop<'_, CompileMemberId>,
) -> Result<ResolvedType, CompileError> {
    use std::ops::Deref;
    let type_units = compiler.named_types.read().unwrap();

    match reqursion_guard.index_of(
        &CompileMemberId::NamedType(type_unit_id)
    ) {
        Index::NotInside => (),
        Index::Inside(_) => {
            panic!("TODO: Circular type error");
        }
        Index::InsideFull(_) => {
            // This is a circular type, but fortunately
            // the circle is formed around a pointer
            // boundary, so it's fine.
            // We can't get a size though.
            return Ok(ResolvedType::Circular { type_unit_id });
        }
    }

    let mut requresion_guard = reqursion_guard.top();
    reqursion_guard.push(
        CompileMemberId::NamedType(type_unit_id)
    );

    // println!("{:?}", reqursion_guard);

    match resolve_type_req(
        compiler,
        resolved_types,
        &type_units[type_unit_id].definition,
        reqursion_guard.temp_clone(),
    ) {
        Ok(resolved_id) => {
            // TODO: After the tree has been built, then
            // we can start converting the ResolvedType trees
            // into indicees.
            
            // This may seem like a logical datarace,
            // but it's fine, because even if it is a
            // "data race", we should be setting the
            // value to the same thing in both cases,
            // so it should be fine anyway.
            // let mut resolved = 
            //     type_units[type_unit_id]
            //         .resolved
            //         .write()
            //         .unwrap();
            // *resolved = Some(resolved_id);
            
            debug!(
                "Resolved type unit {:?} to {:?}", 
                type_unit_id,
                resolved_id
            );

            reqursion_guard.pop();

            Ok(resolved_id)
        }
        Err(err) => Err(err)
    }
}

pub fn resolve_type(
    compiler: &Compiler,
    resolving: &TypeExpression,
) -> Result<ResolvedType, CompileError> {
    let mut resolved_types = 
        compiler
        .resolved_types
        .write()
        .unwrap();
    let namespaces = &compiler.namespaces;
    let mut reqursion_guard = Vec::new();
    resolve_type_req(
        compiler, 
        &mut *resolved_types, 
        resolving, 
        VecTop::at_top(&mut reqursion_guard)
    )
}

/// ``reqursion_guard`` parameter:
/// The values inside the VecTop are types 
/// who will cause an infinite loop
/// if reqursed into, the other ones in the 
/// parent vector are behind pointers and do not cause
/// infinite sizing loops, but should still not
/// be recursed into.
fn resolve_type_req(
    compiler: &Compiler,
    resolved_types: &mut Vec<ResolvedType>,
    resolving: &TypeExpression,
    mut reqursion_guard: VecTop<'_, CompileMemberId>,
) -> Result<ResolvedType, CompileError> {
    use TypeExpression::*;
    match resolving {
        Primitive {
            kind, ..
        } => Ok(ResolvedType::Primitive(*kind)),
        Pointer {
            mutable, nullable, pointing_to, ..
        } => {
            let pointing_to = resolve_type_req(
                compiler,
                resolved_types,
                pointing_to,
                reqursion_guard.top(),
            )?;

            Ok(ResolvedType::Pointer {
                mutable: *mutable,
                nullable: *nullable, 
                pointing_to: Box::new(pointing_to)
            })
        }
        NamedType {
            pos, 
            namespace_id, 
            name,
        } => {
            // Try to get the type
            let other = compiler.namespaces
                    .find_value(
                *namespace_id, 
                *name,
            ).ok_or(CompileError::NotDefined {
                namespace_id: *namespace_id,
                name: *name,
                dependant_name: Identifier {
                    pos: pos.clone(),
                    name: *name,
                }
            })?;

            match other {
                CompileMemberId::Poison =>
                    return Err(CompileError::Poison),
                CompileMemberId::NamedType(other_id) => {
                    Ok(resolve_type_unit(
                        compiler,
                        resolved_types,
                        other_id,
                        reqursion_guard,
                    )?)
                }
                _ => return Err(
                    CompileError::InvalidKind {
                        at: pos.clone(),
                        expected_kind: format!("type"),
                    }
                )
            }
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


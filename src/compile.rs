use std::sync::{ Mutex, RwLock, RwLockReadGuard };
use std::sync::atomic::{ Ordering, AtomicUsize };
use std::collections::HashMap;
use std::mem::drop;
use std::fmt;
use crate::tiny_string::TinyString;
use crate::vec_top::{ VecTop, Index };
use crate::parser::{ SourcePos, Identifier };
use crate::parser::ast::{
    TypeExpression,
    PrimitiveKind,
};

pub mod namespace;
use namespace::{ Namespaces, NamespaceError };

pub mod type_def;
use type_def::{ TypeDef };

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
    type_definitions: RwLock<Vec<TypeDef>>,
    unique_type_ctr: AtomicUsize,

    ready_to_compile: Mutex<Vec<CompileMemberId>>,
    namespaces: Namespaces,
}

impl Compiler {
    pub fn new() -> Compiler {
        let mut types = {
            use TypeDef::*;
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
            type_definitions: RwLock::new(types),
            namespaces: Namespaces::new(),
            ready_to_compile: Mutex::new(Vec::new()),
            unique_type_ctr: AtomicUsize::new(0),
        }
    }

    pub fn add_unique_type(
        &self,
    ) -> usize {
        self.unique_type_ctr.fetch_add(1, Ordering::SeqCst)
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
                match calc_type_def(
                    compiler, 
                    id,
                    VecTop::at_top(&mut vec),
                ) {
                    Ok(value) => {
                        debug!(
                            "Resolved type unit {:?} to {}", 
                            id,
                            value,
                        );
                        Ok(value)
                    }
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

/// Returns the type id of a type definition.
/// There is one and exactly one type id for 
/// every kind of type(except unique types, 
/// who have a unique identifier). If there is already
/// an index, we return that, if not, we create
/// a new type id and return that.
pub fn calc_type_id(
    type_definitions: &mut Vec<TypeDef>,
    index_of: &TypeDef,
) -> usize {
    for (
        i, 
        old_type,
    ) in type_definitions.iter().enumerate() {
        if old_type == index_of {
            return i as usize;
        }
    }

    let id = type_definitions.len() as usize;
    println!("Added type id {}: {:#?}", id, index_of);
    type_definitions.push(index_of.clone());

    id
}

pub fn add_named_type(
    compiler: &Compiler,
    namespace_id: usize,
    name: Identifier,
    definition: TypeExpression,
    is_unique: bool,
) -> Result<usize, CompileError> {
    let mut named_types = compiler.named_types.write().unwrap();
    let named_type_id = named_types.len();
    named_types.push(TypeUnit {
        definition,
        unique_type_id: if is_unique {
            Some(compiler.add_unique_type())
        } else {
            None
        },
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

    // Try to get the type definition
    // If any of the dependencies are not defined,
    // it will wait until(if) that dependency
    // is defined.
    let mut req_guard = Vec::new();
    match calc_type_def(
        compiler,
        named_type_id,
        VecTop::at_top(&mut req_guard)
    ) {
        Ok(value) => {
            debug!(
                "Resolved type unit {:?} to {}", 
                named_type_id,
                value,
            );
        }
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

fn calc_type_def(
    compiler: &Compiler,
    type_unit_id: usize,
    mut reqursion_guard: VecTop<'_, CompileMemberId>,
) -> Result<TypeDef, CompileError> {
    use std::ops::Deref;
    let type_units = compiler.named_types.read().unwrap();

    match reqursion_guard.index_of(
        &CompileMemberId::NamedType(type_unit_id)
    ) {
        Index::NotInside => (),
        Index::Inside(_) => {
            panic!("TODO: Circular type error");
        }
        Index::InsideFull(index) => {
            // This is a circular type, but fortunately
            // the circle is formed around a pointer
            // boundary, so it's fine.
            // We can't get a size though.
            let size = reqursion_guard.full_slice().len();
            return Ok(TypeDef::Circular {
                n_steps_up: size - index,
            });
        }
    }

    let mut requresion_guard = reqursion_guard.top();
    reqursion_guard.push(
        CompileMemberId::NamedType(type_unit_id)
    );

    let unique_type_id = type_units[type_unit_id].unique_type_id;
    match calc_type_def_req(
        compiler,
        &type_units[type_unit_id].definition,
        reqursion_guard.temp_clone(),
    ) {
        Ok(resolved) => {
            reqursion_guard.pop();

            if let Some(unique_id) = unique_type_id {
                Ok(TypeDef::UniqueType(
                    unique_id,
                    Box::new(resolved),
                ))
            } else { 
                Ok(resolved)
            }
        }
        Err(err) => Err(err)
    }
}

/// ``reqursion_guard`` parameter:
/// The values inside the VecTop are types 
/// who will cause an infinite loop
/// if reqursed into, the other ones in the 
/// parent vector are behind pointers and do not cause
/// infinite sizing loops, but should still not
/// be recursed into.
fn calc_type_def_req(
    compiler: &Compiler,
    resolving: &TypeExpression,
    mut reqursion_guard: VecTop<'_, CompileMemberId>,
) -> Result<TypeDef, CompileError> {
    use TypeExpression::*;
    match resolving {
        Primitive {
            kind, ..
        } => Ok(TypeDef::Primitive(*kind)),
        Pointer {
            mutable, nullable, pointing_to, ..
        } => {
            let pointing_to = calc_type_def_req(
                compiler,
                pointing_to,
                reqursion_guard.top(),
            )?;

            Ok(TypeDef::Pointer {
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
                    Ok(calc_type_def(
                        compiler,
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
        UniqueType(internal) => {
            let unique_id = compiler.add_unique_type();
            let internal = calc_type_def_req(
                compiler,
                internal,
                reqursion_guard.top(),
            )?;

            Ok(TypeDef::UniqueType(
                unique_id,
                Box::new(internal)
            ))
        }
        NamedCollection(members) => {
            let mut resolved_members = 
                Vec::with_capacity(members.len());
            for (name, type_, _default) in members {
                let member = calc_type_def_req(
                    compiler,
                    type_,
                    reqursion_guard.temp_clone(),
                )?;
                resolved_members.push((*name, member));
            }

            Ok(TypeDef::Collection(resolved_members))
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
    unique_type_id: Option<usize>,

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


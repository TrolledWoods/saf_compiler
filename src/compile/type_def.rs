use crate::tiny_string::TinyString;
use crate::parser::ast::PrimitiveKind;
use std::fmt;

pub const POINTER_TYPE: PrimitiveKind = PrimitiveKind::Int64;

// /// A TypeDef based on type id:s.
// /// The id:s themselves are always based
// /// on TypeDefs, not IdTypeDefs, because
// /// otherwise circular types won't work.
// pub enum IdTypeDef {
//     Pointer {
//         nullable: bool,
//         mutable: bool,
//         pointing_to: usize,
//     }
//     VariableArray {
//         mutable: bool,
//         content_type: Box<TypeDef>,
//     }
//     FixedArray {
// 
//     }
// }

/// A TypeDef is a type where we know that no
/// infinite "sizing loops" occur, and where we know
/// that all the types that the TypeDef depends
/// on are also TypeDef:s.
/// Also, all constant expressions are calculated
/// in resolved types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeDef {
    Circular { 
        n_steps_up: usize,
    },
    Collection(Vec<(TinyString, TypeDef)>),
    /// Just a pointer to some other type
    Pointer {
        nullable: bool,
        mutable: bool,
        pointing_to: Box<TypeDef>,
    },
    VariableArray {
        mutable: bool,
        content_type: Box<TypeDef>,
    },
    FixedArray {
        size: usize,
        content_type: Box<TypeDef>,
    },
    UniqueType(usize, Box<TypeDef>),
    Primitive(PrimitiveKind),
}

impl TypeDef {
    /// Returns the representation in memory
    /// that the TypeDef will have. This operation
    /// is probably fairly expensive, so please
    /// use with care.
    pub fn calc_repr(&self, data: &mut Vec<PrimitiveKind>) {
        use TypeDef::*;
        match self {
            Circular { .. } => unreachable!("A circular should always be behind a pointer"),
            Collection(members) => {
                for (_, member) in members {
                    member.calc_repr(data);
                }
            }
            Pointer { .. } => {
                data.push(POINTER_TYPE);
            }
            VariableArray { mutable, .. } => {
                if *mutable {
                    // Capacity
                    data.push(PrimitiveKind::Int32);
                }
                // Length
                data.push(PrimitiveKind::Int32);
                // Data pointer
                data.push(POINTER_TYPE);
            }
            FixedArray { size, content_type } => {
                // TODO: This could probably become
                // a lot more efficient, but it should
                // be fine for now. Especially inefficient
                // for large size values.
                // assert!(*size <= 50, 
                //    "Cannot do too big fixed arrays for now");
                for _ in 0..*size {
                    content_type.calc_repr(data);
                }
            }
            UniqueType(_, internal) => 
                internal.calc_repr(data),
            Primitive(kind) => data.push(*kind),
        }
    }
}

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TypeDef::*;
        match self {
            Circular { n_steps_up } => 
                write!(f, "@{}", n_steps_up)?,
            Collection(members) => {
                write!(f, "{} ", '{')?;
                for (i, (name, member)) in members.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, member)?;
                }
                write!(f, " {}", '}')?;
            }
            Pointer {
                nullable,
                mutable,
                pointing_to,
            } => { 
                write!(f, "*")?;
                if *nullable { write!(f, "null ")?; }
                if *mutable  { write!(f, "mut ")?; }
                write!(f, "{}", pointing_to)?;
            }
            VariableArray {
                mutable,
                content_type,
            } => {
                if *mutable {
                    write!(f, "[?] ")?;
                } else {
                    write!(f, "[-] ")?;
                }

                write!(f, "{}", content_type)?;
            }
            FixedArray {
                size,
                content_type,
            } => {
                write!(f, "[{}] {}", size, content_type)?;
            }
            UniqueType(id, internal) => {
                write!(f, "${} {}", id, internal)?;
            }
            Primitive(primitive) => {
                use PrimitiveKind::*;
                match primitive {
                    Float32 => write!(f, "f32")?,
                    Float64 => write!(f, "f64")?,
                    Int32 => write!(f, "i32")?,
                    Int64 => write!(f, "i64")?,
                }
            }
        }

        Ok(())
    }
}

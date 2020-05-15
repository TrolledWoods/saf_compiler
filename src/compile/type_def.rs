use crate::tiny_string::TinyString;
use crate::parser::ast::PrimitiveKind;
use std::fmt;

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

use std::convert::TryInto;

use super::{ 
    Compiler, 
    CompileError,
    FLOAT_32_ID,
    FLOAT_64_ID,
    INT_32_ID,
    INT_64_ID,
};
use crate::parser::ast::{ Expression, PrimitiveKind };
use crate::parser::Literal as LiteralValue;
use crate::parser::SourcePos;
use super::type_def::TypeDef;

#[derive(Clone, Debug, PartialEq)]
pub struct Value {
    pub pos: SourcePos,
    pub kind: ValueKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValueKind {
    Float32(f32),
    Float64(f64),
    Float(f64),

    Int32(u32),
    Int64(u64),
    Int(u64),
}

impl Value {
    pub fn expect_u64(&self) -> Result<u64, CompileError> {
        match &self.kind {
            ValueKind::Int64(value) => Ok(*value),
            ValueKind::Int(value) => Ok(*value),
            _ => Err(CompileError::InvalidValueType {
                at: self.pos.clone(),
                expected_type: TypeDef::Primitive(PrimitiveKind::Int64),
            })
        }
    }

    pub fn approximate_type_def(&self) -> TypeDef {
        use ValueKind::*;
        match &self.kind {
            Int(_)   => TypeDef::Primitive(PrimitiveKind::Int64),
            Int64(_) => TypeDef::Primitive(PrimitiveKind::Int64),
            Int32(_) => TypeDef::Primitive(PrimitiveKind::Int32),

            Float(_) => TypeDef::Primitive(PrimitiveKind::Float64),
            Float64(_) => TypeDef::Primitive(PrimitiveKind::Float64),
            Float32(_) => TypeDef::Primitive(PrimitiveKind::Float32),
        }
    }
}

/// If one of the types is an unspecified integer or an
/// unspecified float, try to make it specified
fn converge_types(
    a: &mut Value,
    b: &mut Value,
) -> Result<(), CompileError> {
    use ValueKind::*;
    match (&a.kind, &b.kind) {
        (Int64(_), Int(val)) => 
            b.kind = Int64(*val as u64),
        (Int(val), Int64(_)) => 
            a.kind = Int64(*val as u64),
        (Int32(_), Int(val)) =>
            b.kind = Int32(
                (*val).try_into()
                .map_err(|_| CompileError::ValueDoesntFit {
                    pos: b.pos.clone(),
                    value: a.clone(),
                    cramming_into: 32,
                })?
            ),
        (Int(val), Int32(_)) =>
            a.kind = Int32(
                (*val).try_into()
                .map_err(|_| CompileError::ValueDoesntFit {
                    pos: a.pos.clone(),
                    value: b.clone(),
                    cramming_into: 32,
                })?
            ),
        (Float64(_), Float(val)) => 
            b.kind = Float64(*val),
        (Float(val), Float64(_)) => 
            a.kind = Float64(*val),
        (Float32(_), Float(val)) =>
            b.kind = Float32(*val as f32),
        (Float(val), Float32(_)) =>
            a.kind = Float32(*val as f32),
        _ => ()
    }

    Ok(())
}

fn try_operation(
    compiler: &Compiler,
    op: &str,
    pos: SourcePos, 
    mut a: Value,
    mut b: Value,
) -> Result<Value, CompileError> {
    use ValueKind::*;

    // This will make me have to implement
    // fewer operations by hand
    converge_types(&mut a, &mut b)?;

    let new_kind = match (op, a.kind, b.kind) {
        ("+", Int(a), Int(b)) => Int(a + b),
        ("+u", Int(a), Int(b)) => Int(a + b),
        ("+i", Int(a), Int(b)) => Int((a as i64 + b as i64) as u64),

        ("-", Int(a), Int(b)) => Int(a - b),
        ("-u", Int(a), Int(b)) => Int(a - b),
        ("-i", Int(a), Int(b)) => Int((a as i64 - b as i64) as u64),

        // TODO: Since operator overoading will be a thing, see
        // if there are values that are overloaded like that.
        // For now there are no generics though, so that is not 
        // really possible
        (op, a, b) => unimplemented!("{:?} {} {:?} is not implemented", a, op, b),
    };

    Ok(Value {
        kind: new_kind,
        pos,
    })
}

/// Evaluates a given expression.
pub fn interpret(
    compiler: &Compiler,
    expression: &Expression,
) -> Result<Option<Value>, CompileError> {
    use Expression::*;
    match expression {
        Operation {
            pos,
            operator,
            arguments,
        } => {
            if arguments.len() == 1 {
                unimplemented!("TODO: Unary operators");
            } else if arguments.len() == 2 {
                let a = interpret(compiler, &arguments[0])?
                    .ok_or_else(|| CompileError::ExpectedValue {
                            at: arguments[0].pos() 
                    })?;
                let b = interpret(compiler, &arguments[1])?
                    .ok_or_else(|| CompileError::ExpectedValue {
                            at: arguments[1].pos()
                    })?;

                Ok(Some(try_operation(
                    compiler,
                    operator,
                    pos.clone(),
                    a,
                    b,
                )?))
            } else {
                unreachable!("Operators don't have 3 arguments, do they?")
            }
        }
        Expression::Literal {
            pos,
            data
        } => match data {
            LiteralValue::Int(val) => Ok(Some(Value {
                pos: pos.clone(),
                kind: ValueKind::Int(*val as u64),
            })),
            LiteralValue::Float(val) => Ok(Some(Value {
                pos: pos.clone(),
                kind: ValueKind::Float(*val as f64),
            })),
            LiteralValue::String(_) => unimplemented!("String literals are not supported yet"),
        },
        _ => unimplemented!(),
    }
}

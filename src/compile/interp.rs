use super::{ 
    Compiler, 
    CompileError,
    FLOAT_32_ID,
    FLOAT_64_ID,
    INT_32_ID,
    INT_64_ID,
};
use crate::parser::ast::Expression;
use crate::parser::Literal as LiteralValue;
use super::type_def::TypeDef;

pub enum PrimitiveValue {
    Float32(f32),
    Float64(f64),
    Int32(i32),
    Int64(i64),
}

pub struct InterpreterValue {
    pub type_id: usize,
    pub definition: Vec<PrimitiveValue>,
}

/// Evaluates a given expression.
pub fn interpret(
    compiler: &Compiler,
    expression: &Expression,
) -> Result<Option<InterpreterValue>, CompileError> {
    use Expression::*;
    match expression {
        Expression::Literal {
            pos,
            data
        }=> match data {
            LiteralValue::Int(val) => Ok(Some(InterpreterValue {
                type_id: INT_64_ID,
                definition: vec![PrimitiveValue::Int64(*val as i64)]
            })),
            LiteralValue::Float(val) => Ok(Some(InterpreterValue {
                type_id: FLOAT_64_ID,
                definition: vec![PrimitiveValue::Float64(*val)]
            })),
            LiteralValue::String(_) => unimplemented!("String literals are not supported yet"),
        },
        _ => unimplemented!(),
    }
}

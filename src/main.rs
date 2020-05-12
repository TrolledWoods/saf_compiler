// TODO: Remove all of these
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused)]

#[macro_use]
extern crate lazy_static;

mod tiny_string;
mod parser;
mod compile;

use std::sync::atomic::{ AtomicU32, Ordering };

fn main() {
    let counter = IdCounter::new();
    let compiler = compile::Compiler::new();
    parser::parse_file(
        "src.saf", 
        &counter,
        |compilation_unit| {
            use parser::CompilationUnit::*;
            match compilation_unit {
                TypeDefinition {
                    definition,
                    name,
                    ..
                } => {
                    let unit = compile::resolve_type(
                        &compiler,
                        &definition,
                    );
                    println!(
                        "type {} = {:#?}", 
                        name.name, 
                        unit
                        );
                }
            }
        },
    ).unwrap();
}

pub struct IdCounter(AtomicU32);

impl IdCounter {
    pub fn new() -> IdCounter {
        IdCounter(AtomicU32::new(0))
    }

    pub fn create_id(&self) -> u32 {
        self.0.fetch_add(1, Ordering::SeqCst)
    }
}

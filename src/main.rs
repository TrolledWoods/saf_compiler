// TODO: Remove all of these
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused)]

#[macro_use]
extern crate lazy_static;

mod tiny_string;
mod vec_top;
mod parser;
mod compile;

use std::sync::atomic::{ AtomicUsize, Ordering };

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
                    namespace_id,
                    name,
                    definition,
                    ..
                } => {
                    compile::add_named_type(
                        &compiler,
                        namespace_id,
                        name,
                        definition,
                    ).unwrap();
                }
            }
        },
    ).unwrap();

    compile::compile_ready(
        &compiler,
    ).unwrap();

    match compile::finish(compiler) {
        Ok(result) => println!("Finished compilation!"),
        Err(errors) => {
            for error in errors {
                println!("Error: {:#?}", error);
            }
        }
    }
}

pub struct IdCounter(AtomicUsize);

impl IdCounter {
    pub fn new() -> IdCounter {
        IdCounter(AtomicUsize::new(0))
    }

    pub fn create_id(&self) -> usize {
        self.0.fetch_add(1, Ordering::SeqCst)
    }
}

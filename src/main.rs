// TODO: Remove all of these
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused)]

#[macro_use]
extern crate lazy_static;

mod tiny_string;
mod parser;

use std::sync::atomic::{ AtomicU32, Ordering };

pub type CompileResult<T> = Result<T, CompileError>;

fn main() {
    let counter = IdCounter::new();
    parser::parse_file(
        "src.saf", 
        &counter,
        |compilation_unit| println!("{:#?}", compilation_unit),
    ).unwrap();
}

pub enum CompileError {
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

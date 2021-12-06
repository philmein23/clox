mod chunk;
use core::fmt;
use std::{env::args, fs};

use chunk::{Chunk, Constant, LineNumber, OpCode};

mod compiler;
use compiler::Compiler;

mod scanner;
mod token;
use scanner::Scanner;

mod debug;
mod value;
mod vm;
use debug::disassemble_chunk;
use vm::VM;

fn main() {
    for arg in args().skip(1) {
        let result = run_file(arg).or_else(|e| Err(e));
    }
}

fn run_file(path: String) -> Result<(), Error> {
    let result = fs::read_to_string(path).ok();

    match result {
        Some(input) => {
            let mut compiler = Compiler::new();

            compiler.compile(&input);

            Ok(())
        }
        None => Err(Error::new("There was a problem reading path as string")),
    }
}
#[derive(Debug)]
struct Error {
    details: String,
}

impl Error {
    fn new(msg: &str) -> Error {
        Error {
            details: msg.to_string(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

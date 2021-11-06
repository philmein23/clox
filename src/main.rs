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
    let chunk = Chunk {
        code: vec![
            (OpCode::Constant(0), LineNumber::new(1)),
            (OpCode::Constant(1), LineNumber::new(2)),
            (OpCode::Add, LineNumber::new(3)),
            (OpCode::Return, LineNumber::new(20)),
        ],
        constants: vec![Constant::Number(10 as f64), Constant::Number(3 as f64)],
    };

    let mut vm = VM::new(chunk);
    vm.interpret();
}

fn run_file(path: String) -> Result<(), Error> {
    let result = fs::read_to_string(path).ok();

    match result {
        Some(input) => {
            let scanner = Scanner::new(&input);
            if let Ok(tokens) = scanner.scan_tokens() {
                let mut iter = tokens.into_iter().peekable();
                let compiler = Compiler::new(&mut iter);

                compiler.compile();
            }

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

use crate::chunk::{Chunk, Constant, LineNumber, OpCode};
use crate::compiler::Compiler;
use crate::debug::disassemble_chunk;
use crate::value::Value;

pub enum InterpretResult {
    Interpret_Ok,
}

pub enum InterpretError {
    InterpretCompileError(String),
    InterpretRuntimeError,
}

pub struct VM {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: vec![],
        }
    }
    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
        let mut compiler = Compiler::new();
        match compiler.compile(source) {
            Ok(_) => {
                self.chunk = compiler.chunk;
                self.run()
            }
            Err(err) => {
                return Err(InterpretError::InterpretCompileError(err));
            }
        }
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            disassemble_chunk(&self.chunk, "test_chunk");
            let op = self.next_op_and_advance();
            match op {
                Some((OpCode::Add, ln))
                | Some((OpCode::Subtract, ln))
                | Some((OpCode::Multiply, ln))
                | Some((OpCode::Divide, ln)) => {
                    if let Some((opcode, _)) = op {
                        self.handle_binary_op(&opcode);
                    }
                }
                Some((OpCode::Negate, ln)) => {
                    if let Some(Value::Number(n)) = self.pop() {
                        self.stack.push(Value::Number(-n));
                    }
                }
                Some((OpCode::Not, ln)) => {
                    let val = match self.pop() {
                        Some(Value::Nil) => Value::Bool(false),
                        Some(Value::Bool(true)) => Value::Bool(false),
                        Some(Value::Bool(false)) => Value::Bool(true),
                        _  => {
                            return Err(InterpretError::InterpretRuntimeError)
                        }
                    };

                    self.stack.push(val);
                }
                Some((OpCode::Return, ln)) => {
                    println!("RETURN VALUE: {:?}", self.pop());
                }
                Some((OpCode::True, ln)) => {
                    self.stack.push(Value::Bool(true));
                }
                Some((OpCode::False, ln)) => {
                    self.stack.push(Value::Bool(false));
                }
                Some((OpCode::Nil, ln)) => {
                    self.stack.push(Value::Nil);
                }
                Some((OpCode::Constant(index), ln)) => {
                    let val = self.chunk.constants.get(index);
                    match val {
                        Some(&Constant::Number(num)) => {
                            println!("Value {:?}", num);
                            let runtime_val = Value::Number(num);
                            self.stack.push(runtime_val);
                        }
                        _ => return Err(InterpretError::InterpretRuntimeError),
                    }
                }
                _ => {
                    break;
                }
            }
        }
        Ok(())
    }

    fn next_op_and_advance(&mut self) -> Option<(OpCode, LineNumber)> {
        let op = self.chunk.code.get(self.ip);
        self.ip += 1;
        op.cloned()
    }

    fn handle_binary_op(&mut self, op: &OpCode) {
        let right = self.pop();
        let left = self.pop();

        let lr_vals = match (left, right) {
            (Some(Value::Number(lnum)), Some(Value::Number(rnum))) => (lnum, rnum),
            _ => {
                panic!(
                    "There is no known runtime value representation with {:?}",
                    (left, right)
                )
            }
        };

        let (lv, rv) = lr_vals;
        match op {
            OpCode::Add => {
                self.stack.push(Value::Number(lv + rv));
            }
            OpCode::Subtract => {
                self.stack.push(Value::Number(lv - rv));
            }
            OpCode::Multiply => {
                self.stack.push(Value::Number(lv * rv));
            }
            OpCode::Divide => {
                self.stack.push(Value::Number(lv / rv));
            }
            _ => {
                panic!(
                    "There is no known instruction with the associated op code {:?}",
                    op
                );
            }
        }
    }
    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }
}

#[test]
fn test_arithmetic() {
    let input = String::from(
        r#"
           12 + 4;
        "#,
    );

    interpret(input);
}

#[test]
fn test_arithmetic_two() {
    let input = String::from(
        r#"
           12 + 10 * 4 - 1;
        "#,
    );

    interpret(input);
}

#[test]
fn test_not() {
    let input = String::from(
        r#"
            !true;
        "#
    );

    interpret(input);
}


#[test]
fn test_not_two() {
    let input = String::from(
        r#"
            !false;
        "#
    );

    interpret(input);
}

fn interpret(source: String) {
    let mut vm = VM::new();
    vm.interpret(source.as_str());
}

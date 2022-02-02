use std::collections::HashMap;

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
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: vec![],
            globals: Default::default(),
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
                    if let Value::Number(n) = self.pop() {
                        self.stack.push(Value::Number(-n));
                    }
                }
                Some((OpCode::Not, ln)) => {
                    let val = match self.pop() {
                        Value::Nil => Value::Bool(false),
                        Value::Bool(true) => Value::Bool(false),
                        Value::Bool(false) => Value::Bool(true),
                        _ => return Err(InterpretError::InterpretRuntimeError),
                    };

                    self.stack.push(val);
                }
                Some((OpCode::Print, ln)) => {
                    match self.pop() {
                        Value::Number(n) => println!("{:?}", n),
                        Value::String(s) => println!("{:?}", s),
                        _ => panic!("There is no value"),
                    };
                }
                Some((OpCode::Pop, ln)) => {
                    let _ = self.pop();
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
                Some((OpCode::Equal, ln)) => {
                    self.handle_equal();
                }
                Some((OpCode::Greater, ln)) => {
                    self.handle_binary_op(&OpCode::Greater);
                }
                Some((OpCode::Less, ln)) => {
                    self.handle_binary_op(&OpCode::Less);
                }
                Some((OpCode::Constant(index), ln)) => {
                    let val = self.chunk.constants.get(index);
                    match val {
                        Some(Constant::Number(num)) => {
                            let runtime_val = Value::Number(num.clone());
                            self.stack.push(runtime_val);
                        }
                        Some(Constant::String(s)) => {
                            let clone = s.clone();
                            let runtime_val = Value::String(clone);
                            self.stack.push(runtime_val);
                        }
                        _ => return Err(InterpretError::InterpretRuntimeError),
                    }
                }
                Some((OpCode::DefineGlobal, ln)) => {
                    if let Value::String(name) = self.pop() {
                        let value = self.pop();
                        self.globals.insert(name, value);
                    }
                }
                Some((OpCode::GetGlobal, ln)) => {
                    if let Value::String(name) = self.pop() {
                        match self.globals.get(&name) {
                            Some(value) => {
                                self.stack.push(value.to_owned());
                            }
                            None => return Err(InterpretError::InterpretRuntimeError),
                        }
                    }
                }
                Some((OpCode::SetGlobal, ln)) => {
                    if let Value::String(name) = self.pop() {
                        let value = self.peek();
                        match self.globals.contains_key(&name) {
                            true => {
                                self.globals.insert(name, value.to_owned());
                            }
                            false => {
                                return Err(InterpretError::InterpretRuntimeError);
                            }
                        }
                    }
                }
                Some((OpCode::SetLocal(idx), ln)) => {
                    let curr = self.peek();
                    match self.stack.get_mut(idx) {
                        Some(val) => {
                            *val = curr;
                        }
                        _ => {
                            return Err(InterpretError::InterpretRuntimeError);
                        }
                    }
                }
                Some((OpCode::GetLocal(idx), ln)) => match self.stack.get(idx) {
                    Some(val) => {
                        let val = val.clone();
                        self.stack.push(val);
                    }
                    _ => {
                        return Err(InterpretError::InterpretRuntimeError);
                    }
                },
                Some((OpCode::JumpIfFalse(jump), ln)) => {
                    if let Value::Bool(false) = self.peek() {
                        self.ip += jump;
                    }
                }
                Some((OpCode::Jump(jump), ln)) => {
                    self.ip += jump;
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

    fn handle_equal(&mut self) {
        let right = self.pop();
        let left = self.pop();

        let result = match (&left, &right) {
            (Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
            (Value::Nil, Value::Nil) => Value::Bool(true),
            (Value::Number(a), Value::Number(b)) => Value::Bool(a == b),
            (Value::String(a), Value::String(b)) => Value::Bool(a.eq(b)),
            _ => {
                panic!(
                    "There is no runtime value representation with {:?}",
                    (left, right)
                )
            }
        };

        self.stack.push(result)
    }

    fn handle_binary_op(&mut self, op: &OpCode) {
        let right = self.pop();
        let left = self.pop();

        if let (Value::String(l), Value::String(r)) = (&left, &right) {
            match &op {
                OpCode::Add => {
                    let lclone = l.clone();
                    let rclone = r.clone();
                    let concat = format!("{}{}", lclone, rclone);
                    self.stack.push(Value::String(concat));
                }
                _ => {
                    panic!("Op {:?} does not work with strings", op)
                }
            }

            return;
        }

        let lr_vals = match (&left, &right) {
            (Value::Number(lnum), Value::Number(rnum)) => (lnum, rnum),
            _ => {
                panic!(
                    "There is no known runtime value representation with {:?}",
                    (left, right)
                )
            }
        };

        let (lv, rv) = lr_vals;
        match op {
            OpCode::Greater => {
                self.stack.push(Value::Bool(lv > rv));
            }
            OpCode::Less => {
                self.stack.push(Value::Bool(lv < rv));
            }
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

    fn peek(&mut self) -> Value {
        let mut stack = self.stack.iter().peekable();
        if let Some(&val) = stack.peek() {
            return val.to_owned();
        } else {
            panic!("The stack is empty");
        }
    }

    fn pop(&mut self) -> Value {
        if let Some(val) = self.stack.pop() {
            return val;
        } else {
            panic!("Nothing left to pop");
        }
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
        "#,
    );

    interpret(input);
}

#[test]
fn test_not_two() {
    let input = String::from(
        r#"
            !false;
        "#,
    );

    interpret(input);
}

#[test]
fn test_equal() {
    let input = String::from(
        r#"
            3 == 3;
        "#,
    );

    interpret(input);
}

#[test]
fn test_equal_two() {
    let input = String::from(
        r#"
            3 == 4;
        "#,
    );

    interpret(input);
}

#[test]
fn test_greater_than_or_equal() {
    let input = String::from(
        r#"
            3 >= 2;
        "#,
    );

    interpret(input);
}

#[test]
fn test_string() {
    let input = String::from(
        r#"
            "Phil Yo";
        "#,
    );

    interpret(input);
}

#[test]
fn test_string_concatenation() {
    let input = String::from(
        r#"
          "Phil" + "Nguyen";
        "#,
    );

    interpret(input);
}

#[test]
fn test_string_equality() {
    let input = String::from(
        r#"
          "Phil" == "Phil";
        "#,
    );

    interpret(input);
}

#[test]
fn test_string_inequality() {
    let input = String::from(
        r#"
          "Phil" == "Bob";
        "#,
    );

    interpret(input);
}

#[test]
fn test_print_statement() {
    let input = String::from(
        r#"
            print 1 + 2 + 5;
        "#,
    );

    interpret(input);
}

#[test]
fn test_read_variable() {
    let input = String::from(
        r#"
            var age = 1 + 2 + 10;
            print age;
        "#,
    );

    interpret(input);
}

#[test]
fn test_set_variable() {
    let input = String::from(
        r#"
            var age = 1 + 2 + 10;
            age = 35;
            print age;
        "#,
    );

    interpret(input);
}

#[test]
fn test_local_variable() {
    let input = String::from(
        r#"
            {
                var age = 2 + 2 + 4;
                var phil = "cool";
                {
                    var age = 4 + 5;
                    age = 27;
                    print age;
                    print phil;
                }
            }
        "#,
    );

    interpret(input);
}

#[test]
fn test_if_statement() {
    let input = String::from(
        r#"
            if (3 > 2) {
                var val = 2;
                print val;
            }

            if (1 > 3) {
            var val = 10;
            print val;
            }

            var age = 33;
            print age;
        "#,
    );

    interpret(input);
}

#[test]
fn test_if_else_statement() {
    let input = String::from(
        r#"
            if (1 > 3) {
                var age = 30;
                print age;
            } else {
                var age = 45;
                print age;
            }
            var phil = "cool";
            print phil;
        "#
    );
    interpret(input);
}

fn interpret(source: String) {
    let mut vm = VM::new();
    vm.interpret(source.as_str());
}

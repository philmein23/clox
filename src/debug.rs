use crate::chunk::{Chunk, OpCode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==\n", name);

    println!("==== Constants ====");
    for (idx, constant) in chunk.constants.iter().enumerate() {
        println!("{:<4} {:?}", idx, constant);
    }

    println!("===== Code =====");
    for (idx, (opcode, ln)) in chunk.code.iter().enumerate() {
        let formatted_op = match opcode {
            OpCode::Return => "OP_RETURN".to_string(),
            OpCode::Constant(index) => {
                format!("OP_CONSTANT {:?} (idx={})", chunk.constants[*index], *index)
            }
            OpCode::Negate => "OP_NEGATE".to_string(),
            OpCode::Add => "OP_ADD".to_string(),
            OpCode::Subtract => "OP_SUBTRACT".to_string(),
            OpCode::Multiply => "OP_MULTIPLY".to_string(),
            OpCode::Divide => "OP_DIVIDE".to_string(),
            OpCode::Nil => "OP_NIL".to_string(),
            OpCode::True => "OP_TRUE".to_string(),
            OpCode::False => "OP_FALSE".to_string(),
            OpCode::Not => "OP_NOT".to_string(),
            OpCode::Greater => "OP_GREATER".to_string(),
            OpCode::Less => "OP_LESS".to_string(),
            OpCode::Equal => "OP_EQUAL".to_string(),
            OpCode::Print => "OP_PRINT".to_string(),
            OpCode::Pop => "OP_POP".to_string(),
            OpCode::DefineGlobal => "OP_DEFINE_GLOBAL".to_string(),
            OpCode::GetGlobal => "OP_GET_GLOBAL".to_string(),
            OpCode::SetGlobal => "OP_SET_GLOBAL".to_string(),
            OpCode::GetLocal(_) => "OP_GET_LOCAL".to_string(),
            OpCode::SetLocal(_) => "OP_SET_LOCAL".to_string(),
            OpCode::JumpIfFalse(_) => "OP_JUMP_IF_FALSE".to_string(),
            OpCode::Jump(_) => "OP_JUMP".to_string(),
            OpCode::Loop(_) => "OP_LOOP".to_string(),
        };

        let instruction = format!(
            "{0: <04} {1: <50} {2: <50}",
            idx,
            formatted_op,
            format!("line {}", ln.value)
        );
        println!("{:?}", instruction);
    }
}

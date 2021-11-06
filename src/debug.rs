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

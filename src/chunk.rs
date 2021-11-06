#[derive(Debug, Clone)]
pub enum OpCode {
    Return,
    Constant(usize),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}
#[derive(Debug, Clone)]
pub enum Constant {
    Number(f64),
}

#[derive(Debug, Copy, Clone)]
pub struct LineNumber {
    pub value: usize,
}

impl LineNumber {
    pub fn new(value: usize) -> Self {
        LineNumber { value }
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<(OpCode, LineNumber)>,
    pub constants: Vec<Constant>,
}

impl Chunk {
    pub fn add_constant(&mut self, val: Constant) -> usize {
        self.constants.push(val);
        let index = self.constants.len() - 1;
        index
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Number(f64),
    Nil,
    Bool(bool)
}

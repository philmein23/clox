#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Nil,
    Bool(bool),
    String(String),
}

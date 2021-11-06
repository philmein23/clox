use crate::chunk::{Chunk, Constant, LineNumber};
use crate::scanner::Scanner;
use crate::token::Token;
use std::default;
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Compiler<'a> {
    tokens: &'a mut Peekable<IntoIter<Token>>,
}

impl<'a> Compiler<'a> {
    pub fn new(tokens: &'a mut Peekable<IntoIter<Token>>) -> Self {
        Compiler { tokens }
    }

    pub fn compile(&mut self) {}

    fn peek(&mut self) -> &Token {
        if let Some(token) = self.tokens.peek() {
            return token;
        } else {
            return &Token::EOF;
        }
    }

    fn advance(&mut self) {
        self.tokens.next();
    }
}

#[test]
fn test_tokens() {
    let input = String::from(
        r#"
          var phil = 34;
          var age = 12 + 34;
          fun getAge() {
             return age;
          }
        "#,
    );
}

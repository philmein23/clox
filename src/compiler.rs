use crate::chunk::{Chunk, Constant, LineNumber, OpCode};
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
use std::default;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd)]
pub enum Precendence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

#[derive(Debug, Copy, Clone)]
pub enum ParseFn {
    Binary,
    Unary,
    Number,
    Grouping,
}

#[derive(Debug, Copy, Clone)]
pub struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precendence,
}

pub struct Compiler<'a> {
    tokens: &'a mut Peekable<IntoIter<Token>>,
    chunk: Chunk,
    curr_line: LineNumber,
}

impl<'a> Compiler<'a> {
    pub fn new(tokens: &'a mut Peekable<IntoIter<Token>>) -> Self {
        let chunk = Chunk::new();
        let ln = LineNumber::new(0);
        Compiler {
            tokens,
            chunk,
            curr_line: ln,
        }
    }

    pub fn compile(&mut self) {
        let _ = self.expression();

        println!("CURRENT CHUNK: {:?}, CURRENT CONSTANT: {:?}", self.chunk.code, self.chunk.constants);
    }

    fn expression(&mut self) -> Result<(), String> {
        self.parse_precedence(Precendence::Assignment);
        Ok(())
    }

    fn get_rule(&mut self) -> ParseRule {
        let token;

        if let Some(tkn) = self.peek() {
            token = tkn;
        } else {
            return ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            }
        }

        match token.token_type {
            TokenType::LEFT_PAREN => ParseRule {
                prefix: Some(ParseFn::Grouping),
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::RIGHT_PAREN => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::LEFT_BRACE => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::RIGHT_BRACE => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::COMMA => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::DOT => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::MINUS => ParseRule {
                prefix: Some(ParseFn::Unary),
                infix: Some(ParseFn::Binary),
                precedence: Precendence::Term,
            },
            TokenType::PLUS => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precendence::Term,
            },
            TokenType::SEMICOLON => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::SLASH => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precendence::Factor,
            },
            TokenType::STAR => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precendence::Factor,
            },
            TokenType::BANG => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::BANG_EQUAL => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::EQUAL => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::EQUAL_EQUAL => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::GREATER => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::GREATER_EQUAL => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::LESS => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::LESS_EQUAL => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::IDENTIFIER(_) => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::STRING(_) => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::NUMBER(_) => ParseRule {
                prefix: Some(ParseFn::Number),
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::AND => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::CLASS => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::ELSE => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::FALSE => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::FOR => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::FUN => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::IF => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::NIL => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::OR => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::PRINT => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::RETURN => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::SUPER => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::THIS => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::TRUE => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::VAR => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::WHILE => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::EOF => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
            TokenType::INVALID(_) => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precendence::None,
            },
        }
    }

    fn apply_parse_fn(&mut self, parse_fn: ParseFn) {
        match parse_fn {
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
            ParseFn::Grouping => self.grouping(),
        }
    }

    fn binary(&mut self) {}

    fn unary(&mut self) {}

    fn number(&mut self) {
        let token = self.advance();
        if let TokenType::NUMBER(n) = token.token_type {
               self.emit_constant(n); 
        }
    }
    fn grouping(&mut self) {}

    fn parse_precedence(&mut self, precedence: Precendence) {
        let prefix_rule = self.get_rule();

        if let ParseRule {
            prefix: Some(prefix_rule),
            infix: _,
            precedence: _,
        } = prefix_rule
        {
            self.apply_parse_fn(prefix_rule);
        } else {
            panic!("Expected expression.");
        }

        while precedence <= self.get_rule().precedence {
            let infix_rule = self.get_rule();

            if let ParseRule {
                prefix: _,
                infix: Some(infix_rule),
                precedence: _,
            } = infix_rule
            {
                self.apply_parse_fn(infix_rule);
            }
        }
    }

    fn emit_byte(&mut self, op: OpCode) {
        self.chunk.code.push((op, self.curr_line));
    }

    fn emit_constant(&mut self, num: f64) {
        let index = self.chunk.add_constant(Constant::Number(num));

        self.emit_byte(OpCode::Constant(index));
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn advance(&mut self) -> Token {
        // consume the token and set the current line number
        match self.tokens.next() {
            Some(token) => {
                self.curr_line = LineNumber::new(token.line);
                token
            }
            None => Token {
                token_type: TokenType::EOF,
                line: 0,
            },
        }
    }
}

#[test]
fn test_binary() {
    let input = String::from(
        r#" 
        12
        "#,
    );

    compile(input);    
}

fn compile(input: String) {
    let mut scanner = Scanner::new(input.as_str());
    let tokens = scanner.scan_tokens().unwrap();
    let mut iter = tokens.into_iter().peekable();
    let mut compiler = Compiler::new(&mut iter);
    compiler.compile();
}

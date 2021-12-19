use crate::chunk::{Chunk, Constant, LineNumber, OpCode};
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
use std::default;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd)]
pub enum Precedence {
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

fn increment_precedence(prec: Precedence) -> Precedence {
    match prec {
        Precedence::None => Precedence::Assignment,
        Precedence::Assignment => Precedence::Or,
        Precedence::Or => Precedence::And,
        Precedence::And => Precedence::Equality,
        Precedence::Equality => Precedence::Comparison,
        Precedence::Comparison => Precedence::Term,
        Precedence::Term => Precedence::Factor,
        Precedence::Factor => Precedence::Unary,
        Precedence::Unary => Precedence::Call,
        Precedence::Call => Precedence::Primary,
        Precedence::Primary => Precedence::Primary,
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ParseFn {
    Binary,
    Unary,
    Number,
    Grouping,
    Literal,
}

#[derive(Debug, Copy, Clone)]
pub struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

pub struct Compiler {
    tokens: Peekable<IntoIter<Token>>,
    pub chunk: Chunk,
    curr_line: LineNumber,
}

impl Compiler {
    pub fn new() -> Self {
        let chunk = Chunk::new();
        let ln = LineNumber::new(0);
        let tokens = vec![].into_iter().peekable();
        Compiler {
            tokens,
            chunk,
            curr_line: ln,
        }
    }

    pub fn compile(&mut self, source: &str) -> Result<(), String> {
        let mut scanner = Scanner::new(source);
        let _ = scanner.scan_tokens();
        self.tokens = scanner.tokens.into_iter().peekable();

        let maybe_ok = self.expression();

        self.emit_return();
        println!(
            "CURRENT CHUNK: {:?}, CURRENT CONSTANT: {:?}",
            self.chunk.code, self.chunk.constants
        );

        maybe_ok
    }

    fn expression(&mut self) -> Result<(), String> {
        self.parse_precedence(Precedence::Assignment);
        Ok(())
    }

    fn get_rule(&mut self, token: &Token) -> ParseRule {
        match token.token_type {
            TokenType::LEFT_PAREN => ParseRule {
                prefix: Some(ParseFn::Grouping),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::RIGHT_PAREN => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::LEFT_BRACE => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::RIGHT_BRACE => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::COMMA => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::DOT => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::MINUS => ParseRule {
                prefix: Some(ParseFn::Unary),
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Term,
            },
            TokenType::PLUS => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Term,
            },
            TokenType::SEMICOLON => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::SLASH => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Factor,
            },
            TokenType::STAR => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Factor,
            },
            TokenType::BANG => ParseRule {
                prefix: Some(ParseFn::Unary),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::BANG_EQUAL => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Equality,
            },
            TokenType::EQUAL => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::EQUAL_EQUAL => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Equality,
            },
            TokenType::GREATER => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Comparison,
            },
            TokenType::GREATER_EQUAL => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Comparison,
            },
            TokenType::LESS => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Comparison,
            },
            TokenType::LESS_EQUAL => ParseRule {
                prefix: None,
                infix: Some(ParseFn::Binary),
                precedence: Precedence::Comparison,
            },
            TokenType::IDENTIFIER(_) => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::STRING(_) => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::NUMBER(_) => ParseRule {
                prefix: Some(ParseFn::Number),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::AND => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::CLASS => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::ELSE => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::FALSE => ParseRule {
                prefix: Some(ParseFn::Literal),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::FOR => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::FUN => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::IF => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::NIL => ParseRule {
                prefix: Some(ParseFn::Literal),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::OR => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::PRINT => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::RETURN => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::SUPER => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::THIS => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::TRUE => ParseRule {
                prefix: Some(ParseFn::Literal),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::VAR => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::WHILE => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::EOF => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::ERROR(_) => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    fn literal(&mut self) -> Result<(), String> {
        let token = self.advance(); // consume the literal

        match token.token_type {
            TokenType::TRUE => self.emit_byte(OpCode::True),
            TokenType::FALSE => self.emit_byte(OpCode::False),
            TokenType::NIL => self.emit_byte(OpCode::Nil),
            _ => {
                return Ok(());
            }
        }
        Ok(())
    }

    fn apply_parse_fn(&mut self, parse_fn: ParseFn) -> Result<(), String> {
        match parse_fn {
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
            ParseFn::Grouping => self.grouping(),
            ParseFn::Literal => self.literal(),
        }
    }

    fn binary(&mut self) -> Result<(), String> {
        let op = self.advance();
        let rule = self.get_rule(&op);
        self.parse_precedence(increment_precedence(rule.precedence));

        match op.token_type {
            TokenType::BANG_EQUAL => self.emit_bytes(OpCode::Equal, OpCode::Not),
            TokenType::EQUAL_EQUAL => self.emit_byte(OpCode::Equal),
            TokenType::GREATER => self.emit_byte(OpCode::Greater),
            TokenType::GREATER_EQUAL => self.emit_bytes(OpCode::Less, OpCode::Not),
            TokenType::LESS => self.emit_byte(OpCode::Less),
            TokenType::LESS_EQUAL => self.emit_bytes(OpCode::Greater, OpCode::Not),
            TokenType::PLUS => self.emit_byte(OpCode::Add),
            TokenType::MINUS => self.emit_byte(OpCode::Subtract),
            TokenType::STAR => self.emit_byte(OpCode::Multiply),
            TokenType::SLASH => self.emit_byte(OpCode::Divide),
            _ => {
                return Ok(());
            }
        }

        Ok(())
    }

    fn unary(&mut self) -> Result<(), String> {
        let unary_op = self.advance();

        self.parse_precedence(Precedence::Unary);

        match unary_op.token_type {
            TokenType::BANG => self.emit_byte(OpCode::Not),
            TokenType::MINUS => self.emit_byte(OpCode::Negate),
            _ => {
                return Ok(());
            }
        }

        Ok(())
    }

    fn number(&mut self) -> Result<(), String> {
        let token = self.advance();
        if let TokenType::NUMBER(n) = token.token_type {
            self.emit_constant(n);
        }

        Ok(())
    }
    fn grouping(&mut self) -> Result<(), String> {
        let _ = self.advance(); // consume the '(' token
        let _ = self.expression();
        let _ = self.advance(); // consume the ')' token

        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), String> {
        let token = self.peek();
        let prefix_rule = self.get_rule(&token);
        let mut maybe_ok = Ok(());

        match prefix_rule.prefix {
            Some(rule) => {
                maybe_ok = self.apply_parse_fn(rule);
            }
            _ => {
                return Err("Expected expression".to_string());
            }
        }

        // loops through an arithmetic expression
        // and only breaks when precedence is too low or not an infix operator
        loop {
            let token = self.peek();
            let maybe_infix_rule = self.get_rule(&token);
            if precedence <= maybe_infix_rule.precedence {
                if let ParseRule {
                    prefix: _,
                    infix: Some(infix_rule),
                    precedence: _,
                } = maybe_infix_rule
                {
                    maybe_ok = self.apply_parse_fn(infix_rule);
                }
            } else {
                break;
            }
        }

        maybe_ok
    }

    fn emit_byte(&mut self, op: OpCode) {
        self.chunk.code.push((op, self.curr_line));
    }

    fn emit_bytes(&mut self, op1: OpCode, op2: OpCode) {
        self.chunk.code.push((op1, self.curr_line));
        self.chunk.code.push((op2, self.curr_line));
    }

    fn emit_constant(&mut self, num: f64) {
        let index = self.chunk.add_constant(Constant::Number(num));

        self.emit_byte(OpCode::Constant(index));
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return);
    }

    fn peek(&mut self) -> Token {
        if let Some(token) = self.tokens.peek() {
            token.to_owned()
        } else {
            return Token {
                token_type: TokenType::EOF,
                line: 0,
            };
        }
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
fn test_number() {
    let input = String::from(
        r#" 
        12;
        "#,
    );

    compile(input);
}

#[test]
fn test_unary() {
    let input = String::from(
        r#"
         -12
        "#,
    );

    compile(input);
}

#[test]
fn test_binary() {
    let input = String::from(
        r#"
        12 + 4 * 3 - 1;
        "#,
    );
    compile(input);
}

#[test]
fn test_binary_two() {
    let input = String::from(
        r#"
        -5 - 10 + 4 / 2;
        "#,
    );

    compile(input);
}

#[test]
fn test_grouping() {
    let input = String::from(
        r#"
        (12 + 4 +  (4 - 2)) * 4;
        "#,
    );

    compile(input);
}
#[test]
fn test_true() {
    let input = String::from(
        r#"
         true;
        "#,
    );

    compile(input);
}

#[test]
fn test_false() {
    let input = String::from(
        r#"
         false;
        "#,
    );

    compile(input);
}

#[test]
fn test_nil() {
    let input = String::from(
        r#"
         nil;
        "#,
    );

    compile(input);
}

fn compile(input: String) {
    let mut compiler = Compiler::new();
    compiler.compile(input.as_str());
}

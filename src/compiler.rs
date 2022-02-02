use crate::chunk::{Chunk, Constant, LineNumber, OpCode};
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
use std::default;
use std::fmt::format;
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
    String,
    Variable,
}

#[derive(Debug, Clone)]
struct Local {
    iden: String,
    depth: isize,
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
    locals: Vec<Local>,
    scope_depth: isize,
    local_count: isize,
}

impl Compiler {
    pub fn new() -> Self {
        let chunk = Chunk::new();
        let ln = LineNumber::new(0);
        let tokens = vec![].into_iter().peekable();
        let locals = Default::default();
        Compiler {
            tokens,
            chunk,
            curr_line: ln,
            locals,
            scope_depth: 0,
            local_count: 0,
        }
    }

    pub fn compile(&mut self, source: &str) -> Result<(), String> {
        let mut scanner = Scanner::new(source);
        let _ = scanner.scan_tokens();
        self.tokens = scanner.tokens.into_iter().peekable();

        let mut maybe_ok = Ok(());
        loop {
            let token = self.peek();
            match token.token_type {
                TokenType::EOF => break,
                _ => {
                    maybe_ok = self.declaration();
                }
            }
        }
        self.advance(); // consume the EOF token

        println!(
            "CURRENT CHUNK: {:?}, CURRENT CONSTANT: {:?}",
            self.chunk.code, self.chunk.constants
        );

        maybe_ok
    }

    fn declaration(&mut self) -> Result<(), String> {
        let maybe_ok = match self.peek().token_type {
            TokenType::VAR => self.var_declaration(),
            _ => self.statement(),
        };

        maybe_ok
    }

    fn var_declaration(&mut self) -> Result<(), String> {
        self.advance(); // consume the var token
        let iden = if let TokenType::IDENTIFIER(i) = self.peek().token_type {
            i
        } else {
            return Err("There is no variable identifier".to_string());
        };

        self.advance(); // consume the identifier token

        self.declare_variable(&iden);

        if let TokenType::EQUAL = self.peek().token_type {
            self.advance(); // consume the equal token
            self.expression();
        } else {
            self.emit_byte(OpCode::Nil);
        }

        self.advance(); // consume semicolon token

        if self.scope_depth > 0 {
            self.mark_initialized();
            return Ok(());
        }

        self.emit_constant(Constant::String(iden));
        self.emit_byte(OpCode::DefineGlobal);

        Ok(())
    }

    fn mark_initialized(&mut self) {
        if let Some(local) = self.locals.get_mut(self.local_count as usize - 1) {
            local.depth = self.scope_depth;
        }
    }

    fn declare_variable(&mut self, name: &String) {
        if self.scope_depth == 0 {
            return;
        };
        let mut len = self.local_count - 1;
        loop {
            if len < 0 {
                break;
            }
            match self.locals.get(len as usize) {
                Some(local) if local.depth != -1 && local.depth < self.scope_depth => {
                    break;
                }
                Some(local) if name.eq(&local.iden) => {
                    panic!(format!(
                        "Already a variable with this name {:?} in this scope.",
                        name
                    ));
                }
                _ => {
                    len -= 1;
                }
            }
        }

        self.add_local(name)
    }

    fn add_local(&mut self, name: &String) {
        self.local_count += 1;

        let new_local = Local {
            iden: name.to_owned(),
            depth: -1,
        };

        self.locals.push(new_local);
    }

    fn statement(&mut self) -> Result<(), String> {
        match self.peek().token_type {
            TokenType::PRINT => self.print_statement(),
            TokenType::LEFT_BRACE => self.block(),
            TokenType::IF => self.if_statement(),
            _ => self.expression_statement(),
        }
    }

    fn if_statement(&mut self) -> Result<(), String> {
        self.advance(); // consume the if token
        self.advance(); // consume the left paren token

        self.expression();

        self.advance(); // consume the right paren token

        let then_jump = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_byte(OpCode::Pop);

        self.statement(); // compile statements in then branch

        let else_jump = self.emit_jump(OpCode::Jump(0));
        self.patch_jump(then_jump);
        
        self.emit_byte(OpCode::Pop);
        let peek_token = self.peek();
        
        if peek_token.token_type == TokenType::ELSE {
            self.advance(); // consume else token;
            self.statement();
        };

        self.patch_jump(else_jump)
    }

    fn emit_jump(&mut self, op: OpCode) -> usize {
        self.emit_byte(op);
        self.chunk.code.len() - 1
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), String> {
        let true_jump = self.chunk.code.len() - offset - 1;
        println!("CODE LEN: {:?}", self.chunk.code.len());
        println!("TRUE JUMP {:?}", true_jump);
        match self.chunk.code.get_mut(offset) {
            Some((OpCode::JumpIfFalse(val), _)) => {
                *val = true_jump;
            }
            Some((OpCode::Jump(val), _)) => {
                *val = true_jump;
            }
            _ => {
                return Err("Too much code to jump over".to_string());
            }
        }

        Ok(())
    }

    fn block(&mut self) -> Result<(), String> {
        self.advance(); // consume left brace token
        self.begin_scope();
        loop {
            match self.peek().token_type {
                TokenType::RIGHT_BRACE | TokenType::EOF => break,
                _ => {
                    self.declaration();
                }
            }
        }

        self.advance(); // consume right brace token
        self.end_scope();

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        loop {
            match self.locals.pop() {
                Some(local) if local.depth > self.scope_depth && self.local_count > 0 => {
                    self.emit_byte(OpCode::Pop);
                    self.local_count -= 1;
                }
                _ => {
                    break;
                }
            }
        }
    }

    fn print_statement(&mut self) -> Result<(), String> {
        self.advance(); // consume print token
        let maybe_ok = self.expression();
        let token = self.advance(); // consume semicolon token
        self.emit_byte(OpCode::Print);

        maybe_ok
    }

    fn expression_statement(&mut self) -> Result<(), String> {
        let maybe_ok = self.expression();
        println!("EXPRESSION STATEMENT");
        self.advance(); // consume semicolon token;

        self.emit_byte(OpCode::Pop);

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
                prefix: Some(ParseFn::Variable),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::STRING(_) => ParseRule {
                prefix: Some(ParseFn::String),
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

    fn literal(&mut self, can_assign: bool) -> Result<(), String> {
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

    fn string(&mut self, can_assign: bool) -> Result<(), String> {
        let token = self.advance(); // consume string token;
        match token.token_type {
            TokenType::STRING(s) => {
                self.emit_constant(Constant::String(s));
            }
            _ => return Ok(()),
        }

        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), String> {
        let iden = if let TokenType::IDENTIFIER(i) = self.peek().token_type {
            i
        } else {
            return Err("There is no variable identifier".to_string());
        };

        self.advance(); // consume identifier token

        if self.scope_depth > 0 {
            self.handle_local_variable(&iden, can_assign);
            return Ok(());
        }

        let peek_token = self.peek().token_type;
        if peek_token == TokenType::EQUAL && can_assign {
            self.advance(); // consume the equal token
            self.expression();

            self.emit_constant(Constant::String(iden));
            self.emit_byte(OpCode::SetGlobal);
        } else {
            self.emit_constant(Constant::String(iden));
            self.emit_byte(OpCode::GetGlobal);
        }

        Ok(())
    }

    fn handle_local_variable(&mut self, name: &String, can_assign: bool) -> Result<(), String> {
        let peek_token = self.peek().token_type;
        let idx = self.resolve_variable(name);

        if peek_token == TokenType::EQUAL && can_assign {
            self.advance(); // consume equal token
            self.expression();

            self.emit_byte(OpCode::SetLocal(idx));
        } else {
            self.emit_byte(OpCode::GetLocal(idx));
        }

        Ok(())
    }

    fn resolve_variable(&mut self, name: &String) -> usize {
        let mut idx = self.local_count - 1;

        loop {
            match self.locals.get(idx as usize) {
                Some(local) if local.depth == -1 => {
                    panic!("Can't read local variable in its own initializer");
                }
                Some(local) if local.iden.eq(name) => {
                    return idx as usize;
                }
                None => {
                    panic!("There is no local variable found.")
                }
                _ => {
                    idx -= 1;
                }
            }
        }
    }

    fn apply_parse_fn(&mut self, parse_fn: ParseFn, can_assign: bool) -> Result<(), String> {
        match parse_fn {
            ParseFn::Unary => self.unary(can_assign),
            ParseFn::Binary => self.binary(can_assign),
            ParseFn::Number => self.number(can_assign),
            ParseFn::Grouping => self.grouping(can_assign),
            ParseFn::Literal => self.literal(can_assign),
            ParseFn::String => self.string(can_assign),
            ParseFn::Variable => self.variable(can_assign),
        }
    }

    fn binary(&mut self, can_assign: bool) -> Result<(), String> {
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

    fn unary(&mut self, can_assign: bool) -> Result<(), String> {
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

    fn number(&mut self, can_assign: bool) -> Result<(), String> {
        let token = self.advance();
        if let TokenType::NUMBER(n) = token.token_type {
            self.emit_constant(Constant::Number(n));
        }

        Ok(())
    }
    fn grouping(&mut self, can_assign: bool) -> Result<(), String> {
        let _ = self.advance(); // consume the '(' token
        let _ = self.expression();
        let _ = self.advance(); // consume the ')' token

        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), String> {
        let token = self.peek();
        let prefix_rule = self.get_rule(&token);
        let mut maybe_ok = Ok(());
        let can_assign = precedence <= Precedence::Assignment;

        match prefix_rule.prefix {
            Some(rule) => {
                maybe_ok = self.apply_parse_fn(rule, can_assign);
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
                    maybe_ok = self.apply_parse_fn(infix_rule, can_assign);
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

    fn emit_constant(&mut self, val: Constant) {
        let index = self.chunk.add_constant(val);

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

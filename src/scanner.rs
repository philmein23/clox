use std::{iter::Peekable, str::CharIndices};

use crate::token::{Token, TokenType};
pub struct Scanner {
    source: String,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: source.to_string(),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, ScannerError> {
        let mut tokens = vec![];
        let mut line_number = 0;
        for line in self.source.lines() {
            line_number += 1;
            let line = line.trim();

            let mut char_indices = line.char_indices().peekable();

            while let Some((pos, ch)) = char_indices.next() {
                let token = match ch {
                    ' ' => continue,
                    '+' => self.create_token(TokenType::PLUS, line_number),
                    '-' => self.create_token(TokenType::MINUS, line_number),
                    '*' => self.create_token(TokenType::STAR, line_number),
                    '!' => match char_indices.next_if_eq(&(pos + 1, '=')) {
                        Some(_equals) => self.create_token(TokenType::BANG_EQUAL, line_number),
                        None => self.create_token(TokenType::BANG, line_number),
                    },
                    '>' => match char_indices.next_if_eq(&(pos + 1, '=')) {
                        Some(_equals) => self.create_token(TokenType::GREATER_EQUAL, line_number),
                        None => self.create_token(TokenType::GREATER, line_number),
                    },
                    '<' => match char_indices.next_if_eq(&(pos + 1, '=')) {
                        Some(_equals) => self.create_token(TokenType::LESS_EQUAL, line_number),
                        None => self.create_token(TokenType::LESS, line_number),
                    },
                    '=' => match char_indices.next_if_eq(&(pos + 1, '=')) {
                        Some(_equals) => self.create_token(TokenType::EQUAL_EQUAL, line_number),
                        None => self.create_token(TokenType::EQUAL, line_number),
                    },
                    '{' => self.create_token(TokenType::LEFT_BRACE, line_number),
                    '}' => self.create_token(TokenType::RIGHT_BRACE, line_number),
                    '(' => self.create_token(TokenType::LEFT_PAREN, line_number),
                    ')' => self.create_token(TokenType::RIGHT_PAREN, line_number),
                    '.' => self.create_token(TokenType::DOT, line_number),
                    ',' => self.create_token(TokenType::COMMA, line_number),
                    ';' => self.create_token(TokenType::SEMICOLON, line_number),
                    '"' => self.emit_string_token(&mut char_indices, line_number),
                    ch if ch.is_ascii_digit() => self.emit_number_token(ch, &mut char_indices, line_number),
                    ch if ch.is_ascii_alphabetic() || ch == '_' => {
                        self.emit_iden_token(ch, &mut char_indices, line_number)
                    }
                    '/' => match char_indices.next_if_eq(&(pos + 1, '/')) {
                        Some(_equals) => {
                            while let Some((_pos, _ch)) = char_indices.next() {
                                continue;
                            }
                            self.create_token(
                                TokenType::INVALID("invalid".to_string()),
                                line_number,
                            )
                        }
                        None => self.create_token(TokenType::SLASH, line_number),
                    },
                    _ => self.create_token(TokenType::INVALID(ch.to_string()), line_number),
                };

                tokens.push(token);
            }
        }

        Ok(tokens)
    }

    fn match_reserved_word(&self, iden: &str, ln: usize) -> Option<Token> {
        match iden {
            "and" => Some(self.create_token(TokenType::AND, ln)),
            "class" => Some(self.create_token(TokenType::CLASS, ln)),
            "else" => Some(self.create_token(TokenType::ELSE, ln)),
            "false" => Some(self.create_token(TokenType::FALSE, ln)),
            "for" => Some(self.create_token(TokenType::FOR, ln)),
            "fun" => Some(self.create_token(TokenType::FUN, ln)),
            "if" => Some(self.create_token(TokenType::IF, ln)),
            "nil" => Some(self.create_token(TokenType::NIL, ln)),
            "or" => Some(self.create_token(TokenType::OR, ln)),
            "print" => Some(self.create_token(TokenType::PRINT, ln)),
            "return" => Some(self.create_token(TokenType::RETURN, ln)),
            "super" => Some(self.create_token(TokenType::SUPER, ln)),
            "this" => Some(self.create_token(TokenType::THIS, ln)),
            "true" => Some(self.create_token(TokenType::TRUE, ln)),
            "var" => Some(self.create_token(TokenType::VAR, ln)),
            "while" => Some(self.create_token(TokenType::WHILE, ln)),
            _ => None,
        }
    }

    fn emit_iden_token(
        &self,
        initial: char,
        char_indices: &mut Peekable<CharIndices>,
        ln: usize,
    ) -> Token {
        let mut iden = initial.to_string();
        while let Some((_pos, ch)) = char_indices.next_if(|(_pos, ch)| ch.is_alphabetic()) {
            iden.push(ch);
        }

        self.match_reserved_word(iden.as_str(), ln)
            .unwrap_or_else(|| self.create_token(TokenType::IDENTIFIER(iden), ln))
    }

    fn emit_string_token(&self, char_indices: &mut Peekable<CharIndices>, ln: usize) -> Token {
        let mut val = "".to_string();
        while let Some((_pos, ch)) = char_indices.next_if(|(_pos, ch)| *ch != '"') {
            val.push(ch);
        }
        let maybe_quote = char_indices.next();
        match maybe_quote {
            Some(_) => self.create_token(TokenType::STRING(val), ln),
            None => self.create_token(TokenType::INVALID("unterminated string".to_string()), ln),
        }
    }

    fn emit_number_token(&self, initial: char, char_indices: &mut Peekable<CharIndices>, ln: usize) -> Token {
        let mut digit = initial.to_string();
        while let Some((_pos, ch)) = char_indices.next_if(|(_pos, ch)| ch.is_ascii_digit()) {
            digit.push(ch);
        }

        let parsed: f64 = digit.parse().unwrap();
        self.create_token(TokenType::NUMBER(parsed), ln)
    }

    fn create_token(&self, token_type: TokenType, line: usize) -> Token {
        Token { token_type, line }
    }
}

#[derive(Debug)]
pub enum ScannerError {
    ScanningError(String),
}

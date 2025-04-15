use std::iter::FusedIterator;

use log::{debug, info};
use phf::phf_map;

use crate::token::{Token, TokenType};

static KEYWORDS: phf::Map<&'static [u8], TokenType> = phf_map! {
    b"and" => TokenType::AND,
    b"class" => TokenType::CLASS,
    b"else" => TokenType::ELSE,
    b"false" => TokenType::FALSE,
    b"fun" => TokenType::FUN,
    b"for" => TokenType::FOR,
    b"if" => TokenType::IF,
    b"nil" => TokenType::NIL,
    b"or" => TokenType::OR,
    b"print" => TokenType::PRINT,
    b"return" => TokenType::RETURN,
    b"super" => TokenType::SUPER,
    b"this" => TokenType::THIS,
    b"true" => TokenType::TRUE,
    b"var" => TokenType::VAR,
    b"while" => TokenType::WHILE,
};

#[derive(Debug, Clone)]
pub struct Scanner {
    source: Vec<u8>,
    start: usize,
    curr_ptr: usize,
    line: usize,
    had_error: bool,
    pending_token: Option<TokenType>,
}

impl Scanner {
    pub fn new(buf: Vec<u8>) -> Self {
        info!("Initializing Scanner with buffer of {} bytes", buf.len());
        Self {
            source: buf,
            start: 0,
            curr_ptr: 0,
            line: 1,
            had_error: false,
            pending_token: None,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.source.len()
    }

    fn scan_token(&mut self) -> Result<(), String> {
        debug!(
            "Scanning token at position {}, line {}",
            self.curr_ptr, self.line
        );
        let byte: u8 = self.advance();
        debug!("Processing byte: '{}'", byte as char);

        match byte {
            b'(' => self.add_token(TokenType::LEFT_PAREN),

            b')' => self.add_token(TokenType::RIGHT_PAREN),

            b'{' => self.add_token(TokenType::LEFT_BRACE),

            b'}' => self.add_token(TokenType::RIGHT_BRACE),

            b',' => self.add_token(TokenType::COMMA),

            b'.' => self.add_token(TokenType::DOT),

            b'-' => self.add_token(TokenType::MINUS),

            b'+' => self.add_token(TokenType::PLUS),

            b';' => self.add_token(TokenType::SEMICOLON),

            b'*' => self.add_token(TokenType::STAR),

            b'!' => {
                let token_type: TokenType = if self.match_byte(b'=') {
                    debug!("Matched '!=' -> BANG_EQUAL");
                    TokenType::BANG_EQUAL
                } else {
                    debug!("Single '!' -> BANG");
                    TokenType::BANG
                };
                self.add_token(token_type);
            }

            b'=' => {
                let token_type: TokenType = if self.match_byte(b'=') {
                    debug!("Matched '==' -> EQUAL_EQUAL");
                    TokenType::EQUAL_EQUAL
                } else {
                    debug!("Single '=' -> EQUAL");
                    TokenType::EQUAL
                };
                self.add_token(token_type);
            }

            b'<' => {
                let token_type: TokenType = if self.match_byte(b'=') {
                    debug!("Matched '<=' -> LESS_EQUAL");
                    TokenType::LESS_EQUAL
                } else {
                    debug!("Single '<' -> LESS");
                    TokenType::LESS
                };
                self.add_token(token_type);
            }

            b'>' => {
                let token_type: TokenType = if self.match_byte(b'=') {
                    debug!("Matched '>=' -> GREATER_EQUAL");
                    TokenType::GREATER_EQUAL
                } else {
                    debug!("Single '>' -> GREATER");
                    TokenType::GREATER
                };
                self.add_token(token_type);
            }

            b' ' | b'\r' | b'\t' => {
                debug!("Skipping whitespace");
            }

            b'\n' => {
                debug!("Incrementing line count to {}", self.line + 1);
                self.line += 1;
            }

            b'/' => {
                if self.match_byte(b'/') {
                    debug!("Found comment, skipping until newline");
                    while self.peek() != b'\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    debug!("Single '/' -> SLASH");
                    self.add_token(TokenType::SLASH);
                }
            }

            b'"' => {
                debug!("Starting string parsing");
                self.parse_string()?;
            }

            b'0'..=b'9' => {
                debug!("Starting number parsing");
                self.parse_number();
            }

            b'a'..=b'z' | b'A'..b'Z' | b'_' => {
                debug!("Starting identifier parsing");
                self.parse_identifier();
            }

            _ => {
                debug!(
                    "Unexpected character '{}' at line {}",
                    byte as char, self.line
                );

                self.had_error = true;

                return Err(format!(
                    "[line {}] Error: Unexpected character: {}",
                    self.line, byte as char
                ));
            }
        }

        Ok(())
    }

    fn parse_string(&mut self) -> Result<(), String> {
        debug!("Parsing string literal at line {}", self.line);
        while !self.is_at_end() && self.peek() != b'"' {
            if self.peek() == b'\n' {
                debug!("Newline in string, incrementing line to {}", self.line + 1);
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            debug!("Unterminated string at line {}", self.line);

            self.had_error = true;

            return Err(format!("[line {}] Error: Unterminated string.", self.line));
        }

        self.advance();

        let parsed_string: String = unsafe {
            String::from_utf8_unchecked(self.source[self.start + 1..self.curr_ptr - 1].to_vec())
        };

        info!("Parsed string literal: {}", parsed_string);

        self.add_token(TokenType::STRING(parsed_string));

        Ok(())
    }

    fn parse_number(&mut self) {
        debug!("Parsing number starting at position {}", self.start);

        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            debug!("Found decimal point in number");

            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let parsed_number: String =
            unsafe { String::from_utf8_unchecked(self.source[self.start..self.curr_ptr].to_vec()) };

        let number: f64 = parsed_number.parse().unwrap_or(0.0);

        info!("Parsed number: {}", number);

        self.add_token(TokenType::NUMBER(number));
    }

    fn parse_identifier(&mut self) {
        debug!("Parsing identifier starting at position {}", self.start);

        while self.peek().is_ascii_alphanumeric() || self.peek() == b'_' {
            self.advance();
        }

        let text: &str =
            unsafe { std::str::from_utf8_unchecked(&self.source[self.start..self.curr_ptr]) };

        match KEYWORDS.get(text.as_bytes()) {
            Some(token_type) => {
                info!("Parsed keyword: {}", text);

                self.add_token(token_type.clone());
            }

            None => {
                info!("Parsed identifier: {}", text);

                self.add_token(TokenType::IDENTIFIER);
            }
        }
    }

    #[inline]
    fn add_token(&mut self, token_type: TokenType) {
        info!("Adding token: {:?}", token_type);

        self.pending_token = Some(token_type);
    }

    #[inline]
    fn advance(&mut self) -> u8 {
        let byte = self.source[self.curr_ptr];

        self.curr_ptr += 1;

        debug!(
            "Advancing to position {}, byte: '{}'",
            self.curr_ptr, byte as char
        );

        byte
    }

    #[inline]
    fn match_byte(&mut self, expected: u8) -> bool {
        if self.is_at_end() || self.source[self.curr_ptr] != expected {
            debug!("No match for byte '{}'", expected as char);

            false
        } else {
            debug!("Matched byte '{}'", expected as char);

            self.curr_ptr += 1;

            true
        }
    }

    #[inline]
    fn peek(&mut self) -> u8 {
        if self.is_at_end() {
            debug!("Peek: at end, returning 0");

            0
        } else {
            debug!("Peek: byte '{}'", self.source[self.curr_ptr] as char);

            self.source[self.curr_ptr]
        }
    }

    #[inline]
    fn peek_next(&mut self) -> u8 {
        if self.curr_ptr + 1 >= self.len() {
            debug!("Peek_next: beyond end, returning 0");

            0
        } else {
            debug!(
                "Peek_next: byte '{}'",
                self.source[self.curr_ptr + 1] as char
            );

            self.source[self.curr_ptr + 1]
        }
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        let at_end = self.curr_ptr >= self.len();

        debug!("Checking is_at_end: {}", at_end);

        at_end
    }
}

impl Iterator for Scanner {
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Self::Item> {
        debug!("Iterator next called at position {}", self.curr_ptr);

        if self.is_at_end() {
            if self.curr_ptr == self.len() {
                self.curr_ptr += 1;

                info!("Reached EOF at line {}", self.line);

                return Some(Ok(Token::new(TokenType::EOF, "".to_string(), self.line)));
            }

            debug!("Iterator fully consumed");

            return None;
        }

        self.pending_token = None;

        self.start = self.curr_ptr;

        debug!("Starting new token scan at position {}", self.start);

        let result: Result<(), String> = self.scan_token();

        if let Err(e) = result {
            debug!("Scan error: {}", e);
            self.had_error = true;
            return Some(Err(e));
        }

        if let Some(token_type) = self.pending_token.take() {
            let lexeme: &str =
                unsafe { std::str::from_utf8_unchecked(&self.source[self.start..self.curr_ptr]) };

            info!(
                "Emitting token: type={:?}, lexeme={}, line={}",
                token_type, lexeme, self.line
            );

            Some(Ok(Token::new(token_type, lexeme.to_string(), self.line)))
        } else {
            debug!("No pending token, continuing scan");

            self.next()
        }
    }
}

impl FusedIterator for Scanner {}

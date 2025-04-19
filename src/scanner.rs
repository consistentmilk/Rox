//! Module `scanner` implements a one‑pass, streaming UTF‑8 lexer for the Lox language.
//!
//! It transforms a byte slice (`&[u8]`) into a sequence of `Token<'a>`s, skipping whitespace
//! and comments, and emitting exactly one `EOF` token at the end. Designed as a `FusedIterator`,
//! it can be chained safely with other iterator adapters.
//!
//! # Public API
//!
//! - `Scanner::new(src: &'a [u8]) -> Scanner<'a>`  
//!   Create a new lexer over the input buffer.
//!
//! - `impl Iterator for Scanner<'a>`  
//!   Yields `Result<Token<'a>, LoxError>` on each `.next()`, where `Ok(token)` is a scanned token
//!   and `Err` reports a lexing error with line information.
//!
//! # Core Phases
//!
//! 1. **Initialization**  
//!    - `start`, `curr`, and `line` counters are set; `pending` holds the next token kind.
//!
//! 2. **Primitive Helpers**  
//!    - `advance()`, `peek()`, `peek_next()`, and `match_byte()` provide fast, inlined access
//!      to the byte stream.  
//!    - `is_at_end()` guards against overrun.  
//!
//! 3. **Lexing Loop** (`next`)  
//!    - On each call, reset `start` and `pending`, then call `scan_token()`.  
//!    - Skip whitespace and comments (including `//` to end-of-line) without setting `pending`.  
//!    - On recognizing a lexeme, set `pending = Some(TokenType)` and return a `Token::new(...)`.  
//!    - At EOF, emit one `EOF` token then return `None`.  
//!
//! 4. **Token Recognition** (`scan_token`)  
//!    - Single‑character tokens: `(`, `)`, `{`, `}`, `,`, `.`, `-`, `+`, `;`, `*`.  
//!    - Two‑character operators: `!=`, `==`, `<=`, `>=`.  
//!    - String literals: `"` … `"`, allowing multi‑line and reporting unterminated errors.  
//!    - Numeric literals: integer and optional fractional part.  
//!    - Identifiers/keywords: alphanumeric/_ sequences, resolved via a perfect‑hash `KEYWORDS` map.  
//!    - Errors: any unexpected byte yields `LoxError::lex(line, message)`.  
//!
//! 5. **Performance Optimizations**  
//!    - Bulk comment skipping via `memchr` for rapid new‑line search.  
//!    - `#[inline(always)]` on hot path helpers.  
//!    - Zero‑allocation lexeme slicing: tokens reference the original buffer.  
//!
//! # Example
//!
//! ```rust
//! let source = b\"print 123; // example\";
//! let mut scanner = Scanner::new(source);
//! for result in &mut scanner {
//!     match result {
//!         Ok(token) => println!(\"{}\", token),
//!         Err(err)    => eprintln!(\"Lex error: {}\", err),
//!     }
//! }
//! ```  

use crate::error::{LoxError, Result};
use crate::token::{Token, TokenType};
use log::{debug, info};
use memchr::memchr;
use phf::phf_map;
use std::iter::FusedIterator;

// ─────────────────────────────────────────────────────────────────────────────
// Static keyword map (compile‑time perfect hash)
// ─────────────────────────────────────────────────────────────────────────────

static KEYWORDS: phf::Map<&'static [u8], TokenType> = phf_map! {
    b"and"    => TokenType::AND,
    b"class"  => TokenType::CLASS,
    b"else"   => TokenType::ELSE,
    b"false"  => TokenType::FALSE,
    b"fun"    => TokenType::FUN,
    b"for"    => TokenType::FOR,
    b"if"     => TokenType::IF,
    b"nil"    => TokenType::NIL,
    b"or"     => TokenType::OR,
    b"print"  => TokenType::PRINT,
    b"return" => TokenType::RETURN,
    b"super"  => TokenType::SUPER,
    b"this"   => TokenType::THIS,
    b"true"   => TokenType::TRUE,
    b"var"    => TokenType::VAR,
    b"while"  => TokenType::WHILE,
};

/// A single pass **scanner / lexer** that converts raw UTF‑8 bytes into a
/// sequence of [`Token`]s.  The lifetime `'a` ties every emitted token’s
/// `lexeme` slice back to the original source buffer.
pub struct Scanner<'a> {
    src: &'a [u8],              // entire source file (memory‑mapped)
    start: usize,               // index of the *first* byte of the current lexeme
    curr: usize,                // index *one past* the last byte examined
    line: usize,                // 1‑based line counter (\n increments)
    pending: Option<TokenType>, // recognised token kind waiting to be emitted
}

impl<'a> Scanner<'a> {
    /// Create a new lexer over `src`.
    #[inline]
    pub fn new(src: &'a [u8]) -> Self {
        info!("Scanner created over {} bytes", src.len());

        Self {
            src,
            start: 0,
            curr: 0,
            line: 1,
            pending: None,
        }
    }

    // ───────────────────────────── primitive helpers ────────────────────────

    /// Return the length of the input slice.
    #[inline(always)]
    const fn len(&self) -> usize {
        self.src.len()
    }

    /// Are we at (or past) the end of input?
    #[inline(always)]
    fn is_at_end(&self) -> bool {
        self.curr >= self.len()
    }

    /// Advance one byte and return it.  *Panics* if called at EOF – higher‑level
    /// code always guards with [`is_at_end`].
    #[inline(always)]
    fn advance(&mut self) -> u8 {
        let b = self.src[self.curr];
        self.curr += 1;
        b
    }

    /// Peek at the current byte without consuming it.  Returns `0` if past EOF
    /// to avoid branching at call‑site.
    #[inline(always)]
    fn peek(&self) -> u8 {
        if self.is_at_end() {
            0
        } else {
            self.src[self.curr]
        }
    }

    /// Peek one byte beyond [`peek`].  Safe at EOF.
    #[inline(always)]
    fn peek_next(&self) -> u8 {
        if self.curr + 1 >= self.len() {
            0
        } else {
            self.src[self.curr + 1]
        }
    }

    /// Conditionally consume a byte **iff** it matches `expected`.
    /// Returns `true` on success so callers can branch inline without an else.
    #[inline(always)]
    fn match_byte(&mut self, expected: u8) -> bool {
        if !self.is_at_end() && self.peek() == expected {
            self.advance();
            true
        } else {
            false
        }
    }

    // ───────────────────────────── core lexing ─────────────────────────────

    /// Scan a *single* token starting at `self.curr`.  If the lexeme produces an
    /// actual token the kind is stored in `self.pending`.  Whitespace and
    /// comments are skipped by returning `Ok(())` with `pending = None`.
    fn scan_token(&mut self) -> Result<()> {
        let b = self.advance();

        match b {
            // ── single‑character punctuators ──────────────────────────────
            b'(' => self.pending = Some(TokenType::LEFT_PAREN),
            b')' => self.pending = Some(TokenType::RIGHT_PAREN),
            b'{' => self.pending = Some(TokenType::LEFT_BRACE),
            b'}' => self.pending = Some(TokenType::RIGHT_BRACE),
            b',' => self.pending = Some(TokenType::COMMA),
            b'.' => self.pending = Some(TokenType::DOT),
            b'-' => self.pending = Some(TokenType::MINUS),
            b'+' => self.pending = Some(TokenType::PLUS),
            b';' => self.pending = Some(TokenType::SEMICOLON),
            b'*' => self.pending = Some(TokenType::STAR),

            // ── two‑character operators (!=, ==, <=, >=) ─────────────────
            b'!' => {
                let tt = if self.match_byte(b'=') {
                    TokenType::BANG_EQUAL
                } else {
                    TokenType::BANG
                };

                self.pending = Some(tt);
            }

            b'=' => {
                let tt = if self.match_byte(b'=') {
                    TokenType::EQUAL_EQUAL
                } else {
                    TokenType::EQUAL
                };

                self.pending = Some(tt);
            }

            b'<' => {
                let tt = if self.match_byte(b'=') {
                    TokenType::LESS_EQUAL
                } else {
                    TokenType::LESS
                };

                self.pending = Some(tt);
            }

            b'>' => {
                let tt = if self.match_byte(b'=') {
                    TokenType::GREATER_EQUAL
                } else {
                    TokenType::GREATER
                };

                self.pending = Some(tt);
            }

            // ── whitespace / newline ─────────────────────────────────────
            b' ' | b'\r' | b'\t' => {
                return Ok(()); // skip insignificants
            }

            b'\n' => {
                self.line += 1; // track for diagnostics

                return Ok(());
            }

            // ── comments (// … until newline) ────────────────────────────
            b'/' => {
                if self.match_byte(b'/') {
                    // Fast‑forward to next newline using `memchr` (≈ 4× faster
                    // than byte‑by‑byte).  If none found, skip to EOF.
                    if let Some(pos) = memchr(b'\n', &self.src[self.curr..]) {
                        self.curr += pos;
                    } else {
                        self.curr = self.len();
                    }

                    return Ok(());
                }

                self.pending = Some(TokenType::SLASH);
            }

            // ── string literal " … " ─────────────────────────────────––
            b'"' => {
                return self.parse_string();
            }

            // ── number literal (digit‑leading) ───────────────────────────
            b'0'..=b'9' => {
                self.parse_number();
            }

            // ── identifiers / keywords (alpha or underscore‑leading) ─────
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                self.parse_identifier();
            }

            // ── unexpected character ─────────────────────────────────────
            _ => {
                return Err(LoxError::lex(
                    self.line,
                    format!("Unexpected character: {}", b as char),
                ));
            }
        }

        Ok(())
    }

    /// Parse a double‑quoted string literal.
    ///
    /// * `self.start` still points to the opening `"`.
    /// * When we return, `self.curr` points **past** the closing `"`.
    fn parse_string(&mut self) -> Result<()> {
        while !self.is_at_end() && self.peek() != b'"' {
            if self.advance() == b'\n' {
                self.line += 1; // support multi‑line strings (allowed in Lox)
            }
        }

        if self.is_at_end() {
            return Err(LoxError::lex(self.line, "Unterminated string."));
        }

        self.advance(); // consume closing quote

        // Slice excluding the surrounding quotes.
        let slice: &[u8] = &self.src[self.start + 1..self.curr - 1];

        // SAFETY: the original source is valid UTF‑8 (guaranteed by caller).
        let s: &str = unsafe { std::str::from_utf8_unchecked(slice) };

        self.pending = Some(TokenType::STRING(s.to_owned()));

        Ok(())
    }

    /// Parse a numeric literal (`123`, `3.14`).  Fractions are optional.
    fn parse_number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Optional fractional part.
        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            self.advance(); // consume "."

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let slice: &[u8] = &self.src[self.start..self.curr];
        let s: &str = unsafe { std::str::from_utf8_unchecked(slice) };
        let n: f64 = s.parse::<f64>().unwrap_or(0.0); // parse never fails (checked digits)
        self.pending = Some(TokenType::NUMBER(n));
    }

    /// Parse an identifier and decide if it is a **keyword** or a generic
    /// `IDENTIFIER` token.
    fn parse_identifier(&mut self) {
        while {
            let c: u8 = self.peek();
            c.is_ascii_alphanumeric() || c == b'_'
        } {
            self.advance();
        }

        let slice: &[u8] = &self.src[self.start..self.curr];

        let tt: TokenType = KEYWORDS
            .get(slice)
            .cloned()
            .unwrap_or(TokenType::IDENTIFIER);

        self.pending = Some(tt);
    }
}

// ───────────────────────── Iterator implementation ─────────────────────────

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token<'a>>; // alias = Result<T, LoxError>

    fn next(&mut self) -> Option<Self::Item> {
        // Loop until we either emit a token, hit EOF, or see an error.
        while self.curr <= self.len() {
            // 1. EOF guard – emit exactly one EOF then terminate.
            if self.curr == self.len() {
                self.curr += 1; // ensure fused semantics
                return Some(Ok(Token::new(TokenType::EOF, "", self.line)));
            }

            // 2. Reset per‑token state.
            self.start = self.curr;
            self.pending = None;

            // 3. Attempt to scan a token.
            if let Err(e) = self.scan_token() {
                return Some(Err(e));
            }

            // 4. If a real token was recognised, build and return it.
            if let Some(tt) = self.pending.take() {
                let slice: &[u8] = &self.src[self.start..self.curr];
                let lex: &str = unsafe { std::str::from_utf8_unchecked(slice) };
                debug!("Scanned token ({:?}) on line {}", tt, self.line);

                return Some(Ok(Token::new(tt, lex, self.line)));
            }
            // Otherwise it was whitespace / comment → continue loop.
        }

        None // already yielded EOF
    }
}

impl<'a> FusedIterator for Scanner<'a> {}

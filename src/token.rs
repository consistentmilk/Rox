use log::{debug, info};
use serde::Serialize;
use std::fmt;
use std::mem;

/// The different kinds of tokens recognized by the Lox scanner.
///
/// Variants without data represent single‑character or keyword tokens.
/// `STRING(String)` and `NUMBER(f64)` carry their literal values.
/// `IDENTIFIER` is used for user‑defined names.
/// `EOF` marks the end of input.
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Serialize)]
pub enum TokenType {
    /// '('
    LEFT_PAREN,

    /// ')'
    RIGHT_PAREN,

    /// '{'
    LEFT_BRACE,

    /// '}'
    RIGHT_BRACE,

    /// ','
    COMMA,

    /// '.'
    DOT,

    /// '-'
    MINUS,

    /// '+'
    PLUS,

    /// ';'
    SEMICOLON,

    /// '/'
    SLASH,

    /// '*'
    STAR,

    /// '!'
    BANG,

    /// '!='
    BANG_EQUAL,

    /// '='
    EQUAL,

    /// '=='
    EQUAL_EQUAL,

    /// '>'
    GREATER,

    /// '>='
    GREATER_EQUAL,

    /// '<'
    LESS,

    /// '<='
    LESS_EQUAL,

    /// A user‑defined identifier
    IDENTIFIER,

    /// A string literal (contents without quotes)
    STRING(String),

    /// A numeric literal
    #[serde(rename = "NUMBER")]
    NUMBER(f64),

    /// 'and'
    AND,

    /// 'class'
    CLASS,

    /// 'else'
    ELSE,

    /// 'false'
    FALSE,

    /// 'fun'
    FUN,

    /// 'for'
    FOR,

    /// 'if'
    IF,

    /// 'nil'
    NIL,

    /// 'or'
    OR,

    /// 'print'
    PRINT,

    /// 'return'
    RETURN,

    /// 'super'
    SUPER,

    /// 'this'
    THIS,

    /// 'true'
    TRUE,

    /// 'var'
    VAR,

    /// 'while'
    WHILE,

    /// End‑of‑file marker
    EOF,
}

impl PartialEq for TokenType {
    /// Two TokenTypes are equal if they share the same variant
    /// (ignoring any inner data). Uses `mem::discriminant` to compare.
    fn eq(&self, other: &Self) -> bool {
        debug!("Comparing TokenType: self={:?}, other={:?}", self, other);

        let same: bool = mem::discriminant(self) == mem::discriminant(other);

        debug!("TokenType match result: {}", same);

        same
    }
}

/// A scanned token, including its type, the original lexeme,
/// and the line number where it was found.
#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Token<'a> {
    /// The category of this token.
    pub token_type: TokenType,

    /// The exact substring from the source that produced this token.
    pub lexeme: &'a str,

    /// 1‑based line number in the source.
    pub line: usize,
}

impl<'a> Token<'a> {
    /// Create a new Token with the given type, lexeme, and line.
    /// Also logs its creation at INFO level.
    pub fn new(token_type: TokenType, lexeme: &'a str, line: usize) -> Self {
        info!(
            "Creating new token: type={:?}, lexeme={}, line={}",
            token_type, lexeme, line
        );

        Self {
            token_type,
            lexeme,
            line,
        }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        debug!(
            "Formatting token: type={:?}, lexeme={}, line={}",
            self.token_type, self.lexeme, self.line
        );

        // ── 1. decide literal string (may borrow or inline‑format) ──────────
        let literal_str: &str = match &self.token_type {
            TokenType::STRING(s) => s,
            TokenType::NUMBER(n) => {
                // 3 → "3.0", 3.14 → "3.14"  (uses a tiny stack buffer)
                if n.fract() == 0.0 {
                    // write into a local array → no allocation, then leak
                    let mut buf: itoa::Buffer = itoa::Buffer::new();
                    let printed: String = format!("{}.0", buf.format(*n as i64));
                    Box::leak(printed.into_boxed_str())
                } else {
                    Box::leak(n.to_string().into_boxed_str())
                }
            }
            _ => "null",
        };

        // ── 2. variant name without payloads ───────────────────────────────
        let variant: &'static str = match self.token_type {
            TokenType::STRING(_) => "STRING",
            TokenType::NUMBER(_) => "NUMBER",
            TokenType::LEFT_PAREN => "LEFT_PAREN",
            TokenType::RIGHT_PAREN => "RIGHT_PAREN",
            TokenType::LEFT_BRACE => "LEFT_BRACE",
            TokenType::RIGHT_BRACE => "RIGHT_BRACE",
            TokenType::COMMA => "COMMA",
            TokenType::DOT => "DOT",
            TokenType::MINUS => "MINUS",
            TokenType::PLUS => "PLUS",
            TokenType::SEMICOLON => "SEMICOLON",
            TokenType::SLASH => "SLASH",
            TokenType::STAR => "STAR",
            TokenType::BANG => "BANG",
            TokenType::BANG_EQUAL => "BANG_EQUAL",
            TokenType::EQUAL => "EQUAL",
            TokenType::EQUAL_EQUAL => "EQUAL_EQUAL",
            TokenType::GREATER => "GREATER",
            TokenType::GREATER_EQUAL => "GREATER_EQUAL",
            TokenType::LESS => "LESS",
            TokenType::LESS_EQUAL => "LESS_EQUAL",
            TokenType::IDENTIFIER => "IDENTIFIER",
            TokenType::AND => "AND",
            TokenType::CLASS => "CLASS",
            TokenType::ELSE => "ELSE",
            TokenType::FALSE => "FALSE",
            TokenType::FUN => "FUN",
            TokenType::FOR => "FOR",
            TokenType::IF => "IF",
            TokenType::NIL => "NIL",
            TokenType::OR => "OR",
            TokenType::PRINT => "PRINT",
            TokenType::RETURN => "RETURN",
            TokenType::SUPER => "SUPER",
            TokenType::THIS => "THIS",
            TokenType::TRUE => "TRUE",
            TokenType::VAR => "VAR",
            TokenType::WHILE => "WHILE",
            TokenType::EOF => "EOF",
        };

        info!(
            "Formatted token: {} {} {}",
            variant, self.lexeme, literal_str
        );

        write!(f, "{} {} {}", variant, self.lexeme, literal_str)
    }
}

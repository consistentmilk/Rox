#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING(String),
    NUMBER(f64),

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    // Special Characters
    EOF,
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TokenType::NUMBER(_), TokenType::NUMBER(_)) => true,
            (TokenType::STRING(_), TokenType::STRING(_)) => true,
            (TokenType::LEFT_PAREN, TokenType::LEFT_PAREN)
            | (TokenType::RIGHT_PAREN, TokenType::RIGHT_PAREN)
            | (TokenType::LEFT_BRACE, TokenType::LEFT_BRACE)
            | (TokenType::RIGHT_BRACE, TokenType::RIGHT_BRACE)
            | (TokenType::COMMA, TokenType::COMMA)
            | (TokenType::DOT, TokenType::DOT)
            | (TokenType::MINUS, TokenType::MINUS)
            | (TokenType::PLUS, TokenType::PLUS)
            | (TokenType::SEMICOLON, TokenType::SEMICOLON)
            | (TokenType::SLASH, TokenType::SLASH)
            | (TokenType::STAR, TokenType::STAR)
            | (TokenType::BANG, TokenType::BANG)
            | (TokenType::BANG_EQUAL, TokenType::BANG_EQUAL)
            | (TokenType::EQUAL, TokenType::EQUAL)
            | (TokenType::EQUAL_EQUAL, TokenType::EQUAL_EQUAL)
            | (TokenType::GREATER, TokenType::GREATER)
            | (TokenType::GREATER_EQUAL, TokenType::GREATER_EQUAL)
            | (TokenType::LESS, TokenType::LESS)
            | (TokenType::LESS_EQUAL, TokenType::LESS_EQUAL)
            | (TokenType::IDENTIFIER, TokenType::IDENTIFIER)
            | (TokenType::TRUE, TokenType::TRUE)
            | (TokenType::FALSE, TokenType::FALSE)
            | (TokenType::NIL, TokenType::NIL)
            | (TokenType::AND, TokenType::AND)
            | (TokenType::CLASS, TokenType::CLASS)
            | (TokenType::ELSE, TokenType::ELSE)
            | (TokenType::FUN, TokenType::FUN)
            | (TokenType::FOR, TokenType::FOR)
            | (TokenType::IF, TokenType::IF)
            | (TokenType::OR, TokenType::OR)
            | (TokenType::PRINT, TokenType::PRINT)
            | (TokenType::RETURN, TokenType::RETURN)
            | (TokenType::SUPER, TokenType::SUPER)
            | (TokenType::THIS, TokenType::THIS)
            | (TokenType::VAR, TokenType::VAR)
            | (TokenType::WHILE, TokenType::WHILE)
            | (TokenType::EOF, TokenType::EOF) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            line,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal: &str = match &self.token_type {
            TokenType::STRING(literal) => literal,

            TokenType::NUMBER(num_literal) => {
                if num_literal.fract() == 0.0 {
                    &format!("{:.1}", num_literal)
                } else {
                    &format!("{}", num_literal)
                }
            }

            _ => "null",
        };

        let tmp: String = format!("{:?}", self.token_type);
        let type_name: &str = tmp.split('(').next().unwrap();

        write!(f, "{} {} {}", type_name, self.lexeme, literal)
    }
}

use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Expr {
    // Used to parse Binary expressions
    Binary(Box<Expr>, Token, Box<Expr>),

    // Used to parse Unary expressions
    Unary(Token, Box<Expr>),

    // Used to parse Literal expressions
    Literal(Token),

    // Used to parse parenthesized grouped expressions
    Grouping(Box<Expr>),

    // Used to parse 'var' keyword expressions
    Variable(Token),

    // Used to parse assignment operators
    Assign(Token, Box<Expr>),

    // Used to parse function calls
    Call(Box<Expr>, Token, Vec<Expr>),
}

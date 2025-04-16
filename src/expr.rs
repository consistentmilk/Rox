use serde::Serialize;

use crate::token::Token;

#[derive(Debug, Clone, Serialize)]
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

impl Expr {
    pub fn line(&self) -> usize {
        match self {
            Expr::Binary(_, token, _) => token.line,

            Expr::Unary(token, _) => token.line,

            Expr::Literal(token) => token.line,

            Expr::Grouping(expr) => expr.line(),

            Expr::Variable(token) => token.line,

            Expr::Assign(token, _) => token.line,

            Expr::Call(_, token, _) => token.line,
        }
    }
}

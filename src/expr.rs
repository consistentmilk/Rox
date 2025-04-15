use crate::token::Token;

#[derive(Debug)]
pub enum Expr<'a> {
    // Used to parse Binary expressions
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),

    // Used to parse Unary expressions
    Unary(Token<'a>, Box<Expr<'a>>),

    // Used to parse Literal expressions
    Literal(Token<'a>),

    // Used to parse parenthesized grouped expressions
    Grouping(Box<Expr<'a>>),

    // Used to parse 'var' keyword expressions
    Variable(Token<'a>),
}

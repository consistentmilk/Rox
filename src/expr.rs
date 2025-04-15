use crate::token::Token;

#[derive(Debug)]
pub enum Expr<'a> {
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Unary(Token<'a>, Box<Expr<'a>>),
    Literal(Token<'a>),
    Grouping(Box<Expr<'a>>),
    Variable(Token<'a>),
}

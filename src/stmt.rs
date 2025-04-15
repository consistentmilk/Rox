use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Print(Expr<'a>),
    Var(Token<'a>, Option<Expr<'a>>),
    Assign(Token<'a>, Expr<'a>),
    Block(Vec<Stmt<'a>>),
    If(Expr<'a>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),
    While(Expr<'a>, Box<Stmt<'a>>),
}

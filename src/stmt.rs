use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Print(Expr<'a>),
    Var(Token<'a>, Option<Expr<'a>>),
}

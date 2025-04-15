use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),

    Print(Expr),

    Var(Token, Option<Expr>),

    Assign(Token, Expr),

    Block(Vec<Stmt>),

    If(Expr, Box<Stmt>, Option<Box<Stmt>>),

    While(Expr, Box<Stmt>),

    For(
        Option<Box<Stmt>>, // initializer (var or expr stmt), boxed
        Option<Expr>,      // condition
        Option<Expr>,      // increment
        Box<Stmt>,         // body
    ),

    Function(Token, Vec<Token>, Box<Stmt>), // name, parameters, body
}

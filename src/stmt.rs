use serde::Serialize;

use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, Serialize)]
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

    Return(Token, Option<Expr>), // New variant for return statements
}

impl Stmt {
    pub fn line(&self) -> usize {
        match self {
            Stmt::Function(token, _, _) => token.line,

            Stmt::Expression(expr) => expr.line(),

            Stmt::Print(expr) => expr.line(),

            Stmt::Var(token, _) => token.line,

            Stmt::Assign(token, _) => token.line,

            Stmt::Block(_) => 0, // Blocks don't have a single line; use 0 or handle differently

            Stmt::If(expr, _, _) => expr.line(),

            Stmt::While(expr, _) => expr.line(),

            Stmt::For(_, _, _, _) => 0, // Use initializer or condition line if needed

            Stmt::Return(token, _) => token.line,
        }
    }
}

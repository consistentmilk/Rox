use serde::Serialize;

use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum Stmt {
    Expression(Expr),

    Print(Expr),

    Var(Token, Option<Expr>),

    Assign(Token, Expr),

    Block(Vec<Stmt>, usize),

    If(Expr, Box<Stmt>, Option<Box<Stmt>>),

    While(Expr, Box<Stmt>),

    For(
        usize,             // store line for 'for' token
        Option<Box<Stmt>>, // initializer
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

            Stmt::If(expr, _, _) => expr.line(),

            Stmt::While(expr, _) => expr.line(),

            Stmt::Block(_, line) => *line,
            
            Stmt::For(line, _, _, _, _) => *line,

            Stmt::Return(token, _) => token.line,
        }
    }
}

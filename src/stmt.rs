use crate::expr::Expr;

#[derive(Debug)]
pub enum Stmt<'a> {
    Expression(Expr<'a>), // e.g., 2 + 3;
    Print(Expr<'a>),      // e.g., print "hello";
}

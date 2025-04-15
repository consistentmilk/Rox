use crate::{expr::Expr, token::TokenType};

pub struct Ast;

impl Ast {
    pub fn print(&self, expr: &Expr) -> String {
        match expr {
            Expr::Binary(left, op, right) => {
                let op_str = match op.token_type {
                    TokenType::PLUS => "+",

                    TokenType::MINUS => "-",

                    TokenType::STAR => "*",

                    TokenType::SLASH => "/",

                    TokenType::BANG_EQUAL => "!=",

                    TokenType::EQUAL_EQUAL => "==",

                    TokenType::GREATER => ">",

                    TokenType::GREATER_EQUAL => ">=",

                    TokenType::LESS => "<",

                    TokenType::LESS_EQUAL => "<=",

                    _ => unreachable!("Invalid binary operator"),
                };

                format!("({} {} {})", op_str, self.print(left), self.print(right))
            }

            Expr::Unary(op, expr) => {
                let op_str = match op.token_type {
                    TokenType::MINUS => "-",

                    TokenType::BANG => "!",

                    _ => unreachable!("Invalid unary operator"),
                };

                format!("({} {})", op_str, self.print(expr))
            }

            Expr::Literal(token) => match &token.token_type {
                TokenType::NUMBER(n) => {
                    if n.fract() == 0.0 {
                        format!("{:.1}", n)
                    } else {
                        n.to_string()
                    }
                }

                TokenType::STRING(s) => s.to_string(),

                TokenType::TRUE => "true".to_string(),

                TokenType::FALSE => "false".to_string(),

                TokenType::NIL => "nil".to_string(),

                _ => unreachable!("Invalid literal"),
            },

            Expr::Grouping(expr) => format!("(group {})", self.print(expr)),

            Expr::Variable(token) => token.lexeme.to_string(),

            Expr::Assign(_name, _expr) => todo!(),
        }
    }
}

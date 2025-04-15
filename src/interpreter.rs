use crate::expr::Expr;
use crate::token::{Token, TokenType};
use crate::value::Value;

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter
    }

    pub fn evaluate(&self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Literal(token) => self.evaluate_literal(token),

            Expr::Unary(op, expr) => self.evaluate_unary(op, expr),

            Expr::Binary(left, op, right) => self.evaluate_binary(left, op, right),

            Expr::Grouping(expr) => self.evaluate(expr),

            Expr::Variable(_token) => todo!(),
        }
    }

    fn evaluate_literal(&self, token: &Token) -> Result<Value, String> {
        match &token.token_type {
            TokenType::NUMBER(n) => Ok(Value::Number(*n)),

            TokenType::STRING(s) => Ok(Value::String(s.clone())),

            TokenType::TRUE => Ok(Value::Bool(true)),

            TokenType::FALSE => Ok(Value::Bool(false)),

            TokenType::NIL => Ok(Value::Nil),

            _ => Err(format!("Invalid literal on line {}", token.line)),
        }
    }

    fn evaluate_unary(&self, op: &Token, expr: &Expr) -> Result<Value, String> {
        let value: Value = self.evaluate(expr)?;

        match op.token_type {
            TokenType::MINUS => match value {
                Value::Number(n) => Ok(Value::Number(-n)),

                _ => Err(format!("Operand must be a number. [line {}]", op.line)),
            },

            TokenType::BANG => Ok(Value::Bool(!is_truthy(&value))),

            _ => Err(format!("Invalid unary operator on line {}", op.line)),
        }
    }

    fn evaluate_binary(&self, left: &Expr, op: &Token, right: &Expr) -> Result<Value, String> {
        let left_val: Value = self.evaluate(left)?;
        let right_val: Value = self.evaluate(right)?;

        match op.token_type {
            TokenType::PLUS => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),

                (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),

                _ => Err(format!(
                    "Operands must be two numbers or two strings on line {}",
                    op.line
                )),
            },

            TokenType::MINUS => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                _ => Err(format!("Operands must be numbers on line {}", op.line)),
            },

            TokenType::STAR => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                _ => Err(format!("Operands must be numbers on line {}", op.line)),
            },

            TokenType::SLASH => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => {
                    if b == 0.0 {
                        Err(format!("Division by zero on line {}", op.line))
                    } else {
                        Ok(Value::Number(a / b))
                    }
                }

                _ => Err(format!("Operands must be numbers on line {}", op.line)),
            },

            TokenType::EQUAL_EQUAL => Ok(Value::Bool(is_equal(&left_val, &right_val))),

            TokenType::BANG_EQUAL => Ok(Value::Bool(!is_equal(&left_val, &right_val))),

            TokenType::LESS => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a < b)),

                _ => Err(format!("Operands must be numbers on line {}", op.line)),
            },

            TokenType::LESS_EQUAL => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a <= b)),

                _ => Err(format!("Operands must be numbers on line {}", op.line)),
            },

            TokenType::GREATER => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a > b)),

                _ => Err(format!("Operands must be numbers on line {}", op.line)),
            },

            TokenType::GREATER_EQUAL => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a >= b)),

                _ => Err(format!("Operands must be numbers on line {}", op.line)),
            },

            _ => Err(format!("Invalid binary operator on line {}", op.line)),
        }
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Nil => false,

        Value::Bool(b) => *b,

        _ => true,
    }
}

fn is_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(a), Value::Number(b)) => a == b,

        (Value::String(a), Value::String(b)) => a == b,

        (Value::Bool(a), Value::Bool(b)) => a == b,

        (Value::Nil, Value::Nil) => true,

        _ => false,
    }
}

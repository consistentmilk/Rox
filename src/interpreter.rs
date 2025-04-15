use crate::environment::Environment;
use crate::expr::Expr;
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, SystemTimeError, UNIX_EPOCH};

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    functions: HashMap<String, Stmt>,
}

impl Interpreter {
    pub fn new() -> Self {
        let environment: Rc<RefCell<Environment>> = Rc::new(RefCell::new(Environment::new()));

        environment.borrow_mut().define(
            "clock",
            Value::NativeFunction {
                name: "clock".to_string(),
                arity: 0,
                func: |_args: &[Value]| {
                    let timestamp: f64 = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .map_err(|e: SystemTimeError| format!("Clock error: {}", e))?
                        .as_secs_f64();

                    Ok(Value::Number(timestamp))
                },
            },
        );

        Interpreter {
            environment,
            functions: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), String> {
        for stmt in statements {
            self.execute(stmt)?;
        }

        Ok(())
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Function(name, parameters, _body) => {
                let function: Value = Value::Function {
                    name: name.lexeme.to_string(),
                    arity: parameters.len(),
                };

                self.functions.insert(name.lexeme.to_string(), stmt.clone());

                self.environment.borrow_mut().define(&name.lexeme, function);

                Ok(())
            }

            Stmt::Expression(expr) => {
                self.evaluate(expr)?;

                Ok(())
            }

            Stmt::Print(expr) => {
                let value: Value = self.evaluate(expr)?;

                println!("{}", value);

                Ok(())
            }

            Stmt::Var(name, initializer) => {
                let value: Value = if let Some(expr) = initializer {
                    self.evaluate(expr)?
                } else {
                    Value::Nil
                };

                self.environment.borrow_mut().define(&name.lexeme, value);

                Ok(())
            }

            Stmt::Assign(name, expr) => {
                let value: Value = self.evaluate(expr)?;

                self.environment
                    .borrow_mut()
                    .assign(&name.lexeme, value, name.line)?;

                Ok(())
            }

            Stmt::Block(statements) => {
                let previous: Rc<RefCell<Environment>> = self.environment.clone();

                self.environment =
                    Rc::new(RefCell::new(Environment::with_enclosing(previous.clone())));

                for stmt in statements {
                    self.execute(stmt)?;
                }

                self.environment = previous;

                Ok(())
            }

            Stmt::If(condition, then_branch, else_branch) => {
                if is_truthy(&self.evaluate(condition)?) {
                    self.execute(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?;
                }

                Ok(())
            }

            Stmt::While(condition, body) => {
                while is_truthy(&self.evaluate(condition)?) {
                    self.execute(body)?;
                }

                Ok(())
            }

            Stmt::For(initializer, condition, increment, body) => {
                let previous: Rc<RefCell<Environment>> = self.environment.clone();

                self.environment =
                    Rc::new(RefCell::new(Environment::with_enclosing(previous.clone())));

                if let Some(init) = initializer {
                    self.execute(init)?;
                }

                while is_truthy(
                    &condition
                        .as_ref()
                        .map_or(Ok(Value::Bool(true)), |c| self.evaluate(c))?,
                ) {
                    self.execute(body)?;

                    if let Some(incr) = increment {
                        self.evaluate(incr)?;
                    }
                }

                self.environment = previous;

                Ok(())
            }
        }
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Literal(token) => self.evaluate_literal(token),

            Expr::Unary(op, expr) => self.evaluate_unary(op, expr),

            Expr::Binary(left, op, right) => self.evaluate_binary(left, op, right),

            Expr::Grouping(expr) => self.evaluate(expr),

            Expr::Variable(token) => self.evaluate_variable(token),

            Expr::Assign(name, expr) => {
                let value = self.evaluate(expr)?;

                self.environment
                    .borrow_mut()
                    .assign(&name.lexeme, value.clone(), name.line)?;

                Ok(value)
            }

            Expr::Call(callee, paren, arguments) => {
                let callee_val = self.evaluate(callee)?;
                let mut arg_values = Vec::new();

                for arg in arguments {
                    arg_values.push(self.evaluate(arg)?);
                }

                match callee_val {
                    Value::NativeFunction { func, arity, .. } => {
                        if arguments.len() != arity {
                            return Err(format!(
                                "Expected {} arguments but got {} at line {}",
                                arity,
                                arguments.len(),
                                paren.line
                            ));
                        }
                        func(&arg_values)
                    }

                    Value::Function { name, arity } => {
                        if arguments.len() != arity {
                            return Err(format!(
                                "Expected {} arguments but got {} at line {}",
                                arity,
                                arguments.len(),
                                paren.line
                            ));
                        }

                        let function: &Stmt = self.functions.get(&name).ok_or_else(|| {
                            format!("Undefined function '{}' at line {}", name, paren.line)
                        })?;

                        let Stmt::Function(_, _, body) = function else {
                            return Err(format!("Invalid function '{}'", name));
                        };

                        let previous: Rc<RefCell<Environment>> = self.environment.clone();

                        self.environment =
                            Rc::new(RefCell::new(Environment::with_enclosing(previous.clone())));

                        let result: Result<(), String> = self.execute(&body.clone());

                        self.environment = previous;

                        result?;

                        Ok(Value::Nil)
                    }

                    _ => Err(format!("Can only call functions at line {}", paren.line)),
                }
            }
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

    fn evaluate_unary(&mut self, op: &Token, expr: &Expr) -> Result<Value, String> {
        let value = self.evaluate(expr)?;

        match op.token_type {
            TokenType::MINUS => match value {
                Value::Number(n) => Ok(Value::Number(-n)),

                _ => Err(format!("Operand must be a number. [line {}]", op.line)),
            },

            TokenType::BANG => Ok(Value::Bool(!is_truthy(&value))),

            _ => Err(format!("Invalid unary operator on line {}", op.line)),
        }
    }

    fn evaluate_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value, String> {
        match op.token_type {
            TokenType::OR => {
                let left_val: Value = self.evaluate(left)?;

                if is_truthy(&left_val) {
                    Ok(left_val)
                } else {
                    self.evaluate(right)
                }
            }
            TokenType::AND => {
                let left_val = self.evaluate(left)?;

                if !is_truthy(&left_val) {
                    Ok(left_val)
                } else {
                    self.evaluate(right)
                }
            }

            _ => {
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
    }

    fn evaluate_variable(&self, token: &Token) -> Result<Value, String> {
        self.environment.borrow().get(&token.lexeme, token.line)
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

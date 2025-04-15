use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, SystemTimeError, UNIX_EPOCH};

use log::{debug, info, warn};

use crate::environment::Environment;
use crate::expr::Expr;
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};
use crate::value::Value;

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    functions: HashMap<String, Stmt>,
}

impl Interpreter {
    pub fn new() -> Self {
        info!("Initializing Interpreter");
        let environment: Rc<RefCell<Environment>> = Rc::new(RefCell::new(Environment::new()));

        debug!("Defining native function 'clock'");
        environment.borrow_mut().define(
            "clock",
            Value::NativeFunction {
                name: "clock".to_string(),
                arity: 0,
                func: |_args: &[Value]| {
                    debug!("Calling native function 'clock'");
                    let timestamp: f64 = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .map_err(|e: SystemTimeError| format!("Clock error: {}", e))?
                        .as_secs_f64();
                    info!("Clock returned: {}", timestamp);
                    Ok(Value::Number(timestamp))
                },
            },
        );

        Self {
            environment,
            functions: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), String> {
        debug!("Interpreting {} statements", statements.len());
        for stmt in statements {
            debug!("Executing statement: {:?}", stmt);
            self.execute(stmt)?;
        }
        info!("Interpretation completed successfully");
        Ok(())
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Function(name, parameters, _body) => {
                debug!("Defining function '{}'", name.lexeme);
                let function: Value = Value::Function {
                    name: name.lexeme.to_string(),
                    arity: parameters.len(),
                };
                self.functions.insert(name.lexeme.to_string(), stmt.clone());
                self.environment.borrow_mut().define(&name.lexeme, function);
                info!(
                    "Function '{}' defined with {} parameters",
                    name.lexeme,
                    parameters.len()
                );
                Ok(())
            }
            Stmt::Expression(expr) => {
                debug!("Evaluating expression statement");
                self.evaluate(expr)?;
                info!("Expression statement executed");
                Ok(())
            }
            Stmt::Print(expr) => {
                debug!("Evaluating print statement");
                let value: Value = self.evaluate(expr)?;
                println!("{}", value);
                info!("Printed value: {}", value);
                Ok(())
            }
            Stmt::Var(name, initializer) => {
                debug!("Defining variable '{}'", name.lexeme);
                let value: Value = if let Some(expr) = initializer {
                    let val = self.evaluate(expr)?;
                    debug!("Initializer evaluated to: {}", val);
                    val
                } else {
                    debug!("No initializer, using Nil");
                    Value::Nil
                };
                self.environment
                    .borrow_mut()
                    .define(&name.lexeme, value.clone());
                info!("Variable '{}' defined with value: {}", name.lexeme, value);
                Ok(())
            }
            Stmt::Assign(name, expr) => {
                debug!("Assigning to variable '{}'", name.lexeme);
                let value: Value = self.evaluate(expr)?;
                self.environment
                    .borrow_mut()
                    .assign(&name.lexeme, value.clone(), name.line)?;
                info!("Assigned value {} to '{}'", value, name.lexeme);
                Ok(())
            }
            Stmt::Block(statements) => {
                debug!("Entering block with {} statements", statements.len());
                let previous: Rc<RefCell<Environment>> = self.environment.clone();
                self.environment =
                    Rc::new(RefCell::new(Environment::with_enclosing(previous.clone())));
                for stmt in statements {
                    self.execute(stmt)?;
                }
                self.environment = previous;
                info!("Exited block");
                Ok(())
            }
            Stmt::If(condition, then_branch, else_branch) => {
                debug!("Evaluating if condition");
                let cond_value = self.evaluate(condition)?;
                if is_truthy(&cond_value) {
                    debug!("Condition is truthy, executing then branch");
                    self.execute(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    debug!("Condition is falsy, executing else branch");
                    self.execute(else_branch)?;
                } else {
                    debug!("Condition is falsy, no else branch");
                }
                info!("If statement executed");
                Ok(())
            }
            Stmt::While(condition, body) => {
                debug!("Entering while loop");
                while is_truthy(&self.evaluate(condition)?) {
                    debug!("While condition is truthy, executing body");
                    self.execute(body)?;
                }
                info!("Exited while loop");
                Ok(())
            }
            Stmt::For(initializer, condition, increment, body) => {
                debug!("Entering for loop");
                let previous: Rc<RefCell<Environment>> = self.environment.clone();
                self.environment =
                    Rc::new(RefCell::new(Environment::with_enclosing(previous.clone())));
                if let Some(init) = initializer {
                    debug!("Executing for initializer");
                    self.execute(init)?;
                }
                while is_truthy(
                    &condition
                        .as_ref()
                        .map_or(Ok(Value::Bool(true)), |c| self.evaluate(c))?,
                ) {
                    debug!("For condition is truthy, executing body");
                    self.execute(body)?;
                    if let Some(incr) = increment {
                        debug!("Evaluating for increment");
                        self.evaluate(incr)?;
                    }
                }
                self.environment = previous;
                info!("Exited for loop");
                Ok(())
            }
        }
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, String> {
        debug!("Evaluating expression: {:?}", expr);
        let result = match expr {
            Expr::Literal(token) => self.evaluate_literal(token),
            Expr::Unary(op, expr) => self.evaluate_unary(op, expr),
            Expr::Binary(left, op, right) => self.evaluate_binary(left, op, right),
            Expr::Grouping(expr) => self.evaluate(expr),
            Expr::Variable(token) => self.evaluate_variable(token),
            Expr::Assign(name, expr) => {
                let value = self.evaluate(expr)?;
                debug!("Assigning {} to '{}'", value, name.lexeme);
                self.environment
                    .borrow_mut()
                    .assign(&name.lexeme, value.clone(), name.line)?;
                Ok(value)
            }
            Expr::Call(callee, paren, arguments) => {
                debug!("Evaluating function call");
                let callee_val = self.evaluate(callee)?;
                let mut arg_values = Vec::new();
                for arg in arguments {
                    let arg_val = self.evaluate(arg)?;
                    debug!("Evaluated argument: {}", arg_val);
                    arg_values.push(arg_val);
                }
                match callee_val {
                    Value::NativeFunction { name, func, arity } => {
                        debug!("Calling native function '{}'", name);
                        if arguments.len() != arity {
                            warn!(
                                "Arity mismatch for '{}': expected {}, got {}",
                                name,
                                arity,
                                arguments.len()
                            );
                            return Err(format!(
                                "Expected {} arguments but got {} at line {}",
                                arity,
                                arguments.len(),
                                paren.line
                            ));
                        }
                        let result = func(&arg_values);
                        info!("Native function '{}' returned: {:?}", name, result);
                        result
                    }
                    Value::Function { name, arity } => {
                        debug!("Calling user function '{}'", name);
                        if arguments.len() != arity {
                            warn!(
                                "Arity mismatch for '{}': expected {}, got {}",
                                name,
                                arity,
                                arguments.len()
                            );
                            return Err(format!(
                                "Expected {} arguments but got {} at line {}",
                                arity,
                                arguments.len(),
                                paren.line
                            ));
                        }
                        let function: &Stmt = self.functions.get(&name).ok_or_else(|| {
                            warn!("Undefined function '{}'", name);
                            format!("Undefined function '{}' at line {}", name, paren.line)
                        })?;
                        let Stmt::Function(_, params, body) = function else {
                            warn!("Invalid function '{}'", name);
                            return Err(format!("Invalid function '{}'", name));
                        };
                        if params.len() != arg_values.len() {
                            warn!(
                                "Parameter mismatch for '{}': expected {}, got {}",
                                name,
                                params.len(),
                                arg_values.len()
                            );
                            return Err(format!(
                                "Function '{}' expected {} arguments but got {} at line {}",
                                name,
                                params.len(),
                                arg_values.len(),
                                paren.line
                            ));
                        }
                        let previous: Rc<RefCell<Environment>> = self.environment.clone();
                        self.environment =
                            Rc::new(RefCell::new(Environment::with_enclosing(previous.clone())));
                        for (param, arg) in params.iter().zip(arg_values.iter()) {
                            debug!("Binding parameter '{}' to value {}", param.lexeme, arg);
                            self.environment
                                .borrow_mut()
                                .define(&param.lexeme, arg.clone());
                        }
                        debug!("Executing function body");
                        let result: Result<(), String> = self.execute(&body.clone());
                        self.environment = previous;
                        result?;
                        info!("Function '{}' returned Nil", name);
                        Ok(Value::Nil)
                    }
                    _ => {
                        warn!("Attempted to call non-function at line {}", paren.line);
                        Err(format!("Can only call functions at line {}", paren.line))
                    }
                }
            }
        };
        debug!("Expression evaluated to: {:?}", result);
        result
    }

    fn evaluate_literal(&self, token: &Token) -> Result<Value, String> {
        debug!("Evaluating literal: {}", token.lexeme);
        let result = match &token.token_type {
            TokenType::NUMBER(n) => Ok(Value::Number(*n)),
            TokenType::STRING(s) => Ok(Value::String(s.clone())),
            TokenType::TRUE => Ok(Value::Bool(true)),
            TokenType::FALSE => Ok(Value::Bool(false)),
            TokenType::NIL => Ok(Value::Nil),
            _ => {
                warn!("Invalid literal: {}", token.lexeme);
                Err(format!("Invalid literal on line {}", token.line))
            }
        };
        debug!("Literal evaluated to: {:?}", result);
        result
    }

    fn evaluate_unary(&mut self, op: &Token, expr: &Expr) -> Result<Value, String> {
        debug!("Evaluating unary operation: {}", op.lexeme);
        let value = self.evaluate(expr)?;
        let result = match op.token_type {
            TokenType::MINUS => match value {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => {
                    warn!("Non-number operand for unary minus");
                    Err(format!("Operand must be a number. [line {}]", op.line))
                }
            },
            TokenType::BANG => Ok(Value::Bool(!is_truthy(&value))),
            _ => {
                warn!("Invalid unary operator: {}", op.lexeme);
                Err(format!("Invalid unary operator on line {}", op.line))
            }
        };
        debug!("Unary operation evaluated to: {:?}", result);
        result
    }

    fn evaluate_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value, String> {
        debug!("Evaluating binary operation: {}", op.lexeme);
        let result = match op.token_type {
            TokenType::OR => {
                let left_val: Value = self.evaluate(left)?;
                debug!("Left operand: {}", left_val);
                if is_truthy(&left_val) {
                    debug!("OR short-circuited with truthy left");
                    Ok(left_val)
                } else {
                    self.evaluate(right)
                }
            }
            TokenType::AND => {
                let left_val = self.evaluate(left)?;
                debug!("Left operand: {}", left_val);
                if !is_truthy(&left_val) {
                    debug!("AND short-circuited with falsy left");
                    Ok(left_val)
                } else {
                    self.evaluate(right)
                }
            }
            _ => {
                let left_val: Value = self.evaluate(left)?;
                let right_val: Value = self.evaluate(right)?;
                debug!("Operands: left={}, right={}", left_val, right_val);
                match op.token_type {
                    TokenType::PLUS => match (left_val, right_val) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                        (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                        _ => {
                            warn!("Invalid operands for +");
                            Err(format!(
                                "Operands must be two numbers or two strings on line {}",
                                op.line
                            ))
                        }
                    },
                    TokenType::MINUS => match (left_val, right_val) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                        _ => {
                            warn!("Invalid operands for -");
                            Err(format!("Operands must be numbers on line {}", op.line))
                        }
                    },
                    TokenType::STAR => match (left_val, right_val) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                        _ => {
                            warn!("Invalid operands for *");
                            Err(format!("Operands must be numbers on line {}", op.line))
                        }
                    },
                    TokenType::SLASH => match (left_val, right_val) {
                        (Value::Number(a), Value::Number(b)) => {
                            if b == 0.0 {
                                warn!("Division by zero");
                                Err(format!("Division by zero on line {}", op.line))
                            } else {
                                Ok(Value::Number(a / b))
                            }
                        }
                        _ => {
                            warn!("Invalid operands for /");
                            Err(format!("Operands must be numbers on line {}", op.line))
                        }
                    },
                    TokenType::EQUAL_EQUAL => Ok(Value::Bool(is_equal(&left_val, &right_val))),
                    TokenType::BANG_EQUAL => Ok(Value::Bool(!is_equal(&left_val, &right_val))),
                    TokenType::LESS => match (left_val, right_val) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a < b)),
                        _ => {
                            warn!("Invalid operands for <");
                            Err(format!("Operands must be numbers on line {}", op.line))
                        }
                    },
                    TokenType::LESS_EQUAL => match (left_val, right_val) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a <= b)),
                        _ => {
                            warn!("Invalid operands for <=");
                            Err(format!("Operands must be numbers on line {}", op.line))
                        }
                    },
                    TokenType::GREATER => match (left_val, right_val) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a > b)),
                        _ => {
                            warn!("Invalid operands for >");
                            Err(format!("Operands must be numbers on line {}", op.line))
                        }
                    },
                    TokenType::GREATER_EQUAL => match (left_val, right_val) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a >= b)),
                        _ => {
                            warn!("Invalid operands for >=");
                            Err(format!("Operands must be numbers on line {}", op.line))
                        }
                    },
                    _ => {
                        warn!("Invalid binary operator: {}", op.lexeme);
                        Err(format!("Invalid binary operator on line {}", op.line))
                    }
                }
            }
        };
        debug!("Binary operation evaluated to: {:?}", result);
        result
    }

    fn evaluate_variable(&self, token: &Token) -> Result<Value, String> {
        debug!("Looking up variable '{}'", token.lexeme);
        let result = self.environment.borrow().get(&token.lexeme, token.line);
        debug!("Variable '{}' evaluated to: {:?}", token.lexeme, result);
        result
    }
}

fn is_truthy(value: &Value) -> bool {
    debug!("Checking truthiness of: {}", value);
    let result = match value {
        Value::Nil => false,
        Value::Bool(b) => *b,
        _ => true,
    };
    debug!("Truthiness result: {}", result);
    result
}

fn is_equal(left: &Value, right: &Value) -> bool {
    debug!("Checking equality: {} == {}", left, right);
    let result = match (left, right) {
        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    };
    debug!("Equality result: {}", result);
    result
}

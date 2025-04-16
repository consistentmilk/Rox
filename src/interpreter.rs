use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, SystemTimeError, UNIX_EPOCH};

use log::{debug, info};
use thiserror::Error; // for custom errors

use crate::environment::Environment;
use crate::expr::Expr;
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};
use crate::value::Value;

#[derive(Error, Debug)]
pub enum InterpretError {
    #[error("Runtime error: {0}")]
    RuntimeError(String),

    #[error("Return signal with value: {0}")]
    ReturnSignal(Value),
}

/// Convenient alias for interpreter results.
pub type IResult<T> = Result<T, InterpretError>;

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    functions: HashMap<String, Stmt>,
}

impl Interpreter {
    /// Creates a new Interpreter and defines native functions such as `clock`.
    pub fn new() -> Self {
        info!("Initializing Interpreter");

        let environment = Rc::new(RefCell::new(Environment::new()));

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
                    info!("Native function 'clock' returned: {}", timestamp);
                    Ok(Value::Number(timestamp))
                },
            },
        );

        Self {
            environment,
            functions: HashMap::new(),
        }
    }

    /// Interprets a list of statements (a "program").
    pub fn interpret(&mut self, statements: &[Stmt]) -> IResult<()> {
        debug!("Interpreting {} statements", statements.len());
        for stmt in statements {
            debug!("Executing statement: {:?}", stmt);
            self.execute(stmt)?;
        }
        info!("Interpretation completed successfully");
        Ok(())
    }

    /// Executes a single statement.
    pub fn execute(&mut self, stmt: &Stmt) -> IResult<()> {
        match stmt {
            Stmt::Function(name, parameters, _body) => {
                debug!("Defining function '{}'", name.lexeme);
                // When defining a function, capture the current environment as the closure.
                let function_value = Value::Function {
                    name: name.lexeme.to_string(),
                    arity: parameters.len(),
                    closure: self.environment.clone(),
                };
                self.functions.insert(name.lexeme.to_string(), stmt.clone());
                self.environment
                    .borrow_mut()
                    .define(&name.lexeme, function_value);
                info!(
                    "Function '{}' defined with {} parameters",
                    name.lexeme,
                    parameters.len()
                );
                Ok(())
            }

            Stmt::Expression(expr) => {
                debug!("Evaluating expression statement");
                let _ = self.evaluate(expr)?;
                info!("Expression statement executed");
                Ok(())
            }

            Stmt::Print(expr) => {
                debug!("Evaluating print statement");
                let value = self.evaluate(expr)?;
                println!("{}", value);
                info!("Printed value: {}", value);
                Ok(())
            }

            Stmt::Var(name, initializer) => {
                debug!("Defining variable '{}'", name.lexeme);
                let value = if let Some(expr) = initializer {
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
                let value = self.evaluate(expr)?;
                self.environment
                    .borrow_mut()
                    .assign(&name.lexeme, value.clone(), name.line)
                    .map_err(InterpretError::RuntimeError)?;
                info!("Assigned value {} to '{}'", value, name.lexeme);
                Ok(())
            }

            Stmt::Block(statements, _line) => {
                debug!("Entering block with {} statements", statements.len());
                let previous = self.environment.clone();
                self.environment = Rc::new(RefCell::new(Environment::with_enclosing(previous)));
                for stmt in statements {
                    self.execute(stmt)?;
                }
                // Restore old environment.
                let tmp = self
                    .environment
                    .borrow()
                    .enclosing
                    .as_ref()
                    .expect("No enclosing environment?")
                    .clone();
                self.environment = tmp;
                info!("Exited block");
                Ok(())
            }

            Stmt::If(condition, then_branch, else_branch) => {
                debug!("Evaluating if condition");
                let cond_value = self.evaluate(condition)?;
                if is_truthy(&cond_value) {
                    debug!("Condition is truthy; executing then branch");
                    self.execute(then_branch)?;
                } else if let Some(else_stmt) = else_branch {
                    debug!("Condition is falsy; executing else branch");
                    self.execute(else_stmt)?;
                }
                info!("If statement executed");
                Ok(())
            }

            Stmt::While(condition, body) => {
                debug!("Entering while loop");
                while is_truthy(&self.evaluate(condition)?) {
                    debug!("While condition is truthy; executing body");
                    self.execute(body)?;
                }
                info!("Exited while loop");
                Ok(())
            }

            Stmt::For(_line, initializer, condition, increment, body) => {
                debug!("Entering for loop");
                let previous_env = self.environment.clone();
                self.environment = Rc::new(RefCell::new(Environment::with_enclosing(previous_env)));
                if let Some(init) = initializer {
                    debug!("Executing for initializer");
                    self.execute(init)?;
                }
                while is_truthy(
                    &condition
                        .as_ref()
                        .map_or(Ok(Value::Bool(true)), |c| self.evaluate(c))?,
                ) {
                    debug!("For condition is truthy; executing body");
                    self.execute(body)?;
                    if let Some(incr) = increment {
                        debug!("Evaluating for increment");
                        self.evaluate(incr)?;
                    }
                }
                let tmp = self
                    .environment
                    .borrow()
                    .enclosing
                    .as_ref()
                    .expect("No enclosing environment?")
                    .clone();
                self.environment = tmp;
                info!("Exited for loop");
                Ok(())
            }

            Stmt::Return(_keyword, expr) => {
                debug!("Executing return statement");
                let value = match expr {
                    Some(e) => self.evaluate(e)?,
                    None => Value::Nil,
                };
                debug!("Returning value: {}", value);
                Err(InterpretError::ReturnSignal(value))
            }
        }
    }

    /// Evaluates an expression and returns a Value.
    pub fn evaluate(&mut self, expr: &Expr) -> IResult<Value> {
        debug!("Evaluating expression: {:?}", expr);
        let value = match expr {
            Expr::Literal(token) => self.evaluate_literal(token)?,
            Expr::Unary(op, right) => self.evaluate_unary(op, right)?,
            Expr::Binary(left, op, right) => self.evaluate_binary(left, op, right)?,
            Expr::Grouping(e) => self.evaluate(e)?,
            Expr::Variable(t) => self.evaluate_variable(t)?,
            Expr::Assign(name, rhs_expr) => {
                let val = self.evaluate(rhs_expr)?;
                self.environment
                    .borrow_mut()
                    .assign(&name.lexeme, val.clone(), name.line)
                    .map_err(InterpretError::RuntimeError)?;
                val
            }
            Expr::Call(callee_expr, paren_token, arguments) => {
                debug!("Evaluating function call");
                let callee_val = self.evaluate(callee_expr)?;
                let mut arg_values = Vec::with_capacity(arguments.len());
                for arg in arguments {
                    let av = self.evaluate(arg)?;
                    debug!("Evaluated argument => {}", av);
                    arg_values.push(av);
                }
                self.invoke_callable(&callee_val, paren_token, &arg_values)?
            }
        };
        debug!("Expression evaluated to: {}", value);
        Ok(value)
    }

    /// Evaluates a literal token.
    fn evaluate_literal(&self, token: &Token) -> IResult<Value> {
        debug!("Evaluating literal token: {}", token.lexeme);
        let val = match &token.token_type {
            TokenType::NUMBER(n) => Value::Number(*n),
            TokenType::STRING(s) => Value::String(s.clone()),
            TokenType::TRUE => Value::Bool(true),
            TokenType::FALSE => Value::Bool(false),
            TokenType::NIL => Value::Nil,
            _ => {
                let msg = format!("Invalid literal on line {}", token.line);
                debug!("Error: {}", msg);
                return Err(InterpretError::RuntimeError(msg));
            }
        };
        Ok(val)
    }

    /// Evaluates a unary expression.
    fn evaluate_unary(&mut self, op: &Token, expr: &Expr) -> IResult<Value> {
        debug!("Evaluating unary operation: {}", op.lexeme);
        let right_val = self.evaluate(expr)?;
        let result = match op.token_type {
            TokenType::MINUS => {
                if let Value::Number(n) = right_val {
                    Value::Number(-n)
                } else {
                    let msg = format!("Operand must be a number. [line {}]", op.line);
                    debug!("Error: {}", msg);
                    return Err(InterpretError::RuntimeError(msg));
                }
            }
            TokenType::BANG => Value::Bool(!is_truthy(&right_val)),
            _ => {
                let msg = format!("Invalid unary operator on line {}", op.line);
                debug!("Error: {}", msg);
                return Err(InterpretError::RuntimeError(msg));
            }
        };
        Ok(result)
    }

    /// Evaluates a binary expression.
    fn evaluate_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> IResult<Value> {
        debug!("Evaluating binary operation: {}", op.lexeme);
        if op.token_type == TokenType::OR {
            let left_val = self.evaluate(left)?;
            if is_truthy(&left_val) {
                return Ok(left_val);
            }
            return self.evaluate(right);
        }
        if op.token_type == TokenType::AND {
            let left_val = self.evaluate(left)?;
            if !is_truthy(&left_val) {
                return Ok(left_val);
            }
            return self.evaluate(right);
        }
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?;
        debug!("Left operand: {}, Right operand: {}", left_val, right_val);
        match op.token_type {
            TokenType::PLUS => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                _ => {
                    let msg = format!(
                        "Operands must be two numbers or two strings on line {}",
                        op.line
                    );
                    debug!("Error: {}", msg);
                    Err(InterpretError::RuntimeError(msg))
                }
            },
            TokenType::MINUS => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                _ => {
                    let msg = format!("Operands must be numbers on line {}", op.line);
                    debug!("Error: {}", msg);
                    Err(InterpretError::RuntimeError(msg))
                }
            },
            TokenType::STAR => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                _ => {
                    let msg = format!("Operands must be numbers on line {}", op.line);
                    Err(InterpretError::RuntimeError(msg))
                }
            },
            TokenType::SLASH => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => {
                    if b == 0.0 {
                        let msg = format!("Division by zero on line {}", op.line);
                        debug!("Error: {}", msg);
                        Err(InterpretError::RuntimeError(msg))
                    } else {
                        Ok(Value::Number(a / b))
                    }
                }
                _ => {
                    let msg = format!("Operands must be numbers on line {}", op.line);
                    debug!("Error: {}", msg);
                    Err(InterpretError::RuntimeError(msg))
                }
            },
            TokenType::EQUAL_EQUAL => Ok(Value::Bool(is_equal(&left_val, &right_val))),
            TokenType::BANG_EQUAL => Ok(Value::Bool(!is_equal(&left_val, &right_val))),
            TokenType::LESS => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a < b)),
                _ => {
                    let msg = format!("Operands must be numbers on line {}", op.line);
                    Err(InterpretError::RuntimeError(msg))
                }
            },
            TokenType::LESS_EQUAL => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a <= b)),
                _ => {
                    let msg = format!("Operands must be numbers on line {}", op.line);
                    Err(InterpretError::RuntimeError(msg))
                }
            },
            TokenType::GREATER => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a > b)),
                _ => {
                    let msg = format!("Operands must be numbers on line {}", op.line);
                    Err(InterpretError::RuntimeError(msg))
                }
            },
            TokenType::GREATER_EQUAL => match (left_val, right_val) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a >= b)),
                _ => {
                    let msg = format!("Operands must be numbers on line {}", op.line);
                    Err(InterpretError::RuntimeError(msg))
                }
            },
            _ => {
                let msg = format!("Invalid binary operator on line {}", op.line);
                debug!("Error: {}", msg);
                Err(InterpretError::RuntimeError(msg))
            }
        }
    }

    /// Evaluates a variable.
    fn evaluate_variable(&self, token: &Token) -> IResult<Value> {
        debug!("Looking up variable '{}'", token.lexeme);
        let val = self
            .environment
            .borrow()
            .get(&token.lexeme, token.line)
            .map_err(InterpretError::RuntimeError)?;
        debug!("Variable '{}' evaluated to: {}", token.lexeme, val);
        Ok(val)
    }

    /// Invokes a callable (native or user-defined function).
    fn invoke_callable(
        &mut self,
        callee_val: &Value,
        paren_token: &Token,
        arg_values: &[Value],
    ) -> IResult<Value> {
        match callee_val {
            Value::NativeFunction { name, arity, func } => {
                debug!("Calling native function '{}'", name);
                if arg_values.len() != *arity {
                    let msg = format!(
                        "Expected {} arguments but got {} at line {}",
                        arity,
                        arg_values.len(),
                        paren_token.line
                    );
                    debug!("Error: {}", msg);
                    return Err(InterpretError::RuntimeError(msg));
                }
                let result = func(arg_values).map_err(InterpretError::RuntimeError)?;
                info!("Native function '{}' returned: {}", name, result);
                Ok(result)
            }

            // Updated to handle closures:
            Value::Function {
                name,
                arity,
                closure,
            } => {
                debug!("Calling user-defined function '{}'", name);
                if arg_values.len() != *arity {
                    let msg = format!(
                        "Expected {} arguments but got {} at line {}",
                        arity,
                        arg_values.len(),
                        paren_token.line
                    );
                    debug!("Error: {}", msg);
                    return Err(InterpretError::RuntimeError(msg));
                }
                let function_stmt: &Stmt = self.functions.get(name).ok_or_else(|| {
                    let msg = format!("Undefined function '{}' at line {}", name, paren_token.line);
                    debug!("Error: {}", msg);
                    InterpretError::RuntimeError(msg)
                })?;
                let Stmt::Function(_, params, body) = function_stmt else {
                    let msg = format!("Invalid function '{}'", name);
                    debug!("Error: {}", msg);
                    return Err(InterpretError::RuntimeError(msg));
                };

                // Save the current environment.
                let saved_env: Rc<RefCell<Environment>> = self.environment.clone();
                // Instead of using the current environment, use the function’s closure.
                self.environment =
                    Rc::new(RefCell::new(Environment::with_enclosing(closure.clone())));

                // Bind parameters.
                for (param, arg_val) in params.iter().zip(arg_values.iter()) {
                    debug!("Binding parameter '{}' to {}", param.lexeme, arg_val);
                    self.environment
                        .borrow_mut()
                        .define(&param.lexeme, arg_val.clone());
                }

                debug!("Executing function body");
                let result = self.execute(&body.clone());
                // Restore the saved environment.
                self.environment = saved_env;

                match result {
                    Ok(()) => {
                        info!("Function '{}' returned Nil", name);
                        Ok(Value::Nil)
                    }
                    Err(InterpretError::ReturnSignal(val)) => {
                        info!("Function '{}' returned: {}", name, val);
                        Ok(val)
                    }
                    Err(InterpretError::RuntimeError(e)) => Err(InterpretError::RuntimeError(e)),
                }
            }

            _ => {
                let msg = format!("Can only call functions at line {}", paren_token.line);
                debug!("Error: {}", msg);
                Err(InterpretError::RuntimeError(msg))
            }
        }
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
    match (left, right) {
        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}

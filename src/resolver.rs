//! Static resolution pass for the Lox interpreter.
//!
//! This module performs a single AST walk to:
//! 1. **Build lexical scopes**: maintains a stack of `HashMap<&str,bool>` tracking declared
//!    (false) and fully defined (true) names in each nested block or function.
//! 2. **Enforce static rules**: reports errors such as redeclaration in the same scope,
//!    reading a variable in its own initializer, invalid `return` outside functions,
//!    and illegal use of `this` outside of class methods.
//! 3. **Record binding distances**: for every variable occurrence (`Expr::Variable` or
//!    `Expr::Assign`), calls back into the interpreter to note whether it is a local
//!    (and at what depth) or a global. This enables the runtime to perform O(1)
//!    lookups by climbing exactly the right number of environment frames.
//!
//! # Workflow Overview
//!
//! 1. **Instantiation** (`Resolver::new`)
//!    - Captures a mutable reference to the `Interpreter`, where binding distances will be recorded.
//!    - Initializes empty scope stack and function/class context flags.
//!
//! 2. **Resolution Entry Point** (`resolve(&[Stmt])`)
//!    - Walks each top‑level statement via `resolve_stmt`, propagating errors.
//!
//! 3. **Statement Resolution** (`resolve_stmt`)
//!    - Declares and defines names for `var`, `fun`, and `class` declarations.
//!    - Handles nested scopes for blocks (`{ … }`), `for`, `if`, `while` statements.
//!    - Manages `return` validity depending on whether inside a function or initializer.
//!    - Injects `this` in class method scopes and enforces `super`/`this` rules.
//!
//! 4. **Expression Resolution** (`resolve_expr`)
//!    - Recursively descends into expression nodes (`Literal`, `Grouping`, `Unary`, `Binary`, `Logical`, `Call`, `Get`, `Set`, `This`).
//!    - For variable reads and assignments, ensures no forward-read in initializers and calls `resolve_local`.
//!
//! 5. **Error Recovery**
//!    - No in-place recovery: resolution halts on the first static error, returning a `LoxError::Resolve`.
//!
//! # Usage
//!
//! After parsing, before interpretation, invoke:
//! ```rust
//! let mut resolver = Resolver::new(&mut interpreter);
//! resolver.resolve(&ast)?;
//! ```
//! This ensures all variables, functions, and classes are properly bound, and enables fast
//! environment lookups during execution.

use crate::error::{LoxError, Result};
use crate::interpreter::Interpreter;
use crate::parser::{Expr, Stmt};
use crate::token::Token;
use log::{debug, info};
use std::collections::HashMap;

/// Are we inside a user function?  Used to validate `return`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
    Initializer,
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum ClassType {
    None,
    Class,
}

/// Resolver: tracks scopes, enforces static rules, and *records* binding
/// distances (locals vs. globals) by calling back into the interpreter.
pub struct Resolver<'a, 'interp> {
    interpreter: &'interp mut Interpreter<'a>,
    scopes: Vec<HashMap<&'a str, bool>>, // false=declared, true=defined
    current_function: FunctionType,
    current_class: ClassType,
}

impl<'a, 'interp> Resolver<'a, 'interp> {
    /// Create a new resolver bound to the given interpreter.
    pub fn new(interpreter: &'interp mut Interpreter<'a>) -> Self {
        info!("Resolver instantiated");
        Resolver {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    /// Walk all top‑level statements.
    pub fn resolve(&mut self, statements: &[Stmt<'a>]) -> Result<()> {
        info!(
            "Beginning resolve pass over {} statement(s)",
            statements.len()
        );
        for stmt in statements {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Statement resolution
    // ─────────────────────────────────────────────────────────────────────────

    fn resolve_stmt(&mut self, stmt: &Stmt<'a>) -> Result<()> {
        debug!("Resolving stmt: {:?}", stmt);

        match stmt {
            Stmt::Class {
                name,
                methods,
                superclass,
            } => {
                // Self inheritance check
                if let Some(super_tok) = superclass {
                    if super_tok.lexeme == name.lexeme {
                        // A class cannot inherit from itself
                        return Err(LoxError::resolve(
                            super_tok.line,
                            "A class can't inherit from itself.",
                        ));
                    }
                }

                // 1. Declare & define the class name so methods can refer to it
                self.declare(name)?;
                self.define(name);

                // 2. Mark that we are inside a class
                let enclosing_class: ClassType = self.current_class;
                self.current_class = ClassType::Class;

                // 3. For each method, open a 'this' scope, then resolve the method body
                for method in methods.iter() {
                    if let Stmt::Function {
                        name: m_name,
                        params,
                        body,
                    } = method
                    {
                        // 4. Mark enclosing function type
                        let enclosing: FunctionType = self.current_function;

                        self.current_function = if m_name.lexeme == "init" {
                            FunctionType::Initializer
                        } else {
                            FunctionType::Function
                        };

                        // 4. Begin a fresh scope for 'this'
                        self.begin_scope();

                        // 5. Inject 'this' so Expr::This will resolve
                        self.scopes.last_mut().unwrap().insert("this", true);

                        // 6. Declare and define parameters
                        for params in params {
                            self.declare(params)?;
                            self.define(params);
                        }

                        // 7. Resolve the body statements
                        for stmt in body {
                            self.resolve_stmt(stmt)?;
                        }

                        // 6. Pop the 'this' scope
                        self.end_scope();

                        // 7. Restore enclosing function
                        self.current_function = enclosing;
                    }
                }

                // 8. Restore the outer class context
                self.current_class = enclosing_class;
            }

            Stmt::Block(statements) => {
                // 1. Push a new anonymous scope for `{ … }`
                self.begin_scope();

                for s in statements {
                    self.resolve_stmt(s)?;
                }

                // 2. Pop the block scope
                self.end_scope();
            }

            Stmt::Var { name, initializer } => {
                // 1. Declare the variable name (marked but not yet defined)
                self.declare(name)?;

                // 2. Resolve the initializer expression, if any
                if let Some(expr) = initializer {
                    self.resolve_expr(expr)?;
                }

                // 3. Define the variable so it’s available in this scope
                self.define(name);
            }

            Stmt::Function { name, params, body } => {
                // 1. Declare the function name (so it’s visible inside its own body)
                self.declare(name)?;
                // 2. Define it immediately (allow recursion)
                self.define(name);
                // 3. Resolve the function’s parameters and body
                self.resolve_function(params, body)?;
            }

            Stmt::Expression(expr) | Stmt::Print(expr) => {
                // 1. Resolve the inner expression of expression/print statements
                self.resolve_expr(expr)?;
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // 1. Resolve the condition expression
                self.resolve_expr(condition)?;

                // 2. Resolve the 'then' branch
                self.resolve_stmt(then_branch)?;

                // 3. Resolve the 'else' branch, if present
                if let Some(eb) = else_branch.as_deref() {
                    self.resolve_stmt(eb)?;
                }
            }

            Stmt::While { condition, body } => {
                // 1. Resolve the loop condition
                self.resolve_expr(condition)?;
                // 2. Resolve the loop body
                self.resolve_stmt(body)?;
            }

            Stmt::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                // 1. Begin outer loop scope for initializer
                self.begin_scope();

                // 2. Handle initializer (either var declaration or statement)
                if let Some(init) = initializer {
                    if let Stmt::Var { name, initializer } = &**init {
                        // 2.1. Declare the loop variable
                        self.declare(name)?;
                        // 2.2. Resolve its initializer
                        if let Some(expr) = initializer {
                            self.resolve_expr(expr)?;
                        }
                        // 2.3. Define the loop variable
                        self.define(name);
                    } else {
                        // 2.4. Resolve the initializer statement
                        self.resolve_stmt(init)?;
                    }
                }

                // 3. Resolve the loop condition, if present
                if let Some(cond) = condition {
                    self.resolve_expr(cond)?;
                }

                // 4. Resolve the increment expression, if present
                if let Some(inc) = increment {
                    self.resolve_expr(inc)?;
                }

                // 5. Begin inner scope for the loop body (allows shadowing)
                self.begin_scope();
                self.resolve_stmt(body)?;

                // 6. Exit inner body scope
                self.end_scope();

                // 7. Exit outer loop scope
                self.end_scope();
            }

            Stmt::Return { keyword, value } => {
                // 1. Ensure we're inside a function or initializer
                if self.current_function == FunctionType::None {
                    return Err(LoxError::resolve(
                        keyword.line,
                        "'return' used outside of function",
                    ));
                }

                // 2. In an initializer, only bare `return;` is allowed
                if self.current_function == FunctionType::Initializer {
                    if value.is_some() {
                        return Err(LoxError::resolve(
                            keyword.line,
                            "Can't return a value from an initializer.",
                        ));
                    }

                    // No expression to resolve for a bare return
                } else {
                    // 3. Normal function: resolve the return expression if present
                    if let Some(expr) = value {
                        self.resolve_expr(expr)?;
                    }
                }
            }
        }

        Ok(())
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Expression resolution
    // ─────────────────────────────────────────────────────────────────────────

    fn resolve_expr(&mut self, expr: &Expr<'a>) -> Result<()> {
        debug!("Resolving expr: {:?}", expr);
        match expr {
            Expr::Literal(_) => {
                // 1. Literals have no sub‑expressions
            }

            Expr::Grouping(inner) => {
                // 2. Resolve the inner expression of a grouping
                self.resolve_expr(inner)?;
            }

            Expr::Unary { right, .. } => {
                // 3. Resolve the operand of a unary expression
                self.resolve_expr(right)?;
            }

            Expr::Binary { left, right, .. } | Expr::Logical { left, right, .. } => {
                // 4. Resolve both sides of binary or logical operators
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }

            Expr::Variable(tok) => {
                // 5. Prevent reading a variable in its own initializer
                if let Some(scope) = self.scopes.last() {
                    if scope.get(tok.lexeme) == Some(&false) {
                        return Err(LoxError::resolve(
                            tok.line,
                            "Cannot read local variable in its own initializer",
                        ));
                    }
                }
                // 6. Bind this variable occurrence at its lexical depth
                self.resolve_local(expr, tok);
            }

            Expr::Assign { name, value } => {
                // 7. Resolve the right‑hand side first, then bind the assignment
                self.resolve_expr(value)?;
                self.resolve_local(expr, name);
            }

            Expr::Call {
                callee, arguments, ..
            } => {
                // 8. Resolve the callee expression and each argument
                self.resolve_expr(callee)?;

                for arg in arguments {
                    self.resolve_expr(arg)?;
                }
            }

            Expr::This(keyword) => {
                // 9. 'this' only valid inside class methods
                if self.current_class == ClassType::None {
                    return Err(LoxError::resolve(
                        keyword.line,
                        "Cannot use 'this' outside of a class",
                    ));
                }

                // 10. Bind 'this' like a local variable
                self.resolve_local(expr, keyword);
            }

            Expr::Get { object, .. } => {
                // 11. Resolve the object whose property is being accessed
                self.resolve_expr(object)?;
            }

            Expr::Set { object, value, .. } => {
                // 12. Resolve the target object then the value being assigned
                self.resolve_expr(object)?;
                self.resolve_expr(value)?;
            }
        }

        Ok(())
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Function helper
    // ─────────────────────────────────────────────────────────────────────────

    /// Enter a fresh scope for a function’s parameters + body.
    fn resolve_function(&mut self, params: &[&'a Token<'a>], body: &[Stmt<'a>]) -> Result<()> {
        // 1. Save the enclosing function context
        let enclosing: FunctionType = self.current_function;

        // 2. Mark that we’re now inside a regular function
        self.current_function = FunctionType::Function;

        // 3. Begin the function’s parameter scope
        self.begin_scope();

        // 4. Declare and define each parameter
        for param in params {
            self.declare(param)?;
            self.define(param);
        }

        // 5. Resolve each statement in the function body
        for stmt in body {
            self.resolve_stmt(stmt)?;
        }

        // 6. End the function’s parameter scope
        self.end_scope();

        // 7. Restore the previous function context
        self.current_function = enclosing;

        Ok(())
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Scope management
    // ─────────────────────────────────────────────────────────────────────────

    #[inline]
    fn begin_scope(&mut self) {
        // 1. Push a new, empty scope map
        self.scopes.push(HashMap::new());
    }

    #[inline]
    fn end_scope(&mut self) {
        // 2. Pop the innermost scope
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token<'a>) -> Result<()> {
        // 1. If in a local scope, ensure no duplicate declarations
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name.lexeme) {
                return Err(LoxError::resolve(
                    name.line,
                    "Variable already declared in this scope",
                ));
            }
            // 2. Mark the name as declared but not yet defined
            scope.insert(name.lexeme, false);
        }
        Ok(())
    }

    fn define(&mut self, name: &Token<'a>) {
        // 1. Mark the name as fully defined in the current scope
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme, true);
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Binding‑distance helper
    // ─────────────────────────────────────────────────────────────────────────

    /// Record this variable occurrence as either:
    ///  - a local at depth `d`, or
    ///  - a global if not found in any scope.
    fn resolve_local(&mut self, expr: &Expr<'a>, name: &Token<'a>) {
        // 1. Search each scope from innermost outward
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name.lexeme) {
                debug!("Resolved '{}' at depth {}", name.lexeme, depth);
                // 2. Tell the interpreter about the local binding
                self.interpreter.note_local(expr, depth);
                return;
            }
        }

        // 3. Not found in any scope → it's a global
        debug!("Resolved '{}' as global", name.lexeme);
        self.interpreter.note_global(expr);
    }
}

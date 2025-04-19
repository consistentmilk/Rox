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
    /// Not inside any class
    None,

    /// Insie a class declaration _without_ a superclass
    Class,

    /// Inside a class declaration _with_ a superclass
    Subclass,
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
                // 1. Self‑inheritance guard
                if let Some(super_tok) = superclass {
                    if super_tok.lexeme == name.lexeme {
                        return Err(LoxError::resolve(
                            super_tok.line,
                            "A class can't inherit from itself.",
                        ));
                    }
                }

                // 2. Declare & define the class name so methods (including init) can refer to it
                self.declare(name)?;
                self.define(name);

                // 3. Save and enter the class context
                let enclosing_class: ClassType = self.current_class;

                // 4. If there's a superclass token, we're in a subclass; otherwise in a plain class
                self.current_class = if superclass.is_some() {
                    ClassType::Subclass
                } else {
                    ClassType::Class
                };

                // 5. If there is a superclass, resolve it and bind `super`
                if let Some(super_tok) = superclass {
                    // Resolve the superclass variable (must exist and be a class)
                    self.resolve_expr(&Expr::Variable(super_tok))?;

                    // Open a scope for `super`
                    self.begin_scope();
                    self.scopes.last_mut().unwrap().insert("super", true);
                }

                // 6. Open the implicit `this` scope for methods
                self.begin_scope();
                self.scopes.last_mut().unwrap().insert("this", true);

                // 7. Resolve each method in its own function context
                for method in methods {
                    if let Stmt::Function {
                        name: m_name,
                        params,
                        body,
                    } = method
                    {
                        // 7a. Declare & define the method name to allow recursion within the class
                        self.declare(m_name)?;
                        self.define(m_name);

                        // 7b. Determine whether this is an initializer or a normal method
                        let kind = if m_name.lexeme == "init" {
                            FunctionType::Initializer
                        } else {
                            FunctionType::Function
                        };

                        // 7c. Resolve the method’s parameters and body under the chosen context
                        self.resolve_function(kind, params, body)?;
                    }
                }

                // 8. Close the `this` scope
                self.end_scope();

                // 9. If we opened a `super` scope, close it now
                if superclass.is_some() {
                    self.end_scope();
                }

                // 10. Restore the outer class context
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

                // 3. Resolve the function’s parameters and body under a normal function context
                self.resolve_function(FunctionType::Function, params, body)?;
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

            Expr::Super { keyword, .. } => {
                // 1. Disallow outside any class
                if self.current_class == ClassType::None {
                    return Err(LoxError::resolve(
                        keyword.line,
                        "Cannot use 'super' outside of a class.",
                    ));
                }

                // 2. Disallow in a class with no superclass
                if self.current_class != ClassType::Subclass {
                    return Err(LoxError::resolve(
                        keyword.line,
                        "Cannot use 'super' in a class with no superclass.",
                    ));
                }

                // 3. Valid. Bind 'super' like a local variable.
                self.resolve_local(expr, keyword);
            }
        }

        Ok(())
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Function helper
    // ─────────────────────────────────────────────────────────────────────────

    /// Enter a fresh scope for a function’s parameters + body.
    ///
    /// `kind` indicates whether this is a normal function or an initializer.
    fn resolve_function(
        &mut self,
        kind: FunctionType,
        params: &[&'a Token<'a>],
        body: &[Stmt<'a>],
    ) -> Result<()> {
        // 1. Save the enclosing function context so we can restore it later.
        let enclosing = self.current_function;

        // 2. Set the current function context to the passed‑in kind
        //    (FunctionType::Function or FunctionType::Initializer).
        self.current_function = kind;

        // 3. Begin a new lexical scope for the function parameters & body.
        self.begin_scope();

        // 4. Declare and immediately define each parameter in this new scope.
        for param in params {
            // 4a. Declare the parameter name (ensuring no duplicate).
            self.declare(param)?;
            // 4b. Mark the parameter as defined so it can be read in its own initializer.
            self.define(param);
        }

        // 5. Resolve each statement in the function body under the current context.
        for stmt in body {
            self.resolve_stmt(stmt)?;
        }

        // 6. End the function’s parameter/body scope, popping all parameter bindings.
        self.end_scope();

        // 7. Restore the previous function context (e.g., back to Initializer or None).
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

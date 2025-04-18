//! Static resolver pass for the **Lox** interpreter.
//!
//! This resolver does three things in one AST walk:
//! 1. Build lexical scopes (stack of `HashMap<&str,bool>` tracking declared/defined).
//! 2. Report static errors (redeclaration, forward‑read in initializer, invalid `return`).
//! 3. Tell the interpreter, for *each* variable occurrence, whether it’s a local
//!    (and at what depth) or a global—so the interpreter never falls back to
//!    dynamic lookup that would see a later shadowing local.

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
            Stmt::Class { name, methods } => {
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
                // ① Push a new anonymous scope for `{ … }`
                self.begin_scope();
                for s in statements {
                    self.resolve_stmt(s)?;
                }
                self.end_scope();
            }

            Stmt::Var { name, initializer } => {
                // ② var declaration: declare → resolve initializer → define
                self.declare(name)?;
                if let Some(expr) = initializer {
                    self.resolve_expr(expr)?;
                }
                self.define(name);
            }

            Stmt::Function { name, params, body } => {
                // ③ function declaration: name is visible *inside* its own body
                self.declare(name)?;
                self.define(name);
                self.resolve_function(params, body)?;
            }

            Stmt::Expression(expr) | Stmt::Print(expr) => {
                // ④ just resolve the inner expression
                self.resolve_expr(expr)?;
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // ⑤ if
                self.resolve_expr(condition)?;
                self.resolve_stmt(then_branch)?;
                if let Some(eb) = else_branch.as_deref() {
                    self.resolve_stmt(eb)?;
                }
            }

            Stmt::While { condition, body } => {
                // ⑥ while
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)?;
            }

            Stmt::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                // ⑦ for—outer loop‐scope for initializer, then inner for body
                self.begin_scope();
                if let Some(init) = initializer {
                    if let Stmt::Var { name, initializer } = &**init {
                        self.declare(name)?;
                        if let Some(expr) = initializer {
                            self.resolve_expr(expr)?;
                        }
                        self.define(name);
                    } else {
                        self.resolve_stmt(init)?;
                    }
                }
                if let Some(cond) = condition {
                    self.resolve_expr(cond)?;
                }
                if let Some(inc) = increment {
                    self.resolve_expr(inc)?;
                }

                // body may shadow loop variables
                self.begin_scope();
                self.resolve_stmt(body)?;
                self.end_scope();

                self.end_scope();
            }

            Stmt::Return { keyword, value } => {
                // Return only allowed in functions/initializers
                if self.current_function == FunctionType::None {
                    return Err(LoxError::resolve(
                        keyword.line,
                        "'return' used outside of function",
                    ));
                }

                // If an initializer, bare 'return;' is oaky, but not 'return expr;'
                if self.current_function == FunctionType::Initializer {
                    if value.is_some() {
                        return Err(LoxError::resolve(
                            keyword.line,
                            "Can't return a value from an initializer.",
                        ));
                    }

                    // No Expression to resolve for a bare return
                } else {
                    // Normal function: resolve returned expression
                    if let Some(expr) = value {
                        self.resolve_expr(expr)?
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
            Expr::Literal(_) => {}

            Expr::Grouping(inner) => {
                self.resolve_expr(inner)?;
            }

            Expr::Unary { right, .. } => {
                self.resolve_expr(right)?;
            }

            Expr::Binary { left, right, .. } | Expr::Logical { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }

            Expr::Variable(tok) => {
                // Cannot read in own initializer
                if let Some(scope) = self.scopes.last() {
                    if scope.get(tok.lexeme) == Some(&false) {
                        return Err(LoxError::resolve(
                            tok.line,
                            "Cannot read local variable in its own initializer",
                        ));
                    }
                }
                // ✅ Bind either local *or* global
                self.resolve_local(expr, tok);
            }

            Expr::Assign { name, value } => {
                // First resolve RHS, then bind LHS
                self.resolve_expr(value)?;
                self.resolve_local(expr, name);
            }

            Expr::Call {
                callee, arguments, ..
            } => {
                self.resolve_expr(callee)?;
                for arg in arguments {
                    self.resolve_expr(arg)?;
                }
            }

            Expr::This(keyword) => {
                if self.current_class == ClassType::None {
                    return Err(LoxError::resolve(
                        keyword.line,
                        "Cannot use 'this' outside of a class",
                    ));
                }

                // Bind `this` like a local so the interpreter can find it.
                self.resolve_local(expr, keyword);
            }

            Expr::Get { object, .. } => self.resolve_expr(object)?,

            Expr::Set { object, value, .. } => {
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
        let enclosing = self.current_function;
        self.current_function = FunctionType::Function;

        self.begin_scope();
        for param in params {
            self.declare(param)?;
            self.define(param);
        }
        for stmt in body {
            self.resolve_stmt(stmt)?;
        }
        self.end_scope();

        self.current_function = enclosing;
        Ok(())
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Scope management
    // ─────────────────────────────────────────────────────────────────────────

    #[inline]
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    #[inline]
    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token<'a>) -> Result<()> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name.lexeme) {
                return Err(LoxError::resolve(
                    name.line,
                    "Variable already declared in this scope",
                ));
            }
            scope.insert(name.lexeme, false);
        }
        Ok(())
    }

    fn define(&mut self, name: &Token<'a>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme, true);
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Binding‑distance helper
    // ─────────────────────────────────────────────────────────────────────────

    /// Record this variable occurrence as either:
    ///  - a local at depth `d`, or
    ///  - a global if not found in *any* scope.
    fn resolve_local(&mut self, expr: &Expr<'a>, name: &Token<'a>) {
        // 1. check innermost → outermost
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name.lexeme) {
                debug!("Resolved '{}' at depth {}", name.lexeme, depth);
                self.interpreter.note_local(expr, depth);
                return;
            }
        }

        // 2. not found in any local scope ⇒ global
        debug!("Resolved '{}' as global", name.lexeme);

        self.interpreter.note_global(expr);
    }
}

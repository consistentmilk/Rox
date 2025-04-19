//! The `interpreter` module implements the runtime execution engine for Lox. It consumes the
//! fully-resolved AST (`Stmt` and `Expr` nodes) produced by the parser and resolver, and executes
//! the program in a dynamically-typed environment. The interpreter maintains a chain of
//! lexical environments (`Environment`s) for variable bindings, a map of local resolution
//! distances to enable O(1) variable lookup, and a set of native and user-defined functions
//! (including methods bound to instances).
//!
//! ## Workflow Overview
//!
//! 1. **Initialization** (`Interpreter::new`)
//!    - Creates the global environment and preloads native functions (e.g., `clock`).
//!    - Sets up the `locals` map, which will be populated by the resolver.
//!
//! 2. **Interpretation** (`interpret`)
//!    - Iterates over top-level statements, calling `execute` on each.
//!    - Ignores stray `return` at the top level.
//!
//! 3. **Statement Execution** (`execute`)
//!    - **Variable declarations**: Evaluate initializer then define in current environment.
//!    - **Blocks**: Swap to a new child environment, execute nested statements, then restore.
//!    - **Control flow**: `if`, `while`, and `for` implement branching and loops.
//!    - **Functions and classes**: Bind new `LoxFunction` or `LoxClass` values in the environment.
//!    - **Returns**: Threaded through nested `execute` calls via the `Control` enum.
//!
//! 4. **Expression Evaluation** (`evaluate`)
//!    - **Literals and variables**: Direct mapping to `Value` or lookup via `locals`/`globals`.
//!    - **Arithmetic and logical operators**: Handled by dedicated helpers (`arith`, `add_values`, etc.).
//!    - **Function calls**: Distinguish native functions, user functions, and constructors.
//!      - Constructors: Instantiate a `LoxInstance`, then invoke its `init` method if present.
//!    - **Property access**: `Get` and `Set` on `LoxInstance` to read/write dynamic fields or methods.
//!    - **`this`**: Looked up as a variable bound in method closures.
//!
//! 5. **Environment Helpers**
//!    - `ancestor`, `get_at`, and `assign_at` enable direct access to enclosing scopes based on
//!      lexical distances computed by the resolver.
//!    - `is_truthy` implements Lox’s truthiness rules (`false` and `nil` are falsey).
//!
//! This design ensures that each AST node has a clear execution path, with errors reported
//! via `LoxError::Runtime`, and leverages the resolver’s static analysis to optimize
//! variable and property lookups to O(1) time.

use crate::error::{LoxError, Result};
use crate::parser::{Expr, LiteralValue, Stmt};
use crate::token::{Token, TokenType};
use log::{debug, info};
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

/// How a resolved variable should be looked up at runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Resolution {
    /// A local binding at `depth` environments out.
    Local(usize),
    /// A truly global binding.
    Global,
}

// ─────────────────────────────────────────────────────────────────────────────
// Control‑flow plumbing
// ─────────────────────────────────────────────────────────────────────────────

/// Internal marker to thread `return` through nested `execute` calls
/// without panicking or using `Result` errors.
#[derive(Debug)]
enum Control<'a> {
    /// Normal flow: keep executing subsequent statements.
    Normal,
    /// A `return` was encountered, carrying its value.
    Return(Value<'a>),
}

// ─────────────────────────────────────────────────────────────────────────────
// Runtime values
// ─────────────────────────────────────────────────────────────────────────────

/// Dynamically‑typed Lox value.
#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
    Nil,

    Boolean(bool),

    Number(f64),

    Str(String),

    Function(LoxFunction<'a>),

    NativeFunction(NativeFunction<'a>),

    Class(LoxClass<'a>),

    Instance(LoxInstance<'a>),
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),

            Value::Boolean(b) => write!(f, "{}", b),

            Value::Number(n) => write!(f, "{}", n),

            Value::Str(s) => write!(f, "{}", s),

            Value::Function(fun) => write!(f, "<fn {}>", fun.name),

            Value::NativeFunction(nf) => write!(f, "<native fn {}>", nf.name),

            Value::Class(class) => write!(f, "{}", class),

            Value::Instance(object) => write!(f, "{}", object),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Classes (OOP)
// ─────────────────────────────────────────────────────────────────────────────
/// A user-defined class
#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass<'a> {
    pub name: String,
    pub methods: HashMap<String, LoxFunction<'a>>,
    pub superclass: Option<Rc<LoxClass<'a>>>,
}

impl<'a> LoxClass<'a> {
    /// Look up a method by name.
    #[inline(always)]
    fn find_method(&self, name: &str) -> Option<LoxFunction<'a>> {
        if let Some(method) = self.methods.get(name) {
            Some(method.clone())
        } else if let Some(ref superclass) = self.superclass {
            superclass.find_method(name)
        } else {
            None
        }
    }

    /// For now, classes take no constructor arguments.
    #[inline(always)]
    fn arity(&self) -> usize {
        if let Some(init) = self.find_method("init") {
            init.arity()
        } else {
            0
        }
    }

    /// Instantiates `this()` and returns the new object.
    fn instantiate(&self) -> LoxInstance<'a> {
        LoxInstance::new(self.clone())
    }
}

impl<'a> std::fmt::Display for LoxClass<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// An object instance produced by `ClassName()`
#[derive(Clone, Debug, PartialEq)]
pub struct LoxInstance<'a> {
    pub class: LoxClass<'a>,
    fields: Rc<RefCell<HashMap<String, Value<'a>>>>,
}

impl<'a> LoxInstance<'a> {
    pub fn new(class: LoxClass<'a>) -> Self {
        LoxInstance {
            class,
            fields: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn get_property(&self, name: &Token<'a>) -> Result<Value<'a>> {
        let key: &str = name.lexeme;

        // 1. Instance Fields
        if let Some(v) = self.fields.borrow().get(key) {
            return Ok(v.clone());
        }

        // 2. Class Methods
        if let Some(method) = self.class.methods.get(key) {
            // Bind 'this' so 'this' inside the method body refers here.
            let bound: LoxFunction<'a> = method.bind(Value::Instance(self.clone()));

            return Ok(Value::Function(bound));
        }

        // 3. Mot found
        Err(LoxError::Runtime(format!(
            "Undefined property '{}' (line {}).",
            key, name.line
        )))
    }

    pub fn set_property(&self, name: &Token<'a>, val: Value<'a>) {
        self.fields
            .borrow_mut()
            .insert(name.lexeme.to_string(), val);
    }
}

impl<'a> std::fmt::Display for LoxInstance<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class.name)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Callables
// ─────────────────────────────────────────────────────────────────────────────

/// A built‑in Lox function backed by a Rust function pointer.
#[derive(Clone, Debug, PartialEq)]
pub struct NativeFunction<'a> {
    pub name: String,
    pub arity: usize,
    pub func: fn(Vec<Value<'a>>) -> Result<Value<'a>>,
}

/// A user‑defined Lox function (closure).
#[derive(Clone, Debug, PartialEq)]
pub struct LoxFunction<'a> {
    pub name: String,
    pub params: Vec<String>,
    pub body: &'a [Stmt<'a>],
    pub closure: Rc<RefCell<Environment<'a>>>,
}

impl<'a> LoxFunction<'a> {
    /// How many parameters this function expects.
    #[inline]
    pub fn arity(&self) -> usize {
        self.params.len()
    }

    /// Call this function under `interp` with the given `args`.
    ///
    /// Temporarily swaps `interp.env` to a fresh child of `self.closure`,
    /// binds parameters, runs the body (early‑return supported), then
    /// restores the previous environment.
    pub fn call(&self, interp: &mut Interpreter<'a>, args: Vec<Value<'a>>) -> Result<Value<'a>> {
        debug!(
            "Calling user function '{}' ({} args)",
            self.name,
            args.len()
        );

        // 1. Create a new frame extending the closure:
        let child_env: Environment<'_> = Environment::with_enclosing(Rc::clone(&self.closure));
        let rc_child: Rc<RefCell<Environment<'_>>> = Rc::new(RefCell::new(child_env));

        // 2. Bind parameters in that frame:
        {
            let mut frame: std::cell::RefMut<'_, Environment<'_>> = rc_child.borrow_mut();
            for (param, arg) in self.params.iter().zip(args.into_iter()) {
                frame.define(param, arg);
            }
        }

        // 3. Swap into interpreter:
        let prev_env: Rc<RefCell<Environment<'a>>> = Rc::clone(&interp.env);
        interp.env = Rc::clone(&rc_child);

        // 4. Execute the body, catching any early return:
        let mut retval: Value<'_> = Value::Nil;

        for stmt in self.body {
            match interp.execute(stmt)? {
                Control::Normal => {}

                Control::Return(v) => {
                    retval = v;

                    break;
                }
            }
        }

        // 5. Choose return
        let result: Value<'_> = if self.name == "init" {
            rc_child.borrow().get("this").unwrap_or(Value::Nil)
        } else {
            retval
        };

        // 6. Restore the previous environment:
        interp.env = prev_env;

        Ok(result)
    }

    /// Create a bound method where `this` is defined to `instance`.
    pub fn bind(&self, instance: Value<'a>) -> LoxFunction<'a> {
        // 1. Create a new environment whose parent is the function's closure.
        let env: Environment<'_> = Environment::with_enclosing(Rc::clone(&self.closure));
        let rc_env: Rc<RefCell<Environment<'_>>> = Rc::new(RefCell::new(env));

        // 2. Define "this" in the newly created environment.
        rc_env.borrow_mut().define("this", instance);

        // 3. Return a new LoxFunction with the same code but wtth this closure.
        LoxFunction {
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body,
            closure: rc_env,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Lexical Environment
// ─────────────────────────────────────────────────────────────────────────────

/// A single scope’s variable map, linked to an optional enclosing scope.
#[derive(Clone, Debug, PartialEq)]
pub struct Environment<'a> {
    values: HashMap<String, Value<'a>>,
    enclosing: Option<Rc<RefCell<Environment<'a>>>>,
}

impl<'a> Environment<'a> {
    /// Create the *global* (root) environment.
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    /// Create a child environment rooted at `parent`.
    pub fn with_enclosing(parent: Rc<RefCell<Environment<'a>>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(parent),
        }
    }

    /// Define or shadow a variable in the current scope.
    pub fn define(&mut self, name: &str, val: Value<'a>) {
        self.values.insert(name.to_string(), val);
    }

    /// Lookup a variable, climbing the chain of enclosing scopes.
    pub fn get(&self, name: &str) -> Result<Value<'a>> {
        if let Some(v) = self.values.get(name) {
            Ok(v.clone())
        } else if let Some(parent) = &self.enclosing {
            parent.borrow().get(name)
        } else {
            Err(LoxError::Runtime(format!("Undefined variable '{}'.", name)))
        }
    }

    /// Assign to an existing variable, climbing scopes until found.
    pub fn assign(&mut self, name: &str, val: Value<'a>) -> Result<()> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), val);
            Ok(())
        } else if let Some(parent) = &self.enclosing {
            parent.borrow_mut().assign(name, val)
        } else {
            Err(LoxError::Runtime(format!("Undefined variable '{}'.", name)))
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Interpreter
// ─────────────────────────────────────────────────────────────────────────────

/// The tree‑walking interpreter.
///
/// - `globals`: the root scope (preloaded with `clock`).
/// - `env`: the current innermost environment.
/// - `locals`: maps each `Expr` pointer to its `Resolution` (set by resolver).
#[allow(unused)]
pub struct Interpreter<'a> {
    globals: Rc<RefCell<Environment<'a>>>,
    env: Rc<RefCell<Environment<'a>>>,
    locals: HashMap<*const Expr<'a>, Resolution>,
}

impl<'a> Interpreter<'a> {
    /// Create a new interpreter, initializing the global scope.
    pub fn new() -> Self {
        info!("Interpreter instantiated");
        let globals: Rc<RefCell<Environment<'_>>> = Rc::new(RefCell::new(Environment::new()));
        {
            let mut g: RefMut<'_, Environment<'_>> = globals.borrow_mut();
            // Register clock(): → seconds since UNIX_EPOCH
            g.define(
                "clock",
                Value::NativeFunction(NativeFunction {
                    name: "clock".into(),
                    arity: 0,
                    func: |_args: Vec<Value<'_>>| {
                        let secs: f64 = SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_secs_f64();

                        Ok(Value::Number(secs))
                    },
                }),
            );
        }

        Interpreter {
            globals: Rc::clone(&globals),
            env: globals,
            locals: HashMap::new(),
        }
    }

    /// Called by the *resolver* when it finds an Expr in a *local* scope.
    #[inline]
    pub fn note_local(&mut self, expr: &Expr<'a>, depth: usize) {
        self.locals
            .insert(expr as *const _, Resolution::Local(depth));
    }

    /// Called by the *resolver* when it finds an Expr *not* in any local scope.
    #[inline]
    pub fn note_global(&mut self, expr: &Expr<'a>) {
        self.locals.insert(expr as *const _, Resolution::Global);
    }

    /// Execute a sequence of statements (a full program).
    pub fn interpret(&mut self, stmts: &'a [Stmt<'a>]) -> Result<()> {
        info!("Interpreting {} statement(s)", stmts.len());

        for stmt in stmts {
            // top‑level `return`s are ignored
            let _ = self.execute(stmt)?;
        }

        Ok(())
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Statement execution
    // ─────────────────────────────────────────────────────────────────────────

    /// Execute one statement, returning whether it was a `return`.
    fn execute(&mut self, stmt: &'a Stmt<'a>) -> Result<Control<'a>> {
        debug!("Execute stmt: {:?}", stmt);

        let ctrl: Control<'_> = match stmt {
            // 1. Expression statement: evaluate & drop the result
            Stmt::Expression(e) => {
                self.evaluate(e)?;

                Control::Normal
            }

            // 2. Print statement: evaluate and print
            Stmt::Print(e) => {
                let v: Value<'a> = self.evaluate(e)?;

                println!("{}", v);

                Control::Normal
            }

            // 3. Variable declaration: resolve initializer then define
            Stmt::Var { name, initializer } => {
                let val: Value<'a> = initializer
                    .as_ref()
                    .map(|e: &Expr<'_>| self.evaluate(e)) // resolve RHS if present
                    .transpose()? // propagate any error
                    .unwrap_or(Value::Nil); // default to nil

                self.env.borrow_mut().define(name.lexeme, val);

                Control::Normal
            }

            // 4. Block: create a new child environment for the block
            Stmt::Block(stmts) => {
                let child: Environment<'_> = Environment::with_enclosing(Rc::clone(&self.env));
                self.execute_block(stmts, child)?
            }

            // 5. If statement: evaluate condition, then branch or else branch
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let c: Value<'a> = self.evaluate(condition)?;

                if self.is_truthy(&c) {
                    self.execute(then_branch)?
                } else if let Some(eb) = else_branch {
                    self.execute(eb)?
                } else {
                    Control::Normal
                }
            }

            // 6. While loop: repeatedly evaluate body while condition is truthy
            Stmt::While { condition, body } => {
                while {
                    let c: Value<'a> = self.evaluate(condition)?;
                    self.is_truthy(&c)
                } {
                    if let Control::Return(v) = self.execute(body)? {
                        return Ok(Control::Return(v));
                    }
                }

                Control::Normal
            }

            // 7. For loop: two nested scopes, one for initializer and one per iteration
            Stmt::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                // 7.1 Create loop‑scope environment once
                let prev: Rc<RefCell<Environment<'a>>> = Rc::clone(&self.env);
                let loop_env: Rc<RefCell<Environment<'_>>> = Rc::new(RefCell::new(
                    Environment::with_enclosing(Rc::clone(&self.env)),
                ));
                self.env = Rc::clone(&loop_env);

                // 7.2 Execute initializer if present
                if let Some(init) = initializer {
                    self.execute(init)?;
                }

                // 7.3 Loop while condition holds (or always if no condition)
                while {
                    if let Some(cond) = condition {
                        let c: Value<'a> = self.evaluate(cond)?;
                        self.is_truthy(&c)
                    } else {
                        true
                    }
                } {
                    // 7.4 Per‑iteration scope
                    let iter_env: Rc<RefCell<Environment<'_>>> = Rc::new(RefCell::new(
                        Environment::with_enclosing(Rc::clone(&loop_env)),
                    ));
                    self.env = Rc::clone(&iter_env);

                    // 7.5 Execute loop body
                    match self.execute(body)? {
                        Control::Normal => {}
                        r @ Control::Return(_) => {
                            self.env = prev;
                            return Ok(r);
                        }
                    }

                    // 7.6 Restore loop scope and run increment
                    self.env = Rc::clone(&loop_env);
                    if let Some(inc) = increment {
                        self.evaluate(inc)?;
                    }
                }

                // 7.7 Restore the previous environment
                self.env = prev;
                Control::Normal
            }

            // 8. Function declaration: bind a new LoxFunction into the environment
            Stmt::Function { name, params, body } => {
                let fun: LoxFunction<'_> = LoxFunction {
                    name: name.lexeme.to_string(),
                    params: params.iter().map(|t| t.lexeme.to_string()).collect(),
                    body,
                    closure: Rc::clone(&self.env),
                };
                self.env
                    .borrow_mut()
                    .define(name.lexeme, Value::Function(fun));
                Control::Normal
            }

            // 9. Return statement: either unwinds or yields a value
            Stmt::Return { value, .. } => {
                let v: Value<'a> = value
                    .as_ref()
                    .map(|e: &Expr<'_>| self.evaluate(e)) // resolve return value if any
                    .transpose()?
                    .unwrap_or(Value::Nil); // default to nil
                return Ok(Control::Return(v));
            }

            // 10. Class declaration: inheritance, super-binding, and method map setup
            Stmt::Class {
                name,
                methods,
                superclass,
            } => {
                // 10.1 Resolve the optional superclass:
                //     If a superclass token is present, look it up at runtime;
                //     ensure the value is actually a class.
                let superclass_cls: Option<Rc<LoxClass<'_>>> = if let Some(super_tok) = superclass {
                    match self.env.borrow().get(super_tok.lexeme)? {
                        Value::Class(ref sup) => Some(Rc::new(sup.clone())),
                        _ => {
                            // Runtime error when the bound value is not a class
                            return Err(LoxError::Runtime(format!(
                                "Superclass must be a class.\n[line {}]",
                                super_tok.line
                            )));
                        }
                    }
                } else {
                    None
                };

                // 10.2 Define the class name placeholder in the current environment:
                //     This allows recursive references to the class within its own methods.
                self.env.borrow_mut().define(name.lexeme, Value::Nil);

                // 10.3 Bind `super` if a superclass exists:
                //     Save the current environment so we can restore it later.
                let prev_env: Rc<RefCell<Environment<'a>>> = Rc::clone(&self.env);

                if let Some(ref sup_cls) = superclass_cls {
                    // Create a new child environment whose parent is the current env
                    let mut super_env: Environment<'_> =
                        Environment::with_enclosing(Rc::clone(&self.env));

                    // Define `super` to point to the superclass value
                    super_env.define("super", Value::Class(sup_cls.as_ref().clone()));

                    // Switch into the `super`-binding environment for method closure creation
                    self.env = Rc::new(RefCell::new(super_env));
                }

                // 10.4 Initialize the method map with inherited methods (if any):
                //     Clone the superclass's methods so we can override selectively.
                let mut method_map: HashMap<String, LoxFunction<'_>> =
                    if let Some(ref sup_cls) = superclass_cls {
                        sup_cls.methods.clone()
                    } else {
                        HashMap::new()
                    };

                // 10.5 Add or override with this class’s own methods:
                //     Each method closure captures the environment where `super` is bound
                for method in methods {
                    if let Stmt::Function {
                        name: m_name,
                        params,
                        body,
                    } = method
                    {
                        // Create a LoxFunction closure:
                        // - `name`: method name
                        // - `params`: parameter list converted to Strings
                        // - `body`: AST slice of statements
                        // - `closure`: the environment that has `super` defined
                        let fun: LoxFunction<'_> = LoxFunction {
                            name: m_name.lexeme.to_string(),
                            params: params
                                .iter()
                                .map(|t: &&Token<'_>| t.lexeme.to_string())
                                .collect(),
                            body: body.as_slice(),
                            closure: Rc::clone(&self.env),
                        };

                        // Insert or override in the method map
                        method_map.insert(m_name.lexeme.to_string(), fun);
                    }
                }

                // 10.6 Restore the outer environment (pop `super` scope):
                //     Method closures will capture the correct `super` binding.
                self.env = prev_env;

                // 10.7 Construct the final LoxClass object:
                //     - `name`: class name
                //     - `methods`: combined inherited + overridden methods
                //     - `superclass`: optional superclass reference
                let class: LoxClass<'_> = LoxClass {
                    name: name.lexeme.to_string(),
                    methods: method_map,
                    superclass: superclass_cls,
                };

                // 10.8 Bind the fully-constructed class into the environment:
                //     Replaces the earlier placeholder with the actual class value.
                self.env
                    .borrow_mut()
                    .define(name.lexeme, Value::Class(class));

                // 10.9 Finally, normal control flow continues
                Control::Normal
            }
        };

        // 11. Return the control flow outcome
        Ok(ctrl)
    }

    /// Execute a block in a fresh `new_env`, restoring the old one after.
    fn execute_block(
        &mut self,
        stmts: &'a [Stmt<'a>],
        new_env: Environment<'a>,
    ) -> Result<Control<'a>> {
        // 1. Save the current environment so we can restore it later
        let prev: Rc<RefCell<Environment<'a>>> = Rc::clone(&self.env);
        // 2. Switch to the new block environment
        self.env = Rc::new(RefCell::new(new_env));

        // 3. Execute each statement in the block, tracking an early return
        let mut outcome: Control<'_> = Control::Normal;
        for stmt in stmts {
            // 3.1 Execute the statement under the new environment
            let c: Control<'a> = self.execute(stmt)?;
            // 3.2 If a `return` flows out, capture it and stop executing further
            if let Control::Return(_) = c {
                outcome = c;
                break;
            }
        }

        // 4. Restore the previous environment after the block finishes
        self.env = prev;

        // 5. Return either the normal or the early-return control flow
        Ok(outcome)
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Expression evaluation
    // ─────────────────────────────────────────────────────────────────────────

    fn evaluate(&mut self, expr: &Expr<'a>) -> Result<Value<'a>> {
        debug!("Evaluate expr: {:?}", expr);

        match expr {
            // 1. Literal constants: directly map to Value
            Expr::Literal(lit) => match lit {
                LiteralValue::Number(n) => Ok(Value::Number(*n)),
                LiteralValue::Str(s) => Ok(Value::Str(s.clone())),
                LiteralValue::True => Ok(Value::Boolean(true)),
                LiteralValue::False => Ok(Value::Boolean(false)),
                LiteralValue::Nil => Ok(Value::Nil),
            },

            // 2. Grouping: evaluate inner expression
            Expr::Grouping(e) => self.evaluate(e),

            // 3. Unary operators: evaluate operand, then apply
            Expr::Unary { operator, right } => {
                let rv: Value<'a> = self.evaluate(right)?;
                match operator.token_type {
                    TokenType::MINUS => self.negate_number(operator.line, rv),

                    TokenType::BANG => Ok(Value::Boolean(!self.is_truthy(&rv))),

                    _ => unreachable!(),
                }
            }

            // 4. Binary arithmetic/comparison: eval both sides, then apply
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let lv: Value<'a> = self.evaluate(left)?;
                let rv: Value<'a> = self.evaluate(right)?;
                match operator.token_type {
                    TokenType::PLUS => self.add_values(operator.line, lv, rv),

                    TokenType::MINUS => self.arith(operator.line, lv, rv, |a, b| a - b),

                    TokenType::STAR => self.arith(operator.line, lv, rv, |a, b| a * b),

                    TokenType::SLASH => self.arith(operator.line, lv, rv, |a, b| a / b),

                    TokenType::GREATER => self.compare(operator.line, lv, rv, |a, b| a > b),

                    TokenType::GREATER_EQUAL => self.compare(operator.line, lv, rv, |a, b| a >= b),

                    TokenType::LESS => self.compare(operator.line, lv, rv, |a, b| a < b),

                    TokenType::LESS_EQUAL => self.compare(operator.line, lv, rv, |a, b| a <= b),

                    TokenType::EQUAL_EQUAL => Ok(Value::Boolean(lv == rv)),

                    TokenType::BANG_EQUAL => Ok(Value::Boolean(lv != rv)),

                    _ => unreachable!(),
                }
            }

            // 5. Logical operators with short‑circuit: eval left, maybe return, else eval right
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let lv: Value<'a> = self.evaluate(left)?;

                if operator.token_type == TokenType::OR {
                    if self.is_truthy(&lv) {
                        return Ok(lv);
                    }
                } else if !self.is_truthy(&lv) {
                    return Ok(lv);
                }

                self.evaluate(right)
            }

            // 6. Variable access: lookup via resolver metadata
            Expr::Variable(tok) => self.look_up_variable(tok, expr),

            // 7. Assignment: eval RHS, then assign and return value
            Expr::Assign { name, value } => {
                let vv: Value<'a> = self.evaluate(value)?;

                self.assign_variable(name, &vv, expr)?;

                Ok(vv)
            }

            // 8. Function or native call: eval callee, eval args, then dispatch
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee_val: Value<'a> = self.evaluate(callee)?;
                let mut args: Vec<Value<'a>> = Vec::with_capacity(arguments.len());

                for arg in arguments {
                    args.push(self.evaluate(arg)?);
                }

                match callee_val {
                    // 8.1 Native function
                    Value::NativeFunction(NativeFunction { arity, func, .. }) => {
                        if args.len() != arity {
                            return Err(LoxError::Runtime(format!(
                                "Expected {} args but got {} (line {}).",
                                arity,
                                args.len(),
                                paren.line
                            )));
                        }
                        func(args)
                    }

                    // 8.2 User‑defined function
                    Value::Function(fun) => {
                        if args.len() != fun.arity() {
                            return Err(LoxError::Runtime(format!(
                                "Expected {} args but got {} (line {}).",
                                fun.arity(),
                                args.len(),
                                paren.line
                            )));
                        }
                        fun.call(self, args)
                    }

                    // 8.3 Class constructor call
                    Value::Class(class) => {
                        // 8.3.1 Check constructor arity
                        let arity: usize = class.arity();
                        if args.len() != arity {
                            return Err(LoxError::Runtime(format!(
                                "Expected {} args but got {} (line {}).",
                                arity,
                                args.len(),
                                paren.line
                            )));
                        }

                        // 8.3.2 Instantiate the class
                        let instance: LoxInstance<'_> = class.instantiate();

                        // 8.3.3 Call init, if present
                        if let Some(initializer) = class.find_method("init") {
                            let bound_init = initializer.bind(Value::Instance(instance.clone()));
                            bound_init.call(self, args)?;
                        }

                        // 8.3.4 Return the initialized instance
                        Ok(Value::Instance(instance))
                    }

                    // 8.4 Not callable
                    _ => Err(LoxError::Runtime(format!(
                        "Can only call functions (line {}).",
                        paren.line
                    ))),
                }
            }

            // 9. The `this` keyword: lookup as a variable bound in a method scope
            Expr::This(keyword) => self.look_up_variable(keyword, expr),

            // 10. Property get: eval object, then get field or method
            Expr::Get { object, name } => {
                let obj: Value<'a> = self.evaluate(object)?;
                if let Value::Instance(inst) = obj {
                    inst.get_property(name)
                } else {
                    Err(LoxError::Runtime(format!(
                        "Only instances have properties (line {}).",
                        name.line
                    )))
                }
            }

            // 11. Property set: eval object, eval value, then set and return value
            Expr::Set {
                object,
                name,
                value,
            } => {
                let obj: Value<'a> = self.evaluate(object)?;
                let val: Value<'a> = self.evaluate(value)?;

                if let Value::Instance(inst) = obj {
                    inst.set_property(name, val.clone());
                    Ok(val)
                } else {
                    Err(LoxError::Runtime(format!(
                        "Only instances have fields (line {}).",
                        name.line
                    )))
                }
            }

            Expr::Super { method, .. } => {
                // 1. Determine the lexical distance for 'super'
                let dist = match self.locals.get(&(expr as *const _)) {
                    Some(Resolution::Local(d)) => *d,

                    _ => unreachable!(),
                };

                // 2. Fetch the superclass (bound under 'super')
                let superclass = match self.get_at(dist, "super")? {
                    Value::Class(c) => c,

                    _ => unreachable!(),
                };

                // 3. Fetch 'this' one level nearer
                let instance = match self.get_at(dist - 1, "this") {
                    Ok(Value::Instance(inst)) => inst,

                    _ => unreachable!(),
                };

                // 4. Look up the method on superclass
                if let Some(func) = superclass.find_method(method.lexeme) {
                    // Bind to 'this' and return
                    Ok(Value::Function(func.bind(Value::Instance(instance))))
                } else {
                    Err(LoxError::Runtime(format!(
                        "Undefined property '{}' (line {}).",
                        method.lexeme, method.line
                    )))
                }
            }
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Variable lookup & assignment
    // ─────────────────────────────────────────────────────────────────────────
    /// Look up a variable, *honoring* the resolver’s marking.
    fn look_up_variable(&self, name: &Token, expr: &Expr<'a>) -> Result<Value<'a>> {
        match self.locals.get(&(expr as *const _)) {
            // 1. A local binding at `depth` frames up
            Some(Resolution::Local(d)) => self.get_at(*d, name.lexeme),

            // 2. A truly global binding → always read from `globals`
            Some(Resolution::Global) => {
                debug!("Variable '{}' forced to GLOBAL lookup", name.lexeme);
                self.globals.borrow().get(name.lexeme)
            }

            // 3. No resolver entry (e.g. top‑level var) → default to current env
            None => {
                debug!("Variable '{}' dynamic lookup", name.lexeme);
                self.env.borrow().get(name.lexeme)
            }
        }
    }

    /// Assign to a variable, honoring lexical resolution.
    fn assign_variable(&self, name: &Token, val: &Value<'a>, expr: &Expr<'a>) -> Result<()> {
        // 1. Check how this expression was bound (local depth or global)
        match self.locals.get(&(expr as *const _)) {
            // 1.1 Local binding: assign at that lexical depth
            Some(Resolution::Local(d)) => self.assign_at(*d, name.lexeme, val.clone()),

            // 1.2 Global binding: force write to globals
            Some(Resolution::Global) => {
                debug!("Assign '{}' forced to GLOBAL", name.lexeme);
                self.globals.borrow_mut().assign(name.lexeme, val.clone())
            }

            // 1.3 No binding: dynamic lookup in current environment
            None => self.env.borrow_mut().assign(name.lexeme, val.clone()),
        }
    }

    /// Climb up `depth` environments to find the right one for a local variable.
    fn ancestor(&self, mut depth: usize) -> Rc<RefCell<Environment<'a>>> {
        // 1. Start from the current environment
        let mut env = Rc::clone(&self.env);

        // 2. Move up `depth` times through `.enclosing`
        while depth > 0 {
            let parent = env.borrow().enclosing.as_ref().unwrap().clone();
            env = parent;
            depth -= 1;
        }

        // 3. Return the found ancestor
        env
    }

    /// Get a variable from an ancestor environment.
    #[inline]
    fn get_at(&self, depth: usize, name: &str) -> Result<Value<'a>> {
        // 1. Retrieve the value at the given ancestor depth
        self.ancestor(depth).borrow().get(name)
    }

    /// Assign a value in an ancestor environment.
    #[inline]
    fn assign_at(&self, depth: usize, name: &str, val: Value<'a>) -> Result<()> {
        // 1. Write the value at the given ancestor depth
        self.ancestor(depth).borrow_mut().assign(name, val)
    }

    /// Determine truthiness according to Lox rules.
    #[inline]
    fn is_truthy(&self, v: &Value<'a>) -> bool {
        // 1. Only `false` and `nil` are falsey; everything else is truthy
        !matches!(v, Value::Boolean(false) | Value::Nil)
    }

    /// Numeric negation helper.
    fn negate_number(&self, line: usize, v: Value<'a>) -> Result<Value<'a>> {
        // 1. If it’s a number, negate it
        if let Value::Number(n) = v {
            Ok(Value::Number(-n))
        } else {
            // 2. Otherwise, runtime error
            Err(LoxError::Runtime(format!(
                "Operand must be a number (line {}).",
                line
            )))
        }
    }

    /// Arithmetic helper for binary ops.
    fn arith<F>(&self, line: usize, l: Value<'a>, r: Value<'a>, op: F) -> Result<Value<'a>>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        // 1. Both must be numbers to compute
        match (l, r) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(op(a, b))),

            // 2. Otherwise, runtime error
            _ => Err(LoxError::Runtime(format!(
                "Operands must be numbers (line {}).",
                line
            ))),
        }
    }

    /// Overloaded `+` for numbers and string concatenation.
    fn add_values(&self, line: usize, l: Value<'a>, r: Value<'a>) -> Result<Value<'a>> {
        match (l, r) {
            // 1. Number addition
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),

            // 2. String concatenation
            (Value::Str(mut s), Value::Str(t)) => {
                s.push_str(&t);
                Ok(Value::Str(s))
            }

            // 3. Mismatched types: runtime error
            _ => Err(LoxError::Runtime(format!(
                "Operands must be two numbers or two strings (line {}).",
                line
            ))),
        }
    }

    /// Comparison helper for numeric comparisons.
    fn compare<F>(&self, line: usize, l: Value<'a>, r: Value<'a>, cmp: F) -> Result<Value<'a>>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        // 1. Both must be numbers to compare
        match (l, r) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(cmp(a, b))),

            // 2. Otherwise, runtime error
            _ => Err(LoxError::Runtime(format!(
                "Operands must be numbers for comparison (line {}).",
                line
            ))),
        }
    }
}

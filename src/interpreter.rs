//! Tree‑walking interpreter
//!
//! This module is entirely *runtime* oriented – it consumes the fully‑resolved
//! AST produced by the `parser`/`resolver` pipeline and executes the program in
//! a dynamically‑typed environment.
//!
//! # Architectural overview
//! 1. [`Interpreter`] maintains the *current* lexical [`Environment`], a stack
//!    of call frames (implicit in `Environment` chains), and a `locals` map
//!    filled in by the resolver for O(1) variable lookup.
//! 2. Each [`Stmt`]/[`Expr`] node is evaluated through `execute` / `evaluate`.
//!    Control‑flow constructs (loops, `return`) are implemented via the
//!    [`Control`] enum which is threaded up the call stack.
//! 3. Functions are first‑class – *userdefined* (`LoxFunction`) and *native*
//!    (`NativeFunction`) values share a common [`Value`] representation and the
//!    same call site in `Expr::Call`.
//!
//! ## Concurrency
//! The interpreter is **single‑threaded** by design; interior mutability is
//! required only for environments captured by closures (`Rc<RefCell<_>>`).  No
//! public APIs expose mutable references that can be aliased across threads.
//!

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
}

impl<'a> LoxClass<'a> {
    /// Look up a method by name.
    #[inline(always)]
    fn find_method(&self, name: &str) -> Option<LoxFunction<'a>> {
        self.methods.get(name).cloned()
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
        let ctrl = match stmt {
            // Expression statement: evaluate & drop
            Stmt::Expression(e) => {
                self.evaluate(e)?;
                Control::Normal
            }

            // Print statement
            Stmt::Print(e) => {
                let v = self.evaluate(e)?;
                println!("{}", v);
                Control::Normal
            }

            // Variable declaration
            Stmt::Var { name, initializer } => {
                let val = initializer
                    .as_ref()
                    .map(|e| self.evaluate(e))
                    .transpose()? // propagate error if any
                    .unwrap_or(Value::Nil);
                self.env.borrow_mut().define(name.lexeme, val);
                Control::Normal
            }

            // Block `{ ... }`: swap in a new child environment
            Stmt::Block(stmts) => {
                let child = Environment::with_enclosing(Rc::clone(&self.env));
                self.execute_block(stmts, child)?
            }

            // If statement
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

            // While loop
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

            // For loop (uses two nested scopes: loop‑scope and per‑iteration scope)
            Stmt::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                // 1. Create the loop‑scope once
                let prev: Rc<RefCell<Environment<'a>>> = Rc::clone(&self.env);
                let loop_env: Rc<RefCell<Environment<'_>>> = Rc::new(RefCell::new(
                    Environment::with_enclosing(Rc::clone(&self.env)),
                ));
                self.env = Rc::clone(&loop_env);

                // 2. Run initializer
                if let Some(init) = initializer {
                    self.execute(init)?;
                }

                // 3. Loop
                while {
                    if let Some(cond) = condition {
                        let c = self.evaluate(cond)?;
                        self.is_truthy(&c)
                    } else {
                        true
                    }
                } {
                    // 4. Per‑iteration scope
                    let iter_env: Rc<RefCell<Environment<'_>>> = Rc::new(RefCell::new(
                        Environment::with_enclosing(Rc::clone(&loop_env)),
                    ));
                    self.env = Rc::clone(&iter_env);

                    // 5. Execute body
                    match self.execute(body)? {
                        Control::Normal => {}
                        r @ Control::Return(_) => {
                            self.env = prev;
                            return Ok(r);
                        }
                    }

                    // 6. Back to loop‑scope for increment
                    self.env = Rc::clone(&loop_env);
                    if let Some(inc) = increment {
                        self.evaluate(inc)?;
                    }
                }

                // 7. Restore
                self.env = prev;
                Control::Normal
            }

            // Function declaration
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

            // Return statement
            Stmt::Return { value, .. } => {
                let v: Value<'a> = value
                    .as_ref()
                    .map(|e: &Expr| self.evaluate(e))
                    .transpose()?
                    .unwrap_or(Value::Nil);
                return Ok(Control::Return(v));
            }

            #[allow(unused)]
            Stmt::Class {
                name,
                methods,
                superclass,
            } => {
                // Build method map
                let mut method_map: HashMap<String, LoxFunction<'_>> = HashMap::new();

                for method in methods.iter() {
                    if let Stmt::Function {
                        name: m_name,
                        params,
                        body,
                    } = method
                    {
                        let fun: LoxFunction<'_> = LoxFunction {
                            name: m_name.lexeme.to_string(),
                            params: params
                                .iter()
                                .map(|t: &&Token<'_>| t.lexeme.to_string())
                                .collect(),
                            body: body.as_slice(),
                            closure: Rc::clone(&self.env),
                        };

                        method_map.insert(m_name.lexeme.to_string(), fun);
                    }
                }

                let class: LoxClass<'_> = LoxClass {
                    name: name.lexeme.to_string(),
                    methods: method_map,
                };

                self.env
                    .borrow_mut()
                    .define(name.lexeme, Value::Class(class));

                Control::Normal
            }
        };

        Ok(ctrl)
    }

    /// Execute a block in a fresh `new_env`, restoring the old one after.
    fn execute_block(
        &mut self,
        stmts: &'a [Stmt<'a>],
        new_env: Environment<'a>,
    ) -> Result<Control<'a>> {
        let prev: Rc<RefCell<Environment<'a>>> = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(new_env));

        let mut outcome: Control<'_> = Control::Normal;

        for stmt in stmts {
            let c: Control<'a> = self.execute(stmt)?;

            if let Control::Return(_) = c {
                outcome = c;

                break;
            }
        }

        self.env = prev;
        Ok(outcome)
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Expression evaluation
    // ─────────────────────────────────────────────────────────────────────────

    fn evaluate(&mut self, expr: &Expr<'a>) -> Result<Value<'a>> {
        debug!("Evaluate expr: {:?}", expr);

        match expr {
            // Literal constants
            Expr::Literal(lit) => match lit {
                LiteralValue::Number(n) => Ok(Value::Number(*n)),
                LiteralValue::Str(s) => Ok(Value::Str(s.clone())),
                LiteralValue::True => Ok(Value::Boolean(true)),
                LiteralValue::False => Ok(Value::Boolean(false)),
                LiteralValue::Nil => Ok(Value::Nil),
            },

            // Grouping: just unwrap
            Expr::Grouping(e) => self.evaluate(e),

            // Unary operators: `-` and `!`
            Expr::Unary { operator, right } => {
                let rv: Value<'a> = self.evaluate(right)?;
                match operator.token_type {
                    TokenType::MINUS => self.negate_number(operator.line, rv),
                    TokenType::BANG => Ok(Value::Boolean(!self.is_truthy(&rv))),
                    _ => unreachable!(),
                }
            }

            // Binary arithmetic/comparison
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

            // Logical `and` / `or` with short‑circuit
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

            // Variable access
            Expr::Variable(tok) => self.look_up_variable(tok, expr),

            // Assignment expression
            Expr::Assign { name, value } => {
                let vv: Value<'a> = self.evaluate(value)?;
                self.assign_variable(name, &vv, expr)?;
                Ok(vv)
            }

            // Function or native call
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

                    Value::Class(class) => {
                        // 1. Check argument count against constructor
                        let arity: usize = class.arity();

                        if args.len() != arity {
                            return Err(LoxError::Runtime(format!(
                                "Expected {} args but got {} (line {}).",
                                arity,
                                args.len(),
                                paren.line
                            )));
                        }

                        // 2. Create the raw instance
                        let instance: LoxInstance<'_> = class.instantiate();

                        // 3. If there's an init method, bind and invoke it on the new instance
                        if let Some(initializer) = class.find_method("init") {
                            let bound_init = initializer.bind(Value::Instance(instance.clone()));

                            // We will ignore the return value of init. Methods that return
                            // this explicitly still yield the instance
                            bound_init.call(self, args)?;
                        }

                        // Always return the instance
                        Ok(Value::Instance(instance))
                    }

                    _ => Err(LoxError::Runtime(format!(
                        "Can only call functions (line {}).",
                        paren.line
                    ))),
                }
            }

            Expr::This(keyword) => {
                // Look up 'this' via the resolver's recorded depth.
                self.look_up_variable(keyword, expr)
            }

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
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Variable lookup & assignment
    // ─────────────────────────────────────────────────────────────────────────
    /// Look up a variable, *honoring* the resolver’s marking.
    fn look_up_variable(&self, name: &Token, expr: &Expr<'a>) -> Result<Value<'a>> {
        match self.locals.get(&(expr as *const _)) {
            // 1) A local binding at `depth` frames up
            Some(Resolution::Local(d)) => self.get_at(*d, name.lexeme),

            // 2) A truly global binding → always read from `globals`
            Some(Resolution::Global) => {
                debug!("Variable '{}' forced to GLOBAL lookup", name.lexeme);
                self.globals.borrow().get(name.lexeme)
            }

            // 3) No resolver entry (e.g. top‑level var) → default to current env
            None => {
                debug!("Variable '{}' dynamic lookup", name.lexeme);
                self.env.borrow().get(name.lexeme)
            }
        }
    }

    /// Similarly for assignment.
    fn assign_variable(&self, name: &Token, val: &Value<'a>, expr: &Expr<'a>) -> Result<()> {
        match self.locals.get(&(expr as *const _)) {
            Some(Resolution::Local(d)) => self.assign_at(*d, name.lexeme, val.clone()),

            Some(Resolution::Global) => {
                debug!("Assign '{}' forced to GLOBAL", name.lexeme);
                self.globals.borrow_mut().assign(name.lexeme, val.clone())
            }

            None => self.env.borrow_mut().assign(name.lexeme, val.clone()),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Ancestor lookup via lexical distance
    // ─────────────────────────────────────────────────────────────────────────

    /// Climb `depth` times through `enclosing` to get the correct
    /// environment for a local variable.
    fn ancestor(&self, mut depth: usize) -> Rc<RefCell<Environment<'a>>> {
        let mut env: Rc<RefCell<Environment<'a>>> = Rc::clone(&self.env);

        while depth > 0 {
            let parent: Rc<RefCell<Environment<'a>>> =
                env.borrow().enclosing.as_ref().unwrap().clone();
            env = parent;
            depth -= 1;
        }

        env
    }

    #[inline]
    fn get_at(&self, depth: usize, name: &str) -> Result<Value<'a>> {
        self.ancestor(depth).borrow().get(name)
    }

    #[inline]
    fn assign_at(&self, depth: usize, name: &str, val: Value<'a>) -> Result<()> {
        self.ancestor(depth).borrow_mut().assign(name, val)
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Utility helpers: truthiness & numeric ops
    // ─────────────────────────────────────────────────────────────────────────

    #[inline]
    fn is_truthy(&self, v: &Value<'a>) -> bool {
        !matches!(v, Value::Boolean(false) | Value::Nil)
    }

    fn negate_number(&self, line: usize, v: Value<'a>) -> Result<Value<'a>> {
        if let Value::Number(n) = v {
            Ok(Value::Number(-n))
        } else {
            Err(LoxError::Runtime(format!(
                "Operand must be a number (line {}).",
                line
            )))
        }
    }

    fn arith<F>(&self, line: usize, l: Value<'a>, r: Value<'a>, op: F) -> Result<Value<'a>>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        match (l, r) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(op(a, b))),

            _ => Err(LoxError::Runtime(format!(
                "Operands must be numbers (line {}).",
                line
            ))),
        }
    }

    fn add_values(&self, line: usize, l: Value<'a>, r: Value<'a>) -> Result<Value<'a>> {
        match (l, r) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::Str(mut s), Value::Str(t)) => {
                s.push_str(&t);
                Ok(Value::Str(s))
            }

            _ => Err(LoxError::Runtime(format!(
                "Operands must be two numbers or two strings (line {}).",
                line
            ))),
        }
    }

    fn compare<F>(&self, line: usize, l: Value<'a>, r: Value<'a>, cmp: F) -> Result<Value<'a>>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        match (l, r) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(cmp(a, b))),

            _ => Err(LoxError::Runtime(format!(
                "Operands must be numbers for comparison (line {}).",
                line
            ))),
        }
    }
}

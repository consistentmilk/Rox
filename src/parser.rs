//! Module `parser` implements a single‑pass, Pratt‑style recursive‑descent parser for the Lox language.
//! It takes a flat slice of `Token`s and produces a structured AST (`Vec<Stmt<'a>>`) in O(n) time,
//! where n is the number of tokens.
//!
//! # High‑Level Phases
//!
//! 1. **Top‑Level Declarations** (`parse` → `declaration`)
//!    - `parse()` loops until EOF, calling `declaration()` for each construct.
//!    - `declaration()` dispatches on `class`, `fun`, or `var` keywords, otherwise defers to `statement()`.
//!    - On parse errors, `synchronize()` skips tokens to the next statement boundary and resumes parsing.
//!
//! 2. **Statement Parsing** (`statement`)
//!    - Supports control flow: `if`, `for`, `while`, `return`.
//!    - Recognizes block scopes (`{ ... }`), creating nested statement lists.
//!    - Handles simple statements: `print`, expression statements, and variable declarations.
//!
//! 3. **Expression Parsing (Pratt / Precedence‑Climbing)**
//!    - Entry: `expression()` ⇒ `assignment()`.
//!    - Logical operators: `logical_or()` ⇒ `logical_and()`.
//!    - Comparisons: `equality()` ⇒ `comparison()` ⇒ `term()` ⇒ `factor()`.
//!    - Unary and call expressions: `unary()` ⇒ `call()` ⇒ `finish_call()` ⇒ `primary()`.
//!    - Each level loops to build left‑associative nodes; `call()` and `finish_call()` also handle
//!      function calls and property access (`.`).
//!
//! 4. **Class and Function Declarations**
//!    - `class_declaration()` parses optional superclass, method list, and injects `this` in method scopes.
//!    - `function()` parses named functions, parameter lists (≤255), and bodies via `block()`.
//!
//! 5. **Error Recovery**
//!    - All parse errors produce a `LoxError::parse` with location info.
//!    - `synchronize()` discards tokens until a semicolon or statement‑starting keyword,
//!      enabling the parser to recover and continue.
//!
//! # Grammar Sketch
//!
//! ```text
//! program        → declaration* EOF ;
//! declaration    → classDecl
//!                | funDecl
//!                | varDecl
//!                | statement ;
//!
//! classDecl      → "class" IDENT ( "<" IDENT )? "{" function* "}" ;
//! funDecl        → "fun" function ;
//! varDecl        → "var" IDENT ( "=" expression )? ";" ;
//!
//! statement      → exprStmt
//!                | forStmt
//!                | ifStmt
//!                | printStmt
//!                | returnStmt
//!                | whileStmt
//!                | block ;
//!
//! expression     → assignment ;
//! assignment     → ( call "." )? IDENT "=" assignment  
//!                | logic_or ;
//! logic_or       → logic_and ( "or" logic_and )* ;
//! logic_and      → equality ( "and" equality )* ;
//! equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//! comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//! term           → factor ( ( "-" | "+" ) factor )* ;
//! factor         → unary ( ( "/" | "*" ) unary )* ;
//! unary          → ( "!" | "-" ) unary | call ;
//! call           → primary ( "(" arguments? ")" | "." IDENT )* ;
//! primary        → "true" | "false" | "nil"   
//!                | NUMBER | STRING | IDENT | "this"   
//!                | "(" expression ")" ;
//! ```
//!
//! This design cleanly separates declarations, control‑flow statements, and expressions,
//! leveraging resolver metadata for error‑tolerant, efficient parsing and recovery.

use crate::error::{LoxError, Result};
use crate::token::{Token, TokenType};

use log::{debug, info};

/// A **literal constant** that appears directly in the source code.
///  
/// These variants are the *terminal leaves* of the expression tree and
/// therefore do **not** retain a reference to the originating [`Token`].
/// The parser copies (or converts) the value at parse‑time so the AST
/// can outlive the lexer’s token buffer.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    /// Numeric literal ‑ stored as IEEE‑754 `f64`.  
    /// Integral lexemes such as `"3"` are still parsed as `3.0`.
    Number(f64),

    /// String literal without surrounding quotes.
    Str(String),

    /// The boolean constant `true`.
    True,

    /// The boolean constant `false`.
    False,

    /// The `nil` literal (Lox’s `null`).
    Nil,
}

/// **Abstract‑Syntax‑Tree node** representing every kind of *expression*
/// in Lox.  Lifetimes ‑`'a` tie nodes that contain token references back
/// to the borrowed token slice held by the [`Parser`].
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    /// A literal constant: number, string, `true`, `false`, or `nil`.
    Literal(LiteralValue),

    /// Prefix unary operator expression  
    /// *Example:* `!isReady` or `-42`
    Unary {
        /// The operator token (`!` or `-`).
        operator: &'a Token<'a>,
        /// Operand to which the operator is applied.
        right: Box<Expr<'a>>,
    },

    /// Infix binary operator expression  
    /// *Example:* `a + b`, `x <= y`
    Binary {
        left: Box<Expr<'a>>,
        /// Operator token such as `+`, `*`, `==`, …  
        operator: &'a Token<'a>,
        right: Box<Expr<'a>>,
    },

    /// Parenthesised sub‑expression: `"(" expression ")"`.
    Grouping(Box<Expr<'a>>),

    /// Variable access ‑ resolves to the identifier’s current value at runtime.
    Variable(&'a Token<'a>),

    /// Assignment expression: `identifier "=" expression`
    Assign {
        name: &'a Token<'a>,
        value: Box<Expr<'a>>,
    },

    /// Short‑circuiting logical operators `and` / `or`.
    Logical {
        left: Box<Expr<'a>>,
        operator: &'a Token<'a>, // `AND` or `OR`
        right: Box<Expr<'a>>,
    },

    /// Function‑ or method‑call expression  
    /// *Example:* `clock()` or `add(1, 2)`
    Call {
        /// Expression that evaluates to a callable (variable, property, etc.).
        callee: Box<Expr<'a>>,
        /// The closing `)` token ‑ retained for error reporting.
        paren: &'a Token<'a>,
        /// Argument list (may be empty).
        arguments: Vec<Expr<'a>>,
    },

    /// object.property
    Get {
        object: Box<Expr<'a>>,
        name: &'a Token<'a>,
    },

    /// object.property = value
    Set {
        object: Box<Expr<'a>>,
        name: &'a Token<'a>,
        value: Box<Expr<'a>>,
    },

    /// The 'this' keyword inside a method.
    This(&'a Token<'a>),

    /// Superclass method invocation: `super.method`
    Super {
        /// The `super` keyword token.
        keyword: &'a Token<'a>,

        /// The method name to invoke.
        method: &'a Token<'a>,
    },
}

/// **Abstract‑Syntax‑Tree node** for *statements* (complete executable
/// constructs).  A program is a sequence of these nodes returned by
/// [`Parser::parse`].
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    /// Stand‑alone expression terminated by a semicolon.
    Expression(Expr<'a>),

    /// `print` statement used for output.
    Print(Expr<'a>),

    /// Variable declaration: `"var" IDENT ("=" initializer)? ";"`.
    Var {
        name: &'a Token<'a>,
        initializer: Option<Expr<'a>>,
    },

    /// Braced scope containing zero or more declarations/statements.
    Block(Vec<Stmt<'a>>),

    /// `if` / `else` conditional.
    If {
        condition: Expr<'a>,
        then_branch: Box<Stmt<'a>>,
        else_branch: Option<Box<Stmt<'a>>>,
    },

    /// `while` loop.
    While {
        condition: Expr<'a>,
        body: Box<Stmt<'a>>,
    },

    /// Function declaration ‑ becomes a first‑class callable value.
    Function {
        name: &'a Token<'a>,

        /// Parameter name tokens (arity ≤ 255).
        params: Vec<&'a Token<'a>>,

        /// Body executed when the function is called.
        body: Vec<Stmt<'a>>,
    },

    For {
        initializer: Option<Box<Stmt<'a>>>,
        condition: Option<Expr<'a>>,
        increment: Option<Expr<'a>>,
        body: Box<Stmt<'a>>,
    },

    /// `return` statement inside a function body.
    Return {
        /// The `return` keyword token (for runtime error locations).
        keyword: &'a Token<'a>,

        /// Optional expression to return.  
        /// Absent ⇒ `nil` is returned.
        value: Option<Expr<'a>>,
    },

    Class {
        name: &'a Token<'a>,
        methods: Vec<Stmt<'a>>,
        superclass: Option<&'a Token<'a>>,
    },
}

/// Top‑level parser over an immutable slice of tokens.
pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    current: usize,
}

impl<'a> Parser<'a> {
    /// Construct a new parser.
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        info!("Parser created with {} tokens", tokens.len());

        Self { tokens, current: 0 }
    }

    // ───────────────────────── public API ─────────────────────────

    /// Parse an entire program and return its statement list.
    pub fn parse(&mut self) -> Result<Vec<Stmt<'a>>> {
        info!("Beginning parse phase");

        let mut statements: Vec<Stmt<'a>> = Vec::new();

        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        Ok(statements)
    }

    // ──────────────────────── declaration rules ───────────────────

    fn declaration(&mut self) -> Result<Stmt<'a>> {
        debug!("Entering declaration");

        // 1. Check for a class declaration
        let result = if self.matches(TokenType::CLASS) {
            self.class_declaration()
        // 2. Check for a function declaration
        } else if self.matches(TokenType::FUN) {
            self.function("function")
        // 3. Check for a variable declaration
        } else if self.matches(TokenType::VAR) {
            self.var_declaration()
        // 4. Otherwise parse a statement
        } else {
            self.statement()
        };

        // 5. If there was a parse error, synchronize to the next statement boundary
        if result.is_err() {
            self.synchronize();
        }

        result
    }

    fn class_declaration(&mut self) -> Result<Stmt<'a>> {
        // 1. Consume and capture the class name
        let name: &Token<'_> = self.consume(TokenType::IDENTIFIER, "Expected class name")?;

        // 2. Optionally parse a superclass: '<' IDENTIFIER
        let superclass = if self.matches(TokenType::LESS) {
            let name = self.consume(TokenType::IDENTIFIER, "Expected superclass name")?;
            Some(name)
        } else {
            None
        };

        // 3. Consume the '{' to begin class body
        self.consume(TokenType::LEFT_BRACE, "Expected '{' before class body")?;

        // 4. Parse zero or more methods inside the class
        let mut methods: Vec<Stmt<'_>> = Vec::new();
        while !self.check(TokenType::RIGHT_BRACE) && !self.is_at_end() {
            // 4.a. Method name
            let method_name = self.consume(TokenType::IDENTIFIER, "Expected method name")?;
            // 4.b. Parameter list start
            self.consume(TokenType::LEFT_PAREN, "Expected '(' after method name")?;

            // 4.c. Parse comma‑separated parameters (up to 255)
            let mut params: Vec<&Token<'_>> = Vec::new();

            if !self.check(TokenType::RIGHT_PAREN) {
                loop {
                    if params.len() >= 255 {
                        return Err(LoxError::parse(
                            method_name.line,
                            "Cannot have more than 255 parameters",
                        ));
                    }

                    params.push(self.consume(TokenType::IDENTIFIER, "Expected parameter name")?);

                    if !self.matches(TokenType::COMMA) {
                        break;
                    }
                }
            }

            // 4.d. Close parameter list and open method body
            self.consume(TokenType::RIGHT_PAREN, "Expected ')' after parameters")?;
            self.consume(TokenType::LEFT_BRACE, "Expected '{' before method body")?;

            // 4.e. Reuse block parsing for the method body
            let body: Vec<Stmt<'a>> = self.block()?;

            methods.push(Stmt::Function {
                name: method_name,
                params,
                body,
            });
        }

        // 5. Consume the '}' to end class body
        self.consume(TokenType::RIGHT_BRACE, "Expected '}' after class body")?;

        // 6. Return the built Class statement
        Ok(Stmt::Class {
            name,
            methods,
            superclass,
        })
    }

    fn function(&mut self, _kind: &str) -> Result<Stmt<'a>> {
        // 1. Consume the function name
        let name: &Token<'_> = self.consume(TokenType::IDENTIFIER, "Expected function name")?;

        // 2. Consume '(' to start parameter list
        self.consume(TokenType::LEFT_PAREN, "Expected '(' after function name")?;

        // 3. Parse comma‑separated parameters
        let mut parameters: Vec<&Token<'_>> = Vec::new();

        if !self.check(TokenType::RIGHT_PAREN) {
            loop {
                if parameters.len() >= 255 {
                    return Err(LoxError::parse(
                        name.line,
                        "Cannot have more than 255 parameters",
                    ));
                }

                parameters.push(self.consume(TokenType::IDENTIFIER, "Expected parameter name")?);

                if !self.matches(TokenType::COMMA) {
                    break;
                }
            }
        }

        // 4. Close parameter list, open function body
        self.consume(TokenType::RIGHT_PAREN, "Expected ')' after parameters")?;
        self.consume(TokenType::LEFT_BRACE, "Expected '{' before function body")?;

        // 5. Parse the function body as a block of statements
        let body: Vec<Stmt<'a>> = self.block()?;

        Ok(Stmt::Function {
            name,
            params: parameters,
            body,
        })
    }

    fn var_declaration(&mut self) -> Result<Stmt<'a>> {
        // 1. Consume the variable name
        let name: &Token<'_> = self.consume(TokenType::IDENTIFIER, "Expected variable name")?;

        // 2. Optionally parse an initializer expression
        let initializer: Option<Expr<'a>> = if self.matches(TokenType::EQUAL) {
            Some(self.expression()?)
        } else {
            None
        };

        // 3. Consume the terminating semicolon
        self.consume(
            TokenType::SEMICOLON,
            "Expected ';' after variable declaration",
        )?;

        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> Result<Stmt<'a>> {
        // 1. Dispatch based on statement keyword
        if self.matches(TokenType::FOR) {
            self.for_statement()
        } else if self.matches(TokenType::IF) {
            self.if_statement()
        } else if self.matches(TokenType::WHILE) {
            self.while_statement()
        } else if self.matches(TokenType::RETURN) {
            self.return_statement()
        } else if self.matches(TokenType::LEFT_BRACE) {
            Ok(Stmt::Block(self.block()?))
        } else if self.matches(TokenType::PRINT) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn for_statement(&mut self) -> Result<Stmt<'a>> {
        // 1. Consume '(' after 'for'
        self.consume(TokenType::LEFT_PAREN, "Expected '(' after 'for'")?;

        // 2. Parse initializer (var decl, expr stmt, or none)
        let initializer: Option<Box<Stmt<'a>>> = if self.matches(TokenType::SEMICOLON) {
            None
        } else if self.matches(TokenType::VAR) {
            Some(Box::new(self.var_declaration()?))
        } else {
            Some(Box::new(self.expression_statement()?))
        };

        // 3. Parse optional condition, consume ';'
        let condition: Option<Expr<'a>> = if !self.check(TokenType::SEMICOLON) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::SEMICOLON, "Expected ';' after loop condition")?;

        // 4. Parse optional increment, consume ')'
        let increment: Option<Expr<'a>> = if !self.check(TokenType::RIGHT_PAREN) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::RIGHT_PAREN, "Expected ')' after for clauses")?;

        // 5. Parse the loop body
        let body = Box::new(self.statement()?);
        Ok(Stmt::For {
            initializer,
            condition,
            increment,
            body,
        })
    }

    fn print_statement(&mut self) -> Result<Stmt<'a>> {
        // 1. Parse the expression to print
        let value: Expr<'a> = self.expression()?;

        // 2. Consume ';'
        self.consume(TokenType::SEMICOLON, "Expected ';' after value")?;

        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt<'a>> {
        // 1. Parse the expression
        let expr: Expr<'a> = self.expression()?;

        // 2. Consume ';'
        self.consume(TokenType::SEMICOLON, "Expected ';' after expression")?;

        Ok(Stmt::Expression(expr))
    }

    fn if_statement(&mut self) -> Result<Stmt<'a>> {
        // 1. Consume '(' after 'if'
        self.consume(TokenType::LEFT_PAREN, "Expected '(' after 'if'")?;

        // 2. Parse the condition
        let condition: Expr<'a> = self.expression()?;

        self.consume(TokenType::RIGHT_PAREN, "Expected ')' after condition")?;

        // 3. Parse the then-branch
        let then_branch: Box<Stmt<'a>> = Box::new(self.statement()?);

        // 4. Optionally parse an else-branch
        let else_branch: Option<Box<Stmt<'a>>> = if self.matches(TokenType::ELSE) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn while_statement(&mut self) -> Result<Stmt<'a>> {
        // 1. Consume '(' after 'while'
        self.consume(TokenType::LEFT_PAREN, "Expected '(' after 'while'")?;

        // 2. Parse the loop condition
        let condition: Expr<'a> = self.expression()?;

        self.consume(TokenType::RIGHT_PAREN, "Expected ')' after condition")?;

        // 3. Parse the loop body
        let body: Box<Stmt<'a>> = Box::new(self.statement()?);
        Ok(Stmt::While { condition, body })
    }

    fn return_statement(&mut self) -> Result<Stmt<'a>> {
        // 1. Capture the 'return' keyword for error reporting
        let keyword: &Token<'_> = self.previous();

        // 2. Optionally parse a return expression
        let value: Option<Expr<'a>> = if !self.check(TokenType::SEMICOLON) {
            Some(self.expression()?)
        } else {
            None
        };

        // 3. Consume ';'
        self.consume(TokenType::SEMICOLON, "Expected ';' after return value")?;
        Ok(Stmt::Return { keyword, value })
    }

    fn block(&mut self) -> Result<Vec<Stmt<'a>>> {
        let mut statements: Vec<Stmt<'a>> = Vec::new();

        // 1. Keep parsing declarations until '}' or EOF
        while !self.check(TokenType::RIGHT_BRACE) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        // 2. Consume the closing '}'
        self.consume(TokenType::RIGHT_BRACE, "Expected '}' after block")?;
        Ok(statements)
    }

    // ─────────────────────── expression rules (Pratt) ─────────────

    fn expression(&mut self) -> Result<Expr<'a>> {
        // 1. Parse assignment expressions
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr<'a>> {
        // 1. Parse a logical-or expression first
        let expr: Expr<'a> = self.logical_or()?;

        // 2. If there's an '=', build an assign or set expression
        if self.matches(TokenType::EQUAL) {
            let equals: &Token<'_> = self.previous();
            let value: Expr<'a> = self.assignment()?;

            // 2.1. Simple variable assignment
            if let Expr::Variable(name) = expr {
                return Ok(Expr::Assign {
                    name,
                    value: Box::new(value),
                });
            }

            // 2.2. Property assignment via Get
            if let Expr::Get { object, name } = expr {
                return Ok(Expr::Set {
                    object,
                    name,
                    value: Box::new(value),
                });
            }

            // 2.3. Otherwise, error
            return Err(LoxError::parse(equals.line, "Invalid assignment target"));
        }

        // 3. No '=' → just return the parsed expression
        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr<'a>> {
        // 1. Parse the left side as a logical_and expression
        let mut expr: Expr<'a> = self.logical_and()?;

        // 2. While we see 'or', parse the right side and build a Logical node
        while self.matches(TokenType::OR) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.logical_and()?;
            expr = Expr::Logical {
                left: Box::new(expr),   // 2.1 left operand
                operator,               // 2.2 'or' token
                right: Box::new(right), // 2.3 right operand
            };
        }

        // 3. Return the fully‑built logical_or expression
        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr<'a>> {
        // 1. Parse the left side as an equality expression
        let mut expr: Expr<'a> = self.equality()?;

        // 2. While we see 'and', parse right side and build a Logical node
        while self.matches(TokenType::AND) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),   // 2.1 left operand
                operator,               // 2.2 'and' token
                right: Box::new(right), // 2.3 right operand
            };
        }

        // 3. Return the fully‑built logical_and expression
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr<'a>> {
        // 1. Parse the left side as a comparison expression
        let mut expr: Expr<'a> = self.comparison()?;

        // 2. Handle '!=' and '==' in a loop for left‑associative parsing
        while self.matches(TokenType::BANG_EQUAL) || self.matches(TokenType::EQUAL_EQUAL) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),   // 2.1 left operand
                operator,               // 2.2 comparison token
                right: Box::new(right), // 2.3 right operand
            };
        }

        // 3. Return the equality expression
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'a>> {
        // 1. Parse the left side as a term expression
        let mut expr: Expr<'a> = self.term()?;

        // 2. Handle <, <=, >, and >= in a loop
        while self.matches(TokenType::GREATER)
            || self.matches(TokenType::GREATER_EQUAL)
            || self.matches(TokenType::LESS)
            || self.matches(TokenType::LESS_EQUAL)
        {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),   // 2.1 left operand
                operator,               // 2.2 comparison token
                right: Box::new(right), // 2.3 right operand
            };
        }

        // 3. Return the comparison expression
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'a>> {
        // 1. Parse the left side as a factor expression
        let mut expr: Expr<'a> = self.factor()?;

        // 2. Handle '+' and '-' in a loop
        while self.matches(TokenType::MINUS) || self.matches(TokenType::PLUS) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),   // 2.1 left operand
                operator,               // 2.2 '+' or '-' token
                right: Box::new(right), // 2.3 right operand
            };
        }

        // 3. Return the term expression
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'a>> {
        // 1. Parse the left side as a unary expression
        let mut expr: Expr<'a> = self.unary()?;

        // 2. Handle '*' and '/' in a loop
        while self.matches(TokenType::STAR) || self.matches(TokenType::SLASH) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),   // 2.1 left operand
                operator,               // 2.2 '*' or '/' token
                right: Box::new(right), // 2.3 right operand
            };
        }

        // 3. Return the factor expression
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'a>> {
        // 1. If we see '!' or '-', consume and build a Unary node
        if self.matches(TokenType::BANG) || self.matches(TokenType::MINUS) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.unary()?; // 1.1 parse operand
            return Ok(Expr::Unary {
                operator, // 1.2 '!' or '-' token
                right: Box::new(right),
            });
        }

        // 2. Otherwise, parse a call expression
        self.call()
    }

    fn call(&mut self) -> Result<Expr<'a>> {
        // 1. Parse a primary expression
        let mut expr: Expr<'a> = self.primary()?;

        // 2. Loop to handle any number of calls or property gets
        loop {
            if self.matches(TokenType::LEFT_PAREN) {
                // 2.a Finish a function call
                expr = self.finish_call(expr)?;
            } else if self.matches(TokenType::DOT) {
                // 2.b Parse a property access
                let name: &Token<'_> =
                    self.consume(TokenType::IDENTIFIER, "Expected property name after '.'")?;
                expr = Expr::Get {
                    object: Box::new(expr), // 2.b.1 target object
                    name,                   // 2.b.2 property token
                };
            } else {
                // 2.c No more call or get → break
                break;
            }
        }

        // 3. Return the call/get expression
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr<'a>) -> Result<Expr<'a>> {
        // 1. Parse zero or more argument expressions
        let mut arguments: Vec<Expr<'a>> = Vec::new();
        if !self.check(TokenType::RIGHT_PAREN) {
            loop {
                if arguments.len() >= 255 {
                    return Err(LoxError::parse(
                        self.peek().line,
                        "Cannot have more than 255 arguments",
                    ));
                }
                arguments.push(self.expression()?); // 1.1 parse each argument
                if !self.matches(TokenType::COMMA) {
                    break;
                }
            }
        }

        // 2. Consume the closing ')'
        let paren: &Token<'_> =
            self.consume(TokenType::RIGHT_PAREN, "Expected ')' after arguments")?;

        // 3. Build the Call node
        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,     // 3.1 closing paren token
            arguments, // 3.2 list of argument expressions
        })
    }

    fn primary(&mut self) -> Result<Expr<'a>> {
        // 1. Literal values
        if self.matches(TokenType::FALSE) {
            return Ok(Expr::Literal(LiteralValue::False));
        }

        if self.matches(TokenType::TRUE) {
            return Ok(Expr::Literal(LiteralValue::True));
        }

        if self.matches(TokenType::NIL) {
            return Ok(Expr::Literal(LiteralValue::Nil));
        }

        // 2. Number literal
        if self.matches(TokenType::NUMBER(0.0)) {
            if let TokenType::NUMBER(n) = self.previous().token_type.clone() {
                return Ok(Expr::Literal(LiteralValue::Number(n)));
            }
        }

        // 3. String literal
        if let TokenType::STRING(ref s) = self.peek().token_type {
            self.advance();
            return Ok(Expr::Literal(LiteralValue::Str(s.clone())));
        }

        // 4. Identifier or variable
        if self.matches(TokenType::IDENTIFIER) {
            return Ok(Expr::Variable(self.previous()));
        }

        // 5. Parenthesized expression
        if self.matches(TokenType::LEFT_PAREN) {
            let expr: Expr<'a> = self.expression()?;
            self.consume(TokenType::RIGHT_PAREN, "Expected ')' after expression")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        // 6. 'this' keyword
        if self.matches(TokenType::THIS) {
            return Ok(Expr::This(self.previous()));
        }

        // 7. 'super' keyword for superclass method calss
        if self.matches(TokenType::SUPER) {
            let keyword: &Token<'_> = self.previous();

            self.consume(TokenType::DOT, "Expected '.' after 'super'.")?;

            let method: &Token<'_> =
                self.consume(TokenType::IDENTIFIER, "EExpected supreclass method name.")?;

            return Ok(Expr::Super { keyword, method });
        }

        // 8. Error if nothing matched
        Err(LoxError::parse(self.peek().line, "Expected expression"))
    }

    // ────────────────────── utility helpers ───────────────────────

    #[inline(always)]
    fn matches(&mut self, ttype: TokenType) -> bool {
        if self.check(ttype) {
            self.advance();

            return true;
        }

        false
    }

    #[inline(always)]
    fn consume(&mut self, ttype: TokenType, message: &str) -> Result<&'a Token<'a>> {
        if self.check(ttype) {
            return Ok(self.advance());
        }

        Err(LoxError::parse(self.peek().line, message))
    }

    #[inline(always)]
    fn check(&self, ttype: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token_type == ttype
    }

    #[inline(always)]
    fn advance(&mut self) -> &'a Token<'a> {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    #[inline(always)]
    fn is_at_end(&self) -> bool {
        matches!(self.peek().token_type, TokenType::EOF)
    }

    #[inline(always)]
    fn peek(&self) -> &'a Token<'a> {
        &self.tokens[self.current]
    }

    #[inline(always)]
    fn previous(&self) -> &'a Token<'a> {
        &self.tokens[self.current - 1]
    }

    /// Discards tokens until it thinks it is at a statement boundary.
    fn synchronize(&mut self) {
        self.advance(); // skip the token that caused the error

        while !self.is_at_end() {
            if matches!(self.previous().token_type, TokenType::SEMICOLON) {
                return;
            }

            match self.peek().token_type {
                TokenType::CLASS
                | TokenType::FUN
                | TokenType::VAR
                | TokenType::FOR
                | TokenType::IF
                | TokenType::WHILE
                | TokenType::PRINT
                | TokenType::RETURN => return,
                _ => {}
            }

            self.advance();
        }
    }
}

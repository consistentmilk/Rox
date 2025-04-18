/*!
Time & Space Complexity — whole‑file overview
============================================

Definitions
-----------
* **n** = number of tokens (including the sole EOF).
* **m** = number of AST nodes (`m ≤ n`, because each token contributes at most one node).

### Time

| Phase / function              | Cost | Rationale                                                             |
|-------------------------------|-----:|-----------------------------------------------------------------------|
| `Parser::parse` main loop     | Θ(n) | Each token is consumed once via `advance()`.                          |
| Individual productions        | O(1) per token | Recursive‑descent & Pratt are stream‑oriented; no extra scans. |
| Error recovery `synchronize()`| O(k) | Discards tokens ≤ next statement boundary ( `k ≤ n`).                 |

**Overall:** **Θ(n)**.

### Space

| Structure                | Asymptotic | Notes                                                         |
|--------------------------|-----------:|---------------------------------------------------------------|
| Borrowed token slice     | O(n)       | Zero‑copy from scanner.                                       |
| AST (`Vec`, `Box`)       | O(m) ≈ O(n)| One `Box` per interior node; literals reuse data from tokens. |
| Parser scratch fields    | O(1)       | A few indices and temporaries.                                |

Call‑stack depth grows with syntactic nesting (≪ n in practice).

### Logging Policy

| Location                     | Level  | Purpose                                   |
|------------------------------|--------|-------------------------------------------|
| `Parser::new`, `parse`       | `info` | Lifecycle milestones.                     |
| `declaration`, `statement`   | `debug`| High‑level descent into grammar branches. |
| Error paths (`consume`, etc.)| `debug`| Context before returning structured error.|

Each log macro is followed by a blank line unless it begins a block, per project style.

--------------------------------------------------------------------------------
Grammar (EBNF — condensed, Crafting Interpreters dialect)
--------------------------------------------------------

```
program        → declaration* EOF ;
declaration    → funDecl | varDecl | statement ;
funDecl        → "fun" IDENT "(" parameters? ")" block ;
varDecl        → "var" IDENT ( "=" expression )? ";" ;
statement      → exprStmt | printStmt | whileStmt
               | ifStmt | block | returnStmt ;
exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;
whileStmt      → "while" "(" expression ")" statement ;
ifStmt         → "if" "(" expression ")" statement
               ( "else" statement )? ;
block          → "{" declaration* "}" ;
parameters     → IDENT ( "," IDENT )* ;
expression     → assignment ;
assignment     → IDENT "=" assignment | logic_or ;
logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality  ( "and" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | call ;
call           → primary ( "(" arguments? ")" )* ;
arguments      → expression ( "," expression )* ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | IDENT | "(" expression ")" ;
```

Expression parsing uses **Pratt (precedence‑climbing)**, enabling compact,
data‑driven operator precedence while remaining Θ(n) over the token stream.
*/

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

        let result = if self.matches(TokenType::CLASS) {
            self.class_declaration()
        } else if self.matches(TokenType::FUN) {
            self.function("function")
        } else if self.matches(TokenType::VAR) {
            self.var_declaration()
        } else {
            self.statement()
        };

        if result.is_err() {
            self.synchronize();
        }

        result
    }

    fn class_declaration(&mut self) -> Result<Stmt<'a>> {
        let name: &Token<'_> = self.consume(TokenType::IDENTIFIER, "Expected class name")?;

        self.consume(TokenType::LEFT_BRACE, "Expected '{' before class body")?;

        let mut methods: Vec<Stmt<'_>> = Vec::new();

        while !self.check(TokenType::RIGHT_BRACE) && !self.is_at_end() {
            // parse a method: IDENT "(" params? ")" block
            let method_name = self.consume(TokenType::IDENTIFIER, "Expected method name")?;
            self.consume(TokenType::LEFT_PAREN, "Expected '(' after method name")?;

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
            self.consume(TokenType::RIGHT_PAREN, "Expected ')' after parameters")?;
            self.consume(TokenType::LEFT_BRACE, "Expected '{' before method body")?;

            let body: Vec<Stmt<'a>> = self.block()?; // reuse block() to parse Vec<Stmt<'a>>

            methods.push(Stmt::Function {
                name: method_name,
                params,
                body,
            });
        }

        self.consume(TokenType::RIGHT_BRACE, "Expected '}' after class body")?;

        Ok(Stmt::Class { name, methods })
    }

    fn function(&mut self, _kind: &str) -> Result<Stmt<'a>> {
        let name: &Token<'_> = self.consume(TokenType::IDENTIFIER, "Expected function name")?;

        self.consume(TokenType::LEFT_PAREN, "Expected '(' after function name")?;

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

        self.consume(TokenType::RIGHT_PAREN, "Expected ')' after parameters")?;

        self.consume(TokenType::LEFT_BRACE, "Expected '{' before function body")?;
        let body = self.block()?;

        Ok(Stmt::Function {
            name,
            params: parameters,
            body,
        })
    }

    fn var_declaration(&mut self) -> Result<Stmt<'a>> {
        let name: &Token<'_> = self.consume(TokenType::IDENTIFIER, "Expected variable name")?;

        let initializer: Option<Expr<'a>> = if self.matches(TokenType::EQUAL) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            TokenType::SEMICOLON,
            "Expected ';' after variable declaration",
        )?;
        Ok(Stmt::Var { name, initializer })
    }

    // ───────────────────────── statement rules ────────────────────
    fn statement(&mut self) -> Result<Stmt<'a>> {
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
        self.consume(TokenType::LEFT_PAREN, "Expected '(' after 'for'")?;
        let initializer = if self.matches(TokenType::SEMICOLON) {
            None
        } else if self.matches(TokenType::VAR) {
            Some(Box::new(self.var_declaration()?))
        } else {
            Some(Box::new(self.expression_statement()?))
        };
        let condition = if !self.check(TokenType::SEMICOLON) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::SEMICOLON, "Expected ';' after loop condition")?;
        let increment = if !self.check(TokenType::RIGHT_PAREN) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::RIGHT_PAREN, "Expected ')' after for clauses")?;
        let body = Box::new(self.statement()?);
        Ok(Stmt::For {
            initializer,
            condition,
            increment,
            body,
        })
    }

    fn print_statement(&mut self) -> Result<Stmt<'a>> {
        let value: Expr<'a> = self.expression()?;

        self.consume(TokenType::SEMICOLON, "Expected ';' after value")?;

        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt<'a>> {
        let expr: Expr<'a> = self.expression()?;
        self.consume(TokenType::SEMICOLON, "Expected ';' after expression")?;
        Ok(Stmt::Expression(expr))
    }

    fn if_statement(&mut self) -> Result<Stmt<'a>> {
        self.consume(TokenType::LEFT_PAREN, "Expected '(' after 'if'")?;
        let condition: Expr<'a> = self.expression()?;
        self.consume(TokenType::RIGHT_PAREN, "Expected ')' after condition")?;

        let then_branch: Box<Stmt<'a>> = Box::new(self.statement()?);
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
        self.consume(TokenType::LEFT_PAREN, "Expected '(' after 'while'")?;
        let condition: Expr<'a> = self.expression()?;
        self.consume(TokenType::RIGHT_PAREN, "Expected ')' after condition")?;
        let body: Box<Stmt<'a>> = Box::new(self.statement()?);

        Ok(Stmt::While { condition, body })
    }

    fn return_statement(&mut self) -> Result<Stmt<'a>> {
        let keyword: &Token<'_> = self.previous();
        let value: Option<Expr<'a>> = if !self.check(TokenType::SEMICOLON) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::SEMICOLON, "Expected ';' after return value")?;
        Ok(Stmt::Return { keyword, value })
    }

    fn block(&mut self) -> Result<Vec<Stmt<'a>>> {
        let mut statements: Vec<Stmt<'a>> = Vec::new();

        while !self.check(TokenType::RIGHT_BRACE) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RIGHT_BRACE, "Expected '}' after block")?;
        Ok(statements)
    }

    // ─────────────────────── expression rules (Pratt) ─────────────
    fn expression(&mut self) -> Result<Expr<'a>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr<'a>> {
        let expr: Expr<'a> = self.logical_or()?;

        if self.matches(TokenType::EQUAL) {
            let equals: &Token<'_> = self.previous();
            let value: Expr<'a> = self.assignment()?;

            match expr {
                Expr::Variable(name) => {
                    return Ok(Expr::Assign {
                        name,
                        value: Box::new(value),
                    });
                }

                Expr::Get { object, name } => {
                    return Ok(Expr::Set {
                        object,
                        name,
                        value: Box::new(value),
                    });
                }

                _ => {
                    return Err(LoxError::parse(equals.line, "Invalid assignment target"));
                }
            }
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr<'a>> {
        let mut expr: Expr<'a> = self.logical_and()?;

        while self.matches(TokenType::OR) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.logical_and()?;

            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr<'a>> {
        let mut expr: Expr<'a> = self.equality()?;

        while self.matches(TokenType::AND) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.equality()?;

            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr<'a>> {
        let mut expr: Expr<'a> = self.comparison()?;

        while self.matches(TokenType::BANG_EQUAL) || self.matches(TokenType::EQUAL_EQUAL) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.comparison()?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'a>> {
        let mut expr = self.term()?;

        while self.matches(TokenType::GREATER)
            || self.matches(TokenType::GREATER_EQUAL)
            || self.matches(TokenType::LESS)
            || self.matches(TokenType::LESS_EQUAL)
        {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'a>> {
        let mut expr: Expr<'a> = self.factor()?;

        while self.matches(TokenType::MINUS) || self.matches(TokenType::PLUS) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'a>> {
        let mut expr: Expr<'a> = self.unary()?;

        while self.matches(TokenType::STAR) || self.matches(TokenType::SLASH) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'a>> {
        if self.matches(TokenType::BANG) || self.matches(TokenType::MINUS) {
            let operator: &Token<'_> = self.previous();
            let right: Expr<'a> = self.unary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr<'a>> {
        let mut expr: Expr<'a> = self.primary()?;

        loop {
            if self.matches(TokenType::LEFT_PAREN) {
                expr = self.finish_call(expr)?;
            } else if self.matches(TokenType::DOT) {
                let name: &Token<'_> =
                    self.consume(TokenType::IDENTIFIER, "Expected property name after '.'")?;

                expr = Expr::Get {
                    object: Box::new(expr),
                    name,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr<'a>) -> Result<Expr<'a>> {
        let mut arguments: Vec<Expr<'a>> = Vec::new();
        if !self.check(TokenType::RIGHT_PAREN) {
            loop {
                if arguments.len() >= 255 {
                    return Err(LoxError::parse(
                        self.peek().line,
                        "Cannot have more than 255 arguments",
                    ));
                }

                arguments.push(self.expression()?);

                if !self.matches(TokenType::COMMA) {
                    break;
                }
            }
        }

        let paren: &Token<'_> =
            self.consume(TokenType::RIGHT_PAREN, "Expected ')' after arguments")?;

        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }

    fn primary(&mut self) -> Result<Expr<'a>> {
        if self.matches(TokenType::FALSE) {
            return Ok(Expr::Literal(LiteralValue::False));
        }
        if self.matches(TokenType::TRUE) {
            return Ok(Expr::Literal(LiteralValue::True));
        }
        if self.matches(TokenType::NIL) {
            return Ok(Expr::Literal(LiteralValue::Nil));
        }

        if self.matches(TokenType::NUMBER(0.0)) {
            if let TokenType::NUMBER(n) = self.previous().token_type.clone() {
                return Ok(Expr::Literal(LiteralValue::Number(n)));
            }
        }

        if let TokenType::STRING(ref s) = self.peek().token_type {
            self.advance();
            return Ok(Expr::Literal(LiteralValue::Str(s.clone())));
        }

        if self.matches(TokenType::IDENTIFIER) {
            return Ok(Expr::Variable(self.previous()));
        }

        if self.matches(TokenType::LEFT_PAREN) {
            let expr: Expr<'a> = self.expression()?;

            self.consume(TokenType::RIGHT_PAREN, "Expected ')' after expression")?;

            return Ok(Expr::Grouping(Box::new(expr)));
        }

        if self.matches(TokenType::THIS) {
            return Ok(Expr::This(self.previous()));
        }

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

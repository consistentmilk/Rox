use crate::expr::Expr;
use crate::scanner::Scanner;
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};
use std::iter::Peekable;

///
/// Parser struct directly leverages the Iterator API
/// exposed by the Scanner struct that tokenizes the
/// provided input.
///
/// The Iterator API allows us to consume tokens one by one,
/// and directly parse them into valid expressions. This keeps
/// memory usage low by reducing cloning and copying
///
/// Another important aspect is that the Iterator API allows
/// us to make the Scanner struct Peekable, which is ncessary
/// to parse more complex expressions.
///
pub struct Parser<'a> {
    tokens: Peekable<Scanner<'a>>,
    previous: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        Parser {
            tokens: scanner.peekable(),
            previous: None,
        }
    }

    // For compatibility with existing parse command (e.g., Codecrafters tests)
    pub fn parse(&mut self) -> Result<Expr<'a>, String> {
        self.expression()
    }

    // Parse a single statement (used by Iterator)
    fn parse_statement(&mut self) -> Result<Stmt<'a>, String> {
        if self.match_tokens(&[TokenType::PRINT])? {
            return self.print_statement();
        }

        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt<'a>, String> {
        let value: Expr<'a> = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            return Err(format!(
                "Expected ';' after print statement on line {}",
                self.peek()?.line
            ));
        }

        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt<'a>, String> {
        let expr: Expr<'a> = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            return Err(format!(
                "Expected ';' after expression on line {}",
                self.peek()?.line
            ));
        }

        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr<'a>, String> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr<'a>, String> {
        let mut expr: Expr<'a> = self.comparison()?;

        while self.match_tokens(&[TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL])? {
            let operator: Token<'a> = self.previous().clone();
            let right: Expr<'a> = self.comparison()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'a>, String> {
        let mut expr: Expr<'a> = self.term()?;

        while self.match_tokens(&[
            TokenType::GREATER,
            TokenType::GREATER_EQUAL,
            TokenType::LESS,
            TokenType::LESS_EQUAL,
        ])? {
            let operator: Token<'a> = self.previous().clone();
            let right: Expr<'a> = self.term()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'a>, String> {
        let mut expr: Expr<'a> = self.factor()?;

        while self.match_tokens(&[TokenType::MINUS, TokenType::PLUS])? {
            let operator: Token<'a> = self.previous().clone();
            let right: Expr<'a> = self.factor()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'a>, String> {
        let mut expr: Expr<'a> = self.unary()?;

        while self.match_tokens(&[TokenType::SLASH, TokenType::STAR])? {
            let operator: Token<'a> = self.previous().clone();
            let right: Expr<'a> = self.unary()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'a>, String> {
        if self.match_tokens(&[TokenType::BANG, TokenType::MINUS])? {
            let operator: Token<'a> = self.previous().clone();
            let right: Expr<'a> = self.unary()?;

            return Ok(Expr::Unary(operator, Box::new(right)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr<'a>, String> {
        if self.match_tokens(&[
            TokenType::NUMBER(0.0),
            TokenType::STRING(String::new()),
            TokenType::TRUE,
            TokenType::FALSE,
            TokenType::NIL,
        ])? {
            return Ok(Expr::Literal(self.previous().clone()));
        }

        if self.match_tokens(&[TokenType::LEFT_PAREN])? {
            let expr: Expr<'a> = self.expression()?;

            if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
                return Err(format!(
                    "Expected ')' after expression on line {}",
                    self.peek()?.line
                ));
            }

            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(format!("Expected expression on line {}", self.peek()?.line))
    }

    ///
    /// Matches the current tokens type to a provided token type
    ///
    /// Advances the iterator to the next token
    ///
    fn match_tokens(&mut self, types: &[TokenType]) -> Result<bool, String> {
        for token_type in types {
            if self.check(token_type)? {
                self.advance()?;

                return Ok(true);
            }
        }

        Ok(false)
    }

    /// Helper method for match_tokens()
    fn check(&mut self, token_type: &TokenType) -> Result<bool, String> {
        if self.is_at_end()? {
            return Ok(false);
        }

        Ok(&self.peek()?.token_type == token_type)
    }

    ///
    /// Consumes the current token from the Scanner and
    /// stores it in self.previous
    ///
    /// Implicitly moves the Scanner to scan for the next
    /// token
    ///
    fn advance(&mut self) -> Result<&Token<'a>, String> {
        self.previous = self.tokens.next().transpose()?;

        self.previous
            .as_ref()
            .ok_or_else(|| "Unexpected end of input".to_string())
    }

    fn is_at_end(&mut self) -> Result<bool, String> {
        Ok(self.peek()?.token_type == TokenType::EOF)
    }

    fn peek(&mut self) -> Result<&Token<'a>, String> {
        self.tokens
            .peek()
            .ok_or_else(|| "Reached EOF".to_string())?
            .as_ref()
            .map_err(|e: &String| e.clone())
    }

    fn previous(&self) -> &Token<'a> {
        self.previous.as_ref().expect("No previous token")
    }

    fn synchronize(&mut self) -> Result<(), String> {
        while !self.is_at_end()? {
            match self.peek()?.token_type {
                TokenType::SEMICOLON => {
                    self.advance()?;
                    return Ok(());
                }

                TokenType::CLASS
                | TokenType::FUN
                | TokenType::VAR
                | TokenType::FOR
                | TokenType::IF
                | TokenType::WHILE
                | TokenType::PRINT
                | TokenType::RETURN => {
                    return Ok(());
                }

                _ => {
                    self.advance()?;
                }
            }
        }
        Ok(())
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Stmt<'a>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        // Check if we're at EOF
        match self.is_at_end() {
            Ok(true) => return None,
            Ok(false) => {}
            Err(e) => return Some(Err(e)),
        }

        // Try to parse a statement
        match self.parse_statement() {
            Ok(stmt) => Some(Ok(stmt)),

            Err(_) => {
                // Synchronize to the next statement
                match self.synchronize() {
                    Ok(()) => self.next(), // Try parsing again
                    Err(sync_err) => Some(Err(sync_err)),
                }
            }
        }
    }
}

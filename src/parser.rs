use crate::expr::Expr;
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
use std::iter::Peekable;

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

    pub fn parse(&mut self) -> Result<Expr<'a>, String> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr<'a>, String> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr<'a>, String> {
        let mut expr = self.comparison()?;

        while self.match_tokens(&[TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL])? {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'a>, String> {
        let mut expr = self.term()?;

        while self.match_tokens(&[
            TokenType::GREATER,
            TokenType::GREATER_EQUAL,
            TokenType::LESS,
            TokenType::LESS_EQUAL,
        ])? {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'a>, String> {
        let mut expr = self.factor()?;

        while self.match_tokens(&[TokenType::MINUS, TokenType::PLUS])? {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'a>, String> {
        let mut expr = self.unary()?;

        while self.match_tokens(&[TokenType::SLASH, TokenType::STAR])? {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'a>, String> {
        if self.match_tokens(&[TokenType::BANG, TokenType::MINUS])? {
            let operator = self.previous().clone();
            let right = self.unary()?;
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
            let expr = self.expression()?;
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

    fn match_tokens(&mut self, types: &[TokenType]) -> Result<bool, String> {
        for token_type in types {
            if self.check(token_type)? {
                self.advance()?;
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn check(&mut self, token_type: &TokenType) -> Result<bool, String> {
        if self.is_at_end()? {
            return Ok(false);
        }
        Ok(&self.peek()?.token_type == token_type)
    }

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
            .ok_or_else(|| "Peeked beyond end of input".to_string())?
            .as_ref()
            .map_err(|e| e.clone())
    }

    fn previous(&self) -> &Token<'a> {
        self.previous.as_ref().expect("No previous token")
    }
}

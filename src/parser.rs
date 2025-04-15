use crate::expr::Expr;
use crate::scanner::Scanner;
use crate::stmt::Stmt;
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

    fn parse_statement(&mut self) -> Result<Stmt<'a>, String> {
        if self.match_tokens(&[TokenType::FOR])? {
            return self.for_statement();
        }

        if self.match_tokens(&[TokenType::WHILE])? {
            return self.while_statement();
        }

        if self.match_tokens(&[TokenType::IF])? {
            return self.if_statement();
        }

        if self.match_tokens(&[TokenType::PRINT])? {
            return self.print_statement();
        }

        if self.match_tokens(&[TokenType::VAR])? {
            return self.parse_var_statement();
        }

        if self.check(&TokenType::IDENTIFIER)? && self.check_next(&TokenType::EQUAL)? {
            return self.assign_statement();
        }

        if self.match_tokens(&[TokenType::LEFT_BRACE])? {
            return self.block();
        }

        self.expression_statement()
    }

    fn for_statement(&mut self) -> Result<Stmt<'a>, String> {
        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            return Err(self.error_at_current("Expected '(' after 'for'"));
        }

        // Initializer
        let initializer: Option<Box<Stmt<'a>>> = if self.match_tokens(&[TokenType::SEMICOLON])? {
            None
        } else if self.match_tokens(&[TokenType::VAR])? {
            Some(Box::new(self.parse_var_statement()?))
        } else {
            let expr_stmt: Stmt<'a> = match self.expression_statement() {
                Ok(stmt) => stmt,

                Err(e) => return Err(e),
            };

            if matches!(expr_stmt, Stmt::Expression(_)) {
                Some(Box::new(expr_stmt))
            } else {
                return Err(self.error_at_previous("Expected expression in for initializer"));
            }
        };

        // Condition
        let condition: Option<Expr<'a>> = if !self.check(&TokenType::SEMICOLON)? {
            let cond: Expr<'a> = self.expression()?;

            if !self.match_tokens(&[TokenType::SEMICOLON])? {
                return Err(self.error_at_current("Expected ';' after loop condition"));
            }

            Some(cond)
        } else {
            self.match_tokens(&[TokenType::SEMICOLON])?;

            None
        };

        // Increment
        let increment: Option<Expr<'a>> = if !self.check(&TokenType::RIGHT_PAREN)? {
            Some(self.expression()?)
        } else {
            None
        };

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            return Err(self.error_at_current("Expected ')' after for clauses"));
        }

        // Body
        let body: Stmt<'a> = match self.parse_statement() {
            Ok(stmt) => stmt,

            Err(e) => return Err(e),
        };
        if matches!(body, Stmt::Var(_, _)) {
            return Err(self.error_at_previous("Expected expression"));
        }

        Ok(Stmt::For(initializer, condition, increment, Box::new(body)))
    }

    fn while_statement(&mut self) -> Result<Stmt<'a>, String> {
        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            return Err(self.error_at_current("Expected '(' after 'while'"));
        }

        let condition: Expr<'a> = self.expression()?;

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            return Err(self.error_at_current("Expected ')' after while condition"));
        }

        let body: Stmt<'a> = self.parse_statement()?;

        Ok(Stmt::While(condition, Box::new(body)))
    }

    fn if_statement(&mut self) -> Result<Stmt<'a>, String> {
        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            return Err(self.error_at_current("Expected '(' after 'if'"));
        }

        let condition: Expr<'a> = self.expression()?;

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            return Err(self.error_at_current("Expected ')' after if condition"));
        }

        let then_branch: Stmt<'a> = self.parse_statement()?;

        let else_branch: Option<Box<Stmt<'a>>> = if self.match_tokens(&[TokenType::ELSE])? {
            let else_stmt: Stmt<'a> = self.parse_statement()?;

            Some(Box::new(else_stmt))
        } else {
            None
        };

        Ok(Stmt::If(condition, Box::new(then_branch), else_branch))
    }

    fn block(&mut self) -> Result<Stmt<'a>, String> {
        let mut statements: Vec<Stmt<'a>> = Vec::new();

        while !self.check(&TokenType::RIGHT_BRACE)? && !self.is_at_end()? {
            statements.push(self.parse_statement()?);
        }

        if !self.match_tokens(&[TokenType::RIGHT_BRACE])? {
            return Err(self.error_at_current("Expected '}' after block"));
        }

        Ok(Stmt::Block(statements))
    }

    fn assign_statement(&mut self) -> Result<Stmt<'a>, String> {
        self.match_tokens(&[TokenType::IDENTIFIER])?;

        let name: Token<'a> = self.previous().clone();

        self.match_tokens(&[TokenType::EQUAL])?;

        let value: Expr<'a> = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            return Err(self.error_at_current("Expected ';' after assignment"));
        }

        Ok(Stmt::Assign(name, value))
    }

    fn print_statement(&mut self) -> Result<Stmt<'a>, String> {
        let value: Expr<'a> = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            return Err(self.error_at_current("Expected ';' after print statement"));
        }

        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt<'a>, String> {
        let expr: Expr<'a> = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            return Err(self.error_at_current("Expected ';' after expression"));
        }

        Ok(Stmt::Expression(expr))
    }

    fn parse_var_statement(&mut self) -> Result<Stmt<'a>, String> {
        if !self.match_tokens(&[TokenType::IDENTIFIER])? {
            return Err(self.error_at_current("Expected variable name"));
        }

        let name: Token<'a> = self.previous().clone();

        let initializer: Option<Expr<'a>> = if self.match_tokens(&[TokenType::EQUAL])? {
            Some(self.expression()?)
        } else {
            None
        };

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            return Err(self.error_at_current("Expected ';' after variable declaration"));
        }

        Ok(Stmt::Var(name, initializer))
    }

    fn expression(&mut self) -> Result<Expr<'a>, String> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr<'a>, String> {
        let expr: Expr<'a> = self.logic_or()?;

        if self.match_tokens(&[TokenType::EQUAL])? {
            let equals: Token<'a> = self.previous().clone();
            let value: Expr<'a> = self.assignment()?;

            if let Expr::Variable(name) = expr {
                return Ok(Expr::Assign(name, Box::new(value)));
            }

            return Err(self.error_at_token(&equals, "Invalid assignment target"));
        }

        Ok(expr)
    }

    fn logic_or(&mut self) -> Result<Expr<'a>, String> {
        let mut expr: Expr<'a> = self.logic_and()?;

        while self.match_tokens(&[TokenType::OR])? {
            let operator: Token<'a> = self.previous().clone();
            let right: Expr<'a> = self.logic_and()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr<'a>, String> {
        let mut expr: Expr<'a> = self.equality()?;

        while self.match_tokens(&[TokenType::AND])? {
            let operator: Token<'a> = self.previous().clone();
            let right: Expr<'a> = self.equality()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
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
        if self.match_tokens(&[TokenType::IDENTIFIER])? {
            return Ok(Expr::Variable(self.previous().clone()));
        }

        if self.match_tokens(&[TokenType::LEFT_PAREN])? {
            let expr: Expr<'a> = self.expression()?;

            if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
                return Err(self.error_at_current("Expected ')' after expression"));
            }

            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(self.error_at_current("Expected expression"))
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

    fn check_next(&mut self, token_type: &TokenType) -> Result<bool, String> {
        let mut tokens: Peekable<Scanner<'a>> = self.tokens.clone();

        tokens.next();

        match tokens.peek() {
            Some(Ok(token)) => Ok(&token.token_type == token_type),
            _ => Ok(false),
        }
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
            .ok_or_else(|| "Reached EOF".to_string())?
            .as_ref()
            .map_err(|e: &String| e.clone())
    }

    fn previous(&mut self) -> &Token<'a> {
        self.previous.as_ref().expect("No previous token")
    }

    fn error_at_current(&mut self, message: &str) -> String {
        match self.peek() {
            Ok(token) => {
                let token: Token<'a> = token.clone();

                self.error_at_token(&token, message)
            }

            Err(_) => format!("[line unknown] Error: {}", message),
        }
    }

    fn error_at_previous(&mut self, message: &str) -> String {
        let previous: Token<'a> = self.previous().clone();

        self.error_at_token(&previous, message)
    }

    fn error_at_token(&self, token: &Token, message: &str) -> String {
        let lexeme: &str = if token.token_type == TokenType::EOF {
            "end"
        } else {
            token.lexeme
        };

        format!("[line {}] Error at '{}': {}", token.line, lexeme, message)
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Stmt<'a>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek() {
            Ok(token) if token.token_type == TokenType::EOF => None,

            Ok(_) => match self.parse_statement() {
                Ok(stmt) => Some(Ok(stmt)),

                Err(e) => Some(Err(e)),
            },

            Err(e) => Some(Err(e)),
        }
    }
}

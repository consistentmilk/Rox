use crate::expr::Expr;
use crate::scanner::Scanner;
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};
use std::iter::Peekable;

pub struct Parser {
    tokens: Peekable<Scanner>,
    previous: Option<Token>,
}

impl Parser {
    pub fn new(scanner: Scanner) -> Self {
        Parser {
            tokens: scanner.peekable(),
            previous: None,
        }
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        self.expression()
    }

    fn parse_statement(&mut self) -> Result<Stmt, String> {
        if self.match_tokens(&[TokenType::FUN])? {
            return self.function_statement();
        }

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

    fn function_statement(&mut self) -> Result<Stmt, String> {
        if !self.match_tokens(&[TokenType::IDENTIFIER])? {
            return Err(self.error_at_current("Expected function name"));
        }
        let name: Token = self.previous().clone();

        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            return Err(self.error_at_current("Expected '(' after function name"));
        }

        let mut parameters: Vec<Token> = Vec::new();

        if !self.check(&TokenType::RIGHT_PAREN)? {
            loop {
                if parameters.len() >= 8 {
                    return Err(self.error_at_current("Cannot have more than 8 parameters"));
                }

                if !self.match_tokens(&[TokenType::IDENTIFIER])? {
                    return Err(self.error_at_current("Expected parameter name"));
                }

                parameters.push(self.previous().clone());

                if !self.match_tokens(&[TokenType::COMMA])? {
                    break;
                }
            }
        }

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            return Err(self.error_at_current("Expected ')' after parameters"));
        }

        if !self.match_tokens(&[TokenType::LEFT_BRACE])? {
            return Err(self.error_at_current("Expected '{' before function body"));
        }

        let body: Stmt = self.block()?;

        Ok(Stmt::Function(name, parameters, Box::new(body)))
    }

    fn for_statement(&mut self) -> Result<Stmt, String> {
        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            return Err(self.error_at_current("Expected '(' after 'for'"));
        }

        // Initializer
        let initializer: Option<Box<Stmt>> = if self.match_tokens(&[TokenType::SEMICOLON])? {
            None
        } else if self.match_tokens(&[TokenType::VAR])? {
            Some(Box::new(self.parse_var_statement()?))
        } else {
            let expr_stmt: Stmt = match self.expression_statement() {
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
        let condition: Option<Expr> = if !self.check(&TokenType::SEMICOLON)? {
            let cond: Expr = self.expression()?;

            if !self.match_tokens(&[TokenType::SEMICOLON])? {
                return Err(self.error_at_current("Expected ';' after loop condition"));
            }

            Some(cond)
        } else {
            self.match_tokens(&[TokenType::SEMICOLON])?;

            None
        };

        // Increment
        let increment: Option<Expr> = if !self.check(&TokenType::RIGHT_PAREN)? {
            Some(self.expression()?)
        } else {
            None
        };

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            return Err(self.error_at_current("Expected ')' after for clauses"));
        }

        // Body
        let body: Stmt = match self.parse_statement() {
            Ok(stmt) => stmt,

            Err(e) => return Err(e),
        };
        if matches!(body, Stmt::Var(_, _)) {
            return Err(self.error_at_previous("Expected expression"));
        }

        Ok(Stmt::For(initializer, condition, increment, Box::new(body)))
    }

    fn while_statement(&mut self) -> Result<Stmt, String> {
        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            return Err(self.error_at_current("Expected '(' after 'while'"));
        }

        let condition: Expr = self.expression()?;

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            return Err(self.error_at_current("Expected ')' after while condition"));
        }

        let body: Stmt = self.parse_statement()?;

        Ok(Stmt::While(condition, Box::new(body)))
    }

    fn if_statement(&mut self) -> Result<Stmt, String> {
        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            return Err(self.error_at_current("Expected '(' after 'if'"));
        }

        let condition: Expr = self.expression()?;

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            return Err(self.error_at_current("Expected ')' after if condition"));
        }

        let then_branch: Stmt = self.parse_statement()?;

        let else_branch: Option<Box<Stmt>> = if self.match_tokens(&[TokenType::ELSE])? {
            let else_stmt: Stmt = self.parse_statement()?;

            Some(Box::new(else_stmt))
        } else {
            None
        };

        Ok(Stmt::If(condition, Box::new(then_branch), else_branch))
    }

    fn block(&mut self) -> Result<Stmt, String> {
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.check(&TokenType::RIGHT_BRACE)? && !self.is_at_end()? {
            statements.push(self.parse_statement()?);
        }

        if !self.match_tokens(&[TokenType::RIGHT_BRACE])? {
            return Err(self.error_at_current("Expected '}' after block"));
        }

        Ok(Stmt::Block(statements))
    }

    fn assign_statement(&mut self) -> Result<Stmt, String> {
        self.match_tokens(&[TokenType::IDENTIFIER])?;

        let name: Token = self.previous().clone();

        self.match_tokens(&[TokenType::EQUAL])?;

        let value: Expr = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            return Err(self.error_at_current("Expected ';' after assignment"));
        }

        Ok(Stmt::Assign(name, value))
    }

    fn print_statement(&mut self) -> Result<Stmt, String> {
        let value: Expr = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            return Err(if self.is_at_end()? {
                self.error_at_previous("Expected ';' after print statement")
            } else {
                self.error_at_current("Expected ';' after print statement")
            });
        }

        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt, String> {
        let expr: Expr = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            return Err(self.error_at_current("Expected ';' after expression"));
        }

        Ok(Stmt::Expression(expr))
    }

    fn parse_var_statement(&mut self) -> Result<Stmt, String> {
        if !self.match_tokens(&[TokenType::IDENTIFIER])? {
            return Err(self.error_at_current("Expected variable name"));
        }

        let name: Token = self.previous().clone();

        let initializer: Option<Expr> = if self.match_tokens(&[TokenType::EQUAL])? {
            Some(self.expression()?)
        } else {
            None
        };

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            return Err(self.error_at_current("Expected ';' after variable declaration"));
        }

        Ok(Stmt::Var(name, initializer))
    }

    fn expression(&mut self) -> Result<Expr, String> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, String> {
        let expr: Expr = self.logic_or()?;

        if self.match_tokens(&[TokenType::EQUAL])? {
            let equals: Token = self.previous().clone();
            let value: Expr = self.assignment()?;

            if let Expr::Variable(name) = expr {
                return Ok(Expr::Assign(name, Box::new(value)));
            }

            return Err(self.error_at_token(&equals, "Invalid assignment target"));
        }

        Ok(expr)
    }

    fn logic_or(&mut self) -> Result<Expr, String> {
        let mut expr: Expr = self.logic_and()?;

        while self.match_tokens(&[TokenType::OR])? {
            let operator: Token = self.previous().clone();
            let right: Expr = self.logic_and()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr, String> {
        let mut expr: Expr = self.equality()?;

        while self.match_tokens(&[TokenType::AND])? {
            let operator: Token = self.previous().clone();
            let right: Expr = self.equality()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr: Expr = self.comparison()?;

        while self.match_tokens(&[TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL])? {
            let operator: Token = self.previous().clone();
            let right: Expr = self.comparison()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr: Expr = self.term()?;

        while self.match_tokens(&[
            TokenType::GREATER,
            TokenType::GREATER_EQUAL,
            TokenType::LESS,
            TokenType::LESS_EQUAL,
        ])? {
            let operator: Token = self.previous().clone();
            let right: Expr = self.term()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, String> {
        let mut expr: Expr = self.factor()?;

        while self.match_tokens(&[TokenType::MINUS, TokenType::PLUS])? {
            let operator: Token = self.previous().clone();
            let right: Expr = self.factor()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr: Expr = self.unary()?;

        while self.match_tokens(&[TokenType::SLASH, TokenType::STAR])? {
            let operator: Token = self.previous().clone();
            let right: Expr = self.unary()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        if self.match_tokens(&[TokenType::BANG, TokenType::MINUS])? {
            let operator: Token = self.previous().clone();
            let right: Expr = self.unary()?;

            return Ok(Expr::Unary(operator, Box::new(right)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, String> {
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
            let identifier: Token = self.previous().clone();

            if self.check(&TokenType::LEFT_PAREN)? {
                return self.parse_call(Expr::Variable(identifier));
            }

            return Ok(Expr::Variable(identifier));
        }

        if self.match_tokens(&[TokenType::LEFT_PAREN])? {
            let expr: Expr = self.expression()?;

            if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
                return Err(self.error_at_current("Expected ')' after expression"));
            }

            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(self.error_at_current("Expected expression"))
    }

    fn parse_call(&mut self, callee: Expr) -> Result<Expr, String> {
        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            return Err(self.error_at_current("Expected '(' for function call"));
        }

        let mut arguments: Vec<Expr> = Vec::new();

        if !self.check(&TokenType::RIGHT_PAREN)? {
            loop {
                if arguments.len() >= 255 {
                    return Err(self.error_at_current("Cannot have more than 255 arguments"));
                }

                arguments.push(self.expression()?);

                if !self.match_tokens(&[TokenType::COMMA])? {
                    break;
                }
            }
        }

        let paren: Token = if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            return Err(self.error_at_current("Expected ')' after arguments"));
        } else {
            self.previous().clone()
        };

        Ok(Expr::Call(Box::new(callee), paren, arguments))
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

        let peeked: &TokenType = &self.peek()?.token_type;

        match (peeked, token_type) {
            (TokenType::NUMBER(_), TokenType::NUMBER(_)) => Ok(true),

            (TokenType::STRING(_), TokenType::STRING(_)) => Ok(true),

            _ => Ok(peeked == token_type),
        }
    }

    fn check_next(&mut self, token_type: &TokenType) -> Result<bool, String> {
        let mut tokens: Peekable<Scanner> = self.tokens.clone();

        tokens.next();

        match tokens.peek() {
            Some(Ok(token)) => Ok(&token.token_type == token_type),
            _ => Ok(false),
        }
    }

    fn advance(&mut self) -> Result<&Token, String> {
        self.previous = self.tokens.next().transpose()?;

        self.previous
            .as_ref()
            .ok_or_else(|| "Unexpected end of input".to_string())
    }

    fn is_at_end(&mut self) -> Result<bool, String> {
        Ok(self.peek()?.token_type == TokenType::EOF)
    }

    fn peek(&mut self) -> Result<&Token, String> {
        self.tokens
            .peek()
            .ok_or_else(|| "Reached EOF".to_string())?
            .as_ref()
            .map_err(|e: &String| e.clone())
    }

    fn previous(&mut self) -> &Token {
        self.previous.as_ref().expect("No previous token")
    }

    fn error_at_current(&mut self, message: &str) -> String {
        match self.peek() {
            Ok(token) => {
                let token: Token = token.clone();

                self.error_at_token(&token, message)
            }

            Err(_) => format!("[line unknown] Error: {}", message),
        }
    }

    fn error_at_previous(&mut self, message: &str) -> String {
        let previous: Token = self.previous().clone();

        self.error_at_token(&previous, message)
    }

    fn error_at_token(&self, token: &Token, message: &str) -> String {
        let lexeme: &str = if token.token_type == TokenType::EOF {
            "end"
        } else {
            &token.lexeme
        };

        format!("[line {}] Error at '{}': {}", token.line, lexeme, message)
    }
}

impl Iterator for Parser {
    type Item = Result<Stmt, String>;

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

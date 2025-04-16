use std::iter::Peekable;

use log::{debug, info};

use crate::expr::Expr;
use crate::scanner::Scanner;
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};

pub struct Parser {
    tokens: Peekable<Scanner>,
    previous: Option<Token>,
}

impl Parser {
    pub fn new(scanner: Scanner) -> Self {
        info!("Initializing Parser");

        Self {
            tokens: scanner.peekable(),
            previous: None,
        }
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        debug!("Starting expression parsing");

        let expr = self.expression()?;

        info!("Parsed expression successfully");

        Ok(expr)
    }

    fn parse_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parsing statement");

        if self.match_tokens(&[TokenType::FUN])? {
            debug!("Detected FUN token, parsing function statement");
            return self.function_statement();
        }

        if self.match_tokens(&[TokenType::FOR])? {
            debug!("Detected FOR token, parsing for statement");

            return self.for_statement();
        }

        if self.match_tokens(&[TokenType::WHILE])? {
            debug!("Detected WHILE token, parsing while statement");

            return self.while_statement();
        }

        if self.match_tokens(&[TokenType::IF])? {
            debug!("Detected IF token, parsing if statement");

            return self.if_statement();
        }

        if self.match_tokens(&[TokenType::RETURN])? {
            debug!("Detected RETURN token, parsing return statement");
            return self.return_statement();
        }

        if self.match_tokens(&[TokenType::PRINT])? {
            debug!("Detected PRINT token, parsing print statement");

            return self.print_statement();
        }

        if self.match_tokens(&[TokenType::VAR])? {
            debug!("Detected VAR token, parsing var statement");

            return self.parse_var_statement();
        }

        if self.check(&TokenType::IDENTIFIER)? && self.check_next(&TokenType::EQUAL)? {
            debug!("Detected IDENTIFIER followed by EQUAL, parsing assign statement");

            return self.assign_statement();
        }

        if self.match_tokens(&[TokenType::LEFT_BRACE])? {
            debug!("Detected LEFT_BRACE, parsing block statement");

            return self.block();
        }

        debug!("Defaulting to expression statement");

        self.expression_statement()
    }

    fn function_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parsing function statement");

        if !self.match_tokens(&[TokenType::IDENTIFIER])? {
            debug!("Missing function name");

            return Err(self.error_at_current("Expected function name"));
        }

        let name: Token = self.previous().clone();

        debug!("Function name: {}", name.lexeme);

        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            debug!("Missing '(' after function name");

            return Err(self.error_at_current("Expected '(' after function name"));
        }

        let mut parameters: Vec<Token> = Vec::new();

        if !self.check(&TokenType::RIGHT_PAREN)? {
            debug!("Parsing function parameters");

            loop {
                if parameters.len() >= 8 {
                    debug!("Too many parameters");

                    return Err(self.error_at_current("Cannot have more than 8 parameters"));
                }

                if !self.match_tokens(&[TokenType::IDENTIFIER])? {
                    debug!("Missing parameter name");

                    return Err(self.error_at_current("Expected parameter name"));
                }

                let param = self.previous().clone();

                debug!("Parsed parameter: {}", param.lexeme);

                parameters.push(param);

                if !self.match_tokens(&[TokenType::COMMA])? {
                    break;
                }

                debug!("Found comma, expecting another parameter");
            }
        }

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            debug!("Missing ')' after parameters");

            return Err(self.error_at_current("Expected ')' after parameters"));
        }

        if !self.match_tokens(&[TokenType::LEFT_BRACE])? {
            debug!("Missing Left_Brace before function body");

            return Err(self.error_at_current("Expected '{' before function body"));
        }

        debug!("Parsing function body");

        let body: Stmt = self.block()?;

        info!("Parsed function statement: {}", name.lexeme);

        Ok(Stmt::Function(name, parameters, Box::new(body)))
    }

    fn return_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parsing return statement");
        let keyword: Token = self.previous().clone();

        let value: Option<Expr> = if !self.check(&TokenType::SEMICOLON)? {
            debug!("Parsing return value expression");

            Some(self.expression()?)
        } else {
            debug!("No return value");

            None
        };

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            debug!("Missing ';' after return statement");

            return Err(self.error_at_current("Expected ';' after return statement"));
        }

        info!("Parsed return statement");

        Ok(Stmt::Return(keyword, value))
    }

    fn for_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parsing for statement");

        let for_token = self.previous().clone();

        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            debug!("Missing '(' after 'for'");

            return Err(self.error_at_current("Expected '(' after 'for'"));
        }

        debug!("Parsing for initializer");

        let initializer: Option<Box<Stmt>> = if self.match_tokens(&[TokenType::SEMICOLON])? {
            debug!("Empty initializer");

            None
        } else if self.match_tokens(&[TokenType::VAR])? {
            debug!("Parsing var initializer");

            Some(Box::new(self.parse_var_statement()?))
        } else {
            debug!("Parsing expression initializer");

            let expr_stmt: Stmt = match self.expression_statement() {
                Ok(stmt) => stmt,

                Err(e) => {
                    debug!("Error in initializer: {}", e);

                    return Err(e);
                }
            };

            if matches!(expr_stmt, Stmt::Expression(_)) {
                Some(Box::new(expr_stmt))
            } else {
                debug!("Invalid initializer");

                return Err(self.error_at_previous("Expected expression in for initializer"));
            }
        };

        debug!("Parsing for condition");

        let condition: Option<Expr> = if !self.check(&TokenType::SEMICOLON)? {
            let cond: Expr = self.expression()?;

            if !self.match_tokens(&[TokenType::SEMICOLON])? {
                debug!("Missing ';' after loop condition");

                return Err(self.error_at_current("Expected ';' after loop condition"));
            }

            Some(cond)
        } else {
            self.match_tokens(&[TokenType::SEMICOLON])?;

            debug!("Empty condition");

            None
        };

        debug!("Parsing for increment");

        let increment: Option<Expr> = if !self.check(&TokenType::RIGHT_PAREN)? {
            Some(self.expression()?)
        } else {
            debug!("Empty increment");

            None
        };

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            debug!("Missing ')' after for clauses");

            return Err(self.error_at_current("Expected ')' after for clauses"));
        }

        debug!("Parsing for body");

        let body: Stmt = match self.parse_statement() {
            Ok(stmt) => stmt,

            Err(e) => {
                debug!("Error in for body: {}", e);

                return Err(e);
            }
        };

        if let Stmt::Var(_, _) = body {
            return Err(self.error_at_previous(
                "Variables must be declared inside a block, not alone in a for loop",
            ));
        }

        info!("Parsed for statement");

        Ok(Stmt::For(
            for_token.line,
            initializer,
            condition,
            increment,
            Box::new(body),
        ))
    }

    fn while_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parsing while statement");

        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            debug!("Missing '(' after 'while'");

            return Err(self.error_at_current("Expected '(' after 'while'"));
        }

        debug!("Parsing while condition");

        let condition: Expr = self.expression()?;

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            debug!("Missing ')' after while condition");

            return Err(self.error_at_current("Expected ')' after while condition"));
        }

        debug!("Parsing while body");

        let body: Stmt = self.parse_statement()?;

        info!("Parsed while statement");

        Ok(Stmt::While(condition, Box::new(body)))
    }

    fn if_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parsing if statement");

        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            debug!("Missing '(' after 'if'");

            return Err(self.error_at_current("Expected '(' after 'if'"));
        }

        debug!("Parsing if condition");

        let condition: Expr = self.expression()?;

        if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            debug!("Missing ')' after if condition");

            return Err(self.error_at_current("Expected ')' after if condition"));
        }

        debug!("Parsing then branch");

        let then_branch: Stmt = self.parse_statement()?;

        debug!("Checking for else branch");

        let else_branch: Option<Box<Stmt>> = if self.match_tokens(&[TokenType::ELSE])? {
            debug!("Parsing else branch");

            let else_stmt: Stmt = self.parse_statement()?;

            Some(Box::new(else_stmt))
        } else {
            debug!("No else branch");

            None
        };

        info!("Parsed if statement");

        Ok(Stmt::If(condition, Box::new(then_branch), else_branch))
    }

    fn block(&mut self) -> Result<Stmt, String> {
        debug!("Parsing block");

        let left_brace_token = self.previous().clone();

        let mut statements: Vec<Stmt> = Vec::new();

        while !self.check(&TokenType::RIGHT_BRACE)? && !self.is_at_end()? {
            debug!("Parsing statement in block");

            statements.push(self.parse_statement()?);
        }

        if !self.match_tokens(&[TokenType::RIGHT_BRACE])? {
            debug!("Missing Right_Brace after block");

            return Err(self.error_at_current("Expected '}' after block"));
        }

        info!("Parsed block with {} statements", statements.len());

        Ok(Stmt::Block(statements, left_brace_token.line))
    }

    fn assign_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parsing assign statement");

        self.match_tokens(&[TokenType::IDENTIFIER])?;
        let name: Token = self.previous().clone();

        debug!("Assignment target: {}", name.lexeme);

        self.match_tokens(&[TokenType::EQUAL])?;

        debug!("Parsing assignment value");

        let value: Expr = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            debug!("Missing ';' after assignment");

            return Err(self.error_at_current("Expected ';' after assignment"));
        }

        info!("Parsed assign statement for {}", name.lexeme);

        Ok(Stmt::Assign(name, value))
    }

    fn print_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parsing print statement");

        let value: Expr = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            debug!("Missing ';' after print statement");

            return Err(if self.is_at_end()? {
                self.error_at_previous("Expected ';' after print statement")
            } else {
                self.error_at_current("Expected ';' after print statement")
            });
        }

        info!("Parsed print statement");

        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parsing expression statement");

        let expr: Expr = self.expression()?;

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            debug!("Missing ';' after expression");
            return Err(self.error_at_current("Expected ';' after expression"));
        }

        info!("Parsed expression statement");

        Ok(Stmt::Expression(expr))
    }

    fn parse_var_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parsing var statement");

        if !self.match_tokens(&[TokenType::IDENTIFIER])? {
            debug!("Missing variable name");

            return Err(self.error_at_current("Expected variable name"));
        }

        let name: Token = self.previous().clone();

        debug!("Variable name: {}", name.lexeme);

        let initializer: Option<Expr> = if self.match_tokens(&[TokenType::EQUAL])? {
            debug!("Parsing initializer");

            Some(self.expression()?)
        } else {
            debug!("No initializer");

            None
        };

        if !self.match_tokens(&[TokenType::SEMICOLON])? {
            debug!("Missing ';' after variable declaration");

            return Err(self.error_at_current("Expected ';' after variable declaration"));
        }

        info!("Parsed var statement for {}", name.lexeme);

        Ok(Stmt::Var(name, initializer))
    }

    fn expression(&mut self) -> Result<Expr, String> {
        debug!("Parsing expression");

        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, String> {
        debug!("Parsing assignment");

        let expr: Expr = self.logic_or()?;

        if self.match_tokens(&[TokenType::EQUAL])? {
            debug!("Found '=' token");
            let equals: Token = self.previous().clone();
            let value: Expr = self.assignment()?;

            if let Expr::Variable(name) = expr {
                info!("Parsed assignment to {}", name.lexeme);
                return Ok(Expr::Assign(name, Box::new(value)));
            }

            debug!("Invalid assignment target");

            return Err(self.error_at_token(&equals, "Invalid assignment target"));
        }

        Ok(expr)
    }

    fn logic_or(&mut self) -> Result<Expr, String> {
        debug!("Parsing logic_or");
        let mut expr: Expr = self.logic_and()?;

        while self.match_tokens(&[TokenType::OR])? {
            debug!("Found OR operator");

            let operator: Token = self.previous().clone();
            let right: Expr = self.logic_and()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr, String> {
        debug!("Parsing logic_and");
        let mut expr: Expr = self.equality()?;

        while self.match_tokens(&[TokenType::AND])? {
            debug!("Found AND operator");

            let operator: Token = self.previous().clone();
            let right: Expr = self.equality()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, String> {
        debug!("Parsing equality");

        let mut expr: Expr = self.comparison()?;

        while self.match_tokens(&[TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL])? {
            debug!("Found equality operator: {}", self.previous().lexeme);

            let operator: Token = self.previous().clone();
            let right: Expr = self.comparison()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        debug!("Parsing comparison");

        let mut expr: Expr = self.term()?;

        while self.match_tokens(&[
            TokenType::GREATER,
            TokenType::GREATER_EQUAL,
            TokenType::LESS,
            TokenType::LESS_EQUAL,
        ])? {
            debug!("Found comparison operator: {}", self.previous().lexeme);

            let operator: Token = self.previous().clone();
            let right: Expr = self.term()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, String> {
        debug!("Parsing term");

        let mut expr: Expr = self.factor()?;

        while self.match_tokens(&[TokenType::MINUS, TokenType::PLUS])? {
            debug!("Found term operator: {}", self.previous().lexeme);

            let operator: Token = self.previous().clone();
            let right: Expr = self.factor()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        debug!("Parsing factor");

        let mut expr: Expr = self.unary()?;

        while self.match_tokens(&[TokenType::SLASH, TokenType::STAR])? {
            debug!("Found factor operator: {}", self.previous().lexeme);

            let operator: Token = self.previous().clone();
            let right: Expr = self.unary()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        debug!("Parsing unary");

        if self.match_tokens(&[TokenType::BANG, TokenType::MINUS])? {
            debug!("Found unary operator: {}", self.previous().lexeme);

            let operator: Token = self.previous().clone();
            let right: Expr = self.unary()?;

            return Ok(Expr::Unary(operator, Box::new(right)));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, String> {
        let mut expr: Expr = self.primary()?;

        while self.check(&TokenType::LEFT_PAREN)? {
            expr = self.parse_call(expr)?;
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, String> {
        debug!("Parsing primary");

        if self.match_tokens(&[
            TokenType::NUMBER(0.0),
            TokenType::STRING(String::new()),
            TokenType::TRUE,
            TokenType::FALSE,
            TokenType::NIL,
        ])? {
            let literal = self.previous().clone();

            info!("Parsed literal: {}", literal.lexeme);

            return Ok(Expr::Literal(literal));
        }

        if self.match_tokens(&[TokenType::IDENTIFIER])? {
            let identifier: Token = self.previous().clone();

            debug!("Found identifier: {}", identifier.lexeme);

            info!("Parsed variable: {}", identifier.lexeme);

            return Ok(Expr::Variable(identifier));
        }

        if self.match_tokens(&[TokenType::LEFT_PAREN])? {
            debug!("Parsing grouped expression");

            let expr: Expr = self.expression()?;

            if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
                debug!("Missing ')' after grouped expression");

                return Err(self.error_at_current("Expected ')' after expression"));
            }

            info!("Parsed grouped expression");

            return Ok(Expr::Grouping(Box::new(expr)));
        }

        // Enhanced error for keywords
        if self.check(&TokenType::FUN)? {
            debug!("Found 'fun' keyword in expression context");

            return Err(
                self.error_at_current("Function declarations are not allowed in expressions")
            );
        }

        debug!("Expected expression");

        Err(self.error_at_current("Expected expression"))
    }

    fn parse_call(&mut self, callee: Expr) -> Result<Expr, String> {
        debug!("Parsing function call");

        if !self.match_tokens(&[TokenType::LEFT_PAREN])? {
            debug!("Missing '(' for function call");

            return Err(self.error_at_current("Expected '(' for function call"));
        }

        let mut arguments: Vec<Expr> = Vec::new();

        if !self.check(&TokenType::RIGHT_PAREN)? {
            debug!("Parsing call arguments");

            loop {
                if arguments.len() >= 255 {
                    debug!("Too many arguments");

                    return Err(self.error_at_current("Cannot have more than 255 arguments"));
                }

                arguments.push(self.expression()?);

                debug!("Parsed argument #{}", arguments.len());

                if !self.match_tokens(&[TokenType::COMMA])? {
                    break;
                }

                debug!("Found comma, expecting another argument");
            }
        }

        let paren: Token = if !self.match_tokens(&[TokenType::RIGHT_PAREN])? {
            debug!("Missing ')' after arguments");

            return Err(self.error_at_current("Expected ')' after arguments"));
        } else {
            self.previous().clone()
        };

        info!("Parsed function call with {} arguments", arguments.len());

        Ok(Expr::Call(Box::new(callee), paren, arguments))
    }

    fn match_tokens(&mut self, types: &[TokenType]) -> Result<bool, String> {
        debug!("Checking for tokens: {:?}", types);

        for token_type in types {
            if self.check(token_type)? {
                debug!("Matched token: {:?}", token_type);
                self.advance()?;
                return Ok(true);
            }
        }

        debug!("No token match");

        Ok(false)
    }

    fn check(&mut self, token_type: &TokenType) -> Result<bool, String> {
        debug!("Checking if next token is {:?}", token_type);

        if self.is_at_end()? {
            debug!("At end, check failed");

            return Ok(false);
        }

        let peeked: &TokenType = &self.peek()?.token_type;

        let result = match (peeked, token_type) {
            (TokenType::NUMBER(_), TokenType::NUMBER(_)) => true,

            (TokenType::STRING(_), TokenType::STRING(_)) => true,

            _ => peeked == token_type,
        };

        debug!("Check result: {}", result);

        Ok(result)
    }

    fn check_next(&mut self, token_type: &TokenType) -> Result<bool, String> {
        debug!("Checking if token after next is {:?}", token_type);

        let mut tokens: Peekable<Scanner> = self.tokens.clone();

        tokens.next();

        match tokens.peek() {
            Some(Ok(token)) => {
                let result = &token.token_type == token_type;
                debug!("Check_next result: {}", result);
                Ok(result)
            }

            _ => {
                debug!("Check_next failed: no token");
                Ok(false)
            }
        }
    }

    fn advance(&mut self) -> Result<&Token, String> {
        debug!("Advancing parser");

        self.previous = self.tokens.next().transpose()?;

        match &self.previous {
            Some(token) => {
                debug!("Advanced to token: {}", token.lexeme);

                Ok(self.previous.as_ref().unwrap())
            }

            None => {
                debug!("Unexpected end of input");

                Err("Unexpected end of input".to_string())
            }
        }
    }

    fn is_at_end(&mut self) -> Result<bool, String> {
        let at_end = self.peek()?.token_type == TokenType::EOF;

        debug!("Checking is_at_end: {}", at_end);

        Ok(at_end)
    }

    fn peek(&mut self) -> Result<&Token, String> {
        debug!("Peeking at next token");

        self.tokens
            .peek()
            .ok_or_else(|| {
                debug!("Reached EOF during peek");

                "Reached EOF".to_string()
            })?
            .as_ref()
            .map_err(|e: &String| {
                debug!("Peek error: {}", e);

                e.clone()
            })
    }

    fn previous(&mut self) -> &Token {
        debug!("Accessing previous token");

        self.previous.as_ref().expect("No previous token")
    }

    fn error_at_current(&mut self, message: &str) -> String {
        debug!("Generating error at current token: {}", message);

        match self.peek() {
            Ok(token) => {
                let token: Token = token.clone();
                self.error_at_token(&token, message)
            }

            Err(_) => format!("[line unknown] Error: {}", message),
        }
    }

    fn error_at_previous(&mut self, message: &str) -> String {
        debug!("Generating error at previous token: {}", message);

        let previous: Token = self.previous().clone();

        self.error_at_token(&previous, message)
    }

    fn error_at_token(&self, token: &Token, message: &str) -> String {
        let lexeme: &str = if token.token_type == TokenType::EOF {
            "end"
        } else {
            &token.lexeme
        };

        let error = format!("[line {}] Error at '{}': {}", token.line, lexeme, message);

        debug!("Parser error: {}", error);

        error
    }
}

impl Iterator for Parser {
    type Item = Result<Stmt, String>;

    fn next(&mut self) -> Option<Self::Item> {
        debug!("Iterator next called");

        match self.peek() {
            Ok(token) if token.token_type == TokenType::EOF => {
                debug!("Reached EOF, stopping iteration");

                None
            }

            Ok(_) => {
                debug!("Parsing next statement");

                match self.parse_statement() {
                    Ok(stmt) => {
                        info!("Parsed statement successfully");

                        Some(Ok(stmt))
                    }

                    Err(e) => {
                        debug!("Statement parsing error: {}", e);

                        Some(Err(e))
                    }
                }
            }

            Err(e) => {
                debug!("Peek error in iterator: {}", e);

                Some(Err(e))
            }
        }
    }
}

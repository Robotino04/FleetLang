use crate::{
    ast::{Executor, ExecutorHost, Expression, Program, Statement, TopLevelStatement},
    tokenizer::{Keyword, SourceLocation, Token, TokenType},
};

type Result<T> = ::core::result::Result<T, ()>;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub start: SourceLocation,
    pub end: SourceLocation,
    pub message: String,
}

#[derive(Clone, Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            index: 0,
            errors: vec![],
        }
    }

    pub fn errors(&self) -> &Vec<ParseError> {
        return &self.errors;
    }

    fn current_token(&self) -> Option<Token> {
        if self.index < self.tokens.len() {
            Some(self.tokens[self.index].clone())
        } else {
            None
        }
    }
    fn current_token_type(&self) -> Option<TokenType> {
        self.current_token().map(|tok| tok.type_)
    }
    fn is_done(&self) -> bool {
        self.index >= self.tokens.len()
    }
    fn consume(&mut self) -> Option<Token> {
        let t = self.current_token();
        self.index += 1;
        return t;
    }
    fn expect(&mut self, token_type: TokenType) -> Result<Token> {
        if self.current_token_type() == Some(token_type.clone()) {
            return Ok(self.consume().unwrap());
        } else {
            if let Some(token) = self.current_token() {
                self.errors.push(ParseError {
                    start: token.start,
                    end: token.end,
                    message: format!("Expected {:?}, but found {:?}", token_type, token.type_),
                });
                return Err(());
            } else {
                self.errors.push(ParseError {
                    start: self.tokens.last().unwrap().start,
                    end: self.tokens.last().unwrap().end,
                    message: format!("Expected {:?}, but found End of file", token_type),
                });
                return Err(());
            }
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut tls = vec![];

        while !self.is_done() {
            if let Ok(stmt) = self.parse_statement() {
                tls.push(TopLevelStatement::LooseStatement(stmt));
            } else {
                if let Some(token) = self.consume() {
                    self.errors.push(ParseError {
                        start: token.start,
                        end: token.end,
                        message: format!("Unexpected token {:?}", token),
                    });
                }
            }
        }

        Ok(Program {
            toplevel_statements: tls,
        })
    }

    pub fn expect_recovering(
        &mut self,
        main_type: TokenType,
        recovery_stops: Vec<TokenType>,
    ) -> Result<Token> {
        let recovery_start = self.current_token();
        let mut recovery_end = self.current_token();
        loop {
            if recovery_stops
                .iter()
                .any(|t| Some(t) == self.current_token_type().as_ref())
            {
                break;
            } else if Some(main_type.clone()) == self.current_token_type() {
                break;
            } else if self.current_token_type() == None {
                break;
            } else {
                self.consume();
                println!(
                    "recovering from error until one of {:?}, expecting {:?}",
                    recovery_stops, main_type
                );
            }
            recovery_end = self.current_token();
        }
        if recovery_start != recovery_end {
            if let (Some(start), Some(end)) = (recovery_start, recovery_end) {
                self.errors.push(ParseError {
                    start: start.start,
                    end: end.end,
                    message: format!(
                        "Expected {:?}. Recovered by skipping until one of {:?} or {:?}",
                        main_type, recovery_stops, main_type
                    ),
                })
            }
        }
        return self.expect(main_type);
    }

    pub fn parse_statement(&mut self) -> Result<Statement> {
        match self.current_token_type() {
            Some(TokenType::Keyword(Keyword::On)) => {
                let on_token = self.expect(TokenType::Keyword(Keyword::On)).unwrap();
                self.expect(TokenType::OpenParen)?;
                let executor = self.parse_executor()?;
                self.expect(TokenType::CloseParen)?;

                return Ok(Statement::On {
                    on_token,
                    executor,
                    body: Box::new(self.parse_statement()?),
                });
            }
            Some(TokenType::OpenBrace) => {
                self.expect(TokenType::OpenBrace)?;

                let mut body = vec![];
                while self.current_token_type() != Some(TokenType::CloseBrace) {
                    if let Ok(stmt) = self.parse_statement() {
                        body.push(stmt);
                    } else {
                        println!("failed to parse statement");
                        break;
                    }
                }
                self.expect_recovering(TokenType::CloseBrace, vec![])?;

                return Ok(Statement::Block(body));
            }
            _ => {
                if let Ok(exp) = self.parse_expression() {
                    self.expect_recovering(TokenType::Semicolon, vec![TokenType::CloseBrace])?;
                    return Ok(Statement::Expression(exp));
                } else {
                    return Err(());
                }
            }
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression> {
        match self.current_token_type() {
            Some(TokenType::Number(value)) => {
                return Ok(Expression::Number {
                    value,
                    token: self.consume().unwrap(),
                });
            }

            Some(TokenType::Identifier(name)) => {
                let name_token = self.consume().unwrap();
                self.expect(TokenType::OpenParen)?;
                let mut arguments = vec![];
                while self.current_token_type() != Some(TokenType::CloseParen) {
                    arguments.push(self.parse_expression()?);
                }
                self.expect(TokenType::CloseParen)?;
                return Ok(Expression::FunctionCall {
                    name,
                    name_token,
                    arguments,
                });
            }
            _ => Err(()),
        }
    }

    pub fn parse_executor(&mut self) -> Result<Executor> {
        let self_token = self.expect(TokenType::Keyword(Keyword::Self_))?;
        self.expect(TokenType::Dot)?;
        let thread_token = self.expect(TokenType::Identifier("threads".to_string()))?;
        self.expect(TokenType::OpenBracket)?;

        if let Ok(exp) = self.parse_expression() {
            let res = Ok(Executor::Thread {
                thread_token,
                index: exp,
                host: ExecutorHost::Self_ { token: self_token },
            });
            self.expect(TokenType::CloseBracket)?;
            return res;
        } else {
            return Err(());
        }
    }
}

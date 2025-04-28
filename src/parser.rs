use crate::{
    ast::{Executor, ExecutorHost, Expression, FunctionDefinition, Program, Statement, Type},
    infra::FleetError,
    tokenizer::{Keyword, Token, TokenType},
};

type Result<T> = ::core::result::Result<T, ()>;

#[derive(Clone, Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    errors: Vec<FleetError>,
}

macro_rules! expect {
    {$self:ident, = $main_type:expr} => {
        expect!($self, token_type if $main_type == token_type => $self.current_token().unwrap())
    };
    {$self:ident, $main_type:pat} => {
        expect!($self, $main_type => $self.current_token().unwrap())
    };
    {$self:ident, $main_type:pat $(if $guard:expr)? => $body:expr} => {
        {
            match $self.current_token_type() {
                Some($main_type) $(if $guard)? => {
                    let result = Ok($body);
                    $self.consume().unwrap();
                    result
                },
                _ => {
                    if let Some(token) = $self.current_token() {
                        $self.errors.push(FleetError {
                            start: token.start,
                            end: token.end,
                            message: format!("Expected {}, but found {:?}", stringify!($main_type), token.type_),
                        });
                        Err(())
                    } else {
                        $self.errors.push(FleetError {
                            start: $self.tokens.last().unwrap().start,
                            end: $self.tokens.last().unwrap().end,
                            message: format!("Expected {}, but found End of file", stringify!($main_type)),
                        });
                        Err(())
                    }
                }
            }
        }
    }
}

macro_rules! recover_until {
    {$self:ident, $start_of_recovery:ident, $($recovery_stops:pat),+} => {
        {
            loop {
                match $self.current_token_type() {
                    None => break,
                    $(Some($recovery_stops) => break),+,
                    _ => {
                        $self.consume();
                        println!(
                            "Recovering from error until one of [{}]",
                            concat!($(stringify!($recovery_stops)), +)
                        );
                    }
                }
            }
            let recovery_end = $self.current_token();
            if $start_of_recovery != recovery_end {
                if let (Some(start), Some(end)) = ($start_of_recovery, recovery_end) {
                    $self.errors.push(FleetError {
                        start: start.start,
                        end: end.end,
                        message: format!(
                            "Recovered by skipping until one of [{}]",
                            stringify!($($recovery_stops), +)
                        ),
                    })
                }
            }
        }
    };
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens
                .iter()
                .cloned()
                .filter(|tok| !matches!(tok.type_, TokenType::UnknownCharacters(_)))
                .collect(),
            index: 0,
            errors: vec![],
        }
    }

    pub fn errors(&self) -> &Vec<FleetError> {
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

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut functions = vec![];

        while !self.is_done() {
            let recovery_start = self.current_token();
            if let Ok(function) = self.parse_function_definition() {
                functions.push(function);
            } else {
                recover_until!(self, recovery_start, TokenType::Keyword(Keyword::Let));
                /*
                if let Some(token) = self.consume() {
                    self.errors.push(ParseError {
                        start: token.start,
                        end: token.end,
                        message: format!("Unexpected token {:?}", token),
                    });
                }*/
            }
        }

        Ok(Program { functions })
    }

    pub fn parse_statement(&mut self) -> Result<Statement> {
        match self.current_token_type() {
            Some(TokenType::Keyword(Keyword::On)) => {
                let on_token = expect!(self, TokenType::Keyword(Keyword::On))?;
                expect!(self, TokenType::OpenParen)?;
                let executor = self.parse_executor()?;
                expect!(self, TokenType::CloseParen)?;

                return Ok(Statement::On {
                    on_token,
                    executor,
                    body: Box::new(self.parse_statement()?),
                });
            }
            Some(TokenType::OpenBrace) => {
                expect!(self, TokenType::OpenBrace)?;

                let mut body = vec![];
                while self.current_token_type() != Some(TokenType::CloseBrace)
                    && self.current_token_type() != None
                {
                    let recovery_start = self.current_token();
                    if let Ok(stmt) = self.parse_statement() {
                        body.push(stmt);
                    } else {
                        println!("failed to parse statement");
                        recover_until!(
                            self,
                            recovery_start,
                            TokenType::Semicolon,
                            TokenType::CloseBrace
                        );
                        if let Some(TokenType::Semicolon) = self.current_token_type() {
                            expect!(self, TokenType::Semicolon)?;
                        }
                    }
                }
                expect!(self, TokenType::CloseBrace)?;

                return Ok(Statement::Block(body));
            }
            Some(TokenType::Keyword(Keyword::Return)) => {
                let return_token = expect!(self, TokenType::Keyword(Keyword::Return))?;
                if let Ok(value) = self.parse_expression() {
                    expect!(self, TokenType::Semicolon)?;
                    return Ok(Statement::Return {
                        return_token,
                        value,
                    });
                } else {
                    return Err(());
                }
            }
            _ => {
                if let Ok(exp) = self.parse_expression() {
                    expect!(self, TokenType::Semicolon)?;
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
                    token: expect!(self, TokenType::Number(_))?,
                });
            }

            Some(TokenType::Identifier(name)) => {
                let name_token = expect!(self, TokenType::Identifier(_))?;
                expect!(self, TokenType::OpenParen)?;
                let mut arguments = vec![];
                while self.current_token_type() != Some(TokenType::CloseParen) {
                    arguments.push(self.parse_expression()?);
                }
                expect!(self, TokenType::CloseParen)?;
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
        let self_token = expect!(self, TokenType::Keyword(Keyword::Self_))?;
        expect!(self, TokenType::Dot)?;
        let thread_token = expect!(self, = TokenType::Identifier("threads".to_string()))?;
        expect!(self, TokenType::OpenBracket)?;

        if let Ok(exp) = self.parse_expression() {
            let res = Ok(Executor::Thread {
                thread_token,
                index: exp,
                host: ExecutorHost::Self_ { token: self_token },
            });
            expect!(self, TokenType::CloseBracket)?;
            return res;
        } else {
            return Err(());
        }
    }
    pub fn parse_function_definition(&mut self) -> Result<FunctionDefinition> {
        let let_token = expect!(self, TokenType::Keyword(Keyword::Let))?;
        let (name_token, name) =
            expect!(self, TokenType::Identifier(name) => (self.current_token().unwrap(), name))?;

        expect!(self, TokenType::EqualSign)?;
        expect!(self, TokenType::OpenParen)?;

        /*
        let mut params = vec![];

        while self.current_token_type() != Some(TokenType::CloseParen) {
            params.push(expect!(self, TokenType::Identifier(name) => (self.current_token(), name)));
        }
        */

        expect!(self, TokenType::CloseParen)?;
        expect!(self, TokenType::SingleRightArrow)?;
        let return_type = self.parse_type()?;
        let body = self.parse_statement()?;

        Ok(FunctionDefinition {
            name,
            name_token,
            let_token,
            return_type,
            body,
        })
    }

    pub fn parse_type(&mut self) -> Result<Type> {
        match self.current_token_type() {
            Some(TokenType::Keyword(Keyword::I32)) => Ok(Type::I32 {
                token: expect!(self, TokenType::Keyword(Keyword::I32))?,
            }),
            _ => Err(()),
        }
    }
}

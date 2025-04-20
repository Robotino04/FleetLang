use crate::{
    ast::{Executor, ExecutorHost, Expression, Program, Statement},
    tokenizer::{Keyword, Token, TokenType},
};

type Result<T> = ::core::result::Result<T, ()>;

#[derive(Clone, Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0 }
    }

    fn current_token(&self) -> Token {
        self.tokens[self.index].clone()
    }
    fn is_done(&self) -> bool {
        self.index >= self.tokens.len()
    }
    fn consume(&mut self) -> Token {
        let t = self.current_token();
        self.index += 1;
        return t;
    }
    fn expect(&mut self, token_type: TokenType) -> Result<Token> {
        if self.current_token().type_ == token_type {
            return Ok(self.consume());
        } else {
            return Err(());
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut functions = vec![];
        let mut loose_statements = vec![];

        while !self.is_done() {
            if let Ok(stmt) = self.parse_statement() {
                loose_statements.push(stmt);
            } else {
                eprintln!("Unexpected token {:?}", self.current_token());
                self.consume();
            }
        }

        Ok(Program {
            functions,
            loose_statements,
        })
    }

    pub fn parse_statement(&mut self) -> Result<Statement> {
        match self.current_token().type_ {
            TokenType::Keyword(Keyword::On) => {
                let on_token = self.consume();
                self.expect(TokenType::OpenParen)?;
                let executor = self.parse_executor()?;
                self.expect(TokenType::CloseParen)?;
                self.expect(TokenType::OpenBrace)?;

                let mut body = vec![];
                while self.current_token().type_ != TokenType::CloseBrace {
                    body.push(self.parse_statement()?);
                }
                self.expect(TokenType::CloseBrace)?;
                return Ok(Statement::On {
                    on_token,
                    executor,
                    body,
                });
            }
            _ => {
                if let Ok(exp) = self.parse_expression() {
                    self.expect(TokenType::Semicolon)?;
                    return Ok(Statement::Expression(exp));
                } else {
                    return Err(());
                }
            }
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression> {
        match self.current_token().type_ {
            TokenType::Number(value) => {
                return Ok(Expression::Number {
                    value,
                    token: self.consume(),
                });
            }

            TokenType::Identifier(name) => {
                let name_token = self.consume();
                self.expect(TokenType::OpenParen)?;
                let mut arguments = vec![];
                while self.current_token().type_ != TokenType::CloseParen {
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

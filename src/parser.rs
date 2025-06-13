use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        BinaryExpression, BinaryOperation, BlockStatement, BoolExpression, BoolType,
        BreakStatement, CastExpression, Executor, ExecutorHost, Expression, ExpressionStatement,
        ExternFunctionBody, ForLoopStatement, FunctionBody, FunctionCallExpression,
        FunctionDefinition, GroupingExpression, IdkType, IfStatement, IntType, NodeID,
        NumberExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost, SimpleBinding,
        SkipStatement, Statement, StatementFunctionBody, ThreadExecutor, Type, UnaryExpression,
        UnaryOperation, UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, WhileLoopStatement,
    },
    infra::{ErrorSeverity, FleetError},
    passes::type_propagation::{FunctionID, RuntimeType, VariableID},
    tokenizer::{Keyword, Token, TokenType},
};

type Result<T> = ::core::result::Result<T, ()>;

#[derive(Debug)]
pub struct Parser<'errors> {
    tokens: Vec<Token>,
    index: usize,
    errors: &'errors mut Vec<FleetError>,
    id_generator: IdGenerator,
}

#[derive(Debug, Clone)]
pub struct IdGenerator {
    node_id_counter: NodeID,
    variable_id_counter: VariableID,
    function_id_counter: FunctionID,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self {
            node_id_counter: NodeID(0),
            variable_id_counter: VariableID(0),
            function_id_counter: FunctionID(0),
        }
    }

    pub fn next_node_id(&mut self) -> NodeID {
        let current_id = self.node_id_counter;
        self.node_id_counter.0 += 1;
        return current_id;
    }
    pub fn next_variable_id(&mut self) -> VariableID {
        let current_id = self.variable_id_counter;
        self.variable_id_counter.0 += 1;
        return current_id;
    }

    pub fn next_function_id(&mut self) -> FunctionID {
        let current_id = self.function_id_counter;
        self.function_id_counter.0 += 1;
        return current_id;
    }
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
                        $self.errors.push(FleetError::from_token(
                            &token,
                            format!("Expected {}, but found {:?}", stringify!($main_type), token.type_),
                            ErrorSeverity::Error,
                        ));
                        Err(())
                    } else {
                        $self.errors.push(FleetError::from_token(
                            $self.tokens.last().unwrap(),
                            format!("Expected {}, but found End of file", stringify!($main_type)),
                            ErrorSeverity::Error,
                        ));
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
                        eprintln!(
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
                        severity: ErrorSeverity::Warning,
                    })
                }
            }
        }
    };
}

macro_rules! unable_to_parse {
    ($self:ident, $fmt_string:expr $(, $($param:expr)+)?) => {
        if let Some(token) = $self.current_token() {
            $self.errors.push(FleetError::from_token(
                &token,
                format!("Unable to parse an expected {}", format!($fmt_string $(, $($param),+)?)),
                ErrorSeverity::Error,
            ));
            return Err(());
        } else {
            $self.errors.push(FleetError::from_token(
                $self.tokens.last().unwrap(),
                format!("Hit EOF while parsing an expected {}", format!($fmt_string $(, $($param),+)?)),
                ErrorSeverity::Error,
            ));
            return Err(());
        }
    };
}

impl<'errors> Parser<'errors> {
    pub fn new(tokens: Vec<Token>, errors: &'errors mut Vec<FleetError>) -> Self {
        Self {
            tokens: tokens
                .iter()
                .cloned()
                .filter(|tok| !matches!(tok.type_, TokenType::UnknownCharacters(_)))
                .collect(),
            index: 0,
            errors,
            id_generator: IdGenerator::new(),
        }
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

    pub fn parse_program(mut self) -> Result<(Program, IdGenerator)> {
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

        Ok((
            Program {
                functions,
                id: self.id_generator.next_node_id(),
            },
            self.id_generator,
        ))
    }

    pub fn parse_function_definition(&mut self) -> Result<FunctionDefinition> {
        let let_token = expect!(self, TokenType::Keyword(Keyword::Let))?;
        let (name_token, name) =
            expect!(self, TokenType::Identifier(name) => (self.current_token().unwrap(), name))?;

        let equal_token = expect!(self, TokenType::EqualSign)?;
        let open_paren_token = expect!(self, TokenType::OpenParen)?;

        let mut parameters = vec![];

        while self.current_token_type() != Some(TokenType::CloseParen) {
            let binding = self.parse_simple_binding()?;
            match self.current_token_type() {
                Some(TokenType::Comma) => {
                    parameters.push((binding, Some(expect!(self, TokenType::Comma)?)))
                }
                _ => {
                    parameters.push((binding, None));
                    break;
                }
            }
        }

        let close_paren_token = expect!(self, TokenType::CloseParen)?;
        let right_arrow_token = expect!(self, TokenType::SingleRightArrow)?;
        let mut moved_errors = vec![];
        std::mem::swap(&mut moved_errors, self.errors);
        let return_type = self.parse_type().ok();
        std::mem::swap(&mut moved_errors, self.errors);
        let body = self.parse_function_body()?;

        Ok(FunctionDefinition {
            let_token,
            name,
            name_token,
            equal_token,
            open_paren_token,
            parameters,
            close_paren_token,
            right_arrow_token,
            return_type,
            body,
            id: self.id_generator.next_node_id(),
        })
    }
    pub fn parse_function_body(&mut self) -> Result<FunctionBody> {
        match self.current_token_type() {
            Some(TokenType::At) => {
                let at_token = expect!(self, TokenType::At)?;
                let extern_token = expect!(self, TokenType::Keyword(Keyword::Extern))?;
                let (symbol_token, symbol) = expect!(self, TokenType::StringLiteral(symbol) => (self.current_token().unwrap(), symbol))?;
                let semicolon_token = expect!(self, TokenType::Semicolon)?;
                return Ok(FunctionBody::Extern(ExternFunctionBody {
                    at_token,
                    extern_token,
                    symbol,
                    symbol_token,
                    semicolon_token,
                    id: self.id_generator.next_node_id(),
                }));
            }
            _ => {
                return Ok(FunctionBody::Statement(StatementFunctionBody {
                    statement: self.parse_statement()?,
                    id: self.id_generator.next_node_id(),
                }));
            }
        }
    }

    pub fn parse_simple_binding(&mut self) -> Result<SimpleBinding> {
        let (name_token, name) =
            expect!(self, TokenType::Identifier(name) => (self.current_token().unwrap(), name))?;

        let type_ = if self.current_token_type() == Some(TokenType::Colon) {
            Some((expect!(self, TokenType::Colon)?, self.parse_type()?))
        } else {
            None
        };

        return Ok(SimpleBinding {
            name_token,
            name,
            type_,
            id: self.id_generator.next_node_id(),
        });
    }

    pub fn parse_statement(&mut self) -> Result<Statement> {
        match self.current_token_type() {
            Some(TokenType::Keyword(Keyword::On)) => {
                return Ok(Statement::On(OnStatement {
                    on_token: expect!(self, TokenType::Keyword(Keyword::On))?,
                    open_paren_token: expect!(self, TokenType::OpenParen)?,
                    executor: self.parse_executor()?,
                    close_paren_token: expect!(self, TokenType::CloseParen)?,
                    body: Box::new(self.parse_statement()?),
                    id: self.id_generator.next_node_id(),
                }));
            }
            Some(TokenType::OpenBrace) => {
                let open_brace_token = expect!(self, TokenType::OpenBrace)?;

                let mut body = vec![];
                while self.current_token_type() != Some(TokenType::CloseBrace)
                    && self.current_token_type() != None
                {
                    let recovery_start = self.current_token();
                    if let Ok(stmt) = self.parse_statement() {
                        body.push(stmt);
                    } else {
                        eprintln!("failed to parse statement");
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
                let close_brace_token = expect!(self, TokenType::CloseBrace)?;

                return Ok(Statement::Block(BlockStatement {
                    open_brace_token,
                    body,
                    close_brace_token,
                    id: self.id_generator.next_node_id(),
                }));
            }
            Some(TokenType::Keyword(Keyword::Return)) => {
                let return_token = expect!(self, TokenType::Keyword(Keyword::Return))?;

                let value = if let Some(TokenType::Semicolon) = self.current_token_type() {
                    None
                } else {
                    Some(self.parse_expression()?)
                };

                return Ok(Statement::Return(ReturnStatement {
                    return_token,
                    value,
                    semicolon_token: expect!(self, TokenType::Semicolon)?,
                    id: self.id_generator.next_node_id(),
                }));
            }
            Some(TokenType::Keyword(Keyword::Let)) => {
                let let_token = expect!(self, TokenType::Keyword(Keyword::Let))?;

                return Ok(Statement::VariableDefinition(VariableDefinitionStatement {
                    let_token,
                    binding: self.parse_simple_binding()?,
                    equals_token: expect!(self, TokenType::EqualSign)?,
                    value: self.parse_expression()?,
                    semicolon_token: expect!(self, TokenType::Semicolon)?,
                    id: self.id_generator.next_node_id(),
                }));
            }
            Some(TokenType::Keyword(Keyword::If)) => {
                let if_token = expect!(self, TokenType::Keyword(Keyword::If))?;
                let condition = self.parse_expression()?;
                let if_body = self.parse_statement()?;

                let mut elifs = vec![];

                while let Some(TokenType::Keyword(Keyword::Elif)) = self.current_token_type() {
                    let elif_token = expect!(self, TokenType::Keyword(Keyword::Elif))?;
                    let elif_condition = self.parse_expression()?;
                    let elif_body = self.parse_statement()?;
                    elifs.push((elif_token, elif_condition, elif_body));
                }

                let mut else_ = None;
                if let Some(TokenType::Keyword(Keyword::Else)) = self.current_token_type() {
                    else_ = Some((
                        expect!(self, TokenType::Keyword(Keyword::Else))?,
                        Box::new(self.parse_statement()?),
                    ));
                }

                return Ok(Statement::If(IfStatement {
                    if_token,
                    condition,
                    if_body: Box::new(if_body),
                    elifs,
                    else_,
                    id: self.id_generator.next_node_id(),
                }));
            }
            Some(TokenType::Keyword(Keyword::While)) => {
                let while_token = expect!(self, TokenType::Keyword(Keyword::While))?;
                let condition = self.parse_expression()?;
                let body = self.parse_statement()?;
                return Ok(Statement::WhileLoop(WhileLoopStatement {
                    while_token,
                    condition,
                    body: Box::new(body),
                    id: self.id_generator.next_node_id(),
                }));
            }
            Some(TokenType::Keyword(Keyword::For)) => {
                let for_token = expect!(self, TokenType::Keyword(Keyword::For))?;
                let open_paren_token = expect!(self, TokenType::OpenParen)?;

                let initializer = self.parse_statement()?;

                let condition = if matches!(self.current_token_type(), Some(TokenType::Semicolon)) {
                    None
                } else {
                    Some(self.parse_expression()?)
                };
                let second_semicolon_token = expect!(self, TokenType::Semicolon)?;

                let incrementer =
                    if matches!(self.current_token_type(), Some(TokenType::CloseParen)) {
                        None
                    } else {
                        Some(self.parse_expression()?)
                    };
                let close_paren_token = expect!(self, TokenType::CloseParen)?;

                let body = self.parse_statement()?;
                return Ok(Statement::ForLoop(ForLoopStatement {
                    for_token,
                    open_paren_token,
                    initializer: Box::new(initializer),
                    condition,
                    second_semicolon_token,
                    incrementer,
                    close_paren_token,
                    body: Box::new(body),
                    id: self.id_generator.next_node_id(),
                }));
            }
            Some(TokenType::Keyword(Keyword::Break)) => {
                return Ok(Statement::Break(BreakStatement {
                    break_token: expect!(self, TokenType::Keyword(Keyword::Break))?,
                    semicolon_token: expect!(self, TokenType::Semicolon)?,
                    id: self.id_generator.next_node_id(),
                }));
            }
            Some(TokenType::Keyword(Keyword::Skip)) => {
                return Ok(Statement::Skip(SkipStatement {
                    skip_token: expect!(self, TokenType::Keyword(Keyword::Skip))?,
                    semicolon_token: expect!(self, TokenType::Semicolon)?,
                    id: self.id_generator.next_node_id(),
                }));
            }
            _ => {
                return Ok(Statement::Expression(ExpressionStatement {
                    expression: self.parse_expression()?,
                    semicolon_token: expect!(self, TokenType::Semicolon)?,
                    id: self.id_generator.next_node_id(),
                }));
            }
        }
    }
    fn parse_expression(&mut self) -> Result<Expression> {
        return self.parse_assignment_expression();
    }

    fn parse_primary_expression(&mut self) -> Result<Expression> {
        match self.current_token_type() {
            Some(TokenType::Number(value)) => {
                return Ok(Expression::Number(NumberExpression {
                    value,
                    token: expect!(self, TokenType::Number(_))?,
                    id: self.id_generator.next_node_id(),
                }));
            }
            Some(TokenType::Keyword(Keyword::True)) => {
                return Ok(Expression::Bool(BoolExpression {
                    value: true,
                    token: expect!(self, TokenType::Keyword(Keyword::True))?,
                    id: self.id_generator.next_node_id(),
                }));
            }
            Some(TokenType::Keyword(Keyword::False)) => {
                return Ok(Expression::Bool(BoolExpression {
                    value: false,
                    token: expect!(self, TokenType::Keyword(Keyword::False))?,
                    id: self.id_generator.next_node_id(),
                }));
            }

            Some(TokenType::Identifier(name)) => {
                let name_token = expect!(self, TokenType::Identifier(_))?;
                match self.current_token_type() {
                    Some(TokenType::OpenParen) => {
                        let open_paren_token = expect!(self, TokenType::OpenParen)?;
                        let mut arguments = vec![];
                        while self.current_token_type() != Some(TokenType::CloseParen) {
                            let arg = self.parse_expression()?;

                            match self.current_token_type() {
                                Some(TokenType::Comma) => {
                                    arguments.push((arg, Some(expect!(self, TokenType::Comma)?)))
                                }
                                _ => {
                                    arguments.push((arg, None));
                                    break;
                                }
                            }
                        }
                        let close_paren_token = expect!(self, TokenType::CloseParen)?;
                        return Ok(Expression::FunctionCall(FunctionCallExpression {
                            name,
                            name_token,
                            arguments,
                            open_paren_token,
                            close_paren_token,
                            id: self.id_generator.next_node_id(),
                        }));
                    }
                    _ => {
                        return Ok(Expression::VariableAccess(VariableAccessExpression {
                            name,
                            name_token,
                            id: self.id_generator.next_node_id(),
                        }));
                    }
                }
            }
            Some(TokenType::OpenParen) => {
                return Ok(Expression::Grouping(GroupingExpression {
                    open_paren_token: expect!(self, TokenType::OpenParen)?,
                    subexpression: Box::new(self.parse_expression()?),
                    close_paren_token: expect!(self, TokenType::CloseParen)?,
                    id: self.id_generator.next_node_id(),
                }));
            }
            _ => unable_to_parse!(self, "primary expression"),
        }
    }
    fn parse_unary_expression(&mut self) -> Result<Expression> {
        match self.current_token_type() {
            Some(TokenType::Tilde) => Ok(Expression::Unary(UnaryExpression {
                operator_token: expect!(self, TokenType::Tilde)?,
                operation: UnaryOperation::BitwiseNot,
                operand: Box::new(self.parse_unary_expression()?),
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Minus) => Ok(Expression::Unary(UnaryExpression {
                operator_token: expect!(self, TokenType::Minus)?,
                operation: UnaryOperation::Negate,
                operand: Box::new(self.parse_unary_expression()?),
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::ExclamationMark) => Ok(Expression::Unary(UnaryExpression {
                operator_token: expect!(self, TokenType::ExclamationMark)?,
                operation: UnaryOperation::LogicalNot,
                operand: Box::new(self.parse_unary_expression()?),
                id: self.id_generator.next_node_id(),
            })),

            Some(_) => return self.parse_primary_expression(),
            None => unable_to_parse!(self, "unary expression"),
        }
    }
    fn parse_cast_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_unary_expression()?;

        while match self.current_token_type() {
            Some(TokenType::Keyword(Keyword::As)) => {
                let as_token = expect!(self, TokenType::Keyword(Keyword::As))?;
                let type_ = self.parse_type()?;
                left = Expression::Cast(CastExpression {
                    operand: Box::new(left),
                    as_token,
                    type_,
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            _ => false,
        } {}

        return Ok(left);
    }
    fn parse_product_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_cast_expression()?;

        while match self.current_token_type() {
            Some(TokenType::Star) => {
                let operator_token = expect!(self, TokenType::Star)?;
                let right = self.parse_cast_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::Multiply,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            Some(TokenType::Slash) => {
                let operator_token = expect!(self, TokenType::Slash)?;
                let right = self.parse_cast_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::Divide,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            Some(TokenType::Percent) => {
                let operator_token = expect!(self, TokenType::Percent)?;
                let right = self.parse_cast_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::Modulo,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            _ => false,
        } {}

        return Ok(left);
    }
    fn parse_sum_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_product_expression()?;

        while match self.current_token_type() {
            Some(TokenType::Plus) => {
                let operator_token = expect!(self, TokenType::Plus)?;
                let right = self.parse_product_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::Add,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            Some(TokenType::Minus) => {
                let operator_token = expect!(self, TokenType::Minus)?;
                let right = self.parse_product_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::Subtract,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            _ => false,
        } {}

        return Ok(left);
    }
    fn parse_comparison_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_sum_expression()?;

        while match self.current_token_type() {
            Some(TokenType::LessThan) => {
                let operator_token = expect!(self, TokenType::LessThan)?;
                let right = self.parse_sum_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::LessThan,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            Some(TokenType::LessThanEqual) => {
                let operator_token = expect!(self, TokenType::LessThanEqual)?;
                let right = self.parse_sum_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::LessThanOrEqual,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            Some(TokenType::GreaterThan) => {
                let operator_token = expect!(self, TokenType::GreaterThan)?;
                let right = self.parse_sum_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::GreaterThan,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            Some(TokenType::GreaterThanEqual) => {
                let operator_token = expect!(self, TokenType::GreaterThanEqual)?;
                let right = self.parse_sum_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::GreaterThanOrEqual,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            _ => false,
        } {}

        return Ok(left);
    }
    fn parse_equality_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_comparison_expression()?;

        while match self.current_token_type() {
            Some(TokenType::DoubleEqual) => {
                let operator_token = expect!(self, TokenType::DoubleEqual)?;
                let right = self.parse_comparison_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::Equal,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            Some(TokenType::NotEqual) => {
                let operator_token = expect!(self, TokenType::NotEqual)?;
                let right = self.parse_comparison_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::NotEqual,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            _ => false,
        } {}

        return Ok(left);
    }
    fn parse_logical_and_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_equality_expression()?;

        while match self.current_token_type() {
            Some(TokenType::DoubleAmpersand) => {
                let operator_token = expect!(self, TokenType::DoubleAmpersand)?;
                let right = self.parse_equality_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::LogicalAnd,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            _ => false,
        } {}

        return Ok(left);
    }
    fn parse_logical_or_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_logical_and_expression()?;

        while match self.current_token_type() {
            Some(TokenType::DoublePipe) => {
                let operator_token = expect!(self, TokenType::DoublePipe)?;
                let right = self.parse_logical_and_expression()?;
                left = Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    operator_token,
                    operation: BinaryOperation::LogicalOr,
                    right: Box::new(right),
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            _ => false,
        } {}

        return Ok(left);
    }
    fn parse_assignment_expression(&mut self) -> Result<Expression> {
        let left = self.parse_logical_or_expression()?;

        match left {
            Expression::VariableAccess(VariableAccessExpression {
                name,
                name_token,
                id: _,
            }) if matches!(self.current_token_type(), Some(TokenType::EqualSign)) => {
                let equal_token = expect!(self, TokenType::EqualSign)?;
                let value = self.parse_assignment_expression()?;

                return Ok(Expression::VariableAssignment(
                    VariableAssignmentExpression {
                        name,
                        name_token,
                        equal_token,
                        right: Box::new(value),
                        id: self.id_generator.next_node_id(),
                    },
                ));
            }
            _ => {
                return Ok(left);
            }
        };
    }

    pub fn parse_executor(&mut self) -> Result<Executor> {
        let res = Ok(Executor::Thread(ThreadExecutor {
            host: ExecutorHost::Self_(SelfExecutorHost {
                token: expect!(self, TokenType::Keyword(Keyword::Self_))?,
                id: self.id_generator.next_node_id(),
            }),
            dot_token: expect!(self, TokenType::Dot)?,
            thread_token: expect!(self, = TokenType::Identifier("threads".to_string()))?,
            open_bracket_token: expect!(self, TokenType::OpenBracket)?,
            index: self.parse_expression()?,
            close_bracket_token: expect!(self, TokenType::CloseBracket)?,
            id: self.id_generator.next_node_id(),
        }));
        return res;
    }

    pub fn parse_type(&mut self) -> Result<Type> {
        match self.current_token_type() {
            Some(TokenType::Keyword(Keyword::I8)) => Ok(Type::Int(IntType {
                token: expect!(self, TokenType::Keyword(Keyword::I8))?,
                type_: RuntimeType::I8,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::I16)) => Ok(Type::Int(IntType {
                token: expect!(self, TokenType::Keyword(Keyword::I16))?,
                type_: RuntimeType::I16,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::I32)) => Ok(Type::Int(IntType {
                token: expect!(self, TokenType::Keyword(Keyword::I32))?,
                type_: RuntimeType::I32,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::I64)) => Ok(Type::Int(IntType {
                token: expect!(self, TokenType::Keyword(Keyword::I64))?,
                type_: RuntimeType::I64,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::Bool)) => Ok(Type::Bool(BoolType {
                token: expect!(self, TokenType::Keyword(Keyword::Bool))?,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::Idk)) => Ok(Type::Idk(IdkType {
                type_: Rc::new(RefCell::new(RuntimeType::Unknown)),
                token: expect!(self, TokenType::Keyword(Keyword::Idk))?,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::OpenParen) => Ok(Type::Unit(UnitType {
                open_paren_token: expect!(self, TokenType::OpenParen)?,
                close_paren_token: expect!(self, TokenType::CloseParen)?,
                id: self.id_generator.next_node_id(),
            })),
            _ => unable_to_parse!(self, "type"),
        }
    }
}

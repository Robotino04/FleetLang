use std::cell::{Ref, RefMut};

use either::Either;
use itertools::Itertools;
use log::{error, warn};
use thiserror::Error;

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, BinaryExpression,
        BinaryOperation, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
        Executor, ExecutorHost, Expression, ExpressionStatement, ExternFunctionBody,
        ForLoopStatement, FunctionBody, FunctionCallExpression, FunctionDefinition, GPUExecutor,
        GroupingExpression, GroupingLValue, IdkType, IfStatement, LValue, LiteralExpression,
        LiteralKind, NodeID, OnStatement, OnStatementIterator, Program, ReturnStatement,
        SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement, Statement,
        StatementFunctionBody, StructAccessExpression, StructAccessLValue, StructExpression,
        StructMemberDefinition, StructMemberValue, StructType, ThreadExecutor, TopLevelStatement,
        Type, TypeAlias, UnaryExpression, UnaryOperation, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue,
        WhileLoopStatement,
    },
    infra::{ErrorSeverity, FleetError},
    passes::{
        pass_manager::{
            Errors, GlobalState, InputSource, Pass, PassError, PassFactory, PassResult,
        },
        runtime_type::RuntimeType,
        scope_analysis::{FunctionID, VariableID},
    },
    tokenizer::{Keyword, SourceRange, Token, TokenType},
};

#[derive(Error, Debug)]
#[error(
    "{file_name}:{start_line}:{start_column} - {end_line}:{end_column}: {msg}",
    start_line = .range.start.line,
    start_column = .range.start.column,
    end_line = .range.end.line,
    end_column = .range.end.column
)]
pub struct ParserError {
    pub file_name: String,
    pub msg: String,
    pub range: SourceRange,
}

impl From<FleetError> for ParserError {
    fn from(value: FleetError) -> Self {
        Self {
            file_name: "unknown_file.fl".to_string(), // TODO: track filename in FleetError
            range: value.start().until(value.end()),
            msg: value.message,
        }
    }
}

type Result<T> = ::core::result::Result<T, ParserError>;

#[derive(Debug, Clone)]
pub struct IdGenerator {
    node_id_counter: NodeID,
    variable_id_counter: VariableID,
    function_id_counter: FunctionID,
}

impl IdGenerator {
    pub fn next_node_id(&mut self) -> NodeID {
        let current_id = self.node_id_counter;
        self.node_id_counter.0 += 1;
        current_id
    }
    pub fn next_variable_id(&mut self) -> VariableID {
        let current_id = self.variable_id_counter;
        self.variable_id_counter.0 += 1;
        current_id
    }

    pub fn next_function_id(&mut self) -> FunctionID {
        let current_id = self.function_id_counter;
        self.function_id_counter.0 += 1;
        current_id
    }
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self {
            node_id_counter: NodeID(0),
            variable_id_counter: VariableID(0),
            function_id_counter: FunctionID(0),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'state> {
    errors: RefMut<'state, Errors>,
    tokens: Ref<'state, Vec<Token>>,

    id_generator: RefMut<'state, IdGenerator>,
    program: RefMut<'state, Program>,

    index: usize,
}

impl PassFactory for Parser<'_> {
    type Output<'state> = Parser<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> ::core::result::Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        let tokens = state.check_named::<Vec<Token>>()?;
        let errors = state.check_named()?;

        let mut id_generator = IdGenerator::default();
        let file_name = state.get_named::<InputSource>()?.file_name.clone();
        let program = state.insert(Program {
            top_level_statements: vec![],
            id: id_generator.next_node_id(),
            file_name,
        });
        let id_generator = state.insert(id_generator);

        let tokens = tokens.get(state);
        let index = tokens
            .iter()
            .find_position(|token| !matches!(token.type_, TokenType::UnknownCharacters(_)))
            .map(|(index, _)| index)
            .unwrap_or(tokens.len()); // all unknown characters means we treat this like an empty vec

        Ok(Self::Output {
            errors: errors.get_mut(state),
            tokens,

            program: program.get_mut(state),
            id_generator: id_generator.get_mut(state),

            index,
        })
    }
}
impl Pass for Parser<'_> {
    fn run<'state>(self: Box<Self>) -> PassResult {
        self.parse_program()
            .map_err(|err| PassError::CompilerError {
                source: err.into(),
                producing_pass: Self::name(),
            })?;

        Ok(())
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
                    let result: Result<_> = Ok($body);
                    $self.consume().unwrap();
                    result
                },
                _ => {
                    if let Some(token) = $self.current_token() {
                        let err = FleetError::from_token(
                            &token,
                            format!("Expected {}, but found {:?}", stringify!($main_type), token.type_),
                            ErrorSeverity::Error,
                        );
                        $self.errors.push(err.clone());
                        Err(err.into())
                    } else {
                        let err = FleetError::from_token(
                            $self.tokens.last().unwrap(),
                            format!("Expected {}, but found End of file", stringify!($main_type)),
                            ErrorSeverity::Error,
                        );
                        $self.errors.push(err.clone());
                        Err(err.into())
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
                        warn!(
                            "Recovering from error until one of [{}]",
                            concat!($(stringify!($recovery_stops)), +)
                        );
                    }
                }
            }
            let recovery_end = $self.current_token();
            if $start_of_recovery != recovery_end {
                if let (Some(start), Some(end)) = ($start_of_recovery, recovery_end) {
                    $self.errors.push(
                        FleetError::try_new(
                            vec![start.range.start.until(end.range.end)],
                            format!(
                                "Recovered by skipping until one of [{}]",
                                stringify!($($recovery_stops), +)
                            ),
                            ErrorSeverity::Warning,
                            start.file_name,
                        )
                        .unwrap(),
                    )
                }
            }
        }
    };
}

macro_rules! unable_to_parse {
    ($self:ident, $fmt_string:expr $(, $($param:expr)+)?) => {
        if let Some(token) = $self.current_token() {
            let err = FleetError::from_token(
                &token,
                format!("Unable to parse an expected {}", format!($fmt_string $(, $($param),+)?)),
                ErrorSeverity::Error,
            );
            $self.errors.push(err.clone());
            return Err(err.into());
        } else {
            let err = FleetError::from_token(
                $self.tokens.last().unwrap(),
                format!("Hit EOF while parsing an expected {}", format!($fmt_string $(, $($param),+)?)),
                ErrorSeverity::Error,
            );
            $self.errors.push(err.clone());
            return Err(err.into());
        }
    };
}

impl<'state> Parser<'state> {
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
        while let Some(TokenType::UnknownCharacters(_)) = self.current_token_type() {
            self.index += 1;
        }
        t
    }

    fn try_parse<F, T>(
        &mut self,
        callback: F,
    ) -> ::core::result::Result<T, (ParserError, Vec<FleetError>)>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        let mut tmp_errors = vec![];
        let tmp_index = self.index;
        std::mem::swap(&mut tmp_errors, &mut self.errors);
        let result = callback(self);
        std::mem::swap(&mut tmp_errors, &mut self.errors);

        match result {
            Ok(ok) => {
                // don't silently drop errors
                self.errors.append(&mut tmp_errors);

                Ok(ok)
            }
            Err(err) => {
                self.index = tmp_index;

                Err((err, tmp_errors))
            }
        }
    }

    pub fn parse_program(mut self) -> Result<()> {
        while !self.is_done() {
            let recovery_start = self.current_token();

            if let Ok(type_alias) = self.try_parse(|this| this.parse_type_alias()) {
                self.program
                    .top_level_statements
                    .push(TopLevelStatement::TypeAlias(type_alias));
            } else if let Ok(function) = self.parse_function_definition() {
                self.program
                    .top_level_statements
                    .push(TopLevelStatement::Function(function));
            } else {
                recover_until!(self, recovery_start, TokenType::Keyword(Keyword::Let));
            }
        }

        Ok(())
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
        let return_type = self.try_parse(|this| this.parse_type()).ok();
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
    pub fn parse_type_alias(&mut self) -> Result<TypeAlias> {
        let let_token = expect!(self, TokenType::Keyword(Keyword::Let))?;
        let (name_token, name) =
            expect!(self, TokenType::Identifier(name) => (self.current_token().unwrap(), name))?;

        let equal_token = expect!(self, TokenType::EqualSign)?;
        let type_ = self.parse_type()?;
        let semicolon_token = expect!(self, TokenType::Semicolon)?;

        Ok(TypeAlias {
            let_token,
            name,
            name_token,
            equal_token,
            type_,
            semicolon_token,
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

                Ok(FunctionBody::Extern(ExternFunctionBody {
                    at_token,
                    extern_token,
                    symbol,
                    symbol_token,
                    semicolon_token,
                    id: self.id_generator.next_node_id(),
                }))
            }
            _ => Ok(FunctionBody::Statement(StatementFunctionBody {
                statement: self.parse_statement()?,
                id: self.id_generator.next_node_id(),
            })),
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

        Ok(SimpleBinding {
            name_token,
            name,
            type_,
            id: self.id_generator.next_node_id(),
        })
    }

    pub fn parse_statement(&mut self) -> Result<Statement> {
        match self.current_token_type() {
            Some(TokenType::Keyword(Keyword::On)) => {
                let on_token = expect!(self, TokenType::Keyword(Keyword::On))?;
                let executor = self.parse_executor()?;

                let mut iterators = vec![];
                while self.current_token_type() == Some(TokenType::OpenBracket) {
                    iterators.push(OnStatementIterator {
                        open_bracket_token: expect!(self, TokenType::OpenBracket)?,
                        binding: self.parse_simple_binding()?,
                        equal_token: expect!(self, TokenType::EqualSign)?,
                        max_value: self.parse_expression()?,
                        close_bracket_token: expect!(self, TokenType::CloseBracket)?,
                    });
                }

                let open_paren_token = expect!(self, TokenType::OpenParen)?;

                let mut bindings = vec![];

                while self.current_token_type() != Some(TokenType::CloseParen) {
                    let binding = self.parse_lvalue()?;
                    match self.current_token_type() {
                        Some(TokenType::Comma) => {
                            bindings.push((binding, Some(expect!(self, TokenType::Comma)?)))
                        }
                        _ => {
                            bindings.push((binding, None));
                            break;
                        }
                    }
                }

                let close_paren_token = expect!(self, TokenType::CloseParen)?;
                let body = self.parse_statement()?;

                Ok(Statement::On(OnStatement {
                    on_token,
                    executor,
                    iterators,
                    open_paren_token,
                    bindings,
                    close_paren_token,
                    body: Box::new(body),
                    id: self.id_generator.next_node_id(),
                }))
            }
            Some(TokenType::OpenBrace) => {
                let open_brace_token = expect!(self, TokenType::OpenBrace)?;

                let mut body = vec![];
                while self.current_token_type() != Some(TokenType::CloseBrace)
                    && self.current_token_type().is_some()
                {
                    let recovery_start = self.current_token();
                    if let Ok(stmt) = self.parse_statement() {
                        body.push(stmt);
                    } else {
                        error!("failed to parse statement");
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

                Ok(Statement::Block(BlockStatement {
                    open_brace_token,
                    body,
                    close_brace_token,
                    id: self.id_generator.next_node_id(),
                }))
            }
            Some(TokenType::Keyword(Keyword::Return)) => {
                let return_token = expect!(self, TokenType::Keyword(Keyword::Return))?;

                let value = if let Some(TokenType::Semicolon) = self.current_token_type() {
                    None
                } else {
                    Some(self.parse_expression()?)
                };

                Ok(Statement::Return(ReturnStatement {
                    return_token,
                    value,
                    semicolon_token: expect!(self, TokenType::Semicolon)?,
                    id: self.id_generator.next_node_id(),
                }))
            }
            Some(TokenType::Keyword(Keyword::Let)) => {
                let let_token = expect!(self, TokenType::Keyword(Keyword::Let))?;

                Ok(Statement::VariableDefinition(VariableDefinitionStatement {
                    let_token,
                    binding: self.parse_simple_binding()?,
                    equals_token: expect!(self, TokenType::EqualSign)?,
                    value: self.parse_expression()?,
                    semicolon_token: expect!(self, TokenType::Semicolon)?,
                    id: self.id_generator.next_node_id(),
                }))
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

                Ok(Statement::If(IfStatement {
                    if_token,
                    condition,
                    if_body: Box::new(if_body),
                    elifs,
                    else_,
                    id: self.id_generator.next_node_id(),
                }))
            }
            Some(TokenType::Keyword(Keyword::While)) => {
                let while_token = expect!(self, TokenType::Keyword(Keyword::While))?;
                let condition = self.parse_expression()?;
                let body = self.parse_statement()?;
                Ok(Statement::WhileLoop(WhileLoopStatement {
                    while_token,
                    condition,
                    body: Box::new(body),
                    id: self.id_generator.next_node_id(),
                }))
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
                Ok(Statement::ForLoop(ForLoopStatement {
                    for_token,
                    open_paren_token,
                    initializer: Box::new(initializer),
                    condition,
                    second_semicolon_token,
                    incrementer,
                    close_paren_token,
                    body: Box::new(body),
                    id: self.id_generator.next_node_id(),
                }))
            }
            Some(TokenType::Keyword(Keyword::Break)) => Ok(Statement::Break(BreakStatement {
                break_token: expect!(self, TokenType::Keyword(Keyword::Break))?,
                semicolon_token: expect!(self, TokenType::Semicolon)?,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::Skip)) => Ok(Statement::Skip(SkipStatement {
                skip_token: expect!(self, TokenType::Keyword(Keyword::Skip))?,
                semicolon_token: expect!(self, TokenType::Semicolon)?,
                id: self.id_generator.next_node_id(),
            })),
            _ => Ok(Statement::Expression(ExpressionStatement {
                expression: self.parse_expression()?,
                semicolon_token: expect!(self, TokenType::Semicolon)?,
                id: self.id_generator.next_node_id(),
            })),
        }
    }
    fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_assignment_expression()
    }

    fn parse_primary_expression(&mut self) -> Result<Expression> {
        match self.current_token_type() {
            Some(TokenType::Integer(value, _)) => Ok(Expression::Literal(LiteralExpression {
                value: LiteralKind::Number(value),
                token: expect!(self, TokenType::Integer(_, _))?,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::CharLiteral(value)) => Ok(Expression::Literal(LiteralExpression {
                value: LiteralKind::Char(value.chars().next().unwrap_or('X')),
                token: expect!(self, TokenType::CharLiteral(_))?,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Float(value, _)) => Ok(Expression::Literal(LiteralExpression {
                value: LiteralKind::Float(value),
                token: expect!(self, TokenType::Float(_, _))?,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::True)) => Ok(Expression::Literal(LiteralExpression {
                value: LiteralKind::Bool(true),
                token: expect!(self, TokenType::Keyword(Keyword::True))?,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::False)) => {
                Ok(Expression::Literal(LiteralExpression {
                    value: LiteralKind::Bool(false),
                    token: expect!(self, TokenType::Keyword(Keyword::False))?,
                    id: self.id_generator.next_node_id(),
                }))
            }
            Some(TokenType::OpenBracket) => {
                let open_bracket_token = expect!(self, TokenType::OpenBracket)?;
                let mut elements = vec![];
                while self.current_token_type() != Some(TokenType::CloseBracket) {
                    let element = self.parse_expression()?;

                    match self.current_token_type() {
                        Some(TokenType::Comma) => {
                            elements.push((element, Some(expect!(self, TokenType::Comma)?)))
                        }
                        _ => {
                            elements.push((element, None));
                            break;
                        }
                    }
                }
                let close_bracket_token = expect!(self, TokenType::CloseBracket)?;
                Ok(Expression::Array(ArrayExpression {
                    open_bracket_token,
                    elements,
                    close_bracket_token,
                    id: self.id_generator.next_node_id(),
                }))
            }
            Some(TokenType::At) => {
                let at_token = expect!(self, TokenType::At)?;

                let (name_token, name) = expect!(self, TokenType::Identifier(name) => (self.current_token().unwrap(), name))?;

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
                Ok(Expression::CompilerExpression(CompilerExpression {
                    at_token,
                    name,
                    name_token,
                    arguments,
                    open_paren_token,
                    close_paren_token,
                    id: self.id_generator.next_node_id(),
                }))
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
                        Ok(Expression::FunctionCall(FunctionCallExpression {
                            name,
                            name_token,
                            arguments,
                            open_paren_token,
                            close_paren_token,
                            id: self.id_generator.next_node_id(),
                        }))
                    }
                    _ => Ok(Expression::VariableAccess(VariableAccessExpression {
                        name,
                        name_token,
                        id: self.id_generator.next_node_id(),
                    })),
                }
            }
            Some(TokenType::OpenParen) => Ok(Expression::Grouping(GroupingExpression {
                open_paren_token: expect!(self, TokenType::OpenParen)?,
                subexpression: Box::new(self.parse_expression()?),
                close_paren_token: expect!(self, TokenType::CloseParen)?,
                id: self.id_generator.next_node_id(),
            })),
            _ => unable_to_parse!(self, "primary expression"),
        }
    }
    fn parse_postfix_expression(&mut self) -> Result<Expression> {
        let mut lhs = if let Ok(type_) = self.try_parse(|this| this.parse_type()) {
            let open_brace_token = expect!(self, TokenType::OpenBrace)?;

            let mut members = vec![];
            while self.current_token_type() != Some(TokenType::CloseBrace) {
                let (name_token, name) = expect!(self, TokenType::Identifier(name) => (self.current_token().unwrap(), name))?;
                let colon_token = expect!(self, TokenType::Colon)?;
                let value = self.parse_expression()?;

                let member = StructMemberValue {
                    name,
                    name_token,
                    colon_token,
                    value,
                };

                match self.current_token_type() {
                    Some(TokenType::Comma) => {
                        members.push((member, Some(expect!(self, TokenType::Comma)?)))
                    }
                    _ => {
                        members.push((member, None));
                        break;
                    }
                }
            }
            let close_brace_token = expect!(self, TokenType::CloseBrace)?;

            Expression::Struct(StructExpression {
                type_,
                open_brace_token,
                members,
                close_brace_token,
                id: self.id_generator.next_node_id(),
            })
        } else {
            self.parse_primary_expression()?
        };

        while match self.current_token_type() {
            Some(TokenType::OpenBracket) => {
                lhs = Expression::ArrayIndex(ArrayIndexExpression {
                    array: Box::new(lhs),
                    open_bracket_token: expect!(self, TokenType::OpenBracket)?,
                    index: Box::new(self.parse_expression()?),
                    close_bracket_token: expect!(self, TokenType::CloseBracket)?,
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            Some(TokenType::Dot) => {
                let dot_token = expect!(self, TokenType::Dot)?;
                let (member_name_token, member_name) = expect!(self, TokenType::Identifier(name) => (self.current_token().unwrap(), name))?;
                lhs = Expression::StructAccess(StructAccessExpression {
                    value: Box::new(lhs),
                    dot_token,
                    member_name,
                    member_name_token,
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            _ => return Ok(lhs),
        } {}

        Ok(lhs)
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

            Some(_) => self.parse_postfix_expression(),
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

        Ok(left)
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

        Ok(left)
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

        Ok(left)
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

        Ok(left)
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

        Ok(left)
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

        Ok(left)
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

        Ok(left)
    }

    fn parse_lvalue_or_experssion(&mut self) -> Result<Either<(LValue, Expression), Expression>> {
        let expr = self.parse_logical_or_expression()?;

        fn expression_to_lvalue(expression: &Expression) -> Option<LValue> {
            Some(match expression.clone() {
                Expression::VariableAccess(VariableAccessExpression {
                    name,
                    name_token,
                    id,
                }) => LValue::Variable(VariableLValue {
                    name,
                    name_token,
                    id,
                }),
                Expression::ArrayIndex(ArrayIndexExpression {
                    array,
                    open_bracket_token,
                    index,
                    close_bracket_token,
                    id,
                }) => LValue::ArrayIndex(ArrayIndexLValue {
                    array: Box::new(expression_to_lvalue(&array)?),
                    open_bracket_token,
                    index,
                    close_bracket_token,
                    id,
                }),
                Expression::StructAccess(StructAccessExpression {
                    value,
                    dot_token,
                    member_name,
                    member_name_token,
                    id,
                }) => LValue::StructAccess(StructAccessLValue {
                    value: Box::new(expression_to_lvalue(&value)?),
                    dot_token,
                    member_name,
                    member_name_token,
                    id,
                }),
                Expression::Grouping(GroupingExpression {
                    open_paren_token,
                    subexpression,
                    close_paren_token,
                    id,
                }) => LValue::Grouping(GroupingLValue {
                    open_paren_token,
                    sublvalue: Box::new(expression_to_lvalue(&subexpression)?),
                    close_paren_token,
                    id,
                }),
                _ => {
                    return None;
                }
            })
        }

        if let Some(lvalue) = expression_to_lvalue(&expr) {
            Ok(Either::Left((lvalue, expr)))
        } else {
            Ok(Either::Right(expr))
        }
    }

    fn parse_lvalue(&mut self) -> Result<LValue> {
        match self.parse_lvalue_or_experssion()? {
            Either::Left((lvalue, _expr)) => Ok(lvalue),
            Either::Right(expr) => {
                let err = FleetError::from_node(
                    &expr,
                    "This expression isn't a valid lvalue",
                    ErrorSeverity::Error,
                );
                self.errors.push(err.clone());
                Err(err.into())
            }
        }
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression> {
        let lv_or_expr = self.parse_lvalue_or_experssion()?;

        let (lvalue, left_expr) = match lv_or_expr {
            Either::Right(expr) => return Ok(expr),
            Either::Left(x) => x,
        };

        if !matches!(self.current_token_type(), Some(TokenType::EqualSign)) {
            return Ok(left_expr);
        }

        let equal_token = expect!(self, TokenType::EqualSign)?;
        let value = self.parse_assignment_expression()?;

        Ok(Expression::VariableAssignment(
            VariableAssignmentExpression {
                lvalue,
                equal_token,
                right: Box::new(value),
                id: self.id_generator.next_node_id(),
            },
        ))
    }

    pub fn parse_executor(&mut self) -> Result<Executor> {
        let host = ExecutorHost::Self_(SelfExecutorHost {
            token: expect!(self, TokenType::Keyword(Keyword::Self_))?,
            id: self.id_generator.next_node_id(),
        });

        let dot_token = expect!(self, TokenType::Dot)?;

        match self.current_token_type() {
            Some(TokenType::Identifier(name)) if name == "threads" => {
                Ok(Executor::Thread(ThreadExecutor {
                    host,
                    dot_token,
                    thread_token: expect!(self, = TokenType::Identifier("threads".to_string()))?,
                    open_bracket_token: expect!(self, TokenType::OpenBracket)?,
                    index: self.parse_expression()?,
                    close_bracket_token: expect!(self, TokenType::CloseBracket)?,
                    id: self.id_generator.next_node_id(),
                }))
            }
            Some(TokenType::Identifier(name)) if name == "gpus" => Ok(Executor::GPU(GPUExecutor {
                host,
                dot_token,
                gpus_token: expect!(self, = TokenType::Identifier("gpus".to_string()))?,
                open_bracket_token: expect!(self, TokenType::OpenBracket)?,
                gpu_index: self.parse_expression()?,
                close_bracket_token: expect!(self, TokenType::CloseBracket)?,
                id: self.id_generator.next_node_id(),
            })),
            _ => unable_to_parse!(self, "executor"),
        }
    }
    pub fn parse_type(&mut self) -> Result<Type> {
        self.parse_postfix_type()
    }

    pub fn parse_postfix_type(&mut self) -> Result<Type> {
        let mut type_ = self.parse_primary_type()?;
        while match self.current_token_type() {
            Some(TokenType::OpenBracket) => {
                let open_bracket_token = expect!(self, TokenType::OpenBracket)?;

                let size = self.try_parse(|this| this.parse_expression()).ok();

                let close_bracket_token = expect!(self, TokenType::CloseBracket)?;
                type_ = Type::Array(ArrayType {
                    subtype: Box::new(type_),
                    open_bracket_token,
                    size: size.map(Box::new),
                    close_bracket_token,
                    id: self.id_generator.next_node_id(),
                });
                true
            }
            _ => false,
        } {}
        Ok(type_)
    }

    pub fn parse_primary_type(&mut self) -> Result<Type> {
        match self.current_token_type() {
            Some(TokenType::Keyword(Keyword::I8)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::I8))?,
                type_: RuntimeType::I8,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::I16)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::I16))?,
                type_: RuntimeType::I16,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::I32)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::I32))?,
                type_: RuntimeType::I32,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::I64)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::I64))?,
                type_: RuntimeType::I64,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::U8)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::U8))?,
                type_: RuntimeType::U8,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::U16)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::U16))?,
                type_: RuntimeType::U16,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::U32)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::U32))?,
                type_: RuntimeType::U32,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::U64)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::U64))?,
                type_: RuntimeType::U64,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::F32)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::F32))?,
                type_: RuntimeType::F32,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::F64)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::F64))?,
                type_: RuntimeType::F64,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::Bool)) => Ok(Type::Simple(SimpleType {
                token: expect!(self, TokenType::Keyword(Keyword::Bool))?,
                type_: RuntimeType::Boolean,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::Idk)) => Ok(Type::Idk(IdkType {
                token: expect!(self, TokenType::Keyword(Keyword::Idk))?,
                id: self.id_generator.next_node_id(),
            })),
            Some(TokenType::Keyword(Keyword::Struct)) => {
                let struct_token = expect!(self, TokenType::Keyword(Keyword::Struct))?;
                let open_brace_token = expect!(self, TokenType::OpenBrace)?;

                let mut members = vec![];
                while self.current_token_type() != Some(TokenType::CloseBrace) {
                    let (name_token, name) = expect!(self, TokenType::Identifier(name) => (self.current_token().unwrap(), name))?;
                    let colon_token = expect!(self, TokenType::Colon)?;
                    let type_ = self.parse_type()?;

                    let member = StructMemberDefinition {
                        name,
                        name_token,
                        colon_token,
                        type_,
                    };

                    match self.current_token_type() {
                        Some(TokenType::Comma) => {
                            members.push((member, Some(expect!(self, TokenType::Comma)?)))
                        }
                        _ => {
                            members.push((member, None));
                            break;
                        }
                    }
                }
                let close_brace_token = expect!(self, TokenType::CloseBrace)?;
                Ok(Type::Struct(StructType {
                    struct_token,
                    open_brace_token,
                    members,
                    close_brace_token,
                    id: self.id_generator.next_node_id(),
                }))
            }
            Some(TokenType::OpenParen) => Ok(Type::Unit(UnitType {
                open_paren_token: expect!(self, TokenType::OpenParen)?,
                close_paren_token: expect!(self, TokenType::CloseParen)?,
                id: self.id_generator.next_node_id(),
            })),
            _ => unable_to_parse!(self, "type"),
        }
    }
}

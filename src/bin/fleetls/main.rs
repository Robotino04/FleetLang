use std::cell::RefCell;
use std::collections::HashMap;
use std::env::args;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::rc::Rc;
use std::sync::LazyLock;

use fleet::ast::{
    ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstNode, AstVisitor,
    BinaryExpression, BinaryOperation, BlockStatement, BoolExpression, BoolType, BreakStatement,
    CastExpression, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
    FunctionCallExpression, FunctionDefinition, GroupingExpression, GroupingLValue, HasID, IdkType,
    IfStatement, IntType, NodeID, NumberExpression, OnStatement, PerNodeData, Program,
    ReturnStatement, SelfExecutorHost, SimpleBinding, SkipStatement, StatementFunctionBody,
    ThreadExecutor, UnaryExpression, UnaryOperation, UnitType, VariableAccessExpression,
    VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
};
use fleet::infra::{CompileStatus, ErrorSeverity, compile_program, format_program};
use fleet::passes::find_containing_node::FindContainingNodePass;
use fleet::passes::function_termination_analysis::FunctionTermination;
use fleet::passes::type_propagation::{Function, RuntimeType, Variable};
use fleet::tokenizer::{SourceLocation, Token, Trivia, TriviaKind};
use indoc::indoc;
use inkwell::context::Context;
use itertools::Itertools;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LspService, Server};
use tower_lsp::{LanguageServer, lsp_types::*};

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: std::sync::RwLock<HashMap<Url, String>>,
}

static SEMANTIC_TOKEN_TYPES: LazyLock<Vec<SemanticTokenType>> = std::sync::LazyLock::new(|| {
    vec![
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::TYPE,
        SemanticTokenType::CLASS,
        SemanticTokenType::ENUM,
        SemanticTokenType::INTERFACE,
        SemanticTokenType::STRUCT,
        SemanticTokenType::TYPE_PARAMETER,
        SemanticTokenType::PARAMETER,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::EVENT,
        SemanticTokenType::FUNCTION,
        SemanticTokenType::METHOD,
        SemanticTokenType::MACRO,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::MODIFIER,
        SemanticTokenType::COMMENT,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::REGEXP,
        SemanticTokenType::OPERATOR,
        SemanticTokenType::DECORATOR,
    ]
});
static SEMANTIC_TOKEN_MODIFIERS: LazyLock<Vec<SemanticTokenModifier>> =
    std::sync::LazyLock::new(|| {
        vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::STATIC,
            SemanticTokenModifier::DEPRECATED,
            SemanticTokenModifier::ABSTRACT,
            SemanticTokenModifier::ASYNC,
            SemanticTokenModifier::MODIFICATION,
            SemanticTokenModifier::DOCUMENTATION,
            SemanticTokenModifier::DEFAULT_LIBRARY,
        ]
    });

fn token_delta_line(prev_start: SourceLocation, start: SourceLocation) -> u32 {
    (start.line - prev_start.line) as u32
}
fn token_delta_start(prev_start: SourceLocation, start: SourceLocation) -> u32 {
    if start.line == prev_start.line {
        (start.column - prev_start.column) as u32
    } else {
        start.column as u32
    }
}
fn token_length(start: SourceLocation, end: SourceLocation) -> u32 {
    (end.index - start.index) as u32
}

impl Backend {
    fn format_option_type(&self, type_: Option<Option<RuntimeType>>) -> String {
        type_
            .as_ref()
            .map(|td| {
                td.as_ref()
                    .map(|t| t.to_string())
                    .unwrap_or("/* missing type data*/".to_string())
            })
            .unwrap_or("/* type data unavailable */".to_string())
    }
    fn get_type_as_hover(
        &self,
        id: NodeID,
        type_data: &Option<PerNodeData<Rc<RefCell<RuntimeType>>>>,
    ) -> String {
        self.format_option_type(
            type_data
                .as_ref()
                .map(|td| td.get(&id).map(|id_t| id_t.borrow().clone())),
        )
    }
    fn generate_node_hover(
        &self,
        node: impl Into<AstNode>,
        variable_data: &Option<PerNodeData<Rc<RefCell<Variable>>>>,
        function_data: &Option<PerNodeData<Rc<RefCell<Function>>>>,
        type_data: &Option<PerNodeData<Rc<RefCell<RuntimeType>>>>,
    ) -> (String, String) {
        match node.into() {
            AstNode::Program(_) => ("".to_string(), "program".to_string()),
            AstNode::FunctionDefinition(FunctionDefinition {
                let_token: _,
                name,
                name_token: _,
                equal_token: _,
                open_paren_token: _,
                parameters,
                close_paren_token: _,
                right_arrow_token: _,
                return_type,
                body: _,
                id,
            }) => (
                format!(
                    "let {name} = ({}) -> {}",
                    Itertools::intersperse(
                        parameters.iter().map(|(param, _comma)| self
                            .generate_node_hover(
                                param.clone(),
                                variable_data,
                                function_data,
                                type_data
                            )
                            .0),
                        ", ".to_string()
                    )
                    .collect::<String>(),
                    return_type
                        .map(|t| self
                            .generate_node_hover(t, variable_data, function_data, type_data)
                            .0)
                        .unwrap_or_else(|| {
                            let ref_func = function_data.as_ref().map(|fd| fd.get(&id));
                            let return_type = self.format_option_type(ref_func.map(|func| {
                                func.map(|func| func.borrow().return_type.borrow().clone())
                            }));
                            return_type
                        })
                ),
                "function definition".to_string(),
            ),
            AstNode::ExternFunctionBody(ExternFunctionBody {
                at_token: _,
                extern_token: _,
                symbol,
                symbol_token: _,
                semicolon_token: _,
                id: _,
            }) => (
                format!("@extern \"{symbol}\""),
                "extern function body".to_string(),
            ),
            AstNode::StatementFunctionBody(StatementFunctionBody { statement, id: _ }) => (
                self.generate_node_hover(statement, variable_data, function_data, type_data)
                    .0,
                "statement function body".to_string(),
            ),
            AstNode::SimpleBinding(SimpleBinding {
                name_token: _,
                name,
                type_,
                id,
            }) => (
                format!(
                    "{name}: {}",
                    type_
                        .map(|(_colon, type_)| self
                            .generate_node_hover(type_, variable_data, function_data, type_data)
                            .0)
                        .unwrap_or_else(|| {
                            let ref_var = variable_data.as_ref().map(|fd| fd.get(&id));
                            let type_ = self.format_option_type(
                                ref_var
                                    .map(|var| var.map(|var| var.borrow().type_.borrow().clone())),
                            );
                            type_
                        })
                ),
                "simple binding".to_string(),
            ),
            AstNode::ExpressionStatement(ExpressionStatement {
                expression,
                semicolon_token: _,
                id: _,
            }) => (
                self.generate_node_hover(expression, variable_data, function_data, type_data)
                    .0,
                "expression statement".to_string(),
            ),
            AstNode::OnStatement(OnStatement {
                on_token: _,
                open_paren_token: _,
                executor,
                close_paren_token: _,
                body: _,
                id: _,
            }) => (
                format!(
                    "on ({})",
                    self.generate_node_hover(executor, variable_data, function_data, type_data)
                        .0
                ),
                "`on` statement".to_string(),
            ),
            AstNode::BlockStatement(BlockStatement { .. }) => ("".to_string(), "block".to_string()),
            AstNode::ReturnStatement(ReturnStatement { id, .. }) => (
                format!("return {}", self.get_type_as_hover(id, type_data)),
                "`return` statement".to_string(),
            ),
            AstNode::VariableDefinitionStatement(VariableDefinitionStatement {
                let_token: _,
                binding,
                equals_token: _,
                value: _,
                semicolon_token: _,
                id: _,
            }) => (
                format!(
                    "let {} = ...", // TODO: once we have consteval, display that here
                    self.generate_node_hover(binding, variable_data, function_data, type_data)
                        .0
                ),
                "variable definition".to_string(),
            ),
            AstNode::IfStatement(IfStatement { .. }) => {
                ("".to_string(), "`if` statement".to_string())
            }
            AstNode::WhileLoopStatement(WhileLoopStatement { .. }) => {
                ("".to_string(), "`while` loop".to_string())
            }
            AstNode::ForLoopStatement(ForLoopStatement {
                for_token: _,
                open_paren_token: _,
                initializer,
                condition,
                second_semicolon_token: _,
                incrementer,
                close_paren_token: _,
                body: _,
                id: _,
            }) => {
                let condition_type = condition
                    .map(|cond| self.get_type_as_hover(cond.get_id(), type_data))
                    .unwrap_or("".to_string());
                let incrementer_type = incrementer
                    .map(|cond| self.get_type_as_hover(cond.get_id(), type_data))
                    .unwrap_or("".to_string());
                (
                    format!(
                        "for ({}; {}; {})",
                        self.generate_node_hover(
                            *initializer,
                            variable_data,
                            function_data,
                            type_data
                        )
                        .0,
                        condition_type,
                        incrementer_type
                    ),
                    "`for` loop".to_string(),
                )
            }
            AstNode::BreakStatement(_break_statement) => {
                ("".to_string(), "`break` statement".to_string())
            }
            AstNode::SkipStatement(_skip_statement) => {
                ("".to_string(), "`skip` statement".to_string())
            }
            AstNode::SelfExecutorHost(SelfExecutorHost { .. }) => {
                ("self".to_string(), "`self` executor host".to_string())
            }
            AstNode::ThreadExecutor(ThreadExecutor {
                host,
                dot_token: _,
                thread_token: _,
                open_bracket_token: _,
                index,
                close_bracket_token: _,
                id: _,
            }) => (
                format!(
                    "{}.threads[{}]",
                    self.generate_node_hover(host, variable_data, function_data, type_data)
                        .0,
                    self.generate_node_hover(index, variable_data, function_data, type_data)
                        .0
                ),
                "thread executor".to_string(),
            ),
            AstNode::UnaryExpression(UnaryExpression {
                operator_token: _,
                operation,
                operand,
                id,
            }) => (
                {
                    let inner_type = self.get_type_as_hover(operand.get_id(), type_data);
                    let outer_type = self.get_type_as_hover(id, type_data);
                    match operation {
                        UnaryOperation::BitwiseNot => {
                            format!("bitwise negation (~{inner_type}) => {outer_type}")
                        }
                        UnaryOperation::LogicalNot => {
                            format!("logical negation (!{inner_type}) => {outer_type}")
                        }
                        UnaryOperation::Negate => {
                            format!("arithmetic negation (-{inner_type}) => {outer_type}")
                        }
                    }
                },
                "unary expression".to_string(),
            ),
            AstNode::CastExpression(CastExpression {
                operand,
                as_token: _,
                type_: _,
                id,
            }) => (
                {
                    let from_type = self.get_type_as_hover(operand.get_id(), type_data);
                    let to_type = self.get_type_as_hover(id, type_data);
                    format!("{from_type} as {to_type}")
                },
                "type cast".to_string(),
            ),
            AstNode::NumberExpression(NumberExpression {
                value,
                token: _,
                id,
            }) => (
                format!("{value} {}", self.get_type_as_hover(id, type_data)),
                "number literal".to_string(),
            ),
            AstNode::BoolExpression(BoolExpression {
                value,
                token: _,
                id,
            }) => (
                format!("{value} {}", self.get_type_as_hover(id, type_data)),
                "boolean literal".to_string(),
            ),
            AstNode::ArrayExpression(ArrayExpression {
                open_bracket_token: _,
                elements: _,
                close_bracket_token: _,
                id,
            }) => (
                self.get_type_as_hover(id, type_data).to_string(),
                "array literal".to_string(),
            ),
            AstNode::BinaryExpression(BinaryExpression {
                left,
                operator_token: _,
                operation,
                right,
                id,
            }) => {
                let left_type = self.get_type_as_hover(left.get_id(), type_data);
                let right_type = self.get_type_as_hover(right.get_id(), type_data);
                let result_type = self.get_type_as_hover(id, type_data);
                let (name, op) = match operation {
                    BinaryOperation::Add => ("addition", "+"),
                    BinaryOperation::Subtract => ("subtraction", "-"),
                    BinaryOperation::Multiply => ("multiplication", "*"),
                    BinaryOperation::Divide => ("division", "/"),
                    BinaryOperation::Modulo => ("modulo", "%"),
                    BinaryOperation::GreaterThan => ("greater than", ">"),
                    BinaryOperation::GreaterThanOrEqual => ("greater than or equal", ">="),
                    BinaryOperation::LessThan => ("less than", "<"),
                    BinaryOperation::LessThanOrEqual => ("less than or equal", "<="),
                    BinaryOperation::Equal => ("equal", "=="),
                    BinaryOperation::NotEqual => ("not equal", "!="),
                    BinaryOperation::LogicalAnd => ("logical and", "&&"),
                    BinaryOperation::LogicalOr => ("logical or", "||"),
                };

                (
                    format!("{name} ({left_type} {op} {right_type} => {result_type})"),
                    "binary expression".to_string(),
                )
            }
            AstNode::GroupingExpression(GroupingExpression {
                open_paren_token: _,
                subexpression,
                close_paren_token: _,
                id: _,
            }) => (
                format!(
                    "({})",
                    self.generate_node_hover(
                        *subexpression,
                        variable_data,
                        function_data,
                        type_data
                    )
                    .0
                ),
                "expression grouping".to_string(),
            ),
            AstNode::FunctionCallExpression(FunctionCallExpression {
                name,
                name_token: _,
                open_paren_token: _,
                arguments: _,
                close_paren_token: _,
                id,
            }) => {
                let ref_func = function_data.as_ref().map(|fd| fd.get(&id));
                let return_type = self.format_option_type(
                    ref_func
                        .map(|func| func.map(|func| func.borrow().return_type.borrow().clone())),
                );
                let parameters = ref_func
                    .map(|func| {
                        func.map(|func| {
                            Itertools::intersperse(
                                func.borrow()
                                    .parameter_types
                                    .iter()
                                    .map(|param| param.borrow().to_string()),
                                ", ".to_string(),
                            )
                            .collect::<String>()
                        })
                        .unwrap_or("/* missing type data*/".to_string())
                    })
                    .unwrap_or("/* type data unavailable */".to_string());

                (
                    format!("let {name} = ({parameters}) -> {return_type}"),
                    "function call".to_string(),
                )
            }
            AstNode::ArrayIndexExpression(ArrayIndexExpression {
                array,
                open_bracket_token: _,
                index,
                close_bracket_token: _,
                id,
            }) => (
                format!(
                    "({})[{}] => {}",
                    self.get_type_as_hover(array.get_id(), type_data),
                    self.get_type_as_hover(index.get_id(), type_data),
                    self.get_type_as_hover(id, type_data)
                ),
                "array index".to_string(),
            ),
            AstNode::VariableAccessExpression(VariableAccessExpression {
                name,
                name_token: _,
                id,
            }) => {
                let type_ = self.get_type_as_hover(id, type_data);
                // TODO: once we have consteval, display the value here
                (
                    format!("let {name}: {type_} = ..."),
                    "variable access".to_string(),
                )
            }
            AstNode::VariableAssignmentExpression(VariableAssignmentExpression {
                lvalue,
                equal_token: _,
                right: _,
                id: _,
            }) => {
                // TODO: once we have consteval, display the value here
                (
                    self.generate_node_hover(lvalue, variable_data, function_data, type_data)
                        .0,
                    "variable assignment".to_string(),
                )
            }
            AstNode::VariableLValue(VariableLValue {
                name,
                name_token: _,
                id,
            }) => {
                // TODO: once we have consteval, display the value here
                let type_ = self.get_type_as_hover(id, type_data);
                (
                    format!("let {name}: {type_} = ..."),
                    "variable lvalue".to_string(),
                )
            }
            AstNode::ArrayIndexLValue(ArrayIndexLValue {
                array,
                open_bracket_token: _,
                index,
                close_bracket_token: _,
                id: _,
            }) => {
                // TODO: once we have consteval, display the value here
                (
                    format!(
                        "let {}[{}] = ...",
                        self.get_type_as_hover(array.get_id(), type_data),
                        self.get_type_as_hover(index.get_id(), type_data)
                    ),
                    "array index lvalue".to_string(),
                )
            }
            AstNode::GroupingLValue(GroupingLValue {
                open_paren_token: _,
                sublvalue,
                close_paren_token: _,
                id: _,
            }) => (
                format!(
                    "({})",
                    self.generate_node_hover(*sublvalue, variable_data, function_data, type_data)
                        .0
                ),
                "lvalue grouping".to_string(),
            ),
            AstNode::IntType(IntType {
                token: _,
                type_,
                id: _,
            }) => (type_.to_string(), "type".to_string()),
            AstNode::UnitType(UnitType {
                open_paren_token: _,
                close_paren_token: _,
                id: _,
            }) => ("()".to_string(), "type".to_string()),
            AstNode::BoolType(BoolType { token: _, id: _ }) => {
                ("bool".to_string(), "type".to_string())
            }
            AstNode::IdkType(IdkType {
                type_: _,
                token: _,
                id,
            }) => {
                let type_ = self.get_type_as_hover(id, type_data);
                (type_.to_string(), "idk type".to_string())
            }
            AstNode::ArrayType(ArrayType {
                subtype: _,
                open_bracket_token: _,
                size: _,
                close_bracket_token: _,
                id,
            }) => {
                let type_ = self.get_type_as_hover(id, type_data);
                (type_.to_string(), "array type".to_string())
            }
        }
    }

    fn full_hover_text(
        &self,
        node_hierarchy: &[AstNode],
        variable_data: &Option<PerNodeData<Rc<RefCell<Variable>>>>,
        function_data: &Option<PerNodeData<Rc<RefCell<Function>>>>,
        type_data: &Option<PerNodeData<Rc<RefCell<RuntimeType>>>>,
        terminations: &Option<PerNodeData<FunctionTermination>>,
        hovered_token: &Option<Token>,
    ) -> String {
        format!(
            indoc! {r##"
                        ```rust
                        {}
                        ```

                        ---- Debug Stats ----
                        {}

                        ---- Token ----
                        ```rust
                        {:#?}
                        ```
                    "##},
            node_hierarchy
                .last()
                .map(|node| self.generate_node_hover(
                    node.clone(),
                    variable_data,
                    function_data,
                    type_data
                ))
                .map_or("No AST Node".to_string(), |(info, debug)| format!(
                    "{info} // {debug}"
                )),
            terminations
                .as_ref()
                .and_then(|ts| ts.get_node(node_hierarchy.last()?).cloned())
                .map_or("No termination info available".to_string(), |t| format!(
                    "{t:?}"
                )),
            hovered_token,
        )
    }
}

struct ExtractSemanticTokensPass {
    previous_token_start: SourceLocation,
    semantic_tokens: Vec<SemanticToken>,
}

impl ExtractSemanticTokensPass {
    pub fn new() -> Self {
        Self {
            previous_token_start: SourceLocation::start(),
            semantic_tokens: vec![],
        }
    }
}

impl ExtractSemanticTokensPass {
    fn build_semantic_token(
        &mut self,
        token: &Token,
        token_type: SemanticTokenType,
        token_modifiers: Vec<SemanticTokenModifier>,
    ) {
        self.build_comment_tokens_from_trivia(&token.leading_trivia);
        self.semantic_tokens.push(SemanticToken {
            delta_line: token_delta_line(self.previous_token_start, token.start),
            delta_start: token_delta_start(self.previous_token_start, token.start),
            length: token_length(token.start, token.end),
            token_type: self.find_token_type_index(token_type),
            token_modifiers_bitset: self.build_token_modifier_bitset(token_modifiers),
        });
        self.previous_token_start = token.start;
        self.build_comment_tokens_from_trivia(&token.trailing_trivia);
    }

    fn build_comment_tokens_from_trivia(&mut self, trivia: &Vec<Trivia>) {
        for Trivia { kind, start, end } in trivia {
            if let TriviaKind::LineComment(content) | TriviaKind::BlockComment(content) = kind {
                let mut mod_start = *start;
                for line in content.split("\n") {
                    let mut length = line.chars().count();
                    if mod_start.line == end.line {
                        length += 2 // the comment end
                    }
                    if mod_start.line == start.line {
                        length += 2 // the comment start
                    }
                    self.semantic_tokens.push(SemanticToken {
                        delta_line: token_delta_line(self.previous_token_start, mod_start),
                        delta_start: token_delta_start(self.previous_token_start, mod_start),
                        length: length as u32,
                        token_type: self.find_token_type_index(SemanticTokenType::COMMENT),
                        token_modifiers_bitset: 0,
                    });
                    self.previous_token_start = mod_start;
                    mod_start.line += 1;
                    mod_start.column = 0;
                    mod_start.index += length + 1; // the newline
                }
            }
        }
    }

    fn build_comment_tokens_only(&mut self, token: &Token) {
        self.build_comment_tokens_from_trivia(&token.leading_trivia);
        self.build_comment_tokens_from_trivia(&token.trailing_trivia);
    }

    fn find_token_type_index(&self, token_type: SemanticTokenType) -> u32 {
        SEMANTIC_TOKEN_TYPES
            .iter()
            .position(|t| t.as_str() == token_type.as_str())
            .unwrap_or(0) as u32
    }
    fn build_token_modifier_bitset(&self, token_modifiers: Vec<SemanticTokenModifier>) -> u32 {
        token_modifiers
            .iter()
            .map(|modifier| {
                1u32 << (SEMANTIC_TOKEN_MODIFIERS
                    .iter()
                    .position(|t| t.as_str() == modifier.as_str())
                    .unwrap_or(0))
            })
            .reduce(|a, b| a | b)
            .unwrap_or(0)
    }
}

impl AstVisitor for ExtractSemanticTokensPass {
    type ProgramOutput = Vec<SemanticToken>;
    type FunctionDefinitionOutput = ();
    type FunctionBodyOutput = ();
    type SimpleBindingOutput = ();
    type StatementOutput = ();
    type ExecutorHostOutput = ();
    type ExecutorOutput = ();
    type ExpressionOutput = ();
    type LValueOutput = ();
    type TypeOutput = ();

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        for f in &mut program.functions {
            self.visit_function_definition(f);
        }
        self.semantic_tokens
    }

    fn visit_function_definition(
        &mut self,
        FunctionDefinition {
            let_token,
            name: _,
            name_token,
            equal_token,
            open_paren_token,
            parameters,
            close_paren_token,
            right_arrow_token,
            return_type,
            body,
            id: _,
        }: &mut FunctionDefinition,
    ) {
        self.build_semantic_token(let_token, SemanticTokenType::KEYWORD, vec![]);
        self.build_semantic_token(
            name_token,
            SemanticTokenType::FUNCTION,
            vec![
                SemanticTokenModifier::DEFINITION,
                SemanticTokenModifier::DECLARATION,
            ],
        );
        self.build_comment_tokens_only(equal_token);
        self.build_comment_tokens_only(open_paren_token);
        for (param, comma) in parameters {
            self.visit_simple_binding(param);
            if let Some(comma) = comma {
                self.build_comment_tokens_only(comma);
            }
        }

        self.build_comment_tokens_only(close_paren_token);
        self.build_comment_tokens_only(right_arrow_token);
        if let Some(return_type) = return_type {
            self.visit_type(return_type);
        }
        self.visit_function_body(body);
    }

    fn visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, id: _ }: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.visit_statement(statement);
    }

    fn visit_extern_function_body(
        &mut self,
        ExternFunctionBody {
            at_token,
            extern_token,
            symbol: _,
            symbol_token,
            semicolon_token,
            id: _,
        }: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.build_semantic_token(at_token, SemanticTokenType::OPERATOR, vec![]);
        self.build_semantic_token(extern_token, SemanticTokenType::KEYWORD, vec![]);
        self.build_semantic_token(
            symbol_token,
            SemanticTokenType::STRING,
            vec![SemanticTokenModifier::READONLY],
        );
        self.build_comment_tokens_only(semicolon_token);
    }

    fn visit_simple_binding(
        &mut self,
        simple_binding: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        self.build_semantic_token(
            &simple_binding.name_token,
            SemanticTokenType::VARIABLE,
            vec![SemanticTokenModifier::DEFINITION],
        );
        if let Some((colon_token, type_)) = &mut simple_binding.type_ {
            self.build_comment_tokens_only(colon_token);
            self.visit_type(type_);
        }
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token,
            id: _,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        self.visit_expression(expression);
        self.build_comment_tokens_only(semicolon_token);
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::StatementOutput {
        self.build_semantic_token(&on_stmt.on_token, SemanticTokenType::KEYWORD, vec![]);
        self.build_comment_tokens_only(&on_stmt.open_paren_token);
        self.visit_executor(&mut on_stmt.executor);
        self.build_comment_tokens_only(&on_stmt.close_paren_token);
        self.visit_statement(&mut on_stmt.body);
    }

    fn visit_block_statement(&mut self, block: &mut BlockStatement) -> Self::StatementOutput {
        self.build_comment_tokens_only(&block.open_brace_token);
        for stmt in &mut block.body {
            self.visit_statement(stmt);
        }
        self.build_comment_tokens_only(&block.close_brace_token);
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        self.build_semantic_token(
            &return_stmt.return_token,
            SemanticTokenType::KEYWORD,
            vec![],
        );
        if let Some(value) = &mut return_stmt.value {
            self.visit_expression(value);
        }
        self.build_comment_tokens_only(&return_stmt.semicolon_token);
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        self.build_semantic_token(&vardef_stmt.let_token, SemanticTokenType::KEYWORD, vec![]);
        self.visit_simple_binding(&mut vardef_stmt.binding);
        self.build_comment_tokens_only(&vardef_stmt.equals_token);
        self.visit_expression(&mut vardef_stmt.value);
        self.build_comment_tokens_only(&vardef_stmt.semicolon_token);
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::StatementOutput {
        self.build_semantic_token(&if_stmt.if_token, SemanticTokenType::KEYWORD, vec![]);
        self.visit_expression(&mut if_stmt.condition);
        self.visit_statement(&mut if_stmt.if_body);
        for (token, condition, body) in &mut if_stmt.elifs {
            self.build_semantic_token(token, SemanticTokenType::KEYWORD, vec![]);
            self.visit_expression(condition);
            self.visit_statement(body);
        }
        if let Some((token, body)) = &mut if_stmt.else_ {
            self.build_semantic_token(token, SemanticTokenType::KEYWORD, vec![]);
            self.visit_statement(body);
        }
    }

    fn visit_while_loop_statement(
        &mut self,
        WhileLoopStatement {
            while_token,
            condition,
            body,
            id: _,
        }: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        self.build_semantic_token(while_token, SemanticTokenType::KEYWORD, vec![]);
        self.visit_expression(condition);
        self.visit_statement(body);
    }

    fn visit_for_loop_statement(
        &mut self,
        ForLoopStatement {
            for_token,
            open_paren_token,
            initializer,
            condition,
            second_semicolon_token,
            incrementer,
            close_paren_token,
            body,
            id: _,
        }: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        self.build_semantic_token(for_token, SemanticTokenType::KEYWORD, vec![]);
        self.build_comment_tokens_only(open_paren_token);
        self.visit_statement(initializer);
        if let Some(cond) = condition {
            self.visit_expression(cond);
        }
        self.build_comment_tokens_only(second_semicolon_token);
        if let Some(inc) = incrementer {
            self.visit_expression(inc);
        }
        self.build_comment_tokens_only(close_paren_token);
        self.visit_statement(body);
    }

    fn visit_break_statement(
        &mut self,
        BreakStatement {
            break_token,
            semicolon_token,
            id: _,
        }: &mut BreakStatement,
    ) -> Self::StatementOutput {
        self.build_semantic_token(break_token, SemanticTokenType::KEYWORD, vec![]);
        self.build_comment_tokens_only(semicolon_token);
    }

    fn visit_skip_statement(
        &mut self,
        SkipStatement {
            skip_token,
            semicolon_token,
            id: _,
        }: &mut SkipStatement,
    ) -> Self::StatementOutput {
        self.build_semantic_token(skip_token, SemanticTokenType::KEYWORD, vec![]);
        self.build_comment_tokens_only(semicolon_token);
    }

    fn visit_self_executor_host(&mut self, executor_host: &mut SelfExecutorHost) {
        self.build_comment_tokens_only(&executor_host.token);
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) {
        self.visit_executor_host(&mut executor.host);
        self.build_comment_tokens_only(&executor.dot_token);
        self.build_semantic_token(&executor.thread_token, SemanticTokenType::VARIABLE, vec![]);
        self.build_comment_tokens_only(&executor.open_bracket_token);
        self.visit_expression(&mut executor.index);
        self.build_comment_tokens_only(&executor.close_bracket_token);
    }

    fn visit_number_expression(
        &mut self,
        NumberExpression {
            value: _,
            token,
            id: _,
        }: &mut NumberExpression,
    ) -> Self::ExpressionOutput {
        self.build_semantic_token(token, SemanticTokenType::NUMBER, vec![]);
    }

    fn visit_bool_expression(
        &mut self,
        BoolExpression {
            value: _,
            token,
            id: _,
        }: &mut BoolExpression,
    ) -> Self::ExpressionOutput {
        self.build_semantic_token(token, SemanticTokenType::KEYWORD, vec![]);
    }

    fn visit_array_expression(
        &mut self,
        ArrayExpression {
            open_bracket_token,
            elements,
            close_bracket_token,
            id: _,
        }: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        self.build_comment_tokens_only(open_bracket_token);
        for (item, comma) in elements {
            self.visit_expression(item);
            if let Some(comma) = comma {
                self.build_comment_tokens_only(comma);
            }
        }
        self.build_comment_tokens_only(close_bracket_token);
    }

    fn visit_function_call_expression(
        &mut self,
        FunctionCallExpression {
            name: _,
            name_token,
            open_paren_token,
            arguments,
            close_paren_token,
            id: _,
        }: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        self.build_semantic_token(name_token, SemanticTokenType::FUNCTION, vec![]);
        self.build_comment_tokens_only(open_paren_token);
        for (arg, comma) in arguments {
            self.visit_expression(arg);
            if let Some(comma) = comma {
                self.build_comment_tokens_only(comma);
            }
        }
        self.build_comment_tokens_only(close_paren_token);
    }

    fn visit_array_index_expression(
        &mut self,
        ArrayIndexExpression {
            array,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        }: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput {
        self.visit_expression(array);
        self.build_comment_tokens_only(open_bracket_token);
        self.visit_expression(index);
        self.build_comment_tokens_only(close_bracket_token);
    }

    fn visit_grouping_expression(
        &mut self,
        GroupingExpression {
            open_paren_token,
            subexpression,
            close_paren_token,
            id: _,
        }: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        self.build_comment_tokens_only(open_paren_token);
        self.visit_expression(subexpression);
        self.build_comment_tokens_only(close_paren_token);
    }

    fn visit_variable_access_expression(
        &mut self,
        VariableAccessExpression {
            name: _,
            name_token,
            id: _,
        }: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        self.build_semantic_token(name_token, SemanticTokenType::VARIABLE, vec![]);
    }

    fn visit_unary_expression(
        &mut self,
        UnaryExpression {
            operator_token,
            operation: _,
            operand,
            id: _,
        }: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        self.build_semantic_token(operator_token, SemanticTokenType::OPERATOR, vec![]);
        self.visit_expression(operand);
    }

    fn visit_cast_expression(
        &mut self,
        CastExpression {
            operand,
            as_token,
            type_,
            id: _,
        }: &mut CastExpression,
    ) -> Self::ExpressionOutput {
        self.visit_expression(operand);
        self.build_semantic_token(as_token, SemanticTokenType::KEYWORD, vec![]);
        self.visit_type(type_);
    }

    fn visit_binary_expression(
        &mut self,
        BinaryExpression {
            left,
            operator_token,
            operation: _,
            right,
            id: _,
        }: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        self.visit_expression(left);
        self.build_semantic_token(operator_token, SemanticTokenType::OPERATOR, vec![]);
        self.visit_expression(right);
    }

    fn visit_variable_assignment_expression(
        &mut self,
        VariableAssignmentExpression {
            lvalue,
            equal_token,
            right,
            id: _,
        }: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        self.visit_lvalue(lvalue);
        self.build_comment_tokens_only(equal_token);
        self.visit_expression(right);
    }

    fn visit_variable_lvalue(
        &mut self,
        VariableLValue {
            name: _,
            name_token,
            id: _,
        }: &mut VariableLValue,
    ) -> Self::LValueOutput {
        self.build_semantic_token(
            name_token,
            SemanticTokenType::VARIABLE,
            vec![SemanticTokenModifier::MODIFICATION],
        );
    }

    fn visit_array_index_lvalue(
        &mut self,
        ArrayIndexLValue {
            array,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        }: &mut ArrayIndexLValue,
    ) -> Self::LValueOutput {
        self.visit_lvalue(array);
        self.build_comment_tokens_only(open_bracket_token);
        self.visit_expression(index);
        self.build_comment_tokens_only(close_bracket_token);
    }

    fn visit_grouping_lvalue(
        &mut self,
        GroupingLValue {
            open_paren_token,
            sublvalue,
            close_paren_token,
            id: _,
        }: &mut GroupingLValue,
    ) -> Self::LValueOutput {
        self.build_comment_tokens_only(open_paren_token);
        self.visit_lvalue(sublvalue);
        self.build_comment_tokens_only(close_paren_token);
    }

    fn visit_int_type(
        &mut self,
        IntType {
            token,
            type_: _,
            id: _,
        }: &mut IntType,
    ) {
        self.build_semantic_token(token, SemanticTokenType::TYPE, vec![]);
    }

    fn visit_unit_type(
        &mut self,
        UnitType {
            open_paren_token,
            close_paren_token,
            id: _,
        }: &mut UnitType,
    ) -> Self::TypeOutput {
        self.build_comment_tokens_only(open_paren_token);
        self.build_comment_tokens_only(close_paren_token);
    }

    fn visit_bool_type(&mut self, BoolType { token, id: _ }: &mut BoolType) -> Self::TypeOutput {
        self.build_semantic_token(token, SemanticTokenType::TYPE, vec![]);
    }

    fn visit_idk_type(
        &mut self,
        IdkType {
            type_: _,
            token,
            id: _,
        }: &mut IdkType,
    ) -> Self::TypeOutput {
        self.build_semantic_token(token, SemanticTokenType::TYPE, vec![]);
    }

    fn visit_array_type(
        &mut self,
        ArrayType {
            subtype,
            open_bracket_token,
            size,
            close_bracket_token,
            id: _,
        }: &mut ArrayType,
    ) -> Self::TypeOutput {
        self.visit_type(subtype);
        self.build_comment_tokens_only(open_bracket_token);
        if let Some(size) = size {
            self.visit_expression(&mut *size);
        }
        self.build_comment_tokens_only(close_bracket_token);
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("fleetls".to_string()),
                        inter_file_dependencies: false,
                        workspace_diagnostics: false,
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                    },
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions {
                                work_done_progress: None,
                            },
                            legend: SemanticTokensLegend {
                                token_types: SEMANTIC_TOKEN_TYPES.clone(),
                                token_modifiers: SEMANTIC_TOKEN_MODIFIERS.clone(),
                            },
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        will_save: None,
                        will_save_wait_until: None,
                        save: None,
                    },
                )),
                document_formatting_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                }),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "FleetLS".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        })
    }

    async fn initialized(&self, params: InitializedParams) {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document.uri)
            .unwrap()
            .clone();

        let context = Context::create();
        let res = compile_program(&context, text.as_str());

        let _program = res
            .status
            .parsed_program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?;

        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: res
                        .errors
                        .iter()
                        .map(|error| Diagnostic {
                            range: Range {
                                start: Position {
                                    line: (error.start.line - 1) as u32,
                                    character: error.start.column as u32,
                                },
                                end: Position {
                                    line: (error.end.line - 1) as u32,
                                    character: error.end.column as u32,
                                },
                            },
                            severity: Some(match error.severity {
                                ErrorSeverity::Error => DiagnosticSeverity::ERROR,
                                ErrorSeverity::Warning => DiagnosticSeverity::WARNING,
                                ErrorSeverity::Note => DiagnosticSeverity::HINT,
                            }),
                            code: None,
                            code_description: None,
                            source: Some("FleetLS".to_string()),
                            message: error.message.clone(),
                            related_information: None,
                            tags: None,
                            data: None,
                        })
                        .collect(),
                },
            }),
        ))
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        self.documents.write().unwrap().insert(
            params.text_document.uri,
            params.content_changes[0].text.clone(),
        );

        let client = self.client.clone();
        tokio::spawn(async move {
            let _ = client.semantic_tokens_refresh().await;
        });
    }
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        self.documents
            .write()
            .unwrap()
            .insert(params.text_document.uri, params.text_document.text);

        let client = self.client.clone();
        tokio::spawn(async move {
            let _ = client.semantic_tokens_refresh().await;
        });
    }
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        self.documents
            .write()
            .unwrap()
            .remove(&params.text_document.uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document_position_params.text_document.uri)
            .unwrap()
            .clone();

        let context = Context::create();
        let res = compile_program(&context, text.as_str());

        let mut program = res
            .status
            .parsed_program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?
            .clone();

        let cpos = params.text_document_position_params.position;
        let find_pass = FindContainingNodePass::new(SourceLocation {
            index: 0,
            line: cpos.line as usize + 1,
            column: cpos.character as usize,
        });

        let terminations = res.status.function_terminations().cloned();
        let type_data = res.status.type_data().cloned();
        let variable_data = res.status.variable_data().cloned();
        let function_data = res.status.function_data().cloned();

        if let Ok((node_hierarchy, hovered_token)) = find_pass.visit_program(&mut program) {
            Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: self.full_hover_text(
                        &node_hierarchy,
                        &variable_data,
                        &function_data,
                        &type_data,
                        &terminations,
                        &hovered_token,
                    ),
                }),
                range: hovered_token.map(|token| Range {
                    start: Position {
                        line: token.start.line as u32 - 1,
                        character: token.start.column as u32,
                    },
                    end: Position {
                        line: token.end.line as u32 - 1,
                        character: token.end.column as u32,
                    },
                }),
            }))
        } else {
            Ok(None)
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document.uri)
            .unwrap()
            .clone();

        let context = Context::create();
        let res = compile_program(&context, text.as_str());

        let mut program = res
            .status
            .parsed_program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?
            .clone();

        let semantic_tokens = ExtractSemanticTokensPass::new().visit_program(&mut program);

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document.uri)
            .unwrap()
            .clone();

        let context = Context::create();
        let res = compile_program(&context, text.as_str());

        if match res.status {
            CompileStatus::ParserFailure { .. } => true,
            CompileStatus::TokenizerOrParserErrors { .. } => true,
            CompileStatus::AnalysisErrors { .. } => false,
            CompileStatus::IrGeneratorFailure { .. } => false,
            CompileStatus::IrGeneratorErrors { .. } => false,
            CompileStatus::Success { .. } => false,
        } {
            return Err(tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Code has parse errors. Not formatting.".into(),
                data: None,
            });
        }

        let program = res
            .status
            .parsed_program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?;
        let id_generator =
            res.status
                .parsed_id_generator()
                .ok_or_else(|| tower_lsp::jsonrpc::Error {
                    code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                    message: "Parsing failed completely".into(),
                    data: None,
                })?;

        let new_text = format_program(program.clone(), id_generator.clone());
        self.documents
            .write()
            .unwrap()
            .insert(params.text_document.uri, new_text.clone());

        let client = self.client.clone();
        tokio::spawn(async move {
            let _ = client.semantic_tokens_refresh().await;
        });

        let doc_end = SourceLocation::end(text.clone());

        return Ok(Some(vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: doc_end.line /* +1 -1 */ as u32,
                    character: 0,
                },
            },
            new_text,
        }]));
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document_position_params.text_document.uri)
            .unwrap()
            .clone();

        let context = Context::create();
        let res = compile_program(&context, text.as_str());

        let mut program = res
            .status
            .parsed_program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?
            .clone();

        let cpos = params.text_document_position_params.position;
        let cpos_sl = SourceLocation {
            index: 0,
            line: cpos.line as usize + 1,
            column: cpos.character as usize,
        };
        let find_pass = FindContainingNodePass::new(cpos_sl);

        let type_data = res.status.type_data().cloned();
        let variable_data = res.status.variable_data().cloned();
        let function_data = res.status.function_data().cloned();

        if let Ok((node_hierarchy, _hovered_token)) = find_pass.visit_program(&mut program) {
            let mut prev_node = None;
            let mut function_call = None;
            for node in node_hierarchy.iter().rev() {
                if let AstNode::FunctionCallExpression(fcall @ FunctionCallExpression { .. }) = node
                {
                    function_call = Some(fcall.clone());
                    break;
                }
                prev_node = Some(node);
            }

            let Some(function_call) = function_call else {
                return Ok(None);
            };

            let label = self
                .generate_node_hover(
                    function_call.clone(),
                    &variable_data,
                    &function_data,
                    &type_data,
                )
                .0;

            let Some(function_data) = function_data else {
                return Ok(None);
            };

            let Some(ref_func) = function_data.get(&function_call.id) else {
                return Ok(None);
            };

            let active_parameter = prev_node
                .map(|argument| {
                    function_call
                        .arguments
                        .iter()
                        .find_position(|(arg, _token)| arg.get_id() == argument.get_id())
                        .map(|(i, _)| i as u32)
                        .unwrap_or(if cpos_sl <= function_call.open_paren_token.start {
                            0
                        } else {
                            function_call
                                .arguments
                                .len()
                                .min(ref_func.borrow().parameter_types.len().saturating_sub(1))
                                as u32
                        })
                })
                .unwrap_or(if cpos_sl <= function_call.open_paren_token.start {
                    0
                } else {
                    function_call
                        .arguments
                        .len()
                        .min(ref_func.borrow().parameter_types.len().saturating_sub(1))
                        as u32
                });

            Ok(Some(SignatureHelp {
                signatures: vec![SignatureInformation {
                    label: label.clone(),
                    documentation: None,
                    parameters: Some(
                        ref_func
                            .borrow()
                            .parameter_types
                            .iter()
                            .enumerate()
                            .map(|(param_i, _param)| ParameterInformation {
                                // TODO: once function_data links to the definition, display
                                // param names here and use them to generate the label ranges
                                // instead of parsing them out of the label
                                label: ParameterLabel::LabelOffsets([
                                    label
                                        .clone()
                                        .chars()
                                        .enumerate()
                                        .filter(|(_i, c)| matches!(*c, ',' | '(' | ')'))
                                        .nth(param_i)
                                        .map(|(i, c)| i + if matches!(c, ',') { 2 } else { 1 })
                                        .unwrap_or(0) as u32,
                                    label
                                        .clone()
                                        .chars()
                                        .enumerate()
                                        .filter(|(_i, c)| matches!(*c, ',' | '(' | ')'))
                                        .nth(param_i + 1)
                                        .map(|(i, _)| i)
                                        .unwrap_or(0) as u32,
                                ]),
                                documentation: None,
                            })
                            .collect(),
                    ),
                    active_parameter: Some(active_parameter),
                }],
                active_signature: Some(0),
                active_parameter: None,
            }))
        } else {
            Ok(None)
        }
    }
}

#[tokio::main]
async fn main() {
    if args().any(|arg| arg == "--stdio") {
        let (service, loopback_socket) = LspService::new(|client| Backend {
            client,
            documents: Default::default(),
        });

        Server::new(tokio::io::stdin(), tokio::io::stdout(), loopback_socket)
            .serve(service)
            .await;
    } else {
        let socket = tokio::net::TcpSocket::new_v4().unwrap();
        socket.set_reuseaddr(true).unwrap();
        socket
            .bind(SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 1234))
            .unwrap();
        let listener = socket.listen(5).unwrap();

        loop {
            let (client_connection, _client_addr) = listener.accept().await.unwrap();

            tokio::spawn(async move {
                let (service, loopback_socket) = LspService::new(|client| Backend {
                    client,
                    documents: Default::default(),
                });

                let (read_half, write_half) = tokio::io::split(client_connection);
                Server::new(read_half, write_half, loopback_socket)
                    .serve(service)
                    .await;
            });
        }
    }
}

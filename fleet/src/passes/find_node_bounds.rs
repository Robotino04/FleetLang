use crate::{
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstNode,
        BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
        LiteralExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost, SimpleBinding,
        SimpleType, SkipStatement, StatementFunctionBody, StructAccessExpression,
        StructAccessLValue, StructExpression, StructType, ThreadExecutor, TypeAlias,
        UnaryExpression, UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
    },
    tokenizer::{NamedSourceRange, SourceRange},
};

pub fn find_node_bounds<I>(node: &I) -> NamedSourceRange
where
    I: Into<AstNode> + Clone,
{
    match &node.clone().into() {
        AstNode::Program(Program {
            top_level_statements: functions,
            id: _,
            file_name,
        }) => functions
            .iter()
            .map(find_node_bounds)
            .reduce(|a, b| a.extend_with(b))
            .unwrap_or(SourceRange::empty_start().named(file_name.clone())),

        AstNode::FunctionDefinition(FunctionDefinition {
            let_token,
            name: _,
            name_token: _,
            equal_token: _,
            open_paren_token: _,
            parameters: _,
            close_paren_token: _,
            right_arrow_token: _,
            return_type: _,
            body,
            id: _,
        }) => let_token.range.clone().extend_with(find_node_bounds(body)),
        AstNode::TypeAlias(TypeAlias {
            let_token,
            name: _,
            name_token: _,
            equal_token: _,
            type_: _,
            semicolon_token,
            id: _,
        }) => let_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone()),

        AstNode::ExternFunctionBody(ExternFunctionBody {
            at_token,
            extern_token: _,
            symbol: _,
            symbol_token: _,
            semicolon_token,
            id: _,
        }) => at_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone()),

        AstNode::StatementFunctionBody(StatementFunctionBody { statement, id: _ }) => {
            find_node_bounds(statement)
        }
        AstNode::SimpleBinding(SimpleBinding {
            name_token,
            name: _,
            type_,
            id: _,
        }) => name_token.range.clone().maybe_extend(
            type_
                .as_ref()
                .map(|(_colon, type_)| find_node_bounds(type_)),
        ),

        AstNode::ExpressionStatement(ExpressionStatement {
            expression,
            semicolon_token,
            id: _,
        }) => semicolon_token
            .range
            .clone()
            .extend_with(find_node_bounds(expression)),

        AstNode::OnStatement(OnStatement {
            on_token,
            executor: _,
            iterators: _,
            open_paren_token: _,
            bindings: _,
            close_paren_token: _,
            body,
            id: _,
        }) => on_token
            .range
            .clone()
            .extend_with(find_node_bounds(&**body)),

        AstNode::BlockStatement(BlockStatement {
            open_brace_token,
            body: _,
            close_brace_token,
            id: _,
        }) => open_brace_token
            .range
            .clone()
            .extend_with(close_brace_token.range.clone()),

        AstNode::ReturnStatement(ReturnStatement {
            return_token,
            value: _,
            semicolon_token,
            id: _,
        }) => return_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone()),

        AstNode::VariableDefinitionStatement(VariableDefinitionStatement {
            let_token,
            binding: _,
            equals_token: _,
            value: _,
            semicolon_token,
            id: _,
        }) => let_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone()),

        AstNode::IfStatement(IfStatement {
            if_token,
            condition: _,
            if_body,
            elifs,
            else_,
            id: _,
        }) => if_token
            .range
            .clone()
            .extend_with(find_node_bounds(&**if_body))
            .maybe_extend(
                else_
                    .as_ref()
                    .map(|(_else_token, else_body)| find_node_bounds(&**else_body)),
            )
            .maybe_extend(
                elifs
                    .last()
                    .map(|(_elif_token, _condition, elif_body)| find_node_bounds(elif_body)),
            ),

        AstNode::WhileLoopStatement(WhileLoopStatement {
            while_token,
            condition: _,
            body,
            id: _,
        }) => while_token
            .range
            .clone()
            .extend_with(find_node_bounds(&**body)),

        AstNode::ForLoopStatement(ForLoopStatement {
            for_token,
            open_paren_token: _,
            initializer: _,
            condition: _,
            second_semicolon_token: _,
            incrementer: _,
            close_paren_token: _,
            body,
            id: _,
        }) => for_token
            .range
            .clone()
            .extend_with(find_node_bounds(&**body)),

        AstNode::BreakStatement(BreakStatement {
            break_token,
            semicolon_token,
            id: _,
        }) => break_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone()),

        AstNode::SkipStatement(SkipStatement {
            skip_token,
            semicolon_token,
            id: _,
        }) => skip_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone()),

        AstNode::SelfExecutorHost(SelfExecutorHost { token, id: _ }) => token.range.clone(),
        AstNode::ThreadExecutor(ThreadExecutor {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index: _,
            close_bracket_token,
            id: _,
        }) => close_bracket_token
            .range
            .clone()
            .extend_with(find_node_bounds(host)),

        AstNode::GPUExecutor(GPUExecutor {
            host,
            dot_token: _,
            gpus_token: _,
            open_bracket_token: _,
            gpu_index: _,
            close_bracket_token,
            id: _,
        }) => close_bracket_token
            .range
            .clone()
            .extend_with(find_node_bounds(host)),

        AstNode::UnaryExpression(UnaryExpression {
            operator_token,
            operation: _,
            operand,
            id: _,
        }) => operator_token
            .range
            .clone()
            .extend_with(find_node_bounds(&**operand)),

        AstNode::CastExpression(CastExpression {
            operand,
            as_token: _,
            type_,
            id: _,
        }) => find_node_bounds(&**operand).extend_with(find_node_bounds(type_)),
        AstNode::LiteralExpression(LiteralExpression {
            value: _,
            token,
            id: _,
        }) => token.range.clone(),
        AstNode::ArrayExpression(ArrayExpression {
            open_bracket_token,
            elements: _,
            close_bracket_token,
            id: _,
        }) => open_bracket_token
            .range
            .clone()
            .extend_with(close_bracket_token.range.clone()),

        AstNode::StructExpression(StructExpression {
            type_,
            open_brace_token: _,
            members: _,
            close_brace_token,
            id: _,
        }) => close_brace_token
            .range
            .clone()
            .extend_with(find_node_bounds(type_)),

        AstNode::BinaryExpression(BinaryExpression {
            left,
            operator_token: _,
            operation: _,
            right,
            id: _,
        }) => find_node_bounds(&**left).extend_with(find_node_bounds(&**right)),
        AstNode::GroupingExpression(GroupingExpression {
            open_paren_token,
            subexpression: _,
            close_paren_token,
            id: _,
        }) => open_paren_token
            .range
            .clone()
            .extend_with(close_paren_token.range.clone()),

        AstNode::FunctionCallExpression(FunctionCallExpression {
            name: _,
            name_token,
            open_paren_token: _,
            arguments: _,
            close_paren_token,
            id: _,
        }) => name_token
            .range
            .clone()
            .extend_with(close_paren_token.range.clone()),

        AstNode::CompilerExpression(CompilerExpression {
            at_token,
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments: _,
            close_paren_token,
            id: _,
        }) => at_token
            .range
            .clone()
            .extend_with(close_paren_token.range.clone()),

        AstNode::ArrayIndexExpression(ArrayIndexExpression {
            array,
            open_bracket_token: _,
            index: _,
            close_bracket_token,
            id: _,
        }) => close_bracket_token
            .range
            .clone()
            .extend_with(find_node_bounds(&**array)),

        AstNode::StructAccessExpression(StructAccessExpression {
            value,
            dot_token: _,
            member_name: _,
            member_name_token,
            id: _,
        }) => member_name_token
            .range
            .clone()
            .extend_with(find_node_bounds(&**value)),
        AstNode::VariableAccessExpression(VariableAccessExpression {
            name: _,
            name_token,
            id: _,
        }) => name_token.range.clone(),
        AstNode::VariableAssignmentExpression(VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id: _,
        }) => find_node_bounds(lvalue).extend_with(find_node_bounds(&**right)),
        AstNode::VariableLValue(VariableLValue {
            name: _,
            name_token,
            id: _,
        }) => name_token.range.clone(),
        AstNode::ArrayIndexLValue(ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index: _,
            close_bracket_token,
            id: _,
        }) => close_bracket_token
            .range
            .clone()
            .extend_with(find_node_bounds(&**array)),

        AstNode::StructAccessLValue(StructAccessLValue {
            value,
            dot_token: _,
            member_name: _,
            member_name_token,
            id: _,
        }) => member_name_token
            .range
            .clone()
            .extend_with(find_node_bounds(&**value)),

        AstNode::GroupingLValue(GroupingLValue {
            open_paren_token,
            sublvalue: _,
            close_paren_token,
            id: _,
        }) => open_paren_token
            .range
            .clone()
            .extend_with(close_paren_token.range.clone()),

        AstNode::SimpleType(SimpleType {
            token,
            type_: _,
            id: _,
        }) => token.range.clone().extend_with(token.range.clone()),
        AstNode::UnitType(UnitType {
            open_paren_token,
            close_paren_token,
            id: _,
        }) => open_paren_token
            .range
            .clone()
            .extend_with(close_paren_token.range.clone()),

        AstNode::IdkType(IdkType { token, id: _ }) => token.range.clone(),
        AstNode::ArrayType(ArrayType {
            subtype,
            open_bracket_token: _,
            size: _,
            close_bracket_token,
            id: _,
        }) => close_bracket_token
            .range
            .clone()
            .extend_with(find_node_bounds(&**subtype)),

        AstNode::StructType(StructType {
            struct_token,
            open_brace_token: _,
            members: _,
            close_brace_token,
            id: _,
        }) => struct_token
            .range
            .clone()
            .extend_with(close_brace_token.range.clone()),

        AstNode::AliasType(AliasType {
            name: _,
            name_token,
            id: _,
        }) => name_token.range.clone(),
    }
}

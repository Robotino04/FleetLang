use crate::{
    ast::{
        AstNode, BinaryExpression, BlockStatement, BoolExpression, BoolType, BreakStatement,
        CastExpression, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
        FunctionCallExpression, FunctionDefinition, GroupingExpression, I32Type, IfStatement,
        NumberExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost, SimpleBinding,
        SkipStatement, StatementFunctionBody, ThreadExecutor, UnaryExpression, UnitType,
        VariableAccessExpression, VariableAssignmentExpression, VariableDefinitionStatement,
        WhileLoopStatement,
    },
    tokenizer::SourceLocation,
};

pub fn find_node_bounds(node: impl Into<AstNode>) -> (SourceLocation, SourceLocation) {
    match node.into() {
        AstNode::Program(Program { functions, id: _ }) => functions
            .iter()
            .map(|f| find_node_bounds(f.clone()))
            .reduce(|(start1, end1), (start2, end2)| (start1.min(start2), end1.max(end2)))
            .unwrap_or((SourceLocation::start(), SourceLocation::start())),
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
        }) => (let_token.start, find_node_bounds(body).1),
        AstNode::ExternFunctionBody(ExternFunctionBody {
            at_token,
            extern_token: _,
            symbol: _,
            symbol_token: _,
            semicolon_token,
            id: _,
        }) => (at_token.start, semicolon_token.end),
        AstNode::StatementFunctionBody(StatementFunctionBody { statement, id: _ }) => {
            find_node_bounds(statement)
        }
        AstNode::SimpleBinding(SimpleBinding {
            name_token,
            name: _,
            colon_token: _,
            type_,
            id: _,
        }) => (name_token.start, find_node_bounds(type_).1),
        AstNode::ExpressionStatement(ExpressionStatement {
            expression,
            semicolon_token,
            id: _,
        }) => (find_node_bounds(expression).0, semicolon_token.end),
        AstNode::OnStatement(OnStatement {
            on_token,
            open_paren_token: _,
            executor: _,
            close_paren_token: _,
            body,
            id: _,
        }) => {
            return (on_token.start, find_node_bounds(*body).1);
        }
        AstNode::BlockStatement(BlockStatement {
            open_brace_token,
            body: _,
            close_brace_token,
            id: _,
        }) => (open_brace_token.start, close_brace_token.end),
        AstNode::ReturnStatement(ReturnStatement {
            return_token,
            value: _,
            semicolon_token,
            id: _,
        }) => (return_token.start, semicolon_token.end),

        AstNode::VariableDefinitionStatement(VariableDefinitionStatement {
            let_token,
            binding: _,
            equals_token: _,
            value: _,
            semicolon_token,
            id: _,
        }) => (let_token.start, semicolon_token.end),
        AstNode::IfStatement(IfStatement {
            if_token,
            condition: _,
            if_body,
            elifs,
            else_,
            id: _,
        }) => (
            if_token.start,
            else_
                .clone()
                .map(|(_else_token, else_body)| find_node_bounds(*else_body).1)
                .or(elifs.last().map(|(_elif_token, _condition, elif_body)| {
                    find_node_bounds(elif_body.clone()).1
                }))
                .unwrap_or(find_node_bounds(*if_body).1),
        ),
        AstNode::WhileLoopStatement(WhileLoopStatement {
            while_token,
            condition: _,
            body,
            id: _,
        }) => (while_token.start, find_node_bounds(*body).1),
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
        }) => (for_token.start, find_node_bounds(*body).1),
        AstNode::BreakStatement(BreakStatement {
            break_token,
            semicolon_token,
            id: _,
        }) => (break_token.start, semicolon_token.end),
        AstNode::SkipStatement(SkipStatement {
            skip_token,
            semicolon_token,
            id: _,
        }) => (skip_token.start, semicolon_token.end),

        AstNode::SelfExecutorHost(SelfExecutorHost { token, id: _ }) => (token.start, token.end),
        AstNode::ThreadExecutor(ThreadExecutor {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index: _,
            close_bracket_token,
            id: _,
        }) => (find_node_bounds(host).0, close_bracket_token.end),
        AstNode::UnaryExpression(UnaryExpression {
            operator_token,
            operation: _,
            operand,
            id: _,
        }) => {
            return (operator_token.start, find_node_bounds(*operand).1);
        }
        AstNode::CastExpression(CastExpression {
            operand,
            as_token: _,
            type_,
            id: _,
        }) => {
            return (find_node_bounds(*operand).0, find_node_bounds(type_).1);
        }
        AstNode::NumberExpression(NumberExpression {
            value: _,
            token,
            id: _,
        }) => (token.start, token.end),
        AstNode::BoolExpression(BoolExpression {
            value: _,
            token,
            id: _,
        }) => (token.start, token.end),
        AstNode::BinaryExpression(BinaryExpression {
            left,
            operator_token: _,
            operation: _,
            right,
            id: _,
        }) => (find_node_bounds(*left).0, find_node_bounds(*right).1),
        AstNode::GroupingExpression(GroupingExpression {
            open_paren_token,
            subexpression: _,
            close_paren_token,
            id: _,
        }) => (open_paren_token.start, close_paren_token.end),

        AstNode::FunctionCallExpression(FunctionCallExpression {
            name: _,
            name_token,
            open_paren_token: _,
            arguments: _,
            close_paren_token,
            id: _,
        }) => (name_token.start, close_paren_token.end),
        AstNode::VariableAccessExpression(VariableAccessExpression {
            name: _,
            name_token,
            id: _,
        }) => (name_token.start, name_token.end),
        AstNode::VariableAssignmentExpression(VariableAssignmentExpression {
            name: _,
            name_token,
            equal_token: _,
            right,
            id: _,
        }) => (name_token.start, find_node_bounds(*right).1),

        AstNode::I32Type(I32Type { token, id: _ }) => (token.start, token.end),
        AstNode::UnitType(UnitType {
            open_paren_token,
            close_paren_token,
            id: _,
        }) => (open_paren_token.start, close_paren_token.end),
        AstNode::BoolType(BoolType { token, id: _ }) => (token.start, token.end),
    }
}

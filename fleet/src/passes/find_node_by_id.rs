use itertools::Itertools;

use crate::ast::{
    AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstNodeRef,
    BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
    ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
    FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
    LiteralExpression, NodeID, OnStatement, OnStatementIterator, Program, ReturnStatement,
    SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement, StatementFunctionBody,
    StructAccessExpression, StructAccessLValue, StructExpression, StructMemberDefinition,
    StructMemberValue, StructType, ThreadExecutor, TypeAlias, UnaryExpression, UnitType,
    VariableAccessExpression, VariableAssignmentExpression, VariableDefinitionStatement,
    VariableLValue, WhileLoopStatement,
};

fn find_in_vec<'a, N, I>(nodes: I, wanted_id: NodeID) -> Option<AstNodeRef<'a>>
where
    N: Into<AstNodeRef<'a>>,
    I: IntoIterator<Item = N>,
{
    nodes
        .into_iter()
        .flat_map(|node| find_node_by_id(node, wanted_id))
        .next()
}

pub fn find_node_by_id<'a, I>(root: I, wanted_id: NodeID) -> Option<AstNodeRef<'a>>
where
    I: Into<AstNodeRef<'a>>,
{
    let root = root.into();
    match root {
        AstNodeRef::Program(Program {
            top_level_statements,
            id,
            file_name: _,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_in_vec(top_level_statements, wanted_id)
            }
        }
        AstNodeRef::FunctionDefinition(FunctionDefinition {
            let_token: _,
            name: _,
            name_token: _,
            equal_token: _,
            open_paren_token: _,
            parameters,
            close_paren_token: _,
            right_arrow_token: _,
            return_type,
            body,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_in_vec(
                    parameters.iter().map(|(binding, _token)| binding),
                    wanted_id,
                )
                .or_else(|| {
                    return_type
                        .as_ref()
                        .and_then(|rt| find_node_by_id(rt, wanted_id))
                })
                .or_else(|| find_node_by_id(&**body, wanted_id))
            }
        }
        AstNodeRef::TypeAlias(TypeAlias {
            let_token: _,
            name: _,
            name_token: _,
            equal_token: _,
            type_,
            semicolon_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(type_, wanted_id)
            }
        }
        AstNodeRef::ExternFunctionBody(ExternFunctionBody {
            at_token: _,
            extern_token: _,
            symbol: _,
            symbol_token: _,
            semicolon_token: _,
            id,
        }) => (*id == wanted_id).then_some(root),
        AstNodeRef::StatementFunctionBody(StatementFunctionBody { statement, id }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(statement, wanted_id)
            }
        }
        AstNodeRef::SimpleBinding(SimpleBinding {
            name_token: _,
            name: _,
            type_,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                type_
                    .as_ref()
                    .and_then(|(_colon, type_)| find_node_by_id(type_, wanted_id))
            }
        }
        AstNodeRef::ExpressionStatement(ExpressionStatement {
            expression,
            semicolon_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**expression, wanted_id)
            }
        }
        AstNodeRef::OnStatement(OnStatement {
            on_token: _,
            executor,
            iterators,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**executor, wanted_id)
                    .or_else(|| {
                        find_in_vec(
                            iterators
                                .iter()
                                .flat_map(
                                    |OnStatementIterator {
                                         open_bracket_token: _,
                                         binding,
                                         equal_token: _,
                                         max_value,
                                         close_bracket_token: _,
                                     }|
                                     -> [AstNodeRef; _] {
                                        [binding.into(), (&**max_value).into()]
                                    },
                                )
                                .collect_vec(),
                            wanted_id,
                        )
                    })
                    .or_else(|| {
                        find_in_vec(bindings.iter().map(|(lvalue, _comma)| lvalue), wanted_id)
                    })
                    .or_else(|| find_node_by_id(&**body, wanted_id))
            }
        }
        AstNodeRef::BlockStatement(BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_in_vec(body, wanted_id)
            }
        }
        AstNodeRef::ReturnStatement(ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                value
                    .as_ref()
                    .and_then(|value| find_node_by_id(&**value, wanted_id))
            }
        }
        AstNodeRef::VariableDefinitionStatement(VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**binding, wanted_id)
                    .or_else(|| find_node_by_id(&**value, wanted_id))
            }
        }
        AstNodeRef::IfStatement(IfStatement {
            if_token: _,
            condition,
            if_body,
            elifs,
            else_,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**condition, wanted_id)
                    .or_else(|| find_node_by_id(&**if_body, wanted_id))
                    .or_else(|| {
                        find_in_vec(
                            elifs
                                .iter()
                                .flat_map(|(_elif, condition, body)| -> [AstNodeRef; _] {
                                    [condition.into(), body.into()]
                                })
                                .collect_vec(),
                            wanted_id,
                        )
                    })
                    .or_else(|| {
                        else_
                            .as_ref()
                            .and_then(|(_else, body)| find_node_by_id(&**body, wanted_id))
                    })
            }
        }
        AstNodeRef::WhileLoopStatement(WhileLoopStatement {
            while_token: _,
            condition,
            body,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**condition, wanted_id)
                    .or_else(|| find_node_by_id(&**body, wanted_id))
            }
        }
        AstNodeRef::ForLoopStatement(ForLoopStatement {
            for_token: _,
            open_paren_token: _,
            initializer,
            condition,
            second_semicolon_token: _,
            incrementer,
            close_paren_token: _,
            body,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**initializer, wanted_id)
                    .or_else(|| {
                        condition
                            .as_ref()
                            .and_then(|condition| find_node_by_id(&**condition, wanted_id))
                    })
                    .or_else(|| {
                        incrementer
                            .as_ref()
                            .and_then(|incrementer| find_node_by_id(&**incrementer, wanted_id))
                    })
                    .or_else(|| find_node_by_id(&**body, wanted_id))
            }
        }
        AstNodeRef::BreakStatement(BreakStatement {
            break_token: _,
            semicolon_token: _,
            id,
        }) => (*id == wanted_id).then_some(root),
        AstNodeRef::SkipStatement(SkipStatement {
            skip_token: _,
            semicolon_token: _,
            id,
        }) => (*id == wanted_id).then_some(root),
        AstNodeRef::SelfExecutorHost(SelfExecutorHost { token: _, id }) => {
            (*id == wanted_id).then_some(root)
        }
        AstNodeRef::ThreadExecutor(ThreadExecutor {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(host, wanted_id).or_else(|| find_node_by_id(&**index, wanted_id))
            }
        }
        AstNodeRef::GPUExecutor(GPUExecutor {
            host,
            dot_token: _,
            gpus_token: _,
            open_bracket_token: _,
            gpu_index,
            close_bracket_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(host, wanted_id)
                    .or_else(|| find_node_by_id(&**gpu_index, wanted_id))
            }
        }
        AstNodeRef::LiteralExpression(LiteralExpression {
            value: _,
            token: _,
            id,
        }) => (*id == wanted_id).then_some(root),
        AstNodeRef::ArrayExpression(ArrayExpression {
            open_bracket_token: _,
            elements,
            close_bracket_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_in_vec(elements.iter().map(|(el, _comma)| el), wanted_id)
            }
        }
        AstNodeRef::StructExpression(StructExpression {
            type_,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(type_, wanted_id).or_else(|| {
                    find_in_vec(
                        members.iter().map(
                            |(
                                StructMemberValue {
                                    name: _,
                                    name_token: _,
                                    colon_token: _,
                                    value,
                                },
                                _comma,
                            )| &**value,
                        ),
                        wanted_id,
                    )
                })
            }
        }
        AstNodeRef::FunctionCallExpression(FunctionCallExpression {
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_in_vec(arguments.iter().map(|(arg, _comma)| arg), wanted_id)
            }
        }
        AstNodeRef::CompilerExpression(CompilerExpression {
            at_token: _,
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_in_vec(arguments.iter().map(|(arg, _comma)| arg), wanted_id)
            }
        }
        AstNodeRef::ArrayIndexExpression(ArrayIndexExpression {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**array, wanted_id)
                    .or_else(|| find_node_by_id(&**index, wanted_id))
            }
        }
        AstNodeRef::StructAccessExpression(StructAccessExpression {
            value,
            dot_token: _,
            member_name: _,
            member_name_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**value, wanted_id)
            }
        }
        AstNodeRef::GroupingExpression(GroupingExpression {
            open_paren_token: _,
            subexpression,
            close_paren_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**subexpression, wanted_id)
            }
        }
        AstNodeRef::VariableAccessExpression(VariableAccessExpression {
            name: _,
            name_token: _,
            id,
        }) => (*id == wanted_id).then_some(root),
        AstNodeRef::UnaryExpression(UnaryExpression {
            operator_token: _,
            operation: _,
            operand,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**operand, wanted_id)
            }
        }
        AstNodeRef::CastExpression(CastExpression {
            operand,
            as_token: _,
            type_,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**operand, wanted_id).or_else(|| find_node_by_id(type_, wanted_id))
            }
        }
        AstNodeRef::BinaryExpression(BinaryExpression {
            left,
            operator_token: _,
            operation: _,
            right,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**left, wanted_id).or_else(|| find_node_by_id(&**right, wanted_id))
            }
        }
        AstNodeRef::VariableAssignmentExpression(VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(lvalue, wanted_id).or_else(|| find_node_by_id(&**right, wanted_id))
            }
        }
        AstNodeRef::VariableLValue(VariableLValue {
            name: _,
            name_token: _,
            id,
        }) => (*id == wanted_id).then_some(root),
        AstNodeRef::ArrayIndexLValue(ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**array, wanted_id)
                    .or_else(|| find_node_by_id(&**index, wanted_id))
            }
        }

        AstNodeRef::StructAccessLValue(StructAccessLValue {
            value,
            dot_token: _,
            member_name: _,
            member_name_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**value, wanted_id)
            }
        }
        AstNodeRef::GroupingLValue(GroupingLValue {
            open_paren_token: _,
            sublvalue,
            close_paren_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**sublvalue, wanted_id)
            }
        }
        AstNodeRef::SimpleType(SimpleType {
            token: _,
            type_: _,
            id,
        }) => (*id == wanted_id).then_some(root),
        AstNodeRef::UnitType(UnitType {
            open_paren_token: _,
            close_paren_token: _,
            id,
        }) => (*id == wanted_id).then_some(root),
        AstNodeRef::IdkType(IdkType { token: _, id }) => (*id == wanted_id).then_some(root),
        AstNodeRef::ArrayType(ArrayType {
            subtype,
            open_bracket_token: _,
            size,
            close_bracket_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_node_by_id(&**subtype, wanted_id).or_else(|| {
                    size.as_ref()
                        .and_then(|size| find_node_by_id(&**size, wanted_id))
                })
            }
        }
        AstNodeRef::StructType(StructType {
            struct_token: _,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id,
        }) => {
            if *id == wanted_id {
                Some(root)
            } else {
                find_in_vec(
                    members.iter().map(
                        |(
                            StructMemberDefinition {
                                name: _,
                                name_token: _,
                                colon_token: _,
                                type_,
                            },
                            _comma,
                        )| type_,
                    ),
                    wanted_id,
                )
            }
        }
        AstNodeRef::AliasType(AliasType {
            name: _,
            name_token: _,
            id,
        }) => (*id == wanted_id).then_some(root),
    }
}

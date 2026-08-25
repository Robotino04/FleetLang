use itertools::Itertools;

use crate::ast::{
    AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstNodeRef,
    BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression, Executor,
    ExecutorHost, Expression, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
    FunctionBody, FunctionCallExpression, FunctionDefinition, GPUExecutor, GroupingExpression,
    GroupingLValue, IdkType, IfStatement, LValue, LiteralExpression, NodeID, OnStatement,
    OnStatementIterator, Program, ReturnStatement, SelfExecutorHost, SimpleBinding, SimpleType,
    SkipStatement, Statement, StatementFunctionBody, StructAccessExpression, StructAccessLValue,
    StructExpression, StructMemberDefinition, StructMemberValue, StructType, ThreadExecutor,
    TopLevelStatement, Type, TypeAlias, UnaryExpression, UnitType, VariableAccessExpression,
    VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
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

        AstNodeRef::TopLevelStatement(node) => match node {
            TopLevelStatement::FunctionDefinition(FunctionDefinition {
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

            TopLevelStatement::TypeAlias(TypeAlias {
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
        },

        AstNodeRef::FunctionBody(node) => match node {
            FunctionBody::Extern(ExternFunctionBody {
                at_token: _,
                extern_token: _,
                symbol: _,
                symbol_token: _,
                semicolon_token: _,
                id,
            }) => (*id == wanted_id).then_some(root),

            FunctionBody::Statement(StatementFunctionBody { statement, id }) => {
                if *id == wanted_id {
                    Some(root)
                } else {
                    find_node_by_id(statement, wanted_id)
                }
            }
        },

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

        AstNodeRef::Statement(node) => match node {
            Statement::Expression(ExpressionStatement {
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

            Statement::On(OnStatement {
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
                                    .flat_map(|iterator| -> [AstNodeRef; _] {
                                        let OnStatementIterator {
                                            open_bracket_token: _,
                                            binding,
                                            equal_token: _,
                                            max_value,
                                            close_bracket_token: _,
                                        } = iterator;

                                        [binding.into(), (&**max_value).into()]
                                    })
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

            Statement::Block(BlockStatement {
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

            Statement::Return(ReturnStatement {
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

            Statement::VariableDefinition(VariableDefinitionStatement {
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

            Statement::If(IfStatement {
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

            Statement::WhileLoop(WhileLoopStatement {
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

            Statement::ForLoop(ForLoopStatement {
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

            Statement::Break(BreakStatement {
                break_token: _,
                semicolon_token: _,
                id,
            }) => (*id == wanted_id).then_some(root),

            Statement::Skip(SkipStatement {
                skip_token: _,
                semicolon_token: _,
                id,
            }) => (*id == wanted_id).then_some(root),
        },

        AstNodeRef::ExecutorHost(node) => match node {
            ExecutorHost::Self_(SelfExecutorHost { token: _, id }) => {
                (*id == wanted_id).then_some(root)
            }
        },

        AstNodeRef::Executor(node) => match node {
            Executor::Thread(ThreadExecutor {
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
                    find_node_by_id(host, wanted_id)
                        .or_else(|| find_node_by_id(&**index, wanted_id))
                }
            }

            Executor::GPU(GPUExecutor {
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
        },

        AstNodeRef::Expression(node) => match node {
            Expression::Literal(LiteralExpression {
                value: _,
                token: _,
                id,
            }) => (*id == wanted_id).then_some(root),

            Expression::Array(ArrayExpression {
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

            Expression::Struct(StructExpression {
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

            Expression::FunctionCall(FunctionCallExpression {
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

            Expression::CompilerExpression(CompilerExpression {
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

            Expression::ArrayIndex(ArrayIndexExpression {
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

            Expression::StructAccess(StructAccessExpression {
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

            Expression::Grouping(GroupingExpression {
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

            Expression::VariableAccess(VariableAccessExpression {
                name: _,
                name_token: _,
                id,
            }) => (*id == wanted_id).then_some(root),

            Expression::Unary(UnaryExpression {
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

            Expression::Cast(CastExpression {
                operand,
                as_token: _,
                type_,
                id,
            }) => {
                if *id == wanted_id {
                    Some(root)
                } else {
                    find_node_by_id(&**operand, wanted_id)
                        .or_else(|| find_node_by_id(type_, wanted_id))
                }
            }

            Expression::Binary(BinaryExpression {
                left,
                operator_token: _,
                operation: _,
                right,
                id,
            }) => {
                if *id == wanted_id {
                    Some(root)
                } else {
                    find_node_by_id(&**left, wanted_id)
                        .or_else(|| find_node_by_id(&**right, wanted_id))
                }
            }

            Expression::VariableAssignment(VariableAssignmentExpression {
                lvalue,
                equal_token: _,
                right,
                id,
            }) => {
                if *id == wanted_id {
                    Some(root)
                } else {
                    find_node_by_id(lvalue, wanted_id)
                        .or_else(|| find_node_by_id(&**right, wanted_id))
                }
            }
        },

        AstNodeRef::LValue(node) => match node {
            LValue::Variable(VariableLValue {
                name: _,
                name_token: _,
                id,
            }) => (*id == wanted_id).then_some(root),

            LValue::ArrayIndex(ArrayIndexLValue {
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

            LValue::StructAccess(StructAccessLValue {
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

            LValue::Grouping(GroupingLValue {
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
        },

        AstNodeRef::Type(node) => match node {
            Type::Simple(SimpleType {
                token: _,
                type_: _,
                id,
            }) => (*id == wanted_id).then_some(root),

            Type::Unit(UnitType {
                open_paren_token: _,
                close_paren_token: _,
                id,
            }) => (*id == wanted_id).then_some(root),

            Type::Idk(IdkType { token: _, id }) => (*id == wanted_id).then_some(root),

            Type::Array(ArrayType {
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

            Type::Struct(StructType {
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

            Type::Alias(AliasType {
                name: _,
                name_token: _,
                id,
            }) => (*id == wanted_id).then_some(root),
        },
    }
}

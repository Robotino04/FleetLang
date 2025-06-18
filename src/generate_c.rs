use std::{cell::RefCell, rc::Rc};

use itertools::Itertools;

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstNode,
        BinaryExpression, BinaryOperation, BlockStatement, BoolExpression, BoolType,
        CastExpression, ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionBody,
        FunctionCallExpression, FunctionDefinition, GroupingExpression, GroupingLValue, IdkType,
        IfStatement, IntType, NumberExpression, OnStatement, ReturnStatement, SelfExecutorHost,
        SimpleBinding, StatementFunctionBody, ThreadExecutor, UnaryExpression, UnaryOperation,
        UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
    },
    passes::type_propagation::RuntimeType,
};

fn generate_function_declaration(function: &FunctionDefinition, mut prefix: String) -> String {
    let params = Itertools::intersperse(
        function
            .parameters
            .iter()
            .map(|(param, _comma)| generate_c(param.clone())),
        ", ".to_string(),
    )
    .collect::<String>();

    if function.name == "main" {
        prefix = "".to_string();
    }

    "void".to_string() //generate_c(function.return_type.clone())
        + " "
        + &prefix
        + function.name.as_str()
        + "("
        + if function.parameters.is_empty() {
            "void"
        } else {
            params.as_str()
        }
        + ")"
}

fn to_c_type(type_: Rc<RefCell<RuntimeType>>) -> String {
    return match *type_.borrow() {
        RuntimeType::I8 => "int8_t".to_string(),
        RuntimeType::I16 => "int16_t".to_string(),
        RuntimeType::I32 => "int32_t".to_string(),
        RuntimeType::I64 => "int64_t".to_string(),
        RuntimeType::UnsizedInt => {
            unreachable!("all types must be known before calling generate_c")
        }
        RuntimeType::Boolean => "bool".to_string(),
        RuntimeType::Unit => "void".to_string(),
        RuntimeType::Unknown => {
            unreachable!("all types must be known before calling generate_c")
        }
        RuntimeType::Error => unreachable!("all types must be known before calling generate_c"),
        RuntimeType::ArrayOf {
            subtype: _,
            size: _,
        } => format!("{}[]", "/* TODO: type inference access in c gen */"),
    };
}

pub fn generate_c(node: impl Into<AstNode>) -> String {
    match node.into() {
        AstNode::Program(program) => {
            let function_definitions = program
                .functions
                .iter()
                .map(|f| generate_c(f.clone()))
                .collect::<Vec<_>>()
                .join("\n");

            let function_declarations = program
                .functions
                .iter()
                .map(|f| generate_function_declaration(f, "fleet_".to_string()) + ";")
                .collect::<Vec<_>>()
                .join("\n");

            format!(
                r##"#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// function declarations
{function_declarations}

// function definitions
{function_definitions}
"##
            )
        }
        AstNode::FunctionDefinition(
            function @ FunctionDefinition {
                body: FunctionBody::Statement(_),
                ..
            },
        ) => {
            generate_function_declaration(&function, "fleet_".to_string())
                + " "
                + generate_c(function.body).as_str()
        }
        AstNode::FunctionDefinition(
            ref function @ FunctionDefinition {
                body:
                    FunctionBody::Extern(ExternFunctionBody {
                        at_token: _,
                        extern_token: _,
                        ref symbol,
                        symbol_token: _,
                        semicolon_token: _,
                        id: _,
                    }),
                ..
            },
        ) => {
            let mut fake_extern_function = function.clone();
            fake_extern_function.name = symbol.clone();

            generate_function_declaration(function, "fleet_".to_string())
                + " {\n"
                + "    extern "
                + &generate_function_declaration(&fake_extern_function, "".to_string())
                + ";\n"
                + "    return "
                + symbol
                + "("
                + &function
                    .parameters
                    .iter()
                    .map(|(param, _comma)| "fleet_".to_string() + &param.name)
                    .join(",")
                + ");\n"
                + "}"
        }
        AstNode::ExternFunctionBody(_) => {
            unreachable!(
                "external function bodies should be handled by FunctionDefinition directly"
            );
        }
        AstNode::StatementFunctionBody(StatementFunctionBody { statement, id: _ }) => {
            generate_c(statement)
        }
        AstNode::SimpleBinding(SimpleBinding {
            name_token: _,
            name,
            type_: _,
            id: _,
        }) => {
            format!("{} fleet_{}", "int32_t" /*generate_c(type_)*/, name)
        }
        AstNode::ExpressionStatement(ExpressionStatement {
            expression,
            semicolon_token: _,
            id: _,
        }) => generate_c(expression) + ";",
        AstNode::OnStatement(OnStatement {
            on_token: _,
            open_paren_token: _,
            executor: _,
            close_paren_token: _,
            body: _,
            id: _,
        }) => {
            todo!();
        }
        AstNode::BlockStatement(BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id: _,
        }) => {
            "{\n".to_string()
                + indent::indent_all_by(
                    4,
                    body.iter()
                        .map(|tls| generate_c(tls.clone()))
                        .collect::<Vec<_>>()
                        .join("\n"),
                )
                .as_str()
                + "\n}"
        }
        AstNode::ReturnStatement(ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id: _,
        }) => "return ".to_string() + &value.map(generate_c).unwrap_or("".to_string()) + ";",
        AstNode::VariableDefinitionStatement(VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id: _,
        }) => generate_c(binding) + " = " + &generate_c(value) + ";",
        AstNode::IfStatement(IfStatement {
            if_token: _,
            condition,
            if_body,
            elifs,
            else_,
            id: _,
        }) => {
            "if (".to_string()
                + &generate_c(condition)
                + ") {"
                + &generate_c(*if_body)
                + "}"
                + &elifs
                    .iter()
                    .map(|(_token, condition, body)| {
                        "else if (".to_string()
                            + &generate_c(condition.clone())
                            + ") {"
                            + &generate_c(body.clone())
                            + "}"
                    })
                    .collect::<String>()
                + &else_
                    .map(|(_token, body)| " else {".to_string() + &generate_c(*body) + "}")
                    .unwrap_or("".to_string())
        }
        AstNode::WhileLoopStatement(WhileLoopStatement {
            while_token: _,
            condition,
            body,
            id: _,
        }) => "while (".to_string() + &generate_c(condition) + ") {" + &generate_c(*body) + "}",
        AstNode::ForLoopStatement(ForLoopStatement {
            for_token: _,
            open_paren_token: _,
            initializer,
            condition,
            second_semicolon_token: _,
            incrementer,
            close_paren_token: _,
            body,
            id: _,
        }) => {
            "for (".to_string()
                + &generate_c(*initializer)
                + &condition.map(generate_c).unwrap_or("".to_string())
                + ";"
                + &incrementer.map(generate_c).unwrap_or("".to_string())
                + ") {"
                + &generate_c(*body)
                + "}"
        }
        AstNode::BreakStatement(_break_statement) => "break;".to_string(),
        AstNode::SkipStatement(_skip_statement) => "continue;".to_string(),
        AstNode::SelfExecutorHost(SelfExecutorHost { token: _, id: _ }) => "self".to_string(),
        AstNode::ThreadExecutor(ThreadExecutor {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }) => generate_c(host) + ".threads[" + generate_c(index).as_str() + "]",
        AstNode::NumberExpression(NumberExpression {
            value,
            token: _,
            id: _,
        }) => value.to_string(),
        AstNode::BoolExpression(BoolExpression {
            value,
            token: _,
            id: _,
        }) => value.to_string(),
        AstNode::ArrayExpression(ArrayExpression {
            open_bracket_token: _,
            elements: _,
            close_bracket_token: _,
            id: _,
        }) => {
            // TODO: once we use AstVisitor here, add this so the type is available
            /*
            return format!("(({}){{ {} }})", /* type of array */, /* items of array */)
            */
            "{/* TODO: array literals */}".to_string()
        }
        AstNode::FunctionCallExpression(FunctionCallExpression {
            name,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id: _,
        }) => {
            "fleet_".to_string()
                + &name
                + "("
                + arguments
                    .iter()
                    .map(|(arg, _comma)| generate_c(arg.clone()))
                    .collect::<Vec<_>>()
                    .join(", ")
                    .as_str()
                + ")"
        }
        AstNode::ArrayIndexExpression(ArrayIndexExpression {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }) => "(".to_string() + &generate_c(*array) + ")[" + &generate_c(*index) + "]",
        AstNode::UnaryExpression(UnaryExpression {
            operation,
            operand,
            operator_token: _,
            id: _,
        }) => {
            match operation {
                UnaryOperation::BitwiseNot => "~",
                UnaryOperation::LogicalNot => "!",
                UnaryOperation::Negate => "-",
            }
            .to_string()
                + generate_c(*operand).as_str()
        }
        AstNode::CastExpression(CastExpression {
            operand,
            as_token: _,
            type_,
            id: _,
        }) => format!("(({})({}))", generate_c(type_), generate_c(*operand)),
        AstNode::BinaryExpression(BinaryExpression {
            left,
            operator_token: _,
            operation,
            right,
            id: _,
        }) => {
            format!(
                "({} {} {})",
                generate_c(*left),
                match operation {
                    BinaryOperation::Add => "+",
                    BinaryOperation::Subtract => "-",
                    BinaryOperation::Multiply => "*",
                    BinaryOperation::Divide => "/",
                    BinaryOperation::Modulo => "%",
                    BinaryOperation::GreaterThan => ">",
                    BinaryOperation::GreaterThanOrEqual => ">=",
                    BinaryOperation::LessThan => "<",
                    BinaryOperation::LessThanOrEqual => "<=",
                    BinaryOperation::Equal => "==",
                    BinaryOperation::NotEqual => "!=",
                    BinaryOperation::LogicalAnd => "&&",
                    BinaryOperation::LogicalOr => "||",
                },
                generate_c(*right)
            )
        }
        AstNode::GroupingExpression(GroupingExpression {
            open_paren_token: _,
            subexpression,
            close_paren_token: _,
            id: _,
        }) => {
            format!("({})", generate_c(*subexpression))
        }
        AstNode::VariableAccessExpression(VariableAccessExpression {
            name,
            name_token: _,
            id: _,
        }) => "fleet_".to_string() + &name,
        AstNode::VariableAssignmentExpression(VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id: _,
        }) => {
            format!("({} = {})", generate_c(lvalue), generate_c(*right))
        }
        AstNode::VariableLValue(VariableLValue {
            name,
            name_token: _,
            id: _,
        }) => name,
        AstNode::ArrayIndexLValue(ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }) => format!("({})[{}]", generate_c(*array), generate_c(*index)),
        AstNode::GroupingLValue(GroupingLValue {
            open_paren_token: _,
            sublvalue,
            close_paren_token: _,
            id: _,
        }) => {
            format!("({})", generate_c(*sublvalue))
        }
        AstNode::IntType(IntType {
            token: _,
            type_,
            id: _,
        }) => match type_ {
            RuntimeType::I8 => "int8_t",
            RuntimeType::I16 => "int16_t",
            RuntimeType::I32 => "int32_t",
            RuntimeType::I64 => "int64_t",
            RuntimeType::UnsizedInt => unreachable!("not sized int type"),
            RuntimeType::Boolean => unreachable!("not sized int type"),
            RuntimeType::Unit => unreachable!("not sized int type"),
            RuntimeType::Unknown => unreachable!("not sized int type"),
            RuntimeType::Error => unreachable!("not sized int type"),
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => unreachable!("not sized int type"),
        }
        .to_string(),
        AstNode::UnitType(UnitType {
            open_paren_token: _,
            close_paren_token: _,
            id: _,
        }) => "void".to_string(),
        AstNode::BoolType(BoolType { token: _, id: _ }) => "bool".to_string(),
        AstNode::IdkType(IdkType { token: _, id: _ }) => {
            "/* TODO: type inference in c */".to_string()
        }
        AstNode::ArrayType(ArrayType {
            subtype,
            open_bracket_token: _,
            size: _,
            close_bracket_token: _,
            id: _,
        }) => format!("*{}", generate_c(*subtype)),
    }
}

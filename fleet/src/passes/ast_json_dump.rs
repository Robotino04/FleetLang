use std::cell::RefMut;

use crate::{
    NewtypeDeref,
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BinaryOperation, BlockStatement, BreakStatement, CastExpression,
        CompilerExpression, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
        FunctionCallExpression, FunctionDefinition, GPUExecutor, GroupingExpression,
        GroupingLValue, IdkType, IfStatement, LiteralExpression, LiteralKind, OnStatement, Program,
        ReturnStatement, SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement,
        StatementFunctionBody, StructAccessExpression, StructAccessLValue, StructExpression,
        StructMemberDefinition, StructMemberValue, StructType, ThreadExecutor, TypeAlias,
        UnaryExpression, UnaryOperation, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue,
        WhileLoopStatement,
    },
    escape::{QuoteType, escape},
    passes::pass_manager::{GlobalState, Pass, PassFactory, PassResult},
};

NewtypeDeref!(pub AstJsonOutput, String);

pub struct AstJsonDumpPass<'state> {
    program: Option<RefMut<'state, Program>>,
    output: Option<RefMut<'state, AstJsonOutput>>,
}

impl Pass for AstJsonDumpPass<'_> {
    fn run(mut self: Box<Self>) -> PassResult {
        let mut output = self.output.take().unwrap();
        let mut program = self.program.take().unwrap();

        *output = self.visit_program(&mut program).into();
        Ok(())
    }
}

impl PassFactory for AstJsonDumpPass<'_> {
    type Output<'state> = AstJsonDumpPass<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        let program = state.check_named()?;
        let output = state.insert_default();

        Ok(AstJsonDumpPass {
            program: Some(program.get_mut(state)),
            output: Some(output.get_mut(state)),
        })
    }
}

impl AstVisitor for AstJsonDumpPass<'_> {
    type ProgramOutput = String;
    type TopLevelOutput = String;
    type FunctionBodyOutput = String;
    type SimpleBindingOutput = String;
    type StatementOutput = String;
    type ExecutorHostOutput = String;
    type ExecutorOutput = String;
    type ExpressionOutput = String;
    type LValueOutput = String;
    type TypeOutput = String;

    fn visit_program(
        mut self,
        Program {
            top_level_statements,
            id: _,
            file_name: _,
        }: &mut Program,
    ) -> Self::ProgramOutput {
        let mut children = Vec::new();
        for tls in top_level_statements.iter_mut() {
            children.push(self.visit_top_level_statement(tls));
        }
        let children_str = children.join(",\n");
        format!("[\"Program\",\n{}\n]", children_str)
    }

    fn visit_function_definition(
        &mut self,
        FunctionDefinition {
            let_token: _,
            name,
            name_token: _,
            equal_token: _,
            open_paren_token: _,
            parameters,
            close_paren_token: _,
            right_arrow_token: _,
            return_type,
            body,
            id: _,
        }: &mut FunctionDefinition,
    ) -> Self::TopLevelOutput {
        let params_str = parameters
            .iter_mut()
            .map(|(param, _)| self.visit_simple_binding(param))
            .collect::<Vec<_>>()
            .join(", ");
        let params_str = if params_str.is_empty() {
            "\"Parameters\"".to_string()
        } else {
            format!("[\"Parameters\", {params_str}]")
        };
        let ret_type_str = match return_type {
            Some(t) => format!(",{}", self.visit_type(t)),
            None => String::new(),
        };
        let body_str = self.visit_function_body(body);
        format!(
            "[\"Function `{}`\",\n{}, [\"Return Type\"{}], [\"Body\", {}]\n]",
            name, params_str, ret_type_str, body_str
        )
    }

    fn visit_type_alias(
        &mut self,
        TypeAlias {
            let_token: _,
            name,
            name_token: _,
            equal_token: _,
            type_,
            semicolon_token: _,
            id: _,
        }: &mut TypeAlias,
    ) -> Self::TopLevelOutput {
        format!("[\"TypeAlias `{}`\",{}]", name, self.visit_type(type_))
    }

    fn visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, id: _ }: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        let stmt_str = self.visit_statement(statement);
        stmt_str.to_string()
    }

    fn visit_extern_function_body(
        &mut self,
        ExternFunctionBody {
            at_token: _,
            extern_token: _,
            symbol,
            symbol_token: _,
            semicolon_token: _,
            id: _,
        }: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        format!("\"Extern `{}`\"", symbol)
    }

    fn visit_simple_binding(
        &mut self,
        SimpleBinding {
            name_token: _,
            name,
            type_,
            id: _,
        }: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        match type_ {
            Some((_, t)) => format!("[\"Binding `{}`\", {}]", name, self.visit_type(t)),
            None => format!("\"Binding `{}`\"", name),
        }
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token: _,
            id: _,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        let expr_str = self.visit_expression(expression);
        format!("[\"ExpressionStatement\",\n{}\n]", expr_str)
    }

    fn visit_on_statement(
        &mut self,
        OnStatement {
            on_token: _,
            executor,
            iterators,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id: _,
        }: &mut OnStatement,
    ) -> Self::StatementOutput {
        let exec_str = self.visit_executor(executor);
        let iter_str = iterators
            .iter_mut()
            .map(|it| {
                let bind_str = self.visit_simple_binding(&mut it.binding);
                let max_str = self.visit_expression(&mut it.max_value);
                format!("[\"Iterator {} = {}\"]", bind_str, max_str)
            })
            .collect::<Vec<_>>()
            .join(", ");
        let binds_str = bindings
            .iter_mut()
            .map(|(lv, _)| self.visit_lvalue(lv))
            .collect::<Vec<_>>()
            .join(", ");
        let body_str = self.visit_statement(body);
        format!(
            "[\"OnStatement\",\n{},\n{},\n{},\n{}\n]",
            exec_str, iter_str, binds_str, body_str
        )
    }

    fn visit_block_statement(
        &mut self,
        BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id: _,
        }: &mut BlockStatement,
    ) -> Self::StatementOutput {
        let stmts_str = body
            .iter_mut()
            .map(|stmt| self.visit_statement(stmt))
            .collect::<Vec<_>>()
            .join(",\n");
        if stmts_str.is_empty() {
            "[\"Block\"]".to_string()
        } else {
            format!("[\"Block\",\n{}\n]", stmts_str)
        }
    }

    fn visit_return_statement(
        &mut self,
        ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id: _,
        }: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        let val_str = match value {
            Some(expr) => self.visit_expression(expr),
            None => String::from("\"None\""),
        };
        format!("[\"Return\",\n{}\n]", val_str)
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id: _,
        }: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        let bind_str = self.visit_simple_binding(binding);
        let val_str = self.visit_expression(value);
        format!("[\"VariableDefinition\",\n{},\n{}\n]", bind_str, val_str)
    }

    fn visit_if_statement(
        &mut self,
        IfStatement {
            if_token: _,
            condition,
            if_body,
            elifs,
            else_,
            id: _,
        }: &mut IfStatement,
    ) -> Self::StatementOutput {
        let cond_str = self.visit_expression(condition);
        let if_body_str = self.visit_statement(if_body);
        let elifs_str = elifs
            .iter_mut()
            .map(|(_, cond, body)| {
                let c = self.visit_expression(cond);
                let b = self.visit_statement(body);
                format!(",[\"Elif\",\n{},\n{}\n]", c, b)
            })
            .collect::<String>();
        let else_str = match else_ {
            Some((_, body)) => format!(",[\"Else\",\n{}\n]", self.visit_statement(body)),
            None => String::new(),
        };
        format!(
            "[\"If\"\n,[\"Condition\",{}]\n,{}\n{}\n{}\n]",
            cond_str, if_body_str, elifs_str, else_str
        )
    }

    fn visit_while_loop_statement(
        &mut self,
        WhileLoopStatement {
            while_token: _,
            condition,
            body,
            id: _,
        }: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        let cond_str = self.visit_expression(condition);
        let body_str = self.visit_statement(body);
        format!("[\"While\",\n{},\n{}\n]", cond_str, body_str)
    }

    fn visit_for_loop_statement(
        &mut self,
        ForLoopStatement {
            for_token: _,
            open_paren_token: _,
            initializer,
            condition,
            second_semicolon_token: _,
            incrementer,
            close_paren_token: _,
            body,
            id: _,
        }: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        let init_str = self.visit_statement(initializer);
        let cond_str = match condition {
            Some(expr) => self.visit_expression(expr),
            None => String::from("\"None\""),
        };
        let inc_str = match incrementer {
            Some(expr) => self.visit_expression(expr),
            None => String::from("\"None\""),
        };
        let body_str = self.visit_statement(body);
        format!(
            "[\"For\",\n{},\n{},\n{},\n{}\n]",
            init_str, cond_str, inc_str, body_str
        )
    }

    fn visit_break_statement(
        &mut self,
        BreakStatement {
            break_token: _,
            semicolon_token: _,
            id: _,
        }: &mut BreakStatement,
    ) -> Self::StatementOutput {
        String::from("\"Break\"")
    }

    fn visit_skip_statement(
        &mut self,
        SkipStatement {
            skip_token: _,
            semicolon_token: _,
            id: _,
        }: &mut SkipStatement,
    ) -> Self::StatementOutput {
        String::from("\"Skip\"")
    }

    fn visit_self_executor_host(
        &mut self,
        SelfExecutorHost { token: _, id: _ }: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        String::from("\"SelfExecutorHost\"")
    }

    fn visit_thread_executor(
        &mut self,
        ThreadExecutor {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }: &mut ThreadExecutor,
    ) -> Self::ExecutorOutput {
        let host_str = self.visit_executor_host(host);
        let idx_str = self.visit_expression(index);
        format!("[\"ThreadExecutor\",\n{},\n{}\n]", host_str, idx_str)
    }

    fn visit_gpu_executor(
        &mut self,
        GPUExecutor {
            host,
            dot_token: _,
            gpus_token: _,
            open_bracket_token: _,
            gpu_index,
            close_bracket_token: _,
            id: _,
        }: &mut GPUExecutor,
    ) -> Self::ExecutorOutput {
        let host_str = self.visit_executor_host(host);
        let idx_str = self.visit_expression(gpu_index);
        format!("[\"GPUExecutor\",\n{},\n{}\n]", host_str, idx_str)
    }

    fn visit_literal_expression(
        &mut self,
        LiteralExpression {
            value,
            token: _,
            id: _,
        }: &mut LiteralExpression,
    ) -> Self::ExpressionOutput {
        match value {
            LiteralKind::Number(n) => format!("\"Number ${}$\"", n),
            LiteralKind::Char(c) => {
                format!("\"Char `'{}'`\"", escape(c.to_string(), QuoteType::Single))
            }
            LiteralKind::Float(f) => format!("\"Float ${}$\"", f),
            LiteralKind::Bool(b) => format!("\"Bool {}\"", b),
        }
    }

    fn visit_array_expression(
        &mut self,
        ArrayExpression {
            open_bracket_token: _,
            elements,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        let elems_str = elements
            .iter_mut()
            .map(|(el, _comma)| self.visit_expression(el))
            .collect::<Vec<_>>()
            .join(", ");
        format!("[\"Array\",\n{}\n]", elems_str)
    }

    fn visit_struct_expression(
        &mut self,
        StructExpression {
            type_,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id: _,
        }: &mut StructExpression,
    ) -> Self::ExpressionOutput {
        let elems_str = members
            .iter_mut()
            .map(
                |(
                    StructMemberValue {
                        name,
                        name_token: _,
                        colon_token: _,
                        value,
                    },
                    _comma,
                )| format!("[\"`{name}`\", {}]", self.visit_expression(value)),
            )
            .collect::<Vec<_>>()
            .join(", ");
        format!(
            "[\"Struct\", {}, [\"Members\", {}]]",
            self.visit_type(type_),
            elems_str
        )
    }

    fn visit_function_call_expression(
        &mut self,
        FunctionCallExpression {
            name,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id: _,
        }: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let args_str = arguments
            .iter_mut()
            .map(|(expr, _)| self.visit_expression(expr))
            .collect::<Vec<_>>()
            .join(", ");
        format!("[\"Call `{}`\",\n{}\n]", name, args_str)
    }

    fn visit_compiler_expression(
        &mut self,
        CompilerExpression {
            at_token: _,
            name,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id: _,
        }: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        let args_str = arguments
            .iter_mut()
            .map(|(arg, _comma)| self.visit_expression(arg))
            .collect::<Vec<_>>()
            .join(", ");
        format!("[\"CompilerCall `{}`\",\n{}\n]", name, args_str)
    }

    fn visit_array_index_expression(
        &mut self,
        ArrayIndexExpression {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput {
        let arr_str = self.visit_expression(array);
        let idx_str = self.visit_expression(index);
        format!("[\"ArrayIndex\",\n{},\n{}\n]", arr_str, idx_str)
    }

    fn visit_struct_access_expression(
        &mut self,
        StructAccessExpression {
            value,
            dot_token: _,
            member_name,
            member_name_token: _,
            id: _,
        }: &mut StructAccessExpression,
    ) -> Self::ExpressionOutput {
        let value_str = self.visit_expression(value);
        format!("[\"StructAccess\",\n{},\n\"{member_name}\"\n]", value_str)
    }

    fn visit_grouping_expression(
        &mut self,
        GroupingExpression {
            open_paren_token: _,
            subexpression,
            close_paren_token: _,
            id: _,
        }: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        let sub_str = self.visit_expression(subexpression);
        format!("[\"Grouping\",\n{}\n]", sub_str)
    }

    fn visit_variable_access_expression(
        &mut self,
        VariableAccessExpression {
            name,
            name_token: _,
            id: _,
        }: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        format!("\"Var `{}`\"", name)
    }

    fn visit_unary_expression(
        &mut self,
        UnaryExpression {
            operator_token: _,
            operation,
            operand,
            id: _,
        }: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        let op_str = match operation {
            UnaryOperation::BitwiseNot => "~",
            UnaryOperation::LogicalNot => "!",
            UnaryOperation::Negate => "-",
        };
        let operand_str = self.visit_expression(operand);
        format!("[\"Unary {}\",\n{}\n]", op_str, operand_str)
    }

    fn visit_cast_expression(
        &mut self,
        CastExpression {
            operand,
            as_token: _,
            type_,
            id: _,
        }: &mut CastExpression,
    ) -> Self::ExpressionOutput {
        let op_str = self.visit_expression(operand);
        let type_str = self.visit_type(type_);
        format!("[\"Cast\",\n{},\n{}\n]", op_str, type_str)
    }

    fn visit_binary_expression(
        &mut self,
        BinaryExpression {
            left,
            operator_token: _,
            operation,
            right,
            id: _,
        }: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        let op_str = match operation {
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
        };
        let left_str = self.visit_expression(left);
        let right_str = self.visit_expression(right);
        format!("[\"`{}`\",\n{},\n{}\n]", op_str, left_str, right_str)
    }

    fn visit_variable_assignment_expression(
        &mut self,
        VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id: _,
        }: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let lv_str = self.visit_lvalue(lvalue);
        let right_str = self.visit_expression(right);
        format!("[\"Assign\",\n{},\n{}\n]", lv_str, right_str)
    }

    fn visit_variable_lvalue(
        &mut self,
        VariableLValue {
            name,
            name_token: _,
            id: _,
        }: &mut VariableLValue,
    ) -> Self::LValueOutput {
        format!("\"LValue `{}`\"", name)
    }

    fn visit_array_index_lvalue(
        &mut self,
        ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayIndexLValue,
    ) -> Self::LValueOutput {
        let arr_str = self.visit_lvalue(array);
        let idx_str = self.visit_expression(index);
        format!("[\"ArrayIndexLValue\",\n{},\n{}\n]", arr_str, idx_str)
    }

    fn visit_struct_access_lvalue(
        &mut self,
        StructAccessLValue {
            value,
            dot_token: _,
            member_name,
            member_name_token: _,
            id: _,
        }: &mut StructAccessLValue,
    ) -> Self::LValueOutput {
        let value_str = self.visit_lvalue(value);
        format!(
            "[\"StructAccessLValue\",\n{},\n\"{member_name}\"\n]",
            value_str
        )
    }

    fn visit_grouping_lvalue(
        &mut self,
        GroupingLValue {
            open_paren_token: _,
            sublvalue,
            close_paren_token: _,
            id: _,
        }: &mut GroupingLValue,
    ) -> Self::LValueOutput {
        let sub_str = self.visit_lvalue(sublvalue);
        format!("[\"GroupingLValue\",\n{}\n]", sub_str)
    }

    fn visit_simple_type(
        &mut self,
        SimpleType {
            token: _,
            type_,
            id: _,
        }: &mut SimpleType,
    ) -> Self::TypeOutput {
        format!("\"Type `{:?}`\"", type_)
    }

    fn visit_unit_type(
        &mut self,
        UnitType {
            open_paren_token: _,
            close_paren_token: _,
            id: _,
        }: &mut UnitType,
    ) -> Self::TypeOutput {
        String::from("\"UnitType\"")
    }

    fn visit_idk_type(&mut self, IdkType { token: _, id: _ }: &mut IdkType) -> Self::TypeOutput {
        String::from("\"IdkType\"")
    }

    fn visit_array_type(
        &mut self,
        ArrayType {
            subtype,
            open_bracket_token: _,
            size,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayType,
    ) -> Self::TypeOutput {
        let subtype_str = self.visit_type(subtype);
        let size_str = match size {
            Some(expr) => self.visit_expression(expr),
            None => String::from("\"None\""),
        };
        format!("[\"ArrayType\",\n{},\n{}]", subtype_str, size_str)
    }

    fn visit_struct_type(
        &mut self,
        StructType {
            struct_token: _,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id: _,
        }: &mut StructType,
    ) -> Self::TypeOutput {
        let member_str = members
            .iter_mut()
            .map(
                |(
                    StructMemberDefinition {
                        name,
                        name_token: _,
                        colon_token: _,
                        type_,
                    },
                    _comma,
                )| format!("[\"Member `{name}`\", {}]", self.visit_type(type_)),
            )
            .collect::<Vec<_>>()
            .join(", ");
        if member_str.is_empty() {
            "\"Struct Type\"".to_string()
        } else {
            format!("[\"Struct Type\", {member_str}]")
        }
    }

    fn visit_alias_type(&mut self, _alias_type: &mut AliasType) -> Self::TypeOutput {
        String::from("\"AliasType\"")
    }
}

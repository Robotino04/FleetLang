use fleet::{
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
        LiteralExpression, LiteralKind, OnStatement, OnStatementIterator, Program, ReturnStatement,
        SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement, StatementFunctionBody,
        StructAccessExpression, StructAccessLValue, StructExpression, StructMemberDefinition,
        StructType, ThreadExecutor, TypeAlias, UnaryExpression, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue,
        WhileLoopStatement,
    },
    tokenizer::{SourceLocation, SourceRange, Token, Trivia, TriviaKind},
};
use tower_lsp_server::lsp_types::{SemanticToken, SemanticTokenModifier, SemanticTokenType};

pub struct ExtractSemanticTokensPass<'inputs> {
    previous_token_start: SourceLocation,
    semantic_tokens: Vec<SemanticToken>,

    semantic_token_types: &'inputs Vec<SemanticTokenType>,
    semantic_token_modifiers: &'inputs Vec<SemanticTokenModifier>,
}

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

impl<'inputs> ExtractSemanticTokensPass<'inputs> {
    pub fn new(
        semantic_token_types: &'inputs Vec<SemanticTokenType>,
        semantic_token_modifiers: &'inputs Vec<SemanticTokenModifier>,
    ) -> Self {
        Self {
            previous_token_start: SourceLocation::start(),
            semantic_tokens: vec![],
            semantic_token_types,
            semantic_token_modifiers,
        }
    }

    fn build_semantic_token(
        &mut self,
        token: &Token,
        token_type: SemanticTokenType,
        token_modifiers: Vec<SemanticTokenModifier>,
    ) {
        self.build_comment_tokens_from_trivia(&token.leading_trivia);
        self.semantic_tokens.push(SemanticToken {
            delta_line: token_delta_line(self.previous_token_start, token.range.range.start),
            delta_start: token_delta_start(self.previous_token_start, token.range.range.start),
            length: token.range.num_chars() as u32,
            token_type: self.find_token_type_index(token_type),
            token_modifiers_bitset: self.build_token_modifier_bitset(token_modifiers),
        });
        self.previous_token_start = token.range.range.start;
        self.build_comment_tokens_from_trivia(&token.trailing_trivia);
    }

    fn build_comment_tokens_from_trivia(&mut self, trivia: &Vec<Trivia>) {
        for Trivia {
            kind,
            range: SourceRange { start, end },
        } in trivia
        {
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
        self.semantic_token_types
            .iter()
            .position(|t| t.as_str() == token_type.as_str())
            .unwrap_or(0) as u32
    }
    fn build_token_modifier_bitset(&self, token_modifiers: Vec<SemanticTokenModifier>) -> u32 {
        token_modifiers
            .iter()
            .map(|modifier| {
                1u32 << (self
                    .semantic_token_modifiers
                    .iter()
                    .position(|t| t.as_str() == modifier.as_str())
                    .unwrap_or(0))
            })
            .reduce(|a, b| a | b)
            .unwrap_or(0)
    }
}

impl AstVisitor for ExtractSemanticTokensPass<'_> {
    type ProgramOutput = Vec<SemanticToken>;
    type TopLevelOutput = ();
    type FunctionBodyOutput = ();
    type SimpleBindingOutput = ();
    type StatementOutput = ();
    type ExecutorHostOutput = ();
    type ExecutorOutput = ();
    type ExpressionOutput = ();
    type LValueOutput = ();
    type TypeOutput = ();

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        for tls in &mut program.top_level_statements {
            self.visit_top_level_statement(tls);
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

    fn visit_type_alias(
        &mut self,
        TypeAlias {
            let_token,
            name: _,
            name_token,
            equal_token,
            type_,
            semicolon_token,
            id: _,
        }: &mut TypeAlias,
    ) -> Self::TopLevelOutput {
        self.build_semantic_token(let_token, SemanticTokenType::KEYWORD, vec![]);
        self.build_semantic_token(
            name_token,
            SemanticTokenType::TYPE,
            vec![
                SemanticTokenModifier::DEFINITION,
                SemanticTokenModifier::DECLARATION,
            ],
        );
        self.build_comment_tokens_only(equal_token);
        self.visit_type(type_);
        self.build_comment_tokens_only(semicolon_token);
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

    fn visit_on_statement(
        &mut self,
        OnStatement {
            on_token,
            executor,
            iterators,
            open_paren_token,
            bindings,
            close_paren_token,
            body,
            id: _,
        }: &mut OnStatement,
    ) -> Self::StatementOutput {
        self.build_semantic_token(on_token, SemanticTokenType::KEYWORD, vec![]);
        self.visit_executor(executor);

        for OnStatementIterator {
            open_bracket_token,
            binding,
            equal_token,
            max_value,
            close_bracket_token,
        } in iterators
        {
            self.build_comment_tokens_only(open_bracket_token);
            self.visit_simple_binding(binding);
            self.build_comment_tokens_only(equal_token);
            self.visit_expression(max_value);
            self.build_comment_tokens_only(close_bracket_token);
        }

        self.build_comment_tokens_only(open_paren_token);
        for (lvalue, comma) in bindings {
            self.visit_lvalue(lvalue);
            if let Some(comma) = comma {
                self.build_comment_tokens_only(comma);
            }
        }
        self.build_comment_tokens_only(close_paren_token);
        self.visit_statement(body);
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
        self.build_semantic_token(
            &vardef_stmt.equals_token,
            SemanticTokenType::OPERATOR,
            vec![],
        );
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

    fn visit_thread_executor(
        &mut self,
        ThreadExecutor {
            host,
            dot_token,
            thread_token,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        }: &mut ThreadExecutor,
    ) {
        self.visit_executor_host(host);
        self.build_comment_tokens_only(dot_token);
        self.build_semantic_token(thread_token, SemanticTokenType::VARIABLE, vec![]);
        self.build_comment_tokens_only(open_bracket_token);
        self.visit_expression(index);
        self.build_comment_tokens_only(close_bracket_token);
    }

    fn visit_gpu_executor(
        &mut self,
        GPUExecutor {
            host,
            dot_token,
            gpus_token,
            open_bracket_token: open_bracket_token_1,
            gpu_index,
            close_bracket_token: close_bracket_token_1,
            id: _,
        }: &mut GPUExecutor,
    ) -> Self::ExecutorOutput {
        self.visit_executor_host(host);
        self.build_comment_tokens_only(dot_token);
        self.build_semantic_token(gpus_token, SemanticTokenType::VARIABLE, vec![]);
        self.build_comment_tokens_only(open_bracket_token_1);
        self.visit_expression(gpu_index);
        self.build_comment_tokens_only(close_bracket_token_1);
    }

    fn visit_literal_expression(
        &mut self,
        LiteralExpression {
            value,
            token,
            id: _,
        }: &mut LiteralExpression,
    ) -> Self::ExpressionOutput {
        self.build_semantic_token(
            token,
            match value {
                LiteralKind::Number(_) => SemanticTokenType::NUMBER,
                LiteralKind::Char(_) => SemanticTokenType::STRING,
                LiteralKind::Float(_) => SemanticTokenType::NUMBER,
                LiteralKind::Bool(_) => SemanticTokenType::KEYWORD,
            },
            vec![],
        );
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

    fn visit_struct_expression(
        &mut self,
        StructExpression {
            type_,
            open_brace_token,
            members,
            close_brace_token,
            id: _,
        }: &mut StructExpression,
    ) -> Self::ExpressionOutput {
        self.visit_type(type_);
        self.build_comment_tokens_only(open_brace_token);
        for (
            fleet::ast::StructMemberValue {
                name: _,
                name_token,
                colon_token,
                value,
            },
            comma,
        ) in members
        {
            self.build_semantic_token(name_token, SemanticTokenType::PROPERTY, vec![]);
            self.build_comment_tokens_only(colon_token);
            self.visit_expression(value);
            if let Some(comma) = comma {
                self.build_comment_tokens_only(comma);
            }
        }
        self.build_comment_tokens_only(close_brace_token);
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

    fn visit_compiler_expression(
        &mut self,
        CompilerExpression {
            at_token,
            name: _,
            name_token,
            open_paren_token,
            arguments,
            close_paren_token,
            id: _,
        }: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        self.build_semantic_token(at_token, SemanticTokenType::OPERATOR, vec![]);
        self.build_comment_tokens_only(open_paren_token);
        self.build_semantic_token(
            name_token,
            SemanticTokenType::FUNCTION,
            vec![SemanticTokenModifier::DEFAULT_LIBRARY],
        );
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

    fn visit_struct_access_expression(
        &mut self,
        StructAccessExpression {
            value,
            dot_token,
            member_name: _,
            member_name_token,
            id: _,
        }: &mut StructAccessExpression,
    ) -> Self::ExpressionOutput {
        self.visit_expression(value);
        self.build_comment_tokens_only(dot_token);
        self.build_semantic_token(member_name_token, SemanticTokenType::PROPERTY, vec![]);
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
        self.build_semantic_token(equal_token, SemanticTokenType::OPERATOR, vec![]);
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

    fn visit_struct_access_lvalue(
        &mut self,
        StructAccessLValue {
            value,
            dot_token,
            member_name: _,
            member_name_token,
            id: _,
        }: &mut StructAccessLValue,
    ) -> Self::LValueOutput {
        self.visit_lvalue(value);
        self.build_comment_tokens_only(dot_token);
        self.build_semantic_token(
            member_name_token,
            SemanticTokenType::PROPERTY,
            vec![SemanticTokenModifier::MODIFICATION],
        );
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

    fn visit_simple_type(
        &mut self,
        SimpleType {
            token,
            type_: _,
            id: _,
        }: &mut SimpleType,
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

    fn visit_idk_type(&mut self, IdkType { token, id: _ }: &mut IdkType) -> Self::TypeOutput {
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

    fn visit_struct_type(
        &mut self,
        StructType {
            struct_token,
            open_brace_token,
            members,
            close_brace_token,
            id: _,
        }: &mut StructType,
    ) -> Self::TypeOutput {
        self.build_semantic_token(struct_token, SemanticTokenType::KEYWORD, vec![]);
        self.build_comment_tokens_only(open_brace_token);
        for (
            StructMemberDefinition {
                name: _,
                name_token,
                colon_token,
                type_,
            },
            comma,
        ) in members
        {
            self.build_semantic_token(
                name_token,
                SemanticTokenType::PROPERTY,
                vec![SemanticTokenModifier::DEFINITION],
            );
            self.build_comment_tokens_only(colon_token);
            self.visit_type(type_);
            if let Some(comma) = comma {
                self.build_comment_tokens_only(comma);
            }
        }
        self.build_comment_tokens_only(close_brace_token);
    }

    fn visit_alias_type(
        &mut self,
        AliasType {
            name: _,
            name_token,
            id: _,
        }: &mut AliasType,
    ) -> Self::TypeOutput {
        self.build_semantic_token(name_token, SemanticTokenType::TYPE, vec![]);
    }
}

use std::{
    cell::{Ref, RefMut},
    mem::swap,
};

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, AstVisitor, BinaryExpression,
        CastExpression, CompilerExpression, Expression, FunctionCallExpression, GroupingLValue,
        HasID, LValue, LiteralExpression, OnStatement, OnStatementIterator, Program,
        StructAccessExpression, StructAccessLValue, StructExpression, StructMemberValue, Type,
        UnaryExpression, VariableAccessExpression, VariableAssignmentExpression, VariableLValue,
    },
    infra::{ErrorSeverity, FleetError},
    passes::{
        partial_visitor::PartialAstVisitor,
        pass_manager::{
            Errors, GlobalState, Pass, PassFactory, PassResult, ScopeData, TypeData, TypeSets,
            VariableData,
        },
        scope_analysis::VariableScopeStack,
    },
};

pub struct LValueReducer<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,
    valid_lvalues: Option<Vec<LValue>>,

    variable_data: Ref<'state, VariableData>,
    type_data: Ref<'state, TypeData>,
    type_sets: Ref<'state, TypeSets>,
    scope_data: Ref<'state, ScopeData>,

    is_top_level_lvalue: bool,
    previous_lvalue_valid: bool,
}

impl PassFactory for LValueReducer<'_> {
    type Output<'state> = LValueReducer<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String> {
        Ok(Self::Output {
            errors: state.get_mut_named::<Errors>()?,
            program: Some(state.get_mut_named::<Program>()?),
            variable_data: state.get_named::<VariableData>()?,
            scope_data: state.get_named::<ScopeData>()?,
            type_data: state.get_named::<TypeData>()?,
            type_sets: state.get_named::<TypeSets>()?,
            valid_lvalues: None,
            is_top_level_lvalue: true,
            previous_lvalue_valid: true,
        })
    }
}
impl Pass for LValueReducer<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let mut program = self.program.take().unwrap();
        self.visit_program(&mut program);

        Ok(())
    }
}

impl LValueReducer<'_> {
    fn is_variable_lvalue_equal(&self, a: &VariableLValue, b: &LValue) -> Option<bool> {
        Some(match b {
            LValue::Variable(b) => {
                self.variable_data.get(&a.id)?.borrow().id
                    == self.variable_data.get(&b.id)?.borrow().id
            }
            LValue::Grouping(GroupingLValue { sublvalue: b, .. }) => {
                self.is_variable_lvalue_equal(a, b)?
            }
            _ => false,
        })
    }
    fn is_array_index_lvalue_equal(&self, a: &ArrayIndexLValue, b: &LValue) -> Option<bool> {
        Some(match b {
            LValue::ArrayIndex(b) => {
                self.is_lvalue_equal(&a.array, &b.array)?
                    && self.is_expression_equal(&a.index, &b.index)?
            }
            LValue::Grouping(GroupingLValue { sublvalue: b, .. }) => {
                self.is_array_index_lvalue_equal(a, b)?
            }
            _ => false,
        })
    }
    fn is_struct_access_lvalue_equal(&self, a: &StructAccessLValue, b: &LValue) -> Option<bool> {
        Some(match b {
            LValue::StructAccess(b) => {
                self.is_lvalue_equal(&a.value, &b.value)? && a.member_name == b.member_name
            }
            LValue::Grouping(GroupingLValue { sublvalue: b, .. }) => {
                self.is_struct_access_lvalue_equal(a, b)?
            }
            _ => false,
        })
    }

    fn is_lvalue_equal(&self, a: &LValue, b: &LValue) -> Option<bool> {
        match a {
            LValue::Variable(a) => self.is_variable_lvalue_equal(a, b),
            LValue::ArrayIndex(a) => self.is_array_index_lvalue_equal(a, b),
            LValue::StructAccess(a) => self.is_struct_access_lvalue_equal(a, b),
            LValue::Grouping(a) => self.is_lvalue_equal(&a.sublvalue, b),
        }
    }

    fn is_expression_equal(&self, a: &Expression, b: &Expression) -> Option<bool> {
        match a {
            Expression::Literal(a) => self.is_literal_expression_equal(a, b),
            Expression::Array(a) => self.is_array_expression_equal(a, b),
            Expression::Struct(a) => self.is_struct_expression_equal(a, b),
            Expression::FunctionCall(a) => self.is_function_call_expression_equal(a, b),
            Expression::CompilerExpression(a) => self.is_compiler_expression_equal(a, b),
            Expression::ArrayIndex(a) => self.is_array_index_expression_equal(a, b),
            Expression::StructAccess(a) => self.is_struct_access_expression_equal(a, b),
            Expression::Grouping(a) => self.is_expression_equal(&a.subexpression, b),
            Expression::VariableAccess(a) => self.is_variable_access_expression_equal(a, b),
            Expression::Unary(a) => self.is_unary_expression_equal(a, b),
            Expression::Cast(a) => self.is_cast_expression_equal(a, b),
            Expression::Binary(a) => self.is_binary_expression_equal(a, b),
            Expression::VariableAssignment(a) => self.is_variable_assignment_expression_equal(a, b),
        }
    }

    fn is_literal_expression_equal(&self, a: &LiteralExpression, b: &Expression) -> Option<bool> {
        Some(match b {
            Expression::Literal(b) => {
                a.value == b.value
                    && self.type_sets.get(*self.type_data.get(&a.id)?)
                        == self.type_sets.get(*self.type_data.get(&b.id)?)
            }
            Expression::Grouping(b) => self.is_literal_expression_equal(a, &b.subexpression)?,
            _ => false,
        })
    }
    fn is_array_expression_equal(&self, a: &ArrayExpression, b: &Expression) -> Option<bool> {
        Some(match b {
            Expression::Array(b) => {
                a.elements.len() == b.elements.len()
                    && a.elements
                        .iter()
                        .zip(&b.elements)
                        .all(|(a, b)| self.is_expression_equal(&a.0, &b.0).unwrap_or(false))
            }
            Expression::Grouping(b) => self.is_array_expression_equal(a, &b.subexpression)?,
            _ => false,
        })
    }
    fn is_struct_expression_equal(&self, a: &StructExpression, b: &Expression) -> Option<bool> {
        Some(match b {
            Expression::Struct(b) => {
                a.members.len() == b.members.len()
                    && a.members.iter().zip(&b.members).all(
                        |(
                            (
                                StructMemberValue {
                                    name: name1,
                                    name_token: _,
                                    colon_token: _,
                                    value: v1,
                                },
                                _comma1,
                            ),
                            (
                                StructMemberValue {
                                    name: name2,
                                    name_token: _,
                                    colon_token: _,
                                    value: v2,
                                },
                                _comma2,
                            ),
                        )| {
                            name1 == name2 && self.is_expression_equal(v1, v2).unwrap_or(false)
                        },
                    )
            }
            Expression::Grouping(b) => self.is_struct_expression_equal(a, &b.subexpression)?,
            _ => false,
        })
    }
    fn is_function_call_expression_equal(
        &self,
        _a: &FunctionCallExpression,
        _b: &Expression,
    ) -> Option<bool> {
        // TODO: handle pure functions
        Some(false)
    }
    fn is_compiler_expression_equal(
        &self,
        _a: &CompilerExpression,
        _b: &Expression,
    ) -> Option<bool> {
        Some(false)
    }
    fn is_array_index_expression_equal(
        &self,
        a: &ArrayIndexExpression,
        b: &Expression,
    ) -> Option<bool> {
        Some(match b {
            Expression::ArrayIndex(b) => {
                self.is_expression_equal(&a.array, &b.array)?
                    && self.is_expression_equal(&a.index, &b.index)?
            }
            Expression::Grouping(b) => self.is_array_index_expression_equal(a, &b.subexpression)?,
            _ => false,
        })
    }
    fn is_struct_access_expression_equal(
        &self,
        a: &StructAccessExpression,
        b: &Expression,
    ) -> Option<bool> {
        Some(match b {
            Expression::StructAccess(b) => {
                self.is_expression_equal(&a.value, &b.value)? && a.member_name == b.member_name
            }
            Expression::Grouping(b) => {
                self.is_struct_access_expression_equal(a, &b.subexpression)?
            }
            _ => false,
        })
    }
    fn is_variable_access_expression_equal(
        &self,
        a: &VariableAccessExpression,
        b: &Expression,
    ) -> Option<bool> {
        Some(match b {
            Expression::VariableAccess(b) => {
                let a_data = self.variable_data.get(&a.id)?.borrow();
                let b_data = self.variable_data.get(&b.id)?.borrow();

                // "is-constant" is technically redundant, but lets be extra sure
                a_data.id == b_data.id && a_data.is_constant && b_data.is_constant
            }
            Expression::Grouping(b) => {
                self.is_variable_access_expression_equal(a, &b.subexpression)?
            }
            _ => false,
        })
    }
    fn is_unary_expression_equal(&self, a: &UnaryExpression, b: &Expression) -> Option<bool> {
        Some(match b {
            Expression::Unary(b) => {
                a.operation == b.operation && self.is_expression_equal(&a.operand, &b.operand)?
            }
            Expression::Grouping(b) => self.is_unary_expression_equal(a, &b.subexpression)?,
            _ => false,
        })
    }
    fn is_binary_expression_equal(&self, a: &BinaryExpression, b: &Expression) -> Option<bool> {
        Some(match b {
            Expression::Binary(b) => {
                a.operation == b.operation
                    && self.is_expression_equal(&a.left, &b.left)?
                    && self.is_expression_equal(&a.right, &b.right)?
            }
            Expression::Grouping(b) => self.is_binary_expression_equal(a, &b.subexpression)?,
            _ => false,
        })
    }
    fn is_cast_expression_equal(&self, a: &CastExpression, b: &Expression) -> Option<bool> {
        Some(match b {
            Expression::Cast(b) => {
                self.is_expression_equal(&a.operand, &b.operand)?
                    && self.is_type_equal(&a.type_, &b.type_)?
            }
            Expression::Grouping(b) => self.is_cast_expression_equal(a, &b.subexpression)?,
            _ => false,
        })
    }
    fn is_variable_assignment_expression_equal(
        &self,
        a: &VariableAssignmentExpression,
        b: &Expression,
    ) -> Option<bool> {
        Some(match b {
            Expression::VariableAssignment(b) => {
                self.is_lvalue_equal(&a.lvalue, &b.lvalue)?
                    && self.is_expression_equal(&a.right, &b.right)?
            }
            Expression::Grouping(b) => {
                self.is_variable_assignment_expression_equal(a, &b.subexpression)?
            }
            _ => false,
        })
    }

    fn is_type_equal(&self, a: &Type, b: &Type) -> Option<bool> {
        Some(
            self.type_sets.get(*self.type_data.get(&a.get_id())?)
                == self.type_sets.get(*self.type_data.get(&b.get_id())?),
        )
    }
}

impl PartialAstVisitor for LValueReducer<'_> {
    fn partial_visit_variable_lvalue(&mut self, a: &mut VariableLValue) {
        let Some(valid_lvalues) = &self.valid_lvalues else {
            // everything is valid
            self.previous_lvalue_valid = true;
            return;
        };

        let is_defined_here = self
            .scope_data
            .get(&a.id)
            .map(|scope| {
                VariableScopeStack::from_scope(scope.clone())
                    .get_write_noref(&a.name)
                    .is_some()
            })
            .unwrap_or(false);
        if !valid_lvalues
            .iter()
            .any(|b| self.is_variable_lvalue_equal(a, b).unwrap_or(false))
            && !is_defined_here
        {
            if self.is_top_level_lvalue {
                self.errors.push(FleetError::from_node(
                    a,
                    "This lvalue isn't available here",
                    ErrorSeverity::Error,
                ));
            }
            self.previous_lvalue_valid = false
        } else {
            self.previous_lvalue_valid = true;
        }
    }
    fn partial_visit_array_index_lvalue(&mut self, a: &mut ArrayIndexLValue) {
        let was_top_level = self.is_top_level_lvalue;

        self.is_top_level_lvalue = true;
        self.partial_visit_expression(&mut a.index);

        self.is_top_level_lvalue = false;
        self.partial_visit_lvalue(&mut a.array);

        self.is_top_level_lvalue = was_top_level;

        let Some(valid_lvalues) = &self.valid_lvalues else {
            // everything is valid
            self.previous_lvalue_valid = true;
            return;
        };

        if self.previous_lvalue_valid
            || valid_lvalues
                .iter()
                .any(|b| self.is_array_index_lvalue_equal(a, b).unwrap_or(false))
        {
            // we either have full access or this index is allowed
            self.previous_lvalue_valid = true;
        } else {
            // invalid because the child failed too
            if self.is_top_level_lvalue {
                self.errors.push(FleetError::from_node(
                    a,
                    "This lvalue isn't available here",
                    ErrorSeverity::Error,
                ));
            }
            self.previous_lvalue_valid = false;
        }
    }
    fn partial_visit_on_statement(
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
    ) {
        self.visit_executor(executor);

        for OnStatementIterator {
            open_bracket_token: _,
            binding,
            equal_token: _,
            max_value,
            close_bracket_token: _,
        } in iterators
        {
            self.partial_visit_simple_binding(binding);
            self.partial_visit_expression(max_value);
        }

        let mut prev_lvalues = Some(
            bindings
                .iter()
                .map(|(lvalue, _comma)| lvalue.clone())
                .collect(),
        );
        swap(&mut prev_lvalues, &mut self.valid_lvalues);

        self.visit_statement(body);

        swap(&mut prev_lvalues, &mut self.valid_lvalues);
    }
}

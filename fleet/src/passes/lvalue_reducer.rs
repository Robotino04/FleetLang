use std::{cell::RefCell, mem::swap, rc::Rc};

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, AstVisitor, BinaryExpression,
        CastExpression, Expression, FunctionCallExpression, GroupingLValue, HasID, LValue,
        LiteralExpression, OnStatement, PerNodeData, Type, UnaryExpression,
        VariableAccessExpression, VariableAssignmentExpression, VariableLValue,
    },
    infra::{ErrorSeverity, FleetError},
    passes::{
        partial_visitor::PartialAstVisitor,
        type_propagation::{
            Function, RuntimeType, TypeAnalysisData, UnionFindSet, UnionFindSetPtr, Variable,
        },
    },
};

pub struct LValueReducer<'errors, 'inputs> {
    errors: &'errors mut Vec<FleetError>,
    valid_lvalues: Option<Vec<LValue>>,

    variable_data: &'inputs PerNodeData<Rc<RefCell<Variable>>>,
    _function_data: &'inputs PerNodeData<Rc<RefCell<Function>>>,
    type_data: &'inputs PerNodeData<UnionFindSetPtr<RuntimeType>>,
    type_sets: &'inputs UnionFindSet<RuntimeType>,
}

impl<'errors, 'inputs> LValueReducer<'errors, 'inputs> {
    pub fn new(
        errors: &'errors mut Vec<FleetError>,
        valid_lvalues: Option<Vec<LValue>>,
        analysis_data: &'inputs TypeAnalysisData,
    ) -> Self {
        Self {
            errors,
            valid_lvalues,
            type_data: &analysis_data.type_data,
            type_sets: &analysis_data.type_sets,
            variable_data: &analysis_data.variable_data,
            _function_data: &analysis_data.function_data,
        }
    }

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

    fn is_lvalue_equal(&self, a: &LValue, b: &LValue) -> Option<bool> {
        match a {
            LValue::Variable(a) => self.is_variable_lvalue_equal(a, b),
            LValue::ArrayIndex(a) => self.is_array_index_lvalue_equal(a, b),
            LValue::Grouping(a) => self.is_lvalue_equal(&a.sublvalue, b),
        }
    }

    fn is_expression_equal(&self, a: &Expression, b: &Expression) -> Option<bool> {
        match a {
            Expression::Literal(a) => self.is_literal_expression_equal(a, b),
            Expression::Array(a) => self.is_array_expression_equal(a, b),
            Expression::FunctionCall(a) => self.is_function_call_expression_equal(a, b),
            Expression::ArrayIndex(a) => self.is_array_index_expression_equal(a, b),
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
    fn is_function_call_expression_equal(
        &self,
        _a: &FunctionCallExpression,
        _b: &Expression,
    ) -> Option<bool> {
        // TODO: handle pure functions
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
    fn is_variable_access_expression_equal(
        &self,
        a: &VariableAccessExpression,
        b: &Expression,
    ) -> Option<bool> {
        Some(match b {
            Expression::VariableAccess(b) => {
                let a_data = self.variable_data.get(&a.id)?.borrow();
                let b_data = self.variable_data.get(&b.id)?.borrow();

                a_data.id == b_data.id && a_data.is_constant && b_data.is_constant // technically redundant, but lets be extra sure
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

impl<'errors, 'inputs> PartialAstVisitor for LValueReducer<'errors, 'inputs> {
    fn partial_visit_variable_lvalue(&mut self, a: &mut VariableLValue) {
        let Some(valid_lvalues) = &self.valid_lvalues else {
            // everything is valid
            return;
        };

        if !valid_lvalues
            .iter()
            .any(|b| self.is_variable_lvalue_equal(a, b).unwrap_or(false))
        {
            self.errors.push(FleetError::from_node(
                a,
                "This lvalue isn't available here",
                ErrorSeverity::Error,
            ));
        }
    }
    fn partial_visit_array_index_lvalue(&mut self, a: &mut ArrayIndexLValue) {
        let Some(valid_lvalues) = &self.valid_lvalues else {
            // everything is valid
            return;
        };
        if !valid_lvalues
            .iter()
            .any(|b| self.is_array_index_lvalue_equal(a, b).unwrap_or(false))
        {
            self.errors.push(FleetError::from_node(
                a,
                "This lvalue isn't available here",
                ErrorSeverity::Error,
            ));
        }
    }
    fn partial_visit_on_statement(
        &mut self,
        OnStatement {
            on_token: _,
            executor,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id: _,
        }: &mut OnStatement,
    ) {
        self.visit_executor(executor);

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

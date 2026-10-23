use std::ops::ControlFlow;

use crate::{
    ast::{ArrayIndexLValue, AstNodeRefMut, VariableLValue},
    passes::partial_visitor::{PartialAstVisitor, PartialAstVisitorExt},
};

pub fn find_top_level_binding<'a>(
    node: impl Into<AstNodeRefMut<'a>>,
) -> Option<&'a mut VariableLValue> {
    TopLevelBindingFinder.visit_node(node.into()).break_value()
}

struct TopLevelBindingFinder;

impl PartialAstVisitor for TopLevelBindingFinder {
    type Break<'a> = &'a mut VariableLValue;

    fn visit_variable_lvalue<'a>(
        &mut self,
        lvalue: &'a mut VariableLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        ControlFlow::Break(lvalue)
    }
    fn visit_array_index_lvalue<'a>(
        &mut self,
        ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index: _,
            close_bracket_token: _,
            id: _,
        }: &'a mut ArrayIndexLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        // only visit the array. indices are never the top-level binding
        self.visit_lvalue(array)
    }
}

use crate::{
    ast::{ArrayIndexLValue, AstVisitor, VariableLValue},
    passes::partial_visitor::PartialAstVisitor,
};

#[derive(Default)]
pub struct TopLevelBindingFinder {
    found_binding: Option<VariableLValue>,
}

impl TopLevelBindingFinder {
    pub fn get_result(self) -> Option<VariableLValue> {
        self.found_binding
    }
}

impl PartialAstVisitor for TopLevelBindingFinder {
    fn partial_visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) {
        self.found_binding = Some(lvalue.clone())
    }
    fn partial_visit_array_index_lvalue(
        &mut self,
        ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index: _,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayIndexLValue,
    ) {
        // only visit the array. indices are never the top-level binding
        self.visit_lvalue(array);
    }
}

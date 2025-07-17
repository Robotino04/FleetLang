use crate::{ast::VariableLValue, passes::partial_visitor::PartialAstVisitor};

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
}

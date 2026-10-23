use std::ops::ControlFlow;

use crate::{
    ast::{AstNodeRefMut, HasID, NodeID},
    passes::partial_visitor::{PartialAstVisitor, PartialAstVisitorExt},
};

pub fn find_node_by_id<'a>(
    node: impl Into<AstNodeRefMut<'a>>,
    wanted_id: NodeID,
) -> Option<AstNodeRefMut<'a>> {
    struct Finder(NodeID);

    impl PartialAstVisitor for Finder {
        type Break<'a> = AstNodeRefMut<'a>;

        fn visit_pre<'a>(&mut self, node: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
            if node.get_id() == self.0 {
                ControlFlow::Break(node)
            } else {
                ControlFlow::Continue(())
            }
        }
    }

    Finder(wanted_id).visit_node(node.into()).break_value()
}

use std::ops::ControlFlow;

use crate::ast::{AstNode, AstNodeRefMut, HasSourceRange};
use crate::passes::partial_visitor::{PartialAstVisitor, PartialAstVisitorExt};
use crate::tokenizer::{SourceLocation, Token};

pub fn find_containing_node<'a>(
    node: impl Into<AstNodeRefMut<'a>>,
    cursor: SourceLocation,
) -> Option<(Vec<AstNode>, Option<&'a mut Token>)> {
    let mut finder = FindContainingNodePass {
        node_hierarchy: Vec::new(),
        search_position: cursor,
    };

    match finder.visit_node(node.into()) {
        ControlFlow::Continue(()) => {
            // cursor wasn't inside `node` at all
            None
        }
        ControlFlow::Break(token) => Some((finder.node_hierarchy, token)),
    }
}

struct FindContainingNodePass {
    node_hierarchy: Vec<AstNode>,
    search_position: SourceLocation,
}

impl PartialAstVisitor for FindContainingNodePass {
    type Break<'a> = Option<&'a mut Token>;

    fn visit_pre<'a>(&mut self, node: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
        self.node_hierarchy.push(node.to_owned());

        ControlFlow::Continue(())
    }

    fn visit_token<'a>(&mut self, token: &'a mut Token) -> ControlFlow<Self::Break<'a>> {
        if token.range.contains(self.search_position) {
            ControlFlow::Break(Some(token))
        } else {
            ControlFlow::Continue(())
        }
    }

    fn visit_post<'a>(&mut self, node: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
        // if we reach this, the token wasn't found inside this node

        if node.get_source_range().contains(self.search_position) {
            // but, the cursor is still inside this node, so we are on trivia
            return ControlFlow::Break(None);
        }

        self.node_hierarchy.pop();

        ControlFlow::Continue(())
    }
}

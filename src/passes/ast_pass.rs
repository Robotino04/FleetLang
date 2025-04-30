use crate::ast::AstNode;

pub trait AstPass {
    type Output;
    fn run(&mut self, node: AstNode) -> Self::Output;
}

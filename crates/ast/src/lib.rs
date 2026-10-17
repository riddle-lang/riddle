use riddle_syntax::{SyntaxKind, SyntaxNode};
use crate::nodes::NodeFunc;

pub mod nodes;

pub trait AstNode: Sized + 'static {
    fn cast(node: SyntaxNode) -> Option<Self>;
    fn syntax(&self) -> &SyntaxNode;

    fn child<N: AstNode>(&self) -> Option<N> {
        self.syntax().children().find_map(N::cast)
    }

    fn children<N: AstNode>(&self) -> impl Iterator<Item = N> + '_ {
        self.syntax().children().filter_map(N::cast)
    }
}
use riddle_syntax::{SyntaxNode, SyntaxKind};
use crate::AstNode;

macro_rules! ast_node {
    ($name:ident, $kind:ident) => {
        pub struct $name{
            syntax: SyntaxNode
        }

        impl AstNode for $name{
            fn cast(node: SyntaxNode) -> Option<Self> {
                (node.kind() == SyntaxKind::$kind).then_some(Self { syntax: node } )
            }
            fn syntax(&self) -> &SyntaxNode {
                &self.syntax
            }
        }
    };
}

ast_node!(NodeName, Name);
impl NodeName{
    fn text(&self) -> String{
        self.syntax.to_string()
    }
}

ast_node!(NodeFunc, Function);
impl NodeFunc{
    fn name(&self) -> Option<NodeName>{
        self.child()
    }


}
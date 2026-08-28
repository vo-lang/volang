#![no_std]

use vo_ui_core::{NodeId, PropertyId, Value};
use vo_ui_protocol::{
    ApplyError, MutationBatch, NodeSnapshot, ProtocolLimits, Renderer, TreeMirror,
};

#[derive(Clone, Debug)]
pub struct HeadlessRenderer {
    tree: TreeMirror,
}

impl HeadlessRenderer {
    pub fn new(session_epoch: u64, root: NodeId, limits: ProtocolLimits) -> Self {
        Self {
            tree: TreeMirror::new(session_epoch, root, limits),
        }
    }

    pub const fn revision(&self) -> u64 {
        self.tree.revision()
    }

    pub const fn root(&self) -> NodeId {
        self.tree.root()
    }

    pub fn node(&self, id: NodeId) -> Option<NodeSnapshot> {
        self.tree.node(id)
    }

    pub fn property(&self, id: NodeId, property: PropertyId) -> Option<Value> {
        self.tree
            .node(id)
            .and_then(|node| node.properties.get(&property).cloned())
    }
}

impl Renderer for HeadlessRenderer {
    type Error = ApplyError;

    fn apply(&mut self, batch: &MutationBatch) -> Result<(), Self::Error> {
        self.tree.apply(batch)
    }
}

#[cfg(test)]
mod tests {
    extern crate alloc;

    use super::*;
    use alloc::string::ToString;
    use vo_ui_core::{Primitive, View};
    use vo_ui_runtime::UiRuntime;

    #[test]
    fn runtime_mount_and_text_update_commit_to_headless_tree() {
        let root = NodeId::new(0, 1);
        let renderer = HeadlessRenderer::new(42, root, ProtocolLimits::default());
        let mut runtime = UiRuntime::new(renderer, 42, root);
        runtime
            .mount(View::element(Primitive::Column).child(View::text("first")))
            .unwrap();
        let column = runtime.renderer().node(root).unwrap().children[0];
        let text = runtime.renderer().node(column).unwrap().children[0];
        assert_eq!(runtime.renderer().node(text).unwrap().text, "first");

        let batch = runtime
            .update(View::element(Primitive::Column).child(View::text("second")))
            .unwrap();
        assert_eq!(runtime.renderer().node(text).unwrap().text, "second");
        assert_eq!(runtime.revision(), 2);
        assert!(batch
            .mutations
            .iter()
            .any(|mutation| matches!(mutation, vo_ui_protocol::Mutation::SetText { text, .. } if text == "second")));
    }

    #[test]
    fn keyed_reorder_preserves_node_identity() {
        let root = NodeId::new(0, 1);
        let renderer = HeadlessRenderer::new(7, root, ProtocolLimits::default());
        let mut runtime = UiRuntime::new(renderer, 7, root);
        let list = |keys: &[&str]| {
            View::element(Primitive::Column).children(
                keys.iter()
                    .map(|key| View::text((*key).to_string()).key(*key)),
            )
        };
        runtime.mount(list(&["a", "b", "c"])).unwrap();
        let column = runtime.renderer().node(root).unwrap().children[0];
        let before = runtime.renderer().node(column).unwrap().children;
        runtime.update(list(&["c", "a", "b"])).unwrap();
        let after = runtime.renderer().node(column).unwrap().children;

        assert_eq!(after, alloc::vec![before[2], before[0], before[1]]);
    }
}

use std::collections::HashMap;

use rowan::{TextRange, ast::SyntaxNodePtr};

use hir::{
    HirFile, Name,
    body::{Body, BodyId, BodyItem, Expr, ExprId, PatId, Pattern, PatternBindingId, Stmt, StmtId},
    item_tree::{
        EnumId, FunctionId, HirPath, HirTypeRef, HirUseTree, HirUseTreeKind, ModuleId, PathAnchor,
        StructId, TopLevelItem,
    },
};
use syntax::{RiddleLang, SyntaxNode};

use super::{DefRef, EdgeId, EdgeKind, Fragment, Node, NodeId, RefOrigin, ScopeGraph, ScopeKind};

/// Builds a complete scope graph.
///
/// The builder starts from `ItemTree::top_level` and recursively encodes modules,
/// imports, functions, and structs. Function bodies are encoded as separate fragments.
#[must_use]
pub fn build_scope_graph(
    hir: &HirFile,
    root_syntax: &SyntaxNode,
) -> (ScopeGraph, Vec<hir::body::Diagnostic>) {
    ScopeGraphBuilder::new(hir, root_syntax).build()
}

#[derive(Clone, Copy)]
struct ModuleScopes {
    internal: NodeId,
    exported: NodeId,
}

pub struct ScopeGraphBuilder<'a> {
    sg: ScopeGraph,
    hir: &'a HirFile,
    root_ptr: SyntaxNodePtr<RiddleLang>,
    mod_scopes: HashMap<ModuleId, ModuleScopes>,
    modules_by_scope: HashMap<NodeId, Vec<ModuleId>>,
    diagnostics: Vec<hir::body::Diagnostic>,
}

impl<'a> ScopeGraphBuilder<'a> {
    #[must_use]
    pub fn new(hir: &'a HirFile, root_syntax: &SyntaxNode) -> Self {
        Self {
            sg: ScopeGraph::new(),
            hir,
            root_ptr: SyntaxNodePtr::new(root_syntax),
            diagnostics: Vec::new(),
            mod_scopes: HashMap::new(),
            modules_by_scope: HashMap::new(),
        }
    }

    #[must_use]
    pub fn build(mut self) -> (ScopeGraph, Vec<hir::body::Diagnostic>) {
        let root_scope = self.sg.root;

        // Pre-allocate module scopes because imports may refer to modules that have not
        // been encoded yet.
        self.pre_alloc_module_scopes();
        self.pre_alloc_impl_scopes();
        self.pre_alloc_enum_scopes();

        let mut frag_nodes = vec![];
        let mut frag_edges = vec![];
        let top_level = self.hir.item_tree.top_level.clone();

        self.diagnose_duplicate_items(&top_level);
        self.encode_items(&top_level, root_scope, &mut frag_nodes, &mut frag_edges);

        // Wrap root-level graph pieces in one root fragment.
        let root_ptr = self.root_ptr;
        let frag = Fragment {
            ptr: root_ptr,
            nodes: frag_nodes,
            edges: frag_edges,
            entry_scope: root_scope,
            parent_anchor: None,
        };
        let fid = self.sg.fragments.alloc(frag);
        self.sg.by_ptr.insert(root_ptr, fid);

        (self.sg, self.diagnostics)
    }

    fn diagnose_duplicate_items(&mut self, items: &[TopLevelItem]) {
        let mut definitions = HashMap::<String, TextRange>::new();
        for &item in items {
            if let Some((name, range)) = self.named_item(item) {
                if let Some(first_range) = definitions.get(&name.0) {
                    self.diagnostics.push(hir::body::Diagnostic {
                        code: "E0064",
                        severity: hir::body::Severity::Error,
                        message: format!("the name `{}` is defined multiple times", name.0),
                        labels: vec![
                            hir::body::SourceLabel {
                                range,
                                message: "duplicate definition".into(),
                                style: hir::body::LabelStyle::Primary,
                            },
                            hir::body::SourceLabel {
                                range: *first_range,
                                message: "first definition".into(),
                                style: hir::body::LabelStyle::Secondary,
                            },
                        ],
                        help: None,
                        notes: Vec::new(),
                    });
                } else {
                    definitions.insert(name.0, range);
                }
            }

            if let TopLevelItem::Module(mid) = item
                && let Some(children) = self.hir.item_tree.modules[mid].items.clone()
            {
                self.diagnose_duplicate_items(&children);
            }
        }
    }

    fn named_item(&self, item: TopLevelItem) -> Option<(Name, TextRange)> {
        match item {
            TopLevelItem::Function(id) => {
                let item = &self.hir.item_tree.functions[id];
                Some((item.name.clone(), item.name_range))
            }
            TopLevelItem::Struct(id) => {
                let item = &self.hir.item_tree.structs[id];
                Some((item.name.clone(), item.name_range))
            }
            TopLevelItem::Enum(id) => {
                let item = &self.hir.item_tree.enums[id];
                Some((item.name.clone(), item.name_range))
            }
            TopLevelItem::Trait(id) => {
                let item = &self.hir.item_tree.traits[id];
                Some((item.name.clone(), item.name_range))
            }
            TopLevelItem::Const(id) => {
                let item = &self.hir.item_tree.consts[id];
                Some((item.name.clone(), item.name_range))
            }
            TopLevelItem::TypeAlias(id) => {
                let item = &self.hir.item_tree.type_aliases[id];
                Some((item.name.clone(), item.name_range))
            }
            TopLevelItem::Module(id) => {
                let item = &self.hir.item_tree.modules[id];
                Some((item.name.clone(), item.name_range))
            }
            TopLevelItem::Use(_) | TopLevelItem::Impl(_) => None,
        }
    }

    fn pre_alloc_module_scopes(&mut self) {
        for (mid, _) in self.hir.item_tree.modules.iter() {
            let internal = self.sg.alloc_node(Node::Scope(ScopeKind::ModInternal));
            let exported = self.sg.alloc_node(Node::Scope(ScopeKind::ModExported));
            self.mod_scopes
                .insert(mid, ModuleScopes { internal, exported });
        }
    }

    fn pre_alloc_impl_scopes(&mut self) {
        for (sid, _) in self.hir.item_tree.structs.iter() {
            let scope = self.sg.alloc_node(Node::Scope(ScopeKind::ImplScope));
            self.sg.impl_scopes_by_struct.insert(sid, scope);
        }
    }

    fn pre_alloc_enum_scopes(&mut self) {
        for (eid, _) in self.hir.item_tree.enums.iter() {
            let scope = self.sg.alloc_node(Node::Scope(ScopeKind::Block));
            self.sg.variant_scopes_by_enum.insert(eid, scope);
        }
    }

    fn encode_items(
        &mut self,
        items: &[TopLevelItem],
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        self.index_module_children(items, parent_scope);
        for &item in items {
            self.encode_item(item, parent_scope, frag_nodes, frag_edges);
        }
    }

    fn encode_item(
        &mut self,
        item: TopLevelItem,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        match item {
            TopLevelItem::Function(fid) => {
                self.encode_function_type_refs(fid, parent_scope, frag_nodes, frag_edges);
                self.encode_function(fid, parent_scope, frag_nodes, frag_edges);
            }
            TopLevelItem::Struct(sid) => {
                self.encode_struct_item(sid, parent_scope, frag_nodes, frag_edges);
            }
            TopLevelItem::Enum(eid) => {
                self.encode_enum_item(eid, parent_scope, frag_nodes, frag_edges);
            }
            TopLevelItem::Trait(tid) => {
                self.encode_trait_item(tid, parent_scope, frag_nodes, frag_edges);
            }
            TopLevelItem::Const(cid) => {
                self.encode_const_item(cid, parent_scope, frag_nodes, frag_edges);
            }
            TopLevelItem::TypeAlias(tid) => {
                self.encode_type_alias_item(tid, parent_scope, frag_nodes, frag_edges);
            }
            TopLevelItem::Module(mid) => {
                self.encode_module(mid, parent_scope, frag_nodes, frag_edges);
            }
            TopLevelItem::Use(uid) => {
                let use_item = self.hir.item_tree.uses[uid].clone();
                self.encode_use_tree(&use_item.tree, parent_scope, frag_nodes, frag_edges);
            }
            TopLevelItem::Impl(iid) => {
                self.encode_impl(iid, parent_scope, frag_nodes, frag_edges);
            }
        }
    }

    fn encode_struct_item(
        &mut self,
        sid: StructId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let strukt = self.hir.item_tree.structs[sid].clone();
        self.emit_bound_type_references(
            &strukt.generic_bounds,
            parent_scope,
            frag_nodes,
            frag_edges,
        );
        for field in &strukt.fields {
            self.emit_type_references(&field.ty, parent_scope, frag_nodes, frag_edges);
        }
        self.encode_struct_impl_scope(sid, parent_scope, frag_nodes, frag_edges);
        let name = self.hir.item_tree.structs[sid].name.clone();
        self.emit_named_def(
            parent_scope,
            name,
            DefRef::Struct(sid),
            frag_nodes,
            frag_edges,
        );
    }

    fn encode_enum_item(
        &mut self,
        eid: EnumId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let enumeration = self.hir.item_tree.enums[eid].clone();
        self.emit_bound_type_references(
            &enumeration.generic_bounds,
            parent_scope,
            frag_nodes,
            frag_edges,
        );
        for variant in &enumeration.variants {
            match &variant.kind {
                hir::item_tree::HirVariantKind::Unit => {}
                hir::item_tree::HirVariantKind::Tuple(fields) => {
                    for field in fields {
                        self.emit_type_references(field, parent_scope, frag_nodes, frag_edges);
                    }
                }
                hir::item_tree::HirVariantKind::Struct(fields) => {
                    for field in fields {
                        self.emit_type_references(&field.ty, parent_scope, frag_nodes, frag_edges);
                    }
                }
            }
        }
        let name = self.hir.item_tree.enums[eid].name.clone();
        self.emit_named_def(
            parent_scope,
            name,
            DefRef::Enum(eid),
            frag_nodes,
            frag_edges,
        );
        self.encode_enum_variant_scope(eid, parent_scope, frag_nodes, frag_edges);
    }

    fn encode_trait_item(
        &mut self,
        tid: hir::item_tree::TraitId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let trait_item = self.hir.item_tree.traits[tid].clone();
        self.emit_bound_type_references(
            &trait_item.generic_bounds,
            parent_scope,
            frag_nodes,
            frag_edges,
        );
        self.emit_bound_type_references(
            &trait_item.supertraits,
            parent_scope,
            frag_nodes,
            frag_edges,
        );
        for default in trait_item.generic_defaults.iter().flatten() {
            self.emit_type_references(default, parent_scope, frag_nodes, frag_edges);
        }
        for method in &trait_item.methods {
            self.emit_function_signature_type_references(
                method,
                parent_scope,
                frag_nodes,
                frag_edges,
            );
        }
        for alias in &trait_item.type_aliases {
            if let Some(ty) = &alias.ty {
                self.emit_type_references(ty, parent_scope, frag_nodes, frag_edges);
            }
        }
        let name = trait_item.name.clone();
        self.emit_named_def(
            parent_scope,
            name,
            DefRef::Trait(tid),
            frag_nodes,
            frag_edges,
        );
        for fid in trait_item.default_methods {
            self.encode_function_type_refs(fid, parent_scope, frag_nodes, frag_edges);
            self.encode_function_body(fid, parent_scope, frag_nodes, frag_edges);
        }
    }

    fn encode_const_item(
        &mut self,
        cid: hir::item_tree::ConstId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let ty = self.hir.item_tree.consts[cid].ty.clone();
        self.emit_type_references(&ty, parent_scope, frag_nodes, frag_edges);
        let name = self.hir.item_tree.consts[cid].name.clone();
        self.emit_named_def(
            parent_scope,
            name,
            DefRef::Const(cid),
            frag_nodes,
            frag_edges,
        );
        self.encode_const_body(cid, parent_scope);
    }

    fn encode_type_alias_item(
        &mut self,
        tid: hir::item_tree::TypeAliasId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        if let Some(ty) = self.hir.item_tree.type_aliases[tid].ty.clone() {
            self.emit_type_references(&ty, parent_scope, frag_nodes, frag_edges);
        }
        let name = self.hir.item_tree.type_aliases[tid].name.clone();
        self.emit_named_def(
            parent_scope,
            name,
            DefRef::TypeAlias(tid),
            frag_nodes,
            frag_edges,
        );
    }

    fn index_module_children(&mut self, items: &[TopLevelItem], parent_scope: NodeId) {
        for it in items {
            let TopLevelItem::Module(mid) = it else {
                continue;
            };

            self.register_module_child(parent_scope, *mid);

            let children = self.hir.item_tree.modules[*mid].items.clone();
            if let Some(children) = children {
                let internal = self.mod_scopes[mid].internal;
                self.index_module_children(&children, internal);
            }
        }
    }

    fn emit_named_def(
        &mut self,
        parent_scope: NodeId,
        name: Name,
        def: DefRef,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let pop = self.sg.alloc_node(Node::PopSymbol { name, define: def });
        frag_nodes.push(pop);
        let e = self.sg.add_edge(parent_scope, pop, EdgeKind::Def, 0);
        frag_edges.push(e);
    }

    fn export_item(
        &mut self,
        item: TopLevelItem,
        export_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        match item {
            TopLevelItem::Function(fid)
                if self.hir.item_tree.functions[fid].visibility.is_public() =>
            {
                let name = self.hir.item_tree.functions[fid].name.clone();
                self.emit_named_def(
                    export_scope,
                    name,
                    DefRef::Function(fid),
                    frag_nodes,
                    frag_edges,
                );
            }
            TopLevelItem::Struct(sid) if self.hir.item_tree.structs[sid].visibility.is_public() => {
                let name = self.hir.item_tree.structs[sid].name.clone();
                self.emit_named_def(
                    export_scope,
                    name,
                    DefRef::Struct(sid),
                    frag_nodes,
                    frag_edges,
                );
            }
            TopLevelItem::Enum(eid) if self.hir.item_tree.enums[eid].visibility.is_public() => {
                let name = self.hir.item_tree.enums[eid].name.clone();
                self.emit_named_def(
                    export_scope,
                    name,
                    DefRef::Enum(eid),
                    frag_nodes,
                    frag_edges,
                );
            }
            TopLevelItem::Trait(tid) if self.hir.item_tree.traits[tid].visibility.is_public() => {
                let name = self.hir.item_tree.traits[tid].name.clone();
                self.emit_named_def(
                    export_scope,
                    name,
                    DefRef::Trait(tid),
                    frag_nodes,
                    frag_edges,
                );
            }
            TopLevelItem::Const(cid) if self.hir.item_tree.consts[cid].visibility.is_public() => {
                let name = self.hir.item_tree.consts[cid].name.clone();
                self.emit_named_def(
                    export_scope,
                    name,
                    DefRef::Const(cid),
                    frag_nodes,
                    frag_edges,
                );
            }
            TopLevelItem::TypeAlias(tid)
                if self.hir.item_tree.type_aliases[tid].visibility.is_public() =>
            {
                let name = self.hir.item_tree.type_aliases[tid].name.clone();
                self.emit_named_def(
                    export_scope,
                    name,
                    DefRef::TypeAlias(tid),
                    frag_nodes,
                    frag_edges,
                );
            }
            TopLevelItem::Module(mid) if self.hir.item_tree.modules[mid].visibility.is_public() => {
                let scopes = self.mod_scopes[&mid];
                let name = self.hir.item_tree.modules[mid].name.clone();
                self.emit_named_def(
                    export_scope,
                    name,
                    DefRef::Module {
                        id: mid,
                        enter: scopes.exported,
                    },
                    frag_nodes,
                    frag_edges,
                );
            }
            TopLevelItem::Use(uid) if self.hir.item_tree.uses[uid].visibility.is_public() => {
                let u = self.hir.item_tree.uses[uid].clone();
                self.encode_use_tree(&u.tree, export_scope, frag_nodes, frag_edges);
            }
            _ => {}
        }
    }

    fn register_module_child(&mut self, parent_scope: NodeId, module_id: ModuleId) {
        let siblings = self.modules_by_scope.entry(parent_scope).or_default();
        if !siblings.contains(&module_id) {
            siblings.push(module_id);
        }
    }

    fn encode_function(
        &mut self,
        fid: FunctionId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        self.encode_function_def(fid, parent_scope, frag_nodes, frag_edges);
        self.encode_function_body(fid, parent_scope, frag_nodes, frag_edges);
    }

    fn encode_function_type_refs(
        &mut self,
        fid: FunctionId,
        parent_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        let function = self.hir.item_tree.functions[fid].clone();
        self.emit_function_signature_type_references(&function, parent_scope, nodes, edges);
    }

    fn emit_function_signature_type_references(
        &mut self,
        function: &hir::item_tree::HirFunction,
        current_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        self.emit_bound_type_references(&function.generic_bounds, current_scope, nodes, edges);
        for param in &function.params {
            self.emit_type_references(&param.ty, current_scope, nodes, edges);
        }
        if let Some(ret) = &function.ret_type {
            self.emit_type_references(ret, current_scope, nodes, edges);
        }
    }

    fn emit_bound_type_references(
        &mut self,
        bounds: &[hir::item_tree::HirGenericBound],
        current_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        for bound in bounds {
            self.emit_type_references(&bound.target_ty, current_scope, nodes, edges);
            self.emit_type_references(&bound.trait_ty, current_scope, nodes, edges);
            if let Some(signature) = &bound.callable {
                for param in &signature.params {
                    self.emit_type_references(param, current_scope, nodes, edges);
                }
                self.emit_type_references(&signature.ret, current_scope, nodes, edges);
            }
            for constraint in &bound.assoc_constraints {
                self.emit_type_references(&constraint.ty, current_scope, nodes, edges);
            }
        }
    }

    fn encode_function_def(
        &mut self,
        fid: FunctionId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let func = &self.hir.item_tree.functions[fid];

        // Register the function name in its parent scope.
        let pop = self.sg.alloc_node(Node::PopSymbol {
            name: func.name.clone(),
            define: DefRef::Function(fid),
        });
        frag_nodes.push(pop);
        let e = self.sg.add_edge(parent_scope, pop, EdgeKind::Def, 0);
        frag_edges.push(e);
    }

    fn encode_function_body(
        &mut self,
        fid: FunctionId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let func = &self.hir.item_tree.functions[fid];

        // Create the function scope shared by parameters and the body root.
        let fn_scope = self.sg.alloc_node(Node::Scope(ScopeKind::FunctionScope));
        frag_nodes.push(fn_scope);
        let e = self.sg.add_edge(fn_scope, parent_scope, EdgeKind::Lex, 0);
        frag_edges.push(e);

        // Register parameter definitions.
        for (idx, p) in func.params.iter().enumerate() {
            let pop = self.sg.alloc_node(Node::PopSymbol {
                name: p.name.clone(),
                define: DefRef::Param {
                    fn_id: fid,
                    index: idx,
                },
            });
            frag_nodes.push(pop);
            let e = self.sg.add_edge(fn_scope, pop, EdgeKind::Def, 0);
            frag_edges.push(e);
        }

        for name in self
            .impl_const_generics_for_method(fid)
            .into_iter()
            .chain(func.const_generics.clone())
        {
            let pop = self.sg.alloc_node(Node::PopSymbol {
                name: name.clone(),
                define: DefRef::ConstParam { name },
            });
            frag_nodes.push(pop);
            let e = self.sg.add_edge(fn_scope, pop, EdgeKind::Def, 0);
            frag_edges.push(e);
        }

        // Encode the body as a separate fragment.
        if let Some(bid) = self.hir.function_bodies.get(&fid).copied() {
            let body = &self.hir.bodies[bid];
            self.encode_body_as_fragment(bid, body, fn_scope);
        }
    }

    fn impl_const_generics_for_method(&self, fid: FunctionId) -> Vec<Name> {
        self.hir
            .item_tree
            .impls
            .iter()
            .find_map(|(_, imp)| {
                imp.methods
                    .contains(&fid)
                    .then(|| imp.const_generics.clone())
            })
            .unwrap_or_default()
    }

    fn encode_struct_impl_scope(
        &mut self,
        sid: StructId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let Some(scope) = self.sg.impl_scopes_by_struct.get(&sid).copied() else {
            return;
        };

        frag_nodes.push(scope);
        let e = self.sg.add_edge(scope, parent_scope, EdgeKind::Lex, 0);
        frag_edges.push(e);
    }

    fn encode_enum_variant_scope(
        &mut self,
        eid: EnumId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let Some(scope) = self.sg.variant_scopes_by_enum.get(&eid).copied() else {
            return;
        };

        frag_nodes.push(scope);
        let e = self.sg.add_edge(scope, parent_scope, EdgeKind::Lex, 0);
        frag_edges.push(e);

        let enum_data = &self.hir.item_tree.enums[eid];
        for (idx, variant) in enum_data.variants.iter().enumerate() {
            self.emit_named_def(
                scope,
                variant.name.clone(),
                DefRef::EnumVariant {
                    enum_id: eid,
                    index: idx,
                },
                frag_nodes,
                frag_edges,
            );
        }
    }

    fn encode_impl(
        &mut self,
        iid: hir::item_tree::ImplId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let imp = &self.hir.item_tree.impls[iid];
        let associated_scope = self.impl_scope_for_self_ty(&imp.self_ty, imp.self_ty_range);
        let item_scope = match associated_scope {
            Some(scope) => scope,
            None => {
                let scope = self.sg.alloc_node(Node::Scope(ScopeKind::ImplScope));
                frag_nodes.push(scope);
                let edge = self.sg.add_edge(scope, parent_scope, EdgeKind::Lex, 0);
                frag_edges.push(edge);
                scope
            }
        };
        let resolution_scope = associated_scope.unwrap_or(parent_scope);
        let self_ty = imp.self_ty.clone();
        let trait_ty = imp.trait_ty.clone();
        let bounds = imp.generic_bounds.clone();

        self.emit_type_references(&self_ty, parent_scope, frag_nodes, frag_edges);
        if let Some(trait_ty) = &trait_ty {
            self.emit_type_references(trait_ty, parent_scope, frag_nodes, frag_edges);
        }
        self.emit_bound_type_references(&bounds, parent_scope, frag_nodes, frag_edges);

        for fid in imp.methods.clone() {
            self.encode_function_type_refs(fid, resolution_scope, frag_nodes, frag_edges);
            self.encode_function_def(fid, item_scope, frag_nodes, frag_edges);
            self.encode_function_body(fid, resolution_scope, frag_nodes, frag_edges);
        }

        for cid in imp.consts.clone() {
            let ty = self.hir.item_tree.consts[cid].ty.clone();
            self.emit_type_references(&ty, resolution_scope, frag_nodes, frag_edges);
            let name = self.hir.item_tree.consts[cid].name.clone();
            let pop = self.sg.alloc_node(Node::PopSymbol {
                name,
                define: DefRef::Const(cid),
            });
            frag_nodes.push(pop);
            let e = self.sg.add_edge(item_scope, pop, EdgeKind::Def, 0);
            frag_edges.push(e);
            self.encode_const_body(cid, resolution_scope);
        }

        for tid in imp.type_aliases.clone() {
            if let Some(ty) = self.hir.item_tree.type_aliases[tid].ty.clone() {
                self.emit_type_references(&ty, resolution_scope, frag_nodes, frag_edges);
            }
            let name = self.hir.item_tree.type_aliases[tid].name.clone();
            let pop = self.sg.alloc_node(Node::PopSymbol {
                name,
                define: DefRef::TypeAlias(tid),
            });
            frag_nodes.push(pop);
            let e = self.sg.add_edge(item_scope, pop, EdgeKind::Def, 0);
            frag_edges.push(e);
        }
    }

    /// The impl scope belonging to the struct an `impl` block is written for.
    ///
    /// The scope is keyed by struct id, and a name alone does not identify one:
    /// every package contributes its top-level items to the same graph, so
    /// `impl<I, J, T> Iterator for Chain<I, J, T>` in std and a user's own
    /// `struct Chain` share the name. The struct declared in the impl's own
    /// package wins; a single match is unaffected.
    fn impl_scope_for_self_ty(
        &self,
        self_ty: &HirTypeRef,
        self_ty_range: TextRange,
    ) -> Option<NodeId> {
        let HirTypeRef::Named(path) = self_ty else {
            return None;
        };
        let name = path.as_single_name()?;
        let package = self.hir.package_for_range(self_ty_range);
        let by_name = |strukt: &hir::item_tree::HirStruct| strukt.name == *name;
        let sid = self
            .hir
            .item_tree
            .structs
            .iter()
            .find_map(|(sid, strukt)| {
                (by_name(strukt) && self.hir.package_for_range(strukt.name_range) == package)
                    .then_some(sid)
            })
            .or_else(|| {
                self.hir
                    .item_tree
                    .structs
                    .iter()
                    .find_map(|(sid, strukt)| by_name(strukt).then_some(sid))
            })?;
        self.sg.impl_scopes_by_struct.get(&sid).copied()
    }

    fn encode_module(
        &mut self,
        mid: ModuleId,
        parent_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        let m = self.hir.item_tree.modules[mid].clone();
        let scopes = self.mod_scopes[&mid];

        frag_nodes.push(scopes.internal);
        frag_nodes.push(scopes.exported);

        // Nested modules can see names from their lexical parent scope.
        let e = self
            .sg
            .add_edge(scopes.internal, parent_scope, EdgeKind::Lex, 0);
        frag_edges.push(e);

        // Register the module name in its parent scope.
        let pop = self.sg.alloc_node(Node::PopSymbol {
            name: m.name.clone(),
            define: DefRef::Module {
                id: mid,
                enter: scopes.exported,
            },
        });
        frag_nodes.push(pop);
        let e = self.sg.add_edge(parent_scope, pop, EdgeKind::Def, 0);
        frag_edges.push(e);

        // Encode child items inside the module's internal scope.
        if let Some(children) = &m.items {
            self.encode_items(children, scopes.internal, frag_nodes, frag_edges);
            for child in children {
                self.export_item(*child, scopes.exported, frag_nodes, frag_edges);
            }
        }
    }

    fn encode_use_tree(
        &mut self,
        tree: &HirUseTree,
        current_scope: NodeId,
        frag_nodes: &mut Vec<NodeId>,
        frag_edges: &mut Vec<EdgeId>,
    ) {
        match &tree.kind {
            HirUseTreeKind::Simple { alias } => {
                let exposed = alias
                    .clone()
                    .or_else(|| tree.prefix.segments.last().cloned());
                let Some(exposed) = exposed else {
                    self.diagnostics.push(hir::body::Diagnostic {
                        code: "E0051",
                        severity: hir::body::Severity::Error,
                        message: "empty use declaration".into(),
                        labels: vec![hir::body::SourceLabel {
                            range: tree.range,
                            message: String::new(),
                            style: hir::body::LabelStyle::Primary,
                        }],
                        help: None,
                        notes: Vec::new(),
                    });
                    return;
                };

                // `rewrite_to` stores the path segments only; the anchor is stored separately.
                let rewrite_to = tree.prefix.segments.clone();
                let anchor = self.anchor_scope_for(tree.prefix.anchor, current_scope);

                let pop = self.sg.alloc_node(Node::PopSymbol {
                    name: exposed,
                    define: DefRef::UseAlias {
                        rewrite_to,
                        anchor,
                        use_range: tree.range,
                    },
                });
                frag_nodes.push(pop);
                let e = self.sg.add_edge(current_scope, pop, EdgeKind::Def, 0);
                frag_edges.push(e);
            }
            HirUseTreeKind::List(children) => {
                for child in children {
                    let joined = Self::join_path(&tree.prefix, &child.prefix);
                    let joined = HirUseTree {
                        prefix: joined,
                        kind: child.kind.clone(),
                        range: child.range,
                    };
                    self.encode_use_tree(&joined, current_scope, frag_nodes, frag_edges);
                }
            }
            HirUseTreeKind::Glob => {
                if let Some(target) = self.resolve_path_scope(&tree.prefix, current_scope) {
                    let e = self
                        .sg
                        .add_edge(current_scope, target, EdgeKind::Export, -1);
                    frag_edges.push(e);
                } else {
                    self.diagnostics.push(hir::body::Diagnostic {
                        code: "E0052",
                        severity: hir::body::Severity::Error,
                        message: format!(
                            "glob import target not found: `{}`",
                            tree.prefix.display()
                        ),
                        labels: vec![hir::body::SourceLabel {
                            range: tree.range,
                            message: String::new(),
                            style: hir::body::LabelStyle::Primary,
                        }],
                        help: None,
                        notes: Vec::new(),
                    });
                }
            }
        }
    }

    fn lexical_parent_scope(&self, scope: NodeId) -> Option<NodeId> {
        self.sg.out_edges.get(&scope).and_then(|v| {
            v.iter().find_map(|eid| {
                let e = self.sg.edges[*eid];
                if e.kind == EdgeKind::Lex {
                    Some(e.to)
                } else {
                    None
                }
            })
        })
    }

    fn anchor_scope_for(&self, anchor: PathAnchor, current: NodeId) -> NodeId {
        match anchor {
            PathAnchor::Plain | PathAnchor::SelfMod => current,
            PathAnchor::Crate | PathAnchor::Absolute => self.sg.root,
            PathAnchor::Super => self.lexical_parent_scope(current).unwrap_or(self.sg.root),
        }
    }

    fn resolve_path_scope(&self, path: &HirPath, current_scope: NodeId) -> Option<NodeId> {
        let mut lookup_scope = self.anchor_scope_for(path.anchor, current_scope);
        let mut result_scope = None;
        let mut first_segment = true;

        for segment in &path.segments {
            let module_id = if first_segment && matches!(path.anchor, PathAnchor::Plain) {
                self.resolve_visible_module(lookup_scope, segment)
            } else {
                self.resolve_module_in_scope(lookup_scope, segment)
            };

            let module_id = module_id?;

            let scopes = self.mod_scopes[&module_id];
            lookup_scope = scopes.internal;
            result_scope = Some(scopes.exported);
            first_segment = false;
        }

        result_scope
    }

    fn resolve_visible_module(&self, mut scope: NodeId, name: &Name) -> Option<ModuleId> {
        loop {
            if let Some(mid) = self.resolve_module_in_scope(scope, name) {
                return Some(mid);
            }
            scope = self.lexical_parent_scope(scope)?;
        }
    }

    fn resolve_module_in_scope(&self, scope: NodeId, name: &Name) -> Option<ModuleId> {
        self.modules_by_scope
            .get(&scope)?
            .iter()
            .copied()
            .find(|mid| self.hir.item_tree.modules[*mid].name == *name)
    }

    fn join_path(prefix: &HirPath, child: &HirPath) -> HirPath {
        if !matches!(child.anchor, PathAnchor::Plain) {
            return child.clone();
        }
        let mut segs = prefix.segments.clone();
        let child_offset = segs.len();
        segs.extend(child.segments.iter().cloned());
        let mut segment_type_args = prefix.segment_type_args.clone();
        segment_type_args.extend(
            child
                .segment_type_args
                .iter()
                .map(|(index, args)| (child_offset + index, args.clone())),
        );
        HirPath {
            anchor: prefix.anchor,
            segments: segs,
            segment_type_args,
            type_args: Vec::new(),
            range: child.range,
        }
    }

    // ===== body fragment =====

    fn encode_body_as_fragment(&mut self, body_id: BodyId, body: &Body, fn_scope: NodeId) {
        // Each function body is encoded as one fragment. Inner blocks can be split later if finer
        // invalidation granularity is needed.
        let mut nodes = vec![];
        let mut edges = vec![];

        let entry = self.encode_block_expr(
            body_id,
            body,
            body.root_block,
            fn_scope,
            &mut nodes,
            &mut edges,
        );

        // Use the body's root block syntax pointer as the fragment key so this body can be
        // independently invalidated when its source range changes.
        let ptr = body.root_ptr;

        let frag = Fragment {
            ptr,
            nodes,
            edges,
            entry_scope: entry,
            parent_anchor: Some(fn_scope),
        };
        let fid = self.sg.fragments.alloc(frag);
        self.sg.by_ptr.insert(ptr, fid);
    }

    /// Encodes a block expression and returns the block's entry scope.
    fn encode_block_expr(
        &mut self,
        body_id: BodyId,
        body: &Body,
        block_expr: ExprId,
        parent_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) -> NodeId {
        let block_scope = self.sg.alloc_node(Node::Scope(ScopeKind::Block));
        nodes.push(block_scope);
        let e = self
            .sg
            .add_edge(block_scope, parent_scope, EdgeKind::Lex, 0);
        edges.push(e);

        if let Expr::Block { stmts, tail } = &body.exprs[block_expr] {
            self.index_body_module_items(body, stmts, block_scope);

            let mut current_scope = block_scope;
            for sid in stmts {
                current_scope =
                    self.encode_body_stmt(body_id, body, *sid, current_scope, nodes, edges);
            }
            if let Some(t) = tail {
                self.walk_expr_for_refs(body_id, body, *t, current_scope, nodes, edges);
            }
        }
        block_scope
    }

    fn encode_body_stmt(
        &mut self,
        body_id: BodyId,
        body: &Body,
        sid: StmtId,
        current_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) -> NodeId {
        match &body.stmts[sid] {
            Stmt::Let {
                pat,
                ty,
                init,
                else_,
                ..
            } => {
                self.emit_type_references(ty, current_scope, nodes, edges);
                let recursive_scope = init.and_then(|init| {
                    matches!(body.pats[*pat], Pattern::Binding { .. })
                        .then_some(())
                        .filter(|_| match &body.exprs[init] {
                            Expr::Lambda {
                                params, ret_type, ..
                            } => {
                                params.iter().all(|param| {
                                    !matches!(param.ty, hir::item_tree::HirTypeRef::Unknown)
                                }) && !matches!(ret_type, hir::item_tree::HirTypeRef::Unknown)
                            }
                            _ => false,
                        })
                        .map(|()| {
                            self.walk_pat_for_bindings(body, *pat, current_scope, nodes, edges)
                        })
                });
                if let Some(init) = init {
                    self.walk_expr_for_refs(
                        body_id,
                        body,
                        *init,
                        recursive_scope.unwrap_or(current_scope),
                        nodes,
                        edges,
                    );
                }
                if let Some(else_) = else_ {
                    // The else block runs when the pattern did not match, so
                    // the bindings are not visible inside it.
                    self.walk_expr_for_refs(body_id, body, *else_, current_scope, nodes, edges);
                }

                // `let` bindings become visible only after the initializer, and
                // ride the same pattern machinery as match arms and for loops.
                recursive_scope.unwrap_or_else(|| {
                    self.walk_pat_for_bindings(body, *pat, current_scope, nodes, edges)
                })
            }
            Stmt::Return { value } => {
                if let Some(v) = value {
                    self.walk_expr_for_refs(body_id, body, *v, current_scope, nodes, edges);
                }
                current_scope
            }
            Stmt::Break { value } => {
                if let Some(v) = value {
                    self.walk_expr_for_refs(body_id, body, *v, current_scope, nodes, edges);
                }
                current_scope
            }
            Stmt::Continue => current_scope,
            Stmt::Expr { expr } => {
                self.walk_expr_for_refs(body_id, body, *expr, current_scope, nodes, edges);
                current_scope
            }
            Stmt::Item { item } => {
                match item {
                    BodyItem::Module(mid) => {
                        // Encode an inline module into the current scope.
                        self.register_module_child(current_scope, *mid);
                        self.encode_module(*mid, current_scope, nodes, edges);
                    }
                    BodyItem::Use(uid) => {
                        let u = self.hir.item_tree.uses[*uid].clone();
                        self.encode_use_tree(&u.tree, current_scope, nodes, edges);
                    }
                }
                current_scope
            }
        }
    }

    fn walk_expr_for_refs(
        &mut self,
        body_id: BodyId,
        body: &Body,
        eid: ExprId,
        current_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        match &body.exprs[eid] {
            Expr::Missing
            | Expr::IntLiteral { .. }
            | Expr::FloatLiteral { .. }
            | Expr::StringLiteral { .. }
            | Expr::CharLiteral { .. }
            | Expr::BoolLiteral { .. } => {}
            Expr::Path { path, .. } => {
                self.emit_path_reference(body_id, eid, path, current_scope, nodes, edges);
            }
            Expr::Struct { path, fields, .. } => {
                self.emit_path_reference(body_id, eid, path, current_scope, nodes, edges);
                for field in fields {
                    self.walk_expr_for_refs(
                        body_id,
                        body,
                        field.value,
                        current_scope,
                        nodes,
                        edges,
                    );
                }
            }
            Expr::Binary { lhs, rhs, .. } => {
                self.walk_expr_for_refs(body_id, body, *lhs, current_scope, nodes, edges);
                self.walk_expr_for_refs(body_id, body, *rhs, current_scope, nodes, edges);
            }
            Expr::Unary { operand, .. }
            | Expr::Try { operand }
            | Expr::Unsafe { body: operand } => {
                self.walk_expr_for_refs(body_id, body, *operand, current_scope, nodes, edges);
            }
            Expr::Call {
                callee,
                args,
                type_args,
            } => {
                self.walk_expr_for_refs(body_id, body, *callee, current_scope, nodes, edges);
                for ty in type_args {
                    self.emit_type_references(ty, current_scope, nodes, edges);
                }
                for a in args {
                    self.walk_expr_for_refs(body_id, body, *a, current_scope, nodes, edges);
                }
            }
            Expr::Lambda { .. } => {
                self.walk_lambda_for_refs(body_id, body, eid, current_scope, nodes, edges);
            }
            Expr::FieldAccess { base, .. } => {
                self.walk_expr_for_refs(body_id, body, *base, current_scope, nodes, edges);
            }

            Expr::IndexAccess { base, index } => {
                self.walk_expr_for_refs(body_id, body, *base, current_scope, nodes, edges);
                self.walk_expr_for_refs(body_id, body, *index, current_scope, nodes, edges);
            }
            Expr::Block { .. } => {
                self.walk_block_for_refs(body_id, body, eid, current_scope, nodes, edges);
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.walk_expr_for_refs(body_id, body, *cond, current_scope, nodes, edges);
                self.walk_expr_for_refs(body_id, body, *then_branch, current_scope, nodes, edges);
                if let Some(e) = else_branch {
                    self.walk_expr_for_refs(body_id, body, *e, current_scope, nodes, edges);
                }
            }
            Expr::While { condition, body: b } => {
                self.walk_expr_for_refs(body_id, body, *condition, current_scope, nodes, edges);
                self.walk_expr_for_refs(body_id, body, *b, current_scope, nodes, edges);
            }
            Expr::Loop { body: b } => {
                self.walk_expr_for_refs(body_id, body, *b, current_scope, nodes, edges);
            }
            Expr::For {
                pat,
                iterable,
                body: b,
            } => {
                self.walk_expr_for_refs(body_id, body, *iterable, current_scope, nodes, edges);
                let body_scope =
                    self.walk_pat_for_bindings(body, *pat, current_scope, nodes, edges);
                self.walk_expr_for_refs(body_id, body, *b, body_scope, nodes, edges);
            }
            Expr::Match { .. } => {
                self.walk_match_for_refs(body_id, body, eid, current_scope, nodes, edges);
            }
            Expr::Cast { base, target } => {
                self.walk_expr_for_refs(body_id, body, *base, current_scope, nodes, edges);
                self.emit_type_references(target, current_scope, nodes, edges);
            }
            Expr::Array { elements } | Expr::Tuple { elements } => {
                for e in elements {
                    self.walk_expr_for_refs(body_id, body, *e, current_scope, nodes, edges);
                }
            }
            Expr::ArrayRepeat { value, len } => {
                self.walk_expr_for_refs(body_id, body, *value, current_scope, nodes, edges);
                self.walk_expr_for_refs(body_id, body, *len, current_scope, nodes, edges);
            }
        }
    }

    fn walk_lambda_for_refs(
        &mut self,
        body_id: BodyId,
        body: &Body,
        eid: ExprId,
        current_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        let Expr::Lambda {
            generic_bounds,
            params,
            ret_type,
            body: lambda_body,
            ..
        } = &body.exprs[eid]
        else {
            unreachable!("walk_lambda_for_refs requires a lambda expression");
        };
        let mut lambda_scope = self.sg.alloc_node(Node::Scope(ScopeKind::FunctionScope));
        nodes.push(lambda_scope);
        let edge = self
            .sg
            .add_edge(lambda_scope, current_scope, EdgeKind::Lex, 0);
        edges.push(edge);
        self.emit_bound_type_references(generic_bounds, lambda_scope, nodes, edges);
        for (index, param) in params.iter().enumerate() {
            self.emit_type_references(&param.ty, lambda_scope, nodes, edges);
            if let Some(pat) = param.pat {
                self.emit_pat_bindings(body, pat, &mut lambda_scope, nodes, edges);
                continue;
            }
            let pop = self.sg.alloc_node(Node::PopSymbol {
                name: param.name.clone(),
                define: DefRef::LambdaParam {
                    body_id,
                    lambda: eid,
                    index,
                },
            });
            nodes.push(pop);
            let edge = self.sg.add_edge(lambda_scope, pop, EdgeKind::Def, 0);
            edges.push(edge);
        }
        self.emit_type_references(ret_type, lambda_scope, nodes, edges);
        self.walk_expr_for_refs(body_id, body, *lambda_body, lambda_scope, nodes, edges);
    }

    fn walk_block_for_refs(
        &mut self,
        body_id: BodyId,
        body: &Body,
        eid: ExprId,
        current_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        let Expr::Block { stmts, tail } = &body.exprs[eid] else {
            unreachable!("walk_block_for_refs requires a block expression");
        };
        let inner = self.sg.alloc_node(Node::Scope(ScopeKind::Block));
        nodes.push(inner);
        let edge = self.sg.add_edge(inner, current_scope, EdgeKind::Lex, 0);
        edges.push(edge);
        self.index_body_module_items(body, stmts, inner);

        let mut block_current = inner;
        for sid in stmts {
            block_current = self.encode_body_stmt(body_id, body, *sid, block_current, nodes, edges);
        }
        if let Some(tail) = tail {
            self.walk_expr_for_refs(body_id, body, *tail, block_current, nodes, edges);
        }
    }

    fn walk_match_for_refs(
        &mut self,
        body_id: BodyId,
        body: &Body,
        eid: ExprId,
        current_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        let Expr::Match { scrutinee, arms } = &body.exprs[eid] else {
            unreachable!("walk_match_for_refs requires a match expression");
        };
        self.walk_expr_for_refs(body_id, body, *scrutinee, current_scope, nodes, edges);
        for arm in arms {
            let arm_scope = self.walk_pat_for_bindings(body, arm.pat, current_scope, nodes, edges);
            if let Some(guard) = arm.guard {
                self.walk_expr_for_refs(body_id, body, guard, arm_scope, nodes, edges);
            }
            self.walk_expr_for_refs(body_id, body, arm.body, arm_scope, nodes, edges);
        }
    }

    fn emit_path_reference(
        &mut self,
        body_id: BodyId,
        eid: ExprId,
        path: &HirPath,
        current_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        for (_, args) in &path.segment_type_args {
            for ty in args {
                self.emit_type_references(ty, current_scope, nodes, edges);
            }
        }
        for ty in &path.type_args {
            self.emit_type_references(ty, current_scope, nodes, edges);
        }
        self.emit_reference(
            path,
            RefOrigin::Expr {
                body: body_id,
                expr: eid,
            },
            current_scope,
            nodes,
            edges,
        );
    }

    fn encode_const_body(&mut self, cid: hir::item_tree::ConstId, parent_scope: NodeId) {
        if let Some(body_id) = self.hir.const_bodies.get(&cid).copied() {
            let body = &self.hir.bodies[body_id];
            self.encode_body_as_fragment(body_id, body, parent_scope);
        }
    }

    fn emit_type_references(
        &mut self,
        ty: &HirTypeRef,
        current_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        match ty {
            HirTypeRef::Named(path) => {
                for (_, args) in &path.segment_type_args {
                    for arg in args {
                        self.emit_type_references(arg, current_scope, nodes, edges);
                    }
                }
                for arg in &path.type_args {
                    self.emit_type_references(arg, current_scope, nodes, edges);
                }
                if !path.segments.is_empty() {
                    self.emit_reference(
                        path,
                        RefOrigin::Type { range: path.range },
                        current_scope,
                        nodes,
                        edges,
                    );
                }
            }
            HirTypeRef::Ref(inner, _)
            | HirTypeRef::Ptr { inner, .. }
            | HirTypeRef::Slice(inner)
            | HirTypeRef::Array(inner, _) => {
                self.emit_type_references(inner, current_scope, nodes, edges);
            }
            HirTypeRef::Tuple(elements) => {
                for element in elements {
                    self.emit_type_references(element, current_scope, nodes, edges);
                }
            }
            HirTypeRef::ImplTrait {
                trait_ty, callable, ..
            } => {
                self.emit_type_references(trait_ty, current_scope, nodes, edges);
                if let Some(signature) = callable {
                    for param in &signature.params {
                        self.emit_type_references(param, current_scope, nodes, edges);
                    }
                    self.emit_type_references(&signature.ret, current_scope, nodes, edges);
                }
            }
            HirTypeRef::DynTrait { trait_ty, .. } => {
                self.emit_type_references(trait_ty, current_scope, nodes, edges);
            }
            HirTypeRef::Never | HirTypeRef::Const(_) | HirTypeRef::Unknown | HirTypeRef::Error => {}
        }
    }

    fn emit_reference(
        &mut self,
        path: &HirPath,
        origin: RefOrigin,
        current_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        let anchor = self.anchor_scope_for(path.anchor, current_scope);
        let r = self.sg.alloc_node(Node::Reference {
            segments: path.segments.clone(),
            anchor,
            origin,
        });
        nodes.push(r);

        // Encode the path as a stack-graphs push chain rather than letting the resolver
        // pre-reverse the segments. A `PushSymbol` node appends its name to the end of the
        // stack when traversed, and the resolver treats the stack *top* (`stack.last()`) as
        // the next segment to match. For `foo::bar::baz`, we want to end up with `foo` on
        // top first, so the chain must push `baz`, then `bar`, then `foo`.
        //
        // Chain shape: Reference -> Push(seg[n-1]) -> ... -> Push(seg[0]) -> anchor
        let mut prev = r;
        for seg in path.segments.iter().rev() {
            let push = self.sg.alloc_node(Node::PushSymbol { name: seg.clone() });
            nodes.push(push);
            let e = self.sg.add_edge(prev, push, EdgeKind::Lex, 0);
            edges.push(e);
            prev = push;
        }

        let e = self.sg.add_edge(prev, anchor, EdgeKind::Lex, 0);
        edges.push(e);
    }

    fn index_body_module_items(&mut self, body: &Body, stmts: &[StmtId], scope: NodeId) {
        for sid in stmts {
            let Stmt::Item {
                item: BodyItem::Module(mid),
            } = &body.stmts[*sid]
            else {
                continue;
            };

            self.register_module_child(scope, *mid);

            let children = self.hir.item_tree.modules[*mid].items.clone();
            if let Some(children) = children {
                let internal = self.mod_scopes[mid].internal;
                self.index_module_children(&children, internal);
            }
        }
    }

    /// Walks a match arm pattern and emits scope-graph nodes for its bindings.
    ///
    /// Returns the inner scope that the arm body should use, nesting `let`-like scopes for
    /// each introduced binding.
    fn walk_pat_for_bindings(
        &mut self,
        body: &Body,
        pat: PatId,
        parent_scope: NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) -> NodeId {
        let mut scope = parent_scope;
        self.emit_pat_bindings(body, pat, &mut scope, nodes, edges);
        scope
    }

    /// Recursively emits `PopSymbol` defs for every binding introduced by a pattern.
    ///
    /// Each binding creates a new scope that shadows the previous one (same shape as `let`).
    fn emit_pat_bindings(
        &mut self,
        body: &Body,
        pat: PatId,
        current: &mut NodeId,
        nodes: &mut Vec<NodeId>,
        edges: &mut Vec<EdgeId>,
    ) {
        match &body.pats[pat] {
            Pattern::Binding { name, .. } => {
                let next = self.sg.alloc_node(Node::Scope(ScopeKind::Block));
                nodes.push(next);
                let e = self.sg.add_edge(next, *current, EdgeKind::Lex, 0);
                edges.push(e);

                let pop = self.sg.alloc_node(Node::PopSymbol {
                    name: name.clone(),
                    define: DefRef::PatternBinding {
                        name: name.clone(),
                        id: PatternBindingId {
                            pattern: pat,
                            field: None,
                        },
                    },
                });
                nodes.push(pop);
                let e = self.sg.add_edge(next, pop, EdgeKind::Def, 0);
                edges.push(e);

                *current = next;
            }
            Pattern::Reference { pattern, .. } => {
                self.emit_pat_bindings(body, *pattern, current, nodes, edges);
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Path { .. } | Pattern::Or { .. } => {
            }
            Pattern::Tuple { elements } | Pattern::TupleStruct { elements, .. } => {
                for e in elements {
                    self.emit_pat_bindings(body, *e, current, nodes, edges);
                }
            }
            Pattern::Struct { fields, .. } => {
                for (index, fp) in fields.iter().enumerate() {
                    if let Some(sub) = fp.pat {
                        self.emit_pat_bindings(body, sub, current, nodes, edges);
                    } else {
                        let next = self.sg.alloc_node(Node::Scope(ScopeKind::Block));
                        nodes.push(next);
                        let edge = self.sg.add_edge(next, *current, EdgeKind::Lex, 0);
                        edges.push(edge);
                        let pop = self.sg.alloc_node(Node::PopSymbol {
                            name: fp.name.clone(),
                            define: DefRef::PatternBinding {
                                name: fp.name.clone(),
                                id: PatternBindingId {
                                    pattern: pat,
                                    field: Some(index),
                                },
                            },
                        });
                        nodes.push(pop);
                        let edge = self.sg.add_edge(next, pop, EdgeKind::Def, 0);
                        edges.push(edge);
                        *current = next;
                    }
                }
            }
        }
    }
}

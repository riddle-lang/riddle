use body_lower::BodyLower;
use std::collections::HashMap;

use body::{Body, BodyId, ResolvedName};
use item_tree::{
    ConstId, FunctionId, HirInternalAttr, HirModule, HirUse, ItemTree, ModuleId, TopLevelItem,
};
use la_arena::Arena;
use lower::{AstLower, Lower};
use rowan::TextRange;
use syntax::SyntaxNode;

use ast::{
    self, Root,
    support::{AstNode, trimmed_range},
};

pub mod body;
pub mod body_lower;
pub mod item_tree;
pub mod lower;
pub mod place;
pub mod render;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(pub String);

#[derive(Debug)]
pub struct HirFile {
    pub item_tree: ItemTree,
    pub bodies: Arena<Body>,
    pub function_bodies: HashMap<FunctionId, BodyId>,
    pub const_bodies: HashMap<ConstId, BodyId>,
    pub type_resolutions: HashMap<TextRange, ResolvedName>,
    pub internal_attrs: Vec<HirInternalAttr>,
    /// Disjoint source ranges owned by packages participating in this compilation.
    pub package_ranges: Vec<TextRange>,
    /// True when the standard library prelude was appended to the source.
    /// Items outside `package_ranges` belong to the standard library.
    pub std_loaded: bool,
    /// Documentation comments attached to syntax nodes, kept in source order.
    pub doc_comments: Vec<(TextRange, Vec<String>)>,
}

#[must_use]
pub fn lower_root(root: &Root) -> HirFile {
    let package_range = root.syntax().text_range();
    let internal_attrs = lower::lower_internal_attrs(root.syntax());
    let mut doc_comments = Vec::new();
    collect_doc_comments(root.syntax(), &mut doc_comments);
    let mut hir = HirFile {
        item_tree: ItemTree {
            functions: Arena::new(),
            structs: Arena::new(),
            modules: Arena::new(),
            uses: Arena::new(),
            enums: Arena::new(),
            traits: Arena::new(),
            impls: Arena::new(),
            consts: Arena::new(),
            type_aliases: Arena::new(),
            top_level: vec![],
            extern_function_ids: vec![],
        },
        bodies: Arena::new(),
        function_bodies: HashMap::new(),
        const_bodies: HashMap::new(),
        type_resolutions: HashMap::new(),
        internal_attrs,
        package_ranges: vec![package_range],
        std_loaded: false,
        doc_comments,
    };

    let top = lower_items(&mut hir, root.stmts().collect());
    hir.item_tree.top_level = top;
    hir
}

fn collect_doc_comments(node: &SyntaxNode, comments: &mut Vec<(TextRange, Vec<String>)>) {
    let docs = ast::doc_comments_for_node(node);
    if !docs.is_empty() {
        comments.push((
            node.text_range(),
            docs.into_iter()
                .map(|token| token.text().to_string())
                .collect(),
        ));
    }
    for child in node.children() {
        collect_doc_comments(&child, comments);
    }
}

impl HirFile {
    #[must_use]
    pub fn package_for_range(&self, range: TextRange) -> Option<usize> {
        self.package_ranges
            .iter()
            .position(|package| package.start() <= range.start() && range.end() <= package.end())
    }
}

pub(crate) fn lower_items(hir: &mut HirFile, stmts: Vec<ast::Stmt>) -> Vec<TopLevelItem> {
    let mut items = Vec::new();
    for stmt in stmts {
        match stmt {
            ast::Stmt::FuncDecl(func) => {
                let body_ast = func.body();
                let fid = func.lower(&mut hir.item_tree.functions);
                items.push(TopLevelItem::Function(fid));
                if let Some(block) = body_ast {
                    let body = BodyLower::lower(hir, &block);
                    let bid = hir.bodies.alloc(body);
                    hir.function_bodies.insert(fid, bid);
                }
            }

            ast::Stmt::StructDecl(s) => {
                let sid = s.lower(&mut hir.item_tree.structs);
                items.push(TopLevelItem::Struct(sid));
            }

            ast::Stmt::ModDecl(m) => {
                let mid = lower_mod_decl(hir, &m);
                items.push(TopLevelItem::Module(mid));
            }

            ast::Stmt::UseDecl(u) => {
                let Some(tree_ast) = u.use_tree() else {
                    continue;
                };
                let tree = tree_ast.lower();
                let attrs = lower::lower_attrs(u.syntax());
                let visibility = lower::lower_visibility(u.is_pub());
                let uid = hir.item_tree.uses.alloc(HirUse {
                    tree,
                    visibility,
                    attrs,
                });
                items.push(TopLevelItem::Use(uid));
            }

            ast::Stmt::ExternBlock(block) => {
                for func in block.functions() {
                    let is_safe = func.is_safe();
                    let fid = func.lower(&mut hir.item_tree.functions);
                    hir.item_tree.functions[fid].is_unsafe = !is_safe;
                    items.push(TopLevelItem::Function(fid));
                    hir.item_tree.extern_function_ids.push(fid);
                }
            }

            ast::Stmt::ExternFnDecl(decl) => {
                let explicitly_unsafe = decl.is_unsafe();
                if let Some(func) = decl.func_decl()
                    && let Some(body_ast) = func.body()
                {
                    let fid = func.lower(&mut hir.item_tree.functions);
                    hir.item_tree.functions[fid].is_unsafe |= explicitly_unsafe;
                    items.push(TopLevelItem::Function(fid));
                    hir.item_tree.extern_function_ids.push(fid);
                    let body = BodyLower::lower(hir, &body_ast);
                    let bid = hir.bodies.alloc(body);
                    hir.function_bodies.insert(fid, bid);
                }
            }

            ast::Stmt::EnumDecl(e) => {
                let eid = e.lower(&mut hir.item_tree.enums);
                items.push(TopLevelItem::Enum(eid));
            }

            ast::Stmt::TraitDecl(t) => {
                let tid = lower_trait_decl(hir, t);
                items.push(TopLevelItem::Trait(tid));
            }

            ast::Stmt::ConstDecl(c) => {
                let value = c.value();
                let cid = c.lower(&mut hir.item_tree.consts);
                if let Some(value) = value {
                    let body = BodyLower::lower_const(hir, value);
                    let body_id = hir.bodies.alloc(body);
                    hir.const_bodies.insert(cid, body_id);
                }
                items.push(TopLevelItem::Const(cid));
            }

            ast::Stmt::TypeAliasDecl(t) => {
                let tid = t.lower(&mut hir.item_tree.type_aliases);
                items.push(TopLevelItem::TypeAlias(tid));
            }

            ast::Stmt::ImplDecl(i) => {
                let iid = lower_impl_decl(hir, &i);
                items.push(TopLevelItem::Impl(iid));
            }

            ast::Stmt::VarDecl(_)
            | ast::Stmt::BreakStmt(_)
            | ast::Stmt::ContinueStmt(_)
            | ast::Stmt::ReturnStmt(_)
            | ast::Stmt::ExprStmt(_) => {}
        }
    }
    items
}

pub(crate) fn lower_trait_decl(hir: &mut HirFile, t: ast::TraitDecl) -> item_tree::TraitId {
    use item_tree::{HirGenericBound, HirPath, HirTypeRef, PathAnchor};

    let default_methods = t
        .methods()
        .filter_map(|method| method.body().map(|body| (method, body)))
        .collect::<Vec<_>>();
    let tid = t.lower(&mut hir.item_tree.traits);
    let trait_name = hir.item_tree.traits[tid].name.clone();
    let trait_name_range = hir.item_tree.traits[tid].name_range;
    let trait_generics = hir.item_tree.traits[tid].generics.clone();

    for (method, body_ast) in default_methods {
        let receivers = method
            .param_list()
            .map(|params| {
                params
                    .params()
                    .map(|param| (param.is_self_receiver(), param.is_ref(), param.is_mut()))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let fid = method.lower(&mut hir.item_tree.functions);
        let range = hir.item_tree.functions[fid].name_range;
        let self_ty = HirTypeRef::Named(HirPath {
            anchor: PathAnchor::Plain,
            segments: vec![Name("Self".into())],
            segment_type_args: Vec::new(),
            type_args: Vec::new(),
            range,
        });
        apply_self_receiver_types(&mut hir.item_tree.functions[fid], &receivers);
        hir.item_tree.functions[fid]
            .generic_bounds
            .push(HirGenericBound {
                param: Name("Self".into()),
                target_ty: self_ty,
                target_range: range,
                trait_ty: HirTypeRef::Named(HirPath {
                    anchor: PathAnchor::Plain,
                    segments: vec![trait_name.clone()],
                    segment_type_args: Vec::new(),
                    type_args: trait_generics
                        .iter()
                        .map(|name| {
                            HirTypeRef::Named(HirPath {
                                anchor: PathAnchor::Plain,
                                segments: vec![name.clone()],
                                segment_type_args: Vec::new(),
                                type_args: Vec::new(),
                                range,
                            })
                        })
                        .collect(),
                    range: trait_name_range,
                }),
                trait_range: range,
                callable: None,
                assoc_constraints: Vec::new(),
            });
        let body = BodyLower::lower(hir, &body_ast);
        let body_id = hir.bodies.alloc(body);
        hir.function_bodies.insert(fid, body_id);
        hir.item_tree.traits[tid].default_methods.push(fid);
    }

    tid
}

/// Lowers `mod foo { ... }` or `mod foo;` into the item tree.
///
/// Both `body_lower` and `lower_items` use this helper so module children are always promoted into
/// the global item tree.
pub(crate) fn lower_mod_decl(hir: &mut HirFile, m: &ast::ModDecl) -> ModuleId {
    let name_token = m.name();
    let name_range = name_token
        .as_ref()
        .map_or_else(|| trimmed_range(m.syntax()), rowan::SyntaxToken::text_range);
    let name = lower::lower_name(name_token);

    // Allocate a placeholder first so the module has a stable id while lowering children.
    let attrs = lower::lower_attrs(m.syntax());
    let visibility = lower::lower_visibility(m.is_pub());
    let mid = hir.item_tree.modules.alloc(HirModule {
        name,
        name_range,
        extent: trimmed_range(m.syntax()),
        visibility,
        items: None,
        attrs,
    });

    if let Some(items_iter) = m.items() {
        let stmts: Vec<ast::Stmt> = items_iter.collect();
        let children = lower_items(hir, stmts);
        hir.item_tree.modules[mid].items = Some(children);
    }
    mid
}

/// Lowers an `impl` block. Methods/consts/type-aliases are allocated into the global arenas; method
/// bodies are lowered like free functions so their references participate in name resolution.
pub(crate) fn lower_impl_decl(hir: &mut HirFile, i: &ast::ImplDecl) -> item_tree::ImplId {
    use item_tree::{HirImpl, HirTypeRef};

    let impl_range = trimmed_range(i.syntax());
    let first_ty_ast = i.self_type();
    let first_ty_range = first_ty_ast.as_ref().map(|ty| trimmed_range(ty.syntax()));
    let first_ty = first_ty_ast.map(lower::Lower::lower);
    let second_ty_ast = i.trait_type();
    let second_ty_range = second_ty_ast.as_ref().map(|ty| trimmed_range(ty.syntax()));
    let second_ty = second_ty_ast.map(lower::Lower::lower);
    let (self_ty, self_ty_range, trait_ty, trait_ty_range) = if i.has_for() {
        (
            second_ty.unwrap_or(HirTypeRef::Error),
            second_ty_range.unwrap_or(impl_range),
            first_ty,
            first_ty_range,
        )
    } else {
        (
            first_ty.unwrap_or(HirTypeRef::Error),
            first_ty_range.unwrap_or(impl_range),
            None,
            None,
        )
    };
    let callable = i
        .has_for()
        .then(|| i.callable_trait_args())
        .flatten()
        .as_ref()
        .map(lower::lower_callable_signature);
    let generic_params = i.generic_params();
    let generics = lower::lower_generic_params(generic_params.clone());
    let const_generics = lower::lower_const_generic_params(generic_params.clone());
    let generic_bounds = lower::lower_generic_bounds(generic_params, i.where_clause());

    let mut methods = Vec::new();
    for func in i.methods() {
        let body_ast = func.body();
        let receivers: Vec<_> = func
            .param_list()
            .map(|pl| {
                pl.params()
                    .map(|p| (p.is_self_receiver(), p.is_ref(), p.is_mut()))
                    .collect()
            })
            .unwrap_or_default();
        let fid = func.lower(&mut hir.item_tree.functions);
        apply_self_receiver_types(&mut hir.item_tree.functions[fid], &receivers);
        methods.push(fid);
        if let Some(block) = body_ast {
            let body = BodyLower::lower(hir, &block);
            let bid = hir.bodies.alloc(body);
            hir.function_bodies.insert(fid, bid);
        }
    }

    let mut consts = Vec::new();
    for c in i.consts() {
        let value = c.value();
        let cid = c.lower(&mut hir.item_tree.consts);
        if let Some(value) = value {
            let body = BodyLower::lower_const(hir, value);
            let body_id = hir.bodies.alloc(body);
            hir.const_bodies.insert(cid, body_id);
        }
        consts.push(cid);
    }
    let type_aliases = i
        .type_aliases()
        .map(|t| t.lower(&mut hir.item_tree.type_aliases))
        .collect();

    hir.item_tree.impls.alloc(HirImpl {
        extent: trimmed_range(i.syntax()),
        self_ty,
        self_ty_range,
        trait_ty,
        trait_ty_range,
        callable,
        generics,
        const_generics,
        generic_bounds,
        methods,
        consts,
        type_aliases,
        attrs: lower::lower_attrs(i.syntax()),
    })
}

fn apply_self_receiver_types(func: &mut item_tree::HirFunction, receivers: &[(bool, bool, bool)]) {
    for (param, (is_self, is_ref, is_mut)) in func.params.iter_mut().zip(receivers) {
        param.is_receiver = *is_self;
        if !*is_self {
            continue;
        }

        let self_ty = item_tree::HirTypeRef::Named(item_tree::HirPath {
            anchor: item_tree::PathAnchor::Plain,
            segments: vec![Name("Self".into())],
            segment_type_args: Vec::new(),
            type_args: Vec::new(),
            range: param.ty_range,
        });
        param.ty = if *is_ref {
            item_tree::HirTypeRef::Ref(Box::new(self_ty), *is_mut)
        } else {
            self_ty
        };
    }
}

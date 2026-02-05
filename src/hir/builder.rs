use crate::hir::expr::HirExpr;
use crate::hir::id::{DefId, ExprId, LocalId, StmtId, TyExprId, TyId};
use crate::hir::items::{
    HirEnum, HirEnumVariant, HirExternFunc, HirFunc, HirFuncParam, HirGlobalVariable, HirImpl,
    HirItem, HirStruct, HirStructField, HirTrait, HirTraitItem,
};
use crate::hir::module::HirModule;
use crate::hir::stmt::{HirStmt, HirStmtKind};
use crate::hir::type_expr::HirTypeExpr;
use crate::hir::types::HirType;

#[derive(Debug)]
pub struct HirBuilder<'a> {
    pub module: &'a mut HirModule,
    active_func: Option<DefId>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum HirBuilderError {
    CurrentFunctionNotSet,
}

impl Into<String> for HirBuilderError {
    fn into(self) -> String {
        format!("{:?}", self)
    }
}

impl<'a> HirBuilder<'a> {
    pub fn new(module: &'a mut HirModule) -> Self {
        Self {
            module,
            active_func: None,
        }
    }

    pub fn set_func(&mut self, id: DefId) {
        self.active_func = Some(id);
    }

    pub fn create_type(&mut self, ty: HirType) -> TyId {
        let id = TyId(self.module.types.len());
        self.module.types.push(ty);
        id
    }

    pub fn create_type_expr(&mut self, ty_expr: HirTypeExpr) -> TyExprId {
        let id = TyExprId(self.module.type_exprs.len());
        self.module.type_exprs.push(ty_expr);
        id
    }

    pub fn next_local_id(&mut self) -> LocalId {
        let id = LocalId(self.module.next_local_id);
        self.module.next_local_id += 1;
        id
    }

    pub fn create_expr(&mut self, expr: HirExpr) -> ExprId {
        let id = ExprId(self.module.exprs.len());
        self.module.exprs.push(expr);
        id
    }

    pub fn create_stmt(&mut self, stmt: HirStmt) -> StmtId {
        let id = StmtId(self.module.stmts.len());
        self.module.stmts.push(stmt);
        id
    }

    pub fn create_func(
        &mut self,
        name: &str,
        generic_params: Vec<String>,
        ret: TyExprId,
        param: Vec<HirFuncParam>,
    ) -> Result<DefId, HirBuilderError> {
        let id = DefId(self.module.items.len());
        let func = HirFunc {
            name: name.to_string(),
            generic_params,
            param,
            ret,
            id,
            body: Vec::new(),
        };
        self.module.items.push(HirItem::Func(func));
        Ok(id)
    }

    pub fn create_extern_func(
        &mut self,
        abi: Option<String>,
        name: &str,
        ret: TyExprId,
        param: Vec<HirFuncParam>,
    ) -> Result<DefId, HirBuilderError> {
        let id = DefId(self.module.items.len());
        let func = HirExternFunc {
            abi,
            name: name.to_string(),
            param,
            ret,
            id,
        };
        self.module.items.push(HirItem::ExternFunc(func));
        Ok(id)
    }

    pub fn create_global_var(
        &mut self,
        name: &str,
        ty: Option<TyExprId>,
        value: ExprId,
    ) -> Result<DefId, HirBuilderError> {
        let id = DefId(self.module.items.len());
        let gv = HirGlobalVariable {
            name: name.to_string(),
            ty,
            value,
        };
        self.module.items.push(HirItem::GlobalVariable(gv));
        Ok(id)
    }

    pub fn create_enum(
        &mut self,
        name: &str,
        generic_params: Vec<String>,
        variants: Vec<HirEnumVariant>,
    ) -> Result<DefId, HirBuilderError> {
        let id = DefId(self.module.items.len());
        let e = HirEnum {
            name: name.to_string(),
            generic_params,
            id,
            variants,
        };
        self.module.items.push(HirItem::Enum(e));
        Ok(id)
    }

    pub fn create_struct(
        &mut self,
        name: &str,
        generic_params: Vec<String>,
        fields: Vec<HirStructField>,
    ) -> Result<DefId, HirBuilderError> {
        let id = DefId(self.module.items.len());
        let s = HirStruct {
            name: name.to_string(),
            generic_params,
            fields,
            id,
        };
        self.module.items.push(HirItem::Struct(s));
        Ok(id)
    }

    pub fn create_trait(
        &mut self,
        name: &str,
        generic_params: Vec<String>,
        items: Vec<HirTraitItem>,
    ) -> Result<DefId, HirBuilderError> {
        let id = DefId(self.module.items.len());
        let t = HirTrait {
            name: name.to_string(),
            generic_params,
            id,
            items,
        };
        self.module.items.push(HirItem::Trait(t));
        Ok(id)
    }

    pub fn create_impl(
        &mut self,
        generic_params: Vec<String>,
        trait_type: Option<TyExprId>,
        target_type: TyExprId,
        methods: Vec<DefId>,
    ) -> Result<DefId, HirBuilderError> {
        let id = DefId(self.module.items.len());
        let im = HirImpl {
            generic_params,
            trait_type,
            target_type,
            items: methods,
            id,
        };
        self.module.items.push(HirItem::Impl(im));
        Ok(id)
    }

    pub fn ret(&mut self, expr: ExprId) -> Result<(), HirBuilderError> {
        let func_id = self
            .active_func
            .ok_or(HirBuilderError::CurrentFunctionNotSet)?;
        let stmt_id = self.create_stmt(HirStmt::new(HirStmtKind::Return { value: Some(expr) }));
        if let HirItem::Func(func) = &mut self.module.items[func_id.0] {
            func.body.push(stmt_id);
        }
        Ok(())
    }

    pub fn local(
        &mut self,
        name: &str,
        ty_annot: Option<TyExprId>,
        init: Option<ExprId>,
    ) -> Result<(), HirBuilderError> {
        let func_id = self
            .active_func
            .ok_or(HirBuilderError::CurrentFunctionNotSet)?;
        let id = self.next_local_id();
        let stmt_id = self.create_stmt(HirStmt::new(HirStmtKind::Let {
            name: name.into(),
            ty_annot,
            init,
            id,
        }));
        if let HirItem::Func(func) = &mut self.module.items[func_id.0] {
            func.body.push(stmt_id);
        }
        Ok(())
    }

    pub fn expr_stmt(&mut self, expr: ExprId, has_semi: bool) -> Result<(), HirBuilderError> {
        let func_id = self
            .active_func
            .ok_or(HirBuilderError::CurrentFunctionNotSet)?;
        let stmt_id = self.create_stmt(HirStmt::new(HirStmtKind::Expr { expr, has_semi }));
        if let HirItem::Func(func) = &mut self.module.items[func_id.0] {
            func.body.push(stmt_id);
        }
        Ok(())
    }
}

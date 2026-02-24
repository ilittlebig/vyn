/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-25
 **/

use crate::passes::PassContext;
use crate::passes::hir::{ HirExprKind, HirExpr, HirStmt };

pub enum Type {
    String,
    Int,
    Bool,
    Nil,
    Error,
}

pub fn type_expr(ctx: &PassContext, expr: &HirExpr) -> Type {
    match expr.kind {
        HirExprKind::String(_) => Type::String,
        HirExprKind::Int(_) => Type::Int,
        HirExprKind::Bool(_) => Type::Bool,
        HirExprKind::Nil => Type::Nil,
        _ => Type::Error,
    }
}

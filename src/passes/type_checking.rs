/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-24
 **/

use crate::passes::types;
use crate::passes::types::Type;

use crate::passes::PassContext;
use crate::diagnostics::Diagnostic;
use crate::passes::hir::{ HirStmt, HirExpr, HirExprKind };

fn check_stmt(ctx: &mut PassContext, stmt: &HirStmt) {
    match stmt {
        HirStmt::Decl { def_id, init } => {
            let name_span = ctx.defs[def_id.0].span;
            let ann = ctx.def_ann[def_id.0].clone();

            let annotated = ann.as_ref().map(|t| {
                let (ty, ty_span) = types::lower_type_ref(ctx, &t);
                if matches!(ty, Type::Error) {
                    ctx.diags.push(Diagnostic::error("unknown type name", ty_span));
                }
                ty
            });

            let inferred = init.as_ref().map(|e| types::type_expr(ctx, e));
            let def_type = annotated.clone().or(inferred.clone()).unwrap_or(Type::Any);
            ctx.def_types[def_id.0] = def_type;

            if let (Some(ann), Some(inf)) = (annotated, inferred) {
                // we will get cascading errors here unless we check if either side
                // was of type error
                if !types::assignable(&ann, &inf) && ann != Type::Error && inf != Type::Error {
                    let span = init.as_ref().map(|e| e.span).unwrap_or(name_span);
                    let msg = format!(
                        "cannot assign '{}' to '{}'",
                        types::fmt_type(ctx, &inf),
                        types::fmt_type(ctx, &ann)
                    );
                    ctx.diags.push(Diagnostic::error(msg, span));
                }
            }

            // local x = function(...) { ... }
            if let Some(HirExprKind::Func(func)) = init.as_ref().map(|e| &e.kind) {
                for block_stmt in &func.body.stmts {
                    check_stmt(ctx, &block_stmt);
                }
            }
        },

        HirStmt::FuncDecl { def_id, params, init } => {
            ctx.def_types[def_id.0] = Type::Func {
                params: Vec::new(),
                ret: Box::new(Type::Any)
            };

            // type-check the entire block with the ret type
            for block_stmt in &init.stmts {
                check_stmt(ctx, &block_stmt);
            }
        },

        HirStmt::Block(block) => {
            for block_stmt in &block.stmts {
                check_stmt(ctx, &block_stmt);
            }
        },

        HirStmt::If { cond, .. } => {
            let cond_ty = types::type_expr(ctx, cond);
            if cond_ty != Type::Named(ctx.builtin_types.bool) && cond_ty != Type::Error {
                let msg = format!(
                    "expected '{}', got '{}'",
                    types::fmt_type(ctx, &Type::Named(ctx.builtin_types.bool)),
                    types::fmt_type(ctx, &cond_ty)
                );
                ctx.diags.push(Diagnostic::error(msg, cond.span));
            }
        },

        HirStmt::Expr(expr) => {
            let expr_ty = types::type_expr(ctx, expr);
        },

        _ => {},
    }
}

pub fn run(ctx: &mut PassContext, hir: &[HirStmt]) {
    for stmt in hir {
        check_stmt(ctx, stmt);
    }
}

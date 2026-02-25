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
            let def_type = annotated.or(inferred).unwrap_or(Type::Any);
            ctx.def_types[def_id.0] = def_type;

            if let (Some(ann), Some(inf)) = (annotated, inferred) {
                // we will get cascading errors here unless we check if either side
                // was of type error
                if !types::assignable(ann, inf) && inf != Type::Error {
                    let span = init.as_ref().map(|e| e.span).unwrap_or(name_span);
                    let msg = format!(
                        "cannot assign ´{}´ to ´{}´",
                        types::fmt_type(ctx, &inf),
                        types::fmt_type(ctx, &ann)
                    );
                    ctx.diags.push(Diagnostic::error(msg, span));
                }
            }
        },
        _ => {},
    }
}

pub fn run(ctx: &mut PassContext, hir: &[HirStmt]) {
    for stmt in hir {
        check_stmt(ctx, stmt);
    }
}

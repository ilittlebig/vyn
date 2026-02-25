/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-25
 **/

use crate::diagnostics::Diagnostic;
use crate::frontend::parser::TypeRef;
use crate::passes::{ PassContext, Symbol };
use crate::passes::hir::{ HirExprKind, HirExpr, HirStmt };

#[derive(Clone, Copy, PartialEq)]
pub struct TypeId(pub usize);

pub struct BuiltinTypes {
    pub string: TypeId,
    pub int: TypeId,
    pub bool: TypeId,
    pub nil: TypeId,
    pub any: TypeId,
}

impl BuiltinTypes {
    pub fn dummy() -> Self {
        let z = TypeId(0);
        BuiltinTypes { string: z, int: z, bool: z, nil: z, any: z }
    }
}

pub enum TypeDefKind {
    Builtin,
    // Struct { ... },
    // Enum { ... },
}

pub struct TypeDef {
    pub name: Symbol,
    pub kind: TypeDefKind,
}

#[derive(Clone, Copy, PartialEq)]
pub enum Type {
    Named(TypeId),
    Any,
    Error,
}

pub fn assignable(t1: Type, t2: Type) -> bool {
    t1 == t2
}

pub fn type_expr(ctx: &PassContext, expr: &HirExpr) -> Type {
    match expr.kind {
        HirExprKind::String(_) => Type::Named(ctx.builtin_types.string),
        HirExprKind::Int(_) => Type::Named(ctx.builtin_types.int),
        HirExprKind::Bool(_) => Type::Named(ctx.builtin_types.bool),
        HirExprKind::Nil => Type::Named(ctx.builtin_types.nil),
        _ => Type::Error,
    }
}

// converts an identifier type to a type enum member, so we get
// int -> Type::Int
pub fn lower_type_ref(ctx: &mut PassContext, type_ref: &TypeRef) -> Type {
    match type_ref {
        TypeRef::Named(name, span) => {
            let symbol = ctx.interner.intern(name);
            let type_id = ctx.type_bindings.get(&symbol);

            match type_id {
                Some(id) => Type::Named(*id),
                _ => {
                    ctx.diags.push(Diagnostic::error("unknown type name", *span));
                    Type::Error
                }
            }
        },
    }
}

pub fn fmt_type(ctx: &mut PassContext, ty: &Type) -> String {
    match ty {
        Type::Any => "any".to_string(),
        Type::Error => "<error>".to_string(),
        Type::Named(id) => {
            let type_def = &ctx.type_defs[id.0];
            // unknown should in theory never happen if the interner is correct
            ctx.interner.resolve(type_def.name).unwrap_or("<unknown>").to_string()
        },
    }
}

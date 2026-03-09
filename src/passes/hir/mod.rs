/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-09
 **/

use crate::passes::PassContext;
use crate::frontend::parser::Stmt;

mod types;
mod lower_hir;

pub use types::*;

pub fn lower(ctx: &mut PassContext, ast: &Vec<Stmt>) -> Vec<types::HirStmt> {
    lower_hir::run(ctx, ast)
}

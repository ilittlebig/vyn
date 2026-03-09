/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-09
 **/

use crate::passes::PassContext;
use crate::passes::hir::HirStmt;

mod types;
mod check;

pub use types::*;

pub fn check(ctx: &mut PassContext, hir: &[HirStmt]) {
    check::run(ctx, hir);
}

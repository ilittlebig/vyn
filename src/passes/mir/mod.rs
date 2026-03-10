/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-03
 **/

use crate::passes::PassContext;
use crate::passes::hir::HirStmt;

mod mir;
mod dump;
mod lower_mir;

pub use mir::*;
pub use dump::MirDumper;

pub fn lower(ctx: &mut PassContext, hir: &[HirStmt]) -> MirProgram {
    lower_mir::run(ctx, hir)
}

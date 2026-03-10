/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-06
 **/

use std::collections::HashMap;

use crate::passes::mir::MirDumper;
use crate::tools::cli::CompileOptions;
use crate::tools::fmt::Printer;
use crate::passes::interner::Interner;
use crate::frontend::parser::{ TypeRef, Stmt };
use crate::passes::type_checking::{ Type, TypeDef, TypeDefKind, TypeId, BuiltinTypes };
use crate::diagnostics::{ Span, Diagnostic, Severity };

mod hir;
mod type_checking;
mod mir;
mod codegen;

mod interner;

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub struct Symbol(usize);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DefId(usize);

#[derive(Debug, Copy, Clone, PartialEq)]
struct ScopeId(usize);

#[derive(Debug)]
pub enum DefKind {
    LocalVar,
    Function,
    Param,
}

#[derive(Debug)]
pub struct Def {
    name: Symbol,
    span: Span,
    kind: DefKind,
}

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<ScopeId>,
    bindings: HashMap<Symbol, DefId>,
}

#[derive(Debug)]
pub struct PassContext {
    pub interner: Interner,
    pub defs: Vec<Def>,
    pub scopes: Vec<Scope>,
    pub diags: Vec<Diagnostic>,
    pub current_scope: ScopeId,

    //
    pub def_types: Vec<Type>,
    pub def_ann: Vec<Option<TypeRef>>,

    pub type_defs: Vec<TypeDef>,
    pub type_bindings: HashMap<Symbol, TypeId>,
    pub builtin_types: BuiltinTypes,
}

impl PassContext {
    fn new() -> Self {
        let mut ctx = PassContext {
            interner: Interner::new(),
            defs: Vec::new(),
            scopes: Vec::new(),
            diags: Vec::new(),
            current_scope: ScopeId(0),

            // types
            def_types: Vec::new(),
            def_ann: Vec::new(),

            type_defs: Vec::new(),
            type_bindings: HashMap::new(),
            builtin_types: BuiltinTypes::dummy(),
        };

        //
        let string = ctx.push_builtin_type("string");
        let int = ctx.push_builtin_type("int");
        let double = ctx.push_builtin_type("double");
        let bool = ctx.push_builtin_type("bool");
        let nil = ctx.push_builtin_type("nil");
        ctx.builtin_types = BuiltinTypes { string, int, double, bool, nil };

        // global scope
        ctx.scopes.push(Scope { parent: None, bindings: HashMap::new() });

        ctx
    }

    fn scope(&self, id: ScopeId) -> &Scope { &self.scopes[id.0] }
    fn scope_mut(&mut self, id: ScopeId) -> &mut Scope { &mut self.scopes[id.0] }

    fn push_scope(&mut self) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        let parent = Some(self.current_scope);
        self.scopes.push(Scope { parent, bindings: HashMap::new() });
        self.current_scope = id;
        id
    }

    fn pop_scope(&mut self) {
        let scope = self.scope(self.current_scope)
            .parent
            .expect("pop root scope");
        self.current_scope = scope;
    }

    fn has_errors(&self) -> bool {
        self.diags.iter().any(|d| matches!(d.severity, Severity::Error))
    }

    //
    fn push_builtin_type(&mut self, name: &'static str) -> TypeId {
        let symbol = self.interner.intern(name);
        let type_id = TypeId(self.type_defs.len());
        self.type_defs.push(TypeDef { name: symbol, kind: TypeDefKind::Builtin });
        self.type_bindings.insert(symbol, type_id);
        type_id
    }
}

pub fn run_passes(ast: &Vec<Stmt>, opts: &CompileOptions) -> Vec<Diagnostic> {
    let mut ctx = PassContext::new();

    let hir = hir::lower(&mut ctx, ast);
    if ctx.has_errors() { return ctx.diags; }

    type_checking::check(&mut ctx, &hir);
    if ctx.has_errors() { return ctx.diags; }

    let mir = mir::lower(&mut ctx, &hir);
    if opts.dump_mir {
        let mut printer = Printer::new(String::new());
        let mut mir_dumper = MirDumper::new(
            &mut printer,
            &mut ctx,
            &mir,
        );
        mir_dumper.dump_program();

        let out = printer.finish();
        println!("{}", out);
    }

    codegen::emit_program(mir);

    ctx.diags
}

use core::fmt;
use std::hash;

use crate::dialect::{SystemRDialect, SystemRExtension};
use crate::patterns::{Pattern, PatternCount};
use crate::system_r_util::span::Span;
use crate::terms::{Arm, Kind, Term};
use crate::types::Type;
use crate::visit::{MutTermVisitor, MutTypeVisitor};

pub struct Shift {
    cutoff: usize,
    shift: isize,
}

impl Shift {
    pub const fn new(shift: isize) -> Shift {
        Shift { cutoff: 0, shift }
    }
}

impl<
        TExtDialect: hash::Hash + Eq + Clone + fmt::Debug + Default + SystemRDialect + PartialEq + PartialOrd,
        TExt: SystemRExtension<TExtDialect> + fmt::Debug + Default + Clone,
    > MutTermVisitor<TExtDialect, TExt> for Shift
{
    fn visit_var(&mut self, sp: &mut Span, var: &mut usize, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {
        if *var >= self.cutoff {
            *var = (*var as isize + self.shift) as usize;
        }
    }

    fn visit_abs(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.cutoff += 1;
        self.visit(term, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_let(
        &mut self,
        sp: &mut Span,
        pat: &mut Pattern<TExtDialect>,
        t1: &mut Term<TExtDialect>,
        t2: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit(t1, ext, ext_state);
        let c = PatternCount::<TExtDialect>::collect(pat, ext, ext_state);
        self.cutoff += c;
        self.visit(t2, ext, ext_state);
        self.cutoff -= c;
    }

    fn visit_case(
        &mut self,
        sp: &mut Span,
        term: &mut Term<TExtDialect>,
        arms: &mut Vec<Arm<TExtDialect>>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit(term, ext, ext_state);
        for arm in arms {
            let c = PatternCount::<TExtDialect>::collect(&arm.pat, ext, ext_state);
            self.cutoff += c;
            self.visit(&mut arm.term, ext, ext_state);
            self.cutoff -= c;
        }
    }

    fn visit_unpack(
        &mut self,
        _: &mut Span,
        package: &mut Term<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit(package, ext, ext_state);
        self.cutoff += 1;
        self.visit(term, ext, ext_state);
        self.cutoff -= 1;
    }
}

pub struct Subst<TExtDialect: hash::Hash + Eq + Clone + fmt::Debug + Default + PartialEq + PartialOrd + SystemRDialect>
{
    cutoff: usize,
    term: Term<TExtDialect>,
}

impl<TExtDialect: hash::Hash + Eq + Clone + fmt::Debug + Default + PartialEq + PartialOrd + SystemRDialect>
    Subst<TExtDialect>
{
    pub fn new(term: Term<TExtDialect>) -> Subst<TExtDialect> {
        Subst { cutoff: 0, term }
    }
}

impl<
        TExtDialect: hash::Hash + Eq + Clone + fmt::Debug + Default + PartialEq + PartialOrd + SystemRDialect,
        TExt: SystemRExtension<TExtDialect> + fmt::Debug + Default + Clone,
    > MutTermVisitor<TExtDialect, TExt> for Subst<TExtDialect>
{
    fn visit_abs(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.cutoff += 1;
        self.visit(term, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_let(
        &mut self,
        sp: &mut Span,
        pat: &mut Pattern<TExtDialect>,
        t1: &mut Term<TExtDialect>,
        t2: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit(t1, ext, ext_state);
        let c = PatternCount::<TExtDialect>::collect(pat, ext, ext_state);
        self.cutoff += c;
        self.visit(t2, ext, ext_state);
        self.cutoff -= c;
    }

    fn visit_case(
        &mut self,
        sp: &mut Span,
        term: &mut Term<TExtDialect>,
        arms: &mut Vec<Arm<TExtDialect>>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit(term, ext, ext_state);
        for arm in arms {
            let c = PatternCount::<TExtDialect>::collect(&arm.pat, ext, ext_state);
            self.cutoff += c;
            self.visit(&mut arm.term, ext, ext_state);
            self.cutoff -= c;
        }
    }

    fn visit_unpack(
        &mut self,
        _: &mut Span,
        package: &mut Term<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit(package, ext, ext_state);
        self.cutoff += 1;
        self.visit(term, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit(&mut self, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {
        let sp = &mut term.span;
        match &mut term.kind {
            Kind::Var(v) if *v == self.cutoff => {
                Shift::new(self.cutoff as isize).visit(&mut self.term, ext, ext_state);
                *term = self.term.clone();
            }
            _ => self.walk(term, ext, ext_state),
        }
    }
}

pub struct TyTermSubst<
    TExtDialect: SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
> {
    cutoff: usize,
    ty: Type<TExtDialect>,
}

impl<TExtDialect: SystemRDialect + Eq + Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash>
    TyTermSubst<TExtDialect>
{
    pub fn new<TExt: SystemRExtension<TExtDialect> + Clone + Default + fmt::Debug>(
        ty: Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) -> TyTermSubst<TExtDialect> {
        use crate::types::visit::*;
        let mut ty = ty;
        Shift::new(1).visit(&mut ty, ext, ext_state);
        TyTermSubst { cutoff: 0, ty }
    }

    fn visit_ty<TExt: SystemRExtension<TExtDialect> + Clone + Default + fmt::Debug>(
        &mut self,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        let mut s = crate::types::visit::Subst {
            cutoff: self.cutoff,
            ty: self.ty.clone(),
        };
        s.visit(ty, ext, ext_state);
    }
}

impl<
        TExtDialect: hash::Hash + Eq + Clone + fmt::Debug + Default + PartialEq + PartialOrd + SystemRDialect,
        TExt: SystemRExtension<TExtDialect> + fmt::Debug + Default + Clone,
    > MutTermVisitor<TExtDialect, TExt> for TyTermSubst<TExtDialect>
{
    fn visit_abs(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        // self.cutoff += 1;
        self.visit_ty(ty, ext, ext_state);
        self.visit(term, ext, ext_state);
        // self.cutoff -= 1;
    }

    fn visit_tyapp(
        &mut self,
        sp: &mut Span,
        term: &mut Term<TExtDialect>,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit_ty(ty, ext, ext_state);
        self.visit(term, ext, ext_state);
    }

    fn visit_tyabs(
        &mut self,
        sp: &mut Span,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.cutoff += 1;
        self.visit(term, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_fold(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit_ty(ty, ext, ext_state);
        self.visit(term, ext, ext_state);
    }

    fn visit_unfold(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit_ty(ty, ext, ext_state);
        self.visit(term, ext, ext_state);
    }

    fn visit_unpack(
        &mut self,
        _: &mut Span,
        package: &mut Term<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit(package, ext, ext_state);
        self.cutoff += 1;
        self.visit(term, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_pack(
        &mut self,
        _: &mut Span,
        wit: &mut Type<TExtDialect>,
        body: &mut Term<TExtDialect>,
        sig: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit_ty(wit, ext, ext_state);
        self.visit(body, ext, ext_state);
        self.visit_ty(sig, ext, ext_state);
    }

    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut Term<TExtDialect>,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.visit_ty(ty, ext, ext_state);
        self.visit(term, ext, ext_state);
    }
}

/// Visitor for handling recursive variants automatically, by inserting a
/// fold term
///
/// Transform an [`Injection`] term of form: `Label tm of Rec(u.T)` into
/// `fold [u.T] Label tm of [X->u.T] T`
pub struct InjRewriter<TExtDialect: Eq + Clone + fmt::Debug + Default + PartialEq + PartialOrd + SystemRDialect>(
    pub TExtDialect::TExtPat,
    pub TExtDialect::TExtKind,
);

impl<
        TExtDialect: hash::Hash + Eq + Clone + fmt::Debug + Default + PartialEq + PartialOrd + SystemRDialect,
        TExt: SystemRExtension<TExtDialect> + fmt::Debug + Default + Clone,
    > MutTermVisitor<TExtDialect, TExt> for InjRewriter<TExtDialect>
{
    fn visit(&mut self, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {
        match &mut term.kind {
            Kind::Injection(label, val, ty) => {
                if let Type::Rec(inner) = *ty.clone() {
                    let ty_prime = crate::types::subst(*ty.clone(), *inner.clone(), ext, ext_state);
                    let rewrite_ty = Term::new(
                        Kind::Injection(label.clone(), val.clone(), Box::new(ty_prime)),
                        term.span,
                    );

                    *term = Term::new(Kind::Fold(ty.clone(), Box::new(rewrite_ty)), term.span);
                }
                self.walk(term, ext, ext_state);
            }
            _ => self.walk(term, ext, ext_state),
        }
    }
}

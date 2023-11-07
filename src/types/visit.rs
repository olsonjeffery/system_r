use super::Type;
use crate::{
    extensions::{SystemRDialect, SystemRExtension},
    visit::MutTypeVisitor,
};
use core::fmt;
use std::{convert::TryFrom, hash};

pub struct Shift {
    pub cutoff: usize,
    pub shift: isize,
}

impl Shift {
    pub const fn new(shift: isize) -> Shift {
        Shift { cutoff: 0, shift }
    }
}

impl<
        TExtDialect: SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
        TExt: SystemRExtension<TExtDialect> + Clone + Default + fmt::Debug,
    > MutTypeVisitor<TExtDialect, TExt> for Shift
{
    fn visit_ext(
        &mut self,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &<TExtDialect as SystemRDialect>::TExtDialectState,
    ) {
        ext.ty_shift_visit_ext(self, ty, ext_state);
    }
    fn visit_var(&mut self, var: &mut usize, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {
        if *var >= self.cutoff {
            *var = usize::try_from(*var as isize + self.shift).expect("Variable has been shifted below 0! Fatal bug");
        }
    }

    fn visit_universal(
        &mut self,
        inner: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_existential(
        &mut self,
        inner: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_rec(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {
        self.cutoff += 1;
        self.visit(ty, ext, ext_state);
        self.cutoff -= 1;
    }
}

/// Represents substituting the provided `Type` into
/// TmVar(0) position in the `&mut Type<TExtDialect>` provided to
/// `visit()`
pub struct Subst<
    TExtDialect: Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
> {
    pub cutoff: usize,
    pub ty: Type<TExtDialect>,
}

impl<TExtDialect: Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash>
    Subst<TExtDialect>
{
    pub fn new(ty: Type<TExtDialect>) -> Subst<TExtDialect> {
        Subst { cutoff: 0, ty }
    }
}

impl<
        TExtDialect: Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
        TExt: SystemRExtension<TExtDialect> + Clone + Default + fmt::Debug,
    > MutTypeVisitor<TExtDialect, TExt> for Subst<TExtDialect>
{
    fn visit_universal(
        &mut self,
        inner: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_existential(
        &mut self,
        inner: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_rec(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {
        self.cutoff += 1;
        self.visit(ty, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_ext(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {
        ext.ty_subst_visit_ext(self, ty, ext_state);
    }

    fn visit(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat | Type::Tag(_) => {}
            Type::PlatformBinding(i, r) => {}
            Type::Var(v) => {
                let makes_cutoff = *v >= self.cutoff;
                if self.ty == Type::Nat {
                    //panic!("about to substitute in Nat to {:?}, would it make
                    // the cutoff? {:?}", ty, makes_cutoff)
                }
                if makes_cutoff {
                    Shift::new(self.cutoff as isize).visit(&mut self.ty, ext, ext_state);
                    *ty = self.ty.clone();
                } else {
                    self.visit_var(v, ext, ext_state); // this is a NOOP; should
                                                       // remove? was previously
                                                       // another arm of this
                                                       // match
                }
            }
            Type::Variant(v) => self.visit_variant(v, ext, ext_state),
            Type::Product(v) => self.visit_product(v, ext, ext_state),
            Type::Alias(v) => self.visit_alias(v, ext, ext_state),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2, ext, ext_state),
            Type::Universal(ty) => self.visit_universal(ty, ext, ext_state),
            Type::Existential(ty) => self.visit_existential(ty, ext, ext_state),
            Type::Rec(ty) => self.visit_rec(ty, ext, ext_state),
            Type::Extended(_) => self.visit_ext(ty, ext, ext_state),
        }
    }
}

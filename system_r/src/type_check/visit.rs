use super::Type;
use crate::{
    dialect::{SystemRDialect, SystemRExtension},
    visit::MutTypeVisitor,
};
use anyhow::{Context, Result};
use std::convert::TryFrom;

pub struct Shift {
    pub cutoff: usize,
    pub shift: isize,
}

impl Shift {
    pub const fn new(shift: isize) -> Shift {
        Shift { cutoff: 0, shift }
    }
}

impl<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>> MutTypeVisitor<TExtDialect, TExt> for Shift {
    fn visit_ext(
        &mut self,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &<TExtDialect as SystemRDialect>::DialectState,
    ) -> Result<()> {
        ext.ty_shift_visit_ext(self, ty, ext_state)
    }

    fn visit_var(&mut self, var: &mut usize, ext: &mut TExt, ext_state: &TExtDialect::DialectState) -> Result<()> {
        if *var >= self.cutoff {
            *var =
                usize::try_from(*var as isize + self.shift).context("Variable has been shifted below 0! Fatal bug")?;
        }
        Ok(())
    }

    fn visit_universal(
        &mut self,
        inner: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state)?;
        self.cutoff -= 1;
        Ok(())
    }

    fn visit_existential(
        &mut self,
        inner: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state)?;
        self.cutoff -= 1;
        Ok(())
    }

    fn visit_rec(
        &mut self,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.cutoff += 1;
        self.visit(ty, ext, ext_state)?;
        self.cutoff -= 1;
        Ok(())
    }
}

/// Represents substituting the provided `Type` into
/// TmVar(0) position in the `&mut Type<TExtDialect>` provided to
/// `visit()`
pub struct Subst<TExtDialect: SystemRDialect> {
    pub cutoff: usize,
    pub ty: Type<TExtDialect>,
}

impl<TExtDialect: SystemRDialect> Subst<TExtDialect> {
    pub fn new(ty: Type<TExtDialect>) -> Subst<TExtDialect> {
        Subst { cutoff: 0, ty }
    }
}

impl<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>> MutTypeVisitor<TExtDialect, TExt>
    for Subst<TExtDialect>
{
    fn visit_universal(
        &mut self,
        inner: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state)?;
        self.cutoff -= 1;
        Ok(())
    }

    fn visit_existential(
        &mut self,
        inner: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state)?;
        self.cutoff -= 1;
        Ok(())
    }

    fn visit_rec(
        &mut self,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.cutoff += 1;
        self.visit(ty, ext, ext_state)?;
        self.cutoff -= 1;
        Ok(())
    }

    fn visit_ext(
        &mut self,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        ext.ty_subst_visit_ext(self, ty, ext_state)
    }

    fn visit(
        &mut self,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        match ty {
            Type::Unit | Type::Bool | Type::Nat | Type::Tag(_) | Type::Bytes => Ok(()),
            Type::PlatformBinding(i, r) => Ok(()),
            Type::Var(v) => {
                let makes_cutoff = *v >= self.cutoff;
                if makes_cutoff {
                    Shift::new(self.cutoff as isize).visit(&mut self.ty, ext, ext_state)?;
                    *ty = self.ty.clone();
                    Ok(())
                } else {
                    self.visit_var(v, ext, ext_state) // this is a NOOP; should
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

use core::fmt;
use std::hash;

use crate::{
    patterns::{PatTyStack, Pattern},
    syntax::parser::Parser,
    terms::Term,
    type_check::{
        patterns::Matrix,
        visit::{Shift, Subst},
        Aliaser, Type, TypeChecker,
    },
};

use anyhow::Result;

pub mod bottom;

pub trait ExtendedTokenKind:
    fmt::Debug + PartialEq + PartialOrd + Default + Clone + Sync + Send + fmt::Display
{
}
pub trait ExtendedKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone {}
pub trait ExtendedPattern: fmt::Debug + PartialEq + PartialOrd + Default + Clone {}
pub trait ExtendedType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash {}
pub trait ExtendedDialectState: fmt::Debug + Default + Clone {}

pub trait SystemRDialect: hash::Hash + Eq + Default + fmt::Debug + Clone + PartialEq + PartialOrd {
    type TokenKind: ExtendedTokenKind;
    type Kind: ExtendedKind;
    type Pattern: ExtendedPattern;
    type Type: ExtendedType;
    type DialectState: ExtendedDialectState;
}

pub trait SystemRExtension<TExtDialect: SystemRDialect>: Copy + Clone + Default + fmt::Debug {
    fn lex_is_ext_single(&self, x: char) -> bool;
    fn lex_is_extended_single_pred(&self, x: char) -> bool;
    fn lex_is_ext_keyword(&self, data: &str) -> bool;
    fn lex_extended_single(&mut self, data: &str) -> TExtDialect::TokenKind;
    fn lex_ext_keyword(&mut self, data: &str) -> Result<TExtDialect::TokenKind>;
    fn parser_has_ext_parse(&self, tk: &TExtDialect::TokenKind) -> Result<bool>;
    fn parser_ext_parse(&mut self, ps: &mut Parser<TExtDialect>) -> Result<Term<TExtDialect>>;
    fn parser_has_ext_atom(&self, tk: &TExtDialect::TokenKind) -> bool;
    fn parser_ext_atom(&mut self, ps: &mut Parser<TExtDialect>) -> Result<Term<TExtDialect>>;
    fn parser_ty(&mut self, ps: &mut Parser<TExtDialect>) -> Result<Type<TExtDialect>>;
    fn parser_ty_bump_if(&mut self, ps: &mut Parser<TExtDialect>) -> bool;
    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::type_check::patterns::Matrix<'a, TExtDialect>,
        ext_pattern: &Pattern<TExtDialect>,
    ) -> bool;
    fn pat_ext_matches(&self, pat: &TExtDialect::Pattern, term: &Term<TExtDialect>) -> bool;
    fn pat_ext_pattern_type_eq(
        &self,
        ctx: &TypeChecker<TExtDialect>,
        pat: &TExtDialect::Pattern,
        ty: &Type<TExtDialect>,
    ) -> bool;
    fn pat_ctor_eq_within(
        &self,
        ctx: &mut TypeChecker<TExtDialect>,
        label: &str,
        inner: &Pattern<TExtDialect>,
        target: &TExtDialect::Type,
    ) -> bool;
    fn type_check_injection_to_ext(
        &mut self,
        ctx: &mut TypeChecker<TExtDialect>,
        label: &str,
        target: &TExtDialect::Type,
        tm: &Term<TExtDialect>,
    ) -> Result<Type<TExtDialect>>;
    fn pat_visit_constructor_of_ext(
        &mut self,
        ext_state: &TExtDialect::DialectState,
        pts: &mut PatTyStack<TExtDialect>,
        label: &str,
        pat: &Pattern<TExtDialect>,
        ext_ty: &TExtDialect::Type,
    ) -> Result<()>;
    fn exhaustive_for_ext(&mut self, matrix: &Matrix<TExtDialect>, ext_state: &mut TExtDialect::DialectState) -> bool;
    fn ty_shift_visit_ext(
        &mut self,
        shift: &mut Shift,
        ext_ty: &mut Type<TExtDialect>,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()>;
    fn ty_aliaser_visit_ext(
        &mut self,
        aliaser: &mut Aliaser<TExtDialect>,
        ext_ty: &mut Type<TExtDialect>,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()>;
    fn ty_subst_visit_ext(
        &mut self,
        subst_visitor: &mut Subst<TExtDialect>,
        ext_ty: &mut Type<TExtDialect>,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()>;
    fn type_check_ext_equals_ty(
        &mut self,
        ctx: &mut TypeChecker<TExtDialect>,
        ext_ty: &mut TExtDialect::Type,
        other_ty: &mut Type<TExtDialect>,
    ) -> bool;
    fn to_plaintext(&self, input: &Term<TExtDialect>) -> Result<String>;
}

pub trait SystemRResolver<InDialect: SystemRDialect, OutDialect: SystemRDialect> {
    fn resolve(&self, ext_state: InDialect::DialectState, tm: Term<InDialect>) -> Result<Term<OutDialect>>;
}

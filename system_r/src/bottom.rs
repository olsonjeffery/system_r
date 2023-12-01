use std::fmt::Display;

use crate::{
    dialect::{
        ExtendedDialectState, ExtendedKind, ExtendedPattern, ExtendedTokenKind, ExtendedType, SystemRDialect,
        SystemRExtension,
    },
    patterns::Pattern,
    terms::Term,
    type_check::{Type, TypeChecker},
};
use anyhow::Result;

#[derive(Eq, Hash, Clone, Default, Debug, PartialEq, PartialOrd)]
pub struct BottomDialect;

impl SystemRDialect for BottomDialect {
    type DialectState = BottomState;
    type Kind = BottomKind;
    type Pattern = BottomPattern;
    type TokenKind = BottomTokenKind;
    type Type = BottomType;
}

#[derive(Default, Debug, Clone)]
pub struct BottomState;
impl ExtendedDialectState for BottomState {}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum BottomTokenKind {
    #[default]
    Placeholder,
}
impl ExtendedTokenKind for BottomTokenKind {}
impl Display for BottomTokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BottomTokenKind")
            .field("variant", &format!("{:?}", self))
            .finish()
    }
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum BottomKind {
    #[default]
    Placeholder,
}
impl ExtendedKind for BottomKind {}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum BottomPattern {
    #[default]
    Placeholder,
}
impl ExtendedPattern for BottomPattern {}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Hash)]
pub enum BottomType {
    #[default]
    Placeholder,
}
impl ExtendedType for BottomType {}

#[derive(Copy, Clone, Default, Debug, PartialEq, PartialOrd)]
pub struct BottomExtension;

impl SystemRExtension<BottomDialect> for BottomExtension {
    fn lex_is_ext_single(&self, x: char) -> bool {
        false
    }

    fn lex_is_extended_single_pred(&self, x: char) -> bool {
        false
    }

    fn lex_is_ext_keyword(&self, data: &str) -> bool {
        false
    }

    fn lex_extended_single(&mut self, data: &str) -> BottomTokenKind {
        BottomTokenKind::Placeholder
    }

    fn lex_ext_keyword(&mut self, data: &str) -> BottomTokenKind {
        BottomTokenKind::Placeholder
    }

    fn pat_ext_pattern_type_eq(
        &self,
        ctx: &TypeChecker<BottomDialect>,
        pat: &BottomPattern,
        ty: &crate::type_check::Type<BottomDialect>,
    ) -> bool {
        false
    }

    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::type_check::patterns::Matrix<'a, BottomDialect>,
        ext_pattern: &Pattern<BottomDialect>,
    ) -> bool {
        false
    }

    fn pat_ext_matches(&self, pat: &BottomPattern, term: &crate::terms::Term<BottomDialect>) -> bool {
        false
    }

    fn parser_has_ext_parse(&self, tk: &BottomTokenKind) -> bool {
        false
    }

    fn parser_has_ext_atom(&self, tk: &BottomTokenKind) -> bool {
        false
    }

    fn parser_ext_parse<'s>(
        &mut self,
        ps: &mut crate::syntax::parser::ParserState<BottomDialect>,
    ) -> Result<Term<BottomDialect>> {
        Err(anyhow!("shouldn't be called"))
    }

    fn parser_ext_atom<'s>(
        &mut self,
        ps: &mut crate::syntax::parser::ParserState<BottomDialect>,
    ) -> Result<Term<BottomDialect>> {
        Err(anyhow!("shouldn't be called"))
    }

    fn parser_ty_bump_if(&mut self, ps: &mut crate::syntax::parser::ParserState<BottomDialect>) -> bool {
        false
    }

    fn parser_ty(
        &mut self,
        ps: &mut crate::syntax::parser::ParserState<BottomDialect>,
    ) -> Result<crate::type_check::Type<BottomDialect>> {
        Err(anyhow!("calling parser_ty on BottomExtension; shouldn't happen"))
    }

    fn pat_ctor_eq_within(
        &self,
        ctx: &mut TypeChecker<BottomDialect>,
        label: &str,
        inner: &Pattern<BottomDialect>,
        target: &<BottomDialect as SystemRDialect>::Type,
    ) -> bool {
        false
    }

    fn type_check_injection_to_ext(
        &mut self,
        ctx: &mut TypeChecker<BottomDialect>,
        label: &str,
        target: &<BottomDialect as SystemRDialect>::Type,
        tm: &Term<BottomDialect>,
    ) -> Result<crate::type_check::Type<BottomDialect>> {
        Err(anyhow!("type_check_injection_to_ext for BottomDialect; should never be called"))
    }

    fn pat_visit_constructor_of_ext(
        &mut self,
        ext_state: &BottomState,
        pts: &mut crate::patterns::PatTyStack<BottomDialect>,
        label: &str,
        pat: &Pattern<BottomDialect>,
        ext_ty: &<BottomDialect as SystemRDialect>::Type,
    ) {
    }

    fn exhaustive_for_ext(
        &mut self,
        matrix: &crate::type_check::patterns::Matrix<BottomDialect>,
        ext_state: &mut <BottomDialect as SystemRDialect>::DialectState,
    ) -> bool {
        false
    }

    fn ty_subst_visit_ext(
        &mut self,
        subst_visitor: &mut crate::type_check::visit::Subst<BottomDialect>,
        ty: &mut Type<BottomDialect>,
        ext_state: &<BottomDialect as SystemRDialect>::DialectState,
    ) {
    }

    fn ty_aliaser_visit_ext(
        &mut self,
        aliaser: &mut crate::type_check::Aliaser<BottomDialect>,
        ext_ty: &mut Type<BottomDialect>,
        ext_state: &<BottomDialect as SystemRDialect>::DialectState,
    ) {
    }

    fn ty_shift_visit_ext(
        &mut self,
        shift: &mut crate::type_check::visit::Shift,
        ext_ty: &mut Type<BottomDialect>,
        ext_state: &<BottomDialect as SystemRDialect>::DialectState,
    ) {
    }

    fn type_check_ext_equals_ty(
        &mut self,
        ctx: &mut TypeChecker<BottomDialect>,
        ext_ty: &mut <BottomDialect as SystemRDialect>::Type,
        other_ty: &mut crate::type_check::Type<BottomDialect>,
    ) -> bool {
        false
    }
}

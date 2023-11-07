use crate::{
    diagnostics::Diagnostic,
    extensions::{SystemRDialect, SystemRExtension},
    patterns::Pattern,
    syntax::error::Error,
    terms::Term,
    types::{Context, Type},
};

#[derive(Eq, Hash, Clone, Default, Debug, PartialEq, PartialOrd)]
pub struct BottomDialect;

impl SystemRDialect for BottomDialect {
    type TExtDialectState = BottomState;
    type TExtKind = BottomKind;
    type TExtPat = BottomPattern;
    type TExtTokenKind = BottomTokenKind;
    type TExtType = BottomType;
}

#[derive(Default, Debug, Clone)]
pub struct BottomState;

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum BottomTokenKind {
    #[default]
    Placeholder,
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum BottomKind {
    #[default]
    Placeholder,
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum BottomPattern {
    #[default]
    Placeholder,
}
//TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Hash)]
pub enum BottomType {
    #[default]
    Placeholder,
}

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
        ctx: &Context<BottomDialect>,
        pat: &BottomPattern,
        ty: &crate::types::Type<BottomDialect>,
    ) -> bool {
        false
    }

    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::types::patterns::Matrix<'a, BottomDialect>,
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
    ) -> Result<Term<BottomDialect>, Error<BottomTokenKind>> {
        panic!("shouldn't be called");
    }

    fn parser_ext_atom<'s>(
        &mut self,
        ps: &mut crate::syntax::parser::ParserState<BottomDialect>,
    ) -> Result<Term<BottomDialect>, Error<BottomTokenKind>> {
        panic!("shouldn't be called");
    }

    fn parser_ty_bump_if(&mut self, ps: &mut crate::syntax::parser::ParserState<BottomDialect>) -> bool {
        false
    }

    fn parser_ty(
        &mut self,
        ps: &mut crate::syntax::parser::ParserState<BottomDialect>,
    ) -> Result<crate::types::Type<BottomDialect>, Error<BottomTokenKind>> {
        panic!("calling parser_ty on BottomExtension; shouldn't happen");
    }

    fn pat_ctor_eq_within(
        &self,
        ctx: &mut Context<BottomDialect>,
        label: &str,
        inner: &Pattern<BottomDialect>,
        target: &<BottomDialect as SystemRDialect>::TExtType,
    ) -> bool {
        false
    }

    fn type_check_injection_to_ext(
        &mut self,
        ctx: &mut Context<BottomDialect>,
        label: &str,
        target: &<BottomDialect as SystemRDialect>::TExtType,
        tm: &Term<BottomDialect>,
    ) -> Result<crate::types::Type<BottomDialect>, Diagnostic> {
        panic!("type_check_injection_to_ext for BottomDialect; should never be called")
    }

    fn type_check_application_of_ext(
        &mut self,
        ctx: &mut Context<BottomDialect>,
        t1: &Term<BottomDialect>,
        ty1: &crate::types::Type<BottomDialect>,
        t2: &Term<BottomDialect>,
        ty2: &crate::types::Type<BottomDialect>,
    ) -> Result<crate::types::Type<BottomDialect>, Diagnostic> {
        panic!("type_check_application_of_ext for BottomDialect; should never be called")
    }

    fn pat_visit_constructor_of_ext(
        &mut self,
        ext_state: &BottomState,
        pts: &mut crate::patterns::PatTyStack<BottomDialect>,
        label: &str,
        pat: &Pattern<BottomDialect>,
        ext_ty: &<BottomDialect as SystemRDialect>::TExtType,
    ) {
    }

    fn exhaustive_for_ext(
        &mut self,
        matrix: &crate::types::patterns::Matrix<BottomDialect>,
        ext_state: &mut <BottomDialect as SystemRDialect>::TExtDialectState,
    ) -> bool {
        false
    }

    fn ty_subst_visit_ext(
        &mut self,
        subst_visitor: &mut crate::types::visit::Subst<BottomDialect>,
        ty: &mut Type<BottomDialect>,
        ext_state: &<BottomDialect as SystemRDialect>::TExtDialectState,
    ) {
    }

    fn ty_aliaser_visit_ext(
        &mut self,
        aliaser: &mut crate::types::Aliaser<BottomDialect>,
        ext_ty: &mut Type<BottomDialect>,
        ext_state: &<BottomDialect as SystemRDialect>::TExtDialectState,
    ) {
    }

    fn ty_shift_visit_ext(
        &mut self,
        shift: &mut crate::types::visit::Shift,
        ext_ty: &mut Type<BottomDialect>,
        ext_state: &<BottomDialect as SystemRDialect>::TExtDialectState,
    ) {
    }

    fn type_check_ext_equals_ty(
        &mut self,
        ctx: &mut Context<BottomDialect>,
        ext_ty: &mut <BottomDialect as SystemRDialect>::TExtType,
        other_ty: &mut crate::types::Type<BottomDialect>,
    ) -> bool {
        false
    }
}

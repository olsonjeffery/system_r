/*
Copyright (C) 2023 AUTHORS

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License in the ./LICENSE.APACHE2 file
in this repository.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
use core::fmt;
use std::hash;

use crate::{
    diagnostics::Diagnostic,
    patterns::{Pattern, PatTyStack},
    syntax::{error::Error, parser::ParserState},
    types::{Context, Type, patterns::Matrix, visit::{Subst, Shift}, Aliaser}, terms::Term,
};

pub mod type_alias;

pub trait SystemRDialect {
    type TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone;
    type TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone;
    type TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone;
    type TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash;
    type TExtDialectState: fmt::Debug + Default + Clone;
}

pub trait SystemRExtension<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Default + fmt::Debug + Clone + PartialEq + PartialOrd,
>
{
    fn lex_is_ext_single(&self, x: char) -> bool;
    fn lex_is_extended_single_pred(&self, x: char) -> bool;
    fn lex_is_ext_keyword(&self, data: &str) -> bool;
    fn lex_extended_single(&mut self, data: &str) -> TExtDialect::TExtTokenKind;
    fn lex_ext_keyword(&mut self, data: &str) -> TExtDialect::TExtTokenKind;
    fn parser_has_ext_parse(&self, tk: &TExtDialect::TExtTokenKind) -> bool;
    fn parser_ext_parse<'s>(
        &mut self,
        ps: &mut ParserState<TExtDialect>,
    ) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>>;
    fn parser_has_ext_atom(&self, tk: &TExtDialect::TExtTokenKind) -> bool;
    fn parser_ext_atom<'s>(
        &mut self,
        ps: &mut ParserState<TExtDialect>,
    ) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>>;
    fn parser_ty(
        &mut self,
        ps: &mut ParserState<TExtDialect>,
    ) -> Result<Type<TExtDialect>, Error<TExtDialect::TExtTokenKind>>;
    fn parser_ty_bump_if(&mut self, ps: &mut ParserState<TExtDialect>) -> bool;
    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::types::patterns::Matrix<'a, TExtDialect>,
        ext_pattern: &Pattern<TExtDialect>,
    ) -> bool;
    fn pat_ext_matches(&self, pat: &TExtDialect::TExtPat, term: &Term<TExtDialect>) -> bool;
    fn pat_ext_pattern_type_eq(
        &self,
        ctx: &Context<TExtDialect>,
        pat: &TExtDialect::TExtPat,
        ty: &Type<TExtDialect>,
    ) -> bool;
    fn pat_ctor_eq_within(
        &self,
        ctx: &mut Context<TExtDialect>,
        label: &str,
        inner: &Pattern<TExtDialect>,
        target: &TExtDialect::TExtType,
    ) -> bool;
    fn type_check_injection_to_ext(
        &mut self,
        ctx: &mut Context<TExtDialect>,
        label: &str,
        target: &TExtDialect::TExtType,
        tm: &Term<TExtDialect>,
    ) -> Result<Type<TExtDialect>, Diagnostic>;
    fn type_check_application_of_ext(
        &mut self,
        ctx: &mut Context<TExtDialect>,
        t1: &Term<TExtDialect>,
        ty1: &Type<TExtDialect>,
        t2: &Term<TExtDialect>,
        ty2: &Type<TExtDialect>,
    ) -> Result<Type<TExtDialect>, Diagnostic>;
    fn pat_visit_constructor_of_ext(
        &mut self,
        ext_state: &mut TExtDialect::TExtDialectState,
        pts: &mut PatTyStack<TExtDialect>,
        label: &str,
        pat: &Pattern<TExtDialect>,
        ext_ty: &TExtDialect::TExtType
    );
    fn exhaustive_for_ext(
        &mut self,
        matrix: &Matrix<TExtDialect>,
        ext_state: &mut TExtDialect::TExtDialectState,
    ) -> bool;
    fn ty_shift_visit_ext(
        &mut self,
        shift: &mut Shift,
        ext_ty: &mut Type<TExtDialect>,
        ext_state: &mut TExtDialect::TExtDialectState,
    );
    fn ty_aliaser_visit_ext(
        &mut self,
        aliaser: &mut Aliaser<TExtDialect>,
        ext_ty: &mut Type<TExtDialect>,
        ext_state: &mut TExtDialect::TExtDialectState,
    );
    fn ty_subst_visit_ext(
        &mut self,
        subst_visitor: &mut Subst<TExtDialect>,
        ext_ty: &mut Type<TExtDialect>,
        ext_state: &mut TExtDialect::TExtDialectState,
    );
    fn type_check_ext_equals_ty(
        &mut self,
        ctx: &mut Context<TExtDialect>,
        ext_ty: &mut TExtDialect::TExtType,
        other_ty: &mut Type<TExtDialect>,
    ) -> bool;
}

pub trait SystemRTranslator<
    InDialect: Eq + SystemRDialect + Clone + PartialEq + PartialOrd + fmt::Debug + Default,
    OutDialect: Eq + SystemRDialect + Clone + PartialEq + PartialOrd + fmt::Debug + Default,
>
{
    fn resolve(
        &self,
        in_ctx: &mut Context<InDialect>,
        tm: Term<InDialect>,
    ) -> Result<(Context<OutDialect>, Term<OutDialect>), Diagnostic>;
}

// FIXME a set of translator visitor structs, with ::new(), to facilitate
// going from In* to Out*
//
// does it become a big, stacked visitor for Kind->{Pattern|Type} ?

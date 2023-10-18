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
    patterns::Pattern,
    syntax::{
        error::Error,
        lexer::{ExtLexer, Lexer},
        parser::ParserState,
        ExtToken,
    },
    terms::ExtTerm,
    types::Type,
};

use self::struct_data::TypeAliasDialect;

pub mod struct_data;

pub trait SystemRDialect {
    type TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone;
    type TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone;
    type TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone;
    type TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash;
    type TExtDialectState: fmt::Debug + Default + Clone;
}

pub trait SystemRExtension<TExtDialect: SystemRDialect + Default + fmt::Debug + Clone + PartialEq + PartialOrd> {
    fn lex_is_ext_single(&self, x: char) -> bool;
    fn lex_is_extended_single_pred(&self, x: char) -> bool;
    fn lex_is_ext_keyword(&self, data: &str) -> bool;
    fn lex_extended_single(&mut self, data: &str) -> TExtDialect::TExtTokenKind;
    fn lex_ext_keyword(&mut self, data: &str) -> TExtDialect::TExtTokenKind;
    fn parser_has_ext_parse(&self, tk: &TExtDialect::TExtTokenKind) -> bool;
    fn parser_ext_parse<'s>(
        &mut self,
        ps: &mut ParserState<TExtDialect>,
    ) -> Result<ExtTerm<TExtDialect>, Error<TExtDialect::TExtTokenKind>>;
    fn parser_has_ext_atom(&self, tk: &TExtDialect::TExtTokenKind) -> bool;
    fn parser_ext_atom<'s>(
        &mut self,
        ps: &mut ParserState<TExtDialect>,
    ) -> Result<ExtTerm<TExtDialect>, Error<TExtDialect::TExtTokenKind>>;
    fn pat_ext_pattern_type_eq(&self, pat: &TExtDialect::TExtPat, ty: &Type<TExtDialect::TExtType>) -> bool;
    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::types::patterns::Matrix<'a, TExtDialect>,
        ext_pattern: &Pattern<TExtDialect>,
    ) -> bool;
    fn pat_ext_matches(
        &self,
        pat: &TExtDialect::TExtPat,
        term: &ExtTerm<TExtDialect>,
    ) -> bool;
    fn parser_ty(
        &mut self,
        ps: &mut ParserState<TExtDialect>,
    ) -> Result<Type<TExtDialect::TExtType>, Error<TExtDialect::TExtTokenKind>>;
    fn parser_ty_bump_if(&mut self, ps: &mut ParserState<TExtDialect>) -> bool;
}

pub trait SystemRTranslator<
    InDialect: SystemRDialect + Clone + PartialEq + PartialOrd + fmt::Debug + Default,
    OutDialect: SystemRDialect + Clone + PartialEq + PartialOrd + fmt::Debug + Default,
>
{
    fn resolve(
        &self,
        st: &mut InDialect::TExtDialectState,
        tm: ExtTerm<InDialect>,
    ) -> Result<ExtTerm<OutDialect>, Diagnostic>;
}

// FIXME a set of translator visitor structs, with ::new(), to facilitate
// going from In* to Out*
//
// does it become a big, stacked visitor for Kind->{Pattern|Type} ?

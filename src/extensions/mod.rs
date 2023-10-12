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
    patterns::ExtPattern,
    syntax::{
        error::Error,
        lexer::{ExtLexer, Lexer},
        parser::ParserState,
        ExtToken,
    },
    terms::ExtTerm,
    types::Type,
};

pub mod struct_data;

pub trait SystemRExtension<
    TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
>
{
    fn lex_is_ext_single(&self, x: char) -> bool;
    fn lex_is_extended_single_pred(&self, x: char) -> bool;
    fn lex_is_ext_keyword(&self, data: &str) -> bool;
    fn lex_extended_single(&mut self, data: &str) -> TExtTokenKind;
    fn lex_ext_keyword(&mut self, data: &str) -> TExtTokenKind;
    fn parser_has_ext_parse(&self, tk: &TExtTokenKind) -> bool;
    fn parser_ext_parse<'s>(
        &mut self,
        ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>>;
    fn parser_has_ext_atom(&self, tk: &TExtTokenKind) -> bool;
    fn parser_ext_atom<'s>(
        &mut self,
        ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>>;
    fn pat_ext_pattern_type_eq(&self, pat: &TExtPat, ty: &Type<TExtType>) -> bool;
    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::types::patterns::Matrix<'a, TExtPat, TExtType>,
        ext_pattern: &ExtPattern<TExtPat>,
    ) -> bool;
    fn pat_ext_matches(&self, pat: &TExtPat, term: &ExtTerm<TExtPat, TExtKind, TExtType>) -> bool;
    fn parser_ty(
        &mut self,
        ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ) -> Result<Type<TExtType>, Error<TExtTokenKind>>;
    fn parser_ty_bump_if(
        &mut self,
        ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ) -> bool;
}

pub trait SystemRTranslator<
    InExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    InExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    InExtType: fmt::Debug + PartialEq + PartialOrd + Default + Clone + Eq + hash::Hash,
    InExtState: fmt::Debug +  Default + Clone,
    OutExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    OutExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    OutExtType: fmt::Debug + PartialEq + PartialOrd + Default + Clone + Eq + hash::Hash,
>
{
    fn resolve(
        &self,
        st: &mut InExtState,
        tm: ExtTerm<InExtPat, InExtKind, InExtType>,
    ) -> Result<ExtTerm<OutExtPat, OutExtKind, OutExtType>, Diagnostic>;
}

// FIXME a set of translator visitor structs, with ::new(), to facilitate
// going from In* to Out*
//
// does it become a big, stacked visitor for Kind->{Pattern|Type} ?
/*
Copyright (C) 2023 AUTHORS

GNU Lesser General Public License Version 3

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
use core::fmt;

use crate::{
    diagnostics::Diagnostic,
    patterns::ExtPattern,
    syntax::{
        lexer::{ExtLexer, Lexer},
        error::Error,
        ExtToken, parser::ParserState,
    },
    terms::ExtTerm,
    types::Type,
};

pub mod struct_data;

#[derive(Default)]
pub enum ParserOp<
    TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
> {
    #[default]
    Unit,
    Panic,
    Foo(TExtTokenKind, TExtKind, TExtPat),
}

pub trait SystemRExtension<
    TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
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
        ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat>,
    ) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>>;
    fn parser_has_ext_atom(&self, tk: &TExtTokenKind) -> bool;
    fn parser_ext_atom<'s>(
        &mut self,
        ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat>,
    ) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>>;
    fn pat_ext_pattern_type_eq(&self, pat: &TExtPat, ty: &Type) -> bool;
    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::types::patterns::Matrix<'a, TExtPat>,
        ext_pattern: &ExtPattern<TExtPat>,
    ) -> bool;
    fn pat_ext_matches(&self, pat: &TExtPat, term: &ExtTerm<TExtPat, TExtKind>) -> bool;
}

pub struct ParserOpCompletion<
    TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
>(pub String, pub ExtToken<TExtTokenKind>, pub ExtTerm<TExtPat, TExtKind>);

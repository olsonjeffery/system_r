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
use crate::{
    diagnostics::Diagnostic,
    extensions::{ParserOp, SystemRExtension},
    patterns::ExtPattern,
    syntax::{
        lexer::ExtLexer, /* lexer2::extlexer2 */
        error::Error,
    },
    system_r_util::span::Span,
    terms::{ExtKind, ExtTerm},
};

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

#[derive(Copy, Clone, Default, Debug)]
pub struct BottomExtension;

impl SystemRExtension<BottomTokenKind, BottomKind, BottomPattern> for BottomExtension {
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

    fn pat_ext_pattern_type_eq(&self, pat: &BottomPattern, ty: &crate::types::Type) -> bool {
        false
    }

    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::types::patterns::Matrix<'a, BottomPattern>,
        ext_pattern: &ExtPattern<BottomPattern>,
    ) -> bool {
        false
    }

    fn pat_ext_matches(&self, pat: &BottomPattern, term: &crate::terms::ExtTerm<BottomPattern, BottomKind>) -> bool {
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
        ps: &mut crate::syntax::parser::ParserState<BottomTokenKind, BottomKind, BottomPattern>,
    ) -> Result<ExtTerm<BottomPattern, BottomKind>, Error<BottomTokenKind>> {
        panic!("shouldn't be called");
    }

    fn parser_ext_atom<'s>(
        &mut self,
        ps: &mut crate::syntax::parser::ParserState<BottomTokenKind, BottomKind, BottomPattern>,
    ) -> Result<ExtTerm<BottomPattern, BottomKind>, Error<BottomTokenKind>> {
        panic!("shouldn't be called");
    }

    fn parser_ty_bump_if(&mut self, 
        ps: &mut crate::syntax::parser::ParserState<BottomTokenKind, BottomKind, BottomPattern>,
    ) -> bool {
        false 
    }

    fn parser_ty(&mut self, 
        ps: &mut crate::syntax::parser::ParserState<BottomTokenKind, BottomKind, BottomPattern>,
    ) -> Result<crate::types::Type, Error<BottomTokenKind>> {
        panic!("calling parser_ty on BottomExtension; shouldn't happen");
    }
}

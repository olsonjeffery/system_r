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
use crate::{extensions::{SystemRExtension, ParserOp}, patterns::ExtPattern, terms::{ExtTerm, ExtKind}, syntax::parser::Error, system_r_util::span::Span, diagnostics::Diagnostic};

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

#[derive(Clone, Default, Debug)]
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

    fn parser_has_top_level_ext(&self, tk: &BottomTokenKind) -> bool {
        false 
    }

    fn parser_top_level_ext<'s>(&mut self,
        c: crate::extensions::ParserOpCompletion<BottomTokenKind, BottomKind, BottomPattern>)
        -> Result<crate::extensions::ParserOpCompletion<BottomTokenKind, BottomKind, BottomPattern>, crate::diagnostics::Diagnostic> {
            Err(Diagnostic::error(Span::default(), "BottomExtension.parser_top_level_ext shouldn't be called"))
    }
}

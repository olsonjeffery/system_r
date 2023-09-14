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
use crate::{syntax::{parser::ParserExtension, lexer::LexerExtension}, patterns::{PatternExtension, ExtPattern}};

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

impl ParserExtension<BottomTokenKind, BottomKind> for BottomExtension {

}

impl PatternExtension<BottomPattern, BottomKind> for BottomExtension {
    fn add_ext_pattern<'a>(&'a self, parent: &'a crate::types::patterns::Matrix<'a, BottomPattern, BottomKind>, ext_pattern: &ExtPattern<BottomPattern, BottomKind>) -> bool {
        // should mutate parent
        false
    }

    fn ext_pattern_type_eq(&self, pat: &BottomPattern, ty: &crate::types::Type) -> bool {
        false
    }

    fn ext_matches(&self, pat: &BottomPattern, term: &crate::terms::ExtTerm<BottomPattern, BottomKind>) -> bool {
        false
    }
}

impl LexerExtension<BottomTokenKind> for BottomExtension {
    fn is_ext_single(&self, x: char) -> bool {
        false
    }

    fn extended_single_pred(&self, x: char) -> bool {

        false
    }

    fn lex_extended_single(&mut self, data: &str) -> BottomTokenKind {
        BottomTokenKind::default()
    }

    fn is_ext_keyword(&self, data: &str) -> bool {
        false
    }

    fn lex_ext_keyword(&mut self, data: &str) -> BottomTokenKind {
        BottomTokenKind::default()
    }
}
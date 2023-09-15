/*
Copyright (C) 2020-2023 Micheal Lazear, AUTHORS

The MIT License (MIT)

Copyright (c) ${license.years} ${license.owner}

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

---------------------

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
//! Extended Lexical analysis and recursive descent parser for System F
pub mod lexer;
pub mod parser;
use crate::system_r_util::span::Span;

use self::{parser::ParserExtension};

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum ExtTokenKind<TExtTokenKind: PartialEq> {
    Uppercase(String),
    Lowercase(String),
    Nat(u32),
    TyNat,
    TyBool,
    TyArrow,
    TyUnit,
    #[default]
    Unit,
    True,
    False,
    Lambda,
    Forall,
    Exists,
    As,
    Pack,
    Unpack,
    Succ,
    Pred,
    If,
    Then,
    Else,
    Let,
    In,
    IsZero,
    Semicolon,
    Colon,
    Comma,
    Proj,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LSquare,
    RSquare,
    Equals,
    Bar,
    Wildcard,
    Gt,
    Case,
    Of,
    Fix,
    Fold,
    Unfold,
    Rec,
    Invalid(char),
    Dummy,
    Eof,
    Tag(String),
    Extended(TExtTokenKind)
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub struct ExtToken<TExtTokenKind: Sized + Clone + Default + PartialEq> where {
    pub kind: ExtTokenKind<TExtTokenKind>,
    pub span: Span,
}

impl<TExtTokenKind: Default + PartialEq + Sized + Clone> ExtToken<TExtTokenKind> {
    pub const fn dummy() -> ExtToken<TExtTokenKind> {
        ExtToken {
            kind: ExtTokenKind::Dummy,
            span: Span::zero(),
        }
    }

    pub const fn new(kind: ExtTokenKind<TExtTokenKind>, span: Span) -> ExtToken<TExtTokenKind> {
        ExtToken { kind, span }
    }
}
/*
Copyright (C) 2020-2023 Micheal Lazear, AUTHORS

The MIT License (MIT)

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
//! Extended Lexical analysis and recursive descent parser for System F
pub mod debruijn;
pub mod error;
pub mod lexer;
pub mod parser;
use crate::system_r_util::span::Span;

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
    Extended(TExtTokenKind),
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub struct Token<TExtTokenKind: Sized + Clone + Default + PartialEq> {
    pub kind: ExtTokenKind<TExtTokenKind>,
    pub span: Span,
}

impl<TExtTokenKind: Default + PartialEq + Sized + Clone> Token<TExtTokenKind> {
    pub const fn dummy() -> Token<TExtTokenKind> {
        Token {
            kind: ExtTokenKind::Dummy,
            span: Span::zero(),
        }
    }

    pub const fn new(kind: ExtTokenKind<TExtTokenKind>, span: Span) -> Token<TExtTokenKind> {
        Token { kind, span }
    }
}

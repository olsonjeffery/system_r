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
use crate::bottom::{BottomExtension, BottomTokenKind};
use super::{ExtToken, ExtTokenKind};
use std::char;
use std::iter::Peekable;
use std::str::Chars;
use system_r_util::span::{Location, Span};

pub type Lexer<'s> = ExtLexer<'s, BottomTokenKind, BottomExtension>;

#[derive(Clone)]
pub struct ExtLexer<'s, TExtTokenKind: PartialEq + Default + Sized + Clone, TLE: LexerExtension<TExtTokenKind>> {
    input: Peekable<Chars<'s>>,
    current: Location,
    extension: TLE,
    _token: TExtTokenKind,
}

pub trait LexerExtension<TExtTokenKind: Sized + Clone> {
    fn is_ext_single(&self, x: char) -> bool;
    fn extended_single_pred(&self, x: char) -> bool;
    fn is_ext_keyword(&self, data: &str) -> bool;
    fn lex_extended_single(&mut self, data: &str) -> TExtTokenKind;
    fn lex_ext_keyword(&mut self, data: &str) -> TExtTokenKind;
}

fn is_tag(x: char) -> bool {
    if x == '@' {
        return true;
    }
    false
}

impl<'s, TExtTokenKind: PartialEq + Default + Clone + Sized, TLE: Clone + LexerExtension<TExtTokenKind>> ExtLexer<'s, TExtTokenKind, TLE> {
    pub fn new(input: Chars<'s>, extension: TLE) -> ExtLexer<'s, TExtTokenKind, TLE> {
        ExtLexer {
            input: input.peekable(),
            current: Location {
                line: 0,
                col: 0,
                abs: 0,
            },
            extension,
            _token: TExtTokenKind::default(),
        }
    }

    /// Peek at the next [`char`] in the input stream
    fn peek(&mut self) -> Option<char> {
        self.input.peek().cloned()
    }

    /// Consume the next [`char`] and advance internal source position
    fn consume(&mut self) -> Option<char> {
        match self.input.next() {
            Some('\n') => {
                self.current.line += 1;
                self.current.col = 0;
                self.current.abs += 1;
                Some('\n')
            }
            Some(ch) => {
                self.current.col += 1;
                self.current.abs += 1;
                Some(ch)
            }
            None => None,
        }
    }

    /// Consume characters from the input stream while pred(peek()) is true,
    /// collecting the characters into a string.
    fn consume_while<F: Fn(char) -> bool>(&mut self, pred: F) -> (String, Span) {
        let mut s = String::new();
        let start = self.current;
        while let Some(n) = self.peek() {
            if pred(n) {
                match self.consume() {
                    Some(ch) => s.push(ch),
                    None => break,
                }
            } else {
                break;
            }
        }
        (s, Span::new(start, self.current))
    }

    /// Eat whitespace
    fn consume_delimiter(&mut self) {
        let _ = self.consume_while(char::is_whitespace);
    }

    /// Lex a natural number
    fn number(&mut self) -> ExtToken<TExtTokenKind> {
        // Since we peeked at least one numeric char, we should always
        // have a string containing at least 1 single digit, as such
        // it is safe to call unwrap() on str::parse<u32>
        let (data, span) = self.consume_while(char::is_numeric);
        let n = data.parse::<u32>().unwrap();
        ExtToken::new(ExtTokenKind::Nat(n), span)
    }

    fn tag(&mut self) -> ExtToken<TExtTokenKind> {
        let (data, span) = self.consume_while(|ch|
            is_tag(ch) || ch.is_ascii_alphanumeric());
        let kind = ExtTokenKind::Tag(data);
        return ExtToken::new(kind, span);
    }

    fn extended_single(&mut self) -> ExtToken<TExtTokenKind> {
        let ext = self.extension.clone();
        let (data, span) = self.consume_while(|ch|
            ext.extended_single_pred(ch));
        let kind = self.extension.lex_extended_single(&data);
        return ExtToken::new(ExtTokenKind::Extended(kind), span);
    }

    /// Lex a reserved keyword or an identifier
    fn keyword(&mut self) -> ExtToken<TExtTokenKind> {
        let (data, span) = self.consume_while(|ch| ch.is_ascii_alphanumeric());
        let kind = match data.as_ref() {
            data if self.extension.is_ext_keyword(data) =>
                ExtTokenKind::Extended(self.extension.lex_ext_keyword(data)),
            "if" => ExtTokenKind::If,
            "then" => ExtTokenKind::Then,
            "else" => ExtTokenKind::Else,
            "true" => ExtTokenKind::True,
            "false" => ExtTokenKind::False,
            "succ" => ExtTokenKind::Succ,
            "pred" => ExtTokenKind::Pred,
            "iszero" => ExtTokenKind::IsZero,
            "zero" => ExtTokenKind::Nat(0),
            "Bool" => ExtTokenKind::TyBool,
            "Nat" => ExtTokenKind::TyNat,
            "Unit" => ExtTokenKind::TyUnit,
            "unit" => ExtTokenKind::Unit,
            "let" => ExtTokenKind::Let,
            "in" => ExtTokenKind::In,
            "fix" => ExtTokenKind::Fix,
            "case" => ExtTokenKind::Case,
            "of" => ExtTokenKind::Of,
            "fold" => ExtTokenKind::Fold,
            "unfold" => ExtTokenKind::Unfold,
            "rec" => ExtTokenKind::Rec,
            "lambda" => ExtTokenKind::Lambda,
            "forall" => ExtTokenKind::Forall,
            "exists" => ExtTokenKind::Exists,
            "pack" => ExtTokenKind::Pack,
            "unpack" => ExtTokenKind::Unpack,
            "as" => ExtTokenKind::As,

            _ => {
                if data.starts_with(|ch: char| ch.is_ascii_uppercase()) {
                    ExtTokenKind::Uppercase(data)
                } else {
                    ExtTokenKind::Lowercase(data)
                }
            }
        };
        ExtToken::new(kind, span)
    }

    /// Consume the next input character, expecting to match `ch`.
    /// Return a [`ExtTokenKind::Invalid`] if the next character does not match,
    /// or the argument `kind` if it does
    fn eat(&mut self, ch: char, kind: ExtTokenKind<TExtTokenKind>) -> ExtToken<TExtTokenKind> {
        let loc = self.current;
        // Lexer::eat() should only be called internally after calling peek()
        // so we know that it's safe to unwrap the result of Lexer::consume()
        let n = self.consume().unwrap();
        let kind = if n == ch { kind } else { ExtTokenKind::Invalid(n) };
        ExtToken::new(kind, Span::new(loc, self.current))
    }

    /// Return the next lexeme in the input as a [`Token`]
    pub fn lex(&mut self) -> ExtToken<TExtTokenKind> {
        self.consume_delimiter();
        let next = match self.peek() {
            Some(ch) => ch,
            None => return ExtToken::new(ExtTokenKind::Eof, Span::new(self.current, self.current)),
        };
        match next {
            x if self.extension.is_ext_single(x) => self.extended_single(),
            x if is_tag(x) => self.tag(),
            x if x.is_ascii_alphabetic() => self.keyword(),
            x if x.is_numeric() => self.number(),
            '(' => self.eat('(', ExtTokenKind::LParen),
            ')' => self.eat(')', ExtTokenKind::RParen),
            ';' => self.eat(';', ExtTokenKind::Semicolon),
            ':' => self.eat(':', ExtTokenKind::Colon),
            ',' => self.eat(',', ExtTokenKind::Comma),
            '{' => self.eat('{', ExtTokenKind::LBrace),
            '}' => self.eat('}', ExtTokenKind::RBrace),
            '[' => self.eat('[', ExtTokenKind::LSquare),
            ']' => self.eat(']', ExtTokenKind::RSquare),
            '\\' => self.eat('\\', ExtTokenKind::Lambda),
            'λ' => self.eat('λ', ExtTokenKind::Lambda),
            '∀' => self.eat('∀', ExtTokenKind::Forall),
            '∃' => self.eat('∃', ExtTokenKind::Exists),
            '.' => self.eat('.', ExtTokenKind::Proj),
            '=' => self.eat('=', ExtTokenKind::Equals),
            '|' => self.eat('|', ExtTokenKind::Bar),
            '_' => self.eat('_', ExtTokenKind::Wildcard),
            '>' => self.eat('>', ExtTokenKind::Gt),
            '-' => {
                self.consume();
                self.eat('>', ExtTokenKind::TyArrow)
            }
            ch => self.eat(' ', ExtTokenKind::Invalid(ch)),
        }
    }
}

impl<'s, TExtTokenKind: PartialEq + Default + Sized + Clone, TLE: Clone + LexerExtension<TExtTokenKind>> Iterator for ExtLexer<'s, TExtTokenKind, TLE> {
    type Item = ExtToken<TExtTokenKind>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            ExtToken {
                kind: ExtTokenKind::Eof, ..
            } => None,
            tok => Some(tok),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::bottom::{BottomTokenKind, BottomExtension};

    use super::*;
    use ExtTokenKind::*;
    #[test]
    fn nested() {
        let input = "succ(succ(succ(0)))";

        let expected = vec![Succ, LParen, Succ, LParen, Succ, LParen, Nat(0), RParen, RParen, RParen];
        let stub_ext = BottomExtension;
        let output = ExtLexer::new(input.chars(), stub_ext)
            .into_iter()
            .map(|t| t.kind)
            .collect::<Vec<ExtTokenKind<BottomTokenKind>>>();
        assert_eq!(expected, output);
    }

    #[test]
    fn case() {
        let input = "case x of | A _ => true | B x => (\\y: Nat. x)";
        let expected = vec![
            Case,
            Lowercase("x".into()),
            Of,
            Bar,
            Uppercase("A".into()),
            Wildcard,
            Equals,
            Gt,
            True,
            Bar,
            Uppercase("B".into()),
            Lowercase("x".into()),
            Equals,
            Gt,
            LParen,
            Lambda,
            Lowercase("y".into()),
            Colon,
            TyNat,
            Proj,
            Lowercase("x".into()),
            RParen,
        ];
        let stub_ext = BottomExtension;
        let output = ExtLexer::new(input.chars(), stub_ext)
            .into_iter()
            .map(|t| t.kind)
            .collect::<Vec<ExtTokenKind<BottomTokenKind>>>();
        assert_eq!(expected, output);
    }
}

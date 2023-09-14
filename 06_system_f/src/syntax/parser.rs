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
use super::lexer::{ExtLexer, LexerExtension};
use super::{ExtToken, ExtTokenKind};
use crate::bottom::{BottomTokenKind, BottomExtension, BottomKind, BottomPattern};

use core::fmt;
use std::collections::VecDeque;
use system_r_util::diagnostic::Diagnostic;
use system_r_util::span::*;

use crate::patterns::{PatVarStack, ExtPattern};
use crate::terms::*;
use crate::platform_bindings::PlatformBindings;
use crate::types::*;

pub type Parser<'s> = ExtParser<'s, BottomPattern, BottomKind, BottomTokenKind, BottomExtension, BottomExtension>;

#[derive(Clone, Debug, Default)]
pub struct DeBruijnIndexer {
    inner: VecDeque<String>,
}

impl<'s> Parser<'s> {
    pub fn new(platform_bindings: &'s PlatformBindings, input: &'s str) -> Self {
        ExtParser::ext_new(platform_bindings, input, BottomExtension, BottomExtension)
    }
}

impl DeBruijnIndexer {
    pub fn push(&mut self, hint: String) -> usize {
        let idx = self.inner.len();
        self.inner.push_front(hint);
        idx
    }

    pub fn pop(&mut self) {
        self.inner.pop_front();
    }

    pub fn lookup(&self, key: &str) -> Option<usize> {
        for (idx, s) in self.inner.iter().enumerate() {
            if key == s {
                return Some(idx);
            }
        }
        None
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

pub struct ExtParser<'s, TExtPat: fmt::Debug + PartialEq + Default + Sized + Clone,
                         TExtKind: fmt::Debug + PartialEq + Default + Sized + Clone,
                         TExtTokenKind: fmt::Debug + PartialEq + Default + Sized + Clone,
                         TLE: Clone + LexerExtension<TExtTokenKind>,
                         TPE: ParserExtension<TExtTokenKind, TExtKind>> {
    tmvar: DeBruijnIndexer,
    tyvar: DeBruijnIndexer,
    diagnostic: Diagnostic<'s>,
    lexer: ExtLexer<'s, TExtTokenKind, TLE>,
    span: Span,
    token: ExtToken<TExtTokenKind>,
    platform_bindings: PlatformBindings,
    extension: TPE,
    _kind: TExtKind,
    _pat: TExtPat,
}

pub trait ParserExtension<TExtTokenKind: fmt::Debug + PartialEq + Default + Sized + Clone,
                          TExtKind: fmt::Debug + PartialEq + Default + Sized + Clone> {

}

#[derive(Clone)]
pub struct Error<TExtTokenKind: PartialEq + Default + Sized + Clone> {
    pub span: Span,
    pub tok: ExtToken<TExtTokenKind>,
    pub kind: ErrorKind<TExtTokenKind>,
}

impl<TExtTokenKind: Default + Sized + Clone + PartialEq> fmt::Debug for Error<TExtTokenKind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error").finish()
    }
}

#[derive(Clone, Debug)]
pub enum ErrorKind<TExtTokenKind: PartialEq> {
    ExpectedAtom,
    ExpectedIdent,
    ExpectedType,
    ExpectedPattern,
    ExpectedToken(ExtTokenKind<TExtTokenKind>),
    UnboundTypeVar,
    Unknown,
    Eof,
}
impl<'s, TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default, TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default, TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
         TLE: Clone + LexerExtension<TExtTokenKind>,
         TPE: ParserExtension<TExtTokenKind, TExtKind>> ExtParser<'s, TExtPat, TExtKind, TExtTokenKind, TLE, TPE> {
    /// Create a new [`Parser`] for the input `&str`
    pub fn ext_new(platform_bindings: &'s PlatformBindings, input: &'s str, lexer_ext: TLE, parser_ext: TPE) -> ExtParser<'s, TExtPat, TExtKind, TExtTokenKind, TLE, TPE> {
        let mut p = ExtParser {
            tmvar: DeBruijnIndexer::default(),
            tyvar: DeBruijnIndexer::default(),
            diagnostic: Diagnostic::new(input),
            lexer: ExtLexer::new(input.chars(), lexer_ext.clone()),
            span: Span::default(),
            token: ExtToken::dummy(),
            platform_bindings: platform_bindings.clone(),
            extension: parser_ext,
            _kind: TExtKind::default(),
            _pat: TExtPat::default(),
        };
        p.bump();
        p
    }

    pub fn diagnostic(self) -> Diagnostic<'s> {
        self.diagnostic
    }
}

impl<'s, TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default, TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
         TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
         TLE: Clone + LexerExtension<TExtTokenKind>, TPE: ParserExtension<TExtTokenKind, TExtKind>> ExtParser<'s, TExtPat, TExtKind, TExtTokenKind, TLE, TPE> where TLE : LexerExtension<TExtTokenKind> {
    /// Kleene Plus combinator
    fn once_or_more<T, F>(&mut self, func: F, delimiter: ExtTokenKind<TExtTokenKind>) -> Result<Vec<T>, Error<TExtTokenKind>>
    where
        F: Fn(&mut ExtParser<TExtPat, TExtKind, TExtTokenKind, TLE, TPE>) -> Result<T, Error<TExtTokenKind>>,
    {
        let mut v = vec![func(self)?];
        while self.bump_if(&delimiter) {
            v.push(func(self)?);
        }
        Ok(v)
    }

    /// Expect combinator
    /// Combinator that must return Ok or a message will be pushed to
    /// diagnostic. This method should only be called after a token has
    /// already been bumped.
    fn once<T, F>(&mut self, func: F, message: &str) -> Result<T, Error<TExtTokenKind>>
    where
        F: Fn(&mut ExtParser<TExtPat, TExtKind, TExtTokenKind, TLE, TPE>) -> Result<T, Error<TExtTokenKind>>,
    {
        match func(self) {
            Ok(t) => Ok(t),
            Err(e) => {
                self.diagnostic.push(message, self.span);
                Err(e)
            }
        }
    }
}

impl<'s, TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
         TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
         TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
         TLE: Clone + LexerExtension<TExtTokenKind>,
         TPE: ParserExtension<TExtTokenKind, TExtKind>> ExtParser<'s, TExtPat, TExtKind, TExtTokenKind, TLE, TPE> where TLE : LexerExtension<TExtTokenKind> {
    fn error<T>(&self, kind: ErrorKind<TExtTokenKind>) -> Result<T, Error<TExtTokenKind>> {
        Err(Error {
            span: self.token.span,
            tok: self.token.clone(),
            kind,
        })
    }

    fn bump(&mut self) -> ExtTokenKind<TExtTokenKind> {
        let prev = std::mem::replace(&mut self.token, self.lexer.lex());
        self.span = prev.span;
        prev.kind
    }

    fn bump_if(&mut self, kind: &ExtTokenKind<TExtTokenKind>) -> bool {
        if &self.token.kind == kind {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: ExtTokenKind<TExtTokenKind>) -> Result<(), Error<TExtTokenKind>> {
        if self.token.kind == kind {
            self.bump();
            Ok(())
        } else {
            self.diagnostic.push(
                format!("expected token {:?}, found {:?}", kind, self.token.kind),
                self.span,
            );
            self.error(ErrorKind::ExpectedToken(kind))
        }
    }

    fn kind(&self) -> &ExtTokenKind<TExtTokenKind> {
        &self.token.kind
    }

    fn ty_variant(&mut self) -> Result<Variant, Error<TExtTokenKind>> {
        let label = self.uppercase_id()?;
        let ty = match self.ty() {
            Ok(ty) => ty,
            _ => Type::Unit,
        };

        Ok(Variant { label, ty })
    }

    fn ty_app(&mut self) -> Result<Type, Error<TExtTokenKind>> {
        if !self.bump_if(&ExtTokenKind::LSquare) {
            return self.error(ErrorKind::ExpectedToken(ExtTokenKind::LSquare));
        }
        let ty = self.ty()?;
        self.expect(ExtTokenKind::RSquare)?;
        Ok(ty)
    }

    fn ty_atom(&mut self) -> Result<Type, Error<TExtTokenKind>> {
        match self.kind() {
            ExtTokenKind::Tag(s) => {
                let v = s.to_owned();
                self.bump();
                Ok(Type::Tag(v))
            }
            ExtTokenKind::TyBool => {
                self.bump();
                Ok(Type::Bool)
            }
            ExtTokenKind::TyNat => {
                self.bump();
                Ok(Type::Nat)
            }
            ExtTokenKind::TyUnit => {
                self.bump();
                Ok(Type::Unit)
            }
            ExtTokenKind::LParen => {
                self.bump();
                let r = self.ty()?;
                self.expect(ExtTokenKind::RParen)?;
                Ok(r)
            }
            ExtTokenKind::Forall => {
                self.bump();
                Ok(Type::Universal(Box::new(self.ty()?)))
            }
            ExtTokenKind::Exists => {
                self.bump();
                let tvar = self.uppercase_id()?;
                self.expect(ExtTokenKind::Proj)?;
                self.tyvar.push(tvar);
                let xs = Type::Existential(Box::new(self.ty()?));
                self.tyvar.pop();
                Ok(xs)
            }
            ExtTokenKind::Uppercase(_) => {
                let ty = self.uppercase_id()?;
                match self.tyvar.lookup(&ty) {
                    Some(idx) => Ok(Type::Var(idx)),
                    None => Ok(Type::Alias(ty)),
                }
            }
            ExtTokenKind::LBrace => {
                self.bump();
                let fields = self.once_or_more(|p| p.ty_variant(), ExtTokenKind::Bar)?;
                self.expect(ExtTokenKind::RBrace)?;
                Ok(Type::Variant(fields))
            }
            _ => self.error(ErrorKind::ExpectedType),
        }
    }

    fn ty_tuple(&mut self) -> Result<Type, Error<TExtTokenKind>> {
        if self.bump_if(&ExtTokenKind::LParen) {
            let mut v = self.once_or_more(|p| p.ty(), ExtTokenKind::Comma)?;
            self.expect(ExtTokenKind::RParen)?;

            if v.len() > 1 {
                Ok(Type::Product(v))
            } else {
                Ok(v.remove(0))
            }
        } else {
            self.ty_atom()
        }
    }

    pub fn ty(&mut self) -> Result<Type, Error<TExtTokenKind>> {
        /*
        match self.kind() {
            ExtTokenKind::AnyTag => return Ok(Type::AnyTag),
            ExtTokenKind::Tag(s) => return Ok(Type::Tag(s.to_owned())),
            _ => {}
        };
        */
        if self.bump_if(&ExtTokenKind::Rec) {
            let name = self.uppercase_id()?;
            self.expect(ExtTokenKind::Equals)?;
            self.tyvar.push(name);
            let ty = self.ty()?;
            self.tyvar.pop();
            return Ok(Type::Rec(Box::new(ty)));
        }

        let mut lhs = self.ty_tuple()?;
        if let ExtTokenKind::TyArrow = self.kind() {
            self.bump();
            while let Ok(rhs) = self.ty() {
                lhs = Type::Arrow(Box::new(lhs), Box::new(rhs));
                if let ExtTokenKind::TyArrow = self.kind() {
                    self.bump();
                } else {
                    break;
                }
            }
        }
        Ok(lhs)
    }

    fn tyabs(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        let tyvar = self.uppercase_id()?;
        let sp = self.span;
        let ty = Box::new(Type::Var(self.tyvar.push(tyvar)));
        let body = self.once(|p| p.parse(), "abstraction body required")?;
        Ok(ExtTerm::new(ExtKind::TyAbs(Box::new(body)), sp + self.span))
    }

    fn tmabs(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        let tmvar = self.lowercase_id()?;
        let sp = self.span;
        self.tmvar.push(tmvar);

        self.expect(ExtTokenKind::Colon)?;
        let ty = self.once(|p| p.ty(), "type annotation required in abstraction")?;
        self.expect(ExtTokenKind::Proj)?;
        let body = self.once(|p| p.parse(), "abstraction body required")?;
        self.tmvar.pop();
        Ok(ExtTerm::new(ExtKind::Abs(Box::new(ty), Box::new(body)), sp + self.span))
    }

    fn fold(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        self.expect(ExtTokenKind::Fold)?;
        let sp = self.span;
        let ty = self.once(|p| p.ty(), "type annotation required after `fold`")?;
        let tm = self.once(|p| p.parse(), "term required after `fold`")?;
        Ok(ExtTerm::new(ExtKind::Fold(Box::new(ty), Box::new(tm)), sp + self.span))
    }

    fn unfold(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        self.expect(ExtTokenKind::Unfold)?;
        let sp = self.span;
        let ty = self.once(|p| p.ty(), "type annotation required after `unfold`")?;
        let tm = self.once(|p| p.parse(), "term required after `unfold`")?;
        Ok(ExtTerm::new(ExtKind::Unfold(Box::new(ty), Box::new(tm)), sp + self.span))
    }

    fn fix(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        let sp = self.span;
        self.expect(ExtTokenKind::Fix)?;
        let t = self.parse()?;
        Ok(ExtTerm::new(ExtKind::Fix(Box::new(t)), sp + self.span))
    }

    fn letexpr(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        let sp = self.span;
        self.expect(ExtTokenKind::Let)?;
        let mut pat = self.once(|p| p.pattern(), "missing pattern")?;

        self.expect(ExtTokenKind::Equals)?;

        let t1 = self.once(|p| p.parse(), "let binder required")?;
        let len = self.tmvar.len();
        for var in PatVarStack::collect(&mut pat).into_iter().rev() {
            self.tmvar.push(var);
        }
        self.expect(ExtTokenKind::In)?;
        let t2 = self.once(|p| p.parse(), "let body required")?;
        while self.tmvar.len() > len {
            self.tmvar.pop();
        }
        Ok(ExtTerm::new(
            ExtKind::Let(Box::new(pat), Box::new(t1), Box::new(t2)),
            sp + self.span,
        ))
    }

    fn lambda(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        self.expect(ExtTokenKind::Lambda)?;
        match self.kind() {
            ExtTokenKind::Uppercase(_) => self.tyabs(),
            ExtTokenKind::Lowercase(_) => self.tmabs(),
            _ => {
                self.diagnostic
                    .push("expected identifier after lambda, found".to_string(), self.span);
                self.error(ErrorKind::ExpectedIdent)
            }
        }
    }

    fn paren(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        self.expect(ExtTokenKind::LParen)?;
        let span = self.span;
        let mut n = self.once_or_more(|p| p.parse(), ExtTokenKind::Comma)?;
        self.expect(ExtTokenKind::RParen)?;
        if n.len() > 1 {
            Ok(ExtTerm::new(ExtKind::Product(n), span + self.span))
        } else {
            // invariant, n.len() >= 1
            Ok(n.remove(0))
        }
    }

    fn uppercase_id(&mut self) -> Result<String, Error<TExtTokenKind>> {
        match self.bump() {
            ExtTokenKind::Uppercase(s) => Ok(s),
            tk => {
                self.diagnostic
                    .push(format!("expected uppercase identifier, found {:?}", tk), self.span);
                self.error(ErrorKind::ExpectedIdent)
            }
        }
    }

    fn lowercase_id(&mut self) -> Result<String, Error<TExtTokenKind>> {
        match self.bump() {
            ExtTokenKind::Lowercase(s) => Ok(s),
            tk => {
                self.diagnostic
                    .push(format!("expected lowercase identifier, found {:?}", tk), self.span);
                self.error(ErrorKind::ExpectedIdent)
            }
        }
    }

    fn literal(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        let lit = match self.bump() {
            ExtTokenKind::Nat(x) => Literal::Nat(x),
            ExtTokenKind::True => Literal::Bool(true),
            ExtTokenKind::False => Literal::Bool(false),
            ExtTokenKind::Unit => Literal::Unit,
            ExtTokenKind::Tag(s) => Literal::Tag(s),
            _ => return self.error(ErrorKind::Unknown),
        };
        Ok(ExtTerm::new(ExtKind::Lit(lit), self.span))
    }

    fn primitive(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        let p = match self.bump() {
            ExtTokenKind::IsZero => Primitive::IsZero,
            ExtTokenKind::Succ => Primitive::Succ,
            ExtTokenKind::Pred => Primitive::Pred,
            _ => return self.error(ErrorKind::Unknown),
        };
        Ok(ExtTerm::new(ExtKind::Primitive(p), self.span))
    }

    /// Important to note that this function can push variable names to the
    /// de Bruijn naming context. Callers of this function are responsible for
    /// making sure that the stack is balanced afterwards
    fn pat_atom(&mut self) -> Result<ExtPattern<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        match self.kind() {
            ExtTokenKind::LParen => self.pattern(),
            ExtTokenKind::Wildcard => {
                self.bump();
                Ok(ExtPattern::Any)
            }
            ExtTokenKind::Uppercase(_) => {
                let tycon = self.uppercase_id()?;
                let inner = match self.pattern() {
                    Ok(pat) => pat,
                    _ => ExtPattern::Any,
                };
                Ok(ExtPattern::Constructor(tycon, Box::new(inner)))
            }
            ExtTokenKind::Lowercase(_) => {
                let var = self.lowercase_id()?;
                // self.tmvar.push(var.clone());
                Ok(ExtPattern::Variable(var))
            }
            ExtTokenKind::True => {
                self.bump();
                Ok(ExtPattern::Literal(Literal::Bool(true)))
            }
            ExtTokenKind::False => {
                self.bump();
                Ok(ExtPattern::Literal(Literal::Bool(false)))
            }
            ExtTokenKind::Unit => {
                self.bump();
                Ok(ExtPattern::Literal(Literal::Unit))
            }
            ExtTokenKind::Nat(n) => {
                // O great borrowck, may this humble offering appease thee
                let n = *n;
                self.bump();
                Ok(ExtPattern::Literal(Literal::Nat(n)))
            }
            ExtTokenKind::Tag(s) => {
                let s = s.clone();
                self.bump();
                Ok(ExtPattern::Literal(Literal::Tag(s)))
            }
            _ => self.error(ErrorKind::ExpectedPattern),
        }
    }

    fn pattern(&mut self) -> Result<ExtPattern<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        match self.kind() {
            ExtTokenKind::LParen => {
                self.bump();
                let mut v = self.once_or_more(|p| p.pat_atom(), ExtTokenKind::Comma)?;
                self.expect(ExtTokenKind::RParen)?;
                if v.len() > 1 {
                    Ok(ExtPattern::Product(v))
                } else {
                    // v must have length == 1, else we would have early returned
                    assert_eq!(v.len(), 1);
                    Ok(v.remove(0))
                }
            }
            _ => self.pat_atom(),
        }
    }

    fn case_arm(&mut self) -> Result<Arm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        // match self.kind() {
        //     ExtTokenKind::Bar => self.bump(),
        //     _ => return self.error(ErrorKind::ExpectedToken(ExtTokenKind::Bar)),
        // };

        // We don't track the length of the debruijn index in other methods,
        // but we have a couple branches where variables might be bound,
        // and this is pretty much the easiest way of doing it

        let len = self.tmvar.len();
        let mut span = self.span;

        let mut pat = self.once(|p| p.pattern(), "missing pattern")?;

        for var in PatVarStack::collect(&mut pat).into_iter().rev() {
            self.tmvar.push(var);
        }

        self.expect(ExtTokenKind::Equals)?;
        self.expect(ExtTokenKind::Gt)?;

        let term = Box::new(self.once(|p| p.application(), "missing case term")?);

        self.bump_if(&ExtTokenKind::Comma);

        // Unbind any variables from the parsing context
        while self.tmvar.len() > len {
            self.tmvar.pop();
        }

        span = span + self.span;

        Ok(Arm { span, pat, term })
    }

    fn case(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        self.expect(ExtTokenKind::Case)?;
        let span = self.span;
        let expr = self.once(|p| p.parse(), "missing case expression")?;
        self.expect(ExtTokenKind::Of)?;

        self.bump_if(&ExtTokenKind::Bar);
        let arms = self.once_or_more(|p| p.case_arm(), ExtTokenKind::Bar)?;

        Ok(ExtTerm::new(ExtKind::Case(Box::new(expr), arms), span + self.span))
    }

    fn injection(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        let label = self.uppercase_id()?;
        let sp = self.span;
        let term = match self.parse() {
            Ok(t) => t,
            _ => ExtTerm::new(ExtKind::Lit(Literal::Unit), self.span),
        };

        self.expect(ExtTokenKind::Of)?;
        let ty = self.ty()?;
        Ok(ExtTerm::new(
            ExtKind::Injection(label, Box::new(term), Box::new(ty)),
            sp + self.span,
        ))
    }

    fn pack(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        self.expect(ExtTokenKind::Pack)?;
        let sp = self.span;
        let witness = self.ty()?;
        self.expect(ExtTokenKind::Comma)?;
        let evidence = self.parse()?;
        self.expect(ExtTokenKind::As)?;
        let signature = self.ty()?;

        Ok(ExtTerm::new(
            ExtKind::Pack(Box::new(witness), Box::new(evidence), Box::new(signature)),
            sp + self.span,
        ))
    }

    fn unpack(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        self.expect(ExtTokenKind::Unpack)?;
        let sp = self.span;
        let package = self.parse()?;
        self.expect(ExtTokenKind::As)?;

        let tyvar = self.uppercase_id()?;
        self.expect(ExtTokenKind::Comma)?;
        let name = self.lowercase_id()?;
        self.tyvar.push(tyvar);
        self.tmvar.push(name);
        self.expect(ExtTokenKind::In)?;
        let expr = self.parse()?;
        self.tmvar.pop();
        self.tyvar.pop();
        Ok(ExtTerm::new(
            ExtKind::Unpack(Box::new(package), Box::new(expr)),
            sp + self.span,
        ))
    }

    fn atom(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        match self.kind() {
            ExtTokenKind::LParen => self.paren(),
            ExtTokenKind::Fix => self.fix(),
            ExtTokenKind::Fold => self.fold(),
            ExtTokenKind::Unfold => self.unfold(),
            ExtTokenKind::Pack => self.pack(),
            ExtTokenKind::Unpack => self.unpack(),
            ExtTokenKind::IsZero | ExtTokenKind::Succ | ExtTokenKind::Pred => self.primitive(),
            ExtTokenKind::Uppercase(_) => self.injection(),
            ExtTokenKind::Lowercase(s) => {
                let var = self.lowercase_id()?;
                match self.tmvar.lookup(&var) {
                    Some(idx) => Ok(ExtTerm::new(ExtKind::Var(idx), self.span)),
                    None => {
                        match self.platform_bindings.has(&var) {
                            Some(idx) => {return Ok(ExtTerm::new(ExtKind::PlatformBinding(idx), self.span));},
                            None => {}
                        }
                        self.diagnostic.push(format!("parser: unbound variable {}", var), self.span);
                        self.error(ErrorKind::UnboundTypeVar)
                    }
                }
            }
            ExtTokenKind::Nat(_) | ExtTokenKind::True | ExtTokenKind::False | ExtTokenKind::Unit
            | ExtTokenKind::Tag(_) => self.literal(),
            ExtTokenKind::Eof => self.error(ErrorKind::Eof),
            ExtTokenKind::Semicolon => {
                self.bump();
                self.error(ErrorKind::ExpectedAtom)
            }
            _ => self.error(ErrorKind::ExpectedAtom),
        }
    }

    /// Parse a term of form:
    /// projection = atom `.` projection
    /// projection = atom
    fn projection(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        let atom = self.atom()?;
        if self.bump_if(&ExtTokenKind::Proj) {
            let idx = match self.bump() {
                ExtTokenKind::Nat(idx) => idx,
                _ => {
                    self.diagnostic
                        .push(format!("expected integer index after {}", atom), self.span);
                    return self.error(ErrorKind::ExpectedToken(ExtTokenKind::Proj));
                }
            };
            let sp = atom.span + self.span;
            Ok(ExtTerm::new(ExtKind::Projection(Box::new(atom), idx as usize), sp))
        } else {
            Ok(atom)
        }
    }

    /// Parse an application of form:
    /// application = atom application' | atom
    /// application' = atom application' | empty
    fn application(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        let mut app = self.projection()?;

        loop {
            let sp = app.span;
            if let Ok(ty) = self.ty_app() {
                // Full type inference for System F is undecidable
                // Additionally, even partial type reconstruction,
                // where only type application types are erased is also
                // undecidable, see TaPL 23.6.2, Boehm 1985, 1989
                //
                // Partial erasure rules:
                // erasep(x) = x
                // erasep(λx:T. t) = λx:T. erasep(t)
                // erasep(t1 t2) = erasep(t1) erasep(t2)
                // erasep(λX. t) = λX. erasep(t)
                // erasep(t T) = erasep(t) []      <--- erasure of TyApp
                app = ExtTerm::new(ExtKind::TyApp(Box::new(app), Box::new(ty)), sp + self.span);
            } else if let Ok(term) = self.projection() {
                app = ExtTerm::new(ExtKind::App(Box::new(app), Box::new(term)), sp + self.span);
            } else {
                break;
            }
        }
        Ok(app)
    }

    pub fn parse(&mut self) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>> {
        match self.kind() {
            ExtTokenKind::Case => self.case(),
            ExtTokenKind::Lambda => self.lambda(),
            ExtTokenKind::Let => self.letexpr(),
            _ => self.application(),
        }
    }
}

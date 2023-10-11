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
use super::debruijn::DeBruijnIndexer;
use super::error::Error;
use super::lexer::ExtLexer;
use super::{ExtToken, ExtTokenKind};
use crate::bottom::{BottomExtension, BottomKind, BottomPattern, BottomState, BottomTokenKind, BottomType};
use crate::extensions::SystemRExtension;

use crate::system_r_util::diagnostic::Diagnostic;
use crate::system_r_util::span::*;
use core::fmt;
use std::hash;

use crate::patterns::{ExtPattern, PatVarStack};
use crate::platform_bindings::PlatformBindings;
use crate::terms::*;
use crate::types::*;

pub type Parser<'s> = ParserState<'s, BottomTokenKind, BottomKind, BottomPattern, BottomType, BottomState>;

#[derive(Default, Debug, Clone)]
pub struct ParserState<
    's,
    TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Sized + Clone,
    TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Sized + Clone,
    TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Sized + Clone,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
> {
    pub tmvar: DeBruijnIndexer,
    pub tyvar: DeBruijnIndexer,
    pub diagnostic: Diagnostic<'s>,
    pub lexer: ExtLexer<'s, TExtTokenKind, TExtKind, TExtPat>,
    pub span: Span,
    pub token: ExtToken<TExtTokenKind>,
    pub platform_bindings: PlatformBindings,
    pub ext_state: TExtState,
    pub ty: TExtType,
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
    ExtendedError(String),
}

impl<
        's,
        TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Sized + Clone,
        TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Sized + Clone,
        TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Sized + Clone,
        TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
        TExtState: fmt::Debug + Default + Clone,
    > ParserState<'s, TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>
{
    pub fn die(self) -> String {
        let d = self.diagnostic;
        d.emit()
    }
}

pub fn new<'s, TExtType: Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash>(
    platform_bindings: &'s PlatformBindings,
    input: &'s str,
) -> Parser<'s> {
    let mut ext = BottomExtension;
    ext_new(platform_bindings, input, &mut ext)
}

pub fn ext_new<
    's,
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    platform_bindings: &'s PlatformBindings,
    input: &'s str,
    ext: &mut TExt,
) -> ParserState<'s, TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState> {
    let mut p = ParserState {
        tmvar: DeBruijnIndexer::default(),
        tyvar: DeBruijnIndexer::default(),
        diagnostic: Diagnostic::new(input),
        lexer: ExtLexer::new(input.chars()),
        span: Span::default(),
        token: ExtToken::dummy(),
        platform_bindings: platform_bindings.clone(),
        ext_state: Default::default(),
        ty: Default::default(),
    };
    bump(&mut p, ext);
    p
}

/// Kleene Plus combinator
pub fn once_or_more<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: Clone + Default + fmt::Debug,
    F: FnMut(&mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>) -> Result<T, Error<TExtTokenKind>>,
    TExt: Clone + Copy + Default + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    T,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    mut func: F,
    delimiter: ExtTokenKind<TExtTokenKind>,
    ext: &mut TExt,
) -> Result<Vec<T>, Error<TExtTokenKind>> {
    let mut v = vec![func(ps)?];
    while bump_if(ps, ext, &delimiter) {
        v.push(func(ps)?);
    }
    Ok(v)
}

/// Expect combinator
/// Combinator that must return Ok or a message will be pushed to
/// diagnostic. This method should only be called after a token has
/// already been bumped.
pub fn once<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: Clone + Default + fmt::Debug,
    F: FnMut(&mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>) -> Result<T, Error<TExtTokenKind>>,
    T,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    mut func: F,
    message: &str,
) -> Result<T, Error<TExtTokenKind>> {
    match func(ps) {
        Err(e) => {
            ps.diagnostic.push(message, ps.span);
            Err(e)
        }
        t => t,
    }
}

pub fn diagnostic<
    's,
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtState: fmt::Debug + Default + Clone,
>(
    ps: ParserState<'s, TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
) -> Diagnostic<'s> {
    ps.diagnostic
}

pub fn error<
    's,
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TError,
>(
    ps: &ParserState<'s, TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    kind: ErrorKind<TExtTokenKind>,
) -> Result<TError, Error<TExtTokenKind>> {
    Err(Error {
        span: ps.token.span,
        tok: ps.token.clone(),
        kind,
    })
}

pub fn bump<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> ExtTokenKind<TExtTokenKind> {
    let prev = std::mem::replace(&mut ps.token, ps.lexer.lex(ext));
    ps.span = prev.span;
    prev.kind
}

pub fn bump_if<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
    kind: &ExtTokenKind<TExtTokenKind>,
) -> bool {
    if &ps.token.kind == kind {
        bump(ps, ext);
        true
    } else {
        false
    }
}

pub fn expect<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
    kind: ExtTokenKind<TExtTokenKind>,
) -> Result<(), Error<TExtTokenKind>> {
    if ps.token.kind == kind {
        bump(ps, ext);
        Ok(())
    } else {
        ps.diagnostic
            .push(format!("expected token {:?}, found {:?}", kind, ps.token.kind), ps.span);
        error(ps, ErrorKind::ExpectedToken(kind))
    }
}

pub fn kind<
    's,
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
>(
    ps: &'s ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
) -> &'s ExtTokenKind<TExtTokenKind> {
    &ps.token.kind
}

pub fn ty_variant<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<Variant<TExtType>, Error<TExtTokenKind>> {
    let label = uppercase_id(ps, ext)?;
    let ty = match ty(ps, ext) {
        Ok(ty) => ty,
        _ => Type::Unit,
    };

    Ok(Variant { label, ty })
}

pub fn ty_app<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<Type<TExtType>, Error<TExtTokenKind>> {
    if !bump_if(ps, ext, &ExtTokenKind::LSquare) {
        return error(ps, ErrorKind::ExpectedToken(ExtTokenKind::LSquare));
    }
    let ty = ty(ps, ext)?;
    expect(ps, ext, ExtTokenKind::RSquare)?;
    Ok(ty)
}

pub fn ty_atom<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<Type<TExtType>, Error<TExtTokenKind>> {
    match kind(ps) {
        ExtTokenKind::Tag(s) => {
            let v = s.to_owned();
            bump(ps, ext);
            Ok(Type::Tag(v))
        }
        ExtTokenKind::TyBool => {
            bump(ps, ext);
            Ok(Type::Bool)
        }
        ExtTokenKind::TyNat => {
            bump(ps, ext);
            Ok(Type::Nat)
        }
        ExtTokenKind::TyUnit => {
            bump(ps, ext);
            Ok(Type::Unit)
        }
        ExtTokenKind::LParen => {
            bump(ps, ext);
            let r = ty(ps, ext)?;
            expect(ps, ext, ExtTokenKind::RParen)?;
            Ok(r)
        }
        ExtTokenKind::Forall => {
            bump(ps, ext);
            Ok(Type::Universal(Box::new(ty(ps, ext)?)))
        }
        ExtTokenKind::Exists => {
            bump(ps, ext);
            let tvar = uppercase_id(ps, ext)?;
            expect(ps, ext, ExtTokenKind::Proj)?;
            ps.tyvar.push(tvar);
            let xs = Type::Existential(Box::new(ty(ps, ext)?));
            ps.tyvar.pop();
            Ok(xs)
        }
        ExtTokenKind::Uppercase(_) => {
            let ty = uppercase_id(ps, ext)?;
            match ps.tyvar.lookup(&ty) {
                Some(idx) => Ok(Type::Var(idx)),
                None => Ok(Type::Alias(ty)),
            }
        }
        ExtTokenKind::LBrace => {
            bump(ps, ext);
            let mut ext2 = ext.clone();
            let mut ext3 = ext.clone();
            let fields = once_or_more(ps, |p| ty_variant(p, &mut ext2), ExtTokenKind::Bar, &mut ext3)?;
            expect(ps, ext, ExtTokenKind::RBrace)?;
            Ok(Type::Variant(fields))
        }
        v => error(ps, ErrorKind::ExpectedType),
    }
}

pub fn ty_tuple<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<Type<TExtType>, Error<TExtTokenKind>> {
    if bump_if(ps, ext, &ExtTokenKind::LParen) {
        let mut ext2 = ext.clone();
        let mut ext3 = ext.clone();
        let mut v = once_or_more(ps, |p| ty(p, &mut ext2), ExtTokenKind::Comma, &mut ext3)?;
        expect(ps, ext, ExtTokenKind::RParen)?;

        if v.len() > 1 {
            Ok(Type::Product(v))
        } else {
            Ok(v.remove(0))
        }
    } else {
        ty_atom(ps, ext)
    }
}

pub fn ty<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<Type<TExtType>, Error<TExtTokenKind>> {
    /*
    match kind(ps) {
        ExtTokenKind::AnyTag => return Ok(Type::AnyTag),
        ExtTokenKind::Tag(s) => return Ok(Type::Tag(s.to_owned())),
        _ => {}
    };
    */
    if ext.parser_ty_bump_if(ps) {
        return ext.parser_ty(ps);
    }

    if bump_if(ps, ext, &ExtTokenKind::Rec) {
        let name = uppercase_id(ps, ext)?;
        expect(ps, ext, ExtTokenKind::Equals)?;
        ps.tyvar.push(name);
        let ty = ty(ps, ext)?;
        ps.tyvar.pop();
        return Ok(Type::Rec(Box::new(ty)));
    }

    let mut lhs = ty_tuple(ps, ext)?;
    if let ExtTokenKind::TyArrow = kind(ps) {
        bump(ps, ext);
        while let Ok(rhs) = ty(ps, ext) {
            lhs = Type::Arrow(Box::new(lhs), Box::new(rhs));
            if let ExtTokenKind::TyArrow = kind(ps) {
                bump(ps, ext);
            } else {
                break;
            }
        }
    }
    Ok(lhs)
}

pub fn tyabs<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let tyvar = uppercase_id(ps, ext)?;
    let sp = ps.span;
    let ty: Box<Type<TExtType>> = Box::new(Type::Var(ps.tyvar.push(tyvar)));
    let body = once(ps, |p| parse(p, ext), "abstraction body required")?;
    Ok(ExtTerm::new(ExtKind::TyAbs(Box::new(body)), sp + ps.span))
}

pub fn tmabs<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let tmvar = lowercase_id(ps, ext)?;
    let sp = ps.span;
    ps.tmvar.push(tmvar);

    expect(ps, ext, ExtTokenKind::Colon)?;
    let ty = once(ps, |p| ty(p, ext), "type annotation required in abstraction")?;
    expect(ps, ext, ExtTokenKind::Proj)?;
    let body = once(ps, |p| parse(p, ext), "abstraction body required")?;
    ps.tmvar.pop();
    Ok(ExtTerm::new(ExtKind::Abs(Box::new(ty), Box::new(body)), sp + ps.span))
}

pub fn fold<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Fold)?;
    let sp = ps.span;
    let ty = once(ps, |p| ty(p, ext), "type annotation required after `fold`")?;
    let tm = once(ps, |p| parse(p, ext), "term required after `fold`")?;
    Ok(ExtTerm::new(ExtKind::Fold(Box::new(ty), Box::new(tm)), sp + ps.span))
}

pub fn unfold<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Unfold)?;
    let sp = ps.span;
    let ty = once(ps, |p| ty(p, ext), "type annotation required after `unfold`")?;
    let tm = once(ps, |p| parse(p, ext), "term required after `unfold`")?;
    Ok(ExtTerm::new(ExtKind::Unfold(Box::new(ty), Box::new(tm)), sp + ps.span))
}

pub fn fix<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let sp = ps.span;
    expect(ps, ext, ExtTokenKind::Fix)?;
    let t = parse(ps, ext)?;
    Ok(ExtTerm::new(ExtKind::Fix(Box::new(t)), sp + ps.span))
}

pub fn letexpr<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let sp = ps.span;
    expect(ps, ext, ExtTokenKind::Let)?;
    let pat = once(ps, |p| pattern(p, ext), "missing pattern")?;

    expect(ps, ext, ExtTokenKind::Equals)?;

    let t1 = once(ps, |p| parse(p, ext), "let binder required")?;
    let len = ps.tmvar.len();
    for var in PatVarStack::<TExtPat, TExtKind>::collect(&pat).into_iter().rev() {
        ps.tmvar.push(var);
    }
    expect(ps, ext, ExtTokenKind::In)?;
    let t2 = once(ps, |p| parse(p, ext), "let body required")?;
    while ps.tmvar.len() > len {
        ps.tmvar.pop();
    }
    Ok(ExtTerm::new(
        ExtKind::Let(Box::new(pat), Box::new(t1), Box::new(t2)),
        sp + ps.span,
    ))
}

pub fn lambda<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Lambda)?;
    match kind(ps) {
        ExtTokenKind::Uppercase(_) => tyabs(ps, ext),
        ExtTokenKind::Lowercase(_) => tmabs(ps, ext),
        _ => {
            ps.diagnostic
                .push("expected identifier after lambda, found".to_string(), ps.span);
            error(ps, ErrorKind::ExpectedIdent)
        }
    }
}

pub fn paren<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::LParen)?;
    let span = ps.span;

    let mut ext2 = ext.clone();
    let mut ext3 = ext.clone();
    let mut n = once_or_more(ps, |p| parse(p, &mut ext2), ExtTokenKind::Comma, &mut ext3)?;
    expect(ps, ext, ExtTokenKind::RParen)?;
    if n.len() > 1 {
        Ok(ExtTerm::new(ExtKind::Product(n), span + ps.span))
    } else {
        // invariant, n.len() >= 1
        Ok(n.remove(0))
    }
}

pub fn uppercase_id<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<String, Error<TExtTokenKind>> {
    match bump(ps, ext) {
        ExtTokenKind::Uppercase(s) => Ok(s),
        tk => {
            ps.diagnostic
                .push(format!("expected uppercase identifier, found {:?}", tk), ps.span);
            error(ps, ErrorKind::ExpectedIdent)
        }
    }
}

pub fn lowercase_id<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<String, Error<TExtTokenKind>> {
    match bump(ps, ext) {
        ExtTokenKind::Lowercase(s) => Ok(s),
        tk => {
            ps.diagnostic
                .push(format!("expected lowercase identifier, found {:?}", tk), ps.span);
            error(ps, ErrorKind::ExpectedIdent)
        }
    }
}

pub fn literal<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let lit = match bump(ps, ext) {
        ExtTokenKind::Nat(x) => Literal::Nat(x),
        ExtTokenKind::True => Literal::Bool(true),
        ExtTokenKind::False => Literal::Bool(false),
        ExtTokenKind::Unit => Literal::Unit,
        ExtTokenKind::Tag(s) => Literal::Tag(s),
        _ => return error(ps, ErrorKind::Unknown),
    };
    Ok(ExtTerm::new(ExtKind::Lit(lit), ps.span))
}

pub fn primitive<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let p = match bump(ps, ext) {
        ExtTokenKind::IsZero => Primitive::IsZero,
        ExtTokenKind::Succ => Primitive::Succ,
        ExtTokenKind::Pred => Primitive::Pred,
        _ => return error(ps, ErrorKind::Unknown),
    };
    Ok(ExtTerm::new(ExtKind::Primitive(p), ps.span))
}

/// Important to note that this function can push variable names to the
/// de Bruijn naming context. Callers of this function are responsible for
/// making sure that the stack is balanced afterwards
pub fn pat_atom<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtPattern<TExtPat>, Error<TExtTokenKind>> {
    match kind(ps) {
        ExtTokenKind::LParen => pattern(ps, ext),
        ExtTokenKind::Wildcard => {
            bump(ps, ext);
            Ok(ExtPattern::Any)
        }
        ExtTokenKind::Uppercase(_) => {
            let tycon = uppercase_id(ps, ext)?;
            let inner = match pattern(ps, ext) {
                Ok(pat) => pat,
                _ => ExtPattern::Any,
            };
            Ok(ExtPattern::Constructor(tycon, Box::new(inner)))
        }
        ExtTokenKind::Lowercase(_) => {
            let var = lowercase_id(ps, ext)?;
            // ps.tmvar.push(var.clone());
            Ok(ExtPattern::Variable(var))
        }
        ExtTokenKind::True => {
            bump(ps, ext);
            Ok(ExtPattern::Literal(Literal::Bool(true)))
        }
        ExtTokenKind::False => {
            bump(ps, ext);
            Ok(ExtPattern::Literal(Literal::Bool(false)))
        }
        ExtTokenKind::Unit => {
            bump(ps, ext);
            Ok(ExtPattern::Literal(Literal::Unit))
        }
        ExtTokenKind::Nat(n) => {
            // O great borrowck, may this humble offering appease thee
            let n = *n;
            bump(ps, ext);
            Ok(ExtPattern::Literal(Literal::Nat(n)))
        }
        ExtTokenKind::Tag(s) => {
            let s = s.clone();
            bump(ps, ext);
            Ok(ExtPattern::Literal(Literal::Tag(s)))
        }
        _ => error(ps, ErrorKind::ExpectedPattern),
    }
}

pub fn pattern<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtPattern<TExtPat>, Error<TExtTokenKind>> {
    match kind(ps) {
        ExtTokenKind::LParen => {
            bump(ps, ext);
            let mut ext2 = ext.clone();
            let mut ext3 = ext.clone();
            let mut v = once_or_more(ps, |p| pat_atom(p, &mut ext2), ExtTokenKind::Comma, &mut ext3)?;
            expect(ps, ext, ExtTokenKind::RParen)?;
            if v.len() > 1 {
                Ok(ExtPattern::Product(v))
            } else {
                // v must have length == 1, else we would have early returned
                assert_eq!(v.len(), 1);
                Ok(v.remove(0))
            }
        }
        _ => pat_atom(ps, ext),
    }
}

pub fn case_arm<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<Arm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    // match kind(ps) {
    //     ExtTokenKind::Bar => bump(ps),
    //     _ => return error(ps, ErrorKind::ExpectedToken(ExtTokenKind::Bar)),
    // };

    // We don't track the length of the debruijn index in other methods,
    // but we have a couple branches where variables might be bound,
    // and this is pretty much the easiest way of doing it

    let len = ps.tmvar.len();
    let mut span = ps.span;

    let pat = once(ps, |p| pattern(p, ext), "missing pattern")?;

    for var in PatVarStack::<TExtPat, TExtKind>::collect(&pat).into_iter().rev() {
        ps.tmvar.push(var);
    }

    expect(ps, ext, ExtTokenKind::Equals)?;
    expect(ps, ext, ExtTokenKind::Gt)?;

    let term = Box::new(once(ps, |p| application(p, ext), "missing case term")?);

    bump_if(ps, ext, &ExtTokenKind::Comma);

    // Unbind any variables from the parsing context
    while ps.tmvar.len() > len {
        ps.tmvar.pop();
    }

    span += ps.span;

    Ok(Arm {
        span,
        pat,
        term,
        _kind: Default::default(),
    })
}

pub fn case<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Case)?;
    let span = ps.span;
    let expr = once(ps, |p| parse(p, ext), "missing case expression")?;
    expect(ps, ext, ExtTokenKind::Of)?;

    bump_if(ps, ext, &ExtTokenKind::Bar);

    let mut ext2 = ext.clone();
    let mut ext3 = ext.clone();
    let arms = once_or_more(ps, |p| case_arm(p, &mut ext3), ExtTokenKind::Bar, &mut ext2)?;

    Ok(ExtTerm::new(ExtKind::Case(Box::new(expr), arms), span + ps.span))
}

pub fn injection<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let label = uppercase_id(ps, ext)?;
    let sp = ps.span;
    let term = match parse(ps, ext) {
        Ok(t) => t,
        _ => ExtTerm::new(ExtKind::Lit(Literal::Unit), ps.span),
    };

    expect(ps, ext, ExtTokenKind::Of)?;
    let ty = ty(ps, ext)?;
    Ok(ExtTerm::new(
        ExtKind::Injection(label, Box::new(term), Box::new(ty)),
        sp + ps.span,
    ))
}

pub fn pack<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Pack)?;
    let sp = ps.span;
    let witness = ty(ps, ext)?;
    expect(ps, ext, ExtTokenKind::Comma)?;
    let evidence = parse(ps, ext)?;
    expect(ps, ext, ExtTokenKind::As)?;
    let signature = ty(ps, ext)?;

    Ok(ExtTerm::new(
        ExtKind::Pack(Box::new(witness), Box::new(evidence), Box::new(signature)),
        sp + ps.span,
    ))
}

pub fn unpack<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Unpack)?;
    let sp = ps.span;
    let package = parse(ps, ext)?;
    expect(ps, ext, ExtTokenKind::As)?;

    let tyvar = uppercase_id(ps, ext)?;
    expect(ps, ext, ExtTokenKind::Comma)?;
    let name = lowercase_id(ps, ext)?;
    ps.tyvar.push(tyvar);
    ps.tmvar.push(name);
    expect(ps, ext, ExtTokenKind::In)?;
    let expr = parse(ps, ext)?;
    ps.tmvar.pop();
    ps.tyvar.pop();
    Ok(ExtTerm::new(
        ExtKind::Unpack(Box::new(package), Box::new(expr)),
        sp + ps.span,
    ))
}

pub fn atom<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    match kind(ps) {
        ExtTokenKind::Extended(tk) => ext_atom(ps, ext),
        ExtTokenKind::LParen => paren(ps, ext),
        ExtTokenKind::Fix => fix(ps, ext),
        ExtTokenKind::Fold => fold(ps, ext),
        ExtTokenKind::Unfold => unfold(ps, ext),
        ExtTokenKind::Pack => pack(ps, ext),
        ExtTokenKind::Unpack => unpack(ps, ext),
        ExtTokenKind::IsZero | ExtTokenKind::Succ | ExtTokenKind::Pred => primitive(ps, ext),
        ExtTokenKind::Uppercase(_) => injection(ps, ext),
        ExtTokenKind::Lowercase(s) => {
            let var = lowercase_id(ps, ext)?;
            match ps.tmvar.lookup(&var) {
                Some(idx) => Ok(ExtTerm::new(ExtKind::Var(idx), ps.span)),
                None => {
                    if let Some(idx) = ps.platform_bindings.has(&var) {
                        return Ok(ExtTerm::new(ExtKind::PlatformBinding(idx), ps.span));
                    }
                    ps.diagnostic.push(format!("parser: unbound variable {}", var), ps.span);
                    error(ps, ErrorKind::UnboundTypeVar)
                }
            }
        }
        ExtTokenKind::Nat(_) | ExtTokenKind::True | ExtTokenKind::False | ExtTokenKind::Unit | ExtTokenKind::Tag(_) => {
            literal(ps, ext)
        }
        ExtTokenKind::Eof => error(ps, ErrorKind::Eof),
        ExtTokenKind::Semicolon => {
            bump(ps, ext);
            error(ps, ErrorKind::ExpectedAtom)
        }
        _ => error(ps, ErrorKind::ExpectedAtom),
    }
}

/// Parse a term of form:
/// projection = atom `.` projection
/// projection = atom
pub fn projection<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let atom = atom(ps, ext)?;
    if bump_if(ps, ext, &ExtTokenKind::Proj) {
        let idx = match bump(ps, ext) {
            ExtTokenKind::Nat(idx) => idx,
            _ => {
                ps.diagnostic
                    .push(format!("expected integer index after {}", atom), ps.span);
                return error(ps, ErrorKind::ExpectedToken(ExtTokenKind::Proj));
            }
        };
        let sp = atom.span + ps.span;
        Ok(ExtTerm::new(ExtKind::Projection(Box::new(atom), idx as usize), sp))
    } else {
        Ok(atom)
    }
}

/// Parse an application of form:
/// application = atom application' | atom
/// application' = atom application' | empty
pub fn application<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let mut app = projection(ps, ext)?;

    loop {
        let sp = app.span;
        if let Ok(ty) = ty_app(ps, ext) {
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
            app = ExtTerm::new(ExtKind::TyApp(Box::new(app), Box::new(ty)), sp + ps.span);
        } else if let Ok(term) = projection(ps, ext) {
            app = ExtTerm::new(ExtKind::App(Box::new(app), Box::new(term)), sp + ps.span);
        } else {
            break;
        }
    }
    Ok(app)
}

pub fn ext_atom<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let r = {
        match ext.parser_ext_atom(ps) {
            Err(e) => return Err(e),
            Ok(t) => t,
        }
    };
    return Ok(r);
}

pub fn ext_parse<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    let r = {
        match ext.parser_ext_parse(ps) {
            Err(e) => return Err(e),
            Ok(t) => t,
        }
    };
    return Ok(r);
}

pub fn parse<
    TExtTokenKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Default + Clone,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ps: &mut ParserState<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TExtPat, TExtKind, TExtType>, Error<TExtTokenKind>> {
    match kind(ps).clone() {
        ExtTokenKind::Case => case(ps, ext),
        ExtTokenKind::Lambda => lambda(ps, ext),
        ExtTokenKind::Let => letexpr(ps, ext),
        ExtTokenKind::Extended(tk) if ext.parser_has_ext_parse(&tk) => ext_parse(ps, ext),
        _ => application(ps, ext),
    }
}

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
use core::fmt;
use std::hash;

use crate::patterns::{ExtPattern, PatternCount};
use crate::system_r_util::span::Span;
use crate::terms::{Arm, ExtKind, ExtTerm};
use crate::types::Type;
use crate::visit::{MutTermVisitor, MutTypeVisitor};

pub struct Shift {
    cutoff: usize,
    shift: isize,
}

impl Shift {
    pub const fn new(shift: isize) -> Shift {
        Shift { cutoff: 0, shift }
    }
}

impl<
        TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
        TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
        TExtType: Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    > MutTermVisitor<TExtPat, TExtKind, TExtType> for Shift
{
    fn visit_var(&mut self, sp: &mut Span, var: &mut usize) {
        if *var >= self.cutoff {
            *var = (*var as isize + self.shift) as usize;
        }
    }

    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type<TExtType>, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_let(
        &mut self,
        sp: &mut Span,
        pat: &mut ExtPattern<TExtPat>,
        t1: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        t2: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ) {
        self.visit(t1);
        let c = PatternCount::<TExtPat, TExtKind>::collect(pat);
        self.cutoff += c;
        self.visit(t2);
        self.cutoff -= c;
    }

    fn visit_case(
        &mut self,
        sp: &mut Span,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        arms: &mut Vec<Arm<TExtPat, TExtKind, TExtType>>,
    ) {
        self.visit(term);
        for arm in arms {
            let c = PatternCount::<TExtPat, TExtKind>::collect(&arm.pat);
            self.cutoff += c;
            self.visit(&mut arm.term);
            self.cutoff -= c;
        }
    }

    fn visit_unpack(
        &mut self,
        _: &mut Span,
        package: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ) {
        self.visit(package);
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }
}

pub struct Subst<
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtType: Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
> {
    cutoff: usize,
    term: ExtTerm<TExtPat, TExtKind, TExtType>,
}

impl<
        TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
        TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
        TExtType: Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
    > Subst<TExtPat, TExtKind, TExtType>
{
    pub fn new(term: ExtTerm<TExtPat, TExtKind, TExtType>) -> Subst<TExtPat, TExtKind, TExtType> {
        Subst { cutoff: 0, term }
    }
}

impl<
        TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
        TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
        TExtType: Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
    > MutTermVisitor<TExtPat, TExtKind, TExtType> for Subst<TExtPat, TExtKind, TExtType>
{
    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type<TExtType>, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_let(
        &mut self,
        sp: &mut Span,
        pat: &mut ExtPattern<TExtPat>,
        t1: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        t2: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ) {
        self.visit(t1);
        let c = PatternCount::<TExtPat, TExtKind>::collect(pat);
        self.cutoff += c;
        self.visit(t2);
        self.cutoff -= c;
    }

    fn visit_case(
        &mut self,
        sp: &mut Span,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        arms: &mut Vec<Arm<TExtPat, TExtKind, TExtType>>,
    ) {
        self.visit(term);
        for arm in arms {
            let c = PatternCount::<TExtPat, TExtKind>::collect(&arm.pat);
            self.cutoff += c;
            self.visit(&mut arm.term);
            self.cutoff -= c;
        }
    }

    fn visit_unpack(
        &mut self,
        _: &mut Span,
        package: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ) {
        self.visit(package);
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit(&mut self, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        let sp = &mut term.span;
        match &mut term.kind {
            ExtKind::Var(v) if *v == self.cutoff => {
                Shift::new(self.cutoff as isize).visit(&mut self.term);
                *term = self.term.clone();
            }
            _ => self.walk(term),
        }
    }
}

pub struct TyTermSubst<TExtType: Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash> {
    cutoff: usize,
    ty: Type<TExtType>,
}

impl<TExtType: Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash> TyTermSubst<TExtType> {
    pub fn new(ty: Type<TExtType>) -> TyTermSubst<TExtType> {
        use crate::types::visit::*;
        let mut ty = ty;
        Shift::new(1).visit(&mut ty);
        TyTermSubst { cutoff: 0, ty }
    }

    fn visit_ty(&mut self, ty: &mut Type<TExtType>) {
        let mut s = crate::types::visit::Subst {
            cutoff: self.cutoff,
            ty: self.ty.clone(),
        };
        s.visit(ty);
    }
}

impl<
        TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
        TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
        TExtType: Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
    > MutTermVisitor<TExtPat, TExtKind, TExtType> for TyTermSubst<TExtType>
{
    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type<TExtType>, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        // self.cutoff += 1;
        self.visit_ty(ty);
        self.visit(term);
        // self.cutoff -= 1;
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>, ty: &mut Type<TExtType>) {
        self.visit_ty(ty);
        self.visit(term);
    }

    fn visit_tyabs(&mut self, sp: &mut Span, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_fold(&mut self, sp: &mut Span, ty: &mut Type<TExtType>, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        self.visit_ty(ty);
        self.visit(term);
    }

    fn visit_unfold(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtType>,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ) {
        self.visit_ty(ty);
        self.visit(term);
    }

    fn visit_unpack(
        &mut self,
        _: &mut Span,
        package: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ) {
        self.visit(package);
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_pack(
        &mut self,
        _: &mut Span,
        wit: &mut Type<TExtType>,
        body: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        sig: &mut Type<TExtType>,
    ) {
        self.visit_ty(wit);
        self.visit(body);
        self.visit_ty(sig);
    }

    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        ty: &mut Type<TExtType>,
    ) {
        self.visit_ty(ty);
        self.visit(term);
    }
}

/// Visitor for handling recursive variants automatically, by inserting a
/// fold term
///
/// Transform an [`Injection`] term of form: `Label tm of Rec(u.T)` into
/// `fold [u.T] Label tm of [X->u.T] T`
pub struct InjRewriter<
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
>(pub TExtPat, pub TExtKind);

impl<
        TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
        TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
        TExtType: Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    > MutTermVisitor<TExtPat, TExtKind, TExtType> for InjRewriter<TExtPat, TExtKind>
{
    fn visit(&mut self, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        match &mut term.kind {
            ExtKind::Injection(label, val, ty) => {
                if let Type::Rec(inner) = *ty.clone() {
                    let ty_prime = crate::types::subst(*ty.clone(), *inner.clone());
                    let rewrite_ty = ExtTerm::new(
                        ExtKind::Injection(label.clone(), val.clone(), Box::new(ty_prime)),
                        term.span,
                    );

                    *term = ExtTerm::new(ExtKind::Fold(ty.clone(), Box::new(rewrite_ty)), term.span);
                }
                self.walk(term);
            }
            _ => self.walk(term),
        }
    }
}

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
//! Visitor traits for [`Pattern`], [`Term`], and [`Type`] objects
use core::fmt;
use std::hash;

use crate::patterns::ExtPattern;
use crate::system_r_util::span::Span;
use crate::terms::{Arm, ExtKind, ExtTerm, Literal, Primitive};
use crate::types::{Type, Variant};

pub trait MutTypeVisitor<TExtType: Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash>:
    Sized
{
    fn visit_pb(&mut self, i: &mut Type<TExtType>, r: &mut Type<TExtType>) {}
    fn visit_var(&mut self, var: &mut usize) {}
    fn visit_alias(&mut self, alias: &mut String) {}

    fn visit_arrow(&mut self, ty1: &mut Type<TExtType>, ty2: &mut Type<TExtType>) {
        self.visit(ty1);
        self.visit(ty2);
    }

    fn visit_universal(&mut self, inner: &mut Type<TExtType>) {
        self.visit(inner);
    }

    fn visit_existential(&mut self, inner: &mut Type<TExtType>) {
        self.visit(inner);
    }

    fn visit_variant(&mut self, variant: &mut Vec<Variant<TExtType>>) {
        for v in variant {
            self.visit(&mut v.ty);
        }
    }

    fn visit_product(&mut self, product: &mut Vec<Type<TExtType>>) {
        for v in product {
            self.visit(v);
        }
    }

    fn visit_rec(&mut self, ty: &mut Type<TExtType>) {
        self.visit(ty);
    }

    fn visit(&mut self, ty: &mut Type<TExtType>) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat | Type::Tag(_) => {}
            Type::PlatformBinding(i, r) => self.visit_pb(i, r),
            Type::Var(v) => self.visit_var(v),
            Type::Variant(v) => self.visit_variant(v),
            Type::Product(v) => self.visit_product(v),
            Type::Alias(s) => self.visit_alias(s),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(ty) => self.visit_universal(ty),
            Type::Existential(ty) => self.visit_existential(ty),
            Type::Rec(ty) => self.visit_rec(ty),
            Type::Extended(ty) => panic!("visit L99 extended ty not impl"),
        }
    }
}

pub trait MutTermVisitor<
    TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    TExtType: Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
>: Sized
{
    fn visit_lit(&mut self, sp: &mut Span, lit: &mut Literal) {}
    fn visit_var(&mut self, sp: &mut Span, var: &mut usize) {}
    fn visit_pb(&mut self, sp: &mut Span, idx: &mut usize) {}

    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type<TExtType>, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        self.visit(term);
    }

    fn visit_app(
        &mut self,
        sp: &mut Span,
        t1: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        t2: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ) {
        self.visit(t1);
        self.visit(t2);
    }

    fn visit_let(
        &mut self,
        sp: &mut Span,
        pat: &mut ExtPattern<TExtPat>,
        t1: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        t2: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ) {
        self.visit(t1);
        self.visit(t2);
    }

    fn visit_tyabs(&mut self, sp: &mut Span, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        self.visit(term);
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>, ty: &mut Type<TExtType>) {
        self.visit(term);
    }

    fn visit_primitive(&mut self, sp: &mut Span, prim: &mut Primitive) {}
    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        ty: &mut Type<TExtType>,
    ) {
        self.visit(term);
    }

    fn visit_case(
        &mut self,
        sp: &mut Span,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        arms: &mut Vec<Arm<TExtPat, TExtKind, TExtType>>,
    ) {
        self.visit(term);
        for arm in arms {
            self.visit(&mut arm.term);
        }
    }

    fn visit_product(&mut self, sp: &mut Span, product: &mut Vec<ExtTerm<TExtPat, TExtKind, TExtType>>) {
        for t in product {
            self.visit(t);
        }
    }

    fn visit_projection(&mut self, sp: &mut Span, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>, index: &mut usize) {
        self.visit(term);
    }

    fn visit_fold(&mut self, sp: &mut Span, ty: &mut Type<TExtType>, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        self.visit(term);
    }
    fn visit_unfold(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtType>,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ) {
        self.visit(term);
    }

    fn visit_pack(
        &mut self,
        sp: &mut Span,
        witness: &mut Type<TExtType>,
        evidence: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        signature: &mut Type<TExtType>,
    ) {
        self.visit(evidence);
    }

    fn visit_unpack(
        &mut self,
        sp: &mut Span,
        package: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
        term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ) {
        self.visit(package);
        self.visit(term);
    }

    fn visit(&mut self, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        self.walk(term);
    }

    fn visit_ext(&mut self, sp: &mut Span, k: &mut TExtKind) {
        panic!("visit_ext unimpl; shouldn't be left this way lol..")
    }

    fn walk(&mut self, term: &mut ExtTerm<TExtPat, TExtKind, TExtType>) {
        let sp = &mut term.span;
        match &mut term.kind {
            ExtKind::Extended(k) => self.visit_ext(sp, k),
            ExtKind::Lit(l) => self.visit_lit(sp, l),
            ExtKind::Var(v) => self.visit_var(sp, v),
            ExtKind::PlatformBinding(v) => self.visit_pb(sp, v),
            ExtKind::Abs(ty, term) => self.visit_abs(sp, ty, term),
            ExtKind::App(t1, t2) => self.visit_app(sp, t1, t2),
            // Do we need a separate branch?
            ExtKind::Fix(term) => self.visit(term),
            ExtKind::Primitive(p) => self.visit_primitive(sp, p),
            ExtKind::Injection(label, tm, ty) => self.visit_injection(sp, label, tm, ty),
            ExtKind::Projection(term, idx) => self.visit_projection(sp, term, idx),
            ExtKind::Product(terms) => self.visit_product(sp, terms),
            ExtKind::Case(term, arms) => self.visit_case(sp, term, arms),
            ExtKind::Let(pat, t1, t2) => self.visit_let(sp, pat, t1, t2),
            ExtKind::TyAbs(term) => self.visit_tyabs(sp, term),
            ExtKind::TyApp(term, ty) => self.visit_tyapp(sp, term, ty),
            ExtKind::Fold(ty, term) => self.visit_fold(sp, ty, term),
            ExtKind::Unfold(ty, term) => self.visit_unfold(sp, ty, term),
            ExtKind::Pack(wit, term, sig) => self.visit_pack(sp, wit, term, sig),
            ExtKind::Unpack(package, term) => self.visit_unpack(sp, package, term),
        }
    }
}

pub trait PatternVisitor<
    TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
>: Sized
{
    fn visit_ext(&mut self, ext: &TExtPat) {}
    fn visit_literal(&mut self, lit: &Literal) {}
    fn visit_variable(&mut self, var: &str) {}
    fn visit_product(&mut self, pats: &Vec<ExtPattern<TExtPat>>) {
        for p in pats {
            self.visit_pattern(p);
        }
    }

    fn visit_constructor(&mut self, label: &str, pat: &ExtPattern<TExtPat>) {
        self.visit_pattern(pat);
    }

    fn visit_pattern(&mut self, pattern: &ExtPattern<TExtPat>) {
        match pattern {
            ExtPattern::Any => {}
            ExtPattern::Constructor(label, pat) => self.visit_constructor(label, pat),
            ExtPattern::Product(pat) => self.visit_product(pat),
            ExtPattern::Literal(lit) => self.visit_literal(lit),
            ExtPattern::Variable(var) => self.visit_variable(var),
            ExtPattern::Extended(v) => panic!("FIXME impl SystemRExtension check in this arm"),
        }
    }
}

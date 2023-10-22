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
//! Visitor traits for [`Pattern`], [`Term`], and [`Type`] objects
use core::fmt;
use std::hash;

use crate::extensions::SystemRDialect;
use crate::patterns::Pattern;
use crate::system_r_util::span::Span;
use crate::terms::{Arm, Kind, Literal, Primitive, Term};
use crate::types::{Type, Variant};

pub trait MutTypeVisitor<
    TExtDialect: SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
>: Sized
{
    fn visit_pb(&mut self, i: &mut Type<TExtDialect>, r: &mut Type<TExtDialect>) {}
    fn visit_var(&mut self, var: &mut usize) {}
    fn visit_alias(&mut self, alias: &mut String) {}

    fn visit_arrow(&mut self, ty1: &mut Type<TExtDialect>, ty2: &mut Type<TExtDialect>) {
        self.visit(ty1);
        self.visit(ty2);
    }

    fn visit_universal(&mut self, inner: &mut Type<TExtDialect>) {
        self.visit(inner);
    }

    fn visit_existential(&mut self, inner: &mut Type<TExtDialect>) {
        self.visit(inner);
    }

    fn visit_variant(&mut self, variant: &mut Vec<Variant<TExtDialect>>) {
        for v in variant {
            self.visit(&mut v.ty);
        }
    }

    fn visit_product(&mut self, product: &mut Vec<Type<TExtDialect>>) {
        for v in product {
            self.visit(v);
        }
    }

    fn visit_rec(&mut self, ty: &mut Type<TExtDialect>) {
        self.visit(ty);
    }

    fn visit_ext(&mut self, ty: &mut TExtDialect::TExtType) {}

    fn visit(&mut self, ty: &mut Type<TExtDialect>) {
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
            Type::Extended(ty) => self.visit_ext(ty),
        }
    }
}

pub trait MutTermVisitor<
    TExtDialect: Eq + hash::Hash + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd,
>: Sized
{
    fn visit_lit(&mut self, sp: &mut Span, lit: &mut Literal) {}
    fn visit_var(&mut self, sp: &mut Span, var: &mut usize) {}
    fn visit_pb(&mut self, sp: &mut Span, idx: &mut usize) {}

    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type<TExtDialect>, term: &mut Term<TExtDialect>) {
        self.visit(term);
    }

    fn visit_app(&mut self, sp: &mut Span, t1: &mut Term<TExtDialect>, t2: &mut Term<TExtDialect>) {
        self.visit(t1);
        self.visit(t2);
    }

    fn visit_let(
        &mut self,
        sp: &mut Span,
        pat: &mut Pattern<TExtDialect>,
        t1: &mut Term<TExtDialect>,
        t2: &mut Term<TExtDialect>,
    ) {
        self.visit(t1);
        self.visit(t2);
    }

    fn visit_tyabs(&mut self, sp: &mut Span, term: &mut Term<TExtDialect>) {
        self.visit(term);
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut Term<TExtDialect>, ty: &mut Type<TExtDialect>) {
        self.visit(term);
    }

    fn visit_primitive(&mut self, sp: &mut Span, prim: &mut Primitive) {}
    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut Term<TExtDialect>,
        ty: &mut Type<TExtDialect>,
    ) {
        self.visit(term);
    }

    fn visit_case(&mut self, sp: &mut Span, term: &mut Term<TExtDialect>, arms: &mut Vec<Arm<TExtDialect>>) {
        self.visit(term);
        for arm in arms {
            self.visit(&mut arm.term);
        }
    }

    fn visit_product(&mut self, sp: &mut Span, product: &mut Vec<Term<TExtDialect>>) {
        for t in product {
            self.visit(t);
        }
    }

    fn visit_projection(&mut self, sp: &mut Span, term: &mut Term<TExtDialect>, index: &mut usize) {
        self.visit(term);
    }

    fn visit_fold(&mut self, sp: &mut Span, ty: &mut Type<TExtDialect>, term: &mut Term<TExtDialect>) {
        self.visit(term);
    }
    fn visit_unfold(&mut self, sp: &mut Span, ty: &mut Type<TExtDialect>, term: &mut Term<TExtDialect>) {
        self.visit(term);
    }

    fn visit_pack(
        &mut self,
        sp: &mut Span,
        witness: &mut Type<TExtDialect>,
        evidence: &mut Term<TExtDialect>,
        signature: &mut Type<TExtDialect>,
    ) {
        self.visit(evidence);
    }

    fn visit_unpack(&mut self, sp: &mut Span, package: &mut Term<TExtDialect>, term: &mut Term<TExtDialect>) {
        self.visit(package);
        self.visit(term);
    }

    fn visit(&mut self, term: &mut Term<TExtDialect>) {
        self.walk(term);
    }

    fn visit_ext(&mut self, sp: &mut Span, k: &mut TExtDialect::TExtKind) {}

    fn walk(&mut self, term: &mut Term<TExtDialect>) {
        let sp = &mut term.span;
        match &mut term.kind {
            Kind::Extended(k) => self.visit_ext(sp, k),
            Kind::Lit(l) => self.visit_lit(sp, l),
            Kind::Var(v) => self.visit_var(sp, v),
            Kind::PlatformBinding(v) => self.visit_pb(sp, v),
            Kind::Abs(ty, term) => self.visit_abs(sp, ty, term),
            Kind::App(t1, t2) => self.visit_app(sp, t1, t2),
            // Do we need a separate branch?
            Kind::Fix(term) => self.visit(term),
            Kind::Primitive(p) => self.visit_primitive(sp, p),
            Kind::Injection(label, tm, ty) => self.visit_injection(sp, label, tm, ty),
            Kind::Projection(term, idx) => self.visit_projection(sp, term, idx),
            Kind::Product(terms) => self.visit_product(sp, terms),
            Kind::Case(term, arms) => self.visit_case(sp, term, arms),
            Kind::Let(pat, t1, t2) => self.visit_let(sp, pat, t1, t2),
            Kind::TyAbs(term) => self.visit_tyabs(sp, term),
            Kind::TyApp(term, ty) => self.visit_tyapp(sp, term, ty),
            Kind::Fold(ty, term) => self.visit_fold(sp, ty, term),
            Kind::Unfold(ty, term) => self.visit_unfold(sp, ty, term),
            Kind::Pack(wit, term, sig) => self.visit_pack(sp, wit, term, sig),
            Kind::Unpack(package, term) => self.visit_unpack(sp, package, term),
        }
    }
}

pub trait PatternVisitor<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd,
>: Sized
{
    fn visit_ext(&mut self, ext: &TExtDialect::TExtPat) {}
    fn visit_literal(&mut self, lit: &Literal) {}
    fn visit_variable(&mut self, var: &str) {}
    fn visit_product(&mut self, pats: &Vec<Pattern<TExtDialect>>) {
        for p in pats {
            self.visit_pattern(p);
        }
    }

    fn visit_constructor(&mut self, label: &str, pat: &Pattern<TExtDialect>) {
        self.visit_pattern(pat);
    }

    fn visit_pattern(&mut self, pattern: &Pattern<TExtDialect>) {
        match pattern {
            Pattern::Any => {}
            Pattern::Constructor(label, pat) => self.visit_constructor(label, pat),
            Pattern::Product(pat) => self.visit_product(pat),
            Pattern::Literal(lit) => self.visit_literal(lit),
            Pattern::Variable(var) => self.visit_variable(var),
            Pattern::Extended(v) => panic!("FIXME impl SystemRExtension check in this arm"),
        }
    }
}

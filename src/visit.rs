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

use crate::extensions::{SystemRDialect, SystemRExtension};
use crate::patterns::Pattern;
use crate::system_r_util::span::Span;
use crate::terms::{Arm, Kind, Literal, Primitive, Term};
use crate::types::{Type, Variant};

pub trait MutTypeVisitor<
    TExtDialect: SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExt: SystemRExtension<TExtDialect> + Clone + Default + fmt::Debug,
>: Sized
{
    fn visit_pb(&mut self, i: &mut Type<TExtDialect>, r: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}
    fn visit_var(&mut self, var: &mut usize, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}
    fn visit_alias(&mut self, alias: &mut String, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}

    fn visit_arrow(&mut self, ty1: &mut Type<TExtDialect>, ty2: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(ty1, ext, ext_state);
        self.visit(ty2, ext, ext_state);
    }

    fn visit_universal(&mut self, inner: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(inner, ext, ext_state);
    }

    fn visit_existential(&mut self, inner: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(inner, ext, ext_state);
    }

    fn visit_variant(&mut self, variant: &mut Vec<Variant<TExtDialect>>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        for v in variant {
            self.visit(&mut v.ty, ext, ext_state);
        }
    }

    fn visit_product(&mut self, product: &mut Vec<Type<TExtDialect>>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        for v in product {
            self.visit(v, ext, ext_state);
        }
    }

    fn visit_rec(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(ty, ext, ext_state);
    }

    fn visit_ext(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        panic!("this path shouldn't be reached; needs override in every concrete impl");
    }

    fn visit(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat | Type::Tag(_) => {}
            Type::PlatformBinding(i, r) => self.visit_pb(i, r, ext, ext_state),
            Type::Var(v) => self.visit_var(v, ext, ext_state),
            Type::Variant(v) => self.visit_variant(v, ext, ext_state),
            Type::Product(v) => self.visit_product(v, ext, ext_state),
            Type::Alias(s) => self.visit_alias(s, ext, ext_state),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2, ext, ext_state),
            Type::Universal(ty) => self.visit_universal(ty, ext, ext_state),
            Type::Existential(ty) => self.visit_existential(ty, ext, ext_state),
            Type::Rec(ty) => self.visit_rec(ty, ext, ext_state),
            Type::Extended(_) => self.visit_ext(ty, ext, ext_state),
        }
    }
}

pub trait MutTermVisitor<
    TExtDialect: Eq + hash::Hash + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    TExt: SystemRExtension<TExtDialect> + Default + fmt::Debug + Clone,
>: Sized
{
    fn visit_lit(&mut self, sp: &mut Span, lit: &mut Literal, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}
    fn visit_var(&mut self, sp: &mut Span, var: &mut usize, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}
    fn visit_pb(&mut self, sp: &mut Span, idx: &mut usize, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}

    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type<TExtDialect>, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(term, ext, ext_state);
    }

    fn visit_app(&mut self, sp: &mut Span, t1: &mut Term<TExtDialect>, t2: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(t1, ext, ext_state);
        self.visit(t2, ext, ext_state);
    }

    fn visit_let(
        &mut self,
        sp: &mut Span,
        pat: &mut Pattern<TExtDialect>,
        t1: &mut Term<TExtDialect>,
        t2: &mut Term<TExtDialect>,
        ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState,
    ) {
        self.visit(t1, ext, ext_state);
        self.visit(t2, ext, ext_state);
    }

    fn visit_tyabs(&mut self, sp: &mut Span, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(term, ext, ext_state);
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut Term<TExtDialect>, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(term, ext, ext_state);
    }

    fn visit_primitive(&mut self, sp: &mut Span, prim: &mut Primitive, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}
    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut Term<TExtDialect>,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState,
    ) {
        self.visit(term, ext, ext_state);
    }

    fn visit_case(&mut self, sp: &mut Span, term: &mut Term<TExtDialect>, arms: &mut Vec<Arm<TExtDialect>>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(term, ext, ext_state);
        for arm in arms {
            self.visit(&mut arm.term, ext, ext_state);
        }
    }

    fn visit_product(&mut self, sp: &mut Span, product: &mut Vec<Term<TExtDialect>>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        for t in product {
            self.visit(t, ext, ext_state);
        }
    }

    fn visit_projection(&mut self, sp: &mut Span, term: &mut Term<TExtDialect>, index: &mut usize, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(term, ext, ext_state);
    }

    fn visit_fold(&mut self, sp: &mut Span, ty: &mut Type<TExtDialect>, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(term, ext, ext_state);
    }
    fn visit_unfold(&mut self, sp: &mut Span, ty: &mut Type<TExtDialect>, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(term, ext, ext_state);
    }

    fn visit_pack(
        &mut self,
        sp: &mut Span,
        witness: &mut Type<TExtDialect>,
        evidence: &mut Term<TExtDialect>,
        signature: &mut Type<TExtDialect>,
 ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState,
    ) {
        self.visit(evidence, ext, ext_state);
    }

    fn visit_unpack(&mut self, sp: &mut Span, package: &mut Term<TExtDialect>, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit(package, ext, ext_state);
        self.visit(term, ext, ext_state);
    }

    fn visit(&mut self, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.walk(term, ext, ext_state);
    }

    fn visit_ext(&mut self, sp: &mut Span, k: &mut TExtDialect::TExtKind, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}

    fn walk(&mut self, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        let sp = &mut term.span;
        match &mut term.kind {
            Kind::Extended(k) => self.visit_ext(sp, k, ext, ext_state),
            Kind::Lit(l) => self.visit_lit(sp, l, ext, ext_state),
            Kind::Var(v) => self.visit_var(sp, v, ext, ext_state),
            Kind::PlatformBinding(v) => self.visit_pb(sp, v, ext, ext_state),
            Kind::Abs(ty, term) => self.visit_abs(sp, ty, term, ext, ext_state),
            Kind::App(t1, t2) => self.visit_app(sp, t1, t2, ext, ext_state),
            // Do we need a separate branch?
            Kind::Fix(term) => self.visit(term, ext, ext_state),
            Kind::Primitive(p) => self.visit_primitive(sp, p, ext, ext_state),
            Kind::Injection(label, tm, ty) => self.visit_injection(sp, label, tm, ty, ext, ext_state),
            Kind::Projection(term, idx) => self.visit_projection(sp, term, idx, ext, ext_state),
            Kind::Product(terms) => self.visit_product(sp, terms, ext, ext_state),
            Kind::Case(term, arms) => self.visit_case(sp, term, arms, ext, ext_state),
            Kind::Let(pat, t1, t2) => self.visit_let(sp, pat, t1, t2, ext, ext_state),
            Kind::TyAbs(term) => self.visit_tyabs(sp, term, ext, ext_state),
            Kind::TyApp(term, ty) => self.visit_tyapp(sp, term, ty, ext, ext_state),
            Kind::Fold(ty, term) => self.visit_fold(sp, ty, term, ext, ext_state),
            Kind::Unfold(ty, term) => self.visit_unfold(sp, ty, term, ext, ext_state),
            Kind::Pack(wit, term, sig) => self.visit_pack(sp, wit, term, sig, ext, ext_state),
            Kind::Unpack(package, term) => self.visit_unpack(sp, package, term, ext, ext_state),
        }
    }
}

pub trait PatternVisitor<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    TExt: SystemRExtension<TExtDialect> + Clone + fmt::Debug + Default,
>: Sized
{
    fn visit_ext(&mut self, pat: &TExtDialect::TExtPat, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}
    fn visit_literal(&mut self, lit: &Literal, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}
    fn visit_variable(&mut self, var: &str, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {}
    fn visit_product(&mut self, pats: &Vec<Pattern<TExtDialect>>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        for p in pats {
            self.visit_pattern(p, ext, ext_state);
        }
    }

    fn visit_constructor(&mut self, label: &str, pat: &Pattern<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.visit_pattern(pat, ext, ext_state);
    }

    fn visit_pattern(&mut self, pattern: &Pattern<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        match pattern {
            Pattern::Any => {}
            Pattern::Constructor(label, pat) => self.visit_constructor(label, pat, ext, ext_state),
            Pattern::Product(pat) => self.visit_product(pat, ext, ext_state),
            Pattern::Literal(lit) => self.visit_literal(lit, ext, ext_state),
            Pattern::Variable(var) => self.visit_variable(var, ext, ext_state),
            Pattern::Extended(v) => self.visit_ext(v, ext, ext_state),
        }
    }
}

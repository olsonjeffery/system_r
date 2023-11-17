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

use crate::dialect::{SystemRDialect, SystemRExtension};
use crate::patterns::Pattern;
use crate::system_r_util::span::Span;
use crate::terms::{Arm, Kind, Literal, Primitive, Term};
use crate::types::{Type, Variant};

pub trait MutTypeVisitor<
    TExtDialect: SystemRDialect,
    TExt: SystemRExtension<TExtDialect>,
>: Sized
{
    fn visit_pb(
        &mut self,
        i: &mut Type<TExtDialect>,
        r: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
    }
    fn visit_var(&mut self, var: &mut usize, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {}
    fn visit_alias(&mut self, alias: &mut String, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {}

    fn visit_arrow(
        &mut self,
        ty1: &mut Type<TExtDialect>,
        ty2: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(ty1, ext, ext_state);
        self.visit(ty2, ext, ext_state);
    }

    fn visit_universal(
        &mut self,
        inner: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(inner, ext, ext_state);
    }

    fn visit_existential(
        &mut self,
        inner: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(inner, ext, ext_state);
    }

    fn visit_variant(
        &mut self,
        variant: &mut Vec<Variant<TExtDialect>>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        for v in variant {
            self.visit(&mut v.ty, ext, ext_state);
        }
    }

    fn visit_product(
        &mut self,
        product: &mut Vec<Type<TExtDialect>>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        for v in product {
            self.visit(v, ext, ext_state);
        }
    }

    fn visit_rec(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {
        self.visit(ty, ext, ext_state);
    }

    fn visit_ext(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {
        panic!("this path shouldn't be reached; needs override in every concrete impl");
    }

    fn visit(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat | Type::Tag(_) | Type::Bytes => {}
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
    TExtDialect: SystemRDialect,
    TExt: SystemRExtension<TExtDialect>,
>: Sized
{
    fn visit_lit(
        &mut self,
        sp: &mut Span,
        lit: &mut Literal,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
    }
    fn visit_var(&mut self, sp: &mut Span, var: &mut usize, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {
    }
    fn visit_pb(&mut self, sp: &mut Span, idx: &mut usize, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {}

    fn visit_abs(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(term, ext, ext_state);
    }

    fn visit_app(
        &mut self,
        sp: &mut Span,
        t1: &mut Term<TExtDialect>,
        t2: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(t1, ext, ext_state);
        self.visit(t2, ext, ext_state);
    }

    fn visit_let(
        &mut self,
        sp: &mut Span,
        pat: &mut Pattern<TExtDialect>,
        t1: &mut Term<TExtDialect>,
        t2: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(t1, ext, ext_state);
        self.visit(t2, ext, ext_state);
    }

    fn visit_tyabs(
        &mut self,
        sp: &mut Span,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(term, ext, ext_state);
    }

    fn visit_tyapp(
        &mut self,
        sp: &mut Span,
        term: &mut Term<TExtDialect>,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(term, ext, ext_state);
    }

    fn visit_primitive(
        &mut self,
        sp: &mut Span,
        prim: &mut Primitive,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
    }
    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut Term<TExtDialect>,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(term, ext, ext_state);
    }

    fn visit_case(
        &mut self,
        sp: &mut Span,
        term: &mut Term<TExtDialect>,
        arms: &mut Vec<Arm<TExtDialect>>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(term, ext, ext_state);
        for arm in arms {
            self.visit(&mut arm.term, ext, ext_state);
        }
    }

    fn visit_product(
        &mut self,
        sp: &mut Span,
        product: &mut Vec<Term<TExtDialect>>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        for t in product {
            self.visit(t, ext, ext_state);
        }
    }

    fn visit_projection(
        &mut self,
        sp: &mut Span,
        term: &mut Term<TExtDialect>,
        index: &mut usize,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(term, ext, ext_state);
    }

    fn visit_fold(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(term, ext, ext_state);
    }
    fn visit_unfold(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(term, ext, ext_state);
    }

    fn visit_pack(
        &mut self,
        sp: &mut Span,
        witness: &mut Type<TExtDialect>,
        evidence: &mut Term<TExtDialect>,
        signature: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(evidence, ext, ext_state);
    }

    fn visit_unpack(
        &mut self,
        sp: &mut Span,
        package: &mut Term<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit(package, ext, ext_state);
        self.visit(term, ext, ext_state);
    }

    fn visit(&mut self, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {
        self.walk(term, ext, ext_state);
    }

    fn visit_ext(
        &mut self,
        sp: &mut Span,
        k: &TExtDialect::Kind,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        panic!("visit_ext in MutTermVisitor should be override in implementors")
    }

    fn walk(&mut self, term: &mut Term<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {
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
    TExtDialect: SystemRDialect,
    TExt: SystemRExtension<TExtDialect>,
>: Sized
{
    fn visit_ext(&mut self, pat: &Pattern<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {
        panic!("shouldn't be reached; needs to be overriden in every implementor")
    }
    fn visit_literal(&mut self, lit: &Literal, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {}
    fn visit_variable(&mut self, var: &str, ext: &mut TExt, ext_state: &TExtDialect::DialectState) {}
    fn visit_product(
        &mut self,
        pats: &Vec<Pattern<TExtDialect>>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        for p in pats {
            self.visit_pattern(p, ext, ext_state);
        }
    }

    fn visit_constructor(
        &mut self,
        label: &str,
        pat: &Pattern<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        self.visit_pattern(pat, ext, ext_state);
    }

    fn visit_pattern(
        &mut self,
        pattern: &Pattern<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) {
        match pattern {
            Pattern::Any => {}
            Pattern::Constructor(label, pat) => self.visit_constructor(label, pat, ext, ext_state),
            Pattern::Product(pat) => self.visit_product(pat, ext, ext_state),
            Pattern::Literal(lit) => self.visit_literal(lit, ext, ext_state),
            Pattern::Variable(var) => self.visit_variable(var, ext, ext_state),
            Pattern::Extended(_) => self.visit_ext(pattern, ext, ext_state),
        }
    }
}

pub trait DialectChangingPatternVisitor<
    InDialect: SystemRDialect,
    OutDialect: SystemRDialect,
>: Sized
{
    fn visit_ext(&self, pat: &Pattern<InDialect>) -> Pattern<OutDialect>;

    fn visit_product(&self, pats: &Vec<Pattern<InDialect>>) -> Pattern<OutDialect> {
        let mut out_pats = Vec::new();
        for p in pats {
            let out_p = self.visit(p);
            out_pats.push(out_p);
        }
        Pattern::Product(out_pats)
    }

    fn visit_constructor(&self, label: &str, pat: &Pattern<InDialect>) -> Pattern<OutDialect> {
        let out_pat = self.visit(pat);
        Pattern::Constructor(label.to_owned(), Box::new(out_pat))
    }

    fn visit(&self, pat: &Pattern<InDialect>) -> Pattern<OutDialect> {
        match pat {
            Pattern::Any => Pattern::Any,
            Pattern::Constructor(label, pat) => self.visit_constructor(label, pat),
            Pattern::Product(pat) => self.visit_product(pat),
            Pattern::Literal(lit) => Pattern::Literal(lit.clone()),
            Pattern::Variable(var) => Pattern::Variable(var.clone()),
            Pattern::Extended(_) => self.visit_ext(pat),
        }
    }
}

pub trait DialectChangingTypeVisitor<
    InDialect: SystemRDialect,
    OutDialect: SystemRDialect,
>: Sized
{
    fn visit_pb(&self, i: &Type<InDialect>, r: &Type<InDialect>) -> Type<OutDialect> {
        let out_i = self.visit(i);
        let out_r = self.visit(r);
        Type::PlatformBinding(Box::new(out_i), Box::new(out_r))
    }
    fn visit_var(&self, var: &usize) -> Type<OutDialect> {
        Type::Var(*var)
    }
    fn visit_alias(&self, alias: &String) -> Type<OutDialect> {
        Type::Alias(alias.clone())
    }

    fn visit_arrow(&self, ty1: &Type<InDialect>, ty2: &Type<InDialect>) -> Type<OutDialect> {
        let out_ty1 = self.visit(ty1);
        let out_ty2 = self.visit(ty2);
        Type::Arrow(Box::new(out_ty1), Box::new(out_ty2))
    }

    fn visit_universal(&self, inner: &Type<InDialect>) -> Type<OutDialect> {
        let out_inner = self.visit(inner);
        Type::Universal(Box::new(out_inner))
    }

    fn visit_existential(&self, inner: &Type<InDialect>) -> Type<OutDialect> {
        let out_inner = self.visit(inner);
        Type::Existential(Box::new(out_inner))
    }

    fn visit_variant(&self, variant: &Vec<Variant<InDialect>>) -> Type<OutDialect> {
        let mut out_variant = Vec::new();
        for v in variant {
            let out_ty = self.visit(&v.ty);
            let out_v = Variant {
                label: v.label.clone(),
                ty: out_ty,
            };
            out_variant.push(out_v)
        }
        Type::Variant(out_variant)
    }

    fn visit_product(&self, product: &Vec<Type<InDialect>>) -> Type<OutDialect> {
        let mut out_product = Vec::new();
        for v in product {
            let out_ty = self.visit(&v);
            out_product.push(out_ty)
        }
        Type::Product(out_product)
    }

    fn visit_rec(&self, ty: &Type<InDialect>) -> Type<OutDialect> {
        let out_ty = self.visit(ty);
        Type::Rec(Box::new(out_ty))
    }

    fn visit_ext(&self, ty: &Type<InDialect>) -> Type<OutDialect>;

    fn visit(&self, ty: &Type<InDialect>) -> Type<OutDialect> {
        match ty {
            Type::Unit => Type::Unit,
            Type::Bytes => Type::Bytes,
            Type::Bool => Type::Bool,
            Type::Nat => Type::Nat,
            Type::Tag(t) => Type::Tag(t.clone()),
            Type::PlatformBinding(i, r) => self.visit_pb(i, r),
            Type::Var(v) => self.visit_var(v),
            Type::Variant(v) => self.visit_variant(v),
            Type::Product(v) => self.visit_product(v),
            Type::Alias(s) => self.visit_alias(s),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(ty) => self.visit_universal(ty),
            Type::Existential(ty) => self.visit_existential(ty),
            Type::Rec(ty) => self.visit_rec(ty),
            Type::Extended(_) => self.visit_ext(ty),
        }
    }
}

pub trait DialectChangingTermVisitor<
    InDialect: SystemRDialect,
    OutDialect: SystemRDialect,
    TTypeVisitor: DialectChangingTypeVisitor<InDialect, OutDialect>,
    TPatVisitor: DialectChangingPatternVisitor<InDialect, OutDialect>,
>: Sized
{
    fn get_type_visitor(&self) -> TTypeVisitor;
    fn get_pat_visitor(&self) -> TPatVisitor;

    fn visit_lit(&self, sp: &Span, lit: &Literal) {}
    fn visit_var(&self, sp: &Span, var: &usize) {}
    fn visit_pb(&self, sp: &Span, idx: &usize) {}

    fn visit_abs(&self, sp: &Span, ty: &Type<InDialect>, term: &Term<InDialect>) -> Term<OutDialect> {
        let out_term = self.visit(term);
        let out_ty = {
            let tv = self.get_type_visitor().visit(ty);
            tv
        };
        Term {
            span: sp.clone(),
            kind: Kind::Abs(Box::new(out_ty), Box::new(out_term)),
        }
    }

    fn visit_app(&self, sp: &Span, t1: &Term<InDialect>, t2: &Term<InDialect>) -> Term<OutDialect> {
        let out_t1 = self.visit(t1);
        let out_t2 = self.visit(t2);
        Term {
            span: sp.clone(),
            kind: Kind::App(Box::new(out_t1), Box::new(out_t2)),
        }
    }

    fn visit_let(
        &self,
        sp: &Span,
        pat: &Pattern<InDialect>,
        t1: &Term<InDialect>,
        t2: &Term<InDialect>,
    ) -> Term<OutDialect> {
        let out_pat = self.get_pat_visitor().visit(pat);
        let out_t1 = self.visit(t1);
        let out_t2 = self.visit(t2);
        Term {
            span: sp.clone(),
            kind: Kind::Let(Box::new(out_pat), Box::new(out_t1), Box::new(out_t2)),
        }
    }

    fn visit_tyabs(&self, sp: &Span, term: &Term<InDialect>) -> Term<OutDialect> {
        let out_term = self.visit(term);

        Term {
            span: sp.clone(),
            kind: Kind::TyAbs(Box::new(out_term)),
        }
    }

    fn visit_tyapp(&self, sp: &Span, term: &Term<InDialect>, ty: &Type<InDialect>) -> Term<OutDialect> {
        let out_term = self.visit(term);
        let out_ty = self.get_type_visitor().visit(ty);
        Term {
            span: sp.clone(),
            kind: Kind::TyApp(Box::new(out_term), Box::new(out_ty)),
        }
    }

    fn visit_injection(
        &self,
        sp: &Span,
        label: &String,
        term: &Term<InDialect>,
        ty: &Type<InDialect>,
    ) -> Term<OutDialect> {
        let out_term = self.visit(term);
        let out_ty = self.get_type_visitor().visit(ty);
        Term {
            span: sp.clone(),
            kind: Kind::Injection(label.to_owned(), Box::new(out_term), Box::new(out_ty)),
        }
    }

    fn visit_case(&self, sp: &Span, term: &Term<InDialect>, arms: &Vec<Arm<InDialect>>) -> Term<OutDialect> {
        let out_term = self.visit(term);
        let mut out_arms = Vec::new();
        for arm in arms {
            let out_arm_term = self.visit(&arm.term);
            let out_pat = self.get_pat_visitor().visit(&arm.pat);
            let out_arm = Arm {
                pat: out_pat,
                span: arm.span.clone(),
                term: Box::new(out_arm_term),
            };
            out_arms.push(out_arm);
        }

        Term {
            span: sp.clone(),
            kind: Kind::Case(Box::new(out_term), out_arms),
        }
    }

    fn visit_product(&self, sp: &Span, product: &Vec<Term<InDialect>>) -> Term<OutDialect> {
        let mut out_product = Vec::new();

        for t in product {
            let out_t = self.visit(t);
            out_product.push(out_t);
        }

        Term {
            span: sp.clone(),
            kind: Kind::Product(out_product),
        }
    }

    fn visit_projection(&self, sp: &Span, term: &Term<InDialect>, index: &usize) -> Term<OutDialect> {
        let out_term = self.visit(term);
        Term {
            span: sp.clone(),
            kind: Kind::Projection(Box::new(out_term), *index),
        }
    }

    fn visit_fold(&self, sp: &Span, ty: &Type<InDialect>, term: &Term<InDialect>) -> Term<OutDialect> {
        let out_ty = self.get_type_visitor().visit(ty);
        let out_term = self.visit(term);

        Term {
            span: sp.clone(),
            kind: Kind::Fold(Box::new(out_ty), Box::new(out_term)),
        }
    }
    fn visit_unfold(&self, sp: &Span, ty: &Type<InDialect>, term: &Term<InDialect>) -> Term<OutDialect> {
        let out_ty = self.get_type_visitor().visit(ty);
        let out_term = self.visit(term);

        Term {
            span: sp.clone(),
            kind: Kind::Unfold(Box::new(out_ty), Box::new(out_term)),
        }
    }

    fn visit_pack(
        &self,
        sp: &Span,
        witness: &Type<InDialect>,
        evidence: &Term<InDialect>,
        signature: &Type<InDialect>,
    ) -> Term<OutDialect> {
        let ty_visitor = self.get_type_visitor();
        let witness = ty_visitor.visit(witness);
        let signature = ty_visitor.visit(signature);
        let evidence = self.visit(evidence);
        let k = Kind::Pack(Box::new(witness), Box::new(evidence), Box::new(signature));
        Term {
            span: sp.clone(),
            kind: k,
        }
    }

    fn visit_unpack(&self, sp: &Span, package: &Term<InDialect>, term: &Term<InDialect>) -> Term<OutDialect> {
        let pkg = self.visit(package);
        let t = self.visit(term);
        let k = Kind::Unpack(Box::new(pkg), Box::new(t));
        Term {
            span: sp.clone(),
            kind: k,
        }
    }

    fn visit_ext(&self, term: &Term<InDialect>) -> Term<OutDialect>;

    fn visit(&self, term: &Term<InDialect>) -> Term<OutDialect> {
        let sp = &term.span;
        match &term.kind {
            Kind::Extended(_) => self.visit_ext(term),
            Kind::Lit(l) => Term {
                span: sp.clone(),
                kind: Kind::Lit(l.clone()),
            },
            Kind::Var(v) => Term {
                span: sp.clone(),
                kind: Kind::Var(*v),
            },
            Kind::PlatformBinding(v) => Term {
                span: sp.clone(),
                kind: Kind::PlatformBinding(*v),
            },
            Kind::Abs(ty, term) => self.visit_abs(sp, ty, term),
            Kind::App(t1, t2) => self.visit_app(sp, t1, t2),
            // Do we need a separate branch?
            Kind::Fix(term) => Term {
                span: sp.clone(),
                kind: Kind::Fix(Box::new(self.visit(term))),
            },
            Kind::Primitive(p) => Term {
                span: sp.clone(),
                kind: Kind::Primitive(*p),
            },
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

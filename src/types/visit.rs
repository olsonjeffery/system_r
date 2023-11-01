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
use super::Type;
use crate::{extensions::{SystemRDialect, SystemRExtension}, visit::MutTypeVisitor};
use core::fmt;
use std::{convert::TryFrom, hash};

pub struct Shift {
    pub cutoff: usize,
    pub shift: isize,
}

impl Shift {
    pub const fn new(shift: isize) -> Shift {
        Shift { cutoff: 0, shift }
    }
}

impl<TExtDialect: SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
    TExt: SystemRExtension<TExtDialect> + Clone + Default + fmt::Debug,
>
    MutTypeVisitor<TExtDialect, TExt> for Shift
{
    fn visit_ext(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut <TExtDialect as SystemRDialect>::TExtDialectState) {
        ext.ty_shift_visit_ext(self, ty, ext_state);
    }
    fn visit_var(&mut self, var: &mut usize, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        if *var >= self.cutoff {
            *var = usize::try_from(*var as isize + self.shift).expect("Variable has been shifted below 0! Fatal bug");
        }
    }

    fn visit_universal(&mut self, inner: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_existential(&mut self, inner: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_rec(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.cutoff += 1;
        self.visit(ty, ext, ext_state);
        self.cutoff -= 1;
    }
}

/// Represents substituting the provided `Type` into
/// TmVar(0) position in the `&mut Type<TExtDialect>` provided to
/// `visit()`
pub struct Subst<
    TExtDialect: Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
> {
    pub cutoff: usize,
    pub ty: Type<TExtDialect>,
}

impl<TExtDialect: Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash>
    Subst<TExtDialect>
{
    pub fn new(ty: Type<TExtDialect>) -> Subst<TExtDialect> {
        Subst { cutoff: 0, ty }
    }
}

impl<TExtDialect: Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
    TExt: SystemRExtension<TExtDialect> + Clone + Default + fmt::Debug,
>
    MutTypeVisitor<TExtDialect, TExt> for Subst<TExtDialect>
{
    fn visit_universal(&mut self, inner: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_existential(&mut self, inner: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.cutoff += 1;
        self.visit(inner, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_rec(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        self.cutoff += 1;
        self.visit(ty, ext, ext_state);
        self.cutoff -= 1;
    }

    fn visit_ext(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        ext.ty_subst_visit_ext(self, ty, ext_state);
    }

    fn visit(&mut self, ty: &mut Type<TExtDialect>, ext: &mut TExt, ext_state: &mut TExtDialect::TExtDialectState) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat | Type::Tag(_) => {}
            Type::PlatformBinding(i, r) => {}
            Type::Var(v) => {
                let makes_cutoff = *v >= self.cutoff;
                if self.ty == Type::Nat {
                    //panic!("about to substitute in Nat to {:?}, would it make the cutoff? {:?}", ty, makes_cutoff)
                }
                if makes_cutoff {
                    Shift::new(self.cutoff as isize).visit(&mut self.ty, ext, ext_state);
                    *ty = self.ty.clone();
                } else {
                    self.visit_var(v, ext, ext_state); // this is a NOOP; should remove? was previously another arm of this match
                }
            }
            Type::Variant(v) => self.visit_variant(v, ext, ext_state),
            Type::Product(v) => self.visit_product(v, ext, ext_state),
            Type::Alias(v) => self.visit_alias(v, ext, ext_state),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2, ext, ext_state),
            Type::Universal(ty) => self.visit_universal(ty, ext, ext_state),
            Type::Existential(ty) => self.visit_existential(ty, ext, ext_state),
            Type::Rec(ty) => self.visit_rec(ty, ext, ext_state),
            Type::Extended(_) => self.visit_ext(ty, ext, ext_state),
        }
    }
}

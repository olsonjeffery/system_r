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
use crate::{extensions::SystemRDialect, visit::MutTypeVisitor};
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

impl<TExtDialect: SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash>
    MutTypeVisitor<TExtDialect> for Shift
{
    fn visit_var(&mut self, var: &mut usize) {
        if *var >= self.cutoff {
            *var = usize::try_from(*var as isize + self.shift).expect("Variable has been shifted below 0! Fatal bug");
        }
    }

    fn visit_universal(&mut self, inner: &mut Type<TExtDialect>) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }

    fn visit_existential(&mut self, inner: &mut Type<TExtDialect>) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }

    fn visit_rec(&mut self, ty: &mut Type<TExtDialect>) {
        self.cutoff += 1;
        self.visit(ty);
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

impl<TExtDialect: Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash>
    MutTypeVisitor<TExtDialect> for Subst<TExtDialect>
{
    fn visit_universal(&mut self, inner: &mut Type<TExtDialect>) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }

    fn visit_existential(&mut self, inner: &mut Type<TExtDialect>) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }

    fn visit_rec(&mut self, ty: &mut Type<TExtDialect>) {
        self.cutoff += 1;
        self.visit(ty);
        self.cutoff -= 1;
    }

    fn visit(&mut self, ty: &mut Type<TExtDialect>) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat | Type::Tag(_) => {}
            Type::PlatformBinding(i, r) => {}
            Type::Var(v) if *v >= self.cutoff => {
                Shift::new(self.cutoff as isize).visit(&mut self.ty);
                *ty = self.ty.clone();
            }
            Type::Var(v) => self.visit_var(v),
            Type::Variant(v) => self.visit_variant(v),
            Type::Product(v) => self.visit_product(v),
            Type::Alias(v) => self.visit_alias(v),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(ty) => self.visit_universal(ty),
            Type::Existential(ty) => self.visit_existential(ty),
            Type::Rec(ty) => self.visit_rec(ty),
            Type::Extended(v) => panic!("handle extended in MutTypeVisitor {:?}", v),
        }
    }
}

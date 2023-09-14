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
use super::Type;
use crate::visit::MutTypeVisitor;
use std::convert::TryFrom;

pub struct Shift {
    pub cutoff: usize,
    pub shift: isize,
}

impl Shift {
    pub const fn new(shift: isize) -> Shift {
        Shift { cutoff: 0, shift }
    }
}

impl MutTypeVisitor for Shift {
    fn visit_var(&mut self, var: &mut usize) {
        if *var >= self.cutoff {
            *var = usize::try_from(*var as isize + self.shift).expect("Variable has been shifted below 0! Fatal bug");
        }
    }

    fn visit_universal(&mut self, inner: &mut Type) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }

    fn visit_existential(&mut self, inner: &mut Type) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }

    fn visit_rec(&mut self, ty: &mut Type) {
        self.cutoff += 1;
        self.visit(ty);
        self.cutoff -= 1;
    }
}

pub struct Subst {
    pub cutoff: usize,
    pub ty: Type,
}

impl Subst {
    pub fn new(ty: Type) -> Subst {
        Subst { cutoff: 0, ty }
    }
}

impl MutTypeVisitor for Subst {
    fn visit_universal(&mut self, inner: &mut Type) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }

    fn visit_existential(&mut self, inner: &mut Type) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }

    fn visit_rec(&mut self, ty: &mut Type) {
        self.cutoff += 1;
        self.visit(ty);
        self.cutoff -= 1;
    }

    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat | Type::Tag(_) => {}
            Type::PlatformBinding(i, r) => {},
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
        }
    }
}

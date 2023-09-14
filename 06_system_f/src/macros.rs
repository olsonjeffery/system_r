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
//! Macros to make writing tests easier

/// Boolean term
macro_rules! lit {
    ($x:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Lit(crate::terms::Literal::Bool($x)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Integer term
macro_rules! nat {
    ($x:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Lit(crate::terms::Literal::Nat($x)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// TmVar term
macro_rules! var {
    ($x:expr) => {
        crate::terms::Term::new(crate::terms::Kind::Var($x), crate::system_r_util::span::Span::dummy())
    };
}

/// Application term
macro_rules! app {
    ($t1:expr, $t2:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::App(Box::new($t1), Box::new($t2)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Lambda abstraction term
macro_rules! abs {
    ($ty:expr, $t:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Abs(Box::new($ty), Box::new($t)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Type application term
macro_rules! tyapp {
    ($t1:expr, $t2:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::TyApp(Box::new($t1), Box::new($t2)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Type abstraction term
macro_rules! tyabs {
    ( $t:expr) => {
        crate::terms::Term::new(crate::terms::Kind::TyAbs(Box::new($t)), crate::system_r_util::span::Span::dummy())
    };
}

/// Primitive term
macro_rules! prim {
    ($t:expr) => {
        crate::terms::Term::new(crate::terms::Kind::Primitive($t), crate::system_r_util::span::Span::dummy())
    };
}

macro_rules! inj {
    ($label:expr, $t:expr, $ty:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Injection($label.to_string(), Box::new($t), Box::new($ty)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Product term
macro_rules! tuple {
    ($($ex:expr),+) => { crate::terms::Term::new(crate::terms::Kind::Product(vec![$($ex),+]),
    crate::system_r_util::span::Span::dummy()) }
}

/// Type arrow
macro_rules! arrow {
    ($ty1:expr, $ty2:expr) => {
        crate::types::Type::Arrow(Box::new($ty1), Box::new($ty2))
    };
}

/// Boolean pattern
macro_rules! boolean {
    ($ex:expr) => {
        crate::patterns::Pattern::Literal(crate::terms::Literal::Bool($ex))
    };
}

/// Numeric pattern
macro_rules! num {
    ($ex:expr) => {
        crate::patterns::Pattern::Literal(crate::terms::Literal::Nat($ex))
    };
}

/// Product pattern
macro_rules! prod {
    ($($ex:expr),+) => { crate::patterns::Pattern::Product(vec![$($ex),+]) }
}

/// Constructor pattern
macro_rules! con {
    ($label:expr, $ex:expr) => {
        crate::patterns::Pattern::Constructor($label.to_string(), Box::new($ex))
    };
}

/// Variant type
macro_rules! variant {
    ($label:expr, $ty:expr) => {
        crate::types::Variant {
            label: $label.to_string(),
            ty: $ty,
        }
    };
}

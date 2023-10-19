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
//! Macros to make writing tests easier

use crate::{
    bottom::{BottomKind, BottomPattern, BottomType},
    terms::Term,
};

/// Boolean term
macro_rules! lit {
    ($x:expr) => {
        Term::<BottomDialect>::new(
            crate::terms::Kind::<BottomDialect>::Lit(crate::terms::Literal::Bool($x)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Integer term
macro_rules! nat {
    ($x:expr) => {
        crate::terms::Term::<BottomDialect>::new(
            crate::terms::Kind::<BottomDialect>::Lit(crate::terms::Literal::Nat($x)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// TmVar term
macro_rules! var {
    ($x:expr) => {
        crate::terms::Term::<BottomDialect>::new(
            crate::terms::Kind::<BottomDialect>::Var($x),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Application term
macro_rules! app {
    ($t1:expr, $t2:expr) => {
        crate::terms::Term::<BottomDialect>::new(
            crate::terms::Kind::<BottomDialect>::App(Box::new($t1), Box::new($t2)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Lambda abstraction term
macro_rules! abs {
    ($ty:expr, $t:expr) => {
        crate::terms::Term::<BottomDialect>::new(
            crate::terms::Kind::<BottomDialect>::Abs(Box::new($ty), Box::new($t)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Type application term
macro_rules! tyapp {
    ($t1:expr, $t2:expr) => {
        crate::terms::Term::<BottomDialect>::new(
            crate::terms::Kind::<BottomDialect>::TyApp(Box::new($t1), Box::new($t2)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Type abstraction term
macro_rules! tyabs {
    ( $t:expr) => {
        crate::terms::Term::<BottomDialect>::new(
            crate::terms::Kind::<BottomDialect>::TyAbs(Box::new($t)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Primitive term
macro_rules! prim {
    ($t:expr) => {
        crate::terms::Term::<BottomDialect>::new(
            crate::terms::Kind::<BottomDialect>::Primitive($t),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

macro_rules! inj {
    ($label:expr, $t:expr, $ty:expr) => {
        crate::terms::Term::<BottomDialect>::new(
            crate::terms::Kind::<BottomDialect>::Injection($label.to_string(), Box::new($t), Box::new($ty)),
            crate::system_r_util::span::Span::dummy(),
        )
    };
}

/// Product term
macro_rules! tuple {
    ($($ex:expr),+) => { crate::terms::Term::<BottomDialect>::new(crate::terms::Kind::<BottomDialect>::Product(vec![$($ex),+]),
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

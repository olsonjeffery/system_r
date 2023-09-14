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
#![allow(unused_variables, unused_macros)]
#[macro_use]
pub mod macros;
pub mod extension;
pub mod diagnostics;
pub mod eval;
pub mod patterns;
pub mod syntax;
pub mod terms;
pub mod types;
pub mod visit;
pub mod testing;
fn main() {}
/*
DEPRECATED - will be removed to a side-crate w/ a system_r repl+script runner
use diagnostics::*;
use std::env;
use std::io::{Read, Write};
use syntax::parser::{self, Parser};
use terms::{visit::InjRewriter, Term};
use types::{Type, Variant};
use visit::MutTermVisitor;

fn test_variant() -> Type {
    Type::Variant(vec![
        Variant {
            label: "A".into(),
            ty: Type::Unit,
        },
        Variant {
            label: "B".into(),
            ty: Type::Nat,
        },
        Variant {
            label: "C".into(),
            ty: Type::Nat,
        },
    ])
}
fn nat_list() -> Type {
    Type::Rec(Box::new(Type::Variant(vec![
        variant!("Nil", Type::Unit),
        variant!("Cons", Type::Product(vec![Type::Nat, Type::Var(0)])),
    ])))
}

fn nat_list2() -> Type {
    Type::Variant(vec![
        variant!("Nil", Type::Unit),
        variant!("Cons", Type::Product(vec![Type::Nat, Type::Var(0)])),
    ])
}

fn main() {
    let mut ctx = types::Context::default();

    ctx.alias("Var".into(), test_variant());
    ctx.alias("NatList".into(), nat_list());
    ctx.alias("NB".into(), nat_list2());

    let args = env::args();
    if args.len() > 1 {
        for f in args.skip(1) {
            println!("reading {}", f);
            let file = std::fs::read_to_string(&f).unwrap();
            let (_, eval_succ, msg) = testing::parse_and_eval(&mut ctx, &file, true);
            if !eval_succ {
                panic!("test failed! {:?} {:?}", f, msg);
            }
        }
        return;
    }

    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();

        testing::parse_and_eval(&mut ctx, &buffer, true);
    }
}

*/
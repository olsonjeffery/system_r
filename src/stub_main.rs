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
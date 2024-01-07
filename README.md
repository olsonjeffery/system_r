# system_r

An extension of the simply typed lambda calculus with parametric polymorphism (known more-widely as ["System F"](https://en.wikipedia.org/wiki/System_F)). Forked from a reasonably complete base implementation, originally provided in the `06_system_f` and `util` crates from [lazear/types-and-programming-languages](https://github.com/lazear/types-and-programming-languages) the lexer, parser, type_checker, and eval-based interpreter have been extended to support wider use, as defined by subsequent contributors.

Consult the gherkin specs in the `/specs/features` directory for more on `system_r`'s capabilities, embedding, etc.

From original README:

> `system_f` contains a parser, typechecker, and evaluator for the simply typed lambda calculus with parametric polymorphism (System F). The implementation of System F is the most complete so far, and I've tried to write a parser, typechecker and diagnostic system that can given meaningful messages

# Structure

The git repository is a Rust workspace containing multiple folders/crates:

- `/system_r` (`system_r` crate): core implementation crate of lex/parse -> type_checker; all terms/kinds/types, dialect API contract, etc defined here
- `/eval` (`system_r_eval` crate): implementation + testing interface for interpreter-based evaluation of a BottomDialect `system_r` term
- `/dialects` (`system_r_dialects` crate): contrib-style additional extension dialects for `system_r`
- `/specs` (`system_r_specs` crate): cucumber/gherkin-style features+test steps+state; depends upon all crates above

### pre-commit ritual:
- `cargo fmt && cargo clippy && cargo test`

# Licenses

This project is licensed under either of

- Apache License, Version 2.0, (`LICENSE-APACHE` or https://www.apache.org/licenses/LICENSE-2.0)
- MIT license (`LICENSE-MIT` or https://opensource.org/licenses/MIT)

at your option.

# Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in `system_r` by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.

# Copyright

All changes are copyright the contributors
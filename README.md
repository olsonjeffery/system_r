# System R

An extension of the simply typed lambda calculus with parametric polymorphism (known classically as System F). Starting from the base provided for `06_system_f` and `util` under `lazear/types-and-programming-languages`, the lexer, parser, type_checker and eval system have been extended to support more "industrial use cases", as defined by subsequent author(s).

Consult the gherkin specs in the `/tests/features` directory for more on system_r's capabilities, embedding, etc.

# Licenses

This project is licensed under either of

- Apache License, Version 2.0, (`LICENSE-APACHE` or https://www.apache.org/licenses/LICENSE-2.0)
- MIT license (`LICENSE-MIT` or https://opensource.org/licenses/MIT)

at your option.

# Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in system_r by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.

### pre-commit ritual:
- `cargo fmt && cargo clippy && cargo test`

# Copyright

All changes are copyright the contributors

# Original System F docs from lazear/types-and-programming-languages main README

- `system_f` contains a parser, typechecker, and evaluator for the simply typed lambda calculus with parametric polymorphism (System F). The implementation of System F is the most complete so far, and I've tried to write a parser, typechecker and diagnostic system that can given meaningful messages
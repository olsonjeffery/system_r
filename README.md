# System R

An extension of the simply typed lambda calculus with parametric polymorphism (known classically as System F). Starting from the base provided for `06_system_f` and `util` under `lazear/types-and-programming-languages`, the lexer, parser, type_checker and eval system have been extended to support more "industrial use cases", as defined by subsequent author(s).

Consult the gherkin specs in the `/tests/features` directory for more on system_r's capabilities, embedding, etc.

# Licenses:

Code in this repository is dual-licensed under the following licenses, as noted below:

- Remaining "original", imported repository files & content from the forked `lazear/types-and-programming-languages` is licensed under the MIT/Expat License, provided in-full at `/LICENSE.MIT`
- After this import in September of 2023, all subsequent contributions (whether as modification to existing repository file contents or added repository files) are made under the terms of the GNU Lesser General Public License Version 3, provided in full at `LICENSE.LGPL3`

# Copyright

- MIT-licensed changes are Copyright Michael Lazear, 2020.
- All subsequent changes under LGPL v3 is attributed to entrants in AUTHORS.md

Every code file in the repository is annotated with one, or both the MIT/Expat and GNU LGPL3 license headers, referencing the `LICENSE.*` files outlined above.

# Original System F docs from lazear/types-and-programming-languages main README

- `system_f` contains a parser, typechecker, and evaluator for the simply typed lambda calculus with parametric polymorphism (System F). The implementation of System F is the most complete so far, and I've tried to write a parser, typechecker and diagnostic system that can given meaningful messages
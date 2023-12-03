@system_r
Feature: Technical Debt
    #
    # Incomplete list:
    # - DONE Supertraits to clean up signatures
    # - pervasive Result<> everywhere;
    #   - DONE use anyhow!
    #   - panic removal/convert-API to result
    #     - all visitors (Result<()>)
    #     - lexer->parser
    #     - typechecker .. ?
    # - adopt git2rs pre-commit hook
    # - rustdoc-documentation of entire API surface
    #    - #[deny(missing_docs)]
    #    - lots of error analysis
    # - Rationalize state & functionality across system_r
    #    - parser is broken into functions and ParserState; can it be re-unified into struct-state + impl? tried once and failed
    #    - SystemRExtensions are reified currently as a marker type/empty struct (has Copy trait) implementing a trait + dialect-state;
    #    - so along with Lexer/Context, which are structs + impl methods & parser; we have 3 distinct layouts for functionality + state;
    #    - fix this
    # - clone->Rc, audit for removal
    # - DONEish clippy patrol; `cargo clippy` is green.. but can be stricter? how far?
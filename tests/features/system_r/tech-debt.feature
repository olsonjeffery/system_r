@system_r
Feature: Technical Debt
    #
    # Incomplete list:
    # - DONE Supertraits to clean up signatures
    # - pervasive Result<> everywhere; use anyhow?
    # - adopt git2rs pre-commit hook
    # - rustdoc-documentation of entire API surface
    # - Rationalize state & functionality across system_r
    #    - parser is broken into functions and ParserState; can it be re-unified into struct-state + impl? tried once and failed
    #    - SystemRExtensions are reified currently as a marker type/empty struct (has Copy trait) implementing a trait + dialect-state;
    #    - so along with Lexer/Context, which are structs + impl methods & parser; we have 3 distinct layouts for functionality + state;
    #    - fix this
    # - clone->Rc, audit for removal
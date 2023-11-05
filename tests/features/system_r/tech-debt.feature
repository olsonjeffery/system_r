@system_r
Feature: Technical Debt
    #
    # Incomplete list:
    # - clone->Rc, audit for removal
    # - Parser is broken into functions and ParserState; can it be re-unified into struct-state + impl? tried once and failed
    # - in line with above, rationalize state & functionality across system_r
    #    - SystemRExtensions are reified currently as a marker type/empty struct implementing a trait + dialect-state;
    #    - so along with Lexer/Context, which are structs + impl methods & parser; we have 3 distinct layouts for functionality + state;
    #    - fix this
    # - adopt git2rs pre-commit hook
    # - rustdoc-documentation of entire API surface
    # - pervasive Result<> everywhere; use anyhow?
@system_r
Feature: Technical Debt
    #
    # Incomplete list:
    # - pervasive Result<> everywhere;
    #   - panic removal/convert-API to result
    #     - all visitors (Result<()>)
    #     - lexer->parser
    #     - typechecker .. ?
    # - adopt git2rs pre-commit hook
    # - rustdoc-documentation of entire API surface
    #    - #[deny(missing_docs)]
    #    - lots of error analysis
    # - clone->Rc, audit for removal
@system_r
Feature: Technical Debt
    #
    # Incomplete list:
    # - pervasive Result<> everywhere;
    #   - panic removal/convert-API to result (45 remaining)
    #     - PatternCount & all collects that take ext
    #     - dialect
    #         - lexer->parser
    #         - typechecker .. ?
    #     - eval (lift out to romeo!)
    # - adopt git2rs pre-commit hook
    # - rustdoc-documentation of entire API surface
    #    - #[deny(missing_docs)]
    #    - add DOCUMENTATION.md with executable/test-code recreating what's in testing.mod for some bottom dialect code
    # - clone->Rc, audit for removal
    # - specs become freestanding crate, have feature romeo feature where
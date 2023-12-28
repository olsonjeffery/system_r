@system_r
Feature: Technical Debt
    #
    # Incomplete list:
    # - pervasive Result<> everywhere;
    #   - panic removal/convert-API to result (45 remaining)
    #     - Add SystemRError
    #       - unify & subsume all panics, Type/Parser error, DiagnosticInfo types
    #     - panic sites
    #       - PatternCount & all collects that take ext
    #       - dialect
    #           - lexer->parser
    #           - typechecker .. ?
    #       - eval (lift out to romeo!)
    # - rename PlatformBindings to just Bindings
    # - rustdoc-documentation of entire API surface
    #    - #[deny(missing_docs)]
    #    - add DOCUMENTATION.md with executable/test-code recreating what's in testing.mod for some bottom dialect code
    # - increase coverage for new feedback::catalog
    # - specs become freestanding crate, double-up testing for romeo-interp & wasm backends
    # - adopt git2rs pre-commit hook (being done manually, intermittently)
    # - LOW clone->Rc, audit for removal
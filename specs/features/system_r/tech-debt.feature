@system_r
Feature: Technical Debt
    #
    # Incomplete list:
    # - rename PlatformBindings to just Bindings
    # - panic site in lexer->parser
    # - rustdoc-documentation of entire API surface
    #   - #[deny(missing_docs)]
    #   - add DOCUMENTATION.md with executable/test-code recreating what's in testing.mod for some bottom dialect code
    # - further feedback work:
    #   - remove ErrorKind::ExtendedError, update where aprop
    #   - thread spans all throughout system, especially into ALL SystemRExtension impls
    # - proj reorg
    #   - eval/testing -> system_r_eval_ref_impl; refactor steps to use this;
    #   - add system_r_dialects crate
    # - tests
    #   - !! use coverage to drive additional test creation; cover the entire language
    #   - port system_r unit tests to specs, dump macros
    # - adopt git2rs pre-commit hook (being done manually, intermittently)
    # - LOW clone->Rc, audit for removal
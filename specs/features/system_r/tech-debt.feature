@system_r
Feature: Technical Debt
    #
    # Incomplete list:
    # - TypeAliasDialect -> DataAbsDialect
    #   - what is done currently in TypeAliasDialect should be reworked, removing the 'type' keyword/form
    #   - replace with `data` expr keyword that takes the "type shape" (in TypeAlias, type decl between the = and in)
    #   - result of `data` can be stored in let (use UppercaseIdentifiers; lose leading $); TypeAliasApp -> DataApp
    # - test improvement
    #   - port system_r unit tests to specs, dump macros
    #     - eval tests are commented-out, should be re-impl'd
    #   - address all FIXMEs associated with blocks turned into specs from system-f.srpt
    #   - !! use coverage to drive additional test creation; cover the entire language
    # - panic site in lexer->parser
    # - rustdoc-documentation of entire API surface
    #   - #[deny(missing_docs)]
    #   - add DOCUMENTATION.md with executable/test-code recreating what's in testing.mod for some bottom dialect code
    # - further feedback work:
    #   - remove ErrorKind::ExtendedError, update where aprop
    #   - thread spans all throughout system, especially into ALL SystemRExtension impls
    #   - type_check appears to be disconnected from span..
    # - adopt git2rs pre-commit hook (being done manually, intermittently)
    # - LOW clone->Rc, audit for removal
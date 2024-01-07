Feature: Pending dialects

    # Scenario: Add letrecs to the language, modeled as:
    # letrec [<decl>, ..] <tail-item>, where:
    #   - decl is declaration, array where each entry is a decl as would be done:
    #      - type ||$Option = \L {Some L | None}|| in ... <- items within || could be a single "decl"
    #      - let ||someGenericFun = \X \c: $Option[of X]. ... || in ... <-- same as above
    #   - A letrec can have zero-or-more bindings, delimited by comma and within the square brackets [ ]
    #   - The identifiers ($Option or someGenericFun above) exposed in letrecs are recursively bound
    #     and available to each other within the scope of the letrec
    #   - tail-item is one of:
    #      - a literal value (Nat, Bool, Bytes, etc)
    #      - a fn decl (eventually also multi-fun, when MultiFunDialect is added)
    #      - a type-shape decl (as in TypeAliasDialect)
    #   - within letrec paradigm, the letrec must be the top-level term of the program,
    #     but Bindings invoked in the [decls] that load other letrecs and evaluate to their
    #     tail-item (defined outside of the current letrec, ie some arbitrary module system) 
    #     would produce "hole-out" programs (or neccesitate a bail-out until all external
    #     letrecs+tail-items are on-hand)
    #   - Once all letrecs are on-hand, they can assembled and represented "upside-down" as
    #     a nested series of let/type+=+in... blocks that represents a "whole program"
    #     (this is left unimplemented in the dialect and is the concern of a compiler ecosystem, eg romeo)

    # Scenario: multi-fn : signature-overload union-type of fn-sig+body entries; each must be unique (use tags)

    # Scenario: extension: record literal kind, type, pattern-matching
@system_r
Feature: Platform Bindings
    #- rust-implemented, aliased fn bindings for Nat arithmetic + logic:
    #    - need generlized facility to bind identifiers to expressable funs w/ params 
    #        - first api for total functions
    #        - subsequent API that captures ARE syntax; expressed as decl?
    #        - need macros
    #    - just functions bound to rust! leave dyad-impl out of system_r
    #    - there will likely be higher-level APIs to emit these binding instructions; used's by an `rstrong_input`

    Scenario: Nat arith functions for usize literal for platform
        Given a new ctx
        And an instrinsic for Nat addition named iiiNatAdd
        And a srpt block:
        """
        let v = 22 in
            iiiNatAdd (v, 10)
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        And the last eval should be successful
        And the resulting eval Kind should be Nat of 32

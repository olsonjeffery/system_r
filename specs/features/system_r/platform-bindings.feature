@system_r
Feature: Platform Bindings
    #- rust-implemented, aliased fn bindings for Nat arithmetic + logic:
    #    - need generlized facility to bind identifiers to expressable funs w/ params 
    #        - v1 api for total functions
    #        - subsequent API that captures TARE syntax; expressed as decl?
    Scenario: Recursive fib() with natAdd & natSub
        Given a new type checker
        And platform bindings for Nat add & sub
        And a srpt block:
        """
let fib = \z: Nat->Nat. \i: Nat.
        case i of
        | 1 => 1
        | 0 => 1
        | n => natAdd( z(natSub(n, 1)), z(natSub(n, 2)) ) in
(fix fib) 4;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        And the last eval should be successful
        And the resulting eval Kind should be Nat of 5

    Scenario: Proper eager eval of nested PBs
        # The regression this captures is to ensure that the
        # arguments to a PB are evaluated-down to normal form
        # before the PB itself is invoked
        Given a new type checker
        And an instrinsic for Nat addition named iiiNatAdd
        And a srpt block:
        """
        let v = 1 in
            iiiNatAdd (v, iiiNatAdd (v, v))
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        And the last eval should be successful
        And the resulting eval Kind should be Nat of 3
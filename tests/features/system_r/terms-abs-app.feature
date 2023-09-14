@system_r
Feature: Terms Function ABS APP
    Scenario: Function Abstraction (ABS)
        Given a new system_r context
        Given a srpt block:
        # Nat identity function abs
        """
        \x: Nat. x
        """
        When it evals successfully
        Then the resulting eval Kind should be a fn abs
    
    Scenario: Function Application (APP)
        Given a new system_r context
        Given a srpt block:
        # applying Nat literal to the Nat identity fn
        """
        (\x: Nat. x)64
        """
        When it evals successfully
        Then the resulting eval Kind should be Nat of 64
    
    Scenario: tuple arg to fn for multi-arg app
        Given a new system_r context
        Given a srpt block:
        """
        let x = (\struct: (Nat, Nat, Nat). 
        let (_, q, _) = struct
        in q) in
        x (10, 12, 13)
        """
        When it evals successfully
        Then the resulting eval Kind should be Nat of 12
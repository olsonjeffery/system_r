@system_r
Feature: Terms Function ABS APP
    Scenario: Function Abstraction (ABS)
        Given a new type checker
        Given a srpt block:
        # Nat identity function abs
        """
        \x: Nat. x
        """
        When it evals successfully
        Then the resulting eval Kind should be a fn abs
    
    Scenario: Function Application (APP) (w/o let)
        Given a new type checker
        Given a srpt block:
        # applying Nat literal to the Nat identity fn
        """
        (\x: Nat. x)64
        """
        When it evals successfully
        Then the resulting eval Kind should be Nat of 64
    
    Scenario: tuple arg to fn for multi-arg app
        Given a new type checker
        Given a srpt block:
        """
        let x = (\struct: (Nat, Nat, Nat). 
        let (_, q, _) = struct
        in q) in
        x (10, 12, 13)
        """
        When it evals successfully
        Then the resulting eval Kind should be Nat of 12
    
    Scenario: poly fn abs + app
        # Hint: `poly` is a polymorphic identity function
        Given a new type checker
        Given a srpt block:
        """
let poly = \X \x: X. x in 
	let x = poly [Nat] 0 in 
	let y = poly [Bool] false in 
	let z = poly [(Nat, Bool)] in 
	z (x, y)
;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful
        Then the final value after eval should equal: "(0, false)"

    Scenario: poly fn abs + app from system-f.srpt 2
        Given a new type checker
        Given a srpt block:
        """
let poly = \X \Y (\func: X->Y. \val: X. func val) in
poly [Nat][Bool]
;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful



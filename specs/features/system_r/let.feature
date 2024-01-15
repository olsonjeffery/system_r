@system_r
Feature: let; let polymorphism
    Scenario: fun with let from system-f.srpt 1
        # FIXME: from system-f.srpt; improve
        Given a new type checker
        Given a srpt block:
        """
let x = 10 in let (y, _) = (x, 1) in y;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful

    Scenario: fun with let from system-f.srpt 2
        # FIXME: from system-f.srpt; improve
        Given a new type checker
        Given a srpt block:
        """
let (x, y) = (0, 10) in let z = x in z ;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful

    Scenario: fun with let from system-f.srpt 3
        # FIXME: from system-f.srpt; improve
        Given a new type checker
        Given a srpt block:
        """
(\x: Nat. \Y \y: Nat->Y. y x) 10 [Nat] succ;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful

    Scenario: fun with let from system-f.srpt 4
        # FIXME: from system-f.srpt; improve
        Given a new type checker
        Given a srpt block:
        """
let x = (\struct: (Nat, Nat, Nat). 
	let (_, q, _) = struct
	in q) in
	x (10, 12, 13);
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful

    Scenario: fun with let from system-f.srpt 4
        # FIXME: from system-f.srpt; improve
        Given a new type checker
        Given a srpt block:
        """
let x = \A \B \C \tuple: (A, B, (C, C)).
	let (_, mid, (n, s)) = tuple in 
	(n, mid, s, mid) in
	x [Nat] [Bool] [Nat] (10, true, (1, 11));
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful
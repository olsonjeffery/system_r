@system_r
Feature: RStrong: System R Hello World
    Scenario: System R with generic identity function, use Bool
        Given a new type checker
        Given a srpt block:
        """
let id = \X \x: X. x in
id [Bool] (true);
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the resulting eval Kind should be Boolean true

    Scenario: System R with generic identity function, use Nat
        Given a new type checker
        Given a srpt block:
        """
let id = \X \x: X. x in
id [Nat] (42);
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful
        Then the resulting eval Kind should be Nat of 42

    Scenario: System R with non-generic succ hello world
        Given a new type checker
        Given a srpt block:
        """
let incr = \x: Nat. succ(x) in
incr (31);
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the resulting eval Kind should be Nat of 32
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

    Scenario: polymorphic abs+app, case, destructuring
        Given a new type checker
        Given a srpt block:
        """
let func = \X (\c: {None | Some X}. \x: X->(X, X). 
	case c of 
		| None => None of {None | Some (X, X)}
		| Some val => Some x val of {None | Some (X, X)} ) in
let Some result = func [Nat] (Some 10 of {None|Some Nat}) (\x: Nat. (x, x)) in
result;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful
        Then the final value after eval should equal: "(10, 10)"

    Scenario: fun with case
        Given a new type checker
        Given a srpt block:
        """let a = case Some (5, 2) of {None | Some (Nat, Nat)} of 
 | None => (0, 0)
 | Some (1, _) => (1, 1)
 | Some(x, y) => (y, x) in
a;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful
        Then the final value after eval should equal: "(2, 5)"

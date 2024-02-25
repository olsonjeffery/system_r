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

    Scenario: Run "system-f.srpt" script
        # NOTE: this is from system-f.srpt, a script of system-f code inherited from
        # the original author/github-repo; it exercises a bunch of different features
        # of the language as a bunch of separate statements; not really written for
        # examining results and verifying correctly, more that it parses/tycks/evals
        # and that is good; you can also examine the output, but some blocks aren't
        # really written for deriving infomation
        Given a new type checker
        Given a srpt block:
        """
let func = \X (\c: {None | Some X}. \x: X->(X, X). 
	case c of 
		| None => None of {None | Some (X, X)}
		| Some val => Some x val of {None | Some (X, X)} )
in func [Nat] (Some 10 of {None|Some Nat}) (\x: Nat. (x, x))
;


case Some (5, 2) of {None | Some (Nat, Nat)} of 
 | None => (0, 0)
 | Some (1, _) => (1, 1)
 | Some(x, y) => (y, x)
;

case (1, (2, 3)) of 
	| (x, (y, z)) => ((z, y), x)
;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful

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

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
let x = 
	\z: (Nat, Nat)->Nat. 
		\y: (Nat, Nat).
			case y of
				| (0, x) => x,
				| x => z (pred y.0, succ (succ x.1))
	in (fix x) (10, 0)

;

let cdr = \list: NatList. 
	case unfold NatList list of 
		| Nil => Nil of NatList
		| Cons (x, xs) => xs
in cdr Cons (10, Cons (20, Nil of NatList) of NatList) of NatList
;

case unfold NatList Cons (10, Cons (20, Nil of NatList) of NatList) of NatList of 
	| Nil => Nil of NatList 
	| Cons (10, xs) => Cons (11, xs) of NatList
	| Cons (x, xs) => xs
;

let nil = Nil of NatList in 
let cons = (\val: Nat. \list: NatList. Cons (val, list) of NatList) in
case unfold NatList (cons 1 nil) of 
	| Nil => nil
	| Cons (x, y) => y

;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful

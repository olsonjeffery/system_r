@system_r
Feature: Recursion
    Scenario: functional recursion with fix
        Given a new type checker
        Given a srpt block:
        """
let x = 
	\z: (Nat, Nat)->Nat. 
		\y: (Nat, Nat).
			case y of
				| (0, x) => x,
				| x => z (pred y.0, succ (succ x.1))
	in (fix x) (10, 0)
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful
        Then the final value after eval should equal: "20"

    Scenario: type-level recursion via unfold
        # NB: This is HIDEOUS because of the need to repeat
        # the type-shape of `rec MyNatList = {Nil | Cons (Nat, MyNatList)}`
        Given a new type checker
        Given a srpt block:
        """
let car = \list: rec MyNatList = {Nil | Cons (Nat, MyNatList)}. 
	case unfold rec MyNatList = {Nil | Cons (Nat, MyNatList)} list of 
		| Nil => 0
		| Cons (x, xs) => x
in car Cons (10, Nil of rec MyNatList = {Nil | Cons (Nat, MyNatList)})
    of rec MyNatList = {Nil | Cons (Nat, MyNatList)}
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful
        Then the final value after eval should equal: "10"

    Scenario: Function+Type recursion via fix+unfold
        Given a new type checker
        Given a srpt block:
        """
let l = \r:(rec MyNatList = {Nil | Cons (Nat, MyNatList)})->Nat.
        \list: rec MyNatList = {Nil | Cons (Nat, MyNatList)}. 
	case unfold rec MyNatList = {Nil | Cons (Nat, MyNatList)} list of 
        | Nil => 0
        | Cons (x, xs) => (case unfold rec MyNatList = {Nil | Cons (Nat, MyNatList)} xs of
            | Nil => x
            | Cons(xx, xxs) => (r Cons (xx, xxs) of rec MyNatList = {Nil | Cons (Nat, MyNatList)})) in
(fix l) Cons (10, Cons (20, Nil of rec MyNatList = {Nil | Cons (Nat, MyNatList)})
    of rec MyNatList = {Nil | Cons (Nat, MyNatList)})
        of rec MyNatList = {Nil | Cons (Nat, MyNatList)}
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful
        Then the final value after eval should equal: "20"
@system_r
Feature: pack / unpack / exists examples
    Scenario: Basic pack / unpack
        # FIXME: from system-f.srpt; improve
        Given a new type checker
        Given a srpt block:
        """
let package = (pack Nat, ((\x: Nat. succ (succ x)), 0) as exists X. (X->Nat, X)) in
	unpack package as T, mod in mod.0 ((\x: T. x) mod.1)
;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful

    Scenario: Basic pack/unpack + exists
        # FIXME: from system-f.srpt; improve
        Given a new type checker
        Given a srpt block:
        """
let package = (pack Bool, ((\x: Bool. case x of | true => 10 | false => 0), true) as exists REPR. (REPR->Nat, REPR)) in
let x = (\x: exists T. (T->Nat, T). unpack x as T, mod in succ (mod.0 mod.1)) in
	x package 
;
        """
        When it is parsed and evaluated
        Then the last parse should be successful
        Then the last eval should be successful

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
        Then the resulting eval Kind should be Nat of 2

    Scenario: Existential Types; pack+exists, pass-by-existential-signature unpack
        # The first line is a tough pill to swallow, a lot is happening:
        # 1. the `pack` keyword declaring a "packed value", along with an existential type
        #    specifying it's "shape", along with a concrete type (Bool)
        # 2. AND THEN a concrete tuple value, where the first arg is a lambda of type Bool->Nat
        #    and the second arg is a Bool value (of true)
        # 3. AND THEN the `as` keyword, separate the "concrete" section (that came first) from
        #    the definition of the existential's "shape", conists of:
        # 4. the `exists` keyword and then a new REPR TypeDecl, it is applied to show a type
        #    shape consisting of a tuple containing two args: 1) a lambda of REPR->Nat and
        #    2) a value of type REPR; this summarizes the abstract type-shape of the concrete
        #    "packed module" or whatever you want to call it that precedes
        # 5. All of the above is stored in the value 'package', via the `let`, the use of
        #    the `package` value proceeds after the `in` keyword that ends the first line
        #
        # To summarize: Declares a packed/modulized value, along with it's type-shape and stores
        # in the name bound by the `let` keyword
        #
        # The second line stores a lambda that takes as it's sole input an existential (adding a
        # T type-decl in the existential and NOT the lambda itself) that happens to conforms to
        # the type-shape of `package` declared previously; this lambda is stored as `x`
        #
        # The third and final line applies `package` to `x`
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
        Then the resulting eval Kind should be Nat of 11

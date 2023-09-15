@system_r
Feature: Extension of system_r
    # Notes: All-in, a "new extension" will require something like:
    #
    #   + ExtTokenKind<TExtTokenKind> (mandatory) - additions to TokenKind in syntax::lexer
    #   + ExtKind<TExtKind> (mandatory) - additions in syntax::parser
    #   + ExtType<TExtType> (mandatory) - additions for types::Context type_check suite
    #   + ExtPattern<TExtPat> (mandatory) - additions to type_check_case in patterns.rs
    #   + ExtResolver<TExtKind, TExtType> (mandatory) - to type any single extension
    #     into vanilla system_r instructions
    #   + CrossExtResolver<TExtKind1, TExtKind2, TExtTy1, TExtTy1>
    #     (optional) - allows multiple impls for a set of extensions, enables converting one
    #     extended lambda calculus to another extended calculus
    #
    # Notes: Eval ONLY operates against TExtKind<BottomKind

    @wip
    Scenario: tylet happy path
        # Add structural type/data-shape capture with the tylet
        # keyword, all tylet alias application with $TypeName[Of V]
        # where V is a TyAbs in the tylet value
        # tylet $TypeAlias = \V. \K {Left V | Right K} in body
        #
        # For this scenario, the below code will convert to
        # vanilla system_r equiv to:
        #
        # let tripler = \X (\c: {None | Some X}. \x: X->(X, X, X). 
	    #   case c of 
		#     | None => None of {None | Some (X, X, X)}
		#     | Some val => Some x val of {None | Some (X, X, X)} ) in
        # let Some(res) = tripler [Nat] (Some 7 of {None|Some Nat}) (\x: Nat. (x, x, x)) in
        # res ;

        Given a system_r toolchain extended for tylet
        And a code block:
        """
tylet $Option = \V {None | Some V} in
let tripler = \X (\c: $Option[of X]. \x: X->(X, X, X). 
	case c of 
		| None => None of $Option[of (X, X, X)]
		| Some val => Some x val of $Option[of (X, X, X)] ) in
let Some(res) = tripler [Nat] (Some 7 of $Option[of Nat]) (\x: Nat. (x, x, x)) in
res ;
        """
        When it is processed for the tylet extension
        Then the last ext should parse successfully
        #When it is converted to bottom-dialect system_r
        #When eval is ran
        #Then the last eval should be successful
        #Then the resulting eval Kind should equal: "(7,7,7)"

    #
    #   - Just like let, but capturing a type decl (scalar, product, variant, who care!)
    #   - As a Uppercase var (we will say NOT a TyAbs, but shouldn't collide w/ local TyAbs, etc)
    #     - maybe leading $ ?
    #     - make all TyAbs leading ? lol?
    #     - needs to include application, eg tylet $List = X/ { Nil | Cons ($List[of X]) }

    # Scenario: tylet pattern matching/destructing? more application?
     
    # Scenario: Exclude/enforce: allow specifying constraints where certain 
    # TokenKinds/Kinds/Types are kept OUT of a Term-tree
    #
    # e.g. Extension for system_r + TearAbs/App (system_tear?) that forbids "vanilla" lambda
    # abs and app, instead encapsulating those within generating vanilla system_r that is the
    # SystemRResolver-output for the extension

    # Scenario: extension: record literal kind, type, pattern-matching
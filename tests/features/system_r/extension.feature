@system_r
Feature: Extension of system_r
    @wip
    Scenario: StructData happy path
        # Add structural type/data-shape capture with the StructData
        # keyword, all StructData alias application with $TypeName[Of V]
        # where V is a TyAbs in the StructData value
        # StructData $TypeAlias = \V. \K {Left V | Right K} in body
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

        Given a system_r toolchain extended for StructData
        And a code block:
        """
type $Option = \V {None | Some V} in
let tripler = \X (\c: $Option[of X]. \x: X->(X, X, X). 
	case c of 
		| None => None of $Option[of (X, X, X)]
		| Some val => Some x val of $Option[of (X, X, X)] ) in
let Some(res) = tripler [Nat] (Some 7 of $Option[of Nat]) (\x: Nat. (x, x, x)) in
res ;
        """
        When it is processed for the StructData extension
        And StructData-dialect is resolved into bottom-dialect system_r
        And eval is ran
        Then the last ext should parse successfully
        Then the last parse should be successful
        Then the last eval should be successful
        Then the resulting eval Kind should equal: "(7,7,7)"

    # Scenario: type decl with no tyabs

    # Scenario: type decl with multiple tyabs

    # Scenario: StructData pattern matching/destructing? more application?
     
    # Scenario: Exclude/enforce: allow specifying constraints where certain 
    # TokenKinds/Kinds/Types are kept OUT of a Term-tree
    #
    # e.g. Extension for system_r + TearAbs/App (system_tear?) that forbids "vanilla" lambda
    # abs and app, instead encapsulating those within generating vanilla system_r that is the
    # SystemRResolver-output for the extension

    # Scenario: extension: record literal kind, type, pattern-matching
@system_r
Feature: Add new SystemRDialect implementations
    # Add structural type/data-shape capture (with arity-based TyAbs) with the `type`
    # keyword, all TypeAlias application with $TypeName[Of V]
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

    Scenario: TypeAliasDialect happy path

        Given a system_r toolchain extended for TypeAlias
        And a code block:
        """type $Option = \V {None | Some V} in
let tripler = \X (\c: $Option[X]. \x: X->(X, X, X). 
	case c of 
		| None => None of $Option[(X, X, X)]
		| Some(val) => Some x val of $Option[(X, X, X)] ) in
let Some(res) = tripler [Nat] (Some 7 of $Option[Nat]) (\x: Nat. (x, x, x)) in
res ;
        """
        When TypeAlias parses the code
        And TypeAlias type checks the code
        And TypeAliasDialect is resolved into BottomDialect system_r
        And type_check and eval for BottomDialect is ran
        Then the last ext should parse successfully
        And the last parse should be successful
        And the last eval should be successful
        Then the final value after eval should equal: "(7,7,7)"

    Scenario: TypeAlias with multiple tyabs
        Given a system_r toolchain extended for TypeAlias
        And adding an instrinsic named iiiNatAdd to the "TypeAlias" context
        And a code block:
        """
type $Either = \L \R {Left L | Right R } in
let toNat = \I (\c: $Either[I, Nat]. \x: I->Nat.
	case c of 
		| Left(l) => x l
		| Right(r) => r ) in
toNat [Bool] (Left true of $Either[Bool, Nat]) (\x: Bool. case x of | true => 1 | false => 0);
        """
        When TypeAlias parses the code
        And TypeAlias type checks the code
        And TypeAliasDialect is resolved into BottomDialect system_r
        And type_check and eval for BottomDialect is ran
        Then the last ext should parse successfully
        And the last parse should be successful
        And the last eval should be successful
        Then the resulting eval Kind should be Nat of 1

    Scenario: TypeAlias with no tyabs
        Given a system_r toolchain extended for TypeAlias
        And adding an instrinsic named iiiNatAdd to the "TypeAlias" context
        And a code block:
        """type $NatOption = {None | Some Nat} in
let doubler = \c: $NatOption.
	case c of 
		| None => None of $NatOption
		| Some(val) => Some iiiNatAdd(val, val) of $NatOption in
let Some(res) = doubler (Some 7 of $NatOption) in
res ;
        """
        When TypeAlias parses the code
        And TypeAlias type checks the code
        And TypeAliasDialect is resolved into BottomDialect system_r
        And type_check and eval for BottomDialect is ran
        Then the last ext should parse successfully
        And the last parse should be successful
        And the last eval should be successful
        Then the final value after eval should equal: "14"

    # Scenario: StructData pattern matching/destructing? more application?
     
    # Scenario: Exclude/enforce: allow specifying constraints where certain 
    # TokenKinds/Kinds/Types are kept OUT of a Term-tree
    #
    # e.g. Extension for system_r + TearAbs/App (system_tear?) that forbids "vanilla" lambda
    # abs and app, instead encapsulating those within generating vanilla system_r that is the
    # SystemRResolver-output for the extension

    # Scenario: extension: record literal kind, type, pattern-matching
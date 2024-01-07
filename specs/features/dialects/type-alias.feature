@system_r_dialects
Feature: The TypeAlias dialect
    # Add structural type/data-shape capture (let-style, with arity-based TyAbs) with the `type`
    # keyword, add TypeAlias application with $TypeName[Of V]
    # where V is a TyAbs in the type value

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
    
    # Scenario: TechDebt
    # - remove need for $-prefix, share Uppercase var namespace w/ tyabs?
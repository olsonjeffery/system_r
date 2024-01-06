@system_r
Feature: Add new SystemRDialect implementations
    # Add structural type/data-shape capture (with arity-based TyAbs) with the `type`
    # keyword, all TypeAlias application with $TypeName[Of V]
    # where V is a TyAbs in the type value
    # type $TypeAlias = \V \K {Left V | Right K} in body
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

    # Scenario: SystemRMultiDialect[DialectOne, DialectTwo]
    # - SystemRMultiDialect itself will impl SystemRDialect, with left-first precedence ordered
    #   merging of two dialects
    # - rework SystemRExtension trait contract to have pairs of want_foo() and do_foo() pairs;
    #   proceed in order and "choose" first dialect that "wants" any given "foo()" extension
    #   operation; return Err() if neither dialect wants
     
    # Scenario: Exclude/enforce: allow specifying constraints where certain 
    # TokenKinds/Kinds/Types are kept OUT of a Term-tree

    ## new dialects

    # Scenario: Add letrecs to the language, modeled as:
    # letrec [<decl>, ..] <tail-item>, where:
    #   - decl is declaration, array where each entry is a decl as would be done:
    #      - type ||$Option = \L {Some L | None}|| in ... <- items within || could be a single "decl"
    #      - let ||someGenericFun = \X \c: $Option[of X]. ... || in ... <-- same as above
    #   - A letrec can have zero-or-more bindings, delimited by comma and within the square brackets [ ]
    #   - The identifiers ($Option or someGenericFun above) exposed in letrecs are recursively bound
    #     and available to each other within the scope of the letrec
    #   - tail-item is one of:
    #      - a literal value (Nat, Bool, Bytes, etc)
    #      - a fn decl (eventually also multi-fun, when MultiFunDialect is added)
    #      - a type-shape decl (as in TypeAliasDialect)
    #   - within letrec paradigm, the letrec must be the top-level term of the program,
    #     but Bindings invoked in the [decls] that load other letrecs and evaluate to their
    #     tail-item (defined outside of the current letrec, ie some arbitrary module system) 
    #     would produce "hole-out" programs (or neccesitate a bail-out until all external
    #     letrecs+tail-items are on-hand)
    #   - Once all letrecs are on-hand, they can assembled and represented "upside-down" as
    #     a nested series of let/type+=+in... blocks that represents a "whole program"
    #     (this is left unimplemented in the dialect and is the concern of a compiler ecosystem, eg romeo)

    # Scenario: multi-fn : signature-overload union-type of fn-sig+body entries; each must be unique (use tags)

    # Scenario: extension: record literal kind, type, pattern-matching

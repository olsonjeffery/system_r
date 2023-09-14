@system_r
Feature: Literals
    # Scenario: Boolean
    # Scenario: Nat
    Scenario: Nat
        Given a new system_r context
        Given a srpt block:
        """
        let f = \x: (Nat, Nat).
            case x of
            | (x, 1) => 1
            | (_, 2) => 0
            | (x, 3) => x
            | _ => 0 in
            f (16, 3)
        """
        When it is parsed
        When eval is ran
        Then the last parse should be successful
        Then the last eval should be successful
        Then the resulting eval Kind should be Nat of 16

    # Scenario: Tag
        #- Tags: have two components: their kind (which all tags share) and their identifier name;
        #      any two tags that share the same name are equivalent for equality purposes;
        #    - anywhere an atomic typespec can appear, there can just be a tag literal
        #    - Should variant injection/type constructions with labels have their labels be in @TagFormat ?
    Scenario: Tag basic
        Given a new system_r context
        Given a srpt block:
        """
        @go
        """
        When it is parsed
        When eval is ran
        Then the last parse should be successful
        Then the last eval should be successful
        Then the resulting eval Kind should be Tag of "@go"

    @tag-impl
    Scenario: Tag in tuple typing
        Given a new system_r context
        Given a srpt block:
        """
let f = \x: (Nat, @Foo).
    case x of
    | (x, @Foo) => x
    | _ => 0 in
    f (2, @Foo)
        """
        When it is parsed
        When eval is ran
        Then the last parse should be successful
        Then the last eval should be successful
        Then the resulting eval Kind should be Nat of 2

    # Scenario: Float
    # Scenario: Decimal
    # Scenario: Signed Nat
    # Scenario: Hex

    # Scenario: Bytes / bin data
    # Scenario: strings (utf8 byte data)
    # Scenario: scientific/engineering notation for nat/float/decimal
    # Scenario: Octal
    # Scenario: regex literal


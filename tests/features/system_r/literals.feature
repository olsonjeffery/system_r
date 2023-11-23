@system_r
Feature: Literals
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

    Scenario: Tag basic
        # Tags: have two components: their kind (which all tags share) and their identifier name;
        # - any two tags that share the same name are equivalent for equality purposes;
        # - anywhere an atomic typespec can appear, there can just be a tag literal
        # - Should variant injection/type constructions with labels have their labels be in @TagFormat ?
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

    Scenario: Bytes / bin numeric literals
        Given a new system_r context
        Given a srpt block:
        # We will support a very inefficient 0-255 byte array encoding for demonstration purposes
        """
        let bytes = [71, 69, 84, 32, 47, 32, 72, 84, 84, 80, 47, 49, 46, 49, 13, 10, 72,
                111, 115, 116, 58, 32, 119, 119, 119, 46, 101, 120, 97, 109, 112, 108, 101,
                46, 99, 111, 109, 13, 10, 67, 111, 110, 110, 101, 99, 116, 105, 111, 110, 58,
                32, 99, 108, 111, 115, 101, 13, 10, 13, 10] in
        bytes;
        """
        When it is parsed
        When eval is ran
        Then the last parse should be successful
        Then the last eval should be successful
        Then the result should be Bytes equal to UTF8 string "GET / HTTP/1.1\r\nHost: www.example.com\r\nConnection: close\r\n\r\n"
    
    Scenario: Bytes numeric literal admits only 0-255
        # note: Nat is intrinsically an unsigned integer,
        # so we get the lower bound for free
        Given a new system_r context
        Given a srpt block:
        """
        let bytes = [71, 69, 256] in
        bytes;
        """
        When it is parsed
        When eval is ran
        Then the last parse should have failed
    
    # Scenario: Bytes in fn signature, case with exact match & catch-all arms
    # Scenario: zero-length Bytes with []

    # Scenario: Boolean
    Scenario: Boolean hello world
        Given a new system_r context
        Given a srpt block:
        """
        let id = \T \x: T. x in
        id [Bool] true
        """
        When it is parsed
        When eval is ran
        Then the last parse should be successful
        Then the last eval should be successful
        Then the result should be Boolean true

    # Scenario: Float

    # Scenario: Hex
    # Scenario: Octal
    # Scenario: scientific/engineering notation for nat/float

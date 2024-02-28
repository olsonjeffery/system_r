Feature: Plaintext
    Scenario: plaintext: Nat literal
        Given a new type checker
        And a code block:
"""42"""
        When it is parsed
        And it is type checked
        And it is converted to plaintext
        Then the plaintext should match the input code block

    Scenario: plaintext: Bool literal
        Given a new type checker
        And a code block:
"""false"""
        When it is parsed
        And it is type checked
        And it is converted to plaintext
        Then the plaintext should match the input code block

    Scenario: plaintext: Tag literal
        Given a new type checker
        And a code block:
"""@someTag"""
        When it is parsed
        And it is type checked
        And it is converted to plaintext
        Then the plaintext should match the input code block

    Scenario: plaintext: Unit literal
        Given a new type checker
        And a code block:
"""Unit"""
        When it is parsed
        And it is type checked
        And it is converted to plaintext
        Then the plaintext should match the input code block

    Scenario: byte literal
        Given a new type checker
        And a code block:
"""[71, 69, 84]"""
        When it is parsed
        And it is type checked
        And it is converted to plaintext
        Then the plaintext should match the input code block

    Scenario: plaintext: application of primitive + nat literal
        Given a new type checker
        And a code block:
"""succ 2"""
        When it is parsed
        And it is type checked
        And it is converted to plaintext
        Then the plaintext should match the input code block
    
    Scenario: plaintext: function abs, generic identity
        Given a new type checker
        And a code block:
"""\A \a: A. a"""
        When it is parsed
        And it is type checked
        And it is converted to plaintext
        Then the plaintext should match the input code block
        And the plaintext should eval to the same term as the input
    
    # todo .. carry varname info forward from lexer/parser into Terms metadata?
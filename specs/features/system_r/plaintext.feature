Feature: Plaintext
    Scenario: plaintext generated from parsed and type checked BottomDialect Term
        # NOTE: because it's possible for heteromorphisms always collapsing to
        # a single given form when converted to plaintext, we will try
        # and stick to input thats "survive" the plaintext-conversion process with
        # textual equivalence intact
        Given a new type checker
        And a code block:
"""let id = \X \x: X. x in
id [Bool] (true);
"""
        When it is parsed
        And it is type checked
        And it is converted to plaintext
        Then the last parse should be successful
        And the last type check should be successful
        And the plaintext should match the input code block
@test_infra
Feature: System R Parsing/EVal
    Scenario: code that has a lex/parse error
        Given a new ctx
        Given a srpt block:
        """
        let f = \X:x -> n.
        """
        When it is parsed
        Then the last parse should have failed
    # Scenario: code that has a typecheck error
    #   it should show a typecheck error in the output
    # Scenario: code that has an eval error
    #   it should show an eval error in output
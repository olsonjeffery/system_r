@system_r
Feature: Data
    Scenario: Tuple data
        Given a new type checker
        Given a srpt block:
        """
let f = (\x: (Bool, Nat).
  case x of
  | (true, x) => x
  | (false, _) => 0
  | _ => 0 ) in
f (false, 16);
        """
        When it evals successfully
        Then the resulting eval Kind should be Nat of 0

    # Scenario: variant data

    # Scenario: NEW fixed length homogenous array of T, pass by value

    # Scenario: NEW unsized slice of T, derived from array, passed by-ref
    # - ensure no unsafety
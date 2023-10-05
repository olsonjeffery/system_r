@system_r
Feature: Data
    Scenario: Tuple data
        Given a new ctx
        Given a srpt block:
        """
let f = (\x: (Bool, Nat).
  case x of
  | (true, x) => x
  | (false, _) => 0
  | _ => 0 ) in
f (true, 16);
        """
        When it evals successfully
        Then the resulting eval Kind should be Nat of 16
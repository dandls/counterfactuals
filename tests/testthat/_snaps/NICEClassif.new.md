# Returns message that `threshold` is ignored, when `correct_classif_only` is set to FALSE.

    Code
      cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0")
    Error <simpleError>
      Assertion on 'newdata' failed: Must be of type 'data.frame', not 'NULL'.

# Returns warning if no counterfactuals could be found

    Code
      cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0")
    Warning <simpleWarning>
      No counterfactuals could be found.


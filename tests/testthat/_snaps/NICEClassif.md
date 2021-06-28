# Returns message that `threshold` is ignored, when `correct_classif_only` is set to FALSE.

    Code
      nice_classif = NICEClassif$new(pred, optimization = "sparsity",
        correct_classif_only = FALSE, threshold = th)
    Message <simpleMessage>
      `threshold` is ignored, when `correct_classif_only` is set to FALSE.

# Returns warning if no counterfactuals could be found

    Code
      cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0")
    Warning <simpleWarning>
      No counterfactuals could be found.


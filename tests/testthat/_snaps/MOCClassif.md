# Returns correct output format for soft binary classification

    Code
      cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "0"))
    Message <simpleMessage>
      `x_interest` was removed from results.

# Returns correct output format for hard binary classification

    Code
      cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "versicolor",
        desired_prob = 1))
    Message <simpleMessage>
      `x_interest` was removed from results.

# Can handle non-numeric target classes

    Code
      cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "pos"))
    Message <simpleMessage>
      `x_interest` was removed from results.

# Can handle ordered factor input columns

    Code
      cfactuals = quiet(moc_classif$find_counterfactuals(x_interest, desired_class = "good",
        desired_prob = c(0.8, 1)))
    Message <simpleMessage>
      `x_interest` was removed from results.


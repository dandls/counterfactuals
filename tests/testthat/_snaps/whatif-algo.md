# whatif_algo returns warning and empty data.table with correct columns, if no counterfactuals were found

    Code
      res = whatif_algo(predictor = mod, n_cfactuals = 5L, x_interest = x_interest,
        pred_column = "pred", desired_y_hat_range = c(5, 10), X_search = mod$data$X,
        distance_function = NULL)
    Warning <simpleWarning>
      Could only find 0 counterfactual(s)

# whatif_algo returns error message if distance_function returns incorrect format

    `distance_function` must return a numeric matrix.


# whatif_algo returns warning and empty data.table with correct columns, if no counterfactuals were found

    Code
      res = whatif_algo(predictor = mod, param_set = ps, n_cfactuals = 5L,
        x_interest = x_interest, pred_column = "pred", desired_y_hat_range = c(5, 10))
    Warning <simpleWarning>
      Could only find 0 counterfactual(s)


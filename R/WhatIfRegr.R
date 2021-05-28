WhatIfRegr = R6::R6Class("WhatIfRegr",
  inherit = CounterfactualsRegr,
  public = list(
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, desired_outcome = NULL,
                          lower = NULL, upper = NULL) {
      
      super$initialize(predictor, lower, upper)
      assert_integerish(n_counterfactuals, lower = 1L, any.missing = FALSE, len = 1L)
      private$n_counterfactuals = n_counterfactuals
      
      if (!is.null(x_interest)) {
        self$find_counterfactuals(x_interest, desired_outcome)
      }
    }
  ),
  
  private = list(
    n_counterfactuals = NULL,
    
    run = function() {
      pred_column = private$get_pred_column()
      y_hat = setDT(private$predictor$predict(private$predictor$data$X))[[pred_column]]

      private$.results = whatif_algo(
        private$predictor$data$X, private$param_set, private$n_counterfactuals, private$x_interest, y_hat,
        private$desired_outcome
      )
    }
  )
)

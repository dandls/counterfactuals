WhatIfClassif = R6::R6Class("WhatIfClassif",
  inherit = CounterfactualsClassif,
  
  public = list(
    # TODO: for hard classification set desired_prob to 0 or 1
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, desired_class = NULL,
                          desired_prob = NULL, n_cores = 1L, lower = NULL, upper = NULL) {
      
      super$initialize(predictor, lower, upper)
      assert_integerish(n_cores, lower = 1L, any.missing = FALSE, len = 1L)
      assert_integerish(n_counterfactuals, lower = 1L, any.missing = FALSE, len = 1L)
      private$n_cores = n_cores
      private$n_counterfactuals = n_counterfactuals
      
      if (!is.null(x_interest)) {
        self$find_counterfactuals(x_interest, desired_class, desired_prob)
      }
    }
  ),
  
  private = list(
    n_cores = NULL,
    n_counterfactuals = NULL,
    
    run = function() {
      pred_column = private$get_pred_column()
      y_hat = setDT(private$predictor$predict(private$predictor$data$X))[[pred_column]]
      
      private$.results = whatif_algo(
        private$predictor$data$X, private$n_cores, private$param_set, private$n_counterfactuals, private$x_interest, 
        y_hat, private$desired_prob
      )
    }
    
  )
)

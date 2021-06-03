#' @export
WhatIfRegr = R6::R6Class("WhatIfRegr", inherit = CounterfactualMethodRegr,
                         
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
      whatif_algo(
        private$predictor, private$param_set, private$n_counterfactuals, private$x_interest, pred_column,
        private$desired_outcome
      )
    },
    
    print_parameters = function() {
      cat("\t", "n_counterfactuals: ", private$n_counterfactuals)
    }
  )
)

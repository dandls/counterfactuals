WhatIfRegr = R6::R6Class("WhatIfRegr",
  inherit = CounterfactualsRegression,
  private = list(
    WhatIfAlgoObj = NULL,
    y_hat = NULL,
    
    calculate = function() {
      pred_column = private$get_pred_column()
      private$WhatIfAlgoObj$run(private$x_interest, private$y_hat[[pred_column]], private$desired_outcome)
    },

    aggregate = function() {
      pred_column = private$get_pred_column()
      private$.results = private$WhatIfAlgoObj$get_results_list(pred_column)
    }
  
  ),
  public = list(
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, desired_outcome = NULL,
                          n_cores = 1L, lower = NULL, upper = NULL) {
      
      arg_list = as.list(environment())
      super$initialize(arg_list)

      private$WhatIfAlgoObj = WhatIfAlgo$new(private$predictor, n_cores, private$param_set, n_counterfactuals)
      private$y_hat = as.data.table(predictor$predict(predictor$data$X))
      
      if (!is.null(arg_list$x_interest)) {
        self$find_counterfactuals(arg_list$x_interest, arg_list$desired_outcome)
      }
    }
  )
)

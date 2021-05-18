#' @import data.table
#'
#' @export
WhatIf_Regr = R6::R6Class("WhatIf_Regr",
  inherit = CounterfactualsRegression,
  private = list(
    WhatIf_Algo_Obj = NULL,
    y_hat = NULL,
    
    calculate = function() {
      private$WhatIf_Algo_Obj$run(private$x_interest, private$y_hat[[1L]], private$desired_outcome)
    },

    aggregate = function() {
      private$.results = private$WhatIf_Algo_Obj$get_results_list(private$x_interest, y_hat_col = 1L)
    },
    
    run_init_arg_checks = function(param_list) {
      # TODO: Check if y is in X -> if yes remove and message
      # TODO: Arg type checks
    }
  
  ),
  public = list(
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, desired_outcome = NULL,
                          n_cores = parallel::detectCores() - 1L, lower = NULL, upper = NULL) {
      
      param_list = as.list(environment())
      super$initialize(param_list)

      private$run_init_arg_checks(param_list)
      private$WhatIf_Algo_Obj = WhatIf_Algo$new(private$predictor, n_cores, private$param_set, n_counterfactuals)

      private$y_hat = as.data.table(predictor$predict(predictor$data$X))
      
      if (!is.null(param_list$x_interest)) {
        self$find_counterfactuals(param_list$x_interest, param_list$desired_class, param_list$desired_prob)
      }
    }
  )
)

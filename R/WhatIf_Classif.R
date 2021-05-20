WhatIf_Classif <- R6::R6Class("WhatIf_Classif",
  inherit = CounterfactualsClassification,
  private = list(
    WhatIf_Algo_Obj = NULL,
    y_hat_desired_class = NULL,
    y_hat = NULL,
    
    preprocess = function() {
      private$y_hat_desired_class = private$y_hat[[private$desired_class]]
    },
    
    calculate = function() {
      private$WhatIf_Algo_Obj$run(private$x_interest, private$y_hat_desired_class, private$desired_prob)
    },
    
    aggregate = function() {
      private$.results = private$WhatIf_Algo_Obj$get_results_list(private$desired_class)
    },

    run_init_arg_checks = function(arg_list) {
      # TODO: Check if y is in X -> if yes remove and message
      # TODO: Arg type checks
    }

  ),
  public = list(
    # for hard classification set desired_prob to 0 or 1
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, desired_class = NULL,
                          desired_prob = NULL, n_cores = 1L, lower = NULL, upper = NULL) {
      
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

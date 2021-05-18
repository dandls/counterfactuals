#' @import data.table
#'
#' @export
WhatIf_Classif <- R6::R6Class("WhatIf_Classif",
  inherit = CounterfactualsClassification,
  private = list(
    WhatIf_Algo_Obj = NULL,
    y_hat_desired_class = NULL,
    
    preprocess = function() {
      private$y_hat_desired_class = private$y_hat[[private$desired_class]]
    },
    
    calculate = function() {
      private$WhatIf_Algo_Obj$run(private$x_interest, private$y_hat_desired_class, private$desired_prob)
    },
    
    aggregate = function() {
      cfactuals = private$WhatIf_Algo_Obj$cfactuals
      pred_cfactuals_one_hot = private$predictor$predict(cfactuals)
      pred_cfactuals = pred_cfactuals_one_hot[[private$desired_class]]
      
      private$.results = private$make_results_list(
        cfactuals, private$x_interest, private$WhatIf_Algo_Obj$dist_x_interest, pred_cfactuals
      )
    },
    
    make_results_list = function(cfactuals, x_interest, dist_x_interest, pred_cfactuals) {
      res_formatter = ResultsFormatter$new(cfactuals, x_interest)
      res_formatter$append_dist_x_interest(dist_x_interest)
      res_formatter$append_pred(pred_cfactuals)
      res_formatter$append_n_changes()
      res_formatter$make_results_list()
    },

    run_init_arg_checks = function(arg_list) {
      # TODO: Check if y is in X -> if yes remove and message
      # TODO: Arg type checks
    }

  ),
  public = list(
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, desired_class = NULL,
                          desired_prob = NULL, n_cores = 1L, lower = NULL, upper = NULL) {
      
      param_list = as.list(environment())
      super$initialize(param_list)
      
      private$run_init_arg_checks(param_list)
      private$WhatIf_Algo_Obj = WhatIf_Algo$new(private$predictor$data$X, n_cores, private$param_set, n_counterfactuals)
      
      if (is.null(private$y_hat)) {
        private$y_hat = as.data.table(predictor$predict(predictor$data$X))
      }
      
      if (!is.null(param_list$x_interest)) {
        self$find_counterfactuals(param_list$x_interest, param_list$desired_class, param_list$desired_prob)
      }
    }
  )
)

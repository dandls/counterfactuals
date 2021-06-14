MOC_Regr = R6::R6Class("MOC_Regr",
  inherit = CounterfactualMethodRegr,

  private = list(
    MOC_Algo_Obj = NULL,
 
    calculate = function() {
      private$MOC_Algo_Obj$run(private$x_interest, private$y_hat_interest, private$desired_outcome)
    },
    
    aggregate = function() {
      self$log = private$MOC_Algo_Obj$log
      private$.results = private$MOC_Algo_Obj$get_results_list()
    },
    
    flush = function() {
      # TODO
    },
    
    run_init_arg_checks = function(arg_list) {
      
    }


  ),
  public = list(
    initialize = function(predictor, x_interest = NULL, desired_outcome = NULL, epsilon = NULL, fixed_features = NULL, 
                          max_changed = NULL, mu = 50L, generations = 50L, p_rec = 0.9, p_rec_gen = 0.7, 
                          p_rec_use_orig = 0.7, p_mut = 0.2, p_mut_gen = 0.5, p_mut_use_orig = 0.2, k = 1L, 
                          weights = NULL, lower = NULL, upper = NULL, conditionals = FALSE, initialization = "random",
                          track_infeas = TRUE) {
      
      arg_list = as.list(environment())
      super$initialize(predictor, lower, upper)
      
      private$run_init_arg_checks(arg_list)
      private$MOC_Algo_Obj = MOC_Algo$new(private$predictor, private$param_set, arg_list)
      
      if (!is.null(arg_list$x_interest)) {
        self$find_counterfactuals(arg_list$x_interest, arg_list$desired_outcome)
      }
    },
    
    subset_results = function() {
      # needs own subset_results method as more distance measures than dist_x_interest and n_changed
    },
    
    plot_hv = function() {
      # TODO
    },

    plot_statistics = function() {
      # TODO
    },

    plot_pareto_front = function() {
      # TODO
    }

  )
)


# MOC$new() # -> does not run anything yet, only specifications
# MOC$find_counterfactuals(x_interest, desired_outcome)
# MOC$results()
# MOC$plot
# MOC$find_counterfactuals(x_interest2, desired_outcome2)

Counterfactuals = R6::R6Class("Counterfactuals",

  public = list(
    initialize = function(cfactuals, prediction.function, x_interest, param_set, desired) {
      self$values = cfactuals
      private$prediction.function = prediction.function
      self$x_interest = x_interest
      private$param_set = param_set
      self$desired = desired
    },
    desired = NULL, # list with either one value (desired_outcome) for regression or two values (desired_probs and class) for classif
    values = NULL,
    x_interest = NULL,
    
    
    
    plot_surface = function() stop("tbd"),

    plot_parallel = function() stop("tbd"),

    get_diff = function() {
      if (!is.null(private$diff)) {
        return(private$diff)
      }
      private$diff = make_cfactuals_diff(self$values, self$x_interest)
      private$diff
    },

    evaluate = function() stop("tbd"),
    
    predict = function() private$prediction.function(self$values)

  ),
  private = list(
    prediction.function = NULL,
    param_set = NULL, # (maybe upper and lower is enough..)
    diff = NULL
  )
)







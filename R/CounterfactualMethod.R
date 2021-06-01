CounterfactualMethod = R6::R6Class("CounterfactualMethod",
  
  public = list(
    initialize = function(predictor, lower, upper) {
      assert_class(predictor, "Predictor")
      data_X = predictor$data$X
      assert_numeric(lower, null.ok = TRUE)
      assert_numeric(upper, null.ok = TRUE)
      assert_true(all(names(lower) %in% names(data_X)))
      assert_true(all(names(upper) %in% names(data_X)))
      
      # If the task could not be derived from the model, then we infer it from the prediction of some training data
      if (predictor$task == "unknown") {
        # Needs to be set to NULL, as the predictor does not infer the task from prediction otherwise
        # See: https://github.com/christophM/iml/blob/master/R/Predictor.R#L141
        # The task is then checked by CounterfactualMethodRegr or CounterfactualMethodClassif
        predictor$task = NULL
        predictor$predict(data_X[1:2, ])
      }
      
      private$predictor = predictor
      private$param_set = make_param_set(data_X, lower, upper)
    },
    
    plot_parallel = function(n_solutions, feature_names) {
      # TODO
    },
    
    print = function() {
      cat("Counterfactual Explanation method: ", class(self)[1], "\n")
      private$print_parameters()
      cat("\n\nAnalysed predictor: \n")
      private$predictor$print()
      cat("\n\nAnalysed data:\n")
      print(private$predictor$data)
      if (!is.null(private$.results)) {
      cat("\n\nHead of results:\n")
        print(head(private$.results))
      }
    }
  ),
  
  active = list(
    results = function(value) {
      if (missing(value)) {
        private$.results
      } else {
        stop("`$results` is read only", call. = FALSE)
      }
    }
  ),
  
  private = list(
    predictor = NULL,
    x_interest = NULL,
    .results = NULL,
    param_set = NULL,
    y_hat_interest = NULL,
    
    run = function() stop("abstract"),
    
    print_parameters = function() {}
  )
)



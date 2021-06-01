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
    
    plot_surface = function(feature_names, grid_size = 50L, epsilon = NULL) {
      
      assert_names(feature_names, subset.of = names(private$predictor$data$X))
      if (is.null(self$results)) {
        stop("There are no results for plotting yet. Run `$find_counterfactuals()` first.")
      }
      
      arg_list = list(
        "feature_names" = feature_names, "grid_size" = grid_size, "epsilon" = epsilon, "results" = self$results,
        "predictor" = private$predictor, "x_interest" = private$x_interest, "param_set" = private$param_set, 
        "y_hat_interest" = private$y_hat_interest, pred_column = private$get_pred_column()
      )
      surface_plot = SurfacePlot$new(arg_list)
      surface_plot$plot()
    },
    
    plot_freq_of_feature_changes = function(subset_zero = FALSE) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)
      }
      
      freq = self$get_freq_of_feature_changes(subset_zero)
      df_freq = data.frame(var_name = names(freq), freq = freq)
      ggplot2::ggplot(df_freq, ggplot2::aes(x = reorder(var_name, -freq), y = freq)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::labs(x = ggplot2::element_blank(), y = "Relative frequency")
    },
    
    get_freq_of_feature_changes = function(subset_zero = FALSE) {
      assert_flag(subset_zero)
      if (is.null(self$results)) {
        stop("There are no results for plotting yet. Run `$find_counterfactuals()` first.")
      }

      diff = self$results$counterfactuals_diff
      feature_names = names(private$x_interest)
      diff_features = diff[, feature_names, with = FALSE]
      freq = colMeans(diff_features != 0, na.rm = TRUE)
      if (subset_zero) {
        freq = freq[freq != 0]
      }
      sort(freq, decreasing = TRUE)
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



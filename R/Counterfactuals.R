Counterfactuals = R6::R6Class("Counterfactuals",
  private = list(
    predictor = NULL,
    x_interest = NULL,
    .results = NULL,
    param_set = NULL,
    y_hat_interest = NULL,
    
    run = function() {
      private$preprocess()
      private$calculate()
      private$aggregate()
    },
    preprocess = function() {},
    calculate = function() {},
    aggregate = function() {},
    
    print_parameters = function() {},

    make_param_set = function(arg_list) {
      ps_maker = ParamSetMaker$new(arg_list$predictor$data$X, arg_list$lower, arg_list$upper)
      ps_maker$make_param_set()
    },
    
    get_pred_column = function() {},
    
    check_x_interest = function(x_interest) {
      checkmate::assert_data_frame(x_interest, nrows = 1L)
      data = private$predictor$data$X
      if (any(names(x_interest) != names(data))) {
        rlang::abort(c(
          "`x_interest` is invalid.",
          x = "`x_interest` and `predictor$data$X` must have the same columns.",
          i = waldo::compare(names(x_interest), names(data), x_arg = "x_interest", y_arg = "predictor$data$X")
        ))
      }
      
      col_types_x_interest = sapply(x_interest, typeof)
      col_data = sapply(data, typeof)
      if (any(col_types_x_interest != col_data)) {
        rlang::abort(c(
          "`x_interest` is invalid.",
          x = "`x_interest` and `predictor$data$X` must have the same column types.",
          i = waldo::compare(col_types_x_interest, col_data, x_arg = "x_interest", y_arg = "predictor$data$X")
        ))
      }
      
      feat_vals_outside_range = !ParamHelpers::isFeasible(private$param_set, as.list(x_interest))
      if (feat_vals_outside_range) {
        rlang::abort(c(
          "`x_interest` is invalid.",
          x = "Feature values of `x_interest` outside of range of `predictor$data$X` or given arguments `lower` or `upper`.",
          i = "Please modify arguments `lower` or `upper` accordingly."
        ))
      }
    },
    
    check_predictor = function(predictor) {
      if (is.null(predictor)) {
        rlang::abort(c(
          "`predictor` is invalid.",
          x = "The `predictor` has to be specified."
        ))
      }
      assert_class(predictor, "Predictor")
    },
    
    throw_error_if_no_results = function() {
      if (is.null(self$results)) {
        rlang::abort(c(
          "There are no results yet.",
          i = "Please run `$find_counterfactuals()` first."
        ))
      }
    },
    
    check_feature_names = function(feature_names) {
      assert_character(feature_names, null.ok = FALSE, len = 2L)
      names_data = names(private$predictor$data$X)
      if (!all(feature_names %in% names_data)) {
        rlang::abort(c(
          "`feature_names` is invalid.",
          x = "The `feature_names` are not in the training data.",
          i = sprintf("The colnames of the training data are: %s.", paste0("'", names_data, "'", collapse = ", ")),
          i = sprintf("`feature_names` are: %s.", paste0("'", feature_names, "'", collapse = ", "))
        ))
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
  public = list(
    measured_runtime = NULL,
    log = NULL,
    
    initialize = function(arg_list) {

      predictor = arg_list$predictor
      private$check_predictor(predictor)
      
      # If the task could not be derived from the model, the we infer it from the prediction of some training data
      if (predictor$task == "unknown") {
        # Needs to be set to NULL, as the predictor does not infer the task from prediction otherwise
        # See: https://github.com/christophM/iml/blob/master/R/Predictor.R#L141
        # The task is then checked by Counterfactuals_Regr or Counterfactuals_Classif
        predictor$task = NULL
        invisible(predictor$predict(predictor$data$X[1:2, ]))
      }
      
      private$predictor = predictor
      private$param_set = private$make_param_set(arg_list)
    },
    
    plot_parallel = function(n_solutions, feature_names) {
      # TODO
    },
    
    plot_surface = function(feature_names, grid_size = 50L, epsilon = NULL) {
      private$check_feature_names(feature_names)
      private$throw_error_if_no_results()
      arg_list = list(
        "feature_names" = feature_names, "grid_size" = grid_size, "epsilon" = epsilon, "results" = self$results,
        "predictor" = private$predictor, "x_interest" = private$x_interest, "param_set" = private$param_set, 
        "y_hat_interest" = private$y_hat_interest, pred_column = private$get_pred_column()
      )
      surface_plot = SurfacePlot$new(arg_list)
      surface_plot$plot()
    },
    
    plot_freq_of_feature_changes = function(subset_zero = FALSE) {
      freq = self$get_freq_of_feature_changes(subset_zero)
      df_freq = data.frame(var_name = names(freq), freq = freq)
      ggplot(df_freq, aes(x = reorder(var_name, -freq), y = freq)) +
        geom_bar(stat = "identity") +
        labs(x = element_blank(), y = "Relative frequency")
    },
    
    get_freq_of_feature_changes = function(subset_zero = FALSE) {
      assert_flag(subset_zero)
      private$throw_error_if_no_results()

      diff = self$results$counterfactuals_diff
      feature_names = names(private$x_interest)
      diff_features = diff[, feature_names, with = FALSE]
      freq = colMeans(diff_features != 0, na.rm = TRUE)
      if (subset_zero) {
        freq = freq[freq != 0]
      }
      sort(freq, decreasing = TRUE)
    },
    
    subset_results = function(n_counterfactuals = 10L) {
      private$throw_error_if_no_results()
      is_out_of_range = n_counterfactuals > nrow(private$.results[[1L]])
      if (is_out_of_range) {
        warning("`n_counterfactuals` out of range, it was set to the number of solutions in self$results.")
      }
      
      lapply(private$.results, head, n_counterfactuals)
    },
    
    print = function() {
      cat("Counterfactual Explanation method: ", class(self)[1], "\n")
      private$print_parameters()
      cat("\n\nAnalysed predictor: \n")
      private$predictor$print()
      cat("\n\nAnalysed data:\n")
      print(private$predictor$data)
      cat("\n\nHead of results:\n")
      if (!is.null(private$.results)) {
        print(head(private$.results))
      }
    }
  
  )
)







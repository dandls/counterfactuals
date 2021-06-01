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
      if (is.null(private$diff)) {
        private$diff = make_cfactuals_diff(self$values, self$x_interest)
      }
      
      feature_names = names(self$x_interest)
      diff_features = private$diff[, feature_names, with = FALSE]
      freq = colMeans(diff_features != 0, na.rm = TRUE)
      if (subset_zero) {
        freq = freq[freq != 0]
      }
      sort(freq, decreasing = TRUE)
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







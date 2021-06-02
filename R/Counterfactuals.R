Counterfactuals = R6::R6Class("Counterfactuals",

  public = list(
    initialize = function(cfactuals, prediction_function, x_interest, param_set, desired, task) {
      assert_data_table(cfactuals)
      private$prediction_function = prediction_function
      private$param_set = param_set
      private$task = task
      private$diff = make_cfactuals_diff(cfactuals, x_interest)
      self$data = cfactuals
      self$x_interest = x_interest
      self$desired = desired
    },
    desired = NULL,
    data = NULL,
    x_interest = NULL,
    
    plot_parallel = function(n_solutions, feature_names) {
      stop("tbd")
    },

    get_diff = function() {private$diff},
    
    plot_freq_of_feature_changes = function(subset_zero = FALSE) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
      }
      
      freq = self$get_freq_of_feature_changes(subset_zero)
      df_freq = data.frame(var_name = names(freq), freq = freq)
      ggplot2::ggplot(df_freq, ggplot2::aes(x = reorder(var_name, -freq), y = freq)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::labs(x = ggplot2::element_blank(), y = "Relative frequency")
    },
    
    get_freq_of_feature_changes = function(subset_zero = FALSE) {
      assert_flag(subset_zero)
      feature_names = names(self$x_interest)
      diff_features = private$diff[, feature_names, with = FALSE]
      freq = colMeans(diff_features != 0, na.rm = TRUE)
      if (subset_zero) {
        freq = freq[freq != 0]
      }
      sort(freq, decreasing = TRUE)
    },
    

    # para, grid_size not used for two categorical features
    plot_surface = function(feature_names, grid_size = 50L) {
      assert_names(feature_names, subset.of = names(self$data))
      
      diff_rel_feats = private$diff[, ..feature_names]
      n_changes_total = count_changes(self$data, self$x_interest)
      n_changes_rel_feats = rowSums(diff_rel_feats != 0)
      has_changes_in_rel_feats_only = (n_changes_rel_feats == n_changes_total)
      cfactuals_plotted = self$data[which(has_changes_in_rel_feats_only)]
      
      make_surface_plot(
        grid_size, private$param_set, cfactuals_plotted, self$x_interest, private$prediction_function, feature_names, 
        private$get_pred_column()
      )
    },
    
    evaluate = function(measures = c("dist_x_interest", "dist_target", "nr_changed")) {
      assert_character(measures)
      assert_names(measures, subset.of = c("dist_x_interest", "dist_target", "nr_changed"))
      evals = self$data
      
      if ("dist_x_interest" %in% measures) {
        ranges = private$param_set$upper - private$param_set$lower 
        X_list = split(self$data, seq(nrow(self$data)))
        dist_vector = future.apply::future_vapply(
          X_list, StatMatch::gower.dist, FUN.VALUE = numeric(1L), self$x_interest, ranges, USE.NAMES = FALSE
        )
        evals$dist_x_interest = dist_vector
      }
      
      if ("dist_target" %in% measures) {
        pred_column = private$get_pred_column()
        pred = as.matrix(self$predict()[[pred_column]])
        if (private$task == "classification") {
          target = self$desired$desired_prob
        } else {
          target = self$desired$desired_outcome
        }
        evals$dist_target = apply(pred, 1L, function(x) min(abs(x - target)))
      }
      
      if ("nr_changed" %in% measures) {
        evals$nr_changed = count_changes(self$data, self$x_interest)
      }
      
      evals
    },
    
    predict = function() private$prediction_function(self$data)

  ),
  private = list(
    prediction_function = NULL,
    param_set = NULL,
    diff = NULL,
    task = NULL, 
    
    get_pred_column = function() {
      if (private$task == "classification") {
        self$desired$desired_class
      } else {
        1L
      }
    }
  )
)







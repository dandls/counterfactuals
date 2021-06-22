Counterfactuals = R6::R6Class("Counterfactuals",

  public = list(
    
    initialize = function(cfactuals, predictor, x_interest, param_set, desired) {
      assert_data_table(cfactuals)
      assert_class(predictor, "Predictor")
      assert_data_table(x_interest, nrows = 1L)
      assert_true(ncol(cfactuals) == ncol(x_interest))
      assert_class(param_set, "ParamSet")
      assert_list(desired, min.len = 1L, max.len = 2L)
      assert_true(all(names(cfactuals) == names(predictor$data$X)))
      if (any(sapply(cfactuals, typeof) != sapply(predictor$data$X, typeof))) {
        stop("Columns of `cfactuals` and `predictor$data$X` must have the same types.")
      }
      
      private$predictor = predictor
      private$param_set = param_set
      private$diff = make_cfactuals_diff(cfactuals, x_interest)
      private$.data = cfactuals
      private$.x_interest = x_interest
      private$.desired = desired
    },

    get_diff = function() {private$diff},
    
    evaluate = function(measures = c("dist_x_interest", "dist_target", "nr_changed")) {
      assert_character(measures)
      assert_names(measures, subset.of = c("dist_x_interest", "dist_target", "nr_changed"))
      evals = private$.data
      
      if ("dist_x_interest" %in% measures) {
        ranges = private$param_set$upper - private$param_set$lower 
        X_list = split(private$.data, seq(nrow(private$.data)))
        dist_vector = future.apply::future_vapply(
          X_list, StatMatch::gower.dist, FUN.VALUE = numeric(1L), private$.x_interest, ranges, USE.NAMES = FALSE
        )
        evals$dist_x_interest = dist_vector
      }
      
      if ("dist_target" %in% measures) {
        pred_column = private$get_pred_column()
        pred = self$predict()[[pred_column]]
        if (private$predictor$task == "classification") {
          target = private$.desired$desired_prob
        } else {
          target = private$.desired$desired_outcome
        }

        evals$dist_target = sapply(
          pred, function(x) ifelse(between(x, target[1L], target[2L]), 0, min(abs(x - target)))
        )
      }
      
      if ("nr_changed" %in% measures) {
        evals$nr_changed = count_changes(private$.data, private$.x_interest)
      }
      
      evals
    },
    
    predict = function() {
      private$predictor$predict(private$.data) 
    },
    
    plot_parallel = function(feature_names = NULL, row_ids = NULL) {
      
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
      }
      
      if (!requireNamespace("GGally", quietly = TRUE)) {
        stop("Package 'GGally' needed for this function to work. Please install it.", call. = FALSE)
      }
      
      if (is.null(feature_names)) {
        feature_names = names(private$.data)
      }
      assert_names(feature_names, subset.of = names(private$.data))
      
      if (is.null(row_ids)) {
        row_ids = 1:nrow(private$.data)
      }
      assert_integerish(row_ids, lower = 0, upper = nrow(private$.data))
      
      cfactuals = private$.data[row_ids, ..feature_names]
      dt = rbind(cfactuals, self$x_interest[, ..feature_names])
      

      is_numeric_col = sapply(dt, function(x) is.numeric(x))
      numeric_cols = names(dt)[is_numeric_col]
      non_numeric_cols = names(dt)[!is_numeric_col]
      if (length(non_numeric_cols) > 0L) {
        dt[, (non_numeric_cols) := NULL]
        warning("Can only consider numeric features for parallel plot. Non-numeric features have been removed.")
      }
      
      line_colors = c(gray.colors(nrow(cfactuals), start = 0.2, end = 0.8, gamma = 2.2), "blue")
      names(line_colors) <- rownames(dt)
      dt[, rn := rownames(dt)]
 
      GGally::ggparcoord(dt, 1:length(numeric_cols), groupColumn = "rn", scale = "uniminmax", showPoints = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::ylim(c(-0.1, 1.1)) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ylab("Scaled feature values") +
        ggplot2::scale_colour_manual(name = "rows", values = line_colors) +
        ggplot2::annotate(
          "text", x = 1:length(numeric_cols), y = 1.05, label = sapply(dt[, ..numeric_cols], max, na.rm = TRUE)
        ) +
        ggplot2::annotate(
          "text", x = 1:length(numeric_cols), y = -0.05, label = sapply(dt[, ..numeric_cols], min, na.rm = TRUE)
        )
      
    },
    
    plot_freq_of_feature_changes = function(subset_zero = FALSE) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
      }
      
      freq = self$get_freq_of_feature_changes(subset_zero)
      df_freq = data.frame(var_name = names(freq), freq = freq)
      ggplot2::ggplot(df_freq, ggplot2::aes(x = reorder(var_name, freq), y = freq)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::labs(x = ggplot2::element_blank(), y = "Relative frequency") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw()
    },
    
    get_freq_of_feature_changes = function(subset_zero = FALSE) {
      assert_flag(subset_zero)
      feature_names = names(private$.x_interest)
      diff_features = private$diff[, feature_names, with = FALSE]
      freq = colMeans(!is.na(diff_features), na.rm = TRUE)
      if (subset_zero) {
        freq = freq[freq != 0]
      }
      sort(freq, decreasing = TRUE)
    },
    

    # para, grid_size not used for two categorical features
    plot_surface = function(feature_names, grid_size = 250L) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
      }
      if (!requireNamespace("ggExtra", quietly = TRUE)) {
        stop("Package 'ggExtra' needed for this function to work. Please install it.", call. = FALSE)
      }
      assert_names(feature_names, subset.of = names(private$.data))

      diff_rel_feats = private$diff[, ..feature_names]
      n_changes_total = count_changes(private$.data, private$.x_interest)
      n_changes_rel_feats = rowSums(!is.na(diff_rel_feats))
      has_changes_in_rel_feats_only = (n_changes_rel_feats == n_changes_total)
      cfactuals_plotted = private$.data[which(has_changes_in_rel_feats_only)]
 
      make_surface_plot(
        grid_size, private$param_set, cfactuals_plotted, private$.x_interest, private$predictor, feature_names, 
        private$get_pred_column()
      )
    }

  ),
  active = list(
    desired = function(value) {
      if (missing(value)) {
        private$.desired
      } else {
        stop("`$desired` is read only", call. = FALSE)
      }
    },
    data = function(value) {
      if (missing(value)) {
        private$.data
      } else {
        stop("`$data` is read only", call. = FALSE)
      }
    },
    x_interest = function(value) {
      if (missing(value)) {
        private$.x_interest
      } else {
        stop("`$x_interest` is read only", call. = FALSE)
      }
    }
  ),
  private = list(
    predictor = NULL,
    param_set = NULL,
    diff = NULL,
    task = NULL, 
    .desired = NULL,
    .data = NULL,
    .x_interest = NULL,
    
    get_pred_column = function() {
      if (private$predictor$task == "classification") {
        private$.desired$desired_class
      } else {
        1L
      }
    }
  )
)







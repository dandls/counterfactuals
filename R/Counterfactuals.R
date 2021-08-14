#' Counterfactuals Class
#'
#' @description 
#' A `Counterfactuals` object should be created by \link{CounterfactualMethodRegr} or \link{CounterfactualMethodClassif}.
#' It contains the counterfactuals and has several methods for their evaluation and visualization.
#' 
#' @references 
#' 
#' Gower, J. C. (1971), "A general coefficient of similarity and some of its properties". Biometrics, 27, 623–637.
#' 
#' Kaufman, L. and Rousseeuw, P.J. (1990), Finding Groups in Data: An Introduction to Cluster Analysis. Wiley, New York.
Counterfactuals = R6::R6Class("Counterfactuals",

  public = list(
    
    #' @description 
    #' Creates a new `Counterfactuals` object.
    #' This method should only be called within the `$find_counterfactuals` methods of \link{CounterfactualMethodRegr} 
    #' and \link{CounterfactualMethodClassif}.
    #' @param cfactuals (`data.table`) \cr
    #'  The counterfactuals. Must have the same column names and types as `predictor$data$X`.
    #' @template lower_upper
    #' @template x_interest
    #' @template predictor
    #' @param param_set (\link[paradox]{ParamSet})\cr
    #'  A \link[paradox]{ParamSet} based on the features of `predictor$data$X`.
    #' @param desired (`list(1)` | `list(2)`)\cr
    #'  A `list` with the desired properties of the counterfactuals. For regression tasks it should have one 
    #'  element `desired_outcome` (\link{CounterfactualMethodRegr}) and for classification tasks two elements 
    #'  `desired_class` and `desired_prob` (\link{CounterfactualMethodRegr}).
    initialize = function(cfactuals, predictor, x_interest, param_set, desired) {
      assert_data_table(cfactuals)
      assert_class(predictor, "Predictor")
      assert_data_frame(x_interest, nrows = 1L)
      setDT(x_interest)
      assert_true(ncol(cfactuals) == ncol(x_interest))
      assert_class(param_set, "ParamSet")
      assert_list(desired, min.len = 1L, max.len = 2L)
      assert_names(names(cfactuals), permutation.of = names(predictor$data$X))
      setcolorder(cfactuals, names(predictor$data$X))
      if (any(sapply(cfactuals, typeof) != sapply(predictor$data$X, typeof))) {
        stop("Columns of `cfactuals` and `predictor$data$X` must have the same types.")
      }
      factor_columns = names(cfactuals)[sapply(cfactuals, is.factor)]
      if (length(factor_columns) > 0L) {
        for (col in factor_columns) {
          assert_factor(
            cfactuals[[col]], levels = levels(predictor$data$X[[col]]), ordered = is.ordered(predictor$data$X[[col]])
          )
        }
      }
      
      private$predictor = predictor
      private$param_set = param_set
      private$diff = make_cfactuals_diff(cfactuals, x_interest)
      private$.data = cfactuals
      private$.x_interest = x_interest
      private$.desired = desired
    },
    
    #' @description 
    #' Evaluates the counterfactuals. It returns the counterfactuals along with evaluation `measures` in additional 
    #' columns.
    #' @param measures (`character`) \cr
    #'  The name of one or more evaluation measures.
    #'  The following measures are available: 
    #'   * `dist_x_interest`: The distance of a counterfactual to `x_interest`. It is measured by the extension of the 
    #'                        Gower's distance proposed by Kaufman and Rousseeuw (1990) and implemented by 
    #'                        \link[StatMatch]{gower.dist}.  
    #'   * `dist_target`: The absolute distance of the prediction of a counterfactual to the `desired_outcome`
    #'                    (regression task) or `desired_prob` (classification task).      
    #'   * `nr_changed`: The number of feature changes w.r.t. `x_interest`.   
    #'   * `dist_train`: The (possibly weighted) distance to the `k` nearest training points.
    #' 
    #' @param show_diff (`logical(1)`)\cr
    #'  Should the counterfactuals be displayed as their differences from `x_interest`? (Default is `FALSE`.)
    #'  If set to `TRUE`, numeric features values of `x_interest` are substracted from the corresponding feature values
    #'  of the counterfactuals. Non-numeric features are displayed with their actual counterfactuals value. 
    #'  No difference is indicated by `NA`.
    #'          
    #' @param k (`integerish(1)`) \cr
    #'  How many nearest training points should be considered for computing the `dist_train` measure? Default is `1L`.
    #' @param weights (`numeric(k)` | `NULL`) \cr
    #'  How should the `k` nearest training points be weighted when computing the `dist_train` measure? If `NULL`
    #'  (default) then all `k` points are weighted equally. If a numeric vector of length `k` is given, the i-th element
    #'  determines the weight of the i-th closest point.
    #'                                              
    #' @md
    evaluate = function(measures = c("dist_x_interest", "dist_target", "nr_changed", "dist_train"), show_diff = FALSE, 
                        k = 1L, weights = NULL) {
      
      assert_names(measures, subset.of = c("dist_x_interest", "dist_target", "nr_changed", "dist_train"))
      assert_flag(show_diff)
      assert_number(k, lower = 1, upper = nrow(private$predictor$data$X))
      assert_numeric(weights, any.missing = FALSE, len = k, null.ok = TRUE)
      assert_data_table(self$data, min.rows = 1L)
      
      if (show_diff) {
        evals = private$diff
      } else {
        evals = private$.data
      }
      
      if ("dist_x_interest" %in% measures) {
        ranges = private$param_set$upper - private$param_set$lower
        evals$dist_x_interest = as.vector(StatMatch::gower.dist(private$.x_interest, private$.data, rngs = ranges, KR.corr = FALSE))
      }
      
      if ("nr_changed" %in% measures) {
        evals$nr_changed = count_changes(private$.data, private$.x_interest)
      }
      
      if ("dist_train" %in% measures) {
        evals$dist_train = gower_topn(private$.data, private$predictor$data$X, n = 1L)$distance[1L, ]
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
        setorder(evals, dist_target)
      }
      
      
      evals
    },
    
    #' @description Predicts the target values of the counterfactuals.
    predict = function() {
      private$predictor$predict(private$.data) 
    },
    
    #' @description Plots a parallel plot of the (scaled) feature values of the counterfactuals. `x_interest` is shown
    #' in blue.
    #' 
    #' @param feature_names (`character` | `NULL`)\cr
    #'  The names of the (numeric) features to plot. If `NULL` (default) all features are plotted.
    #' @param row_ids (`integerish` | `NULL`)\cr
    #'  The row ids of the counterfactuals to plot. If `NULL` (default) all counterfactuals are plotted.
    #' @param digits_min_max Number of digits for the rounding the minimum and maximum features values. Default is `3L`.
    plot_parallel = function(feature_names = NULL, row_ids = NULL, digits_min_max = 2L) {
      
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
      }
      
      if (!requireNamespace("GGally", quietly = TRUE)) {
        stop("Package 'GGally' needed for this function to work. Please install it.", call. = FALSE)
      }
      
      assert_data_table(self$data, min.rows = 1L)
      if (is.null(feature_names)) {
        feature_names = names(private$.data)
      }
      assert_names(feature_names, subset.of = names(private$.data))
      
      if (is.null(row_ids)) {
        row_ids = 1:nrow(private$.data)
      }
      assert_integerish(row_ids, lower = 1L, upper = nrow(private$.data))
      assert_integerish(digits_min_max, len = 1L, lower = 1L)
      
      cfactuals = private$.data[row_ids, ..feature_names]
      dt = rbind(cfactuals, self$x_interest[, ..feature_names])

      is_numeric_col = sapply(dt, function(x) is.numeric(x))
      numeric_cols = names(dt)[is_numeric_col]
      non_numeric_cols = names(dt)[!is_numeric_col]
      if (length(non_numeric_cols) > 0L) {
        dt[, (non_numeric_cols) := NULL]
        warning("Can only consider numeric features for parallel plot. Non-numeric features have been removed.")
      }
      
      line_colors = c(rep("#595959", nrow(cfactuals)), "blue")
      names(line_colors) <- rownames(dt)
      dt[, rn := rownames(dt)]
 
      GGally::ggparcoord(dt, 1:length(numeric_cols), groupColumn = "rn", scale = "uniminmax", showPoints = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::ylim(c(-0.1, 1.1)) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ylab("scaled feature values") +
        ggplot2::xlab("variables") +
        ggplot2::scale_colour_manual(name = "rows", values = line_colors) +
        ggplot2::annotate(
          "text", x = 1:length(numeric_cols), y = 1.05, size = 3.5,
          label = sapply(dt[, ..numeric_cols], function(x) round(max(x, na.rm = TRUE), digits = digits_min_max))
        ) +
        ggplot2::annotate(
          "text", x = 1:length(numeric_cols), y = -0.05, size = 3.5,
          label = sapply(dt[, ..numeric_cols], function(x) round(min(x, na.rm = TRUE), digits = digits_min_max))
        )
      
    },
    
    #' @description Plots a bar chart of the frequency of feature changes across all counterfactuals.
    #' 
    #' @param subset_zero (`logical(1)`)\cr
    #'  Should features that have no changes be excluded from the plot? Default is `FALSE`.
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
    
    #' @description Returns the frequency of feature changes across all counterfactuals.
    #' 
    #' @param subset_zero (`logical(1)`)\cr
    #'  Should features that have no changes be excluded from the plot? Default is `FALSE`.
    #'  
    #' @return A (named) `numeric` vector with the frequency of feature changes.
    get_freq_of_feature_changes = function(subset_zero = FALSE) {
      assert_flag(subset_zero)
      assert_data_table(self$data, min.rows = 1L)
      
      feature_names = names(private$.x_interest)
      diff_features = private$diff[, feature_names, with = FALSE]
      freq = colMeans(!is.na(diff_features), na.rm = TRUE)
      if (subset_zero) {
        freq = freq[freq != 0]
      }
      sort(freq, decreasing = TRUE)
    },
    
    #' @description Creates a surface plot for two features. `x_interest` is represented as bright dot and 
    #' all counterfactuals that differ from `x_interest` **only** in the selected features are represented as black dots.
    #' 
    #'  * (`numeric`, `numeric`): surface plot
    #'  * (`non-numeric`, `non-numeric`): heatmap
    #'  * (`numeric`, `non-numeric`): line graph
    #' @param feature_names (`character(2)`)\cr
    #'  The names of the features to plot.
    #' @param grid_size (`integerish(1)`)\cr
    #'  The grid size of the plot. Ignored in case of two `non-numeric` features. Default is `250L`.
    #' @md
    plot_surface = function(feature_names, grid_size = 250L) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
      }
      assert_data_table(self$data, min.rows = 1L)
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
    },
    
    #' @description Prints the `Counterfactuals` object.
    print = function() {
      desired = private$.desired
      cat(nrow(private$.data), "Counterfactual(s) \n \n")
      if (private$predictor$task == "classification") {
        cat("Desired class:", desired$desired_class, "\n")
        if (desired$desired_prob[1L] != desired$desired_prob[2L]) {
          cat(
            "Desired predicted probability range: [", desired$desired_prob[1L], ", ", 
            desired$desired_prob[2L],  "] \n \n", sep = ""
          )
        } else {
          cat("Desired predicted probability:", private$.desired$desired_prob[1L], "\n \n")
        }
      } else {
        if (private$.desired$desired_outcome[1L] != private$.desired$desired_outcome[2L]) {
          cat(
            "Desired outcome range: [", desired$desired_outcome[1L], ", ", 
            desired$desired_outcome[2L],  "] \n \n", sep = ""
          )
        } else {
          cat("Desired outcome:", private$.desired$desired_outcome[1L], "\n \n")
        }
      }
      cat("Head: \n")
      print(head(private$.data, 3L))
    }

  ),
  active = list(
    #' @field desired (`list(1)` | `list(2)`)\cr
    #'  A `list` with the desired properties of the counterfactuals.  
    #'  For regression tasks it has one element `desired_outcome` (\link{CounterfactualMethodRegr}) and for 
    #'  classification tasks two elements `desired_class` and `desired_prob` (\link{CounterfactualMethodRegr}).
    desired = function(value) {
      if (missing(value)) {
        private$.desired
      } else {
        stop("`$desired` is read only", call. = FALSE)
      }
    },
    #' @field data (`data.table`)\cr
    #'  The counterfactuals.
    data = function(value) {
      if (missing(value)) {
        private$.data
      } else {
        stop("`$data` is read only", call. = FALSE)
      }
    },
    #' @field x_interest (`data.table(1)`) \cr
    #'   A single row with the observation of interest.
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







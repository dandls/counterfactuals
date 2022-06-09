#' Random Search for Regression Tasks
#'
#' @template random_search_info
#'
#' @examples
#' if (require("randomForest")) {
#'   # Train a model
#'   rf = randomForest(mpg ~ ., data = mtcars)
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf)
#'   # Find counterfactuals for x_interest
#'   rs_regr = RandomSearchRegr$new(predictor, n_generations = 30L)
#'   cfactuals = rs_regr$find_counterfactuals(x_interest = mtcars[1L, ], desired_outcome = c(22, 26))
#'   # Print the counterfactuals
#'   cfactuals$data
#'   # Plot evolution of hypervolume and mean and minimum objective values 
#'   rs_regr$plot_statistics()
#' }
#'
#' @export
RandomSearchRegr = R6::R6Class("RandomSearchRegr",
  inherit = CounterfactualMethodRegr,
  public = list(
    #' @description Create a new `RandomSearchRegr` object.
    #' @template predictor
    #' @param fixed_features (`character()` | `NULL`)\cr
    #'   Names of features that are not allowed to be changed. `NULL` (default) allows all features to be changed.
    #' @param max_changed (`integerish(1)` | `NULL`)\cr
    #'   Maximum number of feature changes. `NULL` (default) allows any number of changes.
    #' @param mu (`integerish(1)`)\cr  
    #'   The population size. Default is `20L`. The total number of random samples is set to `mu * n_generations`.
    #'   See the `Details` section for further details.
    #' @param n_generations (`integerish(1)`)\cr  
    #'   The number of generations. Default is `175L`. The total number of random samples is set to `mu * n_generations`.
    #'   See the `Details` section for further details.
    #' @param p_use_orig (`numeric(1)`)\cr
    #'   Probability with which a feature/gene is reset to its original value in `x_interest` after random sampling. Default is `0.5`.
    #' @param k (`integerish(1)`)\cr
    #'   The number of data points to use for the forth objective. Default is `1L`.
    #' @param weights (`numeric(1) | numeric(k)` | `NULL`)\cr
    #'   The weights used to compute the weighted sum of dissimilarities for the forth objective. It is either a single value
    #'   or a vector of length `k`. If it has length `k`, the i-th element specifies the weight of the i-th closest data point.
    #'   The values should sum up to `1`. `NULL` (default) means all data points are weighted equally.
    #' @template lower_upper
   #' @param distance_function (`function()` | `'gower'` | `'gower_c'`)\cr 
    #'  The distance function to be used in the second and fourth objective.
    #'  Either the name of an already implemented distance function
    #'  ('gower' or 'gower_c') or a function.
    #'  If set to 'gower' (default), then Gower's distance (Gower 1971) is used;
    #'  if set to 'gower_c', a C-based more efficient version of Gower's distance is used.
    #'  A function must have three arguments  `x`, `y`, and `data` and should
    #'  return a `double` matrix with `nrow(x)` rows and maximum `nrow(y)` columns.

    initialize = function(predictor, fixed_features = NULL, max_changed = NULL, mu = 20L, n_generations = 175L,
                          p_use_orig = 0.5, k = 1L, weights = NULL, lower = NULL, upper = NULL, distance_function = "gower") {
      
      if (is.character(distance_function)) {
        if (distance_function == "gower") {
          distance_function = gower_dist
        } else if (distance_function == "gower_c") {
          if (!requireNamespace("gower", quietly = TRUE)) {
            stop("Package 'gower' needed for distance_function = 'gower_c'. Please install it.", call. = FALSE)
          }
          distance_function = function(x, y, data) {
            gower_dist_c(x, y, data, k = k)
          }
          class(distance_function) = class(gower_dist_c)
        }
      }
      
      super$initialize(predictor, lower, upper, distance_function)

      if (!is.null(fixed_features)) {
        assert_names(fixed_features, subset.of = private$predictor$data$feature.names)
      }
      assert_integerish(max_changed, lower = 0, len = 1L, null.ok = TRUE)
      assert_integerish(mu, lower = 1, len = 1L)
      assert_integerish(n_generations, lower = 1, len = 1L)
      assert_number(k, lower = 1, upper = nrow(private$predictor$data$X))
      assert_numeric(weights, any.missing = FALSE, len = k, null.ok = TRUE)

      private$fixed_features = fixed_features
      private$max_changed = max_changed
      private$mu = mu
      private$n_generations = n_generations
      private$p_use_orig = p_use_orig
      private$k = k
      private$weights = weights
      private$lower = lower
      private$upper = upper
    },

    #' @description Plots the evolution of the mean and minimum objective values together with the dominated hypervolume over
    #' the generations. All values for a generation are computed based on all non-dominated individuals that emerged until
    #' that generation. The randomly drawn samples are therefore split into `n_generations` folds of size `mu.`
    #' This function mimics MOCs `plot_statistics()` method. See the `Details` section for further information.
    #' @param centered_obj (`logical(1)`)\cr
    #'   Should the objective values be centered? If set to `FALSE`, each objective value is visualized in a separate plot,
    #'   since they (usually) have different scales. If set to `TRUE` (default), they are visualized in a single plot.
    plot_statistics = function(centered_obj = TRUE) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
      }
      if (is.null(self$optimizer)) {
        stop("There are no results yet. Please run `$find_counterfactuals` first.")
      }
      assert_flag(centered_obj)
      archive_folds = private$make_archive_folds()
      make_moc_statistics_plots(archive_folds, private$ref_point, centered_obj)
    },

    #' @description Calculates the dominated hypervolume of each generation. The randomly drawn samples are therefore split 
    #' into `n_generations` folds of size `mu.`
    #' This function mimics MOCs `get_dominated_hv()` method. See the `Details` section for further information.
    #' @return A `data.table` with the dominated hypervolume of each generation.
    get_dominated_hv = function() {
      if (is.null(self$optimizer)) {
        stop("There are no results yet. Please run `$find_counterfactuals` first.")
      }
      archive_folds = private$make_archive_folds()
      comp_domhv_all_gen(archive_folds, private$ref_point)
    },

    #' @description Visualizes two selected objective values of all emerged individuals in a scatter plot.
    #' The randomly drawn samples are therefore split into `n_generations` folds of size `mu.`
    #' This function mimics MOCs `plot_search()` method. See the `Details` section for further information.
    #' @param objectives (`character(2)`)\cr
    #'   The two objectives to be shown in the plot. Possible values are "dist_target", "dist_x_interest, "no_changed",
    #'   and "dist_train".
    plot_search = function(objectives = c("dist_target", "dist_x_interest")) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
      }
      if (is.null(self$optimizer)) {
        stop("There are no results yet. Please run `$find_counterfactuals` first.")
      }
      assert_names(objectives, subset.of = c("dist_target", "dist_x_interest", "no_changed", "dist_train"))
      archive_folds = private$make_archive_folds()
      make_moc_search_plot(archive_folds$data, objectives)
    }
  ),
  active = list(
    #' @field optimizer (\link[bbotk]{OptimInstanceMultiCrit}) \cr
    #'  The object used for optimization.
    optimizer = function(value) {
      if (missing(value)) {
        private$.optimizer
      } else {
        stop("`$optimizer` is read only", call. = FALSE)
      }
    }
  ),
  private = list(
    fixed_features = NULL,
    max_changed = NULL,
    mu = NULL,
    n_generations = NULL,
    p_use_orig = NULL,
    k = NULL,
    weights = NULL,
    lower = NULL,
    upper = NULL,
    ref_point = NULL,
    .optimizer = NULL,
    make_archive_folds = function() {
      folds = rep(seq_len(private$n_generations), each = private$mu)
      archive_folds = self$optimizer$archive$clone()
      archive_folds$data$batch_nr = folds
      archive_folds
    },
    run = function() {
      
      pred_column = private$get_pred_column()
      y_hat_interest = private$predictor$predict(private$x_interest)[[pred_column]]
      private$ref_point = c(min(abs(y_hat_interest - private$desired_outcome)), 1, ncol(private$x_interest), 1)
      
      private$.optimizer = moc_algo(
        predictor = private$predictor,
        x_interest = private$x_interest,
        pred_column = pred_column,
        target = private$desired_outcome,
        param_set = private$param_set,
        lower = private$lower,
        upper = private$upper,
        sdevs_num_feats = NULL,
        epsilon = NULL,
        fixed_features = private$fixed_features,
        max_changed = private$max_changed,
        mu = private$mu * private$n_generations,
        n_generations = 0,
        p_rec = NULL,
        p_rec_gen = NULL,
        p_mut = NULL,
        p_mut_gen = NULL,
        p_mut_use_orig = NULL,
        k = private$k,
        weights = private$weights,
        init_strategy = "random",
        distance_function = private$distance_function
      )

      unique(private$.optimizer$result[, names(private$x_interest), with = FALSE])
    },
    print_parameters = function() {
      cat(" - fixed_features: ", private$fixed_features, "\n")
      cat(" - k: ", private$k, "\n")
      cat(" - lower: ", private$lower, "\n")
      cat(" - max_changed: ", private$max_changed, "\n")
      cat(" - mu: ", private$mu, "\n")
      cat(" - n_generations: ", private$n_generations, "\n")
      cat(" - p_use_orig: ", private$p_use_orig, "\n")
      cat(" - upper: ", private$upper, "\n")
      cat(" - weights: ", private$weights, "\n")
    }
  )
)

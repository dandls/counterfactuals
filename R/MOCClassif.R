#' MOC (Multi-Objective Counterfactual Explanations) for Classification Tasks
#' 
#' @template moc_info
#' 
#' @examples 
#' if (require("randomForest")) {
#'  \donttest{
#'   # Train a model
#'   rf = randomForest(Species ~ ., data = iris)
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf, type = "prob")
#'   # Find counterfactuals for x_interest
#'   moc_classif = MOCClassif$new(predictor, n_generations = 15L, quiet = TRUE)
#'  
#'   cfactuals = moc_classif$find_counterfactuals(
#'     x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
#'   )
#'   # Print the counterfactuals
#'   cfactuals$data
#'   # Plot evolution of hypervolume and mean and minimum objective values
#'   moc_classif$plot_statistics()
#'   }
#' }
#' 
#' @export
MOCClassif = R6::R6Class("MOCClassif", inherit = CounterfactualMethodClassif,

  public = list(
    #' @description Create a new `MOCClassif` object.
    #' @template predictor
    #' @param epsilon (`numeric(1)` | `NULL`)\cr  
    #'   If not `NULL`, candidates whose prediction for the `desired_class` is farther away from the interval `desired_prob` 
    #'   than `epsilon` are penalized. `NULL` (default) means no penalization.  
    #' @param fixed_features (`character()` | `NULL`)\cr  
    #'   Names of features that are not allowed to be changed. `NULL` (default) allows all features to be changed.
    #' @param max_changed (`integerish(1)` | `NULL`)\cr  
    #'   Maximum number of feature changes. `NULL` (default) allows any number of changes.
    #' @param mu (`integerish(1)`)\cr  
    #'   The population size. Default is `20L`.
    #' @param termination_crit (`character(1)`|`NULL`)\cr 
    #'   Termination criterion, currently, two criterions are implemented: "gens" (default), 
    #'   which stops after `n_generations` generations,  and "genstag", which stops after 
    #'   the hypervolume did not improve for `n_generations` generations 
    #'   (the total number of generations is limited to 500).
    #' @param n_generations (`integerish(1)`)\cr  
    #'   The number of generations. Default is `175L`.   
    #' @param p_rec (`numeric(1)`)\cr  
    #'   Probability with which an individual is selected for recombination. Default is `0.71`.
    #' @param p_rec_gen (`numeric(1)`)\cr  
    #'   Probability with which a feature/gene is selected for recombination. Default is `0.62`.
    #' @param p_mut (`numeric(1)`)\cr  
    #'   Probability with which an individual is selected for mutation. Default is `0.73`.    
    #' @param p_mut_gen (`numeric(1)`)\cr  
    #'   Probability with which a feature/gene is selected for mutation. Default is `0.5`.
    #' @param p_mut_use_orig (`numeric(1)`)\cr  
    #'   Probability with which a feature/gene is reset to its original value in `x_interest` after mutation. Default is `0.4`.  
    #' @param k (`integerish(1)`)\cr  
    #'   The number of data points to use for the forth objective. Default is `1L`.
    #' @param weights (`numeric(1) | numeric(k)` | `NULL`)\cr  
    #'   The weights used to compute the weighted sum of dissimilarities for the forth objective. It is either a single value 
    #'   or a vector of length `k`. If it has length `k`, the i-th element specifies the weight of the i-th closest data point.
    #'   The values should sum up to `1`. `NULL` (default) means all data points are weighted equally. 
    #' @template lower_upper
    #' @param init_strategy (`character(1)`)\cr  
    #'   The population initialization strategy. Can be `icecurve` (default), `random`, `sd` or `traindata`. For more information,
    #'   see the `Details` section.
    #' @param use_conditional_mutator (`logical(1)`)\cr 
    #'   Should a conditional mutator be used? The conditional mutator generates plausible feature values based 
    #'   on the values of the other feature. Default is `FALSE`.
    #' @param quiet (`logical(1)`)\cr 
    #'  Should information about the optimization status be hidden? Default is `FALSE`.
    #' @param distance_function (`function()` | `'gower'` | `'gower_c'`)\cr 
    #'  The distance function to be used in the second and fourth objective.
    #'  Either the name of an already implemented distance function
    #'  ('gower' or 'gower_c') or a function.
    #'  If set to 'gower' (default), then Gower's distance (Gower 1971) is used;
    #'  if set to 'gower_c', a C-based more efficient version of Gower's distance is used.
    #'  A function must have three arguments  `x`, `y`, and `data` and should
    #'  return a `double` matrix with `nrow(x)` rows and maximum `nrow(y)` columns.

    initialize = function(predictor, epsilon = NULL, fixed_features = NULL, max_changed = NULL, mu = 20L, 
                          termination_crit = "gens", n_generations = 175L, p_rec = 0.71, p_rec_gen = 0.62, 
                          p_mut = 0.73, p_mut_gen = 0.5, p_mut_use_orig = 0.4, k = 1L, weights = NULL, 
                          lower = NULL, upper = NULL, init_strategy = "icecurve", use_conditional_mutator = FALSE, 
                          quiet = FALSE, distance_function = "gower") {
      
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

      assert_number(epsilon, lower = 0, null.ok = TRUE)
      if (!is.null(fixed_features)) {
        assert_names(fixed_features, subset.of = private$predictor$data$feature.names)
      }
      assert_integerish(max_changed, lower = 0, len = 1L, null.ok = TRUE)
      assert_integerish(mu, lower = 0, len = 1L)
      assert_choice(termination_crit, choices = c("gens", "genstag"))
      assert_integerish(n_generations, lower = 0, len = 1L)
      assert_number(p_rec, lower = 0, upper = 1)
      assert_number(p_rec_gen, lower = 0, upper = 1)
      assert_number(p_mut, lower = 0, upper = 1)
      assert_number(p_mut_gen, lower = 0, upper = 1)
      assert_number(p_mut_use_orig, lower = 0, upper = 1)
      assert_number(k, lower = 1, upper = nrow(private$predictor$data$X))
      assert_numeric(weights, any.missing = FALSE, len = k, null.ok = TRUE)
      assert_choice(init_strategy, choices = c("icecurve", "random", "sd", "traindata"))
      assert_flag(use_conditional_mutator)
      assert_flag(quiet)
      
      if (use_conditional_mutator) {
        if (!requireNamespace("trtf", quietly = TRUE)) {
          stop("Package 'trtf' needed for the conditional mutator to work. Please install it.", call. = FALSE)
        }
        if (!requireNamespace("partykit", quietly = TRUE)) {
          stop("Package 'partykit' needed for this function to work. Please install it.", call. = FALSE)
        }
        if (!requireNamespace("basefun", quietly = TRUE)) {
          stop("Package 'basefun' needed for this function to work. Please install it.", call. = FALSE)
        }
        
        nams_cs = names(predictor$data$X)
        nams_cs = setdiff(nams_cs, fixed_features)
        private$conditional_sampler = sapply(
          nams_cs, function(fname) ConditionalSampler$new(predictor$data$X, fname),
          simplify = FALSE, USE.NAMES = TRUE
        )
        names(private$conditional_sampler) = nams_cs
      }

      private$epsilon = epsilon
      private$fixed_features = fixed_features
      private$max_changed = max_changed
      private$mu = mu
      private$termination_crit = termination_crit
      private$n_generations = n_generations
      private$p_rec = p_rec
      private$p_rec_gen = p_rec_gen
      private$p_mut = p_mut
      private$p_mut_gen = p_mut_gen
      private$p_mut_use_orig = p_mut_use_orig
      private$k = k
      private$weights = weights
      private$init_strategy = init_strategy
      sdevs_num_feats = apply(Filter(is.numeric, private$predictor$data$X), 2L, sd)
      is_integer_col = names(which(private$param_set$class == "ParamInt"))
      sdevs_num_feats[is_integer_col] = as.integer(sdevs_num_feats[is_integer_col])
      private$sdevs_num_feats = sdevs_num_feats
      private$lower = lower
      private$upper = upper
      private$quiet = quiet
    },
    
    #' @description Plots the evolution of the mean and minimum objective values together with the dominated hypervolume over
    #' the generations. All values for a generation are computed based on all non-dominated individuals that emerged until 
    #' that generation.
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
      make_moc_statistics_plots(self$optimizer$archive, private$ref_point, centered_obj)
    },
    
    #' @description Calculates the dominated hypervolume of each generation. 
    #' 
    #' @return A `data.table` with the dominated hypervolume of each generation.
    get_dominated_hv = function() {
      if (is.null(self$optimizer)) {
        stop("There are no results yet. Please run `$find_counterfactuals` first.")
      }
      comp_domhv_all_gen(self$optimizer$archive, private$ref_point)
    },
    
    #' @description Visualizes two selected objective values of all emerged individuals in a scatter plot.
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
      make_moc_search_plot(self$optimizer$archive$data, objectives)
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
    epsilon = NULL,
    fixed_features = NULL,
    max_changed = NULL,
    mu = NULL,
    termination_crit = NULL,
    n_generations = NULL,
    p_rec = NULL,
    p_rec_gen = NULL,
    p_mut = NULL,
    p_mut_gen = NULL,
    p_mut_use_orig = NULL,
    k = NULL,
    weights = NULL,
    init_strategy = NULL,
    sdevs_num_feats = NULL,
    lower = NULL,
    upper = NULL,
    ref_point = NULL,
    .optimizer = NULL,
    conditional_sampler = NULL,
    quiet = NULL,

    run = function() {
      pred_column = private$get_pred_column()
      y_hat_interest = private$predictor$predict(private$x_interest)[[pred_column]]
      private$ref_point = c(min(abs(y_hat_interest - private$desired_prob)), 1, ncol(private$x_interest), 1)
      
      private$.optimizer = moc_algo(
        predictor = private$predictor,
        x_interest = private$x_interest,
        pred_column = pred_column,
        target = private$desired_prob,
        param_set = private$param_set,
        lower = private$lower,
        upper = private$upper,
        sdevs_num_feats = private$sdevs_num_feats,
        epsilon = private$epsilon,
        fixed_features = private$fixed_features,
        max_changed = private$max_changed,
        mu = private$mu,
        termination_crit = private$termination_crit,
        n_generations = private$n_generations,
        p_rec = private$p_rec,
        p_rec_gen = private$p_rec_gen,
        p_mut = private$p_mut,
        p_mut_gen = private$p_mut_gen,
        p_mut_use_orig = private$p_mut_use_orig,
        k = private$k,
        weights = private$weights,
        init_strategy = private$init_strategy,
        distance_function = private$distance_function,
        cond_sampler = private$conditional_sampler,
        ref_point = private$ref_point,
        quiet = private$quiet
      )

      unique(private$.optimizer$result[, names(private$x_interest), with = FALSE])
    },

    print_parameters = function() {
      cat(" - epsilon: ", private$epsilon, "\n")
      cat(" - fixed_features: ", private$fixed_features, "\n")
      cat(" - init_strategy: ", private$init_strategy, "\n")
      cat(" - k: ", private$k, "\n")
      cat(" - lower: ", private$lower, "\n")
      cat(" - max_changed: ", private$max_changed, "\n")
      cat(" - mu: ", private$mu, "\n")
      cat(" - termination_crit: ", private$termination_crit, "\n")    
      cat(" - n_generations: ", private$n_generations, "\n")
      cat(" - p_mut: ", private$p_mut, "\n")
      cat(" - p_mut_gen: ", private$p_mut_gen, "\n")
      cat(" - p_mut_use_orig: ", private$p_mut_use_orig, "\n")
      cat(" - p_rec: ", private$p_rec, "\n")
      cat(" - p_rec_gen: ", private$p_rec_gen, "\n")
      cat(" - upper: ", private$upper)
      cat(" - weights: ", private$weights, "\n")
    }
  )
)

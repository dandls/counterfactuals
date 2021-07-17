#' MOC for Regression Tasks
#' 
#' @template moc_info
#'
#' @examples 
#' if (require("randomForest")) {
#'   # Train a model
#'   rf = randomForest(mpg ~ ., data = mtcars)
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf)
#'   # Find counterfactuals
#'   moc_regr = MOCRegr$new(predictor, n_generations = 30L)
#'   cfactuals = moc_regr$find_counterfactuals(x_interest = mtcars[1L, ], desired_outcome = c(22, 26))
#'   # Print the results
#'   cfactuals$data
#'   # Plot evolution
#'   moc_classif$plot_statistics()
#' }
#' 
#' 
#' @export
MOCRegr = R6::R6Class("MOCRegr", inherit = CounterfactualMethodRegr,
  
  public = list(
    #' @description Create a new `MOCRegr` object.
    #' @template predictor
    #' @param epsilon (`numeric(1)` | `NULL`)\cr  
    #'   If not `NULL`, candidates whose distance between their prediction and target exceeds epsilon are penalized.
    #'   Defauls is `NULL`, which means no penalization
    #' @param fixed_features (`character()` | `NULL`)\cr  
    #'   Names of features that are not allowed to change. `NULL` (default) allows to change all features.
    #' @param max_changed (`integerish(1)` | `NULL`)\cr  
    #'   Maximum number of feature changes. `NULL` (default) allows any number of changes.
    #' @param mu (`integerish(1)`)\cr  
    #'   The population size. Default is `20L`.
    #' @param n_generations (`integerish(1)`)\cr  
    #'   The number of generations. Default is `175L`.   
    #' @param p_rec (`numeric(1)`)\cr  
    #'   Probability with which a child is chosen for recombination. Default is `0.57`.
    #' @param p_rec_gen (`numeric(1)`)\cr  
    #'   Probability with which a feature/gene is chosen for recombination. Default is `0.85`.  
    #' @param p_rec_use_orig (`numeric(1)`)\cr  
    #'   Probability with which a feature/gene is reset to the feature value of `x_interest` after recombination. Default is `0.88`.    
    #' @param p_mut (`numeric(1)`)\cr  
    #'   Probability with which a child is chosen for mutation. Default is `0.79`.    
    #' @param p_mut_gen (`numeric(1)`)\cr  
    #'   Probability with which a feature/gene is chosen for mutation. Default is `0.56`.   
    #' @param p_mut_use_orig (`numeric(1)`)\cr  
    #'   Probability with which a feature/gene is reset to the feature value of `x_interest` after mutation. Default is `0.32`.    
    #' @param k (`integerish(1)`)\cr  
    #'   The number of nearest neighbors to use for the forth objective. Default is `1L`.
    #' @param weights (`numeric(1) | numeric(k)` | `NULL`)\cr  
    #'   The weights used to compute the weighted average distance for the forth objective. It is either a single value 
    #'   or a vector of length `k`. If it has length `k`, the first value corresponds to the nearest neighbor and so on. 
    #'   The values should sum up to `1`. Default is `NULL` which means all neighbors are weighted equally. 
    #' @template lower_upper
    #' @param init_strategy (`character(1)`)\cr  
    #'   The population initialization strategy. Can be `random` (default), `sd` or `icecurve`. Further information
    #'   are given in the `details` section.
    #' @param use_conditional_mutator (`logical(1)`)\cr 
    #'   Should a conditional mutator be used? The conditional mutator generates plausible feature values conditional 
    #'   on the values of the other feature. Default is `FALSE`.
    initialize = function(predictor, epsilon = NULL, fixed_features = NULL, max_changed = NULL, mu = 20L,
                          n_generations = 175L, p_rec = 0.57, p_rec_gen = 0.85, p_rec_use_orig = 0.88, p_mut = 0.79,
                          p_mut_gen = 0.56, p_mut_use_orig = 0.32, k = 1L, weights = NULL, lower = NULL, upper = NULL, 
                          init_strategy = "random", use_conditional_mutator = FALSE) {
      
      super$initialize(predictor, lower, upper)
      
      assert_number(epsilon, lower = 0, null.ok = TRUE)
      if (!is.null(fixed_features)) {
        assert_names(fixed_features, subset.of = private$predictor$data$feature.names)
      } 
      assert_integerish(max_changed, lower = 0, len = 1L, null.ok = TRUE)
      assert_integerish(mu, lower = 0, len = 1L)
      assert_integerish(n_generations, lower = 0, len = 1L)
      assert_number(p_rec, lower = 0, upper = 1)
      assert_number(p_rec_gen, lower = 0, upper = 1)
      assert_number(p_rec_use_orig, lower = 0, upper = 1)
      assert_number(p_mut, lower = 0, upper = 1)
      assert_number(p_mut_gen, lower = 0, upper = 1)
      assert_number(p_mut_use_orig, lower = 0, upper = 1)
      assert_number(k, lower = 1, upper = nrow(private$predictor$data$X))
      assert_numeric(weights, any.missing = FALSE, len = k, null.ok = TRUE)
      assert_choice(init_strategy, choices = c("random", "sd", "icecurve"))
      assert_flag(use_conditional_mutator)
      
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
        
        private$conditional_sampler = sapply(
          names(predictor$data$X), function(fname) ConditionalSampler$new(predictor$data$X, fname),
          simplify = FALSE, USE.NAMES = TRUE
        )
        names(private$conditional_sampler) = names(predictor$data$X)
      }
      
      private$epsilon = epsilon
      private$fixed_features = fixed_features
      private$max_changed = max_changed
      private$mu = mu
      private$n_generations = n_generations
      private$p_rec = p_rec
      private$p_rec_gen = p_rec_gen
      private$p_rec_use_orig = p_rec_use_orig
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
    },
    
    #' @description Plots the evolution of the mean and minimum objective values together with the dominated hypervolume over
    #' the generations. All values for a generation are calculated based on all nondominated individuals of that generation.
    #' For computing the dominated hypervolume the `miesmuschel:::domhv` function is used.
    #' @param centered_obj (`logical(1)`)\cr  
    #'   Should the objective values be centered? If yes, each objective value is visualized in a separate plot, since
    #'   they (usually) have different scales. Otherwise, they are visualized in a single plot. Default is `TRUE`.
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
    #' The `miesmuschel:::domhv` function is used for this.
    #' 
    #' @return A `data.table` with the dominated hypervolume of each generation
    get_dominated_hv = function() {
      if (is.null(self$optimizer)) {
        stop("There are no results yet. Please run `$find_counterfactuals` first.")
      }
      comp_domhv_all_gen(self$optimizer$archive, private$ref_point)
    },
    
    #' @description Visualizes all individuals of all generations in a scatter plot with two objectives on the axes.
    #' @param objectives (`character(2)`)\cr  
    #'   The two objectives to be shown in the plot. Possible values are: "dist_target", "dist_x_interest, "nr_changed" 
    #'   and "dist_train".
    plot_search = function(objectives = c("dist_target", "dist_x_interest")) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
      }
      if (is.null(self$optimizer)) {
        stop("There are no results yet. Please run `$find_counterfactuals` first.")
      }
      assert_names(objectives, subset.of = c("dist_target", "dist_x_interest", "nr_changed", "dist_train"))
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
    n_generations = NULL,
    p_rec = NULL,
    p_rec_gen = NULL,
    p_rec_use_orig = NULL,
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
        sdevs_num_feats = private$sdevs_num_feats,
        epsilon = private$epsilon,
        fixed_features = private$fixed_features,
        max_changed = private$max_changed,
        mu = private$mu,
        n_generations = private$n_generations,
        p_rec = private$p_rec,
        p_rec_gen = private$p_rec_gen,
        p_rec_use_orig = private$p_rec_use_orig,
        p_mut = private$p_mut,
        p_mut_gen = private$p_mut_gen,
        p_mut_use_orig = private$p_mut_use_orig,
        k = private$k,
        weights = private$weights,
        init_strategy = private$init_strategy,
        cond_sampler = private$conditional_sampler
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
      cat(" - n_generations: ", private$n_generations, "\n")
      cat(" - p_mut: ", private$p_mut, "\n")
      cat(" - p_mut_gen: ", private$p_mut_gen, "\n")
      cat(" - p_mut_use_orig: ", private$p_mut_use_orig, "\n")
      cat(" - p_rec: ", private$p_rec, "\n")
      cat(" - p_rec_gen: ", private$p_rec_gen, "\n")
      cat(" - p_rec_use_orig: ", private$p_rec_use_orig, "\n")
      cat(" - upper: ", private$upper)
      cat(" - weights: ", private$weights, "\n")
    }
  )
)

#' MOC for Classification Tasks
#' 
#' @description 
#' `MOC` (Dandl et. al 2020) solves a multi-objective optimization problem to find counterfactuals. The four objectives
#' to minimize are:
#' \enumerate{
#'    \item {Distance between `x_interest` and `desired_prob`}
#'    \item {Distance between `x_interest` and a counterfactual}
#'    \item {Number of feature changes}
#'    \item {(Weighted) average distance between a counterfactual and its `k` nearest observed data points}
#' }  
#' 
#' 
#' For optimization it uses the NSGA II algorithm (Deb et. al 2002) with mixed integer evolutionary 
#' strategies by Li et al. (2013). 
#' 
#' 
#' @details 
#' 
#' Several population initialization strategies are available:
#' \enumerate{
#'    \item {`random`: Sample from numerical feature ranges and discrete feature values from `predictor$data$X`. 
#'    Some features values are randomly reset to the values of `x_interest`.}
#'    \item {`icecurve`: Sample from numerical feature ranges and discrete feature values from `predictor$data$X`. 
#'    The higher the ICE curve variance of a feature, the lower the probability that
#'    values of this feature are reset to the values of `x_interest`.}
#'    \item {`sd`: Sample from numerical feature ranges that are limited by the feature standard deviations extracted
#'    from `predictor$data$X`. Some features values are randomly reset to the values of `x_interest`.}
#' }  
#' 
#' 
#' The R package `miesmuschel` implements the mixed integer evolutionary strategies.
#' 
#' The Gower's dissimilarity measure proposed by Kaufman and Rousseeuw (1990) computes all required distances. 
#' It is implemented by \link[StatMatch]{gower.dist}.
#' 
#' 
#' 
#' @examples 
#' if (require("randomForest")) {
#'   # Train a model
#'   rf = randomForest(Species ~ ., data = iris)
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf, type = "prob")
#'   # Find counterfactuals
#'   moc_classif = MOCClassif$new(predictor)
#'   cfactuals = moc_classif$find_counterfactuals(
#'     x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
#'   )
#'   # Print the results
#'   cfactuals$data
#' }
#' 
#' @references 
#' 
#' Binder Martin (2021). miesmuschel: Mixed Integer Evolutionary Strategies. R
#' package version 0.0.0-9000. https://github.com/mlr-org/miesmuschel
#' 
#' Dandl, Susanne, Christoph Molnar, Martin Binder, and Bernd Bischl. 2020. “Multi-Objective Counterfactual Explanations.” 
#' In Parallel Problem Solving from Nature – PPSN XVI, edited by Thomas Bäck, Mike Preuss, André Deutz, Hao Wang, Carola Doerr, 
#' Michael Emmerich, and Heike Trautmann, 448–69. Cham: Springer International Publishing.
#' 
#' Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. A. M. T. (2002). A fast and elitist multiobjective genetic algorithm: NSGA-II. 
#' IEEE transactions on evolutionary computation, 6(2), 182-197.
#' 
#' R. Li et al., "Mixed Integer Evolution Strategies for Parameter Optimization," in Evolutionary Computation, vol. 21, no. 1, 
#' pp. 29-64, March 2013, doi: 10.1162/EVCO_a_00059.
#' 
#' @export
MOCClassif = R6::R6Class("MOCClassif", inherit = CounterfactualMethodClassif,

  public = list(
    
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
    
    get_dominated_hv = function() {
      if (is.null(self$optimizer)) {
        stop("There are no results yet. Please run `$find_counterfactuals` first.")
      }
      comp_domhv_all_gen(self$optimizer$archive, private$ref_point)
    },
    
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

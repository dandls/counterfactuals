#' @export
MOCClassif = R6::R6Class("MOCClassif", inherit = CounterfactualMethodClassif,

  public = list(
    
    initialize = function(predictor, epsilon = NULL, fixed_features = NULL, max_changed = NULL,
      mu = 50L, n_generations = 50L, p_rec = 0.9, p_rec_gen = 0.7, p_rec_use_orig = 0.7, p_mut = 0.8,
      p_mut_gen = 0.5, p_mut_use_orig = 0.2, k = 1L, weights = NULL, lower = NULL, upper = NULL,
      init_strategy = "random") {

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
      private$sdevs_dbl_feats = apply(Filter(is.double, private$predictor$data$X), 2L, sd)
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
      make_moc_statistics_plots(self$optimizer$archive$data, private$ref_point, centered_obj)
    },
    
    get_dominated_hv = function() {
      if (is.null(self$optimizer)) {
        stop("There are no results yet. Please run `$find_counterfactuals` first.")
      }
      rel_cols = c("batch_nr", "dist_target", "dist_x_interest", "nr_changed", "dist_train")
      fitness_values = self$optimizer$archive$data[, ..rel_cols]
      comp_domhv_all_gen(fitness_values, private$ref_point)
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
    sdevs_dbl_feats = NULL,
    lower = NULL,
    upper = NULL,
    ref_point = NULL,

    .optimizer = NULL,

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
        sdevs_dbl_feats = private$sdevs_dbl_feats,
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
        init_strategy = private$init_strategy
      )

      unique(private$.optimizer$result[, names(private$x_interest), with = FALSE])
    },

    print_parameters = function() {
      cat(" - epsilon: ", private$epsilon, "\n")
      cat(" - fixed_features: ", private$fixed_features, "\n")
      cat(" - max_changed: ", private$max_changed, "\n")
      cat(" - mu: ", private$mu, "\n")
      cat(" - n_generations: ", private$n_generations, "\n")
      cat(" - p_rec: ", private$p_rec, "\n")
      cat(" - p_rec_gen: ", private$p_rec_gen, "\n")
      cat(" - p_rec_use_orig: ", private$p_rec_use_orig, "\n")
      cat(" - p_mut: ", private$p_mut, "\n")
      cat(" - p_mut_gen: ", private$p_mut_gen, "\n")
      cat(" - p_mut_use_orig: ", private$p_mut_use_orig, "\n")
      cat(" - k: ", private$k, "\n")
      cat(" - weights: ", private$weights, "\n")
      cat(" - init_strategy: ", private$init_strategy, "\n")
      cat(" - lower: ", private$lower, "\n")
      cat(" - upper: ", private$upper)
    }
  )
)

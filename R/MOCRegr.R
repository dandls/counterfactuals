#' @export
MOCRegr = R6::R6Class("MOCRegr", inherit = CounterfactualMethodRegr,
  
  public = list(
    initialize = function(predictor, epsilon = NULL, fixed_features = NULL, max_changed = NULL, 
                          mu = 50L, generations = 50L, p_rec = 0.9, p_rec_gen = 0.7, p_rec_use_orig = 0.7, p_mut = 0.2, 
                          p_mut_gen = 0.5, p_mut_use_orig = 0.2, k = 1L, weights = NULL, lower = NULL, upper = NULL, 
                          conditionals = FALSE, init_strategy = "random", track_infeas = TRUE) {
      
      super$initialize(predictor, lower, upper)
      
      # Sanitize features
      feature_names = private$predictor$data$feature.names
      if (is.numeric(fixed_features)) {
        assert_integerish(fixed_features, lower = 1L, upper = length(feature_names), null.ok = TRUE)
        fixed_features = feature_names[fixed_features]
      }
      assert_character(fixed_features, null.ok = TRUE, unique = TRUE)
      if (!is.null(fixed_features)) {
        assert_names(fixed_features, subset.of = feature_names)
      }
      
      
      # fit conditionals if conditionals != false 
      if (is.logical(conditionals) && conditionals) {
        conditionals = fit_conditionals(private$predictor$data$X, ctrl = partykit::ctree_control(maxdepth = 5L))
      } 
      
      private$epsilon = epsilon
      private$fixed_features = fixed_features
      private$max_changed = max_changed
      private$mu = mu
      private$generations = generations
      private$p_rec = p_rec
      private$p_rec_gen = p_rec_gen
      private$p_rec_use_orig = p_rec_use_orig
      private$p_mut = p_mut
      private$p_mut_gen = p_mut_gen
      private$p_mut_use_orig = p_mut_use_orig
      private$k = k
      private$weights = weights
      private$conditionals = conditionals
      private$init_strategy = init_strategy
      private$track_infeas = track_infeas
      sdevs_num_feats = apply(Filter(is.double, private$predictor$data$X), 2L, sd)
      private$sdevs_num_feats = sdevs_num_feats[!names(sdevs_num_feats) %in% fixed_features]
      private$lower = lower
      private$upper = upper
  
    }
  ),
  private = list(
    epsilon = NULL,
    fixed_features = NULL,
    max_changed = NULL,
    mu = NULL,
    generations = NULL,
    p_rec = NULL,
    p_rec_gen = NULL,
    p_rec_use_orig = NULL,
    p_mut = NULL,
    p_mut_gen = NULL,
    p_mut_use_orig = NULL,
    k = NULL,
    weights = NULL,
    conditionals = NULL,
    init_strategy = NULL,
    track_infeas = NULL,
    sdevs_num_feats = NULL,
    lower = NULL,
    upper = NULL,
    ecr_results = NULL,
    
    run = function() {
      pred_column = private$get_pred_column()
      moc_algo(
        predictor = private$predictor,
        x_interest = private$x_interest,
        pred_column = pred_column,
        desired_y_hat_range = private$desired_outcome,
        param_set = private$param_set,
        lower = private$lower,
        upper = private$upper,
        sdevs_num_feats = private$sdevs_num_feats,
        epsilon = private$epsilon,
        fixed_features = private$fixed_features,
        max_changed = private$max_changed,
        mu = private$mu,
        generations = private$generations,
        p_rec = private$p_rec,
        p_rec_gen = private$p_rec_gen,
        p_rec_use_orig = private$p_rec_use_orig,
        p_mut = private$p_mut,
        p_mut_gen = private$p_mut_gen,
        p_mut_use_orig = private$p_mut_use_orig,
        k = private$k,
        weights = private$weights,
        conditionals = private$conditionals,
        init_strategy = private$init_strategy,
        track_infeas = private$track_infeas
      )
    

    },
    print_parameters = function() {
      cat("\t", "epsilon: ", private$epsilon)
    }
  )
)

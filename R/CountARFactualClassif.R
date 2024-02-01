#' CountARFactual (ARF-based Counterfactuals) for Classification Tasks
#' 
#' @description 
#' CountARFactuals generates counterfactuals based on the adversarial random forests introduced in Watson et. al 2023
#' 
#' @references 
#'
#' Watson, D. S., Blesch, K., Kapar, J., and Wright, M. N. (2023). Adversarial
#' random forests for density estimation and generative modeling. In Proceedings
#' of the 26th International Conference on Artificial Intelligence and Statistics, 
#' pages 5357–5375. PMLR.
#' 
#' @examples 
#' if (require("randomForest")) {
#'  \donttest{
#'   # Train a model
#'   rf = randomForest(Species ~ ., data = iris)
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf, type = "prob")
#'   # Find counterfactuals for x_interest
#'   arf_classif = CountARFactualClassif$new(predictor)
#'  
#'   cfactuals = arf_classif$find_counterfactuals(
#'     x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
#'   )
#'   # Print the counterfactuals
#'   cfactuals$data
#'   # Plot evolution of hypervolume and mean and minimum objective values
#'   cfactuals$evaluate_set()
#'   cfactuals$plot_parallel()
#'   }
#' }
#' 
#' @export
CountARFactualClassif = R6::R6Class("CountARFactualClassif", 
  inherit = CounterfactualMethodClassif,
  
  public = list(
    #' @description Create a new `MOCClassif` object.
    #' @template predictor
    #' @param max_feats_to_change (`numeric(1)`)\cr  
    #' The maximum number of features allowed to be altered (default is the number of features in the used data set (`predictor$data$n.features`)).
    #' @param n_synth (`numeric(1)`) \cr 
    #' The number of samples drawn from the marginal distributions (default 10L).
    #' @param n_iterations (`numeric(1)`) \cr 
    #' The number of iteration. In each iteration a new terminal node is chosen 
    #' from which the `n_synth` candidates are drawn. Default 50L.
    #' @param feature_selector (`character(1)`)\cr
    #' The method to choose features that are fixed for a counterfactual and which 
    #' are part of the conditioning set when choosing tree paths. The default 
    #' `random_importance` means that the probability of being in the conditioning set 
    #' is proportional to how unimportant a feature is. 
    #' The strategy `importance` means that all features are chosen except for 
    #' (or less than) the `max_feats_to_change` features that are most important. 
    #' The strategy `random` randomly chooses up to `max_feats_to_change` 
    #' features that are not part of the conditioning set. All others are part.
    #' @param importance_method (`character(1)`)\cr 
    #' The local importance method to variables for the conditioning set. 
    #' Ignored if `feature_selector = "random"`. 
    #' Either "fastshap" based on the `fastshap` package (default) or "icesd" 
    #' based on the standard deviation of the ICE curve is possible.
    #' 
    #' @export
    initialize = function(predictor, max_feats_to_change = predictor$data$n.features, n_synth = 10L, n_iterations = 50L, feature_selector = "random_importance", importance_method = "icesd") { 
      # TODO: add other hyperparameter
      super$initialize(predictor)
      checkmate::assert_integerish(max_feats_to_change, lower = 1L, upper = predictor$data$n.features)
      checkmate::assert_integerish(n_synth, lower = 1L)
      checkmate::assert_choice(feature_selector, choices = c("importance", "random", "random_importance"))
      checkmate::assert_choice(importance_method, choices = c("fastshap", "icesd"))
      private$max_feats_to_change = max_feats_to_change
      private$feature_selector = feature_selector
      private$max_feats_to_change = max_feats_to_change
      private$n_synth = n_synth
      private$n_iterations = n_iterations
      private$importance_method = importance_method
    }
  ),
  
  private = list(
    max_feats_to_change = NULL,
    n_synth = NULL,
    n_iterations = NULL,
    feature_selector = NULL,
    importance_method = NULL,
    run = function() {
      
      # Fit ARF
      dat = copy(private$predictor$data$get.x())
      dat[, yhat := private$predictor$predict(dat)[,private$desired_class]]
      arf = adversarial_rf(dat, always.split.variables = "yhat")
      psi = forde(arf, dat)
      
      # Conditional sampling
      ##  Select conditioning set
      if (grepl("importance", private$feature_selector)) {
        # Shapley values as local importance
        if (private$importance_method == "fastshap") {
          if (!requireNamespace("fastshap", quietly = TRUE)) {
            stop("Package 'fastshap' needed for this measuring importance. Please install it.", call. = FALSE)
          }
          pfun = function(object, newdata) { 
            unname(object$predict(newdata)[,private$desired_class])
          }
          shap = fastshap::explain(private$predictor, X = private$predictor$data$get.x(), 
            pred_wrapper = pfun, newdata = private$x_interest,
            nsim = 1000)
          vim = abs(shap[1, ])
          # ICE curve standard deviation as local importance
        } else if (private$importance_method == "icesd") {
          param_set = make_param_set(private$predictor$data$get.x())
          vim = get_ICE_sd(private$x_interest, private$predictor, param_set)
        }
      }
      
      x_interest = private$x_interest
      # TODO: which desired_prob to use when interval??
      # --> Currently mean
      x_interest[, yhat := mean(private$desired_prob)] 
      synth = data.table()
      for (i in 1:private$n_iterations) {
        feats_not_to_change = sample(seq(
          private$predictor$data$n.features - private$max_feats_to_change, 
          private$predictor$data$n.features - 1L), size = 1L)
        
        if (private$feature_selector == "importance") {
          ordered_features = names(sort(vim)) # Smallest (= less important) first
          cols = ordered_features[1:feats_not_to_change]
        } else if (private$feature_selector == "random_importance") {
          #bassert_true(all(names(vim) == private$predictor$data$feature.names))
          # get probabilities
          p_min = 0.01
          p_max = 0.99
          p_selected = 1 - ((vim - min(vim)) * (p_max - p_min) / 
              (max(vim) - min(vim) + sqrt(.Machine$double.eps)) + p_min)
          cols = sample(private$predictor$data$feature.names, 
            size = feats_not_to_change,
            prob = p_selected)
        } else if (private$feature_selector == "random") {
          cols = sample(private$predictor$data$feature.names, 
            size = feats_not_to_change)
        }
        cols = c("yhat", cols)
        # TODO: Better to condition on yhat >= target_prob (already possible)
        fixed = x_interest[, ..cols]
        synth = rbind(synth, forge(psi, n_synth = private$n_synth, evidence = fixed))
      }
      x_interest[, yhat:= NULL]
      synth[, yhat := NULL]
      
      # Recode factors to original factor levels
      factor_cols = names(which(sapply(predictor$data$X, is.factor)))
      for (factor_col in factor_cols) {
        fact_col_pred = predictor$data$X[[factor_col]]
        value =  factor(synth[[factor_col]], levels = levels(fact_col_pred), 
          ordered = is.ordered(fact_col_pred))
        set(synth, j = factor_col, value = value)
      }
      
      # Keep only valid counterfactuals
      cfs = synth[between(private$predictor$predict(synth)[,private$desired_class], 
        private$desired_prob[1L], private$desired_prob[2L]),]
    },
    
    print_parameters = function() {
      #TODO
    }
  )
)

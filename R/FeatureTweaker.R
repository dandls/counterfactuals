#' @import data.table
#' @import featureTweakR
#' 
#' @export
FeatureTweaker = R6::R6Class("FeatureTweaker",
  inherit = Counterfactuals,
  private = list(
    n_counterfactuals = NULL,
    cfactuals = NULL,
    epsilon = NULL,
    ktree = NULL,
    y_classes = NULL,
    preprocess = function() {
      # TODO:
    },
    calculate = function() {
      cfs_list = lapply(seq_len(private$n_counterfactuals), private$comp_counterfactual)
      cfs = rbindlist(cfs_list)
      # TODO: add dist_x column
      # TODO: add pred column
      setnames(cfs, names(cfs), private$predictor$data$feature.names)
      cfs[, "dist_x_interest" := gower_dist(private$x_interest, cfs, n_cores = 1L)]
      private$cfactuals = cfs
    },
    aggregate = function() {
      # TODO: Removed duplicates
      # TODO: warning if less then `n_counterfactuals` cfs were found
      cfactuals = private$cfactuals
      cfactuals_feats = cfactuals[, names(private$x_interest), with = FALSE]
      pred = private$predictor$predict(cfactuals_feats)
 
      # Check if more than one column as binary classification tasks may be one-hot encoded
      if (length(private$y_classes) > 1) {
        pred = private$one_hot_to_one_col(pred)
      }
      pred = pred[[1L]]
      n_changes = private$count_changes(cfactuals_feats)
      cfactuals[, c("pred", "nr_changed") := list(pred, n_changes)]
      setorder(cfactuals, dist_x_interest)
      private$.results = private$make_results_list(cfactuals)
    },
    
    # Randomness induced by resampling when extracting the rule from the rf (as long as ktree < ntree)  
    comp_counterfactual = function(i) {

      rf = private$predictor$model
      rules = featureTweakR::getRules(rf, ktree = private$ktree, resample = TRUE)
      e_satisfactory = featureTweakR::set.eSatisfactory(rules, epsiron = private$epsilon)
      # .dopar makes no difference as only one observation in newdata
      tweaks = featureTweakR::tweak(
        e_satisfactory, rf, newdata = private$x_interest, 
        label.from = private$y_hat_interest, label.to = private$desired_outcome, .dopar = FALSE
      )
      tweaks$suggest
    },
    one_hot_to_one_col = function(df) {
      as.data.table(colnames(df)[apply(df, 1, which.max)])
    }


  ),
  public = list(
    # tweak function does not allow for fixed features
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, 
                          desired_outcome = NULL, ktree = 30L, epsilon = 0.1, 
                          lower = NULL, upper = NULL) {
                          
      # TODO: does not work with "formula random forest" and ONLY with RF (not other methods) -> Check this
      
      is_randomForest = checkmate::test_multi_class(predictor$model, "randomForest")
      if (!is_randomForest) {
        stop("`FeatureTweaker` only works for randomForest models.")
      }
      
      is_randomForest_formula = checkmate::test_multi_class(predictor$model, "randomForest.formula")
      if (is_randomForest_formula) {
        stop("`FeatureTweaker` cannot be applied to randomForest models specified with a formula.")
      }
      
      predictor$task = predictor$model$type
      private$check_that_classif_task(predictor)

      
      private$predictor = predictor
      private$n_counterfactuals = n_counterfactuals
      private$epsilon = epsilon
      private$ktree = ktree
      private$param_set = private$make_param_set(lower, upper)
      
      # Question: Do we need scaling? -> yes, because of distance calculation
      
      if (!is.null(x_interest)) {
        self$find_counterfactuals(x_interest, desired_outcome)
      }
      
 
    },
    
    find_counterfactuals = function(x_interest, desired_outcome = NULL) {
      # TODO: Check if desired_outcome is in predictor$data$y
      
      private$x_interest = data.table::setDT(x_interest)
      y_hat_interest = private$predictor$predict(private$x_interest) # [1L, ]
      is_multiclass = ncol(y_hat_interest) > 2

      private$y_classes = names(y_hat_interest)
      # Check if more than one column as binary classification tasks may be one-hot encoded
      if (ncol(y_hat_interest) > 1) {
        y_hat_interest = private$one_hot_to_one_col(y_hat_interest)
      }
      y_hat_interest = y_hat_interest[[1]]

      if (!is_multiclass) {
        if (!is.null(desired_outcome)) {
          message("`desired_outcome` is set to the opposite class of `x_interest` for binary classification tasks.")
        }
        if (length(private$y_classes) > 1) {
          desired_outcome = setdiff(private$y_classes, y_hat_interest)
        } else {
          desired_outcome = private$predictor$class
        }
        
      } 
      
      if (is.null(desired_outcome)) {
        stop("The `desired_outcome` has to be specified for multiclass classification tasks.")
      }
      

      private$desired_outcome = desired_outcome
      private$y_hat_interest = y_hat_interest
      private$run()
    }
  )
)





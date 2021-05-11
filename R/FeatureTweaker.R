#' @import data.table
#' @import featureTweakR
#' 
#' @export
FeatureTweaker = R6::R6Class("FeatureTweaker",
  inherit = CounterfactualsClassificationOnly,
  private = list(
    n_counterfactuals = NULL,
    cfactuals = NULL,
    epsilon = NULL,
    ktree = NULL,

    calculate = function() {
      cfs_list = lapply(seq_len(private$n_counterfactuals), private$comp_counterfactual)
      cfs = rbindlist(cfs_list)
      setnames(cfs, names(cfs), private$predictor$data$feature.names)
      cfs[, "dist_x_interest" := gower_dist(private$x_interest, cfs, n_cores = 1L)]
      private$cfactuals = cfs
    },
    aggregate = function() {
      # TODO: Removed duplicates?
      # TODO: warning if less then `n_counterfactuals` cfs were found?
      cfactuals = private$cfactuals
      cfactuals_feats = cfactuals[, names(private$x_interest), with = FALSE]
      pred = private$predictor$predict(cfactuals_feats)
 
      if (private$is_pred_one_hot) {
        pred = private$one_hot_to_one_col(pred)
      }
      pred = pred[[1L]]
      n_changes = private$count_changes(cfactuals_feats)
      cfactuals[, c("pred", "nr_changed") := list(pred, n_changes)]
      setorder(cfactuals, dist_x_interest)
      private$.results = private$make_results_list(cfactuals)
    },
    
   
    comp_counterfactual = function(i) {
      # Rule extraction is a random process as long as ktree < ntree
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
    
    check_standardization = function(dt) {
      is_mean_0 = function(x) isTRUE(all.equal(mean(x, na.rm = TRUE), 0))
      is_sd_1 = function(x) isTRUE(all.equal(sd(x, na.rm = TRUE), 1))
      have_correct_values = dt[, sapply(.SD, function(x) list(is_mean_0(x), is_sd_1(x)))]
      all(have_correct_values == TRUE)
    },
    
    get_desired_outcome_binary_class = function(y_hat_interest) {
      # RandomForest prediction is one-hot encoded (here this means: has more than one column). 
      # Therefore we infer the `desired_outcome` with the colnames.
      # Only if the class was specified in the iml predictor, there is only one column in the prediction
      # and we infer the `desired_outcome` with the class.
      if (private$is_pred_one_hot) {
        desired_outcome = setdiff(private$prediction_colnames, y_hat_interest)
      } else {
        desired_outcome = private$predictor$class
      }
      desired_outcome
    }

  ),
  public = list(
    
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, 
                          desired_outcome = NULL, ktree = 30L, epsilon = 0.1, 
                          lower = NULL, upper = NULL) {
      
      is_randomForest = checkmate::test_multi_class(predictor$model, "randomForest")
      if (!is_randomForest) {
        stop("`FeatureTweaker` only works for randomForest models.")
      }
      
      is_randomForest_formula = checkmate::test_multi_class(predictor$model, "randomForest.formula")
      if (is_randomForest_formula) {
        stop("`FeatureTweaker` cannot be applied to randomForest models specified with a formula.")
      }
      
      categorical_train_variables = any("categorical" %in% predictor$data$feature.types)
      if (categorical_train_variables) {
        stop("`FeatureTweaker` cannot handle categorical variables in the training data.")
      }
      
      all_features_standardized = private$check_standardization(predictor$data$X)
      if (!all_features_standardized) {
        stop("`FeatureTweaker` can only handle standardized features in training data.")
      }
      
      # TODO: Checks that ktree <= total_numer_of_trees (maybe incl warning that if equal than only 
      # one counterfactual can be found)
      
      predictor$task = predictor$model$type
      private$check_that_classif_task(predictor)
      
      private$predictor = predictor
      private$n_counterfactuals = n_counterfactuals
      private$epsilon = epsilon
      private$ktree = ktree
      private$param_set = private$make_param_set(lower, upper)
      
      run_method = !is.null(x_interest)
      if (run_method) {
        self$find_counterfactuals(x_interest, desired_outcome)
      }
 
    }
  )
)





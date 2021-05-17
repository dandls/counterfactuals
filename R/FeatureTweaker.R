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
    set_n_counterfactuals_to_1 = FALSE,

    calculate = function() {
      cfs_list = lapply(seq_len(private$n_counterfactuals), private$comp_counterfactual)
      cfs = rbindlist(cfs_list)
      setnames(cfs, names(cfs), private$predictor$data$feature.names)
      cfs[, "dist_x_interest" := gower_dist(private$x_interest, cfs, n_cores = 1L, param_set = private$param_set)]
      private$cfactuals = cfs
    },
    aggregate = function() {
      # TODO: Removed duplicates?
      # TODO: warning if less then `n_counterfactuals` cfs were found?
      cfactuals = private$cfactuals
      cfactuals_feats = cfactuals[, names(private$x_interest), with = FALSE]
      pred = private$predictor$predict(cfactuals_feats)
 
      if (private$is_pred_one_hot) {
        pred = private$one_hot_to_one_col(pred)  # COMMENT maybe one_hot_to_one_col shouldn't be a private class function, but should be just an utils function in util.R or so
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
      is_mean_0 = function(x) isTRUE(all.equal(mean(x, na.rm = TRUE), 0))  # COMMENT why the all.equal here? we need to check for absence of names / attributes or so?
      is_sd_1 = function(x) isTRUE(all.equal(sd(x, na.rm = TRUE), 1))  # COMMENT as above
      have_correct_values = dt[, sapply(.SD, function(x) list(is_mean_0(x), is_sd_1(x)))]  # COMMENT maybe use c() instead of list() here (or is there a reason why you use list?)
      all(have_correct_values == TRUE)
    },
    
    get_desired_outcome_binary_class = function(is_pred_one_hot, y_hat_interest, prediction_colnames) {
      # RandomForest prediction is one-hot encoded (here this means: has more than one column). 
      # Therefore we infer the `desired_outcome` with the colnames.
      # Only if in the iml predictor the `class` argument was specified, there is just one column in the prediction
      # and we obtain the `desired_outcome` with this `class` argument.
      if (is_pred_one_hot) {
        desired_outcome = setdiff(prediction_colnames, y_hat_interest)
      } else {
        desired_outcome = private$predictor$class
      }
      desired_outcome  # COMMENT don't need desired_outcome here ,just do if (is_pred_one_hot) { setdiff(...) } else { private$predictor$class } (in more than one line) and it returns the result automatically
    },
    
    run_init_arg_checks = function(arg_list) {
      # TODO: Add remaining arg checks
      private$check_model(arg_list$predictor$model)  # COMMENT maybe call the functions something besides check_xxx, because the special meaning of check_xxx in checkmate
      private$check_ktree(arg_list$ktree, arg_list$n_counterfactuals, arg_list$predictor$model)
      private$check_training_data(arg_list$predictor$data)
      task = arg_list$predictor$model$type
      private$check_that_classif_task(task)
    },
    
    check_model = function(model) {
      is_randomForest = checkmate::test_multi_class(model, "randomForest")  # COMMENT why not just do assert_ ?
      if (!is_randomForest) {
        stop("`FeatureTweaker` only works for randomForest models.")
      }
      
      is_randomForest_formula = checkmate::test_multi_class(model, "randomForest.formula")  # COMMENT as above
      if (is_randomForest_formula) {
        stop("`FeatureTweaker` cannot be applied to randomForest models specified with a formula.")
      }
    },
    
    check_ktree = function(ktree, n_counterfactuals, rf) {
      checkmate::assert_integerish(ktree, len = 1L, lower = 1L, null.ok = FALSE)
      is_ktree_out_of_range = ktree > rf$ntree
      if (is_ktree_out_of_range) {
        stop("`ktree` cannot be larger then the total number of trees in the random forest.")
      }
      is_search_deterministic = (ktree == rf$ntree)
      if (is_search_deterministic & n_counterfactuals > 1) {
        warning_msg = paste(
          "The search for counterfactuals is deterministic, since `ktree` is equal to the total number of trees.",  # COMMENT see my comment elsewhere, don't break output text lines, people won't find the source location otherwise
          "`n_counterfactuals` was set to 1.", sep = "\n"
        )
        warning(warning_msg)
        private$set_n_counterfactuals_to_1 = TRUE
      }
    },
    
    check_training_data = function(data_obj) {
      categorical_train_variables = any("categorical" %in% data_obj$feature.types)
      if (categorical_train_variables) {
        stop("`FeatureTweaker` cannot handle categorical variables in the training data.")
      }
      
      all_features_standardized = private$check_standardization(data_obj$X)
      if (!all_features_standardized) {
        stop("`FeatureTweaker` can only handle standardized features in training data.")
      }
    },
    
    assign_init_params = function(arg_list) {
      predictor = arg_list$predictor
      predictor$task = predictor$model$type
      private$predictor = arg_list$predictor
      private$n_counterfactuals = ifelse(private$set_n_counterfactuals_to_1, 1L, arg_list$n_counterfactuals)
      private$epsilon = arg_list$epsilon
      private$ktree = arg_list$ktree
      private$param_set = private$make_param_set(arg_list$lower, arg_list$upper)
    }

  ),
  public = list(
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, desired_outcome = NULL, ktree = 30L,  
                          epsilon = 0.1, lower = NULL, upper = NULL) {
      arg_list = as.list(environment())
      private$run_init_arg_checks(arg_list)
      
      private$assign_init_params(arg_list)
      
      if (!is.null(x_interest)) {
        self$find_counterfactuals(x_interest, desired_outcome)
      }
      
    }
  )
)





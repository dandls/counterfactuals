CounterfactualsClassificationOnly <- R6Class("CounterfactualsClassificationOnly",
  inherit = Counterfactuals,
  
  private = list(
    get_desired_outcome_binary_class = function(y_hat_interest) {},
    
    check_that_classif_task = function(task) {
      if (task != "classification") {
        err_msg = sprintf("`%s` only works for classification tasks.", class(self)[1])
        stop(err_msg)
      }
    },
    
    one_hot_to_one_col = function(df) {
      as.data.table(colnames(df)[apply(df, 1, which.max)])
    },
    
    check_x_interest = function(x_interest) {
      # TODO: Check if desired_outcome is in predictor$data$y
      # TDOO: Arg checks
    },
    
    check_desired_outcome = function(desired_outcome) {
      if (is.null(desired_outcome)) {
        stop("The `desired_outcome` has to be specified for multiclass classification tasks.")
      }
    }
  ),
  
  public = list(
    find_counterfactuals = function(x_interest, desired_outcome = NULL) {
      
      private$check_x_interest(x_interest)

      x_interest = data.table::setDT(x_interest)
      y_hat_interest = private$predictor$predict(x_interest)
      is_binary_class = (ncol(y_hat_interest) <= 2)
      is_pred_one_hot = (ncol(y_hat_interest) > 1)
      prediction_colnames = names(y_hat_interest)
      
      if (is_pred_one_hot) {
        y_hat_interest = private$one_hot_to_one_col(y_hat_interest)
      }
      
      if (is_binary_class) {
        if (!is.null(desired_outcome)) {
          message(paste(
            "For binary classification tasks, `desired_outcome` is set to the opposite class of the prediction",
            "for `x_interest`."
          ))
        }
        desired_outcome = private$get_desired_outcome_binary_class(
          is_pred_one_hot, y_hat_interest, prediction_colnames
        )
      } 
      
      private$check_desired_outcome(desired_outcome)
      private$x_interest = x_interest
      private$is_pred_one_hot = is_pred_one_hot
      private$prediction_colnames = prediction_colnames
      private$desired_outcome = desired_outcome
      private$run()
    }
  )
)

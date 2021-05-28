CounterfactualsRegr = R6::R6Class("CounterfactualsRegr",
  inherit = Counterfactuals,
  
  public = list(
    
    initialize = function(predictor, lower, upper) {
      super$initialize(predictor, lower, upper)
      if (private$predictor$task != "regression") {
        err_msg = sprintf("This class only works for regression tasks.")
        stop(err_msg)
      }
    },
    
    find_counterfactuals = function(x_interest, desired_outcome = NULL) {
      
      # Checks x_interest
      assert_data_frame(x_interest, nrows = 1L)
      assert_names(names(x_interest), must.include = names(private$predictor$data$X))
      x_interest = setDT(x_interest)[, names(private$predictor$data$X), with = FALSE]
      if (any(sapply(x_interest, typeof) != sapply(private$predictor$data$X, typeof))) {
        stop("Columns that appear in `x_interest` and `predictor$data$X` must have the same types.")
      }
      feat_vals_outside_range = !ParamHelpers::isFeasible(private$param_set, as.list(x_interest))
      if (feat_vals_outside_range) {
        stop("Feature values of `x_interest` outside of range of `predictor$data$X` or given arguments `lower` or `upper`. Please modify arguments `lower` or `upper` accordingly.")
      }
      
      # Checks desired_outcome
      assert_numeric(desired_outcome, any.missing = FALSE, min.len = 1L,  max.len = 2L)
      if (length(desired_outcome) == 1L) {
        desired_outcome = c(desired_outcome, desired_outcome)
      }
      if (desired_outcome[2L] < desired_outcome[1L]) {
        stop("The lower bound of `desired_outcome` cannot be higher than the upper bound.")
      }
      
      private$x_interest = x_interest
      private$y_hat_interest = as.data.table(private$predictor$predict(x_interest))
      private$desired_outcome = desired_outcome
      private$run()
    }
  ),
  
  private = list(
    desired_outcome = NULL,
 
    get_pred_column = function() {
      names(private$y_hat_interest)[[1L]]
    }
  )
)

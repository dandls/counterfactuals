CounterfactualsClassif = R6::R6Class("CounterfactualsClassif",
  inherit = Counterfactuals,
  
  public = list(
    
    initialize = function(predictor, lower, upper) {
      super$initialize(predictor, lower, upper)
      if (private$predictor$task != "classification") {
        err_msg = sprintf("This class only works for classification tasks.")
        stop(err_msg)
      }
    },
    
    # For hard classification desired_prob can be set to 0 or 1, respectively.
    find_counterfactuals = function(x_interest, desired_class = NULL, desired_prob = c(0.5, 1)) {
 
      # Checks x_interest
      assert_data_frame(x_interest, nrows = 1L)
      assert_names(names(x_interest), subset.of = names(private$predictor$data$X))
      x_interest = setDT(x_interest)[, names(private$predictor$data$X), with = FALSE]
      if (any(sapply(x_interest, typeof) != sapply(private$predictor$data$X, typeof))) {
        stop("Columns that appear in `x_interest` and `predictor$data$X` must have the same types.")
      }
      feat_vals_outside_range = !ParamHelpers::isFeasible(private$param_set, as.list(x_interest))
      if (feat_vals_outside_range) {
        stop("Feature values of `x_interest` outside of range of `predictor$data$X` or given arguments `lower` or `upper`. Please modify arguments `lower` or `upper` accordingly.")
      }
      
      # Checks desired_prob
      assert_numeric(desired_prob, any.missing = FALSE, min.len = 1L,  max.len = 2L, lower = 0, upper = 1)
      if (length(desired_prob) == 1L) {
        desired_prob = c(desired_prob, desired_prob)
      }
      if (desired_prob[2L] < desired_prob[1L]) {
        stop("The lower bound of `desired_prob` cannot be higher than the upper bound.")
      }
      
      # Checks desired_class
      if (is.null(desired_class)) {
        if (is.null(private$predictor$class)) {
          stop("If `predictor$class` is `NULL`, then the `desired_class` must be specified.")
        }
        desired_class = private$predictor$class
        message(sprintf("The `desired_class` was set to `predictor$class` which is %s.", desired_class))
      }
      assert_character(desired_class, len = 1L, any.missing = FALSE)
      y_hat_interest = as.data.table(private$predictor$predict(x_interest))
      assert_choice(desired_class, names(y_hat_interest))
      
      
      private$y_hat_interest = y_hat_interest
      private$x_interest = x_interest
      private$desired_class = desired_class
      private$desired_prob = desired_prob
      private$run()
    }
  ),
  
  private = list(
    desired_prob = NULL,
    desired_class = NULL,

    get_pred_column = function() {
      private$desired_class
    }
  )
)

CounterfactualsClassification = R6::R6Class("CounterfactualsClassification",
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
      
      checkmate::assert_data_frame(x_interest, nrows = 1L)
      data = private$predictor$data$X
      if (any(names(x_interest) != names(data))) {
        rlang::abort(c(
          "`x_interest` is invalid.",
          x = "`x_interest` and `predictor$data$X` must have the same columns.",
          i = waldo::compare(names(x_interest), names(data), x_arg = "x_interest", y_arg = "predictor$data$X")
        ))
      }
      col_types_x_interest = sapply(x_interest, typeof)
      col_data = sapply(data, typeof)
      if (any(col_types_x_interest != col_data)) {
        rlang::abort(c(
          "`x_interest` is invalid.",
          x = "`x_interest` and `predictor$data$X` must have the same column types.",
          i = waldo::compare(col_types_x_interest, col_data, x_arg = "x_interest", y_arg = "predictor$data$X")
        ))
      }
      
      feat_vals_outside_range = !ParamHelpers::isFeasible(private$param_set, as.list(x_interest))
      if (feat_vals_outside_range) {
        rlang::abort(c(
          "`x_interest` is invalid.",
          x = "Feature values of `x_interest` outside of range of `predictor$data$X` or given arguments `lower` or `upper`.",
          i = "Please modify arguments `lower` or `upper` accordingly."
        ))
      }
      
      
      private$y_hat_interest = as.data.table(private$predictor$predict(x_interest))
      
      if (length(desired_prob) == 1) {
        desired_prob = c(desired_prob, desired_prob)
      }
      assert_numeric(desired_prob, any.missing = FALSE, len = 2L, lower = 0, upper = 1)
      if (desired_prob[2L] < desired_prob[1L]) {
        rlang::abort(c(
          "`desired_prob` is invalid.",
          x = "The lower bound of `desired_prob` cannot be higher than the upper bound."
        ))
      }
      
      if (is.null(desired_class)) {
        desired_class = private$predictor$class
        rlang::inform(sprintf("The `desired_class` was set to `predictor$class` which is %s.", desired_class))
      }
      if (is.null(desired_class)) {
        rlang::abort(c(
          "`desired_class` is invalid.",
          x = "The `desired_class` has to be specified."
        ))
      }
      checkmate::assert_character(desired_class, len = 1L)
      colnames_pred = names(private$y_hat_interest)
      if (!desired_class %in% colnames_pred) {
        rlang::abort(c(
          "`desired_class` is invalid.",
          x = "The `desired_class` needs to be a colname of the prediction.",
          i = sprintf("`desired_class` is: %s.", paste0("'", desired_class, "'")),
          i = sprintf("The colnames of the prediction are: %s.", paste0("'", colnames_pred, "'", collapse = ", "))
        ))
      }
      
      private$x_interest = data.table::setDT(x_interest)
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

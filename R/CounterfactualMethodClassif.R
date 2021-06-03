CounterfactualMethodClassif = R6::R6Class("CounterfactualMethodClassif", inherit = CounterfactualMethod,
  
  public = list(
    
    initialize = function(predictor, lower, upper) {
      super$initialize(predictor, lower, upper)
      if (private$predictor$task != "classification") {
        stop(sprintf("%s only works for classification tasks.", class(self)[1]))
      }
    },
    
    # For hard classification desired_prob can be set to 0 or 1, respectively.
    find_counterfactuals = function(x_interest, desired_class = NULL, desired_prob = c(0.5, 1)) {
      # Checks x_interest
      assert_data_frame(x_interest, nrows = 1L)
      assert_names(names(x_interest), must.include = names(private$predictor$data$X))
      x_interest = setDT(x_interest)[, names(private$predictor$data$X), with = FALSE]
      if (any(sapply(x_interest, typeof) != sapply(private$predictor$data$X, typeof))) {
        stop("Columns that appear in `x_interest` and `predictor$data$X` must have the same types.")
      }
      temp = copy(x_interest)
      factor_cols = names(temp)[sapply(temp, is.factor)]
      if (length(factor_cols) > 0) {
        temp[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
      }
      private$param_set$assert_dt(temp)
      
      # Checks desired_prob
      assert_numeric(desired_prob, any.missing = FALSE, min.len = 1L,  max.len = 2L, lower = 0, upper = 1)
      if (length(desired_prob) == 1L) {
        desired_prob = c(desired_prob, desired_prob)
      }
      if (desired_prob[2L] < desired_prob[1L]) {
        stop("The lower bound of `desired_prob` cannot be greater than the upper bound.")
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
      assert_choice(desired_class, choices = names(private$predictor$predict(x_interest)))
      
      private$x_interest = x_interest
      private$desired_class = desired_class
      private$desired_prob = desired_prob
      cfactuals = private$run()
      
      Counterfactuals$new(
        cfactuals = cfactuals, 
        prediction_function = private$predictor$prediction.function,
        x_interest = private$x_interest, 
        param_set = private$param_set,   
        desired = list("desired_class" = desired_class, "desired_prob" = desired_prob),
        task = "classification"
      )
    }
  ),
  
  private = list(
    desired_prob = NULL,
    desired_class = NULL,

    get_pred_column = function() {private$desired_class}
  )
)


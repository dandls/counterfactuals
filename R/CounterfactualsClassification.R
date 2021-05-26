CounterfactualsClassification = R6::R6Class("CounterfactualsClassification",
  inherit = Counterfactuals,
  
  private = list(
    desired_prob = NULL,
    desired_class = NULL,

    check_that_classif_task = function(task) {
      if (task != "classification") {
        err_msg = sprintf("This class only works for classification tasks.")
        stop(err_msg)
      }
    },
    
    check_desired_class = function(desired_class) {
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
      
    },
    check_desired_prob = function(desired_prob) {
      assert_numeric(desired_prob, any.missing = FALSE, len = 2L, lower = 0, upper = 1)
      if (desired_prob[2L] < desired_prob[1L]) {
        rlang::abort(c(
          "`desired_prob` is invalid.",
          x = "The lower bound of `desired_prob` cannot be higher than the upper bound."
        ))
      }
    },
    
    get_pred_column = function() {
      private$desired_class
    }
    
  ),
  
  public = list(
    
    initialize = function(arg_list) {
      super$initialize(arg_list)
      private$check_that_classif_task(private$predictor$task)
    },
    
    # For hard classification desired_prob can be set to 0 or 1, respectively.
    find_counterfactuals = function(x_interest, desired_class = NULL, desired_prob = c(0.5, 1)) {
      private$check_x_interest(x_interest)
      private$y_hat_interest = as.data.table(private$predictor$predict(x_interest))
      
      if (length(desired_prob) == 1) {
        desired_prob = c(desired_prob, desired_prob)
      }
      private$check_desired_prob(desired_prob)
      
      if (is.null(desired_class)) {
        desired_class = private$predictor$class
        rlang::inform(sprintf("The `desired_class` was set to `predictor$class` which is %s.", desired_class))
      }
      private$check_desired_class(desired_class)
      
      private$x_interest = data.table::setDT(x_interest)
      private$desired_class = desired_class
      private$desired_prob = desired_prob
      private$run()
    }
  )
)

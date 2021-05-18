CounterfactualsClassification = R6::R6Class("CounterfactualsClassification",
  inherit = Counterfactuals,
  
  private = list(
    desired_prob = NULL,
    desired_class = NULL,

    check_that_classif_task = function(task) {
      if (task != "classification") {
        err_msg = sprintf("`%s` only works for classification tasks.", class(self)[1L])
        stop(err_msg)
      }
    },
    
    check_x_interest = function(x_interest) {
      # TODO: Check if desired_outcome is in predictor$data$y
      # TDOO: Arg checks
    },
    
    check_desired_class = function(desired_class, x_interest) {
      if (is.null(desired_class)) {
        rlang::abort(c(
          "`desired_class` is invalid.",
          x = "The `desired_class` has to be specified."
        ))
      }
      # Makes only sense for multiclass
      checkmate::assert_character(desired_class, len = 1L)
      y_hat_interest = private$predictor$predict(x_interest)
      colnames_pred = names(y_hat_interest)
      if (!desired_class %in% colnames_pred) {
        rlang::abort(c(
          "`desired_class` is invalid.",
          x = "The `desired_class` needs to be a colname of the prediction.",
          i = sprintf("`desired_class` is: %s.", paste0("'", desired_class, "'")),
          i = sprintf("The colnames of the prediction are: %s", paste0("'", colnames_pred, "'", collapse = ", "))
        ))
      }
      
    },
    check_desired_prob = function(desired_prob) {
      # TODO
    }
  ),
  
  public = list(
    
    initialize = function(param_list) {
      # TODO: Init checks
      super$initialize(param_list)
      private$check_that_classif_task(private$predictor$task)
    },
    
    find_counterfactuals = function(x_interest, desired_class = NULL, desired_prob = c(0.5, 1)) {

      private$check_x_interest(x_interest)
      private$check_desired_prob(desired_prob)
      
      if (length(desired_prob) == 1) {
        desired_prob = c(desired_prob, desired_prob)
      }
 
      if (is.null(desired_class)) {
        rlang::inform("The `desired_class` was set to `predictor$class`.")
        desired_class = private$predictor$class
      }
      private$check_desired_class(desired_class, x_interest)
      
      private$x_interest = data.table::setDT(x_interest)
      private$desired_class = desired_class
      private$desired_prob = desired_prob
      private$run()
    }
  )
)

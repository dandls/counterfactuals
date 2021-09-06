#' WhatIf for Regression Tasks
#' 
#' @description 
#' 
#' WhatIf returns the `n_counterfactual` observations from `predictor$data$X` that are most similar to `x_interest` and whose
#' prediction is in `desired_outcome`.
#' 
#' @details
#' The dissimilarities are computed using Gower's dissimilarity measure (Gower, 1990) implemented by \link[gower]{gower_topn}. \cr
#' Only observations whose features values lie between the corresponding values in `lower` and `upper` are considered 
#' counterfactual candidates.
#' 
#' @references 
#' 
#' Gower, J. C. (1971), "A general coefficient of similarity and some of its properties". Biometrics, 27, 623–637.
#' 
#' Wexler, J., Pushkarna, M., Bolukbasi, T., Wattenberg, M., Viégas, F., & Wilson, J. (2019). The what-if tool: 
#' Interactive probing of machine learning models. IEEE transactions on visualization and computer graphics, 26(1), 56–65.
#' 
#' @examples 
#' if (require("randomForest")) {
#'   set.seed(123456)
#'   # Train a model
#'   rf = randomForest(mpg ~ ., data = mtcars)
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf)
#'   # Find counterfactuals
#'   wi_regr = WhatIfRegr$new(predictor, n_counterfactuals = 5L)
#'   cfactuals = wi_regr$find_counterfactuals(
#'     x_interest = mtcars[1L, ], desired_outcome = c(22, 26)
#'   )
#'   # Print the results
#'   cfactuals
#' }
#' 
#' @export
WhatIfRegr = R6::R6Class("WhatIfRegr", inherit = CounterfactualMethodRegr,
                         
  public = list(
    #' @description Create a new WhatIfRegr object.
    #' @template predictor
    #' @param n_counterfactuals (`integerish(1)`)\cr
    #'   The number of counterfactuals to be found. Default is `1L`.
    #' @template lower_upper
    initialize = function(predictor, n_counterfactuals = 1L, lower = NULL, upper = NULL) {
      super$initialize(predictor, lower, upper)
      assert_integerish(n_counterfactuals, lower = 1L, any.missing = FALSE, len = 1L)
      private$n_counterfactuals = n_counterfactuals
      X_search = private$predictor$data$X
      if (!is.null(lower)) {
        X_search = X_search[Reduce(`&`, Map(`>=`, X_search[, names(lower), with = FALSE], lower))]
      }
      if (!is.null(upper)) {
        X_search = X_search[Reduce(`&`, Map(`<=`, X_search[, names(upper), with = FALSE], upper))]
      }
      if (nrow(X_search) < n_counterfactuals) {
        warning(sprintf("Could only find %s candidate(s) with feature values between `lower` and `upper`.", nrow(X_search)))
      }
      private$X_search = X_search
    }
  ),
  
  private = list(
    n_counterfactuals = NULL,
    X_search = NULL,
    
    run = function() {
      pred_column = private$get_pred_column()
      whatif_algo(
        predictor = private$predictor, 
        n_cfactuals = private$n_counterfactuals, 
        x_interest = private$x_interest, 
        pred_column = pred_column, 
        desired_y_hat_range = private$desired_outcome,
        X_search = private$X_search
      )
    },
    
    print_parameters = function() {
      cat(" - n_counterfactuals: ", private$n_counterfactuals)
    }
  )
)

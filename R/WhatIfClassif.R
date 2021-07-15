#' WhatIf for Classification Tasks
#' 
#' @description 
#' 
#' `WhatIf` selects the `n_counterfactual` observations from the training data that are most similar to `x_interest`
#' and have the desired predicted probabilities for the desired class. As training data set `predictor$data$X` 
#' is taken.
#' 
#' 
#' 
#' 
#' @details
#' 
#' To measure dissimilarity the extension of the Gower's dissimilarity measure proposed by Kaufman and Rousseeuw (1990) 
#' and implemented by \link[StatMatch]{gower.dist} is used.
#' For numerical features the distances are scaled with the standard deviation of the specific feature values in the 
#' training data.
#' Only data points with features values between `lower` and `upper` are considered as candidates for the nearest data
#' observations.
#' 
#' @references 
#' 
#' Gower, J. C. (1971), "A general coefficient of similarity and some of its properties". Biometrics, 27, 623–637.
#' 
#' Kaufman, L. and Rousseeuw, P.J. (1990), Finding Groups in Data: An Introduction to Cluster Analysis. Wiley, New York.
#' 
#' Wexler, J., Pushkarna, M., Bolukbasi, T., Wattenberg, M., Viégas, F., & Wilson, J. (2019). The what-if tool: 
#' Interactive probing of machine learning models. IEEE transactions on visualization and computer graphics, 26(1), 56–65.
#' 
#' @examples 
#' if (require("randomForest")) {
#'   # Train a model
#'   rf = randomForest(Species ~ ., data = iris)
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf, type = "prob")
#'   # Find counterfactuals
#'   wi_classif = WhatIfClassif$new(predictor, n_counterfactuals = 5L)
#'   cfactuals = wi_classif$find_counterfactuals(
#'     x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
#'   )
#'   # Print the results
#'   cfactuals$data
#' }
#' 
#' @export
WhatIfClassif = R6::R6Class("WhatIfClassif", inherit = CounterfactualMethodClassif,
  
  public = list(
    #' @description Create a new WhatIfClassif object.
    #' @template predictor
    #' @param n_counterfactuals (`integerish(1)`)\cr
    #'   The number of counterfactuals to find. Default is `1L`.
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
        desired_y_hat_range = private$desired_prob,
        X_search = private$X_search
      )
    },
    
    print_parameters = function() {
      cat(" - n_counterfactuals: ", private$n_counterfactuals)
    }
  )
)

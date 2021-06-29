#' WhatIf for Regression Tasks
#' 
#' @description 
#' 
#' `WhatIf` selects the `n_counterfactual` observations from the training data that are most similar to `x_interest`
#' and have the desired predicted outcome. As training data set `predictor$data$X` is taken.
#' 
#' @details
#' 
#' To measure dissimilarity the extension of the Gower's dissimilarity measure proposed by Kaufman and Rousseeuw (1990) 
#' and implemented by \link[StatMatch]{gower.dist} is used.
#' For numerical features the distances are scaled with the standard deviation of the specific feature values in the 
#' training data.
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
    #'   The number of counterfactuals to find. Default is `1L`.
    #' @template lower_upper
    initialize = function(predictor, n_counterfactuals = 1L, lower = NULL, upper = NULL) {
      super$initialize(predictor, lower, upper)
      assert_integerish(n_counterfactuals, lower = 1L, any.missing = FALSE, len = 1L)
      private$n_counterfactuals = n_counterfactuals
    }
  ),
  
  private = list(
    n_counterfactuals = NULL,
    
    run = function() {
      pred_column = private$get_pred_column()
      whatif_algo(
        predictor = private$predictor, 
        n_cfactuals = private$n_counterfactuals, 
        x_interest = private$x_interest, 
        pred_column = pred_column, 
        desired_y_hat_range = private$desired_outcome
      )
    },
    
    print_parameters = function() {
      cat(" - n_counterfactuals: ", private$n_counterfactuals)
    }
  )
)

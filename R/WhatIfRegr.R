#' WhatIf for Regression Tasks
#' 
#' @description 
#' 
#' WhatIf returns the `n_counterfactual` most similar observations to `x_interest` from observations in `predictor$data$X` 
#' whose prediction is in the `desired_outcome` interval.
#' 
#' 
#' @details
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
#'   # Find counterfactuals for x_interest
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
    #'   The number of counterfactuals to return Default is `1L`.
    #' @template lower_upper
    #' @param distance_function (`function()` | `'gower'` | `'gower_c'`)\cr 
    #'  The distance function used to compute the distances between `x_interest`
    #'  and the training data points for finding `x_nn`.
    #'  Either the name of an already implemented distance function
    #'  ('gower' or 'gower_c') or a function.
    #'  If set to 'gower' (default), then Gower's distance (Gower 1971) is used;
    #'  if set to 'gower_c', a C-based more efficient version of Gower's distance is used.
    #'  A function must have three arguments  `x`, `y`, and `data` and should
    #'  return a `double` matrix with `nrow(x)` rows and maximum `nrow(y)` columns.
    initialize = function(predictor, n_counterfactuals = 1L, lower = NULL, upper = NULL, distance_function = "gower") {
      
      if (is.character(distance_function)) {
        if (distance_function == "gower") {
          distance_function = gower_dist
        } else if (distance_function == "gower_c") {
          if (!requireNamespace("gower", quietly = TRUE)) {
            stop("Package 'gower' needed for distance_function = 'gower_c'. Please install it.", call. = FALSE)
          }
          distance_function = function(x, y, data) {
            gower_dist_c(x, y, data, k = n_counterfactuals, idx = TRUE)
          }
          class(distance_function) = class(gower_dist_c)
        }
      }
      
      super$initialize(predictor, lower, upper, distance_function)

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
        X_search = private$X_search,
        distance_function = private$distance_function
      )
    },
    
    print_parameters = function() {
      cat(" - n_counterfactuals: ", private$n_counterfactuals)
    }
  )
)

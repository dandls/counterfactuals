#' NICE (Nearest Instance Counterfactual Explanations) for Regression Tasks
#'
#' @description NICE (Brughmans and Martens 2021) searches for counterfactuals by iteratively replacing feature values
#' of `x_interest` with the corresponding value of its most similar (optionally correctly predicted) instance `x_nn`.
#' While the original method is only applicable to classification tasks (see \link{NICEClassif}), this implementation extend it to regression tasks.
#'
#' @details
#' NICE starts the counterfactual search for `x_interest` by finding its most similar (optionally) correctly predicted
#' neighbor `x_nn` with(in) the desired prediction (range). Correctly predicted means that the prediction of `x_nn` is less 
#' than a user-specified `margin_correct` away from the true outcome of `x_nn`.
#' This is designed to mimic the search for `x_nn` for regression tasks.
#' If no `x_nn` satisfies this constraint, a warning is returned that no counterfactual could be found. 
#' \cr
#' In the first iteration, NICE creates new instances by replacing a different feature value of `x_interest` with the corresponding
#' value of `x_nn` in each new instance. Thus, if `x_nn` differs from `x_interest` in `d` features, `d` new instances are created. \cr
#' Then, the reward values for the created instances are computed with the chosen reward function.
#' Available reward functions are `sparsity`, `proximity`, and `plausibility`. \cr
#' In the second iteration, NICE creates `d-1` new instances by replacing a different feature value of the highest
#' reward instance of the previous iteration with the corresponding value of `x_interest`, and so on. \cr
#' If `finish_early = TRUE`, the algorithm terminates when the predicted outcome for
#' the highest reward instance is in the interval `desired_outcome`; if `finish_early = FALSE`, the
#' algorithm continues until `x_nn` is recreated. \cr
#' Once the algorithm terminated, it depends on `return_multiple` which instances
#' are returned as counterfactuals: if `return_multiple = FALSE`, then only the highest reward instance in the
#' last iteration is returned as counterfactual; if `return_multiple = TRUE`, then all instances (of all iterations)
#' whose predicted outcome is in the interval `desired_outcome` are returned as counterfactuals.
#'
#' If `finish_early = FALSE` and `return_multiple = FALSE`, then `x_nn` is returned as single counterfactual.
#'
#' The function computes the dissimilarities using Gower's dissimilarity measure (Gower 1971).
#'
#'
#' @references
#'
#' Brughmans, D., & Martens, D. (2021). NICE: An Algorithm for Nearest Instance Counterfactual Explanations.
#' \href{https://arxiv.org/abs/2104.07411}{arXiv 2104.07411} v2.
#'
#' Gower, J. C. (1971), "A general coefficient of similarity and some of its properties". Biometrics, 27, 623–637.
#'
#'
#' @examples
#' if (require("randomForest")) {
#'   set.seed(123456)
#'   # Train a model
#'   rf = randomForest(mpg ~ ., data = mtcars)
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf)
#'   # Find counterfactuals
#'   nice_regr = NICERegr$new(predictor)
#'   cfactuals = nice_regr$find_counterfactuals(
#'      x_interest = mtcars[1L, ], desired_outcome = c(22, 26)
#'   )
#'   # Print the results
#'   cfactuals$data
#'   # Print archive
#'   nice_regr$archive
#' }
#'
#' @export
NICERegr = R6::R6Class("NICERegr",
  inherit = CounterfactualMethodRegr,
  public = list(
    
    #' @description Create a new NICERegr object.
    #' @template predictor
    #' @param optimization (`character(1)`)\cr
    #' The reward function to optimize. Can be `sparsity` (default), `proximity` or `plausibility`.
    #' @param x_nn_correct (`logical(1)`)\cr
    #' Should only *correctly* classified data points in `predictor$data$X` be considered for the most similar instance search?
    #' Default is `TRUE`.
    #' @param return_multiple (`logical(1)`)\cr
    #' Should multiple counterfactuals be returned? If TRUE, the algorithm returns all created instances whose
    #' prediction is in the interval `desired_outcome`. For more information, see the `Details` section.
    #' @param finish_early (`logical(1)`)\cr
    #' Should the algorithm terminate after an iteration in which the prediction for the highest reward instance
    #' is in the interval `desired_outcome`. If `FALSE`, the algorithm continues until `x_nn` is recreated.
    #' @param margin_correct (`numeric(1)` | `NULL`)\cr
    #' The accepted margin for considering a prediction as "correct". 
    #' Ignored if `x_nn_correct = FALSE`.
    #' If NULL, the accepted margin is set to half the median absolute distance between the true and predicted outcomes in the data (`predictor$data`).
    #' @param distance_function (`function()` | `'gower'` | `'gower_c'`)\cr 
    #'  The distance function used to compute the distances between `x_interest`
    #'  and the training data points for finding `x_nn`. If `optimization` is set
    #'  to `proximity`, the distance function is also used for calculating the
    #'  distance between candidates and `x_interest`.
    #'  Either the name of an already implemented distance function
    #'  ('gower' or 'gower_c') or a function is allowed as input.
    #'  If set to 'gower' (default), then Gower's distance (Gower 1971) is used;
    #'  if set to 'gower_c', a C-based more efficient version of Gower's distance is used.
    #'  A function must have three arguments  `x`, `y`, and `data` and should
    #'  return a `double` matrix with `nrow(x)` rows and maximum `nrow(y)` columns.
    initialize = function(predictor, optimization = "sparsity", x_nn_correct = TRUE, margin_correct = NULL, 
      return_multiple = FALSE, finish_early = TRUE, distance_function = "gower") {
      
      if (is.character(distance_function)) {
        if (distance_function == "gower") {
          distance_function = gower_dist
        } else if (distance_function == "gower_c") {
          if (!requireNamespace("gower", quietly = TRUE)) {
            stop("Package 'gower' needed for distance_function = 'gower_c'. Please install it.", call. = FALSE)
          }
          distance_function = function(x, y, data) {
            gower_dist_c(x, y, data, k = 1L, idx = TRUE)
          }
          class(distance_function) = class(gower_dist_c)
        }
      }
      super$initialize(predictor, distance_function = distance_function)
      assert_choice(optimization, choices = c("sparsity", "proximity", "plausibility"))
      assert_flag(x_nn_correct)
      assert_flag(return_multiple)
      assert_flag(finish_early)
      assert_numeric(margin_correct, len = 1L, any.missing = FALSE, null.ok = TRUE)
      
      private$optimization = optimization
      private$x_nn_correct = x_nn_correct
      private$return_multiple = return_multiple
      private$finish_early = finish_early
      private$y_hat = private$predictor$predict(predictor$data$X)
      
      if (private$optimization == "plausibility") {
        if (!requireNamespace("keras", quietly = TRUE)) {
          stop("Package 'keras' needed for this function to work. Please install it.", call. = FALSE)
        }
        private$ae_preprocessor = AEPreprocessor$new(private$predictor$data$X)
        private$ae_model = train_AE_model(private$predictor$data$X, private$ae_preprocessor)
      }
      
      private$is_correctly_classified = seq_len(nrow(private$predictor$data$X))
      if (x_nn_correct) {
        if (is.null(private$predictor$data$y)) {
          stop("true outcome variable `predictor$data$y` is not available, update `predictor$data` or consider setting `x_nn_correct = FALSE`.")
        } else {
          if (is.null(margin_correct)) {
            all_residuals = abs(private$y_hat[[1L]] - private$predictor$data$y[[1L]])
            margin_correct = median(all_residuals) / 2
          }
          private$is_correctly_classified = abs(private$y_hat[[1L]] - private$predictor$data$y[[1L]]) < margin_correct
          if (!any(private$is_correctly_classified)) {
            stop("no correctly predicted instance exist, inspect `predictor$model` and consider setting `x_nn_correct = FALSE` or updating `margin_correct`.")
          }
        }
      }
      private$candidates_x_nn = private$predictor$data$X[private$is_correctly_classified]
    }
  ),
  active = list(
    #' @field x_nn (`logical(1)`) \cr
    #'  The most similar (optionally) correctly classified instance of `x_interest`.
    x_nn = function(value) {
      if (missing(value)) {
        private$.x_nn
      } else {
        stop("`$x_nn` is read only", call. = FALSE)
      }
    },
    
    #' @field archive (`list()`) \cr
    #' A list that stores the history of the algorithm run. For each algorithm iteration, it has one element containing
    #' a `data.table`, which stores all created instances of this iteration together with their
    #' reward values and their predictions.
    archive = function(value) {
      if (missing(value)) {
        private$.archive
      } else {
        stop("`$archive` is read only", call. = FALSE)
      }
    }
  ),
  private = list(
    optimization = NULL,
    X_train_class = NULL,
    ae_model = NULL,
    ae_preprocessor = NULL,
    y_hat = NULL,
    x_nn_correct = NULL,
    return_multiple = NULL,
    finish_early = NULL,
    is_correctly_classified = NULL,
    candidates_x_nn = NULL,
    .x_nn = NULL,
    .archive = NULL,
    run = function() {
      # Flush
      private$.archive = NULL
      
      pred_column = private$get_pred_column()
      
      res = nice_algo(
        predictor = private$predictor,
        return_multiple = private$return_multiple,
        finish_early = private$finish_early,
        optimization = private$optimization,
        x_interest = private$x_interest,
        pred_column = pred_column,
        desired_y_hat_range = private$desired_outcome,
        candidates_x_nn = private$candidates_x_nn,
        ae_model = private$ae_model,
        ae_preprocessor = private$ae_preprocessor,
        archive = private$.archive,
        distance_function = private$distance_function
      )
      
      private$.x_nn = res$x_nn
      private$.archive = res$archive
      res$counterfactuals
    },
    print_parameters = function() {
      cat(" - finish_early: ", private$finish_early, "\n")
      cat(" - optimization: ", private$optimization, "\n")
      cat(" - return_multiple: ", private$return_multiple, "\n")
      cat(" - x_nn_correct: ", private$x_nn_correct, "\n")
    }
  )
)

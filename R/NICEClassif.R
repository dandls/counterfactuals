#' NICE (Nearest Instance Counterfactual Explanations) for Classification Task
#' 
#' @description NICE (Brughmans and Martens, 2021) starts the counterfactual search for `x_interest` by finding its nearest
#' (optionally) correctly classified neighbor `x_nn`. The \link[UBL]{distances} function that implements the 
#' Heterogeneous Euclidean Overlap Method (HEOM) (Wilson and Martinez, 1997) is used to compute the distances. \cr 
#' Once `x_nn` was found, NICE iteratively replaces feature values of `x_interest` with the corresponding values of `x_nn` to optimize
#' some predefined reward function. Available reward functions are `sparsity` `proximity` and `plausibility`. \cr 
#' Our NICE implementation is equivalent to the original version if `return_multiple = FALSE`, `finish_early = TRUE` and
#' `x_nn_correct_classif = TRUE`.
#' 
#' @details 
#' In the first iteration, NICE creates new instances by replacing one feature of `x_interest` with the corresponding 
#' values of `x_nn` in each instance. Thus, if `x_nn` differs from `x_interest` in `d` features, `d` instances are created. \cr 
#' Then, the rewards of each instances are calculated with the chosen reward function. \cr 
#' In the second iteration, new instances are created by replacing one feature of the instance with the highest reward 
#' in the previous iteration. \cr 
#' If `finish_early = TRUE`, the algorithm terminates when the predicted probability for
#' the `desired_class` of the instance with the highest reward, is in `desired_prob`. If `finish_early = FALSE`, the algorithm
#' continues until `x_nn` is recreated. \cr 
#' Once the algorithm terminated, it depends on `return_multiple` which instances
#' are returned as counterfactuals. If `return_multiple = FALSE`, then only the instance with the highest reward in the
#' last iteration is returned as counterfactual. If `return_multiple = TRUE`, then all instances of all iterations
#' whose predicted probability for the `desired_class` is in `desired_prob` are returned as counterfactuals. \cr
#' Note that if `finish_early = FALSE` and `return_multiple = FALSE`, then `x_nn` is returned as single counterfactual.
#' 
#' 
#' 
#' @references 
#' 
#' Brughmans, D., & Martens, D. (2021). NICE: An Algorithm for Nearest Instance Counterfactual Explanations. 
#' arXiv preprint arXiv:2104.07411.
#' 
#' Wilson, D Randall, and Tony R Martinez. 1997. “Improved Heterogeneous Distance Functions.” Journal of Artificial 
#' Intelligence Research 6: 1–34.
#' 
#' 
#' @examples 
#' if (require("randomForest")) {
#'   # Train a model
#'   rf = randomForest(Species ~ ., data = iris)
#'   # Create a predictor object
#'   predictor = iml::Predictor$new(rf, type = "prob")
#'   # Find counterfactuals
#'   nice_classif = NICEClassif$new(predictor)
#'   cfactuals = nice_classif$find_counterfactuals(
#'     x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
#'   )
#'   # Print the results
#'   cfactuals$data
#'   # Print archive
#'   nice_classif$archive
#' }
#' 
#' @export
NICEClassif = R6::R6Class("NICEClassif", inherit = CounterfactualMethodClassif,

  public = list(
    
    #' @description Create a new NICEClassif object.
    #' @template predictor
    #' @param optimization (`character(1)`)\cr 
    #' The optimization strategy that determines the reward function. Can be `sparsity` (default), `proximity` or 
    #' `plausibility`.
    #' @param x_nn_correct_classif (`logical(1)`)\cr 
    #' Should only correctly classified observations be considered for the nearest neighbor search?
    #' Default is `TRUE`.
    #' @param return_multiple (`logical(1)`)\cr 
    #' If set to `TRUE` (default) all created feature combinations with the desired prediction are returned.
    #' If set to `FALSE` only the feature combination with the desired prediction that has the highest reward in the last 
    #' iteration is returned. For more information, see the `details` section. (TODO)
    #' @param finish_early (`logical(1)`)\cr 
    #' Should the algorithm finish after an iteration in which the feature combinations with the highest reward
    #' has the desired prediction? Default is `TRUE`.
    initialize = function(predictor, optimization = "sparsity", x_nn_correct_classif = TRUE, return_multiple = TRUE,
                          finish_early = TRUE) {
      
      super$initialize(predictor)
      assert_choice(optimization, choices = c("sparsity", "proximity", "plausibility"))
      assert_flag(x_nn_correct_classif)
      assert_flag(return_multiple)
      assert_flag(finish_early)
      
      private$optimization = optimization
      private$x_nn_correct_classif = x_nn_correct_classif
      private$return_multiple = return_multiple
      private$finish_early = finish_early
      private$y_hat = private$predictor$predict(predictor$data$X)

      if (private$optimization == "plausibility") {
        if (!requireNamespace("keras", quietly = TRUE)) {
          stop("Package 'keras' needed for this function to work. Please install it.", call. = FALSE)
        }
        private$aep = AEPreprocessor$new(private$predictor$data$X)
        private$ae_model = train_AE_model(private$predictor$data$X, private$aep)
      }
      
      private$is_correctly_classified = seq_len(nrow(private$predictor$data$X))
      if (x_nn_correct_classif) {
        pred_classes = names(private$y_hat)[max.col(private$y_hat, ties.method = "random")] 
        private$is_correctly_classified = (private$predictor$data$y[[1L]] == pred_classes)
      }
      private$candidates_x_nn = private$predictor$data$X[private$is_correctly_classified]
   
    }

  ),
  
  active = list(
    #' @field x_nn (`logical(1)`) \cr
    #'  The nearest neighbor. 
    x_nn = function(value) {
      if (missing(value)) {
        private$.x_nn
      } else {
        stop("`$x_nn` is read only", call. = FALSE)
      }
    },
    
    #' @field archive (`list()`) \cr
    #' A list that stores the history of the algorithm. For each algorithm iteration it has one element.
    #' A element contains a `data.table` that stores all feature combinations in this iterations together with their
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
    aep = NULL,
    y_hat = NULL,
    x_nn_correct_classif = NULL,
    return_multiple = NULL,
    finish_early = NULL,
    is_correctly_classified = NULL,
    candidates_x_nn = NULL,
    .x_nn = NULL,
    .archive = NULL,

    run = function() {
      # Flush
      private$.archive = NULL
      
      predictor = private$predictor
      desired_class = private$desired_class
      desired_prob = private$desired_prob
      candidates_x_nn = private$candidates_x_nn
      
      in_desired_prob = between(
        private$y_hat[[desired_class]][private$is_correctly_classified], desired_prob[1L], desired_prob[2L]
      )
      candidates_x_nn = candidates_x_nn[in_desired_prob]
      
      if (nrow(candidates_x_nn) == 0L) {
        warning("No counterfactuals could be found.")
        return(predictor$data$X[0L])
      }
      
      private$.x_nn = candidates_x_nn[gower_topn(private$x_interest, candidates_x_nn, n = 1L)$index]
      x_current = copy(private$x_interest)
      
      finished = FALSE
      while (!finished) {

        diff = which(x_current != private$.x_nn)
        names_diff = names(private$.x_nn)[diff]
        X_candidates = x_current[rep(seq_len(nrow(x_current)), length(diff))]
        for (i in seq(length(diff))) {
          set(X_candidates, i, names_diff[i], value = private$.x_nn[, names_diff[i], with = FALSE])
        }
        
        f_x_current = predictor$predict(x_current)[desired_class][[1L]]
        f_X_candidates = predictor$predict(X_candidates)[desired_class][[1L]]
        
        dist_to_interval = function(x, interval) {
          sapply(x, function(z) {
            ifelse(between(z, interval[1L], interval[2L]), 0, min(abs(z - interval)))
          })
        }
        d_f_x_current = dist_to_interval(f_x_current, desired_prob)
        d_f_X_candidates = dist_to_interval(f_X_candidates, desired_prob)
        if (private$optimization == "sparsity") {
          reward = d_f_x_current - d_f_X_candidates
        } else if (private$optimization == "proximity") {
          d_X_candidates = gower_dist(X_candidates, private$x_interest)
          d_x_current = gower_dist(x_current, private$x_interest)
          reward = (d_f_x_current - d_f_X_candidates) / (d_X_candidates - d_x_current + sqrt(.Machine$double.eps))
        } else {
          
          X_candidates_pp = private$aep$preprocess(X_candidates)
          x_current_pp = private$aep$preprocess(x_current)
          
          ae_pred_X_candidates = private$ae_model$predict(as.matrix(X_candidates_pp))
          AE_loss_X_candidates = rowMeans((X_candidates_pp - ae_pred_X_candidates)^2)
          
          ae_pred_x = private$ae_model$predict(as.matrix(x_current_pp))
          AE_loss_x = rowMeans((x_current_pp - ae_pred_x)^2)
          reward = (d_f_x_current - d_f_X_candidates) * (AE_loss_x - AE_loss_X_candidates)
        }
        
        x_current = X_candidates[which.max(reward)]
        private$.archive = c(
          private$.archive,
          list(cbind(X_candidates, "reward" = as.numeric(reward), predictor$predict(X_candidates)))
        )
        
        finished = identical(x_current, private$.x_nn) | 
          private$finish_early & between(predictor$predict(x_current)[[desired_class]], desired_prob[1L], desired_prob[2L])
        
      }
      
      if (private$return_multiple) {
        # Run backwards through archive and look for candidates that fulfill desired properties
        cfs = lapply(private$.archive, function(iter) {
          iter[between(iter[[desired_class]], desired_prob[1L], desired_prob[2L]), names(X_candidates), with = FALSE]
        })
        cfs = unique(rbindlist(cfs))
      } else {
        cfs = x_current
      }
      
      cfs
    
    },

    print_parameters = function() {
      cat(" - finish_early: ", private$finish_early, "\n")
      cat(" - optimization: ", private$optimization, "\n")
      cat(" - return_multiple: ", private$return_multiple, "\n")
      cat(" - x_nn_correct_classif: ", private$x_nn_correct_classif, "\n")
    }
  )
)

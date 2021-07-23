#' NICE (Nearest Instance Counterfactual Explanations) for Classification Task
#' 
#' @description NICE (nsdfndf) by .. searches for counterfactuals by itaretively replacing feature values of `x_interest` with its
#' nearest (optionnaly correclty classified) neighbor. The features are replaced according to optimize "sparsity", "proximity" 
#' or "plausibility".
#' 
#' Equivalent to the algorithm in the paper is x_nn_correct_classif = TRUE, return_multiple = FALSE, finish_early = TRUE
#' 
#' @references 
#' 
#' Brughmans, D., & Martens, D. (2021). NICE: An Algorithm for Nearest Instance Counterfactual Explanations. 
#' arXiv preprint arXiv:2104.07411.
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
      
      if (!requireNamespace("UBL", quietly = TRUE)) {
        stop("Package 'UBL' needed for this function to work. Please install it.", call. = FALSE)
      }
      if (private$optimization == "plausibility") {
        if (!requireNamespace("keras", quietly = TRUE)) {
          stop("Package 'keras' needed for this function to work. Please install it.", call. = FALSE)
        }
        private$aep = AEPreprocessor$new(predictor$data$X)
        private$ae_model = train_AE_model(predictor$data$X, private$aep)
      }
      
      private$is_correctly_classified = seq_len(nrow(predictor$data$X))
      if (x_nn_correct_classif) {
        pred_classes = names(private$y_hat)[max.col(private$y_hat, ties.method = "random")] 
        private$is_correctly_classified = (predictor$data$y[[1L]] == pred_classes)
      }
      private$candidates_x_nn = predictor$data$X[private$is_correctly_classified]
   
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
      
      candidates_x_nn_list = split(candidates_x_nn, seq(nrow(candidates_x_nn)))
      
      distance = future.apply::future_vapply(
        candidates_x_nn_list, function(x) {
          df = data.frame(rbind(private$x_interest, x))
          df$class = "1"
          UBL::distances(3L, df, dist = "HEOM", p = 1L)[1L, 2L]

        },
        FUN.VALUE = numeric(1L),  USE.NAMES = FALSE
      )

      private$.x_nn = candidates_x_nn[which.min(distance)]
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
        
        if (private$optimization == "sparsity") {
          reward = abs(f_x_current - f_X_candidates)
          
        } else if (private$optimization == "proximity") {
          d_X_candidates = StatMatch::gower.dist(X_candidates, private$x_interest)
          d_x_current = as.vector(StatMatch::gower.dist(x_current, private$x_interest))
          reward = abs((f_x_current - f_X_candidates) / (d_X_candidates - d_x_current + sqrt(.Machine$double.eps)))
          
        } else {
          
          X_candidates_pp = private$aep$preprocess(X_candidates)
          x_current_pp = private$aep$preprocess(x_current)
          
          ae_pred_X_candidates = private$ae_model$predict(as.matrix(X_candidates_pp))
          AE_loss_X_candidates = rowMeans((X_candidates_pp - ae_pred_X_candidates)^2)
          
          ae_pred_x = private$ae_model$predict(as.matrix(x_current_pp))
          AE_loss_x = rowMeans((x_current_pp - ae_pred_x)^2)
          reward = abs((f_x_current - f_X_candidates) * (AE_loss_x - AE_loss_X_candidates))
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

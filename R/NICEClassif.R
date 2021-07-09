#' NICE for Classification Task
#' @export
NICEClassif = R6::R6Class("NICEClassif",
  inherit = CounterfactualMethodClassif,

  public = list(
    
    x_nn = NULL,
    archive = list(),
    
    # TODO:
    # x_nn_correct_classif: Only correctly classified observations are considered as nearest neighbor
    # return_multiple: FALSE then Equivalent to original algo (if finish_early TRUE), returns the last counterfactual found
    #                  Should not be set to FALSE if finish_early = FALSE as x_nn is returned
    # finish_early: Equivalent to paper (if return_multiple FALSE)
    initialize = function(predictor, optimization = "sparsity", x_nn_correct_classif = TRUE, return_multiple = TRUE,
                          finish_early = TRUE, lower = NULL, upper = NULL) {
      
      super$initialize(predictor, lower, upper)
      
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

    run = function() {
      # Flush
      self$archive = NULL
      
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
      
      ranges = private$param_set$upper - private$param_set$lower
      candidates_x_nn_list = split(candidates_x_nn, seq(nrow(candidates_x_nn)))
      distance = future.apply::future_vapply(
        candidates_x_nn_list, StatMatch::gower.dist, FUN.VALUE = numeric(1L), private$x_interest, ranges, 
        USE.NAMES = FALSE
      )

      self$x_nn = candidates_x_nn[which.min(distance)]
      x_current = copy(private$x_interest)
      
      finished = FALSE
      while (!finished) {

        diff = which(x_current != self$x_nn)
        names_diff = names(self$x_nn)[diff]
        X_candidates = x_current[rep(seq_len(nrow(x_current)), length(diff))]
        for (i in seq(length(diff))) {
          set(X_candidates, i, names_diff[i], value = self$x_nn[, names_diff[i], with = FALSE])
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
        self$archive = c(
          self$archive,
          list(cbind(X_candidates, "reward" = as.numeric(reward), predictor$predict(X_candidates)))
        )
        
        finished = identical(x_current, self$x_nn) | 
          private$finish_early & between(predictor$predict(x_current)[[desired_class]], desired_prob[1L], desired_prob[2L])
        
      }
      
      if (private$return_multiple) {
        # Run backwards through archive and look for candidates that fulfill desired properties
        cfs = lapply(self$archive, function(iter) {
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

#' NICE for Classification Task
#' @export
NICEClassif = R6::R6Class("NICEClassif",
  inherit = CounterfactualMethodClassif,

  public = list(
    
    archive = list(),
    
    # correct_classif_only: Only correctly classified observations are considered as nearest neighbor
    initialize = function(predictor, optimization = "sparsity", threshold = NULL, correct_classif_only = TRUE, 
                          lower = NULL, upper = NULL) {
      
      super$initialize(predictor, lower, upper)

      assert_flag(correct_classif_only)
      assert_choice(optimization, choices = c("sparsity", "proximity", "plausibility"))
      private$optimization = optimization
      private$correct_classif_only = correct_classif_only
      private$y_hat = private$predictor$predict(predictor$data$X)
      assert_numeric(threshold, lower = 0L, upper = 1L, any.missing = FALSE, len = ncol(private$y_hat), null.ok = TRUE)
      if (is.null(threshold)) {
        k = ncol(private$y_hat)
        private$threshold = rep(1 / k, k)
        names(private$threshold) = names(private$y_hat)
      } else {
        assert_names(names(threshold), must.include = names(private$y_hat))
        private$threshold = threshold[names(private$y_hat)]
      }
      
      if (private$optimization == "plausibility") {
        if (!requireNamespace("keras", quietly = TRUE)) {
          stop("Package 'keras' needed for this function to work. Please install it.", call. = FALSE)
        }
        private$aep = AEPreprocessor$new(predictor$data$X)
        private$ae_model = train_AE_model(predictor$data$X, private$aep)
      }
      
      if (correct_classif_only) {
        pred_class = private$y_hat > t(private$threshold)
        private$X_train_class = apply(pred_class, 1L, function(x) ifelse(any(x), names(private$y_hat)[which.max(x)], NA))
      } else {
        if (!is.null(threshold)) {
          message("`threshold` is ignored, when `correct_classif_only` is set to FALSE.")
        }
      }
   
    }

  ),

  private = list(
    optimization = NULL,
    X_train_class = NULL,
    desired_class = NULL,
    ae_model = NULL,
    aep = NULL,
    y_hat = NULL,
    threshold = NULL,
    correct_classif_only = NULL,

    run = function() {
      # Flush
      self$archive = NULL
      
      predictor = private$predictor
      desired_class = private$desired_class
      desired_prob = private$desired_prob
      candidates_x_nn = predictor$data$X 
      
      is_correctly_classified = rep(TRUE, nrow(candidates_x_nn))
      if (private$correct_classif_only) {
        # Converts multi-class to binary-class problem
        is_correctly_classified = ifelse(
          predictor$data$y[[1L]] == desired_class, 
          private$X_train_class == desired_class, 
          private$X_train_class != desired_class
        )

        is_correctly_classified[is.na(is_correctly_classified)] = FALSE
        candidates_x_nn = candidates_x_nn[is_correctly_classified]
        
        if (nrow(candidates_x_nn) == 0L) {
          warning("No counterfactuals could be found.")
          return(predictor$data$X[0L])
        }
      }
      
      in_desired_prob = between(
        private$y_hat[[desired_class]][is_correctly_classified], desired_prob[1L], desired_prob[2L]
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

      x_nn = candidates_x_nn[which.min(distance)]
      x_current = copy(private$x_interest)
      
      while (TRUE) {
        x_prev = x_current
        
        diff = which(x_prev != x_nn)
        if (length(diff) == 0) {
          message("No counterfactuals could be found.")
          return(predictor$data$X[0L])
        }
        
        X_candidates = x_prev[rep(seq_len(nrow(x_prev)), length(diff))]
        for (j in seq(length(diff))) {
          col_name = names(x_nn)[diff[j]]
          repl_val = x_nn[, col_name, with = FALSE]
          X_candidates[j, (col_name) := repl_val]
        }
        
        f_x_prev = predictor$predict(x_prev)[desired_class][[1L]]
        f_X_candidates = predictor$predict(X_candidates)[desired_class][[1L]]
        if (private$optimization == "sparsity") {
          reward = abs(f_x_prev - f_X_candidates)
          
        } else if (private$optimization == "proximity") {
          d_X_candidates = StatMatch::gower.dist(X_candidates, private$x_interest)
          d_x_prev = as.vector(StatMatch::gower.dist(x_prev, private$x_interest))
          reward = abs((f_x_prev - f_X_candidates) / (d_X_candidates - d_x_prev + sqrt(.Machine$double.eps)))
          
        } else {
          
          X_candidates_pp = private$aep$preprocess(X_candidates)
          x_prev_pp = private$aep$preprocess(x_prev)
          
          ae_pred_X_candidates = private$ae_model$predict(as.matrix(X_candidates_pp))
          AE_loss_X_candidates = rowMeans((X_candidates_pp - ae_pred_X_candidates)^2)
          
          ae_pred_x = private$ae_model$predict(as.matrix(x_prev_pp))
          AE_loss_x = rowMeans((x_prev_pp - ae_pred_x)^2)
          reward = abs((f_x_prev - f_X_candidates) * (AE_loss_x - AE_loss_X_candidates))
        }
        
        x_current = X_candidates[which.max(reward)]
        self$archive = c(
          self$archive,
          list(list(
            "X_candidates" = X_candidates, 
            "reward" = as.numeric(reward), 
            "pred" = predictor$predict(X_candidates)
          ))
        )
        
        if (between(predictor$predict(x_current)[[desired_class]], desired_prob[1L], desired_prob[2L])) {
          counterfactuals = lapply(
            self$archive, function(el) {
              el$X_candidates[between(el$pred[[desired_class]], desired_prob[1L], desired_prob[2L])]
            } 
          )
          return(unique(rbindlist(counterfactuals)))
        }
      }
    
    },

    print_parameters = function() {
      cat(" - optimization: ", private$optimization, "\n")
      cat(" - threshold: ", paste0(names(private$threshold), ":", private$threshold), "\n")
      cat(" - correct_classif_only: ", private$correct_classif_only, "\n")
    }
  )
)

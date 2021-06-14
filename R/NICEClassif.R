#' @export
NICEClassif = R6::R6Class("NICEClassif",
  inherit = CounterfactualMethodClassif,

  public = list(
    
    cache = list(),
    
    # correct_classif_only: Only correctly classified observations are considered as nearest neighbor
    initialize = function(predictor, optimization = "sparsity", threshold = NULL, correct_classif_only = TRUE, 
                          lower = NULL, upper = NULL) {
      
      super$initialize(predictor, lower, upper)
      
      assert_choice(optimization, choices = c("sparsity", "proximity", "plausibility"))
      private$optimization = optimization
      private$correct_classif_only = correct_classif_only
      
      # TODO: If threshold set and correct_classif_only = FALSE then Message that threshold is ignored
      # TODO: Add checks for threshold
      if (is.null(threshold)) {
        k = nrow(unique(predictor$data$y))
        threshold = rep(1 / k, k)
        names(threshold) = unique(predictor$data$y)[[1L]]
      }
      
      if (private$optimization == "plausibility") {
        if (!requireNamespace("keras", quietly = TRUE)) {
          stop("Package 'keras' needed for this function to work. Please install it.", call. = FALSE)
        }
        private$aep = AEPreprocessor$new(predictor$data$X)
        private$ae_model = train_AE_model(predictor$data$X, private$aep)
      }
      
      private$y_hat = private$predictor$predict(predictor$data$X)
      private$threshold = threshold[names(private$y_hat)]
      
      pred_class = private$y_hat > private$threshold
      
      private$X_train_class = rep(NA, nrow(pred_class))
      for (r in 1:nrow(pred_class)) {
        if (sum(pred_class[r, ]) > 1L) {
          private$X_train_class[r] = names(private$y_hat)[which.max(pred_class[r, ])]
        } else if (!any(pred_class[r, ])) {
          private$X_train_class[r] = NA
        } else {
          private$X_train_class[r] = names(private$y_hat)[which(pred_class[r, ])]
        }
      }
      
    }

  ),

  private = list(
    optimization = NULL,
    predictor = NULL,
    X_train_class = NULL,
    desired_class = NULL,
    X_class = NULL,
    ae_model = NULL,
    aep = NULL,
    y_hat = NULL,
    threshold = NULL,
    correct_classif_only = NULL,

    run = function() {
      predictor = private$predictor
      desired_class = private$desired_class
      desired_prob = private$desired_prob
      
      X_candidates = predictor$data$X 
      is_correctly_classified = rep(TRUE, nrow(X_candidates))
      if (private$correct_classif_only) {
        
        # Converts multi-class to binary-class problem
        is_correctly_classified = ifelse(
          predictor$data$y == desired_class, 
          private$X_train_class == desired_class, 
          private$X_train_class != desired_class
        )
        is_correctly_classified[is.na(is_correctly_classified)] = FALSE
        X_candidates = X_candidates[as.vector(is_correctly_classified)]
        
        if (nrow(X_candidates) == 0L) {
          message("No counterfactuals could be found. Please adapt threshold or consider switching off correct_classif_only TODO!!!")
          return(setNames(data.table(matrix(nrow = 0L, ncol = ncol(predictor$data$X))), names(predictor$data$X)))
        }
      }
      
      in_desired_prob = between(
        private$y_hat[[desired_class]][is_correctly_classified], desired_prob[1L], desired_prob[2L]
      )
      X_candidates = X_candidates[in_desired_prob]
      
      if (nrow(X_candidates) == 0L) {
        message("No counterfactuals could be found. Please adapt threshold. TODO!!!")
        return(setNames(data.table(matrix(nrow = 0L, ncol = ncol(predictor$X))), names(ncol(predictor$X))))
      }
      
      
      distance = StatMatch::gower.dist(private$x_interest, X_candidates)
      x_nn = X_candidates[which.min(distance)]
      x_current = copy(private$x_interest)
      i = 1L
      
      # TODO: Check if x_nn == x_current
      
      while (TRUE) {
        x_prev = x_current
        
        diff = which(x_prev != x_nn)
        if (length(diff) == 0) {
          message("No counterfactuals could be found. Please adapt desired_range or consider switching off correct_classif_only TODO!!!")
          return(setNames(data.table(matrix(nrow = 0L, ncol = ncol(predictor$X))), names(ncol(predictor$X))))
        }
        
        X_prune = x_prev[rep(seq_len(nrow(x_prev)), length(diff))]
        for (j in seq(length(diff))) {
          col_name = names(x_nn)[diff[j]]
          repl_val = x_nn[, col_name, with = FALSE]
          X_prune[j, (col_name) := repl_val]
        }
        
        f_x_prev = predictor$predict(x_prev)[desired_class][[1L]]
        f_X_prune = predictor$predict(X_prune)[desired_class][[1L]]
        if (private$optimization == "sparsity") {
          reward = abs(f_x_prev - f_X_prune)
          
        } else if (private$optimization == "proximity") {
          d_X_prune = StatMatch::gower.dist(X_prune, private$x_interest)
          d_x_prev = as.vector(StatMatch::gower.dist(x_prev, private$x_interest))
          reward = abs((f_x_prev - f_X_prune) / (d_X_prune - d_x_prev + sqrt(.Machine$double.eps)))
          
        } else {
          
          X_prune_pp = private$aep$preprocess(X_prune)
          x_prev_pp = private$aep$preprocess(x_prev)
          
          ae_pred_X_prune = private$ae_model$predict(as.matrix(X_prune_pp))
          AE_loss_X_prune = rowMeans((X_prune_pp - ae_pred_X_prune)^2)
          
          ae_pred_x = private$ae_model$predict(as.matrix(x_prev_pp))
          AE_loss_x = rowMeans((x_prev_pp - ae_pred_x)^2)
          reward = abs((f_x_prev - f_X_prune) * (AE_loss_x - AE_loss_X_prune))
        }
        
        x_current = X_prune[which.max(reward)]
        
        self$cache[[i]] = list("X_prune" = X_prune, "reward" = reward)
        i = i + 1L
        
        if (between(predictor$predict(x_current)[[desired_class]], desired_prob[1], desired_prob[2])) {
          counterfactuals = lapply(
            self$cache, 
            function(X) {
              in_desired_prob = between(
                predictor$predict(X$X_prune)[[desired_class]], desired_prob[1], desired_prob[2]
              )
              X$X_prune[in_desired_prob]
            }
          )
          return(unique(rbindlist(counterfactuals)))
        }
      }
    
    },

    print_parameters = function() {
      cat("\t", "n_counterfactuals: ", private$n_counterfactuals)
    }
  )
)

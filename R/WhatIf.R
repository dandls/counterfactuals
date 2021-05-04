#' @import data.table
WhatIf = R6Class("WhatIf",
  inherit = Counterfactuals,
  
  private = list(
    X = NULL,
    y_hat = NULL,
    X_desired_outcome = NULL,
    dist_vector = NULL,
    n_counterfactuals = NULL,
    n_cores = NULL,
    
    preprocess = function() {
      is_desired_outcome = (private$y_hat == private$desired_outcome)
      private$X_desired_outcome = private$X[is_desired_outcome]
    },
    
    calculate = function() {
      private$dist_vector = private$compute_gower_dist(
        private$x_interest, private$X_desired_outcome
      )
    },
    
    aggregate = function() {
      X_temp = private$X_desired_outcome
      X_temp[, c("dist_x_interest", "pred") := list(private$dist_vector, private$desired_outcome)]
      
      cfactuals = head(data.table::setorder(X_temp, dist_x_interest), private$n_counterfactuals)
      n_changes = private$count_changes(cfactuals[, names(private$x_interest), with = FALSE])
      cfactuals[, "nr_changed" := n_changes]
      
      private$.results = private$make_results_list(cfactuals)
    },
    
    compute_gower_dist = function(x_interest, X, n_cores = private$n_cores) {
      gower_dist(x_interest, X, n_cores)
    },
    
    one_hot_to_one_col = function(df) {
      colnames(df)[apply(df, 1, which.max)]
    },
    
    infer_X = function(X) {
      if (is.null(X)) {
        res = private$predictor$data$get.x()
      } else {
        res = X
      }
      res
    }
    
  ),
  
  public = list(
    
    # Should also run `find_counterfactuals` when x_interest etc, is set
    initialize = function(predictor, X = NULL, n_counterfactuals = 1L, 
                          n_cores = parallel::detectCores() - 1, x_interest = NULL, 
                          desired_outcome = NULL) {
                          
      
      # TODO: Check if y is in X -> if yes remove and message
      private$predictor = predictor
      private$X = data.table::setDT(private$infer_X(X))
      private$n_counterfactuals = n_counterfactuals
      private$n_cores = n_cores
      
      y_hat_raw = predictor$predict(private$X)
      private$check_that_classif_task(y_hat_raw)
      y_hat_one_col = private$one_hot_to_one_col(y_hat_raw)
      private$y_hat = y_hat_one_col
      
      run_method = !is.null(x_interest) & !is.null(desired_outcome)
      if (run_method) {
        self$find_counterfactuals(x_interest, desired_outcome)
      }
      
    },
    
    find_counterfactuals = function(x_interest, desired_outcome) {
      
      # TODO: Check if desired_outcome is in private$y_hat
      
      private$x_interest = data.table::setDT(x_interest)
      private$desired_outcome = desired_outcome

      private$run()
    }
  )
)

library(doParallel)
gower_dist <- function(x, data, n_cores) {

  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))
  
  # Deactivate multiprocessing in case of too less data
  if (floor(nrow(data) / n_cores) == 0) {
    n_cores = 1
  }

  # For numeric variables, build ranges (max-min) to be used in gower-distance.
  get_range_if_numeric = function(x) {
    is_numeric = checkmate::test_numeric(x)
    ifelse(is_numeric, max(x, na.rm = TRUE) - min(x, na.rm = TRUE), NA)
  }
  datax = rbind(data, x)
  ranges = sapply(datax, get_range_if_numeric)

  chunk_size = floor(nrow(data) / n_cores)
  dist_vector = foreach(
      i = 1:n_cores, .packages = c("StatMatch"), .combine = "c", .export = "gower_dist"
    ) %dopar% {
      from = (i - 1) * chunk_size + 1
      to = i * chunk_size
      if (i == n_cores) {
        to = nrow(data)
      }
      dist_vector = as.vector(gower.dist(x, data[from:to,], rngs = ranges))
    }
  
  dist_vector
}


# set.seed(123456)
# iris_sub <- iris[sample(nrow(iris), 20), ]
# oneinst = iris_sub[7, ]
# target = as.vector(oneinst[5]) # virginica
# x.interest = as.vector(oneinst[-5])
# desired_outcome = "setosa"
# iris_sub_desired_outcome = subset(iris_sub, Species == desired_outcome)
# wi <- WhatIf$new(data = iris_sub)
# wi$find_counterfactuals(x.interest, "setosa", n = 3L)
# wi$results

# testi = gower_dist(x.interest, iris_sub_desired_outcome[, -5],  n_cores = 7)
# testi
# iris_sub_desired_outcome[which.min(testi), ]


# library("randomForest")
# rf <- randomForest(Species ~ ., data = iris, ntree = 20)
# mod <- Predictor$new(rf, data = iris)
# preds = mod$predict(iris[50:55, ])
# oneinst = iris[7, ]
# x_interest = as.vector(oneinst[-5])
# 
# wi <- WhatIf$new(mod, X = iris[, -5])
# wi$find_counterfactuals(x_interest, "setosa", n = 3L)
# wi$results
# 
# library(mlbench)
# library("randomForest")
# data(PimaIndiansDiabetes)
# dim(PimaIndiansDiabetes)
# levels(PimaIndiansDiabetes$diabetes)
# head(PimaIndiansDiabetes)
# 
# 
# 
# 
# rf <- randomForest(diabetes ~ ., data = PimaIndiansDiabetes, ntree = 20)
# mod <- Predictor$new(rf)
# oneinst = PimaIndiansDiabetes[7, -9]
# wi <- WhatIf$new(mod, X = PimaIndiansDiabetes[, -9])
# wi$find_counterfactuals(oneinst, "neg", n = 3L)
# wi$results
# 
# 
# gower_dist(oneinst, PimaIndiansDiabetes[1:5, -9],  n_cores = 3)
# 
# library("randomForest")
# testi = mtcars
# testi$am = as.factor(testi$am)
# testi$vs = as.factor(testi$vs)
# testi$cyl = as.factor(testi$cyl)
# rf <- randomForest(am ~ ., data = testi[, c("am", "vs", "cyl")], ntree = 20)
# 
# oneinst = head(subset(testi, select = c(vs, cyl)), 1L)
# gower_dist(oneinst, testi[, c("vs", "cyl")],  n_cores = 3)
# mod_class = Predictor$new(rf, testi[, c("am", "vs", "cyl")])

# wi = WhatIf$new(mod_class)
# oneinst = head(subset(testi, select = c(vs, cyl)), 1L)
# n = 3L
# wi$find_counterfactuals(oneinst, "0", n = n, n_cores = 1L)
# wi$results
# 
# wi$results$counterfactuals_diff[1, 1:7]




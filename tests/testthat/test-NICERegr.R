library(randomForest)
test_that("Returns correct output format for soft binary classification", {
  set.seed(54542140L)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  mydf$cyl = as.integer(mydf$cyl)
  
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")
  
  x_interest = head(subset(mydf, select = -mpg), 1)
  desired_outcome = c(15, 18)
  
  # Optim Sparsity
  nice_regr = NICERegr$new(pred, optimization = "sparsity", return_multiple = TRUE)
  cfactuals = nice_regr$find_counterfactuals(x_interest, desired_outcome = desired_outcome)
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
  
  # Optim Proximity
  nice_regr = NICERegr$new(pred, optimization = "proximity", return_multiple = TRUE)
  cfactuals = nice_regr$find_counterfactuals(x_interest, desired_outcome = desired_outcome)
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
  
  # Optim Plausibility
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("keras")
  set.seed(544564)
  nice_regr = NICERegr$new(pred, optimization = "plausibility", x_nn_correct = FALSE, return_multiple = TRUE)
  cfactuals = nice_regr$find_counterfactuals(x_interest, desired_outcome = desired_outcome)
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

test_that("Returns warning if no counterfactuals could be found", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  rf = randomForest::randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf)
  x_interest = head(subset(mydf, select = -mpg), n = 1L)
  
  nice_regr = NICERegr$new(pred, optimization = "sparsity", x_nn_correct = TRUE, return_multiple = TRUE)
  print(nice_regr)
  expect_warning(cfactuals <- nice_regr$find_counterfactuals(x_interest, desired_outcome = c(1, 5)), "No counterfactuals could be found")
  expect_error(nice_regr$archive <- 1234L, "read only")
  expect_error(nice_regr$x_nn <- c(), "read only")
  expect_null(nice_regr$archive)
  expect_null(nice_regr$x_nn)
})


test_that("Returns x_nn if finish_early=FALSE and return_multiple=FALSE", {
  set.seed(54542142)
  rf = get_rf_regr_mtcars()
  mod = Predictor$new(rf)
  x_interest = mod$data$get.x()[1,]
  nice_regr = NICERegr$new(mod, optimization = "sparsity", finish_early = FALSE, return_multiple = FALSE)
  cfactuals = nice_regr$find_counterfactuals(x_interest, desired_outcome = c(15, 18))
  expect_identical(cfactuals$data, nice_regr$x_nn)
})

test_that("distance_function can be exchanged", {
  set.seed(54542142)
  rf = get_rf_regr_mtcars()
  mtcars_pred = iml::Predictor$new(rf)
  x_interest = mtcars_pred$data$get.x()[1,]
  correct_dist_function = function(x, y, data) {
    res = matrix(NA, nrow = nrow(x), ncol = nrow(y))
    for (i in 1:nrow(x)) for (j in 1:nrow(y)) res[i, j] = sqrt(sum(((x[i, ] - y[j, ])^2)))
    res
  }
  nice_regr = NICERegr$new(
    mtcars_pred, optimization = "sparsity", finish_early = FALSE, return_multiple = FALSE, distance_function = correct_dist_function
  )
  cfactuals = nice_regr$find_counterfactuals(x_interest, desired_outcome = c(15, 18))
  expect_data_table(cfactuals$data)
})

test_that("distance_function gower and gower_c return equal results", {
  set.seed(1007)
  rf = get_rf_regr_mtcars()
  mtcars_pred = iml::Predictor$new(rf)
  x_interest = mtcars_pred$data$get.x()[5,]
  # Find counterfactuals for x_interest with gower distance
  nice_g = NICERegr$new(mtcars_pred, return_multiple = TRUE, optimization = "proximity", distance_function = "gower")
  cfactuals_g = nice_g$find_counterfactuals(
    x_interest = x_interest, desired_outcome = c(20, 25)
  )
  # Find counterfactuals for x_interest with gower distance C function
  nice_gc = NICERegr$new(mtcars_pred, return_multiple = TRUE, optimization = "proximity", distance_function = "gower_c")
  cfactuals_gc = nice_gc$find_counterfactuals(
    x_interest = x_interest, desired_outcome = c(20, 25)
  )
  # Print the results
  expect_equal(cfactuals_g$data, cfactuals_gc$data)
})



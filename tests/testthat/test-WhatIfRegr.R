library(randomForest)

test_that("Returns correct output format for mixed columns", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  mydf$cyl = as.factor(mydf$cyl)
  
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")
  n = 3L
  wi = WhatIfRegr$new(pred, n_counterfactuals = n)
  x_interest = head(subset(mydf, select = -mpg), 1)
  desired_outcome = c(15, 18)
  cfactuals = wi$find_counterfactuals(x_interest, desired_outcome)
  
  expect_data_table(cfactuals$data, nrows = n, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

test_that("Correct handling of lower and upper", {
  set.seed(54542142)
  mydf = mtcars
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")
  n = 3L
  x_interest = mydf[1, ]
  
  wi = WhatIfRegr$new(
    pred, n_counterfactuals = n, 
    lower = c("disp" = 80, "hp" = 50), upper = c("disp" = 180, "hp" = 200)
  )
  cfactuals = wi$find_counterfactuals(x_interest, desired_outcome = c(22, 25))
  expect_true(all(between(cfactuals$data$disp, 80, 180)))
  expect_true(all(between(cfactuals$data$hp, 50, 200)))
  
  expect_snapshot(
    WhatIfRegr$new(
      pred, n_counterfactuals = n, 
      lower = c("disp" = 80, "hp" = 100), upper = c("disp" = 100, "hp" = 120)
    )
  )
  
  expect_snapshot(
    WhatIfRegr$new(
      pred, n_counterfactuals = n, 
      lower = c("disp" = 0), upper = c("disp" = 10)
    )
  ) 
  
  expect_snapshot(
    WhatIfRegr$new(
      pred, n_counterfactuals = n, 
      lower = c("disp" = 1000), upper = c("disp" = 2000)
    )
  ) 
  
})

test_that("distance_function can be exchanged", {
  set.seed(54542142)
  mydf = mtcars
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")
  n = 3L
  x_interest = mydf[1, ]
  
  correct_dist_function = function(x, y, data) {
    res = matrix(NA, nrow = nrow(x), ncol = nrow(y))
    for (i in 1:nrow(x)) for (j in 1:nrow(y)) res[i, j] = sqrt(sum(((x[i, ] - y[j, ])^2)))
    res
  }
  wi = WhatIfRegr$new(
    pred, n_counterfactuals = n, 
    distance_function = correct_dist_function  
  )
  cfactuals = wi$find_counterfactuals(x_interest, desired_outcome = c(22, 25))
  expect_data_table(cfactuals$data, nrows = n)
})




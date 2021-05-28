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
  wi = WhatIfRegr$new(pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = head(subset(mydf, select = -mpg), 1)
  desired_outcome = c(15, 18)
  wi$find_counterfactuals(x_interest, desired_outcome)
  res = wi$results
  
  expect_data_table(res, nrows = n, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(res), identical.to = names(x_interest))
})







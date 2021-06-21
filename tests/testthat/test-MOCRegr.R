library(randomForest)

test_that("Returns correct output format for mixed columns for 'random' initialization", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  mydf$cyl = as.integer(mydf$cyl)
  
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")
  
  x_interest = head(subset(mydf, select = -mpg), 1)
  desired_outcome = c(15, 18)
  mocr = MOCRegr$new(pred, init_strategy = "random", n_generations = 5L)
  cfactuals = quiet(mocr$find_counterfactuals(x_interest, desired_outcome))
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})


test_that("Returns correct output format for mixed columns for 'sd' initialization", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  mydf$cyl = as.factor(mydf$cyl)
  
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")
  
  mocr = MOCRegr$new(pred, init_strategy = "sd", n_generations = 5L)
  x_interest = head(subset(mydf, select = -mpg), 1)
  desired_outcome = c(15, 18)
  cfactuals = quiet(mocr$find_counterfactuals(x_interest, desired_outcome))
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})


test_that("Returns correct output format for mixed columns for 'icecurve' initialization", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  mydf$cyl = as.factor(mydf$cyl)
  
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")
  
  mocr = MOCRegr$new(pred, init_strategy = "icecurve",  n_generations = 5L)
  x_interest = head(subset(mydf, select = -mpg), 1)
  desired_outcome = c(15, 18)
  cfactuals = quiet(mocr$find_counterfactuals(x_interest, desired_outcome))
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

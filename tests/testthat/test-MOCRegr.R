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
  
  expect_snapshot({cfactuals = quiet(mocr$find_counterfactuals(x_interest, desired_outcome))})
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
  
  expect_snapshot({cfactuals = quiet(mocr$find_counterfactuals(x_interest, desired_outcome))})
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
  
  mocr = MOCRegr$new(pred, init_strategy = "icecurve", use_conditional_mutator = TRUE, n_generations = 3L)
  x_interest = head(subset(mydf, select = -mpg), 1)
  desired_outcome = c(15, 18)
  
  cfactuals = quiet(mocr$find_counterfactuals(x_interest, desired_outcome))
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
  mocr$print()
})

test_that("conditional mutator and plotting functions work", {
  
  set.seed(54542142)
  rf = get_rf_regr_mtcars()
  mtcars_pred = iml::Predictor$new(rf, data = mtcars)
  x_interest = mtcars[1L, ]
  moc_regr = MOCRegr$new(
    mtcars_pred, n_generations = 3L, init_strategy = "traindata", use_conditional_mutator = FALSE, quiet = TRUE
  )
  cfactuals = moc_regr$find_counterfactuals(x_interest, desired_outcome = c(1, 10))
  expect_data_table(cfactuals$data)
  p1 = moc_regr$plot_search()
  p2 = moc_regr$plot_statistics()
  expect_data_table(moc_regr$get_dominated_hv(), nrows = 3, ncols = 2)
  expect_data_table(moc_regr$optimizer$archive$data, nrows = 20*3)
  expect_error(moc_regr$optimizer <- 35L, "read only")
})

test_that("distance_function can be exchanged", {
  set.seed(54542142)
  mydf = mtcars
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")
  x_interest = mydf[1, ]
  
  correct_dist_function = function(x, y, data) {
    res = matrix(NA, nrow = nrow(x), ncol = nrow(y))
    for (i in 1:nrow(x)) for (j in 1:nrow(y)) res[i, j] = sqrt(sum(((x[i, ] - y[j, ])^2)))
    res
  }
  moc_regr = MOCRegr$new(
    pred, init_strategy = "random", n_generations = 3L, distance_function = correct_dist_function, quiet = TRUE
  )
  cfactuals = moc_regr$find_counterfactuals(x_interest, desired_outcome = c(22, 25))
  expect_data_table(cfactuals$data)
})

test_that("init_strategy 'icecurve' works ordered factors", {
  df = mtcars
  df$cyl = factor(df$cyl, ordered = TRUE)
  rf = randomForest(mpg ~ ., data = df)
  predictor = iml::Predictor$new(rf, data = df)
  moc_regr = MOCRegr$new(predictor, n_generations = 15L, quiet = TRUE)
  cfactuals = moc_regr$find_counterfactuals(
    x_interest = df[1L, ], desired_outcome = c(22, 24)
  )
  expect_data_table(cfactuals$data)
  expect_factor(cfactuals$data$cyl, ordered = TRUE)
})
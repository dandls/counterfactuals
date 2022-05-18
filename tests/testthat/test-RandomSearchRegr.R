library(randomForest)

set.seed(54542142L)
mydf = mtcars
mydf$am = as.factor(mydf$am)
mydf$vs = as.factor(mydf$vs)
mydf$cyl = as.integer(mydf$cyl)

rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
pred = iml::Predictor$new(rf, data = mydf, y = "mpg")

x_interest = head(subset(mydf, select = -mpg), 1)
desired_outcome = c(15, 18)

test_that("RandomSearchRegr is similar to MOCRegr", {
 
  set.seed(1234L)
  rs_regr = RandomSearchRegr$new(pred, n_generations = 5L)
  cfactuals = rs_regr$find_counterfactuals(
    x_interest = x_interest, desired_outcome = desired_outcome
  )
  expect_data_table(cfactuals$data, max.rows = 5L*20L, ncols = pred$data$n.features)
  
  rs_regr$plot_search()
  rs_regr$plot_statistics()
  expect_data_table(rs_regr$optimizer$archive$data, nrows = 20L*5L)
  expect_error(rs_regr$optimizer <- 35L, "read only")
  expect_data_table(rs_regr$get_dominated_hv(), nrow = 5L)
})

test_that("gower or gower_c returns same result", {
  
  set.seed(1234L)
  rs_regr = RandomSearchRegr$new(pred, n_generations = 5L)
  cfactuals = rs_regr$find_counterfactuals(
    x_interest = x_interest, desired_outcome = desired_outcome
  )
  cfd = cfactuals$data
  
  set.seed(1234L)
  rs_regr = RandomSearchRegr$new(pred, n_generations = 5L, distance_function = "gower_c")
  cfactuals = rs_regr$find_counterfactuals(
    x_interest = x_interest, desired_outcome = desired_outcome
  )
  cfdc = cfactuals$data
  
  expect_equal(cfd, cfdc)
  
})
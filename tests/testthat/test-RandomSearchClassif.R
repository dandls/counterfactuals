library(randomForest)

test_that("RandomSearchClassif is similar to MOCClassif", {
  rf = randomForest(Species ~ ., data = iris)
  predictor = iml::Predictor$new(rf, type = "prob")
  
  set.seed(1234L)
  rs_classif = RandomSearchClassif$new(predictor, n_generations = 30L, distance_function = "gower")
  cfactuals = rs_classif$find_counterfactuals(
    x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
  )
  expect_data_table(cfactuals$data, max.rows = 30L*20L, ncols = 4L)
  
  rs_classif$plot_search()
  rs_classif$plot_statistics()
  expect_data_table(rs_classif$optimizer$archive$data, nrows = 20L*30L)
  expect_error(rs_classif$optimizer <- 35L, "read only")
  expect_data_table(rs_classif$get_dominated_hv(), nrow = 30L)
})

test_that("gower or gower_c returns same result", {
  
  rf = randomForest(Species ~ ., data = iris)
  predictor = iml::Predictor$new(rf, type = "prob")

  set.seed(1234L)
  rs_classif = RandomSearchClassif$new(predictor, n_generations = 30L, distance_function = "gower")
  cfactuals = rs_classif$find_counterfactuals(
    x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
  )
  cfd = cfactuals$data
 
  set.seed(1234L)
  rs_classif = RandomSearchClassif$new(predictor, n_generations = 30L, distance_function = "gower_c")
  cfactuals = rs_classif$find_counterfactuals(
    x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
  )
  cfdc = cfactuals$data
  
  expect_equal(cfd, cfdc)
  
})
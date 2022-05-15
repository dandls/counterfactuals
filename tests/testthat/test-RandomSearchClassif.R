library(randomForest)
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
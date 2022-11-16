library(randomForest)
test_that("Returns correct output format for soft binary classification", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf, type = "class")
  mocc = MOCClassif$new(pred, n_generations = 5L, init_strategy = "random")
  x_interest = head(subset(mydf, select = -am), n = 1L)
  expect_snapshot({cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "0"))})
  
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
  
})

test_that("Returns correct output format for hard binary classification", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "class")
  mocc = MOCClassif$new(iris_pred, n_generations = 5L, init_strategy = "random")
  x_interest = iris[1L, -5L]
  expect_snapshot({
    cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1))
  })  
  
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

test_that("Can handle non-numeric target classes", {
  set.seed(544564)
  test_data = data.frame(a = rnorm(10), b = rnorm(10), cl = as.factor(rep(c("pos", "neg"), each = 5)), 
    it = as.integer(c(2, 6, 1, 0, 2)))
  rf_pima = randomForest::randomForest(cl ~ . , test_data, ntree = 2L)
  pred = iml::Predictor$new(rf_pima, data = test_data, y = "cl")
  x_interest = head(subset(test_data, select = -cl), 1L)
  set.seed(544564)
  mocc = MOCClassif$new(pred, n_generations = 5L, init_strategy = "random")
  expect_snapshot({
    cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "pos"))
  })
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

test_that("Can handle ordered factor input columns", {
  set.seed(5748554)
  data("german", package = "rchallenge")
  rf =  randomForest(credit_risk ~ ., data = german)
  x_interest = german[991L, -ncol(german)]
  pred_credit = iml::Predictor$new(rf, data = german, y = "credit_risk", type = "prob")
  moc_classif = MOCClassif$new(
    pred_credit, n_generations = 3L, fixed_features = c("personal_status_sex", "age"), max_changed = 4L, 
    init_strategy = "random"
  )
  expect_snapshot({
    cfactuals = quiet(moc_classif$find_counterfactuals(x_interest, desired_class = "good", desired_prob = c(0.8 , 1)))
  })
  expect_data_table(cfactuals$data, col.names = "named")
  expect_factor(cfactuals$data$installment_rate, levels = levels(german$installment_rate), ordered = TRUE)
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

test_that("conditional mutator and plotting functions work", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "prob")
  x_interest = iris[1L, ]
  moc_classif = MOCClassif$new(
    iris_pred, n_generations = 3L, init_strategy = "traindata", use_conditional_mutator = TRUE, quiet = TRUE
  )
  cfactuals = moc_classif$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = c(0.5, 1))
  expect_data_table(cfactuals$data)
  p1 = moc_classif$plot_search()
  p2 = moc_classif$plot_statistics()
  expect_data_table(moc_classif$get_dominated_hv(), nrows = 3, ncols = 2)
  expect_data_table(moc_classif$optimizer$archive$data, nrows = 20*3)
  expect_error(moc_classif$optimizer <- 35L, "read only")
})


test_that("distance_function can be exchanged", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "prob")
  x_interest = iris[1L, ]
  correct_dist_function = function(x, y, data) {
    res = matrix(NA, nrow = nrow(x), ncol = nrow(y))
    for (i in 1:nrow(x)) for (j in 1:nrow(y)) res[i, j] = sqrt(sum(((x[i, ] - y[j, ])^2)))
    res
  }
  moc_classif = MOCClassif$new(
    iris_pred, n_generations = 3L, distance_function = correct_dist_function, quiet = TRUE, 
    init_strategy = "random"
  )
  cfactuals = moc_classif$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = c(0.5, 1))
  expect_data_table(cfactuals$data)
})


test_that("distance_function gower and gower_c return equal results", {
  set.seed(1007)
  rf = randomForest(Species ~ ., data = iris)
  # Create a predictor object
  predictor = iml::Predictor$new(rf, type = "prob")
  # Find counterfactuals for x_interest
  set.seed(1007)
  moc_g = MOCClassif$new(predictor, n_generations = 0L, distance_function = "gower", 
    init_strategy = "random")
  cfactuals_g = moc_g$find_counterfactuals(
    x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
  )
  # Find counterfactuals for x_interest with gower distance C function
  set.seed(1007)
  moc_gc = MOCClassif$new(predictor, n_generations = 0L, distance_function = "gower_c", 
    init_strategy = "random")
  cfactuals_gc = moc_gc$find_counterfactuals(
    x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
  )
  # Print the results
  expect_equal(cfactuals_g$data, cfactuals_gc$data)
})



library(randomForest)
test_that("Returns correct output format for soft binary classification", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf, type = "class")
  x_interest = head(subset(mydf, select = -am), n = 1L)

  # Optim Sparsity
  nice_classif = NICEClassif$new(pred, optimization = "sparsity")
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(pred, optimization = "proximity")
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Plausibility
  skip_on_ci()
  set.seed(544564)
  nice_classif = NICEClassif$new(pred, optimization = "plausibility", x_nn_correct_classif = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

test_that("Returns correct output format for hard binary classification", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "class")
  x_interest = iris[1L, -5L]

  # Optim Sparsity
  nice_classif = NICEClassif$new(iris_pred, optimization = "sparsity", x_nn_correct_classif = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1)
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(iris_pred, optimization = "proximity", x_nn_correct_classif = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1)
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Plausibility
  skip_on_ci()
  set.seed(544564)
  nice_classif = NICEClassif$new(iris_pred, optimization = "plausibility", x_nn_correct_classif = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1)
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

})

test_that("Can handle non-numeric target classes", {
  set.seed(544564)
  test_data = data.frame(
    a = c(rbinom(100, size = 10, prob = 0.1), rbinom(100, size = 10, prob = 0.9)),
    b = c(rbinom(100, size = 10, prob = 0.1), rbinom(100, size = 10, prob = 0.9)),
    cl = as.factor(rep(c("pos", "neg"), each = 50))
  )
  rf = randomForest::randomForest(cl ~ . , test_data, ntree = 20L)
  pred = iml::Predictor$new(rf, data = test_data, y = "cl")
  x_interest = head(subset(test_data, select = -cl), 1L)

  # Optim Sparsity
  nice_classif = NICEClassif$new(pred, optimization = "sparsity", x_nn_correct_classif = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "neg")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(pred, optimization = "proximity", x_nn_correct_classif = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "neg")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
  
  # Optim Plausibility
  skip_on_ci()
  set.seed(57421)
  nice_classif = NICEClassif$new(pred, optimization = "plausibility", x_nn_correct_classif = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "neg")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
  
})

test_that("Can handle ordered factor input columns", {
  set.seed(5748554)
  data("german", package = "rchallenge")
  rf =  randomForest(credit_risk ~ ., data = german)
  x_interest = german[991L, -ncol(german)]
  pred_credit = iml::Predictor$new(rf, data = german, y = "credit_risk", type = "prob")

  # Optim Sparsity
  nice_classif = NICEClassif$new(pred_credit, optimization = "sparsity", x_nn_correct_classif = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "good", desired_prob = c(0.8 , 1))
  expect_data_table(cfactuals$data, col.names = "named")
  expect_identical(sapply(cfactuals$data, class), sapply(x_interest, class))
  expect_factor(cfactuals$data$installment_rate, levels = levels(german$installment_rate), ordered = TRUE)
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(pred_credit, optimization = "proximity", x_nn_correct_classif = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "good", desired_prob = c(0.8 , 1))
  expect_data_table(cfactuals$data, col.names = "named")
  expect_identical(sapply(cfactuals$data, class), sapply(x_interest, class))
  expect_factor(cfactuals$data$installment_rate, levels = levels(german$installment_rate), ordered = TRUE)
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Plausibility
  skip_on_ci()
  set.seed(544564)
  nice_classif = NICEClassif$new(pred_credit, optimization = "plausibility", x_nn_correct_classif = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "good", desired_prob = c(0.8 , 1))
  expect_data_table(cfactuals$data, col.names = "named")
  expect_identical(sapply(cfactuals$data, class), sapply(x_interest, class))
  expect_factor(cfactuals$data$installment_rate, levels = levels(german$installment_rate), ordered = TRUE)
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

})


test_that("Returns warning if no counterfactuals could be found", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf, type = "class")
  x_interest = head(subset(mydf, select = -am), n = 1L)
  nice_classif = NICEClassif$new(pred, optimization = "sparsity", x_nn_correct_classif = TRUE)
  expect_snapshot({cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0", desired_prob = 0.454851)})
})


test_that("Returns equal results with and without parallelization", {
  skip_if_not(parallel::detectCores() > 1L)
  # skip_on_ci()
  set.seed(54542142)
  rf = get_rf_classif_iris()
  mod = Predictor$new(rf, data = iris, y = "Species")
  x_interest = iris[1L, ]
  set.seed(54542142)
  nice_classif = NICEClassif$new(mod, optimization = "sparsity")
  future::plan(future::multisession, workers = parallel::detectCores() - 1L)
  set.seed(54542142)
  par = nice_classif$find_counterfactuals(x_interest, "versicolor", c(0.7, 1))
  future::plan(future::sequential)
  sequ = nice_classif$find_counterfactuals(x_interest, "versicolor", c(0.7, 1))
  expect_equal(par, sequ)
})


test_that("Returns x_nn if finish_early=FALSE and return_multiple=FALSE", {
  skip_if_not(parallel::detectCores() > 1L)
  set.seed(54542142)
  rf = get_rf_classif_iris()
  mod = Predictor$new(rf, data = iris, y = "Species")
  x_interest = iris[1L, ]
  nice_classif = NICEClassif$new(mod, optimization = "sparsity", finish_early = FALSE, return_multiple = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "versicolor")
  expect_identical(cfactuals$data, nice_classif$x_nn)
})






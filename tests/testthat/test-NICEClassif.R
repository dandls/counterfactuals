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
  nice_classif = NICEClassif$new(pred, optimization = "sparsity", return_multiple = TRUE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(pred, optimization = "proximity", return_multiple = TRUE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Plausibility
  skip_on_ci()
  skip_on_cran()
  set.seed(544564)
  nice_classif = NICEClassif$new(pred, optimization = "plausibility", x_nn_correct = FALSE, return_multiple = TRUE)
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
  nice_classif = NICEClassif$new(iris_pred, optimization = "sparsity", x_nn_correct = FALSE, return_multiple = TRUE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1)
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(iris_pred, optimization = "proximity", x_nn_correct = FALSE, return_multiple = TRUE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1)
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Plausibility
  skip_on_ci()
  skip_on_cran()
  set.seed(544564)
  nice_classif = NICEClassif$new(iris_pred, optimization = "plausibility", x_nn_correct = FALSE, return_multiple = TRUE)
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
  nice_classif = NICEClassif$new(pred, optimization = "sparsity", x_nn_correct = FALSE, return_multiple = TRUE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "neg")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(pred, optimization = "proximity", x_nn_correct = FALSE, return_multiple = TRUE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "neg")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
  
  # Optim Plausibility
  skip_on_ci()
  skip_on_cran()
  set.seed(57421)
  nice_classif = NICEClassif$new(pred, optimization = "plausibility", x_nn_correct = FALSE, return_multiple = TRUE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "neg")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
  
})

test_that("Can handle ordered factor input columns", {
  set.seed(5748554)
  data("german", package = "rchallenge")
  german = droplevels(german)
  rf =  randomForest(credit_risk ~ ., data = german)
  x_interest = german[991L, -ncol(german)]
  pred_credit = iml::Predictor$new(rf, data = german, y = "credit_risk", type = "prob")

  # Optim Sparsity
  nice_classif = NICEClassif$new(pred_credit, optimization = "sparsity", x_nn_correct = FALSE, return_multiple = TRUE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "good", desired_prob = c(0.8 , 1))
  expect_data_table(cfactuals$data, col.names = "named")
  expect_identical(sapply(cfactuals$data, class), sapply(x_interest, class))
  expect_factor(cfactuals$data$installment_rate, levels = levels(german$installment_rate), ordered = TRUE)
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(pred_credit, optimization = "proximity", x_nn_correct = FALSE, return_multiple = TRUE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "good", desired_prob = c(0.8 , 1))
  expect_data_table(cfactuals$data, col.names = "named")
  expect_identical(sapply(cfactuals$data, class), sapply(x_interest, class))
  expect_factor(cfactuals$data$installment_rate, levels = levels(german$installment_rate), ordered = TRUE)
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Plausibility
  skip_on_ci()
  skip_on_cran()
  set.seed(544564)
  nice_classif = NICEClassif$new(pred_credit, optimization = "plausibility", x_nn_correct = FALSE, return_multiple = TRUE)
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
  nice_classif = NICEClassif$new(pred, optimization = "sparsity", x_nn_correct = TRUE, return_multiple = TRUE)
  print(nice_classif)
  expect_snapshot({cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0", desired_prob = 0.454851)})
  expect_error(nice_classif$archive <- 1234L, "read only")
  expect_error(nice_classif$x_nn <- c(), "read only")
  expect_null(nice_classif$archive)
  expect_null(nice_classif$x_nn)
})


test_that("Returns x_nn if finish_early=FALSE and return_multiple=FALSE", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  mod = Predictor$new(rf, data = iris, y = "Species")
  x_interest = iris[1L, ]
  nice_classif = NICEClassif$new(mod, optimization = "sparsity", finish_early = FALSE, return_multiple = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "versicolor")
  expect_identical(cfactuals$data, nice_classif$x_nn)
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
  nice_classif = NICEClassif$new(
    iris_pred, optimization = "sparsity", finish_early = FALSE, return_multiple = FALSE, distance_function = correct_dist_function
  )
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = c(0.5, 1))
  expect_data_table(cfactuals$data)
})

test_that("distance_function gower and gower_c return equal results", {
  set.seed(1007)
  rf = randomForest(Species ~ ., data = iris)
  # Create a predictor object
  predictor = iml::Predictor$new(rf, type = "prob")
  # Find counterfactuals for x_interest with gower distance
  nice_g = NICEClassif$new(predictor, return_multiple = TRUE, optimization = "proximity", distance_function = "gower")
  cfactuals_g = nice_g$find_counterfactuals(
    x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
  )
  # Find counterfactuals for x_interest with gower distance C function
  nice_gc = NICEClassif$new(predictor, return_multiple = TRUE, optimization = "proximity", distance_function = "gower_c")
  cfactuals_gc = nice_gc$find_counterfactuals(
    x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
  )
  # Print the results
  expect_equal(cfactuals_g$data, cfactuals_gc$data)
})



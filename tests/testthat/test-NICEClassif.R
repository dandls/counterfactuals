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
  nice_classif = NICEClassif$new(pred, optimization = "plausibility", correct_classif_only = FALSE)
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
  nice_classif = NICEClassif$new(iris_pred, optimization = "sparsity", correct_classif_only = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1)
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(iris_pred, optimization = "proximity", correct_classif_only = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1)
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Plausibility
  skip_on_ci()
  set.seed(544564)
  nice_classif = NICEClassif$new(iris_pred, optimization = "plausibility", correct_classif_only = FALSE)
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
  nice_classif = NICEClassif$new(pred, optimization = "sparsity", correct_classif_only = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "neg")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(pred, optimization = "proximity", correct_classif_only = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "neg")
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
  
  # Optim Plausibility
  skip_on_ci()
  set.seed(57421)
  nice_classif = NICEClassif$new(pred, optimization = "plausibility", correct_classif_only = FALSE)
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
  nice_classif = NICEClassif$new(pred_credit, optimization = "sparsity", correct_classif_only = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "good", desired_prob = c(0.8 , 1))
  expect_data_table(cfactuals$data, col.names = "named")
  expect_identical(sapply(cfactuals$data, class), sapply(x_interest, class))
  expect_factor(cfactuals$data$installment_rate, levels = levels(german$installment_rate), ordered = TRUE)
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Proximity
  nice_classif = NICEClassif$new(pred_credit, optimization = "proximity", correct_classif_only = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "good", desired_prob = c(0.8 , 1))
  expect_data_table(cfactuals$data, col.names = "named")
  expect_identical(sapply(cfactuals$data, class), sapply(x_interest, class))
  expect_factor(cfactuals$data$installment_rate, levels = levels(german$installment_rate), ordered = TRUE)
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

  # Optim Plausibility
  skip_on_ci()
  set.seed(544564)
  nice_classif = NICEClassif$new(pred_credit, optimization = "plausibility", correct_classif_only = FALSE)
  cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "good", desired_prob = c(0.8 , 1))
  expect_data_table(cfactuals$data, col.names = "named")
  expect_identical(sapply(cfactuals$data, class), sapply(x_interest, class))
  expect_factor(cfactuals$data$installment_rate, levels = levels(german$installment_rate), ordered = TRUE)
  expect_names(names(cfactuals$data), identical.to = names(x_interest))

})

test_that("Returns message that `threshold` is ignored, when `correct_classif_only` is set to FALSE.", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf, type = "class")
  x_interest = head(subset(mydf, select = -am), n = 1L)
  th = c("0" = 0.5, "1" = 0.5)
  expect_snapshot({
    nice_classif = NICEClassif$new(pred, optimization = "sparsity", correct_classif_only = FALSE, threshold = th)
  })
})

test_that("Returns warning if no counterfactuals could be found", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf, type = "class")
  x_interest = head(subset(mydf, select = -am), n = 1L)
  th = c("0" = 1, "1" = 1)
  nice_classif = NICEClassif$new(pred, optimization = "sparsity", correct_classif_only = TRUE, threshold = th)
  expect_snapshot({cfactuals = nice_classif$find_counterfactuals(x_interest, desired_class = "0")})
})







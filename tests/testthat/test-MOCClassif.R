library(randomForest)
test_that("Returns correct output format for soft binary classification", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf, type = "class")
  mocc = MOCClassif$new(pred, n_generations = 5L)
  x_interest = head(subset(mydf, select = -am), n = 1L)
  cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "1"))
  
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

test_that("Returns correct output format for hard binary classification", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "class")
  mocc = MOCClassif$new(iris_pred, n_generations = 5L)
  x_interest = iris[1L, -5L]
  cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1))
  
  expect_data_table(cfactuals$data, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

test_that("Can handle non-numeric target classes", {
  set.seed(544564)
  test_data = data.frame(a = rnorm(10), b = rnorm(10), cl = as.factor(rep(c("pos", "neg"), each = 5)))
  rf_pima = randomForest::randomForest(cl ~ . , test_data, ntree = 2L)
  pred = iml::Predictor$new(rf_pima, data = test_data, y = "cl")
  x_interest = head(subset(test_data, select = -cl), 1L)
  set.seed(544564)
  mocc = MOCClassif$new(pred, n_generations = 5L)
  cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "pos"))
  
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
    pred_credit, n_generations = 3L, fixed_features = c("personal_status_sex", "age"), max_changed = 4L
  )
  cfactuals = quiet(moc_classif$find_counterfactuals(x_interest, desired_class = "good", desired_prob = c(0.8 , 1)))
  expect_data_table(cfactuals$data, col.names = "named")
  expect_factor(cfactuals$data$installment_rate, levels = levels(german$installment_rate), ordered = TRUE)
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})






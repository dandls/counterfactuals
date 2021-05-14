# library(checkmate)
# library(data.table)
# 
# # Initialization -----------------------------------------------------------------------------------
# test_that("Initialization returns appropriate error message if not a classification task", {
#   set.seed(465465)
#   y = rnorm(10)
#   X = as.data.frame(scale(data.frame(
#     col_a = rnorm(10),
#     col_b = rnorm(10)
#   )))
#   rf = randomForest::randomForest(X, y, ntree = 5L)
#   pred = iml::Predictor$new(rf, data = X, y = y)
#   expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 2L))
# })
# 
# test_that("Initialization returns appropriate error message if randomForest.formula class", {
#   set.seed(465465)
#   test_df = as.data.frame(scale(data.frame(
#     col_a = rnorm(10),
#     col_b = rnorm(10),
#     col_c = rnorm(10)
#   )))
#   rf = randomForest::randomForest(col_c ~ ., data = test_df)
#   pred = iml::Predictor$new(rf, data = test_df, y = "col_c")
#   expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 2L))
# })
# 
# test_that("Initialization returns appropriate error message if model is not randomForest", {
#   set.seed(465465)
#   test_df = as.data.frame(scale(data.frame(
#     col_a = rnorm(10),
#     col_b = rnorm(10),
#     col_c = rnorm(10)
#   )))
#   linear_regression = lm(col_c ~ ., data = test_df)
#   pred = iml::Predictor$new(linear_regression, data = test_df, y = "col_c")
#   expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 10L))
# })
# 
# 
# test_that("Initialization returns appropriate error message if training data are not standardized", {
#   set.seed(465465)
#   X = data.frame(
#     col_a = rnorm(10),
#     col_b = rnorm(10)
#   )
#   y = as.factor(rep(c("a", "b"), each = 5L))
#   rf = randomForest::randomForest(X, y, ntree = 5L)
#   pred = iml::Predictor$new(rf, data = X, y = y)
#   expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 2L))
# })
# 
# test_that("Initialization returns appropriate error message if `ktree` > total number of trees", {
#   set.seed(465465)
#   X = as.data.frame(scale(data.frame(
#     col_a = rnorm(10),
#     col_b = rnorm(10)
#   )))
#   y = as.factor(rep(c("a", "b"), each = 5L))
#   rf = randomForest::randomForest(X, y, ntree = 5L)
#   pred = iml::Predictor$new(rf, data = X, y = y)
#   expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 10L))
# })
# 
# test_that("Initialization returns warning message if `ktree` == total number of trees and sets 
#           `n_counterfactuals` to 1", {
#   set.seed(465465)
#   X = as.data.frame(scale(data.frame(
#     col_a = rnorm(10),
#     col_b = rnorm(10)
#   )))
#   y = as.factor(rep(c("a", "b"), each = 5L))
#   rf = randomForest::randomForest(X, y, ntree = 2L)
#   pred = iml::Predictor$new(rf, data = X, y = y)
#   ft = expect_warning(FeatureTweaker$new(pred, 10L, ktree = 2L), "deterministic")
#   expect_equal(ft$.__enclos_env__$private$n_counterfactuals, 1L)
# })
# 
# 
# # Binary classification ----------------------------------------------------------------------------
# test_that("Returns correct output format for numeric columns only", {
#   set.seed(54542142)
#   X = get_scaled_iris_features()
#   y = iris[, ncol(iris)]
#   rf = randomForest::randomForest(X, y, ntree = 20L)
#   x_interest = X[10L, ]
#   n = 2L
#   pred = Predictor$new(rf, data = X, y = y, class = "versicolor")
#   ft = FeatureTweaker$new(pred, n_counterfactuals = n, ktree = 5L)
#   ft$find_counterfactuals(x_interest)
#   res = ft$results
#   expect_list(res, len = 2L)
#   cfs = res$counterfactuals
#   cfs_diff = res$counterfactuals_diff
#   expect_data_table(cfs, nrows = n)
#   expected_cols = c(colnames(iris)[-5], "dist_x_interest", "pred", "nr_changed")
#   expect_true(all(colnames(cfs) == expected_cols))
#   expect_data_table(cfs_diff, nrows = n)
#   expect_true(all(colnames(cfs_diff) == expected_cols))
#   expected_diff = as.data.table(sweep(ft$results$counterfactuals[, 1:4], 2, as.numeric(x_interest)))
#   expect_equal(expected_diff, ft$results$counterfactuals_diff[, 1:4])
# })
# 
# test_that("Returns error message if categorical variables are in training data", {
#   set.seed(54542142)
#   test_df = mtcars[, c("am", "vs", "cyl")]
#   test_df$am = as.factor(test_df$am)
#   test_df$cyl = as.factor(test_df$cyl)
#   X = test_df[, c("vs", "cyl")]
#   y = test_df$am
#   rf = randomForest::randomForest(X, y, ntree = 20L)
#   pred = Predictor$new(rf, data = X, y = y, type = "class")
#   n = 2L
#   expect_snapshot_error(FeatureTweaker$new(pred, n_counterfactuals = n, ktree = 5L))
# })
# 
# 
# test_that("Init works for classification tasks only", {
#   set.seed(54542142)
#   # Regression task
#   X = as.data.frame(scale(subset(mtcars, select = -mpg)))
#   y = mtcars$mpg
#   rf_regr = randomForest::randomForest(X, y, ntree = 20L)
#   x_interest = X[10L, ]
#   n = 2L
#   pred_regr = Predictor$new(rf_regr, data = X, y = y)
#   expect_snapshot_error(FeatureTweaker$new(pred_regr, ktree = 5L))
# })
# 
# test_that("If `desired_outcome` is specified for binary class, it is set to the opposite of `x_interest`", {
#   X = get_scaled_iris_features()
#   y = iris[, ncol(iris)]
#   rf = randomForest::randomForest(X, y, ntree = 20L)
#   iris_pred = iml::Predictor$new(rf, data = X, y = y, type = "class", class = "versicolor")
#   n = 2L
#   x_interest = X[10L, ]
#   set.seed(544564)
#   ft = FeatureTweaker$new(iris_pred, n_counterfactuals = n, ktree = 1L)
#   ft$find_counterfactuals(x_interest)
#   res1 = ft$results
#   set.seed(544564)
#   ft = FeatureTweaker$new(iris_pred, n_counterfactuals = n, ktree = 1L)
#   ft$find_counterfactuals(x_interest)
#   res2 = ft$results
#   expect_identical(res1, res2)
# })
# 
# test_that("Can handle non-numeric target classes", {
#   set.seed(544564)
#   test_data = data.frame(
#     a = rnorm(10), 
#     b = rnorm(10), 
#     cl = as.factor(rep(c("pos", "neg"), each = 5))
#   )
#   X = as.data.frame(scale(test_data[, c("a", "b")]))
#   y = test_data$cl
#   rf_pima = randomForest::randomForest(X, y, ntree = 20L)
# 
#   pred = iml::Predictor$new(rf_pima, data = X, y = y)
#   n = 2L
#   x_interest = head(subset(test_data, select = -cl), 1L)
#   set.seed(544564)
#   ft = FeatureTweaker$new(pred, n_counterfactuals = n, ktree = 2L)
#   ft$find_counterfactuals(x_interest)
#   expect_data_table(ft$results$counterfactuals, nrows = n)
# })
# 
# # Mutliclass classification ------------------------------------------------------------------------
# test_that("$find_counterfactuals with specified `desired_outcome` returns the same results as if 
#           the `desired_outcome` is set in the iml `Predictor`", {
# 
#   set.seed(54542142)
#   X = get_scaled_iris_features()
#   y = iris[, ncol(iris)]
#   rf = randomForest::randomForest(X, y, ntree = 20L)
#   x_interest = X[10L, ]
#   n = 2L
#   desired_outcome = "versicolor"
# 
#   set.seed(354645)
#   iris_pred_bin = iml::Predictor$new(rf, data = X, y = y)
#   ft = FeatureTweaker$new(iris_pred_bin, n_counterfactuals = n, ktree = 2L)
#   ft$find_counterfactuals(x_interest, desired_outcome = desired_outcome)
#   res_binary = ft$results$counterfactuals
# 
#   set.seed(354645)
#   iris_pred_bin = iml::Predictor$new(rf, data = X, y = y, class = desired_outcome)
#   ft = FeatureTweaker$new(iris_pred_bin, n_counterfactuals = n, ktree = 2L)
#   ft$find_counterfactuals(x_interest)
#   res_multiclass = ft$results$counterfactuals
# 
#   # For binary pred contains 0/1 (if target is not a factor) and for multiclass the classname
#   res_binary$pred = res_multiclass$pred = NULL
#   expect_identical(res_binary, res_multiclass)
# })
# 
# 
# test_that("`desired_outcome` is required for multiclass", {
#   X = get_scaled_iris_features()
#   y = iris[, ncol(iris)]
#   rf = randomForest::randomForest(X, y, ntree = 20L)
#   x_interest = X[10L, ]
#   n = 2L
#   iris_pred_multiclass = iml::Predictor$new(rf, data = X, y = y)
#   ft_multiclass = FeatureTweaker$new(iris_pred_multiclass, n_counterfactuals = n, ktree = 2L)
#   expect_snapshot_error(ft_multiclass$find_counterfactuals(x_interest))
# })
# 

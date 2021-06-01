# library(randomForest)
# 
# # $plot() ------------------------------------------------------------------------------------------------------
# test_that("$plot() creates correct plot", {
#   skip_on_ci()
#   set.seed(54654654)
#   train_data = data.frame(
#     col_a = rep(c(1, 3), 6L),
#     col_b = rep(1:3, each = 4),
#     col_c = rep(c("x", "y", "z"), each = 2),
#     col_d = as.factor(c(rep("a", 4L), rep("b", 4L), rep("c", 4L)))
#   )
#   x_interest = data.table(col_a = 2, col_b = 1, col_c = "y")
#   rf = randomForest(col_d ~ ., data = train_data)
#   mod = Predictor$new(rf, data = train_data, type = "class", class = "b")
#   arg_list = list(predictor = mod)
#   cfs = data.table(subset(train_data, col_d == "c", -col_d))
#   cfs_diff = data.table(sweep(as.matrix(cfs[, 1:2]), 2L, as.matrix(x_interest[, 1:2])))
#   cfs_diff[, col_c := ifelse(cfs$col_c == x_interest$col_c, 0, cfs$col_c)]
#   cfs[, nr_changed := c(2L, 2L, 3L, 3L)]
#   cfs_diff[, nr_changed := c(2L, 2L, 3L, 3L)]
#   results = list("counterfactuals" = cfs, "counterfactuals_diff" = cfs_diff)
#   param_set_maker = ParamSetMaker$new(train_data)
#   ps = param_set_maker$make_param_set()
#   y_hat_interest = mod$predict(x_interest)
#   
#   feature_names = c("col_a", "col_b")
#   arg_list = list(
#     "feature_names" = feature_names, "grid_size" = 25L, "epsilon" = NULL, "results" = results, "predictor" = mod, 
#     "x_interest" = x_interest, "param_set" = ps, "y_hat_interest" = y_hat_interest, pred_column = mod$class
#   )
#   sp = SurfacePlot$new(arg_list)
#   expect_snapshot_file(save_test_png(sp$plot()), "plot_surface_num.png")
#   
#   feature_names = c("col_a", "col_c")
#   arg_list = list(
#     "feature_names" = feature_names, "grid_size" = 25L, "epsilon" = NULL, "results" = results, "predictor" = mod, 
#     "x_interest" = x_interest, "param_set" = ps, "y_hat_interest" = y_hat_interest, pred_column = mod$class
#   )
#   sp = SurfacePlot$new(arg_list)
#   expect_snapshot_file(save_test_png(sp$plot()), "plot_surface_mixed.png")
# })

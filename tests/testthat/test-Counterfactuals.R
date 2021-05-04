library(data.table)

cfactuals = data.table(
  "col_a" = 1:3,
  "col_b" = c("a", "b", "b"),
  "col_c" = as.factor(month.abb)[1:3]
)
x_interst = data.table("col_a" = 1L, "col_b" = "b", "col_c" = as.factor(month.abb)[3])
cfactuals$dist_x_interest = gower_dist(x_interst, cfactuals, n_cores = 1L)
desired_outcome = "1"
ci = Counterfactuals$new()
ci$.__enclos_env__$private$x_interest = x_interst
ci$.__enclos_env__$private$desired_outcome = 1

test_that("make_results_list methods returns correct output for mixed cf variable types", {
  res = ci$.__enclos_env__$private$make_results_list(cfactuals)
  res_cfs = res$counterfactuals
  res_cfs_diffs = res$counterfactuals_diff
  
  # Output format ----------------------------------------------------------------------------------
  expected_cols = c("col_a", "col_b", "col_c", "dist_x_interest")
  expect_list(res, len = 2L)
  expect_data_table(res_cfs, nrows = nrow(cfactuals))
  expect_data_table(res_cfs_diffs, nrows = nrow(cfactuals))
  expect_true(all(colnames(res_cfs) == expected_cols))
  expect_true(all(colnames(res_cfs_diffs) == expected_cols))
  # Column types res_cfs
  expect_numeric(res_cfs$col_a)
  expect_character(res_cfs$col_b)
  expect_factor(res_cfs$col_c, levels = levels(cfactuals$col_c))
  expect_numeric(res_cfs$dist_x_interest)
  # Column types res_cfs_diffs
  expect_numeric(res_cfs_diffs$col_a)
  expect_character(res_cfs_diffs$col_b)
  expect_character(res_cfs_diffs$col_c)
  expect_numeric(res_cfs_diffs$dist_x_interest)
  
  # Output values ----------------------------------------------------------------------------------
  expect_identical(res_cfs, cfactuals)
  expect_identical(diag(as.matrix(res_cfs_diffs[, 1:3])), rep("0", 3))
  expect_identical(
    as.numeric(as.numeric(x_interst[, "col_a"]) + as.matrix(res_cfs_diffs[, "col_a"])),
    as.numeric(as.matrix(res_cfs[, "col_a"]))
  )
})

test_that("count_changes method computes changes correctly", {
  names_x_interest = names(ci$.__enclos_env__$private$x_interest)
  nr_changed = ci$.__enclos_env__$private$count_changes(cfactuals[, ..names_x_interest])
  expect_identical(nr_changed, c(2L, 2L, 1L))
})


test_that("$plot_surface() creates correct plot", {
  set.seed(54654654)
  train_data = data.frame(
    col_a = rep(c(1, 3), 6L),
    col_b = rep(1:4, 3L),
    col_c = as.factor(rep(c("a", "b", "c"), 4L))
  )
  x_interest = data.table(col_a = 2, col_b = 2)
  rf = randomForest::randomForest(col_c ~ ., data = train_data)
  mod = Predictor$new(rf, data = train_data, type = "class", class = "b")
  cfs = data.table(subset(train_data, col_c == "c", -col_c))
  cfs_diff = data.table(sweep(as.matrix(cfs), 2L, as.matrix(x_interest)))
  
  ci = Counterfactuals$new()
  ci$.__enclos_env__$private$x_interest = x_interest
  ci$.__enclos_env__$private$desired_outcome = 1
  ps = ParamHelpers::makeParamSet(params = make_paramlist(train_data))
  ci$.__enclos_env__$private$param_set = ps
  ci$.__enclos_env__$private$predictor = mod
  res_list = list("counterfactuals" = cfs, "counterfactuals_diff" = cfs_diff)
  nr_changed = ci$.__enclos_env__$private$count_changes(cfs) 
  res_list[[1]]$nr_changed = nr_changed  
  res_list[[2]]$nr_changed = nr_changed
  ci$.__enclos_env__$private$.results = res_list
  
  save_png <- function(code, width = 400, height = 400) {
    path <- tempfile(fileext = ".png")
    png(path, width = width, height = height)
    on.exit(dev.off())
    code
    path
  }
  
  plot = ci$plot_surface(names(data)[1:2])
  expect_snapshot_file(save_png(plot), "plot.png")
  
})





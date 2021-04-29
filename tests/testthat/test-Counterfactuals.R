library(data.table)

test_that("make_results_list methods returns correct output for mixed cf variable types", {
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
  ci$.__enclos_env__$private$desired_outcome = "1"
  res = ci$.__enclos_env__$private$make_results_list(cfactuals)

  res_cfs = res$counterfactuals
  res_cfs_diffs = res$counterfactuals_diff
  
  # Output format ----------------------------------------------------------------------------------
  expected_cols = c(colnames(cfactuals), "nr_changed", "pred")
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
  expect_integer(res_cfs$nr_changed)
  expect_character(res_cfs$pred)
  # Column types res_cfs_diffs
  expect_numeric(res_cfs_diffs$col_a)
  expect_character(res_cfs_diffs$col_b)
  expect_character(res_cfs_diffs$col_c)
  expect_numeric(res_cfs_diffs$dist_x_interest)
  expect_integer(res_cfs_diffs$nr_changed)
  expect_character(res_cfs_diffs$pred)
  
  
  # Output values ----------------------------------------------------------------------------------
  expect_identical(res_cfs[, 1:3], cfactuals[, 1:3])
  expect_identical(res_cfs[, 4:6], res_cfs_diffs[, 4:6])
  expect_identical(diag(as.matrix(res_cfs_diffs[, 1:3])), rep("0", 3))
  expect_identical(res_cfs_diffs$nr_changed, c(2L, 2L, 1L))
  expect_identical(res_cfs_diffs$pred, rep(desired_outcome, 3L))
  expect_identical(
    as.numeric(as.numeric(x_interst[, "col_a"]) + as.matrix(res_cfs_diffs[, "col_a"])),
    as.numeric(as.matrix(res_cfs[, "col_a"]))
  )
})








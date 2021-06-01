test_that("make_results_list methods returns correct output for mixed cf variable types", {
  cfactuals = data.table(
    "col_a" = 1:3, "col_b" = c("a", "b", "b"), "col_c" = as.factor(month.abb)[1:3]
  )
  x_interest = data.table("col_a" = 1L, "col_b" = "b", "col_c" = as.factor(month.abb)[3])
  
  cfactuals_diff = make_cfactuals_diff(cfactuals, x_interest)
  expect_data_table(cfactuals_diff, nrows = nrow(cfactuals), ncols = ncol(cfactuals))
  expect_names(names(cfactuals_diff), identical.to = names(cfactuals))
  expect_identical(diag(as.matrix(cfactuals_diff[, 1:3])), rep("0", 3))
  expect_identical(
    as.numeric(as.numeric(x_interst[, "col_a"]) + as.matrix(cfactuals_diff[, "col_a"])),
    as.numeric(as.matrix(cfactuals[, "col_a"]))
  )
})
# 
# test_that("count_changes method computes changes correctly", {
#   cfactuals = data.table(
#     "col_a" = 1:3, "col_b" = c("a", "b", "b"), "col_c" = as.factor(month.abb)[1:3]
#   )
#   x_interst = data.table("col_a" = 1L, "col_b" = "b", "col_c" = as.factor(month.abb)[3])
#   cfactuals$dist_x_interest = gower_dist(x_interst, cfactuals, n_cores = 1L)
#   ci = Counterfactuals$new()
#   ci$.__enclos_env__$private$x_interest = x_interst
#   names_x_interest = names(ci$.__enclos_env__$private$x_interest)
#   nr_changed = ci$.__enclos_env__$private$count_changes(cfactuals[, ..names_x_interest])
#   expect_identical(nr_changed, c(2L, 2L, 1L))
# })

test_that("make_cfactuals_diff returns correct output for mixed cf variable types", {
  cfactuals = data.table(
    "col_a" = 1:3, "col_b" = c("a", "b", "b"), "col_c" = as.factor(month.abb)[1:3]
  )
  x_interest = data.table("col_a" = 1L, "col_b" = "b", "col_c" = as.factor(month.abb)[3])
  
  cfactuals_diff = make_cfactuals_diff(cfactuals, x_interest)
  expect_data_table(cfactuals_diff, nrows = nrow(cfactuals), ncols = ncol(cfactuals))
  expect_names(names(cfactuals_diff), identical.to = names(cfactuals))
  expect_identical(diag(as.matrix(cfactuals_diff[, 1:3])), rep("NA", 3))
  expect_identical(
    as.numeric(as.numeric(x_interest[, "col_a"]) + as.matrix(cfactuals_diff[, "col_a"])),
    c(NA, as.numeric(as.matrix(cfactuals[, "col_a"]))[2:3])
  )
})

test_that("count_changes method computes changes correctly", {
  cfactuals = data.table("col_a" = 1:3, "col_b" = c("a", "b", "b"), "col_c" = as.factor(month.abb)[1:3])
  x_interest = data.table("col_a" = 1L, "col_b" = "b", "col_c" = as.factor(month.abb)[3])
  nr_changed = count_changes(cfactuals, x_interest)
  expect_identical(nr_changed, c(2L, 2L, 1L))
  
  cfactuals = data.table(a = c(1.5457, 1.0000), b = c("a", "b"))
  x_interest = data.table(a = 1.0000, b = "b")
  expect_identical(count_changes(cfactuals, x_interest), c(2L, 0L))
})

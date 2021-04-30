library(data.table)

cfactuals = data.table(
  "col_a" = 1:3,
  "col_b" = c("a", "b", "b"),
  "col_c" = as.factor(month.abb)[1:3]
)
x_interst = data.table("col_a" = 1L, "col_b" = "b", "col_c" = as.factor(month.abb)[3])
desired_outcome = "1"
ci = Counterfactuals$new()
ci$.__enclos_env__$private$x_interest = x_interst
ci$.__enclos_env__$private$desired_outcome = "1"


test_that("compute_diff method returns correct output", {
  diff = ci$.__enclos_env__$private$compute_diff(cfactuals)
  expect_data_table(diff, nrows = nrow(cfactuals))
  expect_true(all(colnames(diff) == colnames(cfactuals)))
  expect_numeric(diff$col_a)
  expect_character(diff$col_b)
  expect_character(diff$col_c)
  expect_identical(diag(as.matrix(diff)), rep("0", 3))
  expect_identical(
    as.numeric(as.numeric(x_interst[, "col_a"]) + as.matrix(diff[, "col_a"])),
    as.numeric(as.matrix(cfactuals[, "col_a"]))
  )
})

test_that("count_changes method computes changes correctly", {
  nr_changed = ci$.__enclos_env__$private$count_changes(cfactuals)
  expect_identical(nr_changed, c(2L, 2L, 1L))
})









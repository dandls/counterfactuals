test_that("create_candidates creates correct candidates", {
  x_nn = data.table(t(c("a", 0, 10, "ab", 100)))
  x_current = data.table(t(c("a", 5, 10, "xy", 1)))
  diff_x_current = setdiff(x_nn, x_current)
  candidates = create_candidates(x_current, x_nn)
  diff_candidates = apply(candidates, 1L, function(x) length(setdiff(x, x_nn)))
  expect_data_table(candidates, nrows = length(diff_x_current))
  expect_identical(diff_candidates, rep(2L, 3))
})

test_that("dist_to_interval returns correct distances", {
  interval = c(-1, 2)
  x = c(-5.5, 0, 3, NA)
  res = dist_to_interval(x, interval)
  expect_identical(res, c(4.5, 0, 1, NA))
})

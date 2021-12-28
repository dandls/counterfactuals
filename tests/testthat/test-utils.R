# dist_to_interval -----------------------------------------------------------------------------------------------------
test_that("dist_to_interval returns correct distances", {
  interval = c(-1, 2)
  x = c(-5.5, 0, 3, NA)
  res = dist_to_interval(x, interval)
  expect_identical(res, c(4.5, 0, 1, NA))
})

# smallest_n_indices ---------------------------------------------------------------------------------------------------
test_that("smallest_n_indices returns correct indices", {
  x = c(-5.5, 4.8, 0, 3.5, NA)
  res = smallest_n_indices(x, 2)
  expect_identical(res, c(1L, 3L))
})


# eval_distance --------------------------------------------------------------------------------------------------------
test_that("eval_distance checks for correct format of distance matrix", {
  set.seed(1234)
  distance_function = function(x, y, data) {
    dists = StatMatch::gower.dist(x, y)
    if (!is.matrix(dists)) {
      dists = matrix(dists, nrow = nrow(x), ncol = ncol(x))
    }
    dists
  }
  X = matrix(rnorm(10), nrow = 2L, ncol = 5L)
  Y = matrix(rnorm(45), nrow = 9L, ncol = 5L)
  res = eval_distance(distance_function, X, Y)
  expect_matrix(res, mode = "double", nrows = nrow(X), ncols = nrow(Y))
  
  wrong_distance_function = function(x, y, data) {
    matrix("a", nrow = 2L, ncol = 2L)
  }
  expect_snapshot_error(eval_distance(wrong_distance_function, X, Y))
  wrong_distance_function = function(x, y, data) {
    matrix(1.5, nrow = 2L, ncol = 5L)
  }
  expect_snapshot_error(eval_distance(wrong_distance_function, X, Y))
})





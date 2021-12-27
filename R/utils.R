top_n_indices = function(x, n = 1L) {
  sort(as.vector(x), index.return = TRUE)$ix[seq_len(n)]
}


eval_distance = function(distance_function, x, y, data) {
  dist_matrix = distance_function(x, y, data)
  if (!test_matrix(dist_matrix, mode = "double", nrows = nrow(x), ncols = nrow(y))) {
    stop("`distance_function` must return a double matrix with `nrow(x)` rows and `nrow(y)` columns.")
  }
  dist_matrix
}

#' Computes the (absolute, pairwise) distance between the vector elements and an interval
#' 
#' @param x (`numeric()`)\cr
#'   A numeric vector.
#' @param interval (`numeric(2)`)\cr
#'   An interval.  
dist_to_interval = function(x, interval) {
  assert_numeric(x)
  assert_numeric(interval, len = 2, any.missing = FALSE)
  sapply(x, function(z) {
    ifelse(between(z, interval[1L], interval[2L]), 0, min(abs(z - interval)))
  })
}


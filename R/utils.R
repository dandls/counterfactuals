#' Returns the indices of the n smallest elements in a vector
#' 
#' @param x (`numeric()`)\cr
#'   A numeric vector.
#' @param n (`numeric(1)`)\cr
#'   A integer indicating how many elements should be returned
smallest_n_indices = function(x, n = 1L) {
  assert_numeric(x)
  assert_integerish(n)
  if (n > length(x)) {
    stop("Cannot return more elements than are present.")
  }
  sort(as.vector(x), index.return = TRUE)$ix[seq_len(n)]
}

#' Evaluates a distance function and checks for correct output format
#' 
#' @description This function serves as an evaluation wrapper for some distance function. It checks that the output 
#' of `distance_function` is a `numeric` matrix with `nrow(x)` rows and `nrow(y)` columns as expected.
#' 
#' @param distance_function (`function()`)\cr
#'   A distance function to evaluate.
#' @param x (`data.frame() | numeric()`)\cr
#'   A matrix or a data frame containing variables that should be used in the computation of the distance.
#' @param y (`data.frame() | numeric()`)\cr
#'   A matrix or a data.frame containing variables that should be used in the computation of the distance.
#' @param data (`data.frame()` | `NULL`)\cr
#'   A data.frame or data.table containing the entire data set. This can be used to compute statistics used in the 
#'   computation of the distance, e.g., standard deviation or range. 
eval_distance = function(distance_function, x, y, data = NULL) {
  dist_matrix = distance_function(x, y, data)
  if (!"topn" %in% class(distance_function)) {
    if (!test_matrix(dist_matrix, mode = "numeric", nrows = nrow(x), ncols = nrow(y))) {
      stop("`distance_function` must return a `numeric` matrix with `nrow(x)` rows and `nrow(y)` columns.")
    } 
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

quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 


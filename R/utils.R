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
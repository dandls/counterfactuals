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

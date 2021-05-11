#' @import doParallel
gower_dist <- function(x, data, n_cores) {
  
  cl <- parallel::makeCluster(n_cores)
  registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))
  `%dopar%` = foreach::`%dopar%`
  
  # Deactivate multiprocessing in case of too less data
  if (floor(nrow(data) / n_cores) == 0) {
    n_cores = 1
  }
  
  # For numeric variables, build ranges (max-min) to be used in gower-distance.
  # TODO: Use the paramHelpers for the ranges
  get_range_if_numeric = function(x) {
    is_numeric = checkmate::test_numeric(x)
    ifelse(is_numeric, max(x, na.rm = TRUE) - min(x, na.rm = TRUE), NA)
  }
  datax = rbind(data, x)
  ranges = sapply(datax, get_range_if_numeric)
  
  chunk_size = floor(nrow(data) / n_cores)
  dist_vector = foreach::foreach(
    i = 1:n_cores, .packages = c("StatMatch"), .combine = "c", .export = "gower_dist"
  ) %dopar% {
    from = (i - 1) * chunk_size + 1
    to = i * chunk_size
    if (i == n_cores) {
      to = nrow(data)
    }
    dist_vector = as.vector(StatMatch::gower.dist(x, data[from:to,], rngs = ranges))
  }

  dist_vector
}

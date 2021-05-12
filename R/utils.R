#' @import doParallel
gower_dist = function(x, data, n_cores, param_set) {
  
  ranges = gower_dist_ranges(param_set)
  
  cl = parallel::makeCluster(n_cores)
  registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))
  `%dopar%` = foreach::`%dopar%`
  
  # Deactivate multiprocessing in case of too less data
  if (floor(nrow(data) / n_cores) == 0L) {
    n_cores = 1L
  }
  
  chunk_size = floor(nrow(data) / n_cores)
  dist_vector = foreach::foreach(
    i = 1:n_cores, .packages = c("StatMatch"), .combine = "c", .export = "gower_dist"
  ) %dopar% {
    from = (i - 1L) * chunk_size + 1L
    to = i * chunk_size
    if (i == n_cores) {
      to = nrow(data)
    }
    as.vector(StatMatch::gower.dist(x, data[from:to,], rngs = ranges))
  }

  dist_vector
}


gower_dist_ranges = function(param_set) {
  ranges_non_discrete = ParamHelpers::getUpper(param_set) - ParamHelpers::getLower(param_set)
  is_non_discrete = ParamHelpers::getParamTypes(param_set) != "discrete"
  
  ranges = rep(NA, length(param_set$pars))
  ranges[which(is_non_discrete)] = ranges_non_discrete
  
  names(ranges) = ParamHelpers::getParamIds(param_set)
  ranges
}



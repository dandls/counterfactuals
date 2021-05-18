gower_dist = function(x, data, n_cores = 1L, param_set = NULL) {
  ranges = gower_dist_ranges(param_set)
  future::plan(future::multisession, workers = n_cores)
  data_list = split(data, seq(nrow(data)))
  future.apply::future_vapply(data_list, StatMatch::gower.dist, FUN.VALUE = numeric(1L), x, ranges, USE.NAMES = FALSE)
}

gower_dist_ranges = function(param_set) {
  ranges_non_discrete = ParamHelpers::getUpper(param_set) - ParamHelpers::getLower(param_set)
  is_non_discrete = ParamHelpers::getParamTypes(param_set) != "discrete"
  
  ranges = rep(NA, length(param_set$pars))
  ranges[which(is_non_discrete)] = ranges_non_discrete
  
  names(ranges) = ParamHelpers::getParamIds(param_set)
  ranges
}




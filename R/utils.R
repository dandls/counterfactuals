#' Computes the gower distance between an observation `x` and each row of a data set `data`.
#' 
#' @param x (`data.frame(1)`) \cr An observation.
#' @param data (`data.frame()`) \cr A data set. 
#' @param n_cores (`numeric(1)`) \cr The number of cores to be used. 
#' @param param_set ([ParamHelpers::ParamSet]) \cr 
#' Meta information about the features. Used for the `rngs` argument in [StatMatch::gower.dist].
#' 
#' @return ([numeric()]) with the computed distances.
gower_dist = function(x, data, n_cores = 1L, param_set = NULL) {
  checkmate::assert_data_frame(x, nrows = 1L)
  checkmate::assert_data_frame(data, min.rows = 1L)
  if (any(names(x) != names(data))) {
    rlang::abort(c(
      "The gower distance cannot be computed for different features.",
      x = "`x` and `data` must have the same columns.",
      i = waldo::compare(names(x), names(data), x_arg = "x", y_arg = "data", max_diffs = 3L)
    ))
  }
  col_types_x = sapply(x, typeof)
  col_data = sapply(data, typeof)
  if (any(col_types_x != col_data)) {
    rlang::abort(c(
      "The gower distance cannot be computed for different feature types.",
      x = "`x` and `data` must have the same column types.",
      i = waldo::compare(col_types_x, col_data, x_arg = "x", y_arg = "data", max_diffs = 3L)
    ))
  }
  checkmate::assert_integerish(n_cores, lower = 1L, any.missing = FALSE)
  checkmate::assert_class(param_set, "ParamSet")
  
  ranges = gower_dist_ranges(param_set)
  future::plan(future::multisession, workers = n_cores)
  data_list = split(data, seq(nrow(data)))
  future.apply::future_vapply(data_list, StatMatch::gower.dist, FUN.VALUE = numeric(1L), x, ranges, USE.NAMES = FALSE)
}

#' Extract ranges from a `ParamSet` for computing the gower distance.
#' 
#' @description For non-discrete features, the range is set to the difference between its upper and lower bounds. For 
#' discrete features, it is set to `NA` as required by [StatMatch::gower.dist].
#' 
#' @param param_set [[ParamHelpers::ParamSet]].
#' 
#' @return [[numeric()]] Ranges of all features.
gower_dist_ranges = function(param_set) {
  ranges = as.numeric(rep(NA, length(param_set$pars)))
  ranges_num_and_int = ParamHelpers::getUpper(param_set) - ParamHelpers::getLower(param_set)
  is_num_or_int = (ParamHelpers::getParamTypes(param_set) %in% c("numeric", "integer"))
  ranges[which(is_num_or_int)] = ranges_num_and_int
  
  names(ranges) = ParamHelpers::getParamIds(param_set)
  ranges
}


#' Transmit levels of factor variable to parameter set
#'
#' @param levels [[character()]] \cr Character vector of feature class labels.
char_to_factor = function(levels){
  sapply(as.character(levels), function(x) factor(x, levels = levels), simplify = FALSE)
}

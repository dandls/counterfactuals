gower_dist = function(x, y, data) {
  assert_data_table(data)
  ranges = data[, sapply(.SD, function(x) ifelse(is.numeric(x), max(x, na.rm = TRUE) - min(x, na.rm = TRUE), NA))]
  dists = StatMatch::gower.dist(x, y, rngs = ranges, KR.corr = FALSE)
  dists
}


gower_dist_c = function(x, y, data, k, idx = FALSE) {
  if (nrow(y) == 1L) {
    return(gower_dist(x, y, data))
  } else {
    gt = gower_topn(x, y, k)
    if (idx) {
      return(gt$index)
    } else {
      return(t(gt$dist))
    }
  }
}
class(gower_dist_c) = c(class(gower_dist_c), "topn")


# Wrapper for gower::gower_topn
# The original functions have the (for us undesired) behavior: 
# - it skips constant variables and shows a warning
# - it does not handle character variables properly
# We correct for this behavior with custom wrappers. 
gower_topn = function(x, y, n = 5L) {
  
  ch_cols = names(which(sapply(y, is.character)))
  for (ch_col in ch_cols) {
    lev = unique(c(y[[ch_col]], x[[ch_col]]))
    set(y, j = ch_col, value = factor(y[[ch_col]], levels = lev))
    set(x, j = ch_col, value = factor(x[[ch_col]], levels = lev))
  }
  
  myWarnings = NULL
  wHandler = function(w) {
    if (grepl("skipping variable with zero", w$message)) {
      myWarnings <<- c(myWarnings, list(w))
      invokeRestart("muffleWarning")
    } else {
      warning(w$message)
    }
  }
  dist = withCallingHandlers(gower::gower_topn(x, y, n = n, nthread = 1L), warning = wHandler)
  
  if (length(ch_cols) > 0L) {
    y[,(ch_cols) := lapply(.SD, as.character), .SDcols = ch_cols]
    x[,(ch_cols) := lapply(.SD, as.character), .SDcols = ch_cols]
  }
  
  if (length(myWarnings) > 0L) {
    dist$distance = dist$distance * (1 - length(myWarnings) / ncol(x))
  }
  dist$distance[is.na(dist$distance)] = 0
  dist
}

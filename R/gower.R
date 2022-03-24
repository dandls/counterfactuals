gower_dist = function(x, y, data) {
  assert_data_table(data)
  ranges = data[, sapply(.SD, function(x) ifelse(is.numeric(x), max(x, na.rm = TRUE) - min(x, na.rm = TRUE), NA))]
  dists = StatMatch::gower.dist(x, y, rngs = ranges, KR.corr = FALSE)
    if (!is.matrix(dists)) {
      dists = matrix(dists, nrow = nrow(x), ncol = ncol(x))
    }
  dists
}


# Wrapper for gower::gower_dist and gower::gower_topn
# The original functions have the (for us undesired) behavior to skip constant variables and show a warning. 
# We correct for this behavior with custom wrappers
gower_dist_c = function(x, y, data, k) {
  if (nrow(y) == 1L) {
    gower_dist(x, y, data)
  } else {
    gower_topn(x, y, k)
  }
}

gower_topn = function(x, y, n = 5L) {
  myWarnings = NULL
  wHandler = function(w) {
    if (grepl("skipping variable with zero", w$message)) {
      myWarnings <<- c(myWarnings, list(w))
      invokeRestart("muffleWarning")
    } else {
      warning(w$message)
    }
  }
  dist = withCallingHandlers(gower::gower_topn(x, y, n = n, nthread = 1L)$dist, warning = wHandler)
  if (length(myWarnings) > 0L) {
   dist = dist * (1 - length(myWarnings) / ncol(x))
  }
  dist[is.na(dist)] = 0
  t(dist)
}
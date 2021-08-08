# Wrapper for gower::gower_dist and gower::gower_topn
# The original functions have the (for us undesired) behavior to skip constant variables and show a warning. 
# We correct for this behavior with custom wrapppers
gower_dist = function(x, y) {
  myWarnings <- NULL
  wHandler <- function(w) {
    if (grepl("skipping variable with zero", w$message)) {
      myWarnings <<- c(myWarnings, list(w))
      invokeRestart("muffleWarning")
    } else {
      warning(w$message)
    }
  }
  dists <- withCallingHandlers(gower::gower_dist(x, y, nthread = 1L), warning = wHandler)
  if (length(myWarnings) > 0L) {
    dists = dists * (1 - length(myWarnings) / ncol(x))
  }
  dists[is.na(dists)] = 0
  dists
}

gower_topn = function(x, y, n = 5L) {
  myWarnings <- NULL
  wHandler <- function(w) {
    if (grepl("skipping variable with zero", w$message)) {
      myWarnings <<- c(myWarnings, list(w))
      invokeRestart("muffleWarning")
    } else {
      warning(w$message)
    }
  }
  tops <- withCallingHandlers(gower::gower_topn(x, y, n = n, nthread = 1L), warning = wHandler)
  if (length(myWarnings) > 0L) {
    tops$distance = tops$distance * (1 - length(myWarnings) / ncol(x))
  }
  tops$distance[is.na(tops$distance)] = 0
  tops
}



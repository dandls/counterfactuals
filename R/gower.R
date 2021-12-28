# TODO: Speed up
gower_dist = function(x, y, data) {
  assert_data_table(data)
  ranges = data[, sapply(.SD, function(x) ifelse(is.numeric(x), max(x, na.rm = TRUE) - min(x, na.rm = TRUE), NA))]
  dists = StatMatch::gower.dist(x, y, rngs = ranges, KR.corr = FALSE)
    if (!is.matrix(dists)) {
      dists = matrix(dists, nrow = nrow(x), ncol = ncol(x))
    }
  dists
}

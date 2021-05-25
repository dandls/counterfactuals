# 1D Grid
equidistant.grid = function(feature, grid_size, integer = FALSE) {
  if (is.numeric(feature)) {
    feature = feature[is.finite(feature)]
    if (!length(feature)) stop("Feature without any finite values")
    gr <- seq(from = min(feature), to = max(feature), length.out = grid_size)
    if (integer) {
      gr <- unique(as.integer(gr))
    }
    data.frame(grid = gr)
  } else {
    data.frame(grid = unique(feature))
  }
}



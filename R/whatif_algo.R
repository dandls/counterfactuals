whatif_algo = function(predictor, n_cfactuals, x_interest, pred_column, desired_y_hat_range, X_search) {

  y_hat = setDT(predictor$predict(X_search))[[pred_column]]
  X_search = X_search[y_hat %between% desired_y_hat_range]

  if (nrow(X_search) < n_cfactuals) {
    warning(sprintf("Could only find %s counterfactual(s)", nrow(X_search)))
  }
  if (nrow(X_search) == 0L) {
    return(X_search)
  }

  ranges = rep(NA, ncol(X_search))
  names(ranges) = names(X_search)
  idx_numeric_cols = sapply(X_search, is.numeric)
  if (length(idx_numeric_cols) > 0) {
    ranges[idx_numeric_cols] = predictor$data$X[, sapply(.SD, sd, na.rm = TRUE), .SDcols = idx_numeric_cols]
  }
  
  X_list = split(X_search, seq(nrow(X_search)))
  dist_vector = future.apply::future_vapply(
    X_list, StatMatch::gower.dist, FUN.VALUE = numeric(1L), x_interest, rngs = ranges, KR.corr = FALSE, 
    USE.NAMES = FALSE
  )
  
  ix = sort(dist_vector, index.return = TRUE, na.last = TRUE)$ix
  ix_cfactuals = ix[1:min(n_cfactuals, nrow(X_search))]
  
  X_search[ix_cfactuals]
}
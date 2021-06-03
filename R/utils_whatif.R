whatif_algo = function(predictor, param_set, n_cfactuals, x_interest, pred_column, desired_y_hat_range) {
  
  X = predictor$data$X
  y_hat = setDT(predictor$predict(X))[[pred_column]]
  X_search = setDT(X)[y_hat %between% desired_y_hat_range]

  if (nrow(X_search) < n_cfactuals) {
    warning(sprintf("Could only find %s counterfactual(s)", nrow(X_search)))
  }
  if (nrow(X_search) == 0L) {
    return(X_search)
  }
  
  ranges = param_set$upper - param_set$lower 
  X_list = split(X_search, seq(nrow(X_search)))
  dist_vector = future.apply::future_vapply(
    X_list, StatMatch::gower.dist, FUN.VALUE = numeric(1L), x_interest, ranges, USE.NAMES = FALSE
  )
  
  ix = sort(dist_vector, index.return = TRUE, na.last = TRUE)$ix
  ix_cfactuals = ix[1:min(n_cfactuals, nrow(X_search))]
  
  X_search[ix_cfactuals]
}
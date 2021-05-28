whatif_algo = function(X, param_set, n_cfactuals, x_interest, y_hat, desired_y_hat_range) {
  
  X_search = setDT(X)[y_hat %between% desired_y_hat_range]

  if (nrow(X_search) < n_cfactuals) {
    warning(sprintf("Could only find %s counterfactual(s)", nrow(X_search)))
  }
  if (nrow(X_search) == 0) {
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
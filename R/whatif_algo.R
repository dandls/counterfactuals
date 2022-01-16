whatif_algo = function(predictor, n_cfactuals, x_interest, pred_column, desired_y_hat_range, X_search) {

  y_hat = setDT(predictor$predict(X_search))[[pred_column]]
  X_search = X_search[y_hat %between% desired_y_hat_range]
  X_search = unique(X_search)

  if (nrow(X_search) < n_cfactuals) {
    warning(sprintf("Could only find %s counterfactual(s)", nrow(X_search)))
  }
  if (nrow(X_search) == 0L) {
    return(X_search)
  }
  
  idx = gower_topn(x_interest, X_search, n = n_cfactuals)$index
  idx = idx[idx > 0L]
  X_search[idx]
}
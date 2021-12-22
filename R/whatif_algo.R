whatif_algo = function(predictor, n_cfactuals, x_interest, pred_column, desired_y_hat_range, X_search, distance_function) {
  
  if (is.null(distance_function)) {
    distance_function = gower_dist
  }
  
  y_hat = setDT(predictor$predict(X_search))[[pred_column]]
  X_search = X_search[y_hat %between% desired_y_hat_range]

  if (nrow(X_search) < n_cfactuals) {
    warning(sprintf("Could only find %s counterfactual(s)", nrow(X_search)))
  }
  if (nrow(X_search) == 0L) {
    return(X_search)
  }
  
  dist_matrix = distance_function(x_interest, X_search, predictor$data$X)
  if (!test_matrix(dist_matrix, mode = "double")) {
    stop("`distance_function` must return a numeric matrix.")
  }
  idx = sort(as.vector(dist_matrix), index.return = TRUE)$ix[seq_len(n_cfactuals)]
  X_search[idx]
}
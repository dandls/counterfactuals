whatif_algo = function(predictor, n_cfactuals, x_interest, pred_column, desired_y_hat_range, X_search, distance_function) {
  y_hat = setDT(predictor$predict(X_search))[[pred_column]]
  X_search = X_search[y_hat %between% desired_y_hat_range]
  X_search = unique(X_search)

  if (nrow(X_search) < n_cfactuals) {
    warning(sprintf("Could only find %s counterfactual(s)", nrow(X_search)))
  }
  if (nrow(X_search) == 0L) {
    return(X_search)
  }

  dist_matrix = eval_distance(distance_function, x_interest, X_search, predictor$data$X)
  if ("topn" %in% class(distance_function)) {
    idx = c(dist_matrix)
  } else {
    idx = smallest_n_indices(as.vector(dist_matrix), n = n_cfactuals) 
  }
  X_search[idx]
}

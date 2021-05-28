whatif_algo = function(data_X, n_cores, param_set, n_cfactuals, x_interest, y_hat, desired_y_hat_range) {
  data_X_search = setDT(data_X)[y_hat %between% desired_y_hat_range]
  if (nrow(data_X_search) < n_cfactuals) {
    warning(sprintf("Could only find %s counterfactual(s)", nrow(data_X_search)))
  }
  if (nrow(data_X_search) == 0) {
    return(data_X_search)
  }
  
  dist_vector = gower_dist(x_interest, data_X_search, n_cores, param_set)
  indexes = sort(dist_vector, index.return = TRUE, na.last = TRUE)$ix
  indexes_cfactuals = indexes[1:min(n_cfactuals, nrow(data_X_search))]
  
  data_X_search[indexes_cfactuals]
}
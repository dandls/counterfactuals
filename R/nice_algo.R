nice_algo = function(predictor, return_multiple, finish_early, optimization, x_interest, pred_column, 
                      desired_y_hat_range, candidates_x_nn, ae_model, ae_preprocessor, archive, distance_function) {
  
  res_list = list(
    x_nn = NULL, 
    archive = NULL,
    counterfactuals = NULL
  )
  
  y_hat = setDT(predictor$predict(candidates_x_nn))[[pred_column]]
  candidates_x_nn = candidates_x_nn[y_hat %between% desired_y_hat_range]
  
  if (nrow(candidates_x_nn) == 0L) {
    warning("No counterfactuals could be found.")
    res_list$counterfactuals = predictor$data$X[0L]
    return(res_list)
  }
  dist_matrix = eval_distance(distance_function, x_interest, candidates_x_nn, predictor$data$X)
  if ("topn" %in% class(distance_function)) {
    idx = c(dist_matrix)
  } else {
    idx = smallest_n_indices(as.vector(dist_matrix), n = 1L) 
  }
  x_nn = candidates_x_nn[idx]
  x_current = copy(x_interest)
  
  finished = FALSE
  while (!finished) {
    X_candidates = create_candidates(x_current, x_nn)
    f_x_current = predictor$predict(x_current)[pred_column][[1L]]
    f_X_candidates = predictor$predict(X_candidates)[pred_column][[1L]]
    d_f_x_current = dist_to_interval(f_x_current, desired_y_hat_range)
    d_f_X_candidates = dist_to_interval(f_X_candidates, desired_y_hat_range)
    rewards = compute_rewards(
      predictor, optimization, d_f_x_current, d_f_X_candidates, X_candidates, x_interest, x_current, ae_preprocessor, 
      ae_model, distance_function
    )
    x_current = X_candidates[which.max(rewards)]
    archive = c(
      archive,
      list(cbind(X_candidates, "reward" = as.numeric(rewards), predictor$predict(X_candidates)))
    )
    finished = identical(x_current, x_nn) | 
      finish_early & between(predictor$predict(x_current)[[pred_column]], desired_y_hat_range[1L], desired_y_hat_range[2L])
  }
  
  if (return_multiple) {
    # Run backwards through archive and look for candidates whose prediction is in desired_prob
    cfs = lapply(archive, function(iter) {
      in_range = between(iter[[pred_column]], desired_y_hat_range[1L], desired_y_hat_range[2L])
      iter[in_range, names(X_candidates), with = FALSE]
    })
    cfs = unique(rbindlist(cfs))
  } else {
    cfs = x_current
  }

  res_list$x_nn = x_nn
  res_list$archive = archive
  res_list$counterfactuals = cfs
  
  res_list
}


create_candidates = function(x_current, x_nn) {
  diff = which(x_current != x_nn)
  names_diff = names(x_nn)[diff]
  X_candidates = x_current[rep(seq_len(nrow(x_current)), length(diff))]
  for (i in seq(length(diff))) {
    set(X_candidates, i, names_diff[i], value = x_nn[, names_diff[i], with = FALSE])
  }
  X_candidates
}

compute_rewards = function(predictor, optimization, d_f_x_current, d_f_X_candidates, X_candidates, x_interest, x_current, 
                           ae_preprocessor, ae_model, distance_function) {
  
  if (optimization == "sparsity") {
    rewards = d_f_x_current - d_f_X_candidates
  } else if (optimization == "proximity") {
    d_X_candidates = as.vector(eval_distance(distance_function, X_candidates, x_interest, predictor$data$X))
    d_x_current = as.vector(eval_distance(distance_function, x_current, x_interest, predictor$data$X))
    rewards = (d_f_x_current - d_f_X_candidates) / (d_X_candidates - d_x_current + sqrt(.Machine$double.eps))
  } else {
    X_candidates_pp = ae_preprocessor$preprocess(X_candidates)
    x_current_pp = ae_preprocessor$preprocess(x_current)
    ae_pred_X_candidates = ae_model$predict(as.matrix(X_candidates_pp))
    AE_loss_X_candidates = rowMeans((X_candidates_pp - ae_pred_X_candidates)^2)
    ae_pred_x = ae_model$predict(as.matrix(x_current_pp))
    AE_loss_x = rowMeans((x_current_pp - ae_pred_x)^2)
    rewards = (d_f_x_current - d_f_X_candidates) * (AE_loss_x - AE_loss_X_candidates + sqrt(.Machine$double.eps))
  }
  rewards
}


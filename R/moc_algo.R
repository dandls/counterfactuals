moc_algo = function(predictor, x_interest, pred_column, desired_y_hat_range, param_set, lower, upper, sdevs_num_feats, 
                    epsilon,  fixed_features, max_changed, mu, generations, p_rec, p_rec_gen, p_rec_use_orig, p_mut,  
                    p_mut_gen, p_mut_use_orig, k, weights, conditionals, init_strategy, track_infeas) {
  y_hat_interest = predictor$predict(x_interest)
  ref_point = c(min(abs(y_hat_interest - desired_y_hat_range)), 1, ncol(x_interest))
  obj_names = c("dist_target", "dist_x_interest", "nr_changed")   
  
  # adds 4th objective (plausilibty)
  if (track_infeas) {
    ref_point = c(ref_point, 1)
    obj_names = c(obj_names, "dist_train")
  }
  
  # reference coordinate value of "dist_target" objective may be infinite
  if (is.infinite(ref_point[1L])) {
    pred = predictor$predict(predictor$data$X)
    ref_point[1L] = diff(c(min(pred), max(pred))) # maybe just max(pred) - min(pred)?
  }
  
  # Set codomain
  codomain = ParamSet$new(list(
    ParamDbl$new("dist_target", tags = "minimize"),
    ParamDbl$new("dist_x_interest", tags = "minimize"),
    ParamInt$new("nr_changed", tags = "minimize")
  ))
  
  if (track_infeas) {
    codomain$add(ParamDbl$new("dist_train", tags = "minimize"))
  } 
  
  param_range = param_set$upper - param_set$lower
  fitness_function = make_fitness_function(
    predictor, x_interest, param_range, obj_names, pred_column, desired_y_hat_range, track_infeas, weights, k,
    fixed_features
  )
  
  param_set_flex = param_set$clone()
  flex_cols = setdiff(names(x_interest), fixed_features)
  param_set_flex$subset(flex_cols)
  objective = bbotk::ObjectiveRFunDt$new(
    fun = fitness_function,
    domain = param_set_flex,
    codomain = codomain
  )
  
  oi = bbotk::OptimInstanceMultiCrit$new(
    objective, 
    terminator = bbotk::trm("evals", n_evals = generations * 20)
  )

  op_m = make_moc_mutator(param_set_flex, max_changed, sdevs_num_feats, p_mut_gen, p_mut_use_orig)
  op_r = make_moc_recombinator(param_set_flex, max_changed, sdevs_num_feats, p_rec_use_orig)
  # TODO: Replace this by tournament selection
  op_parent = miesmuschel::sel("best")
  # TODO: Is crowding distance included?
  sel_nondom_penalized = ScalorNondomPenalized$new(epsilon)
  op_survival = miesmuschel::sel("best", sel_nondom_penalized)                  
  
  pop_initializer = make_moc_pop_initializer(param_set_flex, x_interest, max_changed, init_strategy, flex_cols,
                                             sdevs_num_feats, lower, upper, predictor)
  miesmuschel::mies_prime_operators(oi$search_space, list(op_m), list(op_r), list(op_parent, op_survival))
  miesmuschel::mies_init_population(oi, mu, initializer = pop_initializer)
  
  tryCatch({
    repeat {
      offspring = miesmuschel::mies_generate_offspring(oi, lambda = 10L, op_parent, op_m, op_r)
      miesmuschel::mies_evaluate_offspring(oi, offspring)
      miesmuschel::mies_survival_plus(oi, mu, op_survival)
    }
  }, terminated_error = function(cond) {
  })
  bbotk::assign_result_default(oi)
  
  # Transform column types w.r.t to original data
  factor_cols = names(predictor$data$X)[sapply(predictor$data$X, is.factor)]
  for (factor_col in factor_cols) {
    oi$result[, (factor_col) := factor(oi$result[[factor_col]], levels = levels(predictor$data$X[[factor_col]]))]
  }
  
  int_cols = names(predictor$data$X)[sapply(predictor$data$X, is.integer)]
  for (int_col in int_cols) {
    oi$result[, (int_col) := as.integer(oi$result[[int_col]])]
  }
  
  # Re-attach fixed features
  if (!is.null(fixed_features)) {
    oi$result[, (fixed_features) := x_interest[, fixed_features, with = FALSE]]
  }
  
  oi$result[, names(x_interest), with = FALSE]
  
}
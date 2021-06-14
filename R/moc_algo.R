moc_algo = function(predictor, x_interest, pred_column, desired_y_hat_range, param_set, lower, upper, sdevs_num_feats, 
                    epsilon,  fixed_features, max_changed, mu, generations, p_rec, p_rec_gen, p_rec_use_orig, p_mut,  
                    p_mut_gen, p_mut_use_orig, k, weights, conditionals, initialization, track_infeas) {
  
  y_hat_interest = predictor$predict(x_interest)
  ref_point = c(min(abs(y_hat_interest - desired_y_hat_range)), 1, ncol(x_interest))
  obj_names = c("dist_target", "dist_x_interest", "nr_changed")          
  n_objectives = 3L
  
  # adds 4th objective (plausilibty)
  if (track_infeas) {
    ref_point = c(ref_point, 1)
    obj_names = c(obj_names, "dist_train")
    n_objectives = n_objectives + 1L
  }
  
  # reference coordinate value of "dist_target" objective may be infinite
  if (is.infinite(ref_point[1L])) {
    pred = predictor$predict(predictor$data$X)
    ref_point[1L] = diff(c(min(pred), max(pred))) # maybe just max(pred) - min(pred)?
  }
  
  param_range = param_set$upper - param_set$lower
  param_range = param_range[names(param_range) != "use_orig"]
  
  # Set codomain
  codomain = ParamSet$new(list(
    ParamDbl$new("dist_target", tags = "minimize"),
    ParamDbl$new("dist_x_interest", tags = "minimize"),
    ParamInt$new("nr_changed", tags = "minimize")
  ))
  
  if (track_infeas) {
    codomain$add(ParamDbl$new("dist_train", tags = "minimize"))
  } 
  
  
  fitness_function = make_fitness_function(
    predictor, x_interest, param_range, obj_names, pred_column, desired_y_hat_range, track_infeas, weights, k
  )
  
  
  objective = bbotk::ObjectiveRFunDt$new(
    fun = fitness_function,
    domain = param_set,
    codomain = codomain
  )
  
  
  oi = bbotk::OptimInstanceMultiCrit$new(
    objective, 
    terminator = bbotk::trm("evals", n_evals = generations)
  )
  
  
  # Mutator
  ops_m_list = list()
  if ("ParamDbl" %in% param_set$class) {
    ops_m_list = c(ops_m_list, "ParamDbl" = miesmuschel::mut("gauss", sdev = sdevs_num_feats))
  }
  if ("ParamLgl" %in% param_set$class) {
    ops_m_list = c(ops_m_list, "ParamLgl" = miesmuschel::mut("unif", can_mutate_to_same = FALSE))
  }
  if ("ParamFct" %in% param_set$class) {
    ops_m_list = c(ops_m_list, "ParamFct" = miesmuschel::mut("unif", can_mutate_to_same = FALSE))
  }
  op_m = miesmuschel::mut("combine", operators = ops_m_list)
  
  # Recombinator
  ops_r_list = list()
  if ("ParamDbl" %in% param_set$class) {
    # TODO: Replace this with "simulated binary crossover recombinator"
    ops_r_list = c(ops_r_list, "ParamDbl" = miesmuschel::rec("xounif", p = p_rec_gen))
  }
  if ("ParamLgl" %in% param_set$class) {
    ops_r_list = c(ops_r_list, "ParamLgl" = miesmuschel::rec("xounif", p = p_rec_gen))
  }
  if ("ParamFct" %in% param_set$class) {
    ops_r_list = c(ops_r_list, "ParamFct" = miesmuschel::rec("xounif", p = p_rec_gen))
  }
  op_r = miesmuschel::rec("combine", operators = ops_r_list)
  
  # TODO: Replace this by tournament selection
  op_parent = miesmuschel::sel("best")
  # TODO: Is crowding distance included?
  op_survival = miesmuschel::sel("best", miesmuschel::scl("nondom"))                                 
  
  
  mies = bbotk::opt(
    "mies", 
    mutator = op_m, 
    recombinator = op_r, 
    parent_selector = op_parent, 
    survival_selector = op_survival,
    mu = mu, 
    lambda = 10L
  )
  
  mies$optimize(oi)
  factor_cols = names(predictor$data$X)[sapply(predictor$data$X, is.factor)]
  for (factor_col in factor_cols) {
    oi$result[, (factor_col) := factor(oi$result[[factor_col]], levels = levels(predictor$data$X[[factor_col]]))]
  }
  oi$result[, names(x_interest), with = FALSE]
  
}
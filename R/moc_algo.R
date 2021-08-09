moc_algo = function(predictor, x_interest, pred_column, target, param_set, lower, upper, sdevs_num_feats, 
                    epsilon,  fixed_features, max_changed, mu, n_generations, p_rec, p_rec_gen, p_rec_use_orig,
                    p_mut, p_mut_gen, p_mut_use_orig, k, weights, init_strategy, cond_sampler = NULL) {
  
  codomain = ParamSet$new(list(
    ParamDbl$new("dist_target", tags = "minimize"),
    ParamDbl$new("dist_x_interest", tags = "minimize"),
    ParamInt$new("nr_changed", tags = "minimize"),
    ParamDbl$new("dist_train", tags = "minimize")
  ))
  
  fitness_function = make_fitness_function(
    predictor, x_interest, pred_column, target, weights, k, fixed_features, param_set
  )
  
  flex_cols = setdiff(names(x_interest), fixed_features)
  sdevs_flex_num_feats = sdevs_num_feats[names(sdevs_num_feats) %in% flex_cols]
  param_set_flex = param_set$clone()
  param_set_flex$subset(flex_cols)
  
  objective = bbotk::ObjectiveRFunDt$new(
    fun = fitness_function, 
    domain = param_set_flex, 
    codomain = codomain
  )
  
  oi = bbotk::OptimInstanceMultiCrit$new(
    objective, 
    terminator = bbotk::trm("gens", generations = n_generations)
  )
  
  if (is.null(cond_sampler)) {
    op_m = make_moc_mutator(
      ps = param_set_flex, 
      x_interest = x_interest, 
      max_changed = max_changed, 
      sdevs = sdevs_flex_num_feats, 
      p_mut = p_mut,
      p_mut_gen = p_mut_gen, 
      p_mut_use_orig = p_mut_use_orig
    )
  } else {
    op_m = make_moc_conditional_mutator(
      ps = param_set_flex, 
      x_interest = x_interest,
      max_changed = max_changed, 
      p_mut = p_mut,
      p_mut_gen = p_mut_gen, 
      p_mut_use_orig = p_mut_use_orig,
      cond_sampler = cond_sampler
    )
  }
  
  
  op_r = make_moc_recombinator(
    ps = param_set_flex, 
    x_interest = x_interest, 
    max_changed = max_changed, 
    p_rec = p_rec,
    p_rec_gen = p_rec_gen, 
    p_rec_use_orig = p_rec_use_orig
  )

  # TODO: Replace this by tournament selection
  op_parent = sel("best")
  
  sel_nondom_penalized = ScalorNondomPenalized$new(epsilon)
  op_survival = sel("best", sel_nondom_penalized)   
  
  
  pop_initializer = make_moc_pop_initializer(
    ps = param_set_flex, 
    x_interest = x_interest, 
    max_changed = max_changed, 
    init_strategy = init_strategy, 
    flex_cols = flex_cols, 
    sdevs = sdevs_flex_num_feats, 
    lower = lower, 
    upper = upper, 
    predictor = predictor,
    fitness_function = fitness_function,
    mu = mu
  )
  
  mies_prime_operators(
    search_space = oi$search_space, 
    mutators = list(op_m), 
    recombinators = list(op_r),
    selectors = list(op_parent, op_survival)
  )

  mies_init_population(
    inst = oi, 
    mu = mu, 
    initializer = pop_initializer
  )
  
  tryCatch({
    repeat {
      offspring = mies_generate_offspring(oi, lambda = mu, op_parent, op_m, op_r)
      mies_evaluate_offspring(oi, offspring)
      mies_survival_plus(oi, mu, op_survival)
    }
  }, terminated_error = function(cond) {
  })
  bbotk::assign_result_default(oi)
  
  # Re-attach fixed features
  if (!is.null(fixed_features)) {
    oi$result[, (fixed_features) := x_interest[, fixed_features, with = FALSE]]
  }
  
  # Transform factor column w.r.t to original data
  factor_cols = names(which(sapply(predictor$data$X, is.factor)))
  for (factor_col in factor_cols) {
    fact_col_pred = predictor$data$X[[factor_col]]
    value =  factor(oi$result[[factor_col]], levels = levels(fact_col_pred), ordered = is.ordered(fact_col_pred))
    oi$result[, (factor_col) := value]
  }
  
  int_cols = names(which(sapply(predictor$data$X, is.integer)))
  if (length(int_cols) > 0L) {
    oi$result[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
  }
  
  setorder(oi$result, dist_target)
  oi
}


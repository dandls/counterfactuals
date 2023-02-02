moc_algo = function(predictor, x_interest, pred_column, target, param_set, lower, upper, sdevs_num_feats, 
  epsilon,  fixed_features, max_changed, mu, termination_crit, n_generations, p_rec, p_rec_gen,
  p_mut, p_mut_gen, p_mut_use_orig, k, weights, init_strategy, distance_function, cond_sampler = NULL, 
  ref_point, quiet = TRUE) {
  
  codomain = ParamSet$new(list(
    ParamDbl$new("dist_target", tags = "minimize"),
    ParamDbl$new("dist_x_interest", tags = "minimize"),
    ParamInt$new("no_changed", tags = "minimize"),
    ParamDbl$new("dist_train", tags = "minimize")
  ))
  
  fitness_function = make_fitness_function(
    predictor, x_interest, pred_column, target, weights, k, fixed_features, param_set, distance_function
  )
  
  flex_cols = setdiff(names(x_interest), fixed_features)
  if (!is.null(sdevs_num_feats)) {
    sdevs_flex_num_feats = sdevs_num_feats[names(sdevs_num_feats) %in% flex_cols]
  }
  
  param_set_flex = param_set$clone()
  param_set_flex$subset(flex_cols)
  
  objective = bbotk::ObjectiveRFunDt$new(
    fun = fitness_function, 
    domain = param_set_flex, 
    codomain = codomain
  )
  
  if (n_generations > 0L) {
    if (termination_crit == "gens") {
      terminator = bbotk::trm("gens", generations = n_generations)
    } else if (termination_crit == "genstag") {
      terminator = bbotk::trm("combo",
        list(
          bbotk::trm("genstag",
            fitness_aggregator = function(fitnesses) {
              domhv(-as.matrix(fitnesses), nadir = -ref_point)
            },
            include_previous_generations = TRUE,
            min_delta = 0.00,
            patience = n_generations), 
          bbotk::trm("gens", generations = 500)
        )
      )
    }
  } else {
    terminator = bbotk::trm("none")
  }
  
  oi = bbotk::OptimInstanceMultiCrit$new(
    objective, 
    terminator = terminator
  )
  
  if (n_generations > 0L) {
    # Mutator
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
    
    # Recombinator
    op_r = make_moc_recombinator(
      ps = param_set_flex, 
      x_interest = x_interest, 
      max_changed = max_changed, 
      p_rec = p_rec,
      p_rec_gen = p_rec_gen
    )
    
    # Selectors
    # TODO: Replace this by tournament selection
    selobj1 = scl("one", objective = 1L)
    op_parent = sel("best", selobj1)
    
    sel_nondom_penalized = ScalorNondomPenalized$new(epsilon)
    op_survival = sel("best", sel_nondom_penalized)   
    
    mies_prime_operators(
      search_space = oi$search_space, 
      mutators = list(op_m), 
      recombinators = list(op_r),
      selectors = list(op_parent, op_survival)
    )  
  }
  
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
  
  if (quiet) {
    quiet(mies_init_population(inst = oi, mu = mu, initializer = pop_initializer))
  } else {
    mies_init_population(inst = oi, mu = mu, initializer = pop_initializer)
  }
  
  if (n_generations > 0L) {
    tryCatch({
      repeat {
        # warning handling
        withCallingHandlers(
          offspring <- mies_generate_offspring(oi, lambda = mu, op_parent, op_m, op_r),
          warning = function(w){
            if(grepl("no columns to delete or assign RHS to", w$message)){
              invokeRestart("muffleWarning")
            } 
          })
        if (quiet) {
          quiet(mies_evaluate_offspring(oi, offspring))
        } else {
          mies_evaluate_offspring(oi, offspring)
        }
        mies_survival_plus(oi, mu, op_survival)
      }
    }, terminated_error = function(cond) {
    })
  }
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
  setorder(oi$result, "dist_target")
  oi
}


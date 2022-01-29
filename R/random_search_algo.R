random_search_algo = function(predictor, x_interest, pred_column, target, param_set, lower, upper, 
                              fixed_features, max_changed, n_samples, k, weights, p_use_orig) {
  
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
  param_set_flex = param_set$clone()
  param_set_flex$subset(flex_cols)
  
  objective = bbotk::ObjectiveRFunDt$new(
    fun = fitness_function, 
    domain = param_set_flex, 
    codomain = codomain
  )
  
  oi = bbotk::OptimInstanceMultiCrit$new(
    objective, 
    terminator = bbotk::trm("none")
  )
  
  pop_initializer = make_moc_pop_initializer(
    ps = param_set_flex, 
    x_interest = x_interest, 
    max_changed = max_changed, 
    init_strategy = "random", 
    flex_cols = flex_cols, 
    sdevs = NULL, 
    lower = lower, 
    upper = upper, 
    predictor = predictor,
    fitness_function = fitness_function,
    mu = n_samples,
    p_use_orig = p_use_orig
  )

  quiet(mies_init_population(inst = oi, mu = n_samples, initializer = pop_initializer))
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


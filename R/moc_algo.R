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
  
  param_set_flex = param_set$clone()
  flex_cols = setdiff(names(x_interest), fixed_features)
  param_set_flex$subset(flex_cols)
  
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
  
  objective = bbotk::ObjectiveRFunDt$new(
    fun = fitness_function,
    domain = param_set_flex,
    codomain = codomain
  )
  
  oi = bbotk::OptimInstanceMultiCrit$new(
    objective, 
    terminator = bbotk::trm("evals", n_evals = generations * 10)
  )
  
  # Mutator
  ops_m_list = list()
  if ("ParamDbl" %in% param_set_flex$class) {
    ops_m_list[["ParamDbl"]] = miesmuschel::mut(
      "maybe", miesmuschel::mut("gauss", sdev = sdevs_num_feats), miesmuschel::mut("null"), p = p_mut_gen
    )
  }
  if ("ParamInt" %in% param_set_flex$class) {
    ops_m_list[["ParamInt"]] = miesmuschel::mut(
      "maybe", miesmuschel::mut("gauss"), miesmuschel::mut("null"), p = p_mut_gen
    )
  }
  if ("ParamFct" %in% param_set_flex$class) {
    n_facts = sum("ParamFct" == param_set_flex$class)
    ls_op_factor = rep(list(miesmuschel::mut("unif", can_mutate_to_same = FALSE)), n_facts)
    names(ls_op_factor) = param_set_flex$ids()[which(param_set_flex$class == "ParamFct")]
    ops_m_list = c(ops_m_list, ls_op_factor)
  }
  
  op_m_seq1 = miesmuschel::mut("combine", operators = ops_m_list)
  op_m_seq2 = MutatorReset$new(x_interest, p_mut_use_orig, max_changed)
  op_m = miesmuschel::mut("sequential", list(op_m_seq1, op_m_seq2))

  
  # Recombinator
  # TODO: DO we need probs here?
  ops_r_list = list()
  if ("ParamDbl" %in% param_set_flex$class) {
    # TODO: Replace this with "simulated binary crossover recombinator"
    ops_r_list[["ParamDbl"]] = miesmuschel::rec("xounif")
  }
  
  if ("ParamInt" %in% param_set_flex$class) {
    ops_r_list[["ParamInt"]] = miesmuschel::rec("xounif")
  }
  
  if ("ParamFct" %in% param_set_flex$class) {
    n_facts = sum("ParamFct" == param_set_flex$class)
    ls_op_factor = rep(list(miesmuschel::rec("xounif")), n_facts)
    names(ls_op_factor) = names(param_set_flex$class)[which(param_set_flex$class == "ParamFct")]
    ops_r_list = c(ops_r_list, ls_op_factor)
  }
  op_r_seq_1 = miesmuschel::rec("combine", operators = ops_r_list)
  op_r_seq_2 = RecombinatorReset$new(x_interest, p_mut_use_orig, max_changed)
  op_r = miesmuschel::rec("sequential", list(op_r_seq_1, op_r_seq_2))
  
  # TODO: Replace this by tournament selection
  op_parent = miesmuschel::sel("best")
  # TODO: Is crowding distance included?
  op_survival = miesmuschel::sel("best", miesmuschel::scl("nondom"))                                 
  
  if (is.null(max_changed)) {
    pop_initializer = paradox::generate_design_random
  } else {
    pop_initializer = function(param_set, n) {
      my_design = paradox::SamplerUnif$new(param_set_flex)$sample(n)
      x_interest_reorderd = x_interest[, names(my_design$data), with = FALSE]
      n_changes = count_changes(my_design$data, x_interest_reorderd)
      
      for (i in 1:nrow(my_design$data)) {
        if (n_changes[i] > max_changed) {
          pos_diff = which(my_design$data[i, ] != x_interest_reorderd)
          to_be_reverted = sample(pos_diff, size = n_changes[i] - max_changed)
          to_be_reverted = names(my_design$data)[to_be_reverted]
          my_design$data[i, (to_be_reverted) := x_interest_reorderd[, to_be_reverted, with = FALSE]]
        }
      }
      
      my_design
    }
  }
  
  miesmuschel::mies_prime_operators(oi$search_space, list(op_m), list(op_r), list(op_parent, op_survival))
  miesmuschel::mies_init_population(oi, mu, initializer = pop_initializer)
  offspring = miesmuschel::mies_generate_offspring(oi, lambda = 10L, op_parent, op_m, op_r)
  miesmuschel::mies_evaluate_offspring(oi, offspring)
  miesmuschel::mies_survival_plus(oi, mu, op_survival)
  
  tryCatch({
    repeat {
      offspring = miesmuschel::mies_generate_offspring(oi, lambda = 10L, op_parent, op_m, op_r)
      miesmuschel::mies_evaluate_offspring(oi, offspring)
      miesmuschel::mies_survival_plus(oi, mu, op_survival)
    }
  }, terminated_error = function(cond) {
  })
  bbotk::assign_result_default(oi)
  
  factor_cols = names(predictor$data$X)[sapply(predictor$data$X, is.factor)]
  for (factor_col in factor_cols) {
    oi$result[, (factor_col) := factor(oi$result[[factor_col]], levels = levels(predictor$data$X[[factor_col]]))]
  }
  int_cols = names(predictor$data$X)[sapply(predictor$data$X, is.integer)]
  for (int_col in int_cols) {
    oi$result[, (int_col) := as.integer(oi$result[[int_col]])]
  }
  
  if (!is.null(fixed_features)) {
    oi$result[, (fixed_features) := x_interest[, fixed_features, with = FALSE]]
  }
  
  oi$result[, names(x_interest), with = FALSE]
  
}
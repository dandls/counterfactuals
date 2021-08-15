make_fitness_function = function(predictor, x_interest, pred_column, target, weights, k, fixed_features, param_set) {
  
  function(xdt) {
    # Add values of fixed_features just for prediction
    if (!is.null(fixed_features)) {
      xdt[, (fixed_features) := x_interest[, fixed_features, with = FALSE]]
    }
    xdt = xdt[, names(x_interest), with = FALSE]
    factor_cols = names(which(sapply(predictor$data$X, is.factor)))
    for (factor_col in factor_cols) {
      fact_col_pred = predictor$data$X[[factor_col]]
      value =  factor(xdt[[factor_col]], levels = levels(fact_col_pred), ordered = is.ordered(fact_col_pred))
      set(xdt, j = factor_col, value = value)
    }
    int_cols = names(which(sapply(predictor$data$X, is.integer)))
    if (length(int_cols) > 0L) {
      xdt[,(int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
    }
    pred = predictor$predict(xdt)[[pred_column]]
    
    dist_target = sapply(pred, function(x) ifelse(between(x, target[1L], target[2L]), 0, min(abs(x - target))))
    ranges = param_set$upper - param_set$lower
    dist_x_interest = as.vector(StatMatch::gower.dist(x_interest, xdt, rngs = ranges, KR.corr = FALSE))
    nr_changed = rowSums(xdt != x_interest[rep(seq_len(nrow(x_interest)), nrow(xdt)), ])
    dist_train = gower_topn(x = xdt, y = predictor$data$X, n = k)$distance
    if (!is.null(weights)) {
      dist_train = apply(dist_train, 2L, weighted.mean, w = weights)
    } else {
      dist_train = apply(dist_train, 2L, mean)
    }
    data.table(cbind(dist_target, dist_x_interest, nr_changed, dist_train))
  }
}

# Reset mutated feature values to feature value of x_interest with prop `p_use_orig` and controls that maximum
# `max_changed` features are changed
MutatorReset = R6::R6Class("MutatorReset", inherit = Mutator,
  public = list(
    initialize = function(x_interest, p_use_orig, max_changed) {
      assert_data_table(x_interest)
      assert_numeric(p_use_orig, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
      assert_integerish(max_changed, lower = 0, len = 1L, any.missing = FALSE, null.ok = TRUE)

      params = ps(
        max_changed = p_int(special_vals = list(NULL)),
        p_use_orig = p_dbl()
      )
      params$values = list(
        "max_changed" = max_changed,
        "p_use_orig" = p_use_orig
      )
      super$initialize(param_set = params)
      private$.x_interest = x_interest
    },
    
    prime = function(param_set) {
      super$prime(param_set)
    }
  ),
  private = list(
    .x_interest = NULL,
    
    .mutate = function(values, context) {
      params = self$param_set$get_values(context = context)
      reset_columns(values, params$p_use_orig, params$max_changed, private$.x_interest)
    }
  )
)



# Reset recombinated values to feature value of x_interest with prop `p_use_orig` and controls that maximum
# `max_changed` features are changed
RecombinatorReset = R6::R6Class("RecombinatorReset", inherit = Recombinator,
  
  public = list(
    initialize = function(x_interest, p_use_orig, max_changed) {
      assert_data_table(x_interest)
      assert_numeric(p_use_orig, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
      assert_integerish(max_changed, lower = 0, len = 1L, any.missing = FALSE, null.ok = TRUE)
      params = ps(
        max_changed = p_int(special_vals = list(NULL)),
        p_use_orig = p_dbl()
      )
      params$values = list(
        "max_changed" = max_changed,
        "p_use_orig" = p_use_orig
      )
      super$initialize(param_set = params)
      private$.x_interest = x_interest
    }
  ),
  
  private = list(
    .x_interest = NULL,

    .recombine = function(values, context) {
      params = self$param_set$get_values(context = context)
      reset_columns(values, params$p_use_orig, params$max_changed, private$.x_interest)
    }
  )
)

# Resets columns of `values` to feature value of `x_interest` with prop `p_use_orig` and controls that maximum
# `max_changed` features are changed
reset_columns = function(values, p_use_orig, max_changed, x_interest) {
  values_reset = copy(values)

  # Removes fixed features from x_interest, if present, as only flex features are contained in values_reset
  x_interest_sub = x_interest[, names(values_reset), with = FALSE]
  
  factor_cols = which(sapply(x_interest_sub, is.factor))
  if (length(factor_cols) > 0L) {
    x_interest_sub[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
  }
  
  for (i in seq_len(nrow(values_reset))) {
    # Reset columns with prob p_use_orig
    idx_reset = which(sample(
      c(TRUE, FALSE), size = ncol(values_reset), replace = TRUE, prob = c(p_use_orig, 1 - p_use_orig)
    ))
    
    if (length(idx_reset) > 0L) {
      set(values_reset, i, j = idx_reset, value = x_interest_sub[, ..idx_reset])
    }

    # If more changes than allowed, randomly reset some features such that constraint holds
    n_changes = count_changes(values_reset[i, ], x_interest_sub)
    if (!is.null(max_changed)) {
      if (n_changes > max_changed) {
        idx_diff = which(values_reset[i, ] != x_interest_sub)
        idx_reset = sample(idx_diff, size = n_changes - max_changed)
        set(values_reset, i, j = idx_reset, value = x_interest_sub[, ..idx_reset])
      }
    }
  }
  values_reset
}

# Slightly adapted from miesmuschel::ScalorNondom
# Penalizes candidates whose distance between their prediction and target exceeds epsilon by moving them up by shifting 
# them up by to the last fronts.
ScalorNondomPenalized = R6::R6Class("ScalorNondomPenalized", inherit = Scalor,
  
  public = list(
    initialize = function(epsilon) {
      param_set = ps(
        epsilon = p_dbl(lower = 0, tags = "required", special_vals = list(NULL)),
        scale_output = p_lgl(tags = "required"),
        jitter = p_lgl(tags = "required"),
        tiebreak = p_fct("crowding-dist")
      )
      param_set$values = list(epsilon = epsilon, scale_output = FALSE, jitter = TRUE, tiebreak = "crowding-dist")
      super$initialize(param_set = param_set)
    }
  ),
  private = list(
    .scale = function(values, fitnesses, context) {

      params = self$param_set$get_values(context = context)
      if (params$jitter) {
        fitnesses = fitnesses * (1 + runif(length(fitnesses)) * sqrt(.Machine$double.eps))
      }
      sorted = order_nondominated(fitnesses)$fronts
      
      # Add penalization for individuals with -dist_target lower than -epsilon
      epsilon = params$epsilon
      if (is.null(epsilon)) epsilon = Inf
      is_penalized = fitnesses[colnames(fitnesses) == "dist_target"] < -epsilon
      sorted[is_penalized] = max(sorted) + order(fitnesses[colnames(fitnesses) == "dist_target"][is_penalized])
      front_indexes = sort(unique(sorted))
      fronts = lapply(split(as.data.frame(fitnesses), sorted), as.matrix)
      subranks = lapply(fronts, function(x) rank(dist_crowding_custom(x, values)) / (length(x) + 1))
      for (i in seq_along(subranks)) {
        sr = subranks[[i]]
        # There may be empty fronts in very few cases due to penalization. Therefore this is slightly adapted from 
        # original ScalorNondom
        front_index = front_indexes[i]
        sorted[sorted == front_index] = front_index + sr
      }
      
      # want high front values for high fitnesses, so reverse ordering here
      sorted = max(sorted) + 1 - sorted 
    }
  )
)

# Computes crowding distance in target and feature space
# source: https://github.com/susanne-207/moc/blob/master/counterfactuals/R/generate_counterfactuals.R#L237
dist_crowding_custom = function(fitnesses, candidates) {
  
  assert_matrix(fitnesses, mode = "numeric", any.missing = FALSE, min.cols = 1, min.rows = 1)
  assert_data_table(candidates, min.rows = 1L)
  
  fitnesses = t(fitnesses)
  candidates_this_front = candidates[as.numeric(colnames(fitnesses))]
  
  n = ncol(fitnesses)
  max = apply(fitnesses, 1L, max)
  min = apply(fitnesses, 1L, min)
  ods = dds = cds = numeric(n)
  
  g_dist = StatMatch::gower.dist(candidates_this_front, KR.corr = FALSE)
  
  for (i in seq_len(4L)) {
    # get the order of the points when sorted according to the i-th objective
    ord = order(fitnesses[i, ])
    
    # set the extreme values to Inf
    ods[ord[c(1L, n)]] = dds[ord[c(1L, n)]] = Inf
    
    # update the remaining crowding numbers
    if (n > 2L) {
      for (j in 2:(n - 1L)) {
        ods[ord[j]] = ods[ord[j]] + (fitnesses[i, ord[j + 1L]] - fitnesses[i, ord[j - 1L]]) / (max[i] - min[i])
        dds[ord[j]] = dds[ord[j]] + g_dist[ord[j], ord[j - 1L]] + g_dist[ord[j], ord[j + 1L]]
      }
    }
  }
  
  cds = rank(ods, ties.method = "random") + rank(dds, ties.method = "random")
  jitter(cds, factor = 1L)
}


make_moc_mutator = function(ps, x_interest, max_changed, sdevs, p_mut, p_mut_gen, p_mut_use_orig) {
  ops_list = list()
 
  ids_param_num = names(which(ps$is_number))
  for (id in ids_param_num) {
    ops_list[[id]] = mut("maybe", mut("gauss", sdev = sdevs[[id]], truncated_normal = TRUE), mut("null"), p = p_mut_gen)
  }

  if ("ParamFct" %in% ps$class) {
    idx_facts = which("ParamFct" == ps$class)
    mut_maybe_unif = mut("maybe", mut("unif", can_mutate_to_same = FALSE), mut("null"), p = p_mut_gen)
    ls_op_factor = rep(list(mut_maybe_unif), length(idx_facts))
    names(ls_op_factor) = ps$ids()[idx_facts]
    ops_list = c(ops_list, ls_op_factor)
  }
  
  op_seq1_mut = mut("combine", operators = ops_list)
  op_seq1_no_mut = mut("null")
  op_seq1 = mut("maybe", op_seq1_mut, op_seq1_no_mut, p = p_mut)
  
  op_seq2 = MutatorReset$new(x_interest, p_mut_use_orig, max_changed)
  mut("sequential", list(op_seq1, op_seq2))
}


make_moc_recombinator = function(ps, x_interest, max_changed, p_rec, p_rec_gen, p_rec_use_orig) {

  ops_list = list()
  # If clauses are necessary to avoid warning that no corresponding dimensions
  if ("ParamDbl" %in% ps$class) {
    # TODO: Replace this with "simulated binary crossover recombinator" once it is available
    ops_list[["ParamDbl"]] = rec("maybe", rec("xounif"), rec("null", n_indivs_in = 2L, n_indivs_out = 2L), p = p_rec)
  }
  rec_fact_int = rec("maybe", rec("xounif"), rec("null", n_indivs_in = 2L, n_indivs_out = 2L), p = p_rec)
  if ("ParamInt" %in% ps$class) {
    ops_list[["ParamInt"]] = rec_fact_int
  }
  if ("ParamFct" %in% ps$class) {
    ops_list[["ParamFct"]] = rec_fact_int
  }
  
  op_seq1_rec = rec("combine", operators = ops_list)
  op_seq1_no_rec = rec("null", n_indivs_in = 2L, n_indivs_out = 2L)
  op_r_seq_1 = rec("maybe", op_seq1_rec, op_seq1_no_rec, p = p_rec_gen)
  op_r_seq_2 = RecombinatorReset$new(x_interest, p_rec_use_orig, max_changed)
  rec("sequential", list(op_r_seq_1, op_r_seq_2))
}


make_moc_pop_initializer = function(ps, x_interest, max_changed, init_strategy, flex_cols, sdevs, lower, upper, 
                                    predictor, fitness_function, mu) {
  function(param_set, n) {
    
    if (init_strategy == "random") {
      f_design = function(ps, n) {
        mydesign = SamplerUnif$new(ps)$sample(n)
        mydesign$data = reset_columns(mydesign$data, p_use_orig = 0.5, max_changed = 1e15, x_interest = x_interest)
        mydesign
      }
    } else if (init_strategy == "sd") {
      if (length(sdevs) == 0L) {
        f_design = function(ps, n) {
          mydesign = SamplerUnif$new(ps)$sample(n)
          mydesign$data = reset_columns(mydesign$data, p_use_orig = 0.5, max_changed = 1e15, x_interest = x_interest)
          mydesign
        }
      } else {
        make_f_design = function(X, x_interest, sdevs, lower, upper) {
          x_interest_num = x_interest[, names(sdevs), with = FALSE]
          lower_bounds = pmax(ps$lower[names(sdevs)], x_interest_num - sdevs)
          upper_bounds = pmin(ps$upper[names(sdevs)], x_interest_num + sdevs)
          lower_bounds[names(lower)] = lower
          upper_bounds[names(upper)] = upper

          param_set_init = make_param_set(X, lower = lower_bounds, upper = upper_bounds)
          function(ps, n) {
            mydesign = SamplerUnif$new(param_set_init)$sample(n)
            mydesign$data = reset_columns(mydesign$data, p_use_orig = 0.5, max_changed = 1e15, x_interest = x_interest)
            mydesign
          }
        }

        sdevs_flex_cols = sdevs[names(sdevs) %in% flex_cols]
        lower_flex_cols = lower[names(lower) %in% flex_cols]
        upper_flex_cols = upper[names(upper) %in% flex_cols]
        f_design = make_f_design(
          predictor$data$X[, ..flex_cols], x_interest[, ..flex_cols], sdevs_flex_cols, lower_flex_cols, upper_flex_cols
        )
      }
    } else if (init_strategy == "icecurve") {

      make_f_design = function(X, flex_cols, x_interest, sdevs_num_feats) {
        function(ps, n) {
          param_set = make_param_set(X, lower = NULL, upper = NULL)
          mydesign = SamplerUnif$new(param_set)$sample(n)

          ice_sds = get_ICE_sd(x_interest, predictor, param_set)
          
          # p_min and p_max set to default values stated in MOC paper
          p_min = 0.01
          p_max = 0.99
          p_differs = (ice_sds - min(ice_sds)) * (p_max - p_min) / 
            (max(ice_sds) - min(ice_sds) + sqrt(.Machine$double.eps)) + p_min

          x_interest_sub = copy(x_interest)
          fixed_cols = which(!names(mydesign$data) %in% flex_cols)
          if (length(fixed_cols) > 0L) {
            mydesign$data[, (fixed_cols) := NULL]
            x_interest_sub = x_interest_sub[, ..flex_cols]
          }

          factor_cols = names(x_interest_sub)[sapply(x_interest_sub, is.factor)]
          if (length(factor_cols) > 0L) {
            x_interest_sub[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
          }
          
          for (j in seq_len(ncol(mydesign$data))) {
            p = p_differs[j]
            reset = which(sample(c(TRUE, FALSE), size = nrow(mydesign$data), replace = TRUE, prob = c(1 - p, p)))
            set(mydesign$data, reset, j, x_interest_sub[[j]])
          }
          mydesign
        }
      }

      f_design = make_f_design(predictor$data$X, flex_cols, x_interest, sdevs_num_feats)
    } else if (init_strategy == "traindata") {
      
      make_f_design = function(X, flex_cols, x_interest, sdevs_num_feats) {
        function(ps, n) {
      
          X_sub = predictor$data$X[sample.int(nrow(predictor$data$X), 200L, replace = TRUE)]
          for (rx in seq_len(nrow(X_sub))) {
            use_orig_feats = sample.int(ncol(x_interest), 1L) - 1
            use_orig = seq_len(ncol(x_interest)) <= use_orig_feats
            X_sub[rx] = reset_columns(X_sub[rx], mean(use_orig), 1e15, x_interest)
          }
          
          fitness_vals = fitness_function(X_sub)
          if (nrow(X_sub) > mu) {
            X_nondom = unique(X_sub[!bbotk::is_dominated(t(fitness_vals))])
            if (nrow(X_nondom) > mu) {
              X_nondom = X_nondom[sample.int(nrow(X_nondom), mu)]
            }
          } else {
            X_nondom = X_sub
          }
          
          param_set = make_param_set(X, lower = NULL, upper = NULL)
          mydesign = SamplerUnif$new(param_set)$sample(n)
          mydesign$data[sample.int(nrow(mydesign$data), nrow(X_nondom))] = X_nondom
          mydesign$data = reset_columns(mydesign$data, p_use_orig = 0.5, max_changed = 1e15, x_interest = x_interest)
          mydesign
        }
      }
      f_design = make_f_design(predictor$data$X, flex_cols, x_interest, sdevs_num_feats)
    }
    
    my_design = f_design(param_set, n)
    x_interest_reorderd = x_interest[, names(my_design$data), with = FALSE]
    
    
    factor_cols = names(x_interest_reorderd)[sapply(x_interest_reorderd, is.factor)]
    if (length(factor_cols) > 0L) {
      x_interest_reorderd[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
    }
    
    # If more changes than allowed, randomly reset some features such that constraint holds
    if (!is.null(max_changed)) {
      for (i in seq_len(nrow(my_design$data))) {
        n_changes = count_changes(my_design$data[i, ], x_interest_reorderd)
        if (n_changes > max_changed) {
          idx_diff = which(my_design$data[i, ] != x_interest_reorderd)
          idx_reset = sample(idx_diff, size = n_changes - max_changed)
          set(my_design$data, i, j = idx_reset, value = x_interest_reorderd[, ..idx_reset])
        }
      }
    }

    my_design
  }

}


# Calculate ice curve standard deviation for all features
get_ICE_sd = function(x_interest, predictor, param_set) {
  vapply(names(x_interest), function(i_name) {
    ps_sub = param_set$clone()
    ps_sub$subset(i_name)
    grid1d = generate_design_grid(ps_sub, resolution = 20L)$data
    if (is.factor(x_interest[[i_name]])) {
      grid1d[, (i_name) := factor(grid1d[[i_name]], levels = levels(x_interest[[i_name]]))]
    }
    x_interest_sub = copy(x_interest)
    x_interest_sub[, (i_name) := NULL]
    dt = data.table(cbind(grid1d, x_interest_sub))
    pred = predictor$predict(dt)
    sd = sd(pred[[1L]])
    # No need to change constant features
    sd[is.na(sd)] = 0
    sd
  }, FUN.VALUE = numeric(1L))
}


make_moc_statistics_plots = function(archive, ref_point, normalize_objectives) {
  obj_names = c("dist_target", "dist_x_interest", "nr_changed", "dist_train")

  ls_stats = lapply(seq_len(max(archive$data$batch_nr)), function(i){
    best = archive$best(seq_len(i))
    best_mean = best[, lapply(.SD, mean, na.rm = TRUE), .SDcols = obj_names]
    best_min = best[, lapply(.SD, min, na.rm = TRUE), .SDcols = obj_names]
    # TODO: ecr::computeHV gives slightly different results (monotonic increase of domhv)
    # hv = data.table(
    #   hv = ecr::computeHV(t(best[, ..obj_names]), ref_point),
    #   generations = i
    # )
    hv = data.table(
      hv = ecr::computeHV(t(best[, obj_names, with = FALSE]), ref_point),
      #hv = miesmuschel:::domhv(-as.matrix(best[, ..obj_names]), nadir = -ref_point, on_worse_than_nadir = "quiet"),
      generations = i
    )
    
    best_mean[, generation := i]
    best_min[, generation := i]
    
    list(best_mean, best_min, hv)
  }) 

  dt_agg_mean = rbindlist(lapply(ls_stats, "[[", 1L))
  dt_agg_min = rbindlist(lapply(ls_stats, "[[", 2L))
  dt_hv = rbindlist(lapply(ls_stats, "[[", 3L))
  
  if (normalize_objectives) {
    eps = .Machine$double.eps
    dt_agg_mean[, (obj_names) := lapply(.SD, function(x) (x - min(x)) / (max(x) - min(x) + eps)), .SDcols = obj_names]
    dt_agg_min[, (obj_names) := lapply(.SD, function(x)  (x - min(x)) / (max(x) - min(x) + eps)), .SDcols = obj_names]
    dt_agg_mean = melt(dt_agg_mean, id.vars = "generation", measure.vars = obj_names)
    dt_agg_min = melt(dt_agg_min, id.vars = "generation", measure.vars = obj_names)
    
    gg_mean = ggplot2::ggplot(dt_agg_mean) + 
      ggplot2::geom_line(ggplot2::aes(x = generation, y = value, color = variable)) +
      ggplot2::xlab("generations") +
      ggplot2::ggtitle("Mean objective values (normalized)") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      ggplot2::theme(legend.title = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
    
    gg_min = ggplot2::ggplot(dt_agg_min) + 
      ggplot2::geom_line(ggplot2::aes(x = generation, y = value, color = variable)) +
      ggplot2::xlab("generations") +
      ggplot2::ggtitle("Minimum objective values (normalized)") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      ggplot2::theme(legend.title = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
    
  } else {
    dt_agg_mean = melt(dt_agg_mean, id.vars = "generation", measure.vars = obj_names)
    dt_agg_min = melt(dt_agg_min, id.vars = "generation", measure.vars = obj_names)
    
    gg_mean = ggplot2::ggplot(dt_agg_mean) + 
      ggplot2::geom_line(ggplot2::aes(x = generation, y = value)) +
      ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_y", nrow = 4L) +
      ggplot2::xlab("generations") +
      ggplot2::ggtitle("Mean objective values") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    
    gg_min = ggplot2::ggplot(dt_agg_min) + 
      ggplot2::geom_line(ggplot2::aes(x = generation, y = value)) +
      ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_y", nrow = 4L) +
      ggplot2::xlab("generations") +
      ggplot2::ggtitle("Minimum objective values") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  }
  
  gg_hv = ggplot2::ggplot(dt_hv) + 
    ggplot2::geom_line(ggplot2::aes(x = generations, y = hv)) +
    ggplot2::xlab("generations") +
    ggplot2::ggtitle("Dominated hypervolume") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
  
  list(gg_mean, gg_min, gg_hv)
}

comp_domhv_all_gen = function(archive, ref_point) {
  obj_names = c("dist_target", "dist_x_interest", "nr_changed", "dist_train")
  data.table(
    generations = unique(archive$data$batch_nr), 
    hv = vapply(
      seq_len(max(archive$data$batch_nr)), 
      function(i) {
        best = archive$best(seq_len(i))
        # TODO: ecr::computeHV gives slightly different results (monotonic increase of domhv)
        ecr::computeHV(t(best[, obj_names, with = FALSE]), ref_point)
        # miesmuschel:::domhv(
        #   -as.matrix(best[, obj_names, with = FALSE]), 
        #   nadir = -ref_point,
        #   on_worse_than_nadir = "quiet"      
        # )
      } ,
      FUN.VALUE = numeric(1L)
    )
  )
}


make_moc_search_plot = function(data, objectives) {
  ggplot2::ggplot(data, ggplot2::aes_string(x = objectives[[1L]], y = objectives[[2L]], alpha = "batch_nr")) +
    ggplot2::geom_point(color = "black") +
    ggplot2::scale_alpha_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1), n = 5)))) +
    ggplot2::labs(alpha = "generation") +
    ggplot2::theme_bw()
}



# Conditional mutator as described in the MOC paper
MutatorConditional = R6::R6Class("MutatorConditional", inherit = Mutator,
  public = list(
    initialize = function(cond_sampler, param_set, p_mut, p_mut_gen) {
      super$initialize()
      assert_class(param_set, "ParamSet")
      assert_list(cond_sampler, len = length(param_set$ids()))
      private$param_set = param_set
      private$cond_sampler = cond_sampler
      private$p_mut = p_mut
      private$p_mut_gen = p_mut_gen
    }
  ),
  private = list(
    cond_sampler = NULL,
    param_set = NULL,
    p_mut = NULL,
    p_mut_gen = NULL,
    
    .mutate = function(values, context) {
      values_mutated = copy(values)
      for (i in seq_len(nrow(values))) {
        if (runif(1L) < private$p_mut) {
          for (j in sample(names(values))) {
            if (runif(1L) < private$p_mut_gen) {
              set(values_mutated, i, j, value = private$cond_sampler[[j]]$sample(values[i, ]))
            }
          }
        }
        
      }
      values_mutated
    }
 )
)


make_moc_conditional_mutator = function(ps, x_interest, max_changed, p_mut, p_mut_gen, p_mut_use_orig, cond_sampler) {
  op_seq1 = MutatorConditional$new(cond_sampler, ps, p_mut, p_mut_gen)
  op_seq2 = MutatorReset$new(x_interest, p_mut_use_orig, max_changed)
  mut("sequential", list(op_seq1, op_seq2))
}



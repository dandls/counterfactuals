make_fitness_function = function(predictor, x_interest, param_set, pred_column, target, weights, k, fixed_features) {
  
  param_range = param_set$upper - param_set$lower
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
    dist_x_interest = as.vector(StatMatch::gower.dist(x_interest, xdt, rngs = param_range, KR.corr = FALSE))
    nr_changed = rowSums(xdt != x_interest[rep(seq_len(nrow(x_interest)), nrow(xdt)), ])
    dist_train = apply(
      StatMatch::gower.dist(predictor$data$X, xdt, rngs = param_range, KR.corr = FALSE),
      MARGIN = 2L,
      FUN = function(dist) {
        d = sort(dist)[1:k]
        if (!is.null(weights)) {
          d = weighted.mean(d, w = weights)
        } else {
          d = mean(d)
        }
        d
      }
    )
    data.table(cbind(dist_target, dist_x_interest, nr_changed, dist_train))
  }
}

# Reset mutated feature values to feature value of x_interest with prop p_use_orig and controls that maximum
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



# Reset recombinated values to feature value of x_interest with prop p_use_orig and controls that maximum
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


reset_columns = function(values, p_use_orig, max_changed, x_interest) {
  values_reset = copy(values)
  
  for (i in seq_len(nrow(values_reset))) {

    # Removes fixed features from x_interest, if present, as only flex features are contained in values_reset
    x_interest_sub = x_interest[, names(values_reset), with = FALSE]
    
    factor_cols = which(sapply(x_interest_sub, is.factor))
    if (length(factor_cols) > 0L) {
      x_interest_sub[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
    }
    
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

# Slightly adapted from ScalorNondom
ScalorNondomPenalized = R6::R6Class("ScalorNondomPenalized", inherit = Scalor,
  
  public = list(
    #' @description
    #' Initialize the `ScalorNondomPenalized` object.
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
      
      # Add penalization for individuals with -dist_target lower than -epsilon (shifted up by one front)
      epsilon = params$epsilon
      if (is.null(epsilon)) epsilon = Inf
      is_penalized = fitnesses[colnames(fitnesses) == "dist_target"] < -epsilon
      sorted[is_penalized] = sorted[is_penalized] + 1L
      front_indexes = sort(unique(sorted))
      
      fronts = lapply(split(as.data.frame(fitnesses), sorted), as.matrix)
      subranks = lapply(fronts, function(x) rank(dist_crowding(x)) / (length(x) + 1))
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

make_moc_mutator = function(ps, x_interest, max_changed, sdevs, p_mut, p_mut_gen, p_mut_use_orig) {
  ops_list = list()
  if ("ParamDbl" %in% ps$class) {
    ops_list[["ParamDbl"]] = mut("maybe", mut("gauss", sdev = sdevs), mut("null"), p = p_mut_gen)
  }
  if ("ParamInt" %in% ps$class) {
    ops_list[["ParamInt"]] = mut("maybe", mut("gauss"), mut("null"), p = p_mut_gen)
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
    # TODO: Replace this with "simulated binary crossover recombinator"
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
                                    predictor) {
  function(param_set, n) {
    
    if (init_strategy == "random") {
      f_design = function(ps, n) {
        SamplerUnif$new(ps)$sample(n)
      }
    } else if (init_strategy == "sd") {

      if (length(sdevs) == 0L) {
        f_design = function(ps, n) {
          SamplerUnif$new(ps)$sample(n)
        }
      } else {
        make_f_design = function(X, x_interest, sdevs, lower, upper) {
          x_interest_dbl = x_interest[, names(sdevs), with = FALSE]
          lower_bounds = pmax(ps$lower[names(sdevs)], x_interest_dbl - sdevs)
          upper_bounds = pmin(ps$upper[names(sdevs)], x_interest_dbl + sdevs)
          lower_bounds[names(lower)] = lower
          upper_bounds[names(upper)] = upper

          param_set_init = make_param_set(X, lower = lower_bounds, upper = upper_bounds)
          function(ps, n) {
            SamplerUnif$new(param_set_init)$sample(n)
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
          p_pax = 0.99
          p_differs = (ice_sds - min(ice_sds)) * (p_pax - p_min) / (max(ice_sds) - min(ice_sds)) + p_min

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
            reset = which(sample(c(TRUE, FALSE), size = ncol(mydesign$data), replace = TRUE, prob = c(1 - p, p)))
            set(mydesign$data, reset, j, x_interest_sub[[j]])
          }
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
    sd(pred[[1L]])
  }, FUN.VALUE = numeric(1L))
}


make_moc_statistics_plots = function(data, ref_point, normalize_objectives) {
  obj_names = c("dist_target", "dist_x_interest", "nr_changed", "dist_train")
  dt = data[, c("batch_nr", obj_names), with = FALSE]
  dt_agg_mean = dt[, lapply(.SD, mean), by = .(batch_nr), .SDcols = obj_names]
  dt_agg_min = dt[, lapply(.SD, min), by = .(batch_nr), .SDcols = obj_names]
  if (normalize_objectives) {
    dt_agg_mean[, (obj_names) := lapply(.SD, function(x) (x - min(x)) / (max(x) - min(x))), .SDcols = obj_names]
    dt_agg_min[, (obj_names) := lapply(.SD, function(x)  (x - min(x)) / (max(x) - min(x))), .SDcols = obj_names]
    dt_agg_mean = melt(dt_agg_mean, id.vars = "batch_nr", measure.vars = obj_names)
    dt_agg_min = melt(dt_agg_min, id.vars = "batch_nr", measure.vars = obj_names)
    
    gg_mean = ggplot2::ggplot(dt_agg_mean) + 
      ggplot2::geom_line(ggplot2::aes(x = batch_nr, y = value, color = variable)) +
      ggplot2::xlab("generations") +
      ggplot2::ggtitle("Mean objective values") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    
    gg_min = ggplot2::ggplot(dt_agg_min) + 
      ggplot2::geom_line(ggplot2::aes(x = batch_nr, y = value, color = variable)) +
      ggplot2::xlab("generations") +
      ggplot2::ggtitle("Minimum objective values") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    
  } else {
    dt_agg_mean = melt(dt_agg_mean, id.vars = "batch_nr", measure.vars = obj_names)
    dt_agg_min = melt(dt_agg_min, id.vars = "batch_nr", measure.vars = obj_names)
    
    gg_mean = ggplot2::ggplot(dt_agg_mean) + 
      ggplot2::geom_line(ggplot2::aes(x = batch_nr, y = value)) +
      ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_y", nrow = 4L) +
      ggplot2::xlab("generations") +
      ggplot2::ggtitle("Mean objective values") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    
    gg_min = ggplot2::ggplot(dt_agg_min) + 
      ggplot2::geom_line(ggplot2::aes(x = batch_nr, y = value)) +
      ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_y", nrow = 4L) +
      ggplot2::xlab("generations") +
      ggplot2::ggtitle("Minimum objective values") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  }
  
  
  dt_hv = comp_domhv_all_gen(dt, ref_point)
  gg_hv = ggplot2::ggplot(dt_hv) + 
    ggplot2::geom_line(ggplot2::aes(x = generations, y = hv)) +
    ggplot2::xlab("generations") +
    ggplot2::ggtitle("Dominated hypervolume") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  
  list(gg_mean, gg_min, gg_hv)
}

comp_domhv_all_gen = function(fitness_values, ref_point) {
  data.table(
    generations = unique(fitness_values$batch_nr), 
    hv = vapply(
      seq_len(max(fitness_values$batch_nr)), 
      # TODO: Replace this with miesmuschel:::domhv
      function(i) emoa::dominated_hypervolume(t(as.matrix(fitness_values[batch_nr == i, -"batch_nr"])), ref = ref_point),
      FUN.VALUE = numeric(1L)
    )
  )
}

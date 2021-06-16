make_fitness_function = function(predictor, x_interest, param_range, obj_names, pred_column, desired_y_hat_range,
  track_infeas, weights, k, fixed_features) {
  function(xdt) {

    if (!is.null(fixed_features)) {
      xdt[, (fixed_features) := x_interest[, fixed_features, with = FALSE]]
    }

    xdt = xdt[, names(x_interest), with = FALSE]

    factor_cols = names(predictor$data$X)[sapply(predictor$data$X, is.factor)]
    for (factor_col in factor_cols) {
      xdt[, (factor_col) := factor(xdt[[factor_col]], levels = levels(predictor$data$X[[factor_col]]))]
    }

    int_cols = names(predictor$data$X)[sapply(predictor$data$X, is.integer)]
    for (int_col in int_cols) {
      xdt[, (int_col) := as.integer(xdt[[int_col]])]
    }

    pred = predictor$predict(xdt)[[pred_column]]

    # objective criteria
    tg = desired_y_hat_range
    q1 = sapply(
      pred, function(x) ifelse(between(x, tg[1L], tg[2L]), 0, min(abs(x - tg)))
    )
    q2 = as.vector(StatMatch::gower.dist(x_interest, xdt, rngs = param_range, KR.corr = FALSE))
    q3 = rowSums(xdt != x_interest[rep(seq_len(nrow(x_interest)), nrow(xdt)), ])
    q_dt = data.table(cbind(q1, q2, q3))
    names(q_dt) = obj_names[1:3]

    if (track_infeas) {
      # TODO: Check that k < nrow(predictor$data$X)
      q4 = apply(
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
      q_dt[, (obj_names[4L]) := q4]
    }
    q_dt
  }
}


MutatorReset = R6::R6Class("MutatorReset",
  inherit = miesmuschel::Mutator,
  public = list(
    initialize = function(x_interest, p_mut_use_orig, max_changed) {
      private$.x_interest = assert_data_table(x_interest)
      assert_numeric(p_mut_use_orig, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
      assert_integerish(max_changed, lower = 0, len = 1L, any.missing = FALSE, null.ok = TRUE)

      params = ps(
        max_changed = p_int(special_vals = list(NULL)),
        p_mut_use_orig = p_dbl()
      )
      params$values = list(
        "max_changed" = max_changed,
        "p_mut_use_orig" = p_mut_use_orig
      )
      super$initialize(param_set = params)
    },
    prime = function(param_set) {
      # see if param_set is compatible with private$.x_interest
      super$prime(param_set)
    }
  ),
  active = list(
    x_interest = function(rhs) {
      if (!missing(rhs)) stop("x_interest is read-only")
      private$.x_interest
    }
  ),
  private = list(
    .x_interest = NULL,
    .mutate = function(values, context) {
      max_changed = private$.param_set$values$max_changed
      p_mut_use_orig = private$.param_set$values$p_mut_use_orig
      values_mutated = copy(values)

      for (i in 1:nrow(values_mutated)) {
        use_origin = rep(FALSE, ncol(values_mutated))
        names(use_origin) = names(values_mutated)

        draws = sample(
          c(TRUE, FALSE),
          size = ncol(values_mutated), replace = TRUE, prob = c(p_mut_use_orig, 1 - p_mut_use_orig)
        )
        n_true = sum(draws)
        if (n_true > 0L) {
          origin_cols = names(values_mutated)[draws]
          values_mutated[i, (origin_cols) := private$.x_interest[, (origin_cols), with = FALSE]]
        }

        x_interest_sub = private$.x_interest[, names(values_mutated), with = FALSE]

        n_changes = counterfactuals:::count_changes(values_mutated[i, ], x_interest_sub)

        if (!is.null(max_changed)) {
          if (n_changes > max_changed) {
            pos_diff = which(values_mutated[i, ] != x_interest_sub)
            to_be_reverted = sample(pos_diff, size = n_changes - max_changed)
            to_be_reverted = names(values_mutated)[to_be_reverted]
            values_mutated[i, (to_be_reverted) := x_interest_sub[, to_be_reverted, with = FALSE]]
          }
        }

      }
      values_mutated
    }
  )
)



# Recmobuinator
RecombinatorReset = R6::R6Class("RecombinatorReset",
  inherit = miesmuschel::Recombinator,
  public = list(
    initialize = function(x_interest, p_mut_use_orig, max_changed) {
      private$.x_interest = assert_data_table(x_interest)
      assert_numeric(p_mut_use_orig, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
      assert_integerish(max_changed, lower = 0, len = 1L, any.missing = FALSE, null.ok = TRUE)
      params = ps(
        max_changed = p_int(special_vals = list(NULL)),
        p_mut_use_orig = p_dbl()
      )
      params$values = list(
        "max_changed" = max_changed,
        "p_mut_use_orig" = p_mut_use_orig
      )
      super$initialize(param_set = params)
    }
  ),
  private = list(
    .x_interest = NULL,

    .recombine = function(values, context) {
      max_changed = private$.param_set$values$max_changed
      p_mut_use_orig = private$.param_set$values$p_mut_use_orig

      values_rec = copy(values)

      for (i in 1:nrow(values_rec)) {
        use_origin = rep(FALSE, ncol(values_rec))
        names(use_origin) = names(values_rec)

        draws = sample(
          c(TRUE, FALSE),
          size = ncol(values_rec), replace = TRUE, prob = c(p_mut_use_orig, 1 - p_mut_use_orig)
        )
        n_true = sum(draws)
        if (n_true > 0L) {
          origin_cols = names(values_rec)[draws]
          values_rec[i, (origin_cols) := private$.x_interest[, (origin_cols), with = FALSE]]
        }

        x_interest_sub = private$.x_interest[, names(values_rec), with = FALSE]
        n_changes = counterfactuals:::count_changes(values_rec[i, ], x_interest_sub)
        if (!is.null(max_changed)) {
          if (n_changes > max_changed) {
            pos_diff = which(values_rec[i, ] != x_interest_sub)
            to_be_reverted = sample(pos_diff, size = n_changes - max_changed)
            to_be_reverted = names(values_rec)[to_be_reverted]
            values_rec[i, (to_be_reverted) := x_interest_sub[, to_be_reverted, with = FALSE]]
          }
        }

      }

      values_rec
    }
  )
)

make_moc_mutator = function(ps, max_changed, sdevs_num_feats, p_mut_gen, p_mut_use_orig) {
  ops_m_list = list()
  if ("ParamDbl" %in% ps$class) {
    ops_m_list[["ParamDbl"]] = miesmuschel::mut(
      "maybe", miesmuschel::mut("gauss", sdev = sdevs_num_feats), miesmuschel::mut("null"),
      p = p_mut_gen
    )
  }
  if ("ParamInt" %in% ps$class) {
    ops_m_list[["ParamInt"]] = miesmuschel::mut(
      "maybe", miesmuschel::mut("gauss"), miesmuschel::mut("null"),
      p = p_mut_gen
    )
  }
  if ("ParamFct" %in% ps$class) {
    n_facts = sum("ParamFct" == ps$class)
    ls_op_factor = rep(list(miesmuschel::mut("unif", can_mutate_to_same = FALSE)), n_facts)
    names(ls_op_factor) = ps$ids()[which(ps$class == "ParamFct")]
    ops_m_list = c(ops_m_list, ls_op_factor)
  }

  op_m_seq1 = miesmuschel::mut("combine", operators = ops_m_list)
  op_m_seq2 = MutatorReset$new(x_interest, p_mut_use_orig, max_changed)
  miesmuschel::mut("sequential", list(op_m_seq1, op_m_seq2))
}


make_moc_recombinator = function(ps, max_changed, sdevs_num_feats, p_rec_use_orig) {
  # TODO: DO we need probs here?
  ops_r_list = list()
  if ("ParamDbl" %in% ps$class) {
    # TODO: Replace this with "simulated binary crossover recombinator"
    ops_r_list[["ParamDbl"]] = miesmuschel::rec("xounif")
  }
  if ("ParamInt" %in% ps$class) {
    ops_r_list[["ParamInt"]] = miesmuschel::rec("xounif")
  }
  if ("ParamFct" %in% ps$class) {
    n_facts = sum("ParamFct" == ps$class)
    ls_op_factor = rep(list(miesmuschel::rec("xounif")), n_facts)
    names(ls_op_factor) = names(ps$class)[which(ps$class == "ParamFct")]
    ops_r_list = c(ops_r_list, ls_op_factor)
  }
  op_r_seq_1 = miesmuschel::rec("combine", operators = ops_r_list)
  op_r_seq_2 = RecombinatorReset$new(x_interest, p_rec_use_orig, max_changed = 2L)
  miesmuschel::rec("sequential", list(op_r_seq_1, op_r_seq_2))
}


make_moc_pop_initializer = function(ps, x_interest, max_changed) {
  if (is.null(max_changed)) {
    pop_initializer = paradox::generate_design_random
  } else {
    pop_initializer = function(param_set, n) {
      my_design = paradox::SamplerUnif$new(ps)$sample(n)
      x_interest_reorderd = x_interest[, names(my_design$data), with = FALSE]
      n_changes = counterfactuals:::count_changes(my_design$data, x_interest_reorderd)

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
  pop_initializer
}

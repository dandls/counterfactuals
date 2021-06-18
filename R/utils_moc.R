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

        n_changes = count_changes(values_mutated[i, ], x_interest_sub)

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
        n_changes = count_changes(values_rec[i, ], x_interest_sub)
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

# Slightly adaptapred from miesmuschel::ScalorNondom
ScalorNondomPenalized <- R6::R6Class("ScalorNondomPenalized",
  inherit = miesmuschel::Scalor,
  public = list(
    #' @description
    #' Initialize the `ScalorNondomPenalized` object.
    initialize = function(epsilon) {
      param_set <- ps(
        epsilon = p_dbl(lower = 0, tags = "required"),
        scale_output = p_lgl(tags = "required"),
        jitter = p_lgl(tags = "required"),
        tiebreak = p_fct(c("crowding-dist", "hv-contrib", "domcount", "none"))
      )

      if (is.null(epsilon)) epsilon = Inf
      param_set$values <- list(epsilon = epsilon, scale_output = FALSE, jitter = TRUE, tiebreak = "crowding-dist")
      super$initialize(param_set = param_set, dict_entry = "nondom")
    }
  ),
  private = list(
    .scale = function(values, fitnesses, context) {

      params <- self$param_set$get_values(context = context)
      if (params$jitter) {
        fitnesses <- fitnesses *
          (1 + runif(length(fitnesses)) * sqrt(.Machine$double.eps))
      }
      # Add penalization for individuals with -dist_target lower than -epsilon (shifted up by one front)
      epsilon = params$epsilon
      is_penalized = fitnesses[colnames(fitnesses) == "dist_target"] < -epsilon
      sorted = miesmuschel::order_nondominated(fitnesses)$fronts
      sorted[is_penalized] = sorted[is_penalized] + 1L
      front_indexes = sort(unique(sorted))
      sorted = switch(params$tiebreak,
        `crowding-dist` = {
          fronts <- lapply(split(as.data.frame(fitnesses), sorted), as.matrix)
          subranks <- lapply(fronts, function(x) rank(miesmuschel::dist_crowding(x)) / (length(x) + 1))
          for (i in seq_along(subranks)) {
            sr <- subranks[[i]]
            # There may be empty fronts in very few cases due to penalization
            front_index = front_indexes[i]
            if (length(sorted[sorted == front_index]) != length(front_index + sr)) {
              browser()
            }
            sorted[sorted == front_index] <- front_index + sr
          }
          sorted
        },
        `hv-contrib` = stop("not supported yet"),
        domcount = stop("not supported yet"),
        none = sorted
      )

      sorted = max(sorted) + 1 - sorted # want high front values for high fitnesses, so reverse ordering here

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


make_moc_pop_initializer = function(ps, x_interest, max_changed, init_strategy, flex_cols, sdevs_num_feats,
  lower, upper, predictor) {
  function(param_set, n) {

    if (init_strategy == "random") {
      f_design = function(ps, n) {
        paradox::SamplerUnif$new(ps)$sample(n)
      }
    } else if (init_strategy == "sd") {

      if (length(sdevs_num_feats) == 0L) {
        f_design = function(ps, n) paradox::SamplerUnif$new(ps)$sample(n)
      } else {
        make_f_design = function(X, x_interest, sdevs_num_feats, lower, upper) {

          x_interest_num = x_interest[, names(sdevs_num_feats), with = FALSE]
          lower_sdev = x_interest_num - sdevs_num_feats
          upper_sdev = x_interest_num + sdevs_num_feats

          lower_bound = pmax(ps$lower[names(sdevs_num_feats)], lower_sdev)
          upper_bound = pmin(ps$upper[names(sdevs_num_feats)], upper_sdev)

          lower_bound[names(lower)] = lower
          upper_bound[names(upper)] = upper

          param_set_init = make_param_set(X, lower = lower_bound, upper = upper_bound)
          function(ps, n) {
            paradox::SamplerUnif$new(param_set_init)$sample(n)
          }
        }

        f_design = make_f_design(predictor$data$X[, ..flex_cols], x_interest, sdevs_num_feats, lower, upper)
      }
    } else if (init_strategy == "icecurve") {


      make_f_design = function(X, flex_cols, x_interest, sdevs_num_feats) {
        function(ps, n) {

          param_set = make_param_set(X, lower = NULL, upper = NULL)
          mydesign = paradox::SamplerUnif$new(param_set)$sample(n)

          ice_sds = get_ICE_sd(x_interest, predictor, param_set)
          p_differs = (ice_sds - min(ice_sds)) * (0.99 - 0.01) / (max(ice_sds) - min(ice_sds)) + 0.01

          x_interest_sub = copy(x_interest)
          fixed_cols = which(!names(mydesign$data) %in% flex_cols)
          if (length(fixed_cols) > 0L) {
            mydesign$data[, (fixed_cols) := NULL]
            x_interest_sub = x_interest_sub[, ..flex_cols]
          }

          factor_cols = names(x_interest_sub)[sapply(x_interest_sub, is.factor)]
          if (length(factor_cols)) {
            x_interest_sub[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
          }

          for (j in 1:ncol(mydesign$data)) {
            p = p_differs[j]
            use_orig = which(sample(c(TRUE, FALSE), size = ncol(mydesign$data), replace = TRUE, prob = cbind(1 - p, p)))
            set(mydesign$data, use_orig, j, x_interest_sub[[j]])
          }
          mydesign
        }
      }

      f_design = make_f_design(predictor$data$X, flex_cols, x_interest, sdevs_num_feats)


    }

    my_design = f_design(param_set, n)
    x_interest_reorderd = x_interest[, names(my_design$data), with = FALSE]
    n_changes = count_changes(my_design$data, x_interest_reorderd)

    if (!is.null(max_changed)) {
      for (i in 1:nrow(my_design$data)) {
        if (n_changes[i] > max_changed) {
          pos_diff = which(my_design$data[i, ] != x_interest_reorderd)
          to_be_reverted = sample(pos_diff, size = n_changes[i] - max_changed)
          to_be_reverted = names(my_design$data)[to_be_reverted]
          my_design$data[i, (to_be_reverted) := x_interest_reorderd[, to_be_reverted, with = FALSE]]
        }
      }
    }

    my_design
  }

}


#' Calculate ice curve standard deviation for all features
#'
#' @section Arguments:
#' \describe{
#' \item{x_interest: }{(data.frame)\cr Data point, for which ice curves
#' should be calculated.}
#' \item{predictor: }{(Predictor)\cr Object, that holds the prediction model and dataset.}
#' \item{param_set: }{(ParamSet)\cr Parameter set of features in dataset.}
#' }
#' @return (numeric) Vector with standard deviations for each feature.
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

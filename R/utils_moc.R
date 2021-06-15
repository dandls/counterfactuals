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
    q1 = ifelse(data.table::between(pred, tg[1L], tg[2L]), 0, min(abs(pred - tg[1L]), abs(pred - tg[2L])))
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


MutatorReset = R6::R6Class("MutatorReset", inherit = Mutator,
  public = list(
    initialize = function(x_interest, p_mut_use_orig, max_changed) {
      private$.x_interest = assert_data_table(x_interest)
      assert_numeric(p_mut_use_orig, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
      assert_integerish(max_changed, lower = 0, len = 1L, any.missing = FALSE)

      params = ps(
        max_changed = p_int(max_changed),
        p_mut_use_orig = p_dbl(p_mut_use_orig)
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
          if (!is.null(max_changed) && sum(draws) > max_changed) {
            to_be_reverted = sample(which(draws), size = sum(draws) - max_changed)
            draws[to_be_reverted] = FALSE
          }

          origin_cols = names(values_mutated)[draws]
          values_mutated[i, (origin_cols) := private$.x_interest[, (origin_cols), with = FALSE]]
        }
      }

      values_mutated
    }
  )
)



RecombinatorReset = R6::R6Class("RecombinatorReset", inherit = Recombinator,
  public = list(
    initialize = function(x_interest, p_mut_use_orig, max_changed) {
      private$.x_interest = assert_data_table(x_interest)
      assert_numeric(p_mut_use_orig, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
      assert_integerish(max_changed, lower = 0, len = 1L, any.missing = FALSE)
      params = ps(
        max_changed = p_int(max_changed),
        p_mut_use_orig = p_dbl(p_mut_use_orig)
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
        if (!is.null(max_changed) && n_true > 0L) {
          if (sum(draws) > max_changed) {
            to_be_reverted = sample(which(draws), size = sum(draws) - max_changed)
            draws[to_be_reverted] = FALSE
          }

          origin_cols = names(values_rec)[draws]
          values_rec[i, (origin_cols) := private$.x_interest[, (origin_cols), with = FALSE]]
        }
      }

      values_rec
    }
  )
)

make_fitness_function = function(predictor, x_interest, param_range, obj_names, pred_column, desired_y_hat_range,
                                 track_infeas, weights, k) {
  function(xdt) {
    if ("use_orig" %in% names(xdt)) {
      xdt[, ("use_orig") := NULL]
    }
    
    factor_cols = names(predictor$data$X)[sapply(predictor$data$X, is.factor)]
    for (factor_col in factor_cols) {
      xdt[, (factor_col) := factor(xdt[[factor_col]], levels = levels(predictor$data$X[[factor_col]]))]
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
        StatMatch::gower.dist(predictor$data$X, xdt, rngs = param_range, KR.corr = FALSE), MARGIN = 2L,
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
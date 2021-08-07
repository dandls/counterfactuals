make_cfactuals_diff = function(cfactuals, x_interest) {
  cfactuals_diff = as.data.table(matrix(nrow = nrow(cfactuals), ncol = ncol(cfactuals)))
  setnames(cfactuals_diff, old = names(cfactuals_diff), new = names(cfactuals))
  if (nrow(cfactuals_diff) == 0L) {
    return(cfactuals_diff)
  }
  
  is_numeric_col = sapply(cfactuals, test_numeric)
  idx_numeric = which(is_numeric_col)
  idx_non_numeric = which(!is_numeric_col)
  
  if (length(idx_numeric) > 0L) {
    m_num = as.matrix(cfactuals[, ..idx_numeric])
    x_interest_num = as.numeric(x_interest[1L , ..idx_numeric])
    diff_num = data.table::as.data.table(sweep(m_num, 2, x_interest_num))
    diff_num[diff_num == 0] = NA
    data.table::set(cfactuals_diff, j = idx_numeric, value = diff_num)
  }
  
  if (length(idx_non_numeric) > 0L) {
    m_char = as.matrix(cfactuals[, ..idx_non_numeric])
    x_interest_char = as.matrix(x_interest[1L , ..idx_non_numeric])
    no_diff = sweep(m_char, 2L, x_interest_char, FUN = "==")
    m_char[no_diff] = NA
    diff_char = data.table::as.data.table(m_char)
    data.table::set(cfactuals_diff, j = idx_non_numeric, value = diff_char)
  }
  
  cfactuals_diff
}

count_changes = function(cfactuals, x_interest) {
  cfactuals[, sum(.SD != x_interest), by = seq_len(nrow(cfactuals))][[2L]]
}

make_surface_plot = function(grid_size, param_set, cfactuals_plotted, x_interest, predictor, feature_names, 
                             pred_column) {
  
  param_set_sub = param_set$clone()$subset(feature_names)
  dt_grid = make_ice_curve_area(predictor, x_interest, grid_size, param_set_sub, pred_column)
  x_feat_name = feature_names[1L]
  y_feat_name = feature_names[2L]
  
  if (param_set_sub$all_numeric) {
    # TODO: adapt this for hard classification
    p = ggplot2::ggplot(data = dt_grid, ggplot2::aes_string(x = x_feat_name, y = y_feat_name)) + 
      ggplot2::geom_tile(ggplot2::aes_string(fill = "pred")) +
      ggplot2::geom_contour(ggplot2::aes_string(z = "pred"), colour = "white") +
      ggplot2::guides(z = ggplot2::guide_legend(title = "pred")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "left")
    
    if (nrow(cfactuals_plotted) > 0L) {
      p = p + 
        ggplot2::geom_point(ggplot2::aes_string(x = x_feat_name, y = y_feat_name), cfactuals_plotted, colour = "black") +
        ggplot2::geom_rug(ggplot2::aes_string(x = x_feat_name, y = y_feat_name), cfactuals_plotted)
    }
    
    p = p + ggplot2::geom_point(ggplot2::aes_string(x = x_feat_name, y = y_feat_name), x_interest, colour = "white")
    
  } else if (param_set_sub$all_categorical) {
    p = ggplot2::ggplot(dt_grid, ggplot2::aes_string(x_feat_name, y_feat_name)) +
      ggplot2::geom_tile(ggplot2::aes_string(fill = "pred")) +
      ggplot2::geom_point(ggplot2::aes_string(x_feat_name, y_feat_name), x_interest, color = "white") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "pred")) +
      ggplot2::theme_bw()
    
    if (nrow(cfactuals_plotted) > 0L) {
      p = p + 
        ggplot2::geom_jitter(ggplot2::aes_string(x_feat_name, y_feat_name), cfactuals_plotted, width = 0.2, height = 0.2)
    }
    
  } else {
    cat_feature = feature_names[param_set_sub$is_categ]
    num_feature = setdiff(feature_names[1:2], cat_feature)
    cfactuals_plotted$pred = predictor$predict(cfactuals_plotted)[[pred_column]]
    y_hat_interest = predictor$predict(x_interest)
    x_interest_with_pred = cbind(x_interest, pred = y_hat_interest[[pred_column]])
    
    p = ggplot2::ggplot() +
      ggplot2::geom_line(
        ggplot2::aes_string(x = num_feature, y = "pred", group = cat_feature, color = cat_feature), dt_grid
      ) +
      ggplot2::theme_bw()
    
    if (nrow(cfactuals_plotted) > 0L) {
      p = p +
        ggplot2::geom_point(ggplot2::aes_string(x = num_feature, y = "pred"), cfactuals_plotted, colour = "black") +
        ggplot2::geom_rug(ggplot2::aes_string(x = num_feature, y = "pred"), cfactuals_plotted)
    }
    
    p = p +
      ggplot2::geom_point(ggplot2::aes_string(x = num_feature, y = "pred"), x_interest_with_pred, colour = "grey")
    
  }
  p
}


make_ice_curve_area = function(predictor, x_interest, grid_size, ps, pred_column) {
  exp_grid = generate_design_grid(ps, grid_size)$data
  feat1_name = names(ps$class)[1L]
  if (is.factor(x_interest[[feat1_name]])) {
    exp_grid[[feat1_name]] = as.factor(exp_grid[[feat1_name]])
  }
  feat2_name = names(ps$class)[2L]
  if (is.factor(x_interest[[feat2_name]])) {
    exp_grid[[feat2_name]] = as.factor(exp_grid[[feat2_name]])
  }
  
  x_interest_sub = x_interest[, !names(x_interest) %in% names(ps$class), with = FALSE]
  instance_dt = x_interest_sub[rep(1:nrow(x_interest_sub), nrow(exp_grid))]
  grid_dt = cbind(instance_dt, exp_grid)
  
  # Transform factor column w.r.t to original data (required for prediction)
  factor_cols = names(which(sapply(predictor$data$X, is.factor)))
  for (factor_col in factor_cols) {
    fact_col_pred = predictor$data$X[[factor_col]]
    value =  factor(grid_dt[[factor_col]], levels = levels(fact_col_pred), ordered = is.ordered(fact_col_pred))
    grid_dt[, (factor_col) := value]
  }
  
  pred = predictor$predict(grid_dt)[[pred_column]]
  cbind(grid_dt, pred)
}



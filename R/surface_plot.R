make_surface_plot = function(grid_size, param_set, cfactuals_plotted, x_interest, prediction_function, feature_names,
                             pred_column) {
  
  param_set_sub = param_set$clone()$subset(feature_names)
  dt_grid = make_ice_curve_area(prediction_function, x_interest, grid_size, param_set_sub, pred_column)
  x_feat_name = feature_names[1L]
  y_feat_name = feature_names[2L]
  
  if (param_set_sub$all_numeric) {
    # TODO: adapt this for hard classification
    p = ggplot2::ggplot(data = dt_grid) +
      ggplot2::geom_contour_filled(ggplot2::aes_string(x = x_feat_name, y = y_feat_name, z = "pred")) +
      ggplot2::geom_point(ggplot2::aes_string(x = x_feat_name, y = y_feat_name), x_interest, colour = "white") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "pred")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "left")
    
    if (nrow(cfactuals_plotted) > 0L) {
      p = p + 
        ggplot2::geom_point(ggplot2::aes_string(x = x_feat_name, y = y_feat_name), cfactuals_plotted, colour = "black")
    }
    
    p = ggExtra::ggMarginal(p, type = "histogram")
    
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
    cfactuals_plotted$pred = prediction_function(cfactuals_plotted)[[pred_column]]
    y_hat_interest = prediction_function(x_interest)
    x_interest_with_pred = cbind(x_interest, pred = y_hat_interest[[pred_column]])
    
    p = ggplot2::ggplot() +
      ggplot2::geom_line(
        ggplot2::aes_string(x = num_feature, y = "pred", group = cat_feature, color = cat_feature), dt_grid
      ) +
      ggplot2::theme_bw()
    
    if (nrow(cfactuals_plotted) > 0L) {
      p = p +
        ggplot2::geom_point(ggplot2::aes_string(x = num_feature, y = "pred"), cfactuals_plotted, colour = "black")
    }
    
    p = p +
      ggplot2::geom_point(ggplot2::aes_string(x = num_feature, y = "pred"), x_interest_with_pred, colour = "gray")
    p = ggExtra::ggMarginal(p, type = "histogram", margins = "x")
    
  }
  p
}


make_ice_curve_area = function(prediction_function, x_interest, grid_size, ps, pred_column) {
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
  
  grid_df = cbind(instance_dt, exp_grid)
  pred = prediction_function(grid_df)[[pred_column]]
  cbind(grid_df, pred)
}
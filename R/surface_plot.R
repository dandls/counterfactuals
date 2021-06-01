make_ice_curve_area = function(prediction_function, x_interest, feature_names, grid_size, param_set, pred_column) {

  param_set_filtered = param_set$subset(feature_names)
  min_max = data.frame(rbind(param_set_filtered$lower, param_set_filtered$upper))
  values = param_set$get_values()
  ls_values = lapply(values, function(x) unlist(x, use.names = FALSE))
  ls_min_max_or_values = c(as.list(min_max), ls_values)
  
  grids = sapply(feature_names, function(feat) {
    as.data.frame(equidistant.grid(ls_min_max_or_values[[feat]], grid_size))
  })
  
  # stringsAsFactors must not be set to TRUE as this can lead to factors that are not contained in
  # the original training data, which might result in an error in the predict function
  expgrid = expand.grid(a = grids[[1L]], b = grids[[2L]], stringsAsFactors = FALSE)
  colnames(expgrid) = feature_names
  x_interest = x_interest[, !names(x_interest) %in% feature_names, with = FALSE]
  instance_dt = x_interest[rep(1:nrow(x_interest), nrow(expgrid))]
  
  grid_df = cbind(instance_dt, expgrid)
  pred = prediction_function(grid_df)[[pred_column]]
  cbind(expgrid, pred)
}

plot_ice_curve_area = function(grid_size, param_set, cfactuals_plotted, x_interest, prediction_function, 
                               feature_names, pred_column) {
  
  all_plot_feats_numeric = param_set$subset(feature_names)$all_numeric
  all_plot_feats_categorical = param_set$subset(feature_names)$all_categorical

  y_hat_interest = prediction_function(x_interest)
  x_interest_with_pred = cbind(x_interest, pred = y_hat_interest[[pred_column]])
  
  x_feat_name = feature_names[1L]
  y_feat_name = feature_names[2L]
  
  # TODO: all_plot_feats_categorical -> heat map or something
  if (all_plot_feats_numeric) {
    grid = make_ice_curve_area(prediction_function, x_interest, feature_names, grid_size, param_set, pred_column)
    p = ggplot2::ggplot()  +
      ggplot2::theme_bw() +
      ggplot2::geom_tile(
        data = grid,
        mapping = ggplot2::aes_string(x = x_feat_name, y = y_feat_name, fill = "pred")
      ) +
      ggplot2::stat_contour(
        mapping = ggplot2::aes_string(x = x_feat_name, y = y_feat_name, z = "pred"), 
        data = grid, 
        colour = "white"
      ) +
      metR::geom_text_contour(
        data = grid, 
        mapping = ggplot2::aes_string(x = x_feat_name, y = y_feat_name, z = "pred"),
        colour = "white"
      )
    
    if (nrow(cfactuals_plotted) > 0L) {
      p = p + 
        ggplot2::geom_point(
          data = cfactuals_plotted, 
          mapping = ggplot2::aes_string(x = x_feat_name, y = y_feat_name), 
          colour = "black"
        )
    }
    p = p + 
      ggplot2::geom_point(
        data = x_interest_with_pred, 
        mapping = ggplot2::aes_string(x = x_feat_name, y = y_feat_name), 
        colour = "white"
      )
    p = ggExtra::ggMarginal(p, type = "histogram")
    
  } else if (all_plot_feats_categorical) {
    
    var_levels = param_set$levels
    exp_grid = expand.grid(var_levels)
    x_interest_sub = x_interest[, !names(x_interest) %in% feature_names, with = FALSE]
    instance_dt = x_interest_sub[rep(1:nrow(x_interest_sub), nrow(exp_grid))]
    
    grid_df = cbind(instance_dt, exp_grid)
    pred = prediction_function(grid_df)[[pred_column]]
    
    grid_pred = cbind(grid_df, pred)
    
    p = ggplot2::ggplot(grid_pred, ggplot2::aes_string(x_feat_name, y_feat_name)) +
      ggplot2::geom_tile(ggplot2::aes_string(fill = pred)) +
      ggplot2::geom_jitter(width = 0.2, height = 0.2) +
      ggplot2::geom_jitter(data = x_interest, ggplot2::aes_string(x_feat_name, y_feat_name), color = "white")

  }  else {
    
    # dt$pred = predictor$predict(dt)[, 1L]
    # cat_feature = plot_feat_names[feat_types[plot_feat_names] == "categorical"]
    # num_feature = setdiff(plot_feat_names[1:2], cat_feature)
    # 
    # p = ggplot2::ggplot() +
    #   # ggplot2::geom_point(
    #   #   data = dt,
    #   #   mapping = ggplot2::aes_string(x = num_feature, y = "pred"),
    #   #   color = "white"
    #   # ) +
    #   ggplot2::geom_line(
    #     data = grid, 
    #     mapping = ggplot2::aes_string(x = num_feature, y = "pred", group = cat_feature, color = cat_feature)
    #   ) +
    #   ggplot2::theme_bw()
    # 
    # if (nrow(cfactuals_plotted) > 0L) {
    #   p = p + 
    #     ggplot2::geom_point(
    #       data = cfactuals_plotted, 
    #       mapping = ggplot2::aes_string(x = num_feature, y = "pred"), colour = "black"
    #     )
    # }
    # 
    # p = p + 
    #   ggplot2::geom_point(
    #     data = x_interest_with_pred,
    #     mapping = ggplot2::aes_string(x = num_feature, y = "pred"), 
    #     colour = "gray"
    #   )
    # 
    # p = ggExtra::ggMarginal(p, type = "histogram", margins = "x")
  }
  
  p
}
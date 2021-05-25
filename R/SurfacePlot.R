SurfacePlot = R6::R6Class("SurfacePlot",
  public = list(
    initialize = function(arg_list) {
      private$run_init_arg_checks(arg_list)
      private$arg_list = arg_list
    },

    plot = function() {
      cfactuals_plotted = private$get_cfactuals_plotted() 
      
      ice_curve_area = private$make_ice_curve_area()
      
      arg_list = private$arg_list
      x_interest_with_pred = cbind(arg_list$x_interest, pred = arg_list$y_hat_interest[[arg_list$pred_column]])
      private$plot_ice_curve_area(ice_curve_area, arg_list$predictor, cfactuals_plotted, x_interest_with_pred)
    }

  ),
  private = list(
    arg_list = NULL,
    
    run_init_arg_checks = function(arg_list) {
      # assert_integerish(grid_size, len = 1L)
      # assert_numeric(epsilon, len = 1L, null.ok = TRUE)
      # private$throw_error_if_no_results()
      # TODO: Also check that nr_changed in results$counterfactuals_diff
    },
   
    
    get_cfactuals_plotted = function() {
      arg_list = private$arg_list
      
      results = arg_list$results
      epsilon = arg_list$epsilon
      cfactuals = results$counterfactuals
      n_changes = cfactuals$nr_changed
      
      feature_names = arg_list$feature_names
      diff_rel_feats = results$counterfactuals_diff[, ..feature_names]
      n_changes_rel_feats = rowSums(diff_rel_feats != 0)
      has_changes_in_rel_feats_only = (n_changes_rel_feats == n_changes)
      cfactuals_plotted = cfactuals[which(has_changes_in_rel_feats_only)]
      
      dist_target_col_exists = "dist_target" %in% names(cfactuals)
      if (dist_target_col_exists & !is.null(epsilon)) {
        cfactuals_plotted = cfactuals_plotted[dist_target <= epsilon]
      }
      cfactuals_plotted
    },
    
    make_ice_curve_area = function() {
      
      arg_list = private$arg_list
      x_interest = arg_list$x_interest
      feature_names = arg_list$feature_names
      
      param_set_filtered = ParamHelpers::filterParams(arg_list$param_set, feature_names)
      min_max = data.frame(
        rbind(ParamHelpers::getLower(param_set_filtered), ParamHelpers::getUpper(param_set_filtered))
      )
      
      values = ParamHelpers::getValues(param_set_filtered)
      ls_values = lapply(values, function(x) unlist(x, use.names = FALSE))
      ls_min_max_or_values = c(as.list(min_max), ls_values)
      
      grids = sapply(feature_names, function(feat) {
        as.data.frame(equidistant.grid(ls_min_max_or_values[[feat]], arg_list$grid_size))
      })

      # stringsAsFactors needs to be set to TRUE as this can lead to factors that are not contained in
      # the original training data, which might resul in an error in the predict function
      expgrid = expand.grid(a = grids[[1L]], b = grids[[2L]], stringsAsFactors = FALSE)
      colnames(expgrid) = feature_names
      x_interest = x_interest[, !names(x_interest) %in% feature_names, with = FALSE]
      instance_dt = x_interest[rep(1:nrow(x_interest), nrow(expgrid))]
      
      if (nrow(instance_dt) > 0L) {
        grid_df = cbind(instance_dt, expgrid)
      } else {
        grid_df = expgrid
      }
      
      pred = arg_list$predictor$predict(grid_df)[[arg_list$pred_column]]
      cbind(expgrid, pred)
    },
    
    plot_ice_curve_area = function(grid, predictor, instances = NULL, x_interest_with_pred) {
      
      plot_feat_names = names(grid)[1:2]
      dt = predictor$data$get.x()
      feat_types = predictor$data$feature.types
      
      all_plot_feats_numeric = all(feat_types[plot_feat_names] %in% "numerical")
      all_plot_feats_categorical = all(feat_types[plot_feat_names] %in% "categorical")
      
      # TODO: all_plot_feats_categorical -> heat map or something
      if (all_plot_feats_numeric || all_plot_feats_categorical) {
        
        x_feat_name = plot_feat_names[1L]
        y_feat_name = plot_feat_names[2L]
        
        p = ggplot()  +
          geom_point(
            data = dt,
            mapping = aes_string(x = x_feat_name, y = y_feat_name),
            color = "white"
          ) +
          theme_bw() +
          geom_tile(
            data = grid,
            mapping = aes_string(x = x_feat_name, y = y_feat_name, fill = "pred")
          ) +
          stat_contour(
            mapping = aes_string(x = x_feat_name, y = y_feat_name, z = "pred"), 
            data = grid, 
            colour = "white"
          ) +
          metR::geom_text_contour(
            data = grid, 
            mapping = aes_string(x = x_feat_name, y = y_feat_name, z = "pred"),
            colour = "white"
          )
        
        if (nrow(instances) > 0L) {
          p = p + 
            geom_point(
              data = instances, 
              mapping = aes_string(x = x_feat_name, y = y_feat_name), 
              colour = "black"
            )
        }
        p = p + 
          geom_point(
            data = x_interest_with_pred, 
            mapping = aes_string(x = x_feat_name, y = y_feat_name), 
            colour = "white"
          )
        p = ggExtra::ggMarginal(p, type = "histogram")
        
      } else {
        
        dt$pred = predictor$predict(dt)[, 1L]
        cat_feature = plot_feat_names[feat_types[plot_feat_names] == "categorical"]
        num_feature = setdiff(plot_feat_names[1:2], cat_feature)
        
        p = ggplot() +
          geom_point(
            data = dt,
            mapping = aes_string(x = num_feature, y = "pred"),
            color = "white"
          ) +
          geom_line(
            data = grid, 
            mapping = aes_string(x = num_feature, y = "pred", group = cat_feature, color = cat_feature)
          ) +
          theme_bw()
        
        if (nrow(instances) > 0L) {
          p = p + 
            geom_point(
              data = instances, 
              mapping = aes_string(x = num_feature, y = "pred"), colour = "black"
            )
        }
        
        p = p + 
          geom_point(
            data = x_interest_with_pred,
            mapping = aes_string(x = num_feature, y = "pred"), 
            colour = "gray"
          )
        
        p = ggExtra::ggMarginal(p, type = "histogram", margins = "x")
      }
      
      p
    }
    
  )

)

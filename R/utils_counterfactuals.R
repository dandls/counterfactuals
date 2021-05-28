make_param_set = function(dt, lower, upper) {
  param_list = lapply(names(dt), function(col_name){
    column = dt[[col_name]]
    
    # lower bound
    if (col_name %in% names(lower)) {
      lb = lower[[col_name]]
    } else {
      lb = ifelse(is.numeric(column), min(column, na.rm = TRUE), NA)
    }
    
    # upper bound
    if (col_name %in% names(upper)) {
      ub = upper[[col_name]]
    } else {
      ub = ifelse(is.numeric(column), max(column, na.rm = TRUE), NA)
    }
    
    # make param
    if (is.double(column)) {
      param = paradox::ParamDbl$new(col_name, lower = lb, upper = ub)
    } else if (is.integer(column)) {
      param = paradox::ParamInt$new(col_name, lower = lb, upper = ub)
    } else {
      if (is.character(column)) {
        levels = unique(column)
      } else {
        levels = levels(column)
      }
      param = paradox::ParamFct$new(col_name, levels = levels)
    }
    
    param
  })

  paradox::ParamSet$new(param_list)
}


# 1D Grid
equidistant.grid = function(feature, grid_size, integer = FALSE) {
  if (is.numeric(feature)) {
    feature = feature[is.finite(feature)]
    if (!length(feature)) stop("Feature without any finite values")
    gr <- seq(from = min(feature), to = max(feature), length.out = grid_size)
    if (integer) {
      gr <- unique(as.integer(gr))
    }
    data.frame(grid = gr)
  } else {
    data.frame(grid = unique(feature))
  }
}



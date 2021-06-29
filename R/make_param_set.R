#' Creates a ParamSet for the features of a data.table.
#' 
#' @description 
#' Creates a \link[paradox]{ParamSet} for the columns of `dt`. Depending on the class of a column, a different
#' \link[paradox]{Param} is created:
#' * `double`: \link[paradox]{ParamDbl}
#' * `integer`: \link[paradox]{ParamInt}
#' * `character`: \link[paradox]{ParamFct} (with unique values as levels)
#' * `factor`: \link[paradox]{ParamFct} (with factor levels as levels)
#' 
#' @param dt (`data.table()`)\cr
#'  The data for the \link[paradox]{ParamSet}.
#' @param lower (numeric() | NULL)\cr
#' Vector of minimum values for numeric features. If not NULL, it should be named with the corresponding feature names. 
#' If NULL (default) lower is taken for each numeric feature as its minimum value in `dt`.
#' @param upper (numeric() | NULL)\cr
#' Vector of maximum values for numeric features. If not NULL, it should be named with the corresponding feature names. 
#' If NULL (default) upper is taken for each numeric feature as its maximum value in `dt`.
#' 
#' @return A \link[paradox]{ParamSet} with the features of `dt`.
#' 
#' @md
make_param_set = function(dt, lower = NULL, upper = NULL) {
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
      param = ParamDbl$new(col_name, lower = lb, upper = ub)
    } else if (is.integer(column)) {
      param = ParamInt$new(col_name, lower = lb, upper = ub)
    } else if (is.character(column)) {
      param = ParamFct$new(col_name, levels = unique(column))
    } else {
      param = ParamFct$new(col_name, levels = levels(column))
    }
    
    param
  })

  ParamSet$new(param_list)
}



#' ParamSetMaker R6 Class
#' 
#' @description  
#' Extracts information about each feature from input dataset
#' 
#' @details   
#' Based on a data set and optionally specified lower and upper bounds for each feature, it creates a 
#' [ParamSet][ParamHelpers::makeParamSet].
ParamSetMaker = R6::R6Class("ParamSetMaker",
  public = list(
    #' @description
    #' Create a new `ParamSetMaker` object.
    #' @param data [[data.frame()]] \cr A data set.
    #' @param lower [[numeric()]]\cr Named vector of minimum values for numeric features. 
    #' A subset of features can also be specified. If NULL the minimum values are extracted from `data`.
    #' @param upper [[numeric()]]\cr Named vector of maximum values for numeric features.
    #' A subset of features can also be specified. If NULL the maximum values are extracted from `data`.
    #' @return A new `ParamSetMaker` object.
    initialize = function(data, lower = NULL, upper = NULL) {
      assert_data_frame(data)
      assert_numeric(lower, null.ok = TRUE)
      assert_numeric(upper, null.ok = TRUE)
      assert_true(all(names(lower) %in% names(data)))
      assert_true(all(names(upper) %in% names(data)))
      
      private$data = data
      private$lower = lower
      private$upper = upper
    },
    
    #' @description
    #' Makes a [ParamSet][ParamHelpers::makeParamSet].
    #' 
    #' @return A [ParamSet][ParamHelpers::makeParamSet].
    make_param_set = function() {
      param_list = lapply(names(private$data), private$make_param)
      ParamHelpers::makeParamSet(params = param_list)
    }

  ),
  private = list(
    data = NULL,
    lower = NULL,
    upper = NULL,

    make_param = function(column_name) {
      column = private$data[[column_name]]
      col_lower = private$set_lower_bound(column, column_name, private$lower)
      col_upper = private$set_upper_bound(column, column_name, private$upper)
      
      if (is.double(column)) {
        param = ParamHelpers::makeNumericParam(column_name, lower = col_lower, upper = col_upper)
      } else if (is.integer(column)) {
        param = ParamHelpers::makeIntegerParam(column_name, lower = col_lower, upper = col_upper)
      } else {
        if (is.character(column)) {
          values = unique(column)
        } else {
          values = char_to_factor(levels(column))
        }
        param = ParamHelpers::makeDiscreteParam(column_name, values = values)
      }
      
      param
    },
    
    set_lower_bound = function(column, column_name, lower) {
      if (column_name %in% names(lower)) {
        col_lower = lower[[column_name]]
      } else {
        col_lower = ifelse(is.numeric(column), min(column, na.rm = TRUE), NA)
      }
      col_lower
    },
    
    set_upper_bound = function(column, column_name, upper) {
      if (column_name %in% names(upper)) {
        col_upper = upper[[column_name]]
      } else {
        col_upper = ifelse(is.numeric(column), max(column, na.rm = TRUE), NA)
      }
      col_upper
    }
  )
)

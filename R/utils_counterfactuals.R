make_ice_curve_area = function(x_interest, features, predictor, param_set, grid_size) {
  
  param_set_filtered = ParamHelpers::filterParams(param_set, features)
  min_max = data.frame(
    rbind(ParamHelpers::getLower(param_set_filtered), ParamHelpers::getUpper(param_set_filtered))
  )
  
  values = ParamHelpers::getValues(param_set_filtered)
  ls_values = lapply(values, function(x) unlist(x, use.names = FALSE))
  ls_min_max_or_values = c(as.list(min_max), ls_values)
  
  grids = sapply(features, function(feat) {
    as.data.frame(equidistant.grid(ls_min_max_or_values[[feat]], grid_size))
  })
  
  # stringsAsFactors needs to be set to TRUE as this can lead to factors that are not contained in
  # the original training data, which might resul in an error in the predict function
  expgrid = expand.grid(a = grids[[1L]], b = grids[[2L]], stringsAsFactors = FALSE)
  colnames(expgrid) = features
  x_interest = x_interest[, !names(x_interest) %in% features, with = FALSE]
  instance_dt = x_interest[rep(1:nrow(x_interest), nrow(expgrid))]
  
  if (nrow(instance_dt) > 0L) {
    grid_df = cbind(instance_dt, expgrid)
  } else {
    grid_df = expgrid
  }
  
  pred = predictor$predict(newdata = grid_df)[[1L]]
  cbind(expgrid, pred)
}

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

###### Utils Counterfactual #####

#' Create a list with named vectors of standard deviation
#'
#' Will be grouped by the feature type extracted from param.set.
#' @section Arguments:
#' \describe{
#' \item{sdev: }{(numeric)\cr Vector of standard deviations.}
#' \item{param.set: }{(ParamSet)\cr Paramset to create elements of list,
#' one for digit features and one for integer features.}
#' }
#' @return (list)
# sdev_to_list = function(sdev, param.set) {
#   checkmate::assert_numeric(sdev, any.missing = FALSE)
#   checkmate::assert_class(param.set, "ParamSet")
#   checkmate::assert_true(all(names(sdev) %in% ParamHelpers::getParamIds(param.set)))
#   param.ids = ParamHelpers::getParamIds(param.set)
#   paramtypes = gsub("vector$", "", ParamHelpers::getParamTypes(param.set))
#   needed_type = c("numeric", "integer")
#   typegroups = sapply(needed_type, function(type) {
#     sdev[param.ids[paramtypes == type]]
#   }, simplify = FALSE)
#   return(typegroups)
# }
















#' Get difference between two feature vectors
#'
#' Difference is 0 if two characters or factors are equal.
#' Otherwise the class of x is displayed.
#' @section Arguments:
#' \describe{
#' \item{x: }{(data.frame)\cr Data frame with one row.}
#' \item{x.interest: }{(data.frame)\cr Data frame with one row.}
#' }
#' @return (data.frame)
#' get_diff = function(x, x.interest) {
#'   assertDataFrame(x, ncols = ncol(x.interest))
#'   assertDataFrame(x.interest, nrows = 1)
#' 
#'   diff = data.frame(matrix(data = NA, nrow = nrow(x), ncol = length(x.interest)))
#'   names(diff) = names(x.interest)
#' 
#'   x.interest = x.interest[rep(row.names(x.interest), nrow(x)), ]
#' 
#'   for (i in 1:ncol(x)) {
#'     if (class(x[,i]) == "character"|
#'         class(x[,i]) == "factor") {
#'       p.val = as.character(x[,i])
#'       x.val = as.character(x.interest[,i])
#'       diff[,i] = ifelse(p.val != x.val, p.val, "0")
#'     } else {
#'       diff[,i] = x[,i] - x.interest[,i]
#'     }
#'   }
#'   return(diff)
#' }
#' 
#' #' Transform features to x.interest
#' #'
#' #' Replace features of x by value of x.interest
#' #' where use.orig is set to TRUE.
#' #'
#' #' @section Arguments:
#' #' \describe{
#' #' \item{x: }{(list)\cr List of features, must have list element
#' #' use.origin.}
#' #' \item{x.interest: }{(data.frame)\cr Data frame with one row.}
#' #' \item{delete.use.orig: }{(logical(1))\cr Whether to
#' #' delete list element use.orig of x.}
#' #' \item{fixed.features: }{(character)\cr
#' #' Indicate fixed features by feature name.}
#' #' \item{max.changed: }{numeric(1)\cr Number indicating
#' #' how many feature are allowed to maximially differ from the original data point.}
#' #' }
#' #'
#' #' @return (list)
#' transform_to_orig = function(x, x.interest, delete.use.orig = FALSE,
#'   fixed.features = NULL, max.changed = NULL) {
#'   checkmate::assert_list(x, len = ncol(x.interest) + 1)
#'   checkmate::assert_data_frame(x.interest, any.missing = FALSE, nrows = 1)
#'   checkmate::assert(
#'     check_character(fixed.features, null.ok = TRUE),
#'     check_true(fixed.features %in% names(x))
#'   )
#'   checkmate::assert_integerish(max.changed, null.ok = TRUE)
#'   types = lapply(x[names(x)!="use.orig"], class)
#' 
#'   if (!is.null(fixed.features)) {
#'     pos = which(names(x) %in% fixed.features)
#'     x$use.orig[pos] = TRUE
#'   }
#' 
#'   use.orig = x$use.orig
#'   if (!is.null(max.changed)) {
#'     n.changed = sum(!use.orig)
#'     if (n.changed > max.changed) {
#'       n = n.changed - max.changed
#'       mut.idx = sample(which(!use.orig), n)
#'       use.orig[mut.idx] = TRUE
#'     }
#'   }
#'   x$use.orig = NULL
#'   x[use.orig] = x.interest[use.orig]
#'   types.after.trans = lapply(x, class)
#'   if (!identical(types, types.after.trans)) {
#'     stop("setting values to x.interest values introduced a type shift")
#'   }
#'   if (!delete.use.orig) {
#'     x$use.orig = use.orig
#'   }
#'   return(x)
#' }
#' 
#' 
#' 
#' #' Round numeric elements in data frame
#' #' @section Arguments:
#' #' \describe{
#' #' \item{df: }{(data.frame)\cr Data frame in which numeric elments will be rounded.}
#' #' \item{digits: }{integer(1)\cr Number of decimal places used. Default is 3.}
#' #' }
#' #' @return (data.frame) with rounded elements
#' round_df = function(df, digits = 3) {
#'   nums = vapply(df, is.numeric, FUN.VALUE = logical(1))
#'   df[,nums] = round(df[,nums], digits = digits)
#'   return(df)
#' }
#' 
#' #' Calculate ice curve variance over all features
#' #'
#' #' @section Arguments:
#' #' \describe{
#' #' \item{x.interest: }{(data.frame)\cr Data point, for which ice curves
#' #' should be calculated.}
#' #' \item{predictor: }{(Predictor)\cr Object, that holds the prediction model and dataset.}
#' #' \item{param.set: }{(ParamSet)\cr Parameter set of features in dataset.}
#' #' }
#' #' @return (numeric) Vector with standard deviations for each feature.
#' get_ICE_var = function(x.interest, predictor, param.set) {
#'   min.max = as.data.frame(rbind(getLower(param.set),
#'     getUpper(param.set)))
#'   val = getValues(param.set)
#'   val$use.orig = NULL
#'   val.l = lapply(val, function(x) unlist(x, use.names = FALSE))
#' 
#'   values = c(as.list(min.max), val.l)
#' 
#'   sd.eff = sapply(names(x.interest), function(x){
#'     get_ice_curve(x.interest, x, predictor, values = values[[x]])
#'   })
#' 
#'   return(sd.eff)
#' 
#' }
#' 
#' # Calculate ice curve variance per feature
#' get_ice_curve = function(instance, feature, predictor, values,
#'   grid.size = 20) {
#'   # determine if integer
#'   INTEGER <- is.integer(instance[, feature])
#'   
#'   # make grid of one feature
#'   grid = as.data.frame(equidistant.grid(values, grid.size, integer = INTEGER))
#'   grid.size = nrow(grid)
#'   colnames(grid) = feature
#'   
#'   instance = instance[, !names(instance) %in% feature]
#'   instance.df = instance[rep(row.names(instance), grid.size), ]
#' 
#'   grid.df = cbind.data.frame(instance.df, grid)
#' 
#'   # predict outcomes
#'   pred = predictor$predict(newdata = grid.df)[[1]]
#' 
#'   # calculate sd
#'   return(sd(pred))
#' 
#' }
#' 
#' 
#' 
#' 
#' 
#' #' #' Algorithm to remove solutions of final set
#' #' #'
#' #' #' @section Arguments:
#' #' #' \describe{
#' #' #' \item{fitness: }{(data.frame)\cr Data frame of fitness values. Each column
#' #' #' represents one objective.}
#' #' #' \item{pareto.set: }{(data.frame)\cr Corresponding Pareto set to
#' #' #' fitness.}
#' #' #' \item{nr.solutions: }{(numeric(1))\cr Number of solutions that should
#' #' #' be returned.}
#' #' #' }
#' #' get_diverse_solutions = function(fitness, pareto.set, nr.solutions) {
#' #'
#' #'   assert_data_frame(fitness, any.missing = FALSE, nrows = nrow(pareto.set))
#' #'   assert_data_frame(pareto.set, any.missing = FALSE)
#' #'   assert_number(nr.solutions)
#' #'
#' #'   n = nrow(pareto.set)
#' #'   max = apply(fitness, 2, max)
#' #'   min = apply(fitness, 2, min)
#' #'   g.dist = StatMatch::gower.dist(pareto.set, KR.corr = FALSE)
#' #'
#' #'   dds = numeric(n)
#' #'   ods = numeric(n)
#' #'
#' #'   for (i in c(1, 2, 3, 4)) {
#' #'
#' #'     ord = order(fitness[i, ])
#' #'
#' #'     # set the extreme values to Inf
#' #'     ods[ord[1]] = Inf
#' #'     ods[ord[n]] = Inf
#' #'     dds[ord[1]] = Inf
#' #'     dds[ord[n]] = Inf
#' #'
#' #'     # update the remaining crowding numbers
#' #'     if (n > 2L) {
#' #'       for (j in 2:(n - 1L)) {
#' #'
#' #'         if (max[i] - min[i] != 0) {
#' #'           ods[ord[j]] = ods[ord[j]] +
#' #'             (abs(fitness[ord[j + 1L], i] - fitness[ord[j - 1L], i])/(max[i]-min[i]))
#' #'         }
#' #'         dds[ord[j]] = dds[ord[j]] +
#' #'           g.dist[ord[j], ord[j-1]] +
#' #'           g.dist[ord[j], ord[j+1]]
#' #'
#' #'       }
#' #'     }
#' #'   }
#' #'   cds = rank(ods) + rank(dds)
#' #'   cds = jitter(cds, factor = 1)
#' #'   idx = order(cds, decreasing = TRUE)[1:nr.solutions]
#' #'   return(idx)
#' #' }

#'
#' #' Calculate diversity
#' #'
#' #' The diversity is equal to pair-wise mean of Gower distances.
#' #'
#' #' @section Arguments:
#' #' \describe{
#' #' \item{df: }{(data.frame)\cr Pareto set.}
#' #' \item{range: }{(numeric)\cr Vector of ranges for numeric features.
#' #' Must have same ordering as columns in df.}
#' #' }
#' #' @return (numeric(1))
#' compute_diversity = function(df, range) {
#'   dis = StatMatch::gower.dist(data.x = df,
#'     rngs = range, KR.corr = FALSE)
#'   single.dist = dis[lower.tri(dis)]
#'   mean.dist = mean(single.dist)
#'   return(mean.dist)
#' }
#'




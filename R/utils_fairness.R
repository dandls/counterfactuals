make_fitness_function_cf = function(predictor, predictor_protected, x_interest, pred_column, weights, k, fixed_features, param_set) {
  function(xdt) {
    # Add values of fixed_features just for prediction
    if (!is.null(fixed_features)) {
      xdt[, (fixed_features) := x_interest[, fixed_features, with = FALSE]]
    }
    xdt = xdt[, names(x_interest), with = FALSE]
    factor_cols = names(which(sapply(predictor$data$X, is.factor)))
    for (factor_col in factor_cols) {
      fact_col_pred = predictor$data$X[[factor_col]]
      value =  factor(xdt[[factor_col]], levels = levels(fact_col_pred), ordered = is.ordered(fact_col_pred))
      set(xdt, j = factor_col, value = value)
    }
    int_cols = names(which(sapply(predictor$data$X, is.integer)))
    if (length(int_cols) > 0L) {
      xdt[,(int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
    }
    ranges = param_set$upper - param_set$lower
    prob_prot = 1 - as.data.table(predictor_protected$predict_newdata(xdt))[[pred_column]]
    dist_x_interest = as.vector(StatMatch::gower.dist(x_interest, xdt, rngs = ranges, KR.corr = FALSE))
    dist_train = gower_topn(x = xdt, y = predictor$data$X, n = k)$distance
    if (!is.null(weights)) {
      dist_train = apply(dist_train, 2L, weighted.mean, w = weights)
    } else {
      dist_train = apply(dist_train, 2L, mean)
    }
    data.table(cbind(prob_prot, dist_x_interest, dist_train))
  }
}

plot_counterfactuals = function(cfactuals, data, attribute = NULL) {
    library("ggplot2")
    require_namespaces("Rtsne")
    setDT(data)
    data[, role := "data"]
    
    cdf = cfactuals$data[, role := "counterfactuals"]
    idf = cfactuals$x_interest[, role := "x_interest"]
    df = rbind(idf, cdf, data[, colnames(cdf), with = FALSE])
    df = unique(df)
    X = model.matrix( ~ ., data = df)
    X = Rtsne::normalize_input(X)
    rtdf = Rtsne::Rtsne(X)$Y
    edf = cbind(data.frame(rtdf), df[, "role", with = FALSE])
    if (!is.null(attribute)) edf = cbind(edf,  df[, attribute, with = FALSE])
    ggplot(edf, aes(x = X1, y = X2, color = role, shape = role, size = role)) +
      geom_point(aes_string(color = eval(attribute))) +
      geom_point(data=edf %>%
          filter(role %in% "counterfactuals"),
        pch = 23,
        size=4,
        colour = "black") +
      theme_minimal() +
      scale_shape_manual(values = c(18,16,15)) +
      scale_size_manual(values = 2*c(3,.7,5)) +
      scale_colour_brewer(palette = "Set1") +
      theme(
          legend.title = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = "bottom"
      )
}

fit_prot_predictor = function(data, new_target) {
    require_namespaces(c("mlr3learners", "mlr3pipelines"))
    newtsk = convert_prot_task(data, new_target)
    lrn = lrn("classif.ranger", predict_type = "prob")
    prot_predictor = as_learner(ppls("robustify", task = newtsk, learner = lrn) %>>% lrn)
    prot_predictor$train(newtsk)
    return(prot_predictor)
}

convert_prot_task = function(data, new_target) {
    TaskClassif$new("pprot", data, new_target)
}


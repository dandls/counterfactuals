ConditionalSampler = R6::R6Class(
  public = list(
    model = NULL,
    
    initialize = function(X, feature_name) {
      assert_data_table(X, min.rows = 1L)
      assert_names(feature_name, subset.of = names(X))
      private$X = X
      private$feature_name = feature_name
      private$y = X[[feature_name]]
      private$fit_conditional()
    },

    sample = function(x, size = 1L) {
      assert_integerish(size, lower = 1L, any.missing = FALSE)
      assert_data_table(x, nrows = 1L)
      y_grid = private$create_y_grid()

      dt_densities = private$comp_conditional_densites(x, y_grid)
      row_idxs = sample.int(nrow(dt_densities), size = size, prob = dt_densities[["density"]], replace = TRUE)
      dt_densities[row_idxs, y]
    }
  ),

  private = list(
    data_nodes = NULL,
    y = NULL,
    X = NULL,
    feature_name = NULL,
    
    fit_conditional = function() {
      if (!requireNamespace("partykit", quietly = TRUE)) {
        stop("Package 'partykit' needed for this function to work. Please install it.", call. = FALSE)
      }
      ctrl = partykit::ctree_control(maxdepth = 5L)
      y_unique = unique(private$y)
      if (is.numeric(y_unique) && length(y_unique[!is.na(y_unique)]) > 5L) {
        yvar = variables::numeric_var(
          private$feature_name, support = c(min(y_unique, na.rm = TRUE), max(y_unique, na.rm = TRUE))
        )
        bernstein_basis_y = basefun::Bernstein_basis(yvar, order = 5L, ui = "increasing")
        cond_transfo_model = mlt::ctm(bernstein_basis_y, todistr = "Normal", data = private$X)
        tree_formula = as.formula(sprintf("%s ~ 1 | .", private$feature_name))
        self$model = trtf::trafotree(cond_transfo_model, formula = tree_formula, data = private$X, control = ctrl)
      } else {
        tree_formula = as.formula(sprintf("%s ~ 1 | .", private$feature_name))
        self$model = partykit::ctree(tree_formula, data = private$X, control = ctrl)
      }
    },
    
    comp_conditional_densites = function(x, y_grid = NULL){
      y = private$y
      if (inherits(self$model, "trafotree")) {
        if (!is.integer(y)) {
          density = as.numeric(predict(self$model, x, type = "logdensity", q = y_grid))
          density = exp(density - max(density))
          density = density / sum(density)
          dt_densities = data.table(y = y_grid, density = density)
        } else {
          if (is.null(private$data_nodes)) {
            private$data_nodes = predict(self$model, private$X, type = "node")
          }
          x_node = predict(self$model, x, type = "node")
          len = min(max(y_grid) - min(y_grid) + 1L, 100L)
          y_sub = y[which(private$data_nodes == x_node)]
          density = density(y_sub, n = len, from = min(y_grid), to = max(y_grid))
          dt_densities = data.table(y = y_grid, density = density$y)
        }
      } else if (is.character(y) || is.factor(y)) {
        density = predict(self$model, x, type = "prob")
        y_grid = factor(colnames(density), levels = levels(y))
        dt_densities = data.table(y = y_grid, density = as.numeric(density))
      } else {
        pr = predict(self$model, x, type = "density")
        unique_y = unique(y)
        density = as.matrix(vapply(pr, function(pr) pr(unique_y) / sum(pr(unique_y)), numeric(length(unique_y))))[, 1L]
        dt_densities = data.table(y = unique_y, density = density)
      }
      dt_densities
    },
    
    create_y_grid = function() {
      y = private$y
      if (is.character(y) || is.factor(y)) {
        y_grid = unique(y)
      } else if (test_integerish(y)) {
        len = min(max(y) - min(y) + 1L, 100L)
        y_grid = as.integer(seq.int(min(y), max(y), length.out = len))
      } else {
        y_grid = seq(from = min(y), to = max(y), length.out = 100L)
      }
      y_grid
    }
  )

)


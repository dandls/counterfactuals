train_AE_model = function(X_train, aep) {
  X_processed = aep$preprocess(X_train)
  AE = AE_model(ncol(X_processed), 2L)
  keras::fit(AE, X_processed, X_processed, batch_size = ceiling(nrow(X_train) / 10L), epochs = 20L, verbose = FALSE)
  AE
}

AE_model = function(input_dim, center_dim) {

  input = keras::layer_input(shape = input_dim)

  encoded = keras::layer_dense(input, units = as.integer(ceiling(input_dim / 2L)), activation = "relu")
  encoded = keras::layer_dense(encoded, units = as.integer(ceiling(input_dim / 4L)), activation = "relu")
  encoded = keras::layer_dense(encoded, units = center_dim, activation = "relu")

  decoded = keras::layer_dense(encoded, units = as.integer(ceiling(input_dim / 4L)), activation = "relu")
  decoded = keras::layer_dense(decoded, units = as.integer(ceiling(input_dim / 2L)), activation = "relu")
  decoded = keras::layer_dense(decoded, units = input_dim, activation = "sigmoid")

  ae = keras::keras_model(input, decoded)
  keras::compile(ae, optimizer = "adam", loss = "mse")
}


AEPreprocessor = R6Class("AEPreprocessor",
  private = list(
    x_min = NULL,
    x_max = NULL,
    ind_num = NULL,
    contrasts = NULL,

    normalize = function(x) {
      (x - private$x_min) / (private$x_max - private$x_min)
    }
  ),
  public = list(
    initialize = function(X) {
      private$ind_num = vapply(X, is.numeric, logical(1L))

      if (any(private$ind_num)) {
        X_num = X[, private$ind_num, with = FALSE]
        private$x_min = vapply(X_num, min, numeric(1L), rm.na = TRUE)
        private$x_max = vapply(X_num, max, numeric(1L), rm.na = TRUE)
      }

      if (!all(private$ind_num)) {
        not_num_cols = !private$ind_num
        X_cat = X[, not_num_cols, with = FALSE]
        private$contrasts = lapply(X_cat, function(x) contrasts(x, contrasts = FALSE))
      }
    },

    preprocess = function(X) {
      res = list()

      if (any(private$ind_num)) {
        res$numeric = private$normalize(X[, private$ind_num, with = FALSE])
      }

      if (!all(private$ind_num)) {
        res$m_cat = model.matrix(~ 0 + ., data = X[, !private$ind_num, with = FALSE], contrasts = private$contrasts)
      }

      as.matrix(do.call(cbind, res))
    }
  )
)

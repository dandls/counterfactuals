#' @param lower (`numeric()` | `NULL`)\cr
#'   Vector of minimum values for numeric features. If not `NULL`, it should be named with the corresponding feature names.
#'   If `NULL` (default) `lower` is taken for each numeric feature as its minimum value in `predictor$data$X`.
#' @param upper (`numeric()` | `NULL`)\cr
#'   Vector of maximum values for numeric features. If not `NULL`, it should be named with the corresponding feature names.
#'   If `NULL` (default) `upper` is taken for each numeric feature as its maximum value in `predictor$data$X`.

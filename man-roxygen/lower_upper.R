#' @param lower (`numeric()` | `NULL`)\cr
#'   Vector of minimum values for numeric features. 
#'   If `NULL` (default), the element for each numeric feature in `lower` is taken as its minimum value in `predictor$data$X`.
#'   If not `NULL`, it should be named with the corresponding feature names.
#' @param upper (`numeric()` | `NULL`)\cr
#'   Vector of maximum values for numeric features. 
#'   If `NULL` (default), the element for each numeric feature in `upper` is taken as its maximum value in `predictor$data$X`.
#'   If not `NULL`, it should be named with the corresponding feature names.

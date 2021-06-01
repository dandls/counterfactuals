make_cfactuals_diff = function(cfactuals, x_interest) {
  names_x_interest = names(x_interest)
  diff_temp = add_diff_numeric_cols(cfactuals[, ..names_x_interest], x_interest)
  diff = add_diff_non_numeric_cols(diff_temp, x_interest)
  cfactuals_diff = data.table::copy(cfactuals)
  data.table::set(cfactuals_diff, j = names_x_interest, value = diff)
  cfactuals_diff
}

add_diff_numeric_cols = function(dt, x_interest) {
  idx_numeric = which(sapply(dt, test_numeric))
  if (length(idx_numeric) == 0) {
    return(dt)
  }
  m_num = as.matrix(dt[, ..idx_numeric])
  x_interest_num = as.numeric(x_interest[1L , ..idx_numeric])
  diff_num = data.table::as.data.table(sweep(m_num, 2, x_interest_num))
  data.table::set(dt, j = idx_numeric, value = diff_num)
  dt
}

add_diff_non_numeric_cols = function(dt, x_interest) {
  idx_non_numeric = which(sapply(dt, function(x) !test_numeric(x)))
  if (length(idx_non_numeric) == 0) {
    return(dt)
  }
  m_char = as.matrix(dt[, ..idx_non_numeric])
  x_interest_char = as.matrix(x_interest[1L , ..idx_non_numeric])
  no_diff = sweep(m_char, 2, x_interest_char, FUN = "==")
  m_char[no_diff] = "0"
  diff_char = data.table::as.data.table(m_char)
  data.table::set(dt, j = idx_non_numeric, value = diff_char)
  dt
}
make_cfactuals_diff = function(cfactuals, x_interest) {
  cfactuals_diff = as.data.table(matrix(nrow = nrow(cfactuals), ncol = ncol(cfactuals)))
  setnames(cfactuals_diff, old = names(cfactuals_diff), new = names(cfactuals))
  if (nrow(cfactuals_diff) == 0L) {
    return(cfactuals_diff)
  }
  
  is_numeric_col = sapply(cfactuals, test_numeric)
  idx_numeric = which(is_numeric_col)
  idx_non_numeric = which(!is_numeric_col)
  
  if (length(idx_numeric) > 0L) {
    m_num = as.matrix(cfactuals[, ..idx_numeric])
    x_interest_num = as.numeric(x_interest[1L , ..idx_numeric])
    diff_num = data.table::as.data.table(sweep(m_num, 2, x_interest_num))
    diff_num[diff_num == 0] = NA
    data.table::set(cfactuals_diff, j = idx_numeric, value = diff_num)
  }
  
  if (length(idx_non_numeric) > 0L) {
    m_char = as.matrix(cfactuals[, ..idx_non_numeric])
    x_interest_char = as.matrix(x_interest[1L , ..idx_non_numeric])
    no_diff = sweep(m_char, 2L, x_interest_char, FUN = "==")
    m_char[no_diff] = NA
    diff_char = data.table::as.data.table(m_char)
    data.table::set(cfactuals_diff, j = idx_non_numeric, value = diff_char)
  }
  
  cfactuals_diff
}

count_changes = function(cfactuals, x_interest) {
  assert_data_table(cfactuals)
  assert_data_table(x_interest, nrows = 1L)
  assert_true(ncol(cfactuals) == ncol(x_interest))
  assert_true(all(names(cfactuals) == names(x_interest)))
  cfactuals_temp = copy(cfactuals)
  cfactuals_temp[, n_changes := sum(.SD != x_interest), by = seq_len(nrow(cfactuals_temp))]
  as.integer(cfactuals_temp$n_changes)
}



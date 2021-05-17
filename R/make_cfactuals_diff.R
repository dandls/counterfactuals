make_cfactuals_diff = function(cfactuals, x_interest) {
  names_x_interest = names(x_interest)
  diff = comp_cfactuals_diff(cfactuals[, ..names_x_interest], x_interest)
  cfactuals_diff = data.table::copy(cfactuals)
  data.table::set(cfactuals_diff, j = names_x_interest, value = diff)
  cfactuals_diff
}

comp_cfactuals_diff = function(cfactuals, x_interest) {
  diff_temp = add_diff_numeric_cols(cfactuals, x_interest)
  diff = add_diff_non_numeric_cols(diff_temp, x_interest)
  diff  # COMMENT don't need this, just have 'add_diff_non_numeric..' be the last line here
}

add_diff_numeric_cols = function(dt, x_interest) {
  idx_numeric = which(sapply(dt, checkmate::test_numeric))
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
  idx_non_numeric = which(sapply(dt, function(x) !checkmate::test_numeric(x)))  # COMMENT some of the calls are with `::`, some are not. I think you should just import data.table and checkmate and use them without :: because they are used everywhere. with partykit OTOH you could still use ::
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

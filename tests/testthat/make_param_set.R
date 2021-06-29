test_that("make_param_set returns correct ParamSet", {
  set.seed(454541524)
  discval = c("a", "b", "c")
  df = data.frame(
    num = runif(10, 0, 1),
    char = sample(discval, size = 10L, replace = TRUE),
    fact = factor(sample(discval, size = 10L, replace = TRUE), levels = c(discval)),
    int = sample(1:20, size = 10L)
  )
  
  # no lower or upper
  ps = make_param_set(df, lower = NULL, upper = NULL)
  exp_ps_class = c("ParamDbl", "ParamFct", "ParamFct", "ParamInt")
  names(exp_ps_class) = names(df)
  expect_identical(ps$class, exp_ps_class)
  expect_identical(ps$upper, c(num = max(df$num), char = NA, fact = NA, int = max(df$int)))
  expect_identical(ps$lower, c(num = min(df$num), char = NA, fact = NA, int = min(df$int)))
  
  # manually set lower
  lower = c(-1, 0L)
  names(lower) = c("num", "int")
  ps_low = make_param_set(df, lower = lower, upper = NULL)
  expect_identical(ps_low$lower, c(num = -1, char = NA, fact = NA, int = 0L))
  
  # manually set upper
  upper = c(1, 11L)
  names(upper) = c("num", "int")
  ps_upper = make_param_set(df, lower = NULL, upper = upper)
  expect_identical(ps_upper$upper, c(num = 1, char = NA, fact = NA, int = 11L))
})

test_that("$make_param_set() returns error if values in the data are lower (higher) then lower (upper) bounds", {
  df = data.frame(num = 1:5)
  lower = 100L
  names(lower) = "num"
  expect_snapshot_error(make_param_set(df, lower = lower, upper = NULL))
})


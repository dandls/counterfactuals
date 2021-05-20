test_that("ParamSetMaker returns correct ParamSet", {
  set.seed(454541524)
  discval = c("a", "b", "c")
  test_df = data.frame(
    num = runif(10, 0, 1),
    char = sample(discval, size = 10L, replace = TRUE),
    fact = factor(sample(discval, size = 10L, replace = TRUE), levels = c(discval)),
    int = sample(1:20, size = 10L)
  )

  param_set_maker = ParamSetMaker$new(test_df)
  ps = param_set_maker$make_param_set()
  ps_values = getValues(ps)
  ps_upper = getUpper(ps)
  ps_lower = getLower(ps)

  expect_true(all(c("num", "char", "fact", "int") %in% getParamIds(ps)))
  invisible(sapply(ps_values[[1L]], expect_class, "character"))
  invisible(sapply(ps_values[[2L]], expect_class, "factor"))
  expect_identical(
    getParamTypes(ps), c("numeric", "discrete", "discrete", "integer")
  )
  expect_identical(ps_upper, c(num = max(test_df$num), int = max(test_df$int)))
  expect_identical(ps_lower, c(num = min(test_df$num), int = min(test_df$int)))

  # manually set lower
  lower = c(-1, 0L)
  names(lower) = c("num", "int")
  ps_maker_low = ParamSetMaker$new(test_df, lower = lower)
  ps_low = ps_maker_low$make_param_set()
  expect_identical(getLower(ps_low), lower)

  # manually set upper
  upper = c(1, 11)
  names(upper) = c("num", "int")
  ps_maker_upper = ParamSetMaker$new(test_df, upper = upper)
  ps_upper = ps_maker_upper$make_param_set()
  expect_identical(getUpper(ps_upper), upper)
})

test_that("$make_param_set() returns error if values in the data are lower (higher) then lower (upper) bounds", {
  test_df = data.frame(num = 1:5)
  lower = 100L
  names(lower) = "num"
  ps_maker = ParamSetMaker$new(test_df, lower)
  expect_snapshot_error(ps_maker$make_param_set())
})


test_that("$initialization returns error if wrong names in upper and/or lower", {
  test_df = data.frame(num = runif(10, 0, 1), int = 1:10)
  vals = c(1, 5L)
  names(vals) = c("num3", "int")
  expect_snapshot_error(ParamSetMaker$new(test_df, upper = vals))
  expect_snapshot_error(ParamSetMaker$new(test_df, lower = vals))
})

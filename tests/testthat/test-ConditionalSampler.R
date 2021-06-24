test_that("Correct output format for double y", {
  set.seed(454564)
  dt = as.data.table(mtcars)
  dt$am = as.factor(dt$am)
  cs = ConditionalSampler$new(dt, feature_name = "mpg")
  samples = cs$sample(dt[2L, ], 20L)
  expect_numeric(samples, any.missing = FALSE, len = 20L)
})

test_that("Correct output format for integer y", {
  set.seed(454564)
  dt = as.data.table(mtcars)
  dt$carb = as.integer(dt$carb)
  cs = ConditionalSampler$new(dt, feature_name = "carb")
  samples = cs$sample(dt[5L, ], 10L)
  expect_integer(samples, any.missing = FALSE, len = 10L)
})

test_that("Correct output format for factor y", {
  set.seed(454564)
  dt = as.data.table(mtcars)
  dt$am = as.factor(dt$am)
  cs = ConditionalSampler$new(dt, feature_name = "am")
  samples = cs$sample(dt[10L, ], 20)
  expect_factor(samples, levels = levels(dt$am))
})

test_that("same results of gower distances for factor and character", {
  n = 5
  set.seed(1220)
  # with factor column
  traindata = data.table(X1 = as.numeric(sample(1:3, size = n, replace = TRUE)), 
    X2 = as.factor(sample(c("a", "b", "d"), size = n, replace = TRUE)))
  cfs = traindata[1:2,]
  gtf = gower_dist_c(cfs, traindata, data = traindata, k = n)
  
  # with character column
  traindata$X2 = as.character(traindata$X2)
  cfs = traindata[1:2,]
  gtc = gower_dist_c(cfs, traindata, data = traindata, k = n)
  
  expect_equal(gtf, gtc)
  
  # with statmatch
  st = t(apply(StatMatch::gower.dist(cfs, traindata), MARGIN = 1L, FUN = sort))
  expect_equal(matrix(gtc), matrix(st))
}) 
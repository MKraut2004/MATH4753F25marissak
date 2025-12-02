library(testthat)

test_that("myncurve returns correct mu", {
  result <- myncurve(10, 5, 6)
  expect_equal(result$mu, 10)
})

test_that("myncurve returns correct sigma", {
  result <- myncurve(10, 5, 6)
  expect_equal(result$sigma, 5)
})

test_that("myncurve returns correct P_X_le_a", {
  result <- myncurve(10, 5, 6)
  expect_equal(result$P_X_le_a, pnorm(6, mean = 10, sd = 5))
})

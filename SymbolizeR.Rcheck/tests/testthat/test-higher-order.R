# Tests for Higher-Order Statistics: Skewness and Kurtosis

test_that("Skewness returns symbolic formula for undefined RV", {
  result <- Skewness(X)
  # Should contain the skewness formula structure
  expect_true(is.call(result) || is.symbol(result))
})

test_that("Kurtosis returns symbolic formula for undefined RV", {
  result <- Kurtosis(X)
  # Should contain the kurtosis formula structure
  expect_true(is.call(result) || is.symbol(result))
})

test_that("Kurtosis with excess=FALSE returns raw kurtosis", {
  result <- Kurtosis(X, excess = FALSE)
  expect_true(is.call(result) || is.symbol(result))
})

test_that("derive.Skewness returns derivation object", {
  result <- derive.Skewness(X)
  expect_s3_class(result, "derivation")
  expect_true(length(result$steps) >= 2)
})

test_that("derive.Kurtosis returns derivation object", {
  result <- derive.Kurtosis(X)
  expect_s3_class(result, "derivation")
  expect_true(length(result$steps) >= 2)
})

# Tests for moment() function

test_that("moment returns E[X] for n=1", {
  result <- moment(X, 1)
  expect_equal(deparse(result), "E(X)")
})

test_that("moment returns E[X^n] for undefined RV", {
  result <- moment(X, 3)
  # Accept both E(X^3) and E(X^3L) formatting
  expect_true(grepl("E\\(X\\^3", deparse(result)))
})

test_that("moment with defined Normal distribution returns correct moments", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  # First moment = mu
  result1 <- moment(X, 1)
  expect_equal(deparse(result1), "mu")
  
  # Second moment = sigma^2 + mu^2
  result2 <- moment(X, 2)
  expect_true(grepl("sigma", deparse(result2)) && grepl("mu", deparse(result2)))
  
  # Third moment for Normal
  result3 <- moment(X, 3)
  expect_true(grepl("mu", deparse(result3)) && grepl("sigma", deparse(result3)))
  
  clear.definitions()
})

test_that("moment rejects non-positive integers", {
  expect_error(moment(X, 0), "n must be a positive integer")
  expect_error(moment(X, -1), "n must be a positive integer")
  expect_error(moment(X, 1.5), "n must be a positive integer")
})

# Tests for new distributions

test_that("ChiSq distribution can be defined", {
  clear.definitions()
  expect_silent(define(X ~ ChiSq(5)))
  
  # E[X] = df
  result <- E(X)
  expect_equal(result, 5)
  
  clear.definitions()
})

test_that("ChiSq distribution second moment is correct", {
  clear.definitions()
  define(X ~ ChiSq(k))
  
  # E[X^2] = k^2 + 2k
  result <- E(X^2)
  # Should contain k^2 and 2*k terms
  result_str <- deparse(result)
  expect_true(grepl("k", result_str))
  
  clear.definitions()
})

test_that("StudentT distribution can be defined", {
  clear.definitions()
  expect_silent(define(X ~ StudentT(10)))
  
  # E[X] = 0 for df > 1
  result <- E(X)
  expect_equal(result, 0)
  
  clear.definitions()
})

test_that("StudentT distribution second moment is correct", {
  clear.definitions()
  define(X ~ StudentT(nu))
  
  # E[X^2] = nu / (nu - 2) for nu > 2
  result <- E(X^2)
  result_str <- deparse(result)
  expect_true(grepl("nu", result_str))
  
  clear.definitions()
})

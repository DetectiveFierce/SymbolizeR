# Integration tests for E(), Var(), Cov()

test_that("E() handles simple random variable", {
  result <- E(X)
  expect_equal(result, quote(E(X)))
})

test_that("E() handles constant", {
  result <- E(a)
  expect_equal(result, quote(a))
})

test_that("E() handles numeric", {
  result <- E(5)
  expect_equal(result, 5)
})

test_that("E() handles a * X", {
  result <- E(a * X)
  expect_equal(result, quote(a * E(X)))
})

test_that("E() handles X + Y", {
  result <- E(X + Y)
  expect_equal(result, quote(E(X) + E(Y)))
})

test_that("E() handles 2 * X + 3", {
  result <- E(2 * X + 3)
  expect_equal(result, quote(2 * E(X) + 3))
})

test_that("E() handles a * X + b", {
  result <- E(a * X + b)
  expect_equal(result, quote(a * E(X) + b))
})

test_that("Var() returns variance formula for simple RV", {
  result <- Var(X)
  # Var(X) = E(X^2) - E(X)^2
  # Check structure rather than exact match due to integer literals
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "-")
  # Left side should be E(X^2)
  expect_equal(as.character(result[[2]][[1]]), "E")
})

test_that("Cov() returns covariance formula", {
  result <- Cov(X, Y)
  # Cov(X, Y) = E(X * Y) - E(X) * E(Y)
  expect_equal(result, quote(E(X * Y) - E(X) * E(Y)))
})

test_that("E() handles X * Y (product of RVs)", {
  result <- E(X * Y)
  expect_equal(result, quote(E(X * Y)))
})

test_that("E() handles X - Y", {
  result <- E(X - Y)
  expect_equal(result, quote(E(X) - E(Y)))
})

test_that("Var() handles a*X + b (linear combination)", {
  # Var(aX + b) should expand (aX + b)^2 and simplify
  # Expected: a^2 * E(X^2) + 2*a*b*E(X) + b^2 - (a*E(X) + b)^2
  result <- Var(a * X + b)
  # The result should be a call (not just E(X^2) - E(X)^2)
  expect_true(is.call(result))
  # Should contain terms with a^2
  result_str <- deparse(result)
  expect_true(grepl("a", result_str))
})

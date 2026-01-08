# Tests for expect.recursive()

test_that("expect.recursive returns numeric literals unchanged", {
  expect_equal(expect.recursive(5), 5)
  expect_equal(expect.recursive(0), 0)
  expect_equal(expect.recursive(3.14), 3.14)
})

test_that("expect.recursive returns constants unchanged", {
  result <- expect.recursive(quote(a))
  expect_equal(result, quote(a))
  
  result <- expect.recursive(quote(mu))
  expect_equal(result, quote(mu))
})

test_that("expect.recursive wraps random variables in E()", {
  result <- expect.recursive(quote(X))
  expect_equal(result, quote(E(X)))
  
  result <- expect.recursive(quote(Y))
  expect_equal(result, quote(E(Y)))
})

test_that("expect.recursive handles addition with linearity", {
  result <- expect.recursive(quote(X + Y))
  expect_equal(result, quote(E(X) + E(Y)))
})

test_that("expect.recursive handles subtraction with linearity", {
  result <- expect.recursive(quote(X - Y))
  expect_equal(result, quote(E(X) - E(Y)))
})

test_that("expect.recursive handles constant * RV", {
  result <- expect.recursive(quote(a * X))
  expect_equal(result, quote(a * E(X)))
})

test_that("expect.recursive handles RV * constant", {
  result <- expect.recursive(quote(X * a))
  expect_equal(result, quote(E(X) * a))
})

test_that("expect.recursive handles RV * RV (cannot simplify)", {
  result <- expect.recursive(quote(X * Y))
  expect_equal(result, quote(E(X * Y)))
})

test_that("expect.recursive handles constant * constant", {
  result <- expect.recursive(quote(a * b))
  expect_equal(result, quote(a * b))
})

test_that("expect.recursive handles numeric * RV", {
  result <- expect.recursive(quote(2 * X))
  expect_equal(result, quote(2 * E(X)))
})

test_that("expect.recursive handles mixed expression", {
  result <- expect.recursive(quote(a * X + b))
  expected <- quote(a * E(X) + b)
  expect_equal(result, expected)
})

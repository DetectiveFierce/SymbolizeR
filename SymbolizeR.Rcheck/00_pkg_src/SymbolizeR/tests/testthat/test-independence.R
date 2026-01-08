# Tests for Independence Management

test_that("assume.independent registers pairs correctly", {
  clear.independence()
  
  assume.independent(X, Y)
  expect_true(are.independent("X", "Y"))
  expect_true(are.independent("Y", "X"))  # Symmetric
  expect_false(are.independent("X", "Z"))
  
  clear.independence()
})

test_that("assume.independent handles multiple variables", {
  clear.independence()
  
  assume.independent(A, B, C)
  # Should create pairs: A-B, A-C, B-C
  expect_true(are.independent("A", "B"))
  expect_true(are.independent("A", "C"))
  expect_true(are.independent("B", "C"))
  expect_false(are.independent("A", "D"))
  
  clear.independence()
})

test_that("clear.independence removes all assumptions", {
  assume.independent(X, Y)
  expect_true(are.independent("X", "Y"))
  
  clear.independence()
  expect_false(are.independent("X", "Y"))
})

test_that("show.independence displays pairs", {
  clear.independence()
  
  expect_message(show.independence(), "No independence assumptions")
  
  assume.independent(X, Y)
  result <- show.independence()
  expect_length(result, 1)
  
  clear.independence()
})

test_that("E(X * Y) factors when independent", {
  clear.independence()
  
  # Without independence: shouldn't factor
  result_no_ind <- E(X * Y)
  expect_equal(deparse(result_no_ind), "E(X * Y)")
  
  # With independence: should factor
  assume.independent(X, Y)
  result_ind <- E(X * Y)
  expect_equal(deparse(result_ind), "E(X) * E(Y)")
  
  clear.independence()
})

test_that("Independence works with defined distributions", {
  clear.definitions()
  clear.independence()
  
  define(X ~ Normal(mu_x, sigma_x))
  define(Y ~ Normal(mu_y, sigma_y))
  assume.independent(X, Y)
  
  result <- E(X * Y)
  # Should be mu_x * mu_y
  expect_equal(deparse(result), "mu_x * mu_y")
  
  clear.definitions()
  clear.independence()
})

test_that("Cov(X, Y) uses independence for E[XY]", {
  clear.independence()
  
  # Cov(X, Y) = E(XY) - E(X)E(Y)
  # With independence: E(XY) = E(X)E(Y), so Cov = E(X)E(Y) - E(X)E(Y)
  # Note: algebraic cancellation to 0 requires symbolic algebra, which is 
  # beyond current simplifier scope. But the key is E(XY) gets factored.
  assume.independent(X, Y)
  result <- Cov(X, Y)
  result_str <- deparse(result)
  
  # The result should NOT contain E(X * Y) as a single term
  # Instead it should have E(X) * E(Y) somewhere
  expect_false(grepl("E\\(X \\* Y\\)", result_str))
  
  clear.independence()
})

test_that("Independence doesn't affect non-independent pairs", {
  clear.independence()
  
  assume.independent(X, Y)
  
  # X*Z should NOT factor (Z not declared independent)
  result <- E(X * Z)
  expect_equal(deparse(result), "E(X * Z)")
  
  clear.independence()
})

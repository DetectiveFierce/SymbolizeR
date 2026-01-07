# Tests for simplify.expr()

test_that("simplify.expr returns numerics unchanged", {
  expect_equal(simplify.expr(5), 5)
  expect_equal(simplify.expr(0), 0)
})

test_that("simplify.expr returns symbols unchanged", {
  expect_equal(simplify.expr(quote(a)), quote(a))
  expect_equal(simplify.expr(quote(X)), quote(X))
})

test_that("simplify.expr handles x + 0 = x", {
  result <- simplify.expr(quote(a + 0))
  expect_equal(result, quote(a))
  
  result <- simplify.expr(quote(X + 0))
  expect_equal(result, quote(X))
})

test_that("simplify.expr handles 0 + x = x", {
  result <- simplify.expr(quote(0 + a))
  expect_equal(result, quote(a))
})

test_that("simplify.expr handles x - 0 = x", {
  result <- simplify.expr(quote(a - 0))
  expect_equal(result, quote(a))
})

test_that("simplify.expr handles x * 1 = x", {
  result <- simplify.expr(quote(a * 1))
  expect_equal(result, quote(a))
  
  result <- simplify.expr(quote(X * 1))
  expect_equal(result, quote(X))
})

test_that("simplify.expr handles 1 * x = x", {
  result <- simplify.expr(quote(1 * a))
  expect_equal(result, quote(a))
})

test_that("simplify.expr handles x * 0 = 0", {
  result <- simplify.expr(quote(a * 0))
  expect_equal(result, 0)
  
  result <- simplify.expr(quote(X * 0))
  expect_equal(result, 0)
})

test_that("simplify.expr handles 0 * x = 0", {
  result <- simplify.expr(quote(0 * a))
  expect_equal(result, 0)
})

test_that("simplify.expr handles 0 / x = 0", {
  result <- simplify.expr(quote(0 / a))
  expect_equal(result, 0)
})

test_that("simplify.expr handles x / 1 = x", {
  result <- simplify.expr(quote(a / 1))
  expect_equal(result, quote(a))
})

test_that("simplify.expr handles x^0 = 1", {
  result <- simplify.expr(quote(a^0))
  expect_equal(result, 1)
})

test_that("simplify.expr handles x^1 = x", {
  result <- simplify.expr(quote(a^1))
  expect_equal(result, quote(a))
})

test_that("simplify.expr handles E(constant) = constant", {
  result <- simplify.expr(quote(E(a)))
  expect_equal(result, quote(a))
  
  result <- simplify.expr(quote(E(5)))
  expect_equal(result, 5)
})

test_that("simplify.expr preserves E(RV)", {
  result <- simplify.expr(quote(E(X)))
  expect_equal(result, quote(E(X)))
})

# --- Power Combination Tests ---

test_that("x * x^n = x^(n+1)", {
  result <- simplify.expr(quote(x * x^3))
  expect_equal(deparse(result), "x^4")
})

test_that("(x^m)^n = x^(m*n)", {
  result <- simplify.expr(quote((sigma^2)^2))
  expect_equal(deparse(result), "sigma^4")
})

test_that("x / x = 1", {
  result <- simplify.expr(quote(sigma^4 / sigma^4))
  expect_equal(result, 1)
})

test_that("(c * x) / x = c", {
  # Build expression without parentheses (like the real Kurtosis code does)
  num <- call("*", 3, quote(sigma^4))
  denom <- quote(sigma^4)
  expr <- call("/", num, denom)
  result <- simplify.expr(expr)
  expect_equal(result, 3)
})

# --- Higher-Order Statistics Simplification ---

test_that("Skewness(X) for Normal = 0", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  result <- Skewness(X)
  expect_equal(result, 0)
  clear.definitions()
})

test_that("Kurtosis(X) for Normal = 0", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  result <- Kurtosis(X)
  expect_equal(result, 0)
  clear.definitions()
})

test_that("Var(X) for Normal = sigma^2", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  result <- Var(X)
  expect_equal(deparse(result), "sigma^2")
  clear.definitions()
})

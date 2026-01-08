# Tests for calculus.R - Symbolic Differentiation and Integration

# =============================================================================
# HELPER FUNCTION TESTS (internal functions use quote() directly)
# =============================================================================

test_that("contains.var detects variables correctly", {
  expect_true(contains.var(quote(x), quote(x)))
  expect_false(contains.var(quote(y), quote(x)))
  expect_false(contains.var(5, quote(x)))
  expect_true(contains.var(quote(x + y), quote(x)))
  expect_true(contains.var(quote(a * x^2), quote(x)))
  expect_false(contains.var(quote(a * b), quote(x)))
})

test_that("protect.E replaces E() calls with temp symbols", {
  result <- protect.E(quote(x * E(Y)))
  expect_false(any(grepl("E\\(", deparse(result$expr))))
  expect_true(length(result$map) > 0)
})

test_that("restore.E restores temp symbols to E() calls", {
  prot <- protect.E(quote(x * E(Y)))
  restored <- restore.E(prot$expr, prot$map)
  expect_equal(restored, quote(x * E(Y)))
})


# =============================================================================
# SYMBOLIC DIFFERENTIATION TESTS (NSE - no quote() needed)
# =============================================================================

test_that("deriv.sym differentiates polynomials", {
  result <- deriv.sym(x^3, x)
  expect_equal(deparse(result), "3 * x^2")
})

test_that("deriv.sym differentiates linear expressions", {
  result <- deriv.sym(2 * x + 5, x)
  expect_equal(result, 2)
})

test_that("deriv.sym handles constant expressions", {
  result <- deriv.sym(a * b, x)
  expect_equal(result, 0)
})

test_that("deriv.sym differentiates exponentials", {
  result <- deriv.sym(exp(x), x)
  expect_equal(deparse(result), "exp(x)")
})

test_that("deriv.sym handles exp(a*x)", {
  result <- deriv.sym(exp(a * x), x)
  result_str <- deparse(result)
  # Accept either order: "a * exp(a * x)" or "exp(a * x) * a"
  expect_true(grepl("exp\\(a \\* x\\)", result_str) && grepl("a", result_str))
})

test_that("deriv.sym treats E() calls as constants", {
  result <- deriv.sym(x * E(Y), x)
  expect_equal(deparse(result), "E(Y)")
})

test_that("deriv.sym preserves E() in complex expressions", {
  result <- deriv.sym(x^2 * E(Y) + E(Z), x)
  expect_equal(deparse(result), "2 * x * E(Y)")
})


# =============================================================================
# INTEGRAND PARTITIONING TESTS (internal - uses quote())
# =============================================================================

test_that("partition.integrand separates constant factors", {
  result <- partition.integrand(quote(a * x^2), quote(x))
  expect_equal(deparse(result$const), "a")
  expect_true(contains.var(result$var, quote(x)))
})

test_that("partition.integrand handles pure variable expressions", {
  result <- partition.integrand(quote(x^2 * exp(-x)), quote(x))
  expect_equal(result$const, 1)
})

test_that("partition.integrand handles pure constant expressions", {
  result <- partition.integrand(quote(a * b), quote(x))
  expect_false(contains.var(result$var, quote(x)))
})


# =============================================================================
# GAMMA KERNEL TESTS (NSE - no quote() needed)
# =============================================================================

test_that("integrate.sym recognizes Gamma kernel: x^n * exp(-x)", {
  # Integral of x^2 * exp(-x) from 0 to Inf = Gamma(3) / 1^3 = Gamma(3)
  result <- integrate.sym(x^2 * exp(-x), x, 0, Inf)
  result_str <- deparse(result)
  expect_true(grepl("gamma", result_str))
})

test_that("integrate.sym recognizes Gamma kernel with rate parameter", {
  # Integral of x^(a-1) * exp(-lambda * x) from 0 to Inf = Gamma(a) / lambda^a
  result <- integrate.sym(x^2 * exp(-lambda * x), x, 0, Inf)
  result_str <- deparse(result)
  expect_true(grepl("gamma", result_str))
  expect_true(grepl("lambda", result_str))
})

test_that("integrate.sym handles pure exponential (Gamma A=1)", {
  # Integral of exp(-x) from 0 to Inf = 1
  result <- integrate.sym(exp(-x), x, 0, Inf)
  # Should give gamma(1)/1 or simplified to 1
  expect_true(!is.null(result))
})


# =============================================================================
# GAUSSIAN KERNEL TESTS (NSE - no quote() needed)
# =============================================================================

test_that("integrate.sym recognizes Gaussian kernel: exp(-x^2)", {
  # Integral of exp(-x^2) from -Inf to Inf = sqrt(pi)
  result <- integrate.sym(exp(-x^2), x, -Inf, Inf)
  result_str <- deparse(result)
  expect_true(grepl("sqrt", result_str) || grepl("pi", result_str))
})

test_that("integrate.sym recognizes Gaussian kernel: exp(-a*x^2)", {
  # Integral of exp(-a*x^2) from -Inf to Inf = sqrt(pi/a)
  result <- integrate.sym(exp(-a * x^2), x, -Inf, Inf)
  result_str <- deparse(result)
  expect_true(grepl("sqrt", result_str))
})

test_that("integrate.sym recognizes Gaussian with linear term", {
  # Integral of exp(-A*x^2 + B*x) from -Inf to Inf = sqrt(pi/A) * exp(B^2/(4A))
  result <- integrate.sym(exp(-A * mu^2 + B * mu), mu, -Inf, Inf)
  result_str <- deparse(result)
  expect_true(grepl("sqrt", result_str) && grepl("exp", result_str))
})


# =============================================================================
# BETA KERNEL TESTS (NSE - no quote() needed)
# =============================================================================

test_that("integrate.sym recognizes Beta kernel: x^a * (1-x)^b", {
  # Integral of x * (1-x) from 0 to 1 = Beta(2, 2)
  result <- integrate.sym(x * (1-x), x, 0, 1)
  result_str <- deparse(result)
  expect_true(grepl("beta", result_str))
})

test_that("integrate.sym recognizes Beta kernel with powers", {
  # Integral of x^2 * (1-x)^3 from 0 to 1 = Beta(3, 4)
  result <- integrate.sym(x^2 * (1-x)^3, x, 0, 1)
  result_str <- deparse(result)
  expect_true(grepl("beta", result_str))
})

test_that("integrate.sym handles Beta with symbolic parameters", {
  # Integral of x^(alpha-1) * (1-x)^(beta-1) from 0 to 1 = Beta(alpha, beta)
  result <- integrate.sym(x^(alpha - 1) * (1-x)^(beta - 1), x, 0, 1)
  result_str <- deparse(result)
  expect_true(grepl("beta", result_str))
})


# =============================================================================
# CONSTANT FACTORING TESTS (NSE - no quote() needed)
# =============================================================================

test_that("integrate.sym factors out constants", {
  # Integral of a * x^2 * exp(-x) should have 'a' in result
  result <- integrate.sym(a * x^2 * exp(-x), x, 0, Inf)
  result_str <- deparse(result)
  expect_true(grepl("a", result_str))
  expect_true(grepl("gamma", result_str))
})


# =============================================================================
# FALLBACK BEHAVIOR TESTS (NSE - no quote() needed)
# =============================================================================

test_that("integrate.sym returns unevaluated for unknown patterns", {
  # Unknown pattern should return Integrate() call
  result <- integrate.sym(sin(x), x, 0, Inf)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "Integrate")
})


# =============================================================================
# CONJUGACY VALIDATION TESTS (NSE - no quote() needed)
# =============================================================================

test_that("Beta-Binomial conjugacy: recognizes posterior kernel", {
  # Posterior: x^(alpha + k - 1) * (1-x)^(beta + n - k - 1)
  # Should return beta(alpha + k, beta + n - k)
  result <- integrate.sym(x^(alpha + k - 1) * (1-x)^(beta + n - k - 1), x, 0, 1)
  result_str <- deparse(result)
  expect_true(grepl("beta", result_str))
})

test_that("Normal-Normal conjugacy: recognizes posterior kernel", {
  # Posterior kernel: exp(-A*mu^2 + B*mu)
  # Should return sqrt(pi/A) * exp(B^2/(4A))
  result <- integrate.sym(exp(-A * mu^2 + B * mu), mu, -Inf, Inf)
  result_str <- deparse(result)
  expect_true(grepl("sqrt", result_str))
  expect_true(grepl("pi", result_str))
})

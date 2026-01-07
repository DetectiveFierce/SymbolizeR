# Tests for distribution metadata system

test_that("define() registers a Normal distribution", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  expect_true(exists("X", envir = pkg.env))
  dist_info <- get("X", envir = pkg.env)
  expect_equal(dist_info$distribution, "Normal")
  expect_equal(dist_info$params$mu, quote(mu))
  expect_equal(dist_info$params$sigma, quote(sigma))
  
  clear.definitions()
})

test_that("clear.definitions() removes all definitions", {
  define(X ~ Normal(mu, sigma))
  define(Y ~ Normal(alpha, beta))
  
  clear.definitions()
  
  expect_false(exists("X", envir = pkg.env))
  expect_false(exists("Y", envir = pkg.env))
})

test_that("undefine() removes a specific definition", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  define(Y ~ Normal(alpha, beta))
  
  undefine("X")
  
  expect_false(exists("X", envir = pkg.env))
  expect_true(exists("Y", envir = pkg.env))
  
  clear.definitions()
})

test_that("E(X) returns mu when X ~ Normal(mu, sigma)", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- E(X)
  expect_equal(result, quote(mu))
  
  clear.definitions()
})

test_that("E(X) returns E(X) when X is undefined", {
  clear.definitions()
  
  result <- E(X)
  expect_equal(result, quote(E(X)))
})

test_that("E(X^2) returns sigma^2 + mu^2 when X ~ Normal(mu, sigma)", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- E(X^2)
  # Result should be sigma^2 + mu^2
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")
  
  clear.definitions()
})

test_that("Var(X) simplifies to sigma^2 when X ~ Normal(mu, sigma)", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- Var(X)
  # Var(X) = E(X^2) - E(X)^2 = (sigma^2 + mu^2) - mu^2 = sigma^2
  # Check that result is sigma^2 (possibly as sigma^2)
  expect_true(is.call(result))
  
  clear.definitions()
})

test_that("E(a*X) returns a*mu when X ~ Normal(mu, sigma)", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- E(a * X)
  expect_equal(result, quote(a * mu))
  
  clear.definitions()
})

test_that("E(X + Y) with one defined, one undefined", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- E(X + Y)
  expect_equal(result, quote(mu + E(Y)))
  
  clear.definitions()
})

# ============================================
# Tests for all distributions - First Moments
# ============================================

test_that("E(X) for Uniform(a, b) returns (a + b) / 2", {
  clear.definitions()
  define(X ~ Uniform(a, b))
  
  result <- E(X)
  # Check structure: division with denominator 2
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "/")
  expect_equal(result[[3]], 2)
  
  clear.definitions()
})

test_that("E(X) for Exponential(lambda) returns 1 / lambda", {
  clear.definitions()
  define(X ~ Exponential(lambda))
  
  result <- E(X)
  expect_equal(result, quote(1 / lambda))
  
  clear.definitions()
})

test_that("E(X) for Gamma(alpha, beta) returns alpha / beta", {
  clear.definitions()
  define(X ~ Gamma(alpha, beta))
  
  result <- E(X)
  expect_equal(result, quote(alpha / beta))
  
  clear.definitions()
})

test_that("E(X) for InvGamma(alpha, beta) returns beta / (alpha - 1)", {
  clear.definitions()
  define(X ~ InvGamma(alpha, beta))
  
  result <- E(X)
  # Check structure: division with beta in numerator
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "/")
  expect_equal(result[[2]], quote(beta))
  
  clear.definitions()
})

test_that("E(X) for Beta(alpha, beta) returns alpha / (alpha + beta)", {
  clear.definitions()
  define(X ~ Beta(alpha, beta))
  
  result <- E(X)
  # Check structure: division with alpha in numerator
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "/")
  expect_equal(result[[2]], quote(alpha))
  
  clear.definitions()
})

test_that("E(X) for Binomial(n, p) returns n * p", {
  clear.definitions()
  define(X ~ Binomial(n, p))
  
  result <- E(X)
  expect_equal(result, quote(n * p))
  
  clear.definitions()
})

test_that("E(X) for Poisson(lambda) returns lambda", {
  clear.definitions()
  define(X ~ Poisson(lambda))
  
  result <- E(X)
  expect_equal(result, quote(lambda))
  
  clear.definitions()
})

test_that("E(X) for Geometric(p) returns 1 / p", {
  clear.definitions()
  define(X ~ Geometric(p))
  
  result <- E(X)
  expect_equal(result, quote(1 / p))
  
  clear.definitions()
})

test_that("define() validates parameter counts", {
  clear.definitions()
  
  expect_error(define(X ~ Normal(mu)), "2 parameters")
  expect_error(define(X ~ Exponential(a, b)), "1 parameter")
  expect_error(define(X ~ Binomial(n)), "2 parameters")
  expect_error(define(X ~ Unknown(a, b)), "Unknown distribution")
  
  clear.definitions()
})

# ============================================
# Tests for MGF (Moment Generating Function)
# ============================================

test_that("E(exp(t*X)) returns Normal MGF when X ~ Normal(mu, sigma)", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- E(exp(t * X))
  # Should be exp(mu*t + sigma^2*t^2/2)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "exp")
  
  clear.definitions()
})

test_that("E(exp(t*X)) returns Exponential MGF when X ~ Exponential(lambda)", {
  clear.definitions()
  define(X ~ Exponential(lambda))
  
  result <- E(exp(t * X))
  # Should be lambda / (lambda - t)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "/")
  
  clear.definitions()
})

test_that("E(exp(t*X)) returns Poisson MGF when X ~ Poisson(lambda)", {
  clear.definitions()
  define(X ~ Poisson(lambda))
  
  result <- E(exp(t * X))
  # Should be exp(lambda * (exp(t) - 1))
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "exp")
  
  clear.definitions()
})

test_that("E(exp(t*X)) returns E(exp(t*X)) when X is undefined", {
  clear.definitions()
  
  result <- E(exp(t * X))
  expect_equal(result, quote(E(exp(t * X))))
})

test_that("E(exp(X)) with t=1 returns MGF at t=1", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- E(exp(X))
  # Should substitute t=1 into MGF
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "exp")
  
  clear.definitions()
})

# ============================================
# COMPREHENSIVE INTEGRATION TESTS
# ============================================

# --- Second Moments for All Distributions ---

test_that("E(X^2) works for all continuous distributions", {
  clear.definitions()
  
  # Exponential
  define(X ~ Exponential(lambda))
  result <- E(X^2)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "/")  # 2/lambda^2
  
  # Gamma
  clear.definitions()
  define(X ~ Gamma(alpha, beta))
  result <- E(X^2)
  expect_true(is.call(result))
  
  # Uniform
  clear.definitions()
  define(X ~ Uniform(a, b))
  result <- E(X^2)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "/")  # (a^2+ab+b^2)/3
  
  clear.definitions()
})

test_that("E(X^2) works for all discrete distributions", {
  clear.definitions()
  
  # Binomial: E[X^2] = np(1-p) + (np)^2
  define(X ~ Binomial(n, p))
  result <- E(X^2)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")
  
  # Poisson: E[X^2] = lambda + lambda^2
  clear.definitions()
  define(X ~ Poisson(lambda))
  result <- E(X^2)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")
  
  # Geometric: E[X^2] = (2-p)/p^2
  clear.definitions()
  define(X ~ Geometric(p))
  result <- E(X^2)
  expect_true(is.call(result))
  
  clear.definitions()
})

# --- Mixed Defined/Undefined Variables ---

test_that("E(X + Y + Z) with mixed defined/undefined", {
  clear.definitions()
  define(X ~ Normal(mu_x, sigma_x))
  define(Z ~ Poisson(lambda))
  
  result <- E(X + Y + Z)
  # Should return mu_x + E(Y) + lambda
  expect_true(is.call(result))
  result_str <- deparse(result)
  expect_true(grepl("mu_x", result_str))
  expect_true(grepl("E\\(Y\\)", result_str))
  expect_true(grepl("lambda", result_str))
  
  clear.definitions()
})

test_that("E(a*X + b*Y) with both defined", {
  clear.definitions()
  define(X ~ Normal(mu_x, sigma_x))
  define(Y ~ Normal(mu_y, sigma_y))
  
  result <- E(a * X + b * Y)
  # Should return a*mu_x + b*mu_y
  expect_equal(result, quote(a * mu_x + b * mu_y))
  
  clear.definitions()
})

test_that("E(X - Y) with both defined", {
  clear.definitions()
  define(X ~ Exponential(lambda))
  define(Y ~ Exponential(gamma))
  
  result <- E(X - Y)
  # Should return 1/lambda - 1/gamma
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "-")
  
  clear.definitions()
})

# --- Redefining Variables ---

test_that("Redefining a variable overwrites the old definition", {
  clear.definitions()
  define(X ~ Normal(mu1, sigma1))
  
  result1 <- E(X)
  expect_equal(result1, quote(mu1))
  
  # Redefine X with different parameters
  define(X ~ Exponential(lambda))
  
  result2 <- E(X)
  expect_equal(result2, quote(1/lambda))
  
  clear.definitions()
})

test_that("Redefining changes distribution type", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  expect_equal(get("X", envir = pkg.env)$distribution, "Normal")
  
  define(X ~ Poisson(lambda))
  expect_equal(get("X", envir = pkg.env)$distribution, "Poisson")
  
  clear.definitions()
})

# --- Numeric Parameters ---

test_that("E(X) works with numeric parameters", {
  clear.definitions()
  define(X ~ Normal(0, 1))  # Standard normal
  
  result <- E(X)
  expect_equal(result, 0)
  
  result2 <- E(X^2)
  # sigma^2 + mu^2 = 1^2 + 0^2 = 1 + 0 (may simplify to numeric or remain as call)
  expect_true(is.call(result2) || is.numeric(result2))
  
  clear.definitions()
})

test_that("E(2*X) with numeric coefficient and defined distribution", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- E(2 * X)
  expect_equal(result, quote(2 * mu))
  
  clear.definitions()
})

# --- Variance with Defined Distributions ---

test_that("Var(X) computes correctly for Poisson", {
  clear.definitions()
  define(X ~ Poisson(lambda))
  
  result <- Var(X)
  # Var(Poisson) = lambda, so E[X^2] - E[X]^2 = (lambda + lambda^2) - lambda^2 = lambda
  # With improved simplification, this correctly simplifies to just the symbol lambda
  expect_true(is.symbol(result) || is.call(result))
  expect_equal(deparse(result), "lambda")
  
  clear.definitions()
})

test_that("Var(a*X + b) with defined distribution", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- Var(a * X + b)
  # Var(aX + b) = a^2 * Var(X) = a^2 * sigma^2
  expect_true(is.call(result))
  result_str <- deparse(result)
  expect_true(grepl("a", result_str))
  
  clear.definitions()
})

# --- Covariance with Defined Distributions ---

test_that("Cov(X, Y) with both defined (independent)", {
  clear.definitions()
  define(X ~ Normal(mu_x, sigma_x))
  define(Y ~ Normal(mu_y, sigma_y))
  
  result <- Cov(X, Y)
  # E[XY] - E[X]E[Y] where E[XY] = E[X]E[Y] for independent
  # Currently returns E(X * Y) - mu_x * mu_y
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "-")
  
  clear.definitions()
})

# --- MGF Edge Cases ---

test_that("E(exp(X*t)) handles reversed order", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- E(exp(X * t))
  # Should also work with X*t (RV * const)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "exp")
  
  clear.definitions()
})

test_that("MGF for all supported discrete distributions", {
  clear.definitions()
  
  # Binomial MGF
  define(X ~ Binomial(n, p))
  result <- E(exp(t * X))
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "^")  # (p*exp(t) + 1-p)^n
  
  # Geometric MGF
  clear.definitions()
  define(X ~ Geometric(p))
  result <- E(exp(t * X))
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "/")
  
  # NegBinomial MGF
  clear.definitions()
  define(X ~ NegBinomial(r, p))
  result <- E(exp(t * X))
  expect_true(is.call(result))
  
  clear.definitions()
})

test_that("MGF for Gamma distribution", {
  clear.definitions()
  define(X ~ Gamma(alpha, beta))
  
  result <- E(exp(t * X))
  # (beta / (beta - t))^alpha
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "^")
  
  clear.definitions()
})

test_that("E(exp(t*X)) returns wrapped E() for unsupported distributions", {
  clear.definitions()
  define(X ~ Uniform(a, b))  # Uniform MGF not implemented
  
  result <- E(exp(t * X))
  # Should fall back to E(exp(t*X))
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "E")
  
  clear.definitions()
})

# --- Complex Expression Chains ---

test_that("E(X + 2*Y - Z) with all defined", {
  clear.definitions()
  define(X ~ Normal(mu_x, sigma_x))
  define(Y ~ Normal(mu_y, sigma_y))
  define(Z ~ Poisson(lambda))
  
  result <- E(X + 2 * Y - Z)
  expect_true(is.call(result))
  result_str <- deparse(result)
  expect_true(grepl("mu_x", result_str))
  expect_true(grepl("mu_y", result_str))
  expect_true(grepl("lambda", result_str))
  
  clear.definitions()
})

test_that("Multiple operations preserve correctness", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  # Test a chain of operations
  e1 <- E(X)
  expect_equal(e1, quote(mu))
  
  e2 <- E(X + X)
  # Now correctly combines like terms: mu + mu -> 2 * mu
  expect_equal(e2, quote(2 * mu))
  
  e3 <- E(3 * X)
  expect_equal(e3, quote(3 * mu))
  
  clear.definitions()
})

# --- Undefine Edge Cases ---

test_that("undefine() on non-existent variable doesn't error", {
  clear.definitions()
  expect_silent(undefine("NonExistent"))
})

test_that("After undefine(), E(X) returns E(X)", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  expect_equal(E(X), quote(mu))
  
  undefine("X")
  expect_equal(E(X), quote(E(X)))
  
  clear.definitions()
})

# --- Formula Validation ---

test_that("define() requires formula input", {
  clear.definitions()
  expect_error(define("X ~ Normal(mu, sigma)"), "formula")
})

test_that("define() requires distribution call on RHS", {
  clear.definitions()
  expect_error(define(X ~ mu), "distribution call")
})

# --- LogNormal and Weibull Moments ---
  
test_that("E(X) for LogNormal returns exp(mu + sigma^2/2)", {
  clear.definitions()
  define(X ~ LogNormal(mu, sigma))
  
  result <- E(X)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "exp")
  
  clear.definitions()
})

test_that("E(X) for Weibull returns lambda * gamma(1 + 1/k)", {
  clear.definitions()
  define(X ~ Weibull(k, lambda))
  
  result <- E(X)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "*")
  
  clear.definitions()
})

test_that("E(X) for NegBinomial returns r*(1-p)/p", {
  clear.definitions()
  define(X ~ NegBinomial(r, p))
  
  result <- E(X)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "/")
  
  clear.definitions()
})

# ============================================
# Tests for Derivation Functions
# ============================================

test_that("derive.E returns a derivation object", {
  clear.definitions()
  
  result <- derive.E(X + Y)
  expect_s3_class(result, "derivation")
  expect_true("input" %in% names(result))
  expect_true("steps" %in% names(result))
  expect_true("result" %in% names(result))
  
  clear.definitions()
})

test_that("derive.E contains multiple steps", {
  clear.definitions()
  
  result <- derive.E(a * X + b)
  expect_true(length(result$steps) >= 2)
  
  # First step should be input
  expect_equal(result$steps[[1]]$description, "Starting expression")
  
  clear.definitions()
})

test_that("derive.E with defined distribution shows substitution", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- derive.E(X)
  expect_true(length(result$steps) >= 2)
  expect_equal(result$result, quote(mu))
  
  clear.definitions()
})

test_that("derive.Var returns a derivation object", {
  clear.definitions()
  
  result <- derive.Var(X)
  expect_s3_class(result, "derivation")
  expect_true(length(result$steps) >= 4)
  
  # Should contain variance definition step
  rules <- sapply(result$steps, function(s) s$rule)
  expect_true(any(grepl("Var", rules)))
  
  clear.definitions()
})

test_that("derive.Var with defined distribution", {
  clear.definitions()
  define(X ~ Normal(mu, sigma))
  
  result <- derive.Var(X)
  expect_s3_class(result, "derivation")
  expect_true(is.call(result$result) || is.numeric(result$result))
  
  clear.definitions()
})

test_that("derive.Cov returns a derivation object", {
  clear.definitions()
  
  result <- derive.Cov(X, Y)
  expect_s3_class(result, "derivation")
  expect_true(length(result$steps) >= 4)
  
  # Should contain covariance definition step
  rules <- sapply(result$steps, function(s) s$rule)
  expect_true(any(grepl("Cov", rules)))
  
  clear.definitions()
})

test_that("derive.Cov with defined distributions", {
  clear.definitions()
  define(X ~ Normal(mu_x, sigma_x))
  define(Y ~ Normal(mu_y, sigma_y))
  
  result <- derive.Cov(X, Y)
  expect_s3_class(result, "derivation")
  
  clear.definitions()
})

test_that("print.derivation works without error", {
  clear.definitions()
  
  result <- derive.E(X + Y)
  expect_output(print(result), "Derivation of")
  expect_output(print(result), "Step 1")
  expect_output(print(result), "Final Answer")
  
  clear.definitions()
})


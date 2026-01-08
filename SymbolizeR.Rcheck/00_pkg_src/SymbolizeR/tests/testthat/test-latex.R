# Tests for LaTeX Conversion

# Helper to compare latex output as character
latex_eq <- function(expr, expected) {
  as.character(expr) == expected
}

test_that("to.latex handles basic symbols", {
  expect_equal(as.character(to.latex(quote(x))), "x")
  expect_equal(as.character(to.latex(quote(X))), "X")
  expect_equal(as.character(to.latex(quote(5))), "5")
})

test_that("to.latex converts Greek letters", {
  expect_equal(as.character(to.latex(quote(mu))), "\\mu")
  expect_equal(as.character(to.latex(quote(sigma))), "\\sigma")
  expect_equal(as.character(to.latex(quote(alpha))), "\\alpha")
  expect_equal(as.character(to.latex(quote(beta))), "\\beta")
  expect_equal(as.character(to.latex(quote(lambda))), "\\lambda")
})

test_that("to.latex handles subscripts", {
  expect_equal(as.character(to.latex(quote(X_1))), "X_{1}")
  expect_equal(as.character(to.latex(quote(mu_i))), "\\mu_{i}")
})

test_that("to.latex handles addition", {
  expect_equal(as.character(to.latex(quote(a + b))), "a + b")
  expect_equal(as.character(to.latex(quote(X + Y + Z))), "X + Y + Z")
})

test_that("to.latex handles subtraction", {
  expect_equal(as.character(to.latex(quote(a - b))), "a - b")
  # Unary minus
  expect_equal(as.character(to.latex(quote(-x))), "-x")
})

test_that("to.latex handles multiplication with implicit style", {
  result <- as.character(to.latex(quote(a * b)))
  expect_true(grepl("a", result) && grepl("b", result))
  
  # Number * symbol
  result2 <- as.character(to.latex(quote(2 * x)))
  expect_true(grepl("2", result2) && grepl("x", result2))
})

test_that("to.latex handles division as fraction", {
  expect_equal(as.character(to.latex(quote(a / b))), "\\frac{a}{b}")
  expect_equal(as.character(to.latex(quote(1 / n))), "\\frac{1}{n}")
})

test_that("to.latex handles exponentiation", {
  expect_equal(as.character(to.latex(quote(x^2))), "x^{2}")
  expect_equal(as.character(to.latex(quote(sigma^2))), "\\sigma^{2}")
})

test_that("to.latex handles E() function", {
  expect_equal(as.character(to.latex(quote(E(X)))), "\\mathbb{E}[X]")
  expect_equal(as.character(to.latex(quote(E(X + Y)))), "\\mathbb{E}[X + Y]")
})

test_that("to.latex handles Var() function", {
  expect_equal(as.character(to.latex(quote(Var(X)))), "\\text{Var}(X)")
})

test_that("to.latex handles Cov() function", {
  expect_equal(as.character(to.latex(quote(Cov(X, Y)))), "\\text{Cov}(X, Y)")
})

test_that("to.latex handles exp() function", {
  expect_equal(as.character(to.latex(quote(exp(x)))), "e^{x}")
})

test_that("to.latex handles sqrt() function", {
  expect_equal(as.character(to.latex(quote(sqrt(x)))), "\\sqrt{x}")
})

test_that("to.latex handles log() function", {
  expect_equal(as.character(to.latex(quote(log(x)))), "\\ln(x)")
})

test_that("to.latex handles complex nested expressions", {
  # E[X^2] - E[X]^2
  result <- as.character(to.latex(quote(E(X^2) - E(X)^2)))
  expect_true(grepl("\\\\mathbb\\{E\\}", result))
  expect_true(grepl("-", result))
})

test_that("to.latex handles character input", {
  expect_equal(as.character(to.latex("x + y")), "x + y")
  expect_equal(as.character(to.latex("E(X)")), "\\mathbb{E}[X]")
})

test_that("to.latex.derivation works", {
  deriv <- derive.E(a * X + b)
  result <- as.character(to.latex(deriv))
  
  # Should contain align environment
  expect_true(grepl("\\\\begin\\{align\\*\\}", result))
  expect_true(grepl("\\\\end\\{align\\*\\}", result))
})

test_that("to.latex handles gamma function", {
  expect_equal(as.character(to.latex(quote(gamma(x)))), "\\Gamma\\left(x\\right)")
})

test_that("to.latex parenthesizes correctly", {
  # (a + b)^2 should have parentheses around a + b
  result <- as.character(to.latex(quote((a + b)^2)))
  expect_true(grepl("\\\\left\\(", result))
})

test_that("to.latex delimiters parameter works", {
  # Inline delimiters
  result <- as.character(to.latex(quote(x), delimiters = "inline"))
  expect_equal(result, "$x$")
  
  # Display delimiters
  result2 <- as.character(to.latex(quote(x), delimiters = "display"))
  expect_true(grepl("\\\\\\[", result2))
})


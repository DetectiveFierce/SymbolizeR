## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SymbolizeR)

## ----deriv-basic--------------------------------------------------------------
# Polynomial differentiation
deriv.sym(x^3, x)

# Exponential functions
deriv.sym(exp(x), x)

# Product rule applied automatically
deriv.sym(x^2 * exp(x), x)

## ----deriv-const--------------------------------------------------------------
# d/dx of ax + b
deriv.sym(a * x + b, x)

# d/dx of exp(a*x)
deriv.sym(exp(a * x), x)

## ----deriv-E------------------------------------------------------------------
# d/dx of x * E(Y) — treats E(Y) as constant
deriv.sym(x * E(Y), x)

# More complex example
deriv.sym(x^2 * E(Y) + E(Z), x)

## ----gamma-kernel-------------------------------------------------------------
# Basic Gamma integral
integrate.sym(x^2 * exp(-x), x, 0, Inf)

# With rate parameter
integrate.sym(x^2 * exp(-lambda * x), x, 0, Inf)

# Simple exponential (A = 1)
integrate.sym(exp(-x), x, 0, Inf)

## ----gaussian-kernel----------------------------------------------------------
# Standard Gaussian integral
integrate.sym(exp(-x^2), x, -Inf, Inf)

# With coefficient
integrate.sym(exp(-a * x^2), x, -Inf, Inf)

# Full quadratic with linear term
integrate.sym(exp(-A * mu^2 + B * mu), mu, -Inf, Inf)

## ----beta-kernel--------------------------------------------------------------
# Simple Beta
integrate.sym(x * (1-x), x, 0, 1)

# With explicit powers
integrate.sym(x^2 * (1-x)^3, x, 0, 1)

# Symbolic parameters
integrate.sym(x^(alpha - 1) * (1-x)^(beta - 1), x, 0, 1)

## ----const-factor-------------------------------------------------------------
# The 'a' is extracted from the integrand
integrate.sym(a * x^2 * exp(-x), x, 0, Inf)

## ----bayes-beta---------------------------------------------------------------
# Integrate to find normalizing constant
integrate.sym(x^(alpha + k - 1) * (1-x)^(beta + n - k - 1), x, 0, 1)

## ----bayes-normal-------------------------------------------------------------
# Integrate to find normalizing constant
integrate.sym(exp(-A * mu^2 + B * mu), mu, -Inf, Inf)

## ----fallback-----------------------------------------------------------------
# No kernel for trigonometric functions
integrate.sym(sin(x), x, 0, Inf)


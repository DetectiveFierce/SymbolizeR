## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SymbolizeR)

## ----convention---------------------------------------------------------------
E(X)      # X is a random variable
E(mu)     # mu is a constant  
E(5)      # 5 is a number

## ----expectation--------------------------------------------------------------
# Constants factor out
E(a * X)

# Distributes over sums
E(X + Y)

# Linear combinations
E(a * X + b)

# Numeric coefficients
E(2 * X + 3)

## ----product------------------------------------------------------------------
E(X * Y)

## ----variance-----------------------------------------------------------------
Var(X)

Var(a * X)

## ----covariance---------------------------------------------------------------
Cov(X, Y)

## ----limitations--------------------------------------------------------------
E(1/X)    # Cannot simplify

E(X^Y)    # Non-linear in RV


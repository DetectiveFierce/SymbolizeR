## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SymbolizeR)

## ----define-basic-------------------------------------------------------------
define(X ~ Normal(mu, sigma))

E(X)      # Returns the mean
E(X^2)    # Returns the second moment

## ----dist-examples------------------------------------------------------------
clear.definitions()

# Exponential
define(T ~ Exponential(lambda))
E(T)
E(T^2)

# Poisson
define(N ~ Poisson(lambda))
E(N)

# Binomial
define(X ~ Binomial(n, p))
E(X)

## ----variance-----------------------------------------------------------------
clear.definitions()
define(X ~ Normal(mu, sigma))

Var(X)

## ----mgf----------------------------------------------------------------------
clear.definitions()

# Normal MGF
define(X ~ Normal(mu, sigma))
E(exp(t * X))

# Exponential MGF
define(Y ~ Exponential(lambda))
E(exp(t * Y))

# Poisson MGF
define(N ~ Poisson(lambda))
E(exp(t * N))

## ----multiple-----------------------------------------------------------------
clear.definitions()
define(X ~ Normal(mu_x, sigma_x))
define(Y ~ Normal(mu_y, sigma_y))

E(X + Y)
E(a * X + b * Y)

# Mixed: X known, Z unknown
E(X + Z)

## ----clear--------------------------------------------------------------------
clear.definitions()
E(X)    # Back to generic

## ----undefine-----------------------------------------------------------------
define(X ~ Normal(mu, sigma))
define(Y ~ Poisson(lambda))

undefine("X")
E(X)    # Generic
E(Y)    # Still lambda

clear.definitions()

## ----redefine-----------------------------------------------------------------
define(X ~ Normal(mu, sigma))
E(X)

define(X ~ Exponential(lambda))
E(X)

clear.definitions()

## ----sample-mean--------------------------------------------------------------
clear.definitions()
define(X1 ~ Normal(mu, sigma))
define(X2 ~ Normal(mu, sigma))
define(X3 ~ Normal(mu, sigma))

# E[X₁ + X₂ + X₃] = 3μ
E(X1 + X2 + X3)

# Therefore E[X̄] = (3μ)/3 = μ ✓
clear.definitions()


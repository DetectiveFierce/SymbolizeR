## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SymbolizeR)

## ----derive-e-----------------------------------------------------------------
derive.E(a * X + b)

## ----derive-e-defined---------------------------------------------------------
clear.definitions()
define(X ~ Normal(mu, sigma))

derive.E(a * X + b)

clear.definitions()

## ----derive-e-linear----------------------------------------------------------
derive.E(X + Y + Z)

## ----derive-var---------------------------------------------------------------
derive.Var(X)

## ----derive-var-linear--------------------------------------------------------
derive.Var(a * X + b)

## ----derive-var-defined-------------------------------------------------------
clear.definitions()
define(X ~ Normal(mu, sigma))

derive.Var(X)

clear.definitions()

## ----derive-cov---------------------------------------------------------------
derive.Cov(X, Y)

## ----structure----------------------------------------------------------------
d <- derive.E(X + Y)

d$result           # Final answer
length(d$steps)    # Number of steps

## ----teach-linearity----------------------------------------------------------
derive.E(a * X + b)

## ----teach-var----------------------------------------------------------------
derive.Var(a * X + b)

## ----verify-------------------------------------------------------------------
clear.definitions()
define(X ~ Poisson(lambda))

# Verify Var(Poisson) = λ
derive.Var(X)

clear.definitions()


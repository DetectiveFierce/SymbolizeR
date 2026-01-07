# SymbolizeR

A lightweight symbolic probability engine for R that helps derive Expectations, Variances, and Covariances using standard R syntax.

## Installation

```r
# Install from source
devtools::install_local("path/to/SymbolizeR")

# Or load for development
devtools::load_all()
```

## Usage

### Symbolic Expectation

```r
library(SymbolizeR)

E(X)           # E(X)
E(a * X)       # a * E(X)
E(X + Y)       # E(X) + E(Y)
E(2 * X + 3)   # 2 * E(X) + 3
```

### Symbolic Variance

```r
Var(X)         # E(X^2) - E(X)^2
```

### Symbolic Covariance

```r
Cov(X, Y)      # E(X * Y) - E(X) * E(Y)
```

## The Capital Letter Heuristic

SymbolizeR uses a simple naming convention to distinguish between random variables and constants:

| First Character | Classification | Examples |
|-----------------|----------------|----------|
| **Uppercase** (A-Z) | Random Variable | `X`, `Y`, `Normal`, `Sigma` |
| **Lowercase** (a-z) | Constant | `a`, `b`, `mu`, `sigma`, `beta` |
| **Numeric** | Number | `1`, `2.5`, `3.14` |

This means:
- `E(a * X)` → `a * E(X)` (constant pulled out)
- `E(X * Y)` → `E(X * Y)` (cannot simplify product of RVs)

## Mathematical Properties Applied

The engine applies the **linearity of expectation**:

1. **E[aX] = a·E[X]** — Constants factor out
2. **E[X + Y] = E[X] + E[Y]** — Expectation distributes over addition
3. **E[c] = c** — Expectation of a constant is the constant

## Scope & Limitations

SymbolizeR performs **symbolic manipulation only**. It does not:

- Solve integrals
- Evaluate numerical expectations
- Assume independence (unless explicitly told)

If an expectation cannot be simplified (e.g., `E(1/X)`), it returns `E(1/X)` as-is.

## License

MIT

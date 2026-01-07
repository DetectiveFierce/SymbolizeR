# SymbolizeR: Symbolic Probability Engine

A lightweight R package for deriving Expectations, Variances, and Covariances symbolically using standard R syntax.

## Proposed Changes

### Package Metadata

#### [MODIFY] [DESCRIPTION](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/DESCRIPTION)
Update with proper title, description, and license.

---

### Core Implementation

#### [NEW] [00_utils.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/R/00_utils.R)

Helper functions for the type system:
- `classify_type(sym)` - Inspects a symbol and returns `"number"`, `"rv"`, or `"const"` based on naming conventions (capitals = RV, lowercase = constant)
- `ensure_expression(expr)` - Handles NSE logic to normalize input expressions

---

#### [NEW] [01_internal_expect.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/R/01_internal_expect.R)

The recursive expectation engine:
- `expect_recursive(expr)` - Recursively transforms expressions by applying linearity of expectation
  - Base cases: numerics return as-is, constants return as-is, RVs wrap in `E()`
  - Addition/subtraction: recurse on both sides (linearity)
  - Multiplication: pull out constants, stop at RV*RV products

---

#### [NEW] [02_internal_simplify.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/R/02_internal_simplify.R)

Expression simplification:
- `simplify_expr(expr)` - Recursive simplifier that prunes identity elements
  - Rules: `x + 0 → x`, `x * 1 → x`, `x * 0 → 0`, `0 / x → 0`, `E(c) → c`

---

#### [NEW] [03_user_wrappers.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/R/03_user_wrappers.R)

Public API using Non-Standard Evaluation:
- `E(expr)` - Computes symbolic expectation
- `Var(expr)` - Computes variance as `E(X²) - E(X)²`
- `Cov(x, y)` - Computes covariance as `E(XY) - E(X)E(Y)`

---

### Testing Infrastructure

#### [NEW] [tests/testthat.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/tests/testthat.R)
Standard testthat loader.

#### [NEW] [tests/testthat/test-utils.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/tests/testthat/test-utils.R)
Tests for `classify_type()` function covering numerics, RVs, and constants.

#### [NEW] [tests/testthat/test-expect.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/tests/testthat/test-expect.R)
Tests for `expect_recursive()` covering linearity and multiplication cases.

#### [NEW] [tests/testthat/test-simplify.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/tests/testthat/test-simplify.R)
Tests for simplification rules.

#### [NEW] [tests/testthat/test-wrappers.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/tests/testthat/test-wrappers.R)
Integration tests for `E()`, `Var()`, `Cov()`.

---

### Documentation

#### [NEW] [README.md](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/README.md)
Package overview with:
- Installation instructions
- Usage examples
- Explanation of the Capital Letter heuristic
- Scope limitations

---

## Verification Plan

### Automated Tests

Run the full testthat suite:
```bash
cd "/home/shared-psychosis/Landing Zone/R Projects/SymbolizeR"
Rscript -e "devtools::test()"
```

Key test cases:
1. `E(a*X)` should produce `quote(a * E(X))`
2. `E(X + Y)` should produce `quote(E(X) + E(Y))`
3. `E(5)` should produce `5`
4. `Var(X)` should produce the variance formula
5. Simplification of `x + 0` should yield `x`

### Manual Verification

Interactive REPL testing:
```r
devtools::load_all()
E(a * X + b)        # Should show: a * E(X) + b
Var(X)              # Should show variance formula
Cov(X, Y)           # Should show covariance formula
```

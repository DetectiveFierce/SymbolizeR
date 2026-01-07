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

**`classify.type(sym)`** - Inspects a symbol and classifies it
```r
#' @title Classify Expression Type
#' @description Determines if a symbol is a number, random variable, or constant
#'   based on naming conventions. Uppercase = RV, lowercase = constant.
#' @param sym A symbol or expression to classify
#' @return Character: "number", "rv", or "const"
#' @examples
#' classify.type(quote(X))     # "rv"
#' classify.type(quote(mu))    # "const"
#' classify.type(5)            # "number"
#' @keywords internal
```

**`ensure.expression(expr)`** - Handles NSE normalization
```r
#' @title Ensure Expression
#' @description Normalizes input to an unevaluated expression, handling both
#'   raw calls and variables holding calls.
#' @param expr An expression or variable containing an expression
#' @return An unevaluated call object
#' @keywords internal
```

---

#### [NEW] [01_internal_expect.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/R/01_internal_expect.R)

**`expect.recursive(expr)`** - The recursive expectation engine
```r
#' @title Recursive Expectation Engine
#' @description Recursively transforms an expression tree by applying the
#'   linearity of expectation. Constants pass through unchanged, random
#'   variables are wrapped in E(), and operators are handled appropriately.
#' @param expr An unevaluated R expression
#' @return A transformed expression with E() applied symbolically
#' @details
#' \itemize{
#'   \item Numerics and constants return unchanged
#'   \item Random variables (uppercase) are wrapped in \code{E()}
#'   \item Addition/subtraction: E(X + Y) = E(X) + E(Y)
#'   \item Const * RV: E(a*X) = a*E(X)
#'   \item RV * RV: Returns E(X*Y) (cannot simplify without independence)
#' }
#' @keywords internal
```

---

#### [NEW] [02_internal_simplify.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/R/02_internal_simplify.R)

**`simplify.expr(expr)`** - Expression simplifier
```r
#' @title Simplify Expression
#' @description Recursively simplifies an expression by removing identity
#'   elements and evaluating trivial operations.
#' @param expr An unevaluated R expression
#' @return A simplified expression
#' @details
#' Simplification rules applied:
#' \itemize{
#'   \item \code{x + 0} → \code{x}
#'   \item \code{x * 1} → \code{x}
#'   \item \code{x * 0} → \code{0}
#'   \item \code{0 / x} → \code{0}
#'   \item \code{E(c)} where c is constant → \code{c}
#' }
#' @keywords internal
```

---

#### [NEW] [03_user_wrappers.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/R/03_user_wrappers.R)

Public API with full documentation:

**`E(expr)`** - Symbolic expectation
```r
#' @title Symbolic Expectation
#' @description Computes the symbolic expectation of an expression using
#'   the linearity of expectation. Uses non-standard evaluation.
#' @param expr An R expression involving random variables and constants.
#'   Random variables are identified by uppercase first letter.
#' @return An unevaluated expression representing E[expr]
#' @export
#' @examples
#' E(X)           # Returns: E(X)
#' E(a * X)       # Returns: a * E(X)
#' E(X + Y)       # Returns: E(X) + E(Y)
#' E(2 * X + 3)   # Returns: 2 * E(X) + 3
```

**`Var(expr)`** - Symbolic variance
```r
#' @title Symbolic Variance
#' @description Computes the symbolic variance using the identity
#'   Var(X) = E(X^2) - E(X)^2
#' @param expr An R expression involving random variables and constants
#' @return An unevaluated expression representing Var[expr]
#' @export
#' @examples
#' Var(X)         # Returns: E(X^2) - E(X)^2
#' Var(a * X)     # Returns: a^2 * (E(X^2) - E(X)^2)
```

**`Cov(x, y)`** - Symbolic covariance
```r
#' @title Symbolic Covariance
#' @description Computes the symbolic covariance using the identity
#'   Cov(X, Y) = E(XY) - E(X)E(Y)
#' @param x First random variable expression
#' @param y Second random variable expression
#' @return An unevaluated expression representing Cov[x, y]
#' @export
#' @examples
#' Cov(X, Y)      # Returns: E(X * Y) - E(X) * E(Y)
```

---

### Testing Infrastructure

#### [NEW] [tests/testthat.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/tests/testthat.R)
Standard testthat loader.

#### [NEW] [tests/testthat/test-utils.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/tests/testthat/test-utils.R)
Tests for `classify.type()` function covering numerics, RVs, and constants.

#### [NEW] [tests/testthat/test-expect.R](file:///home/shared-psychosis/Landing%20Zone/R%20Projects/SymbolizeR/tests/testthat/test-expect.R)
Tests for `expect.recursive()` covering linearity and multiplication cases.

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

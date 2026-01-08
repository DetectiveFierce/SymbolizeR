# =============================================================================
# Symbolic Integration Module
# Definite Integration via Kernel Recognition
# =============================================================================


#' @title Safely Negate an Expression
#' @description Negates an expression, computing the result directly if the
#'   expression evaluates to a pure number, otherwise creating a unary minus call.
#'   Handles double negation: -(-x) returns x.
#' @param expr An expression to negate
#' @return The negated expression (numeric if input is numeric, otherwise call)
#' @keywords internal
negate.expr <- function(expr) {
  # If already numeric, negate directly
  if (is.numeric(expr)) {
    return(-expr)
  }
  
  # Check for double negation: if expr is already -something, return something
  if (is.call(expr) && as.character(expr[[1]]) == "-" && length(expr) == 2) {
    # expr is -inner, so -expr is --inner = inner
    return(expr[[2]])
  }
  
  # Try to evaluate as a pure number (e.g., quote(-2) -> -2)
  result <- tryCatch({
    val <- eval(expr)
    if (is.numeric(val) && length(val) == 1) {
      return(-val)
    }
    NULL
  }, error = function(e) NULL)
  
  if (!is.null(result)) {
    return(result)
  }
  
  # Cannot evaluate to number, create a call
  simplify.expr(call("-", expr))
}


# -----------------------------------------------------------------------------
# INTEGRATION HELPER FUNCTIONS
# -----------------------------------------------------------------------------

#' @title Partition Integrand
#' @description Separates an integrand into constant and variable-dependent parts.
#'   For a product C * f(x), returns list(const = C, var = f(x)).
#' @param expr The integrand expression
#' @param var The variable of integration (as symbol)
#' @return A list with 'const' and 'var' components
#' @keywords internal
partition.integrand <- function(expr, var) {
  # Base case: expression is a product
  if (is.call(expr) && as.character(expr[[1]]) == "*") {
    left <- expr[[2]]
    right <- expr[[3]]
    
    has_left <- contains.var(left, var)
    has_right <- contains.var(right, var)
    
    if (has_left && !has_right) {
      # Recurse on left, accumulate right in const
      sub <- partition.integrand(left, var)
      return(list(
        const = simplify.expr(call("*", sub$const, right)),
        var = sub$var
      ))
    }
    
    if (!has_left && has_right) {
      # Recurse on right, accumulate left in const
      sub <- partition.integrand(right, var)
      return(list(
        const = simplify.expr(call("*", left, sub$const)),
        var = sub$var
      ))
    }
    
    if (!has_left && !has_right) {
      # Entire expression is constant
      return(list(const = expr, var = 1))
    }
    
    # Both sides contain var - try to partition each
    left_part <- partition.integrand(left, var)
    right_part <- partition.integrand(right, var)
    
    return(list(
      const = simplify.expr(call("*", left_part$const, right_part$const)),
      var = simplify.expr(call("*", left_part$var, right_part$var))
    ))
  }
  
  # Handle exp(a + b) -> exp(a) * exp(b) for partitioning
  if (is.call(expr) && as.character(expr[[1]]) == "exp") {
    inner <- expr[[2]]
    
    if (is.call(inner) && as.character(inner[[1]]) == "+") {
      left <- inner[[2]]
      right <- inner[[3]]
      
      has_left <- contains.var(left, var)
      has_right <- contains.var(right, var)
      
      if (has_left && !has_right) {
        return(list(
          const = call("exp", right),
          var = call("exp", left)
        ))
      }
      
      if (!has_left && has_right) {
        return(list(
          const = call("exp", left),
          var = call("exp", right)
        ))
      }
    }
  }
  
  # Default: if contains var, all goes to var part; else all to const
  if (contains.var(expr, var)) {
    return(list(const = 1, var = expr))
  } else {
    return(list(const = expr, var = 1))
  }
}


#' @title Extract Quadratic Coefficients
#' @description Extracts coefficients from a quadratic expression in var:
#'   c2*x^2 + c1*x + c0. Returns NULL if not quadratic.
#' @param expr The expression to analyze
#' @param var The variable symbol
#' @return A list with c2, c1, c0 coefficients, or NULL
#' @keywords internal
extract.quadratic <- function(expr, var) {
  var_name <- as.character(var)
  
  # We'll collect terms and identify coefficients
  # This is a simplified approach - we differentiate twice
  
  # If expr doesn't contain var, it's c0 only
  if (!contains.var(expr, var)) {
    return(list(c2 = 0, c1 = 0, c0 = expr))
  }
  
  # Get first derivative (should be 2*c2*x + c1)
  d1 <- deriv.sym(expr, var)
  
  # Get second derivative (should be 2*c2)
  d2 <- deriv.sym(d1, var)
  
  # If d2 still contains var, it's not quadratic
  if (contains.var(d2, var)) {
    return(NULL)
  }
  
  # c2 = d2 / 2
  c2 <- simplify.expr(call("/", d2, 2))
  
  # Evaluate d1 at x=0 to get c1
  # d1 at x=0: substitute 0 for var
  c1 <- substitute.var(d1, var, 0)
  c1 <- simplify.expr(c1)
  
  # Evaluate expr at x=0 to get c0
  c0 <- substitute.var(expr, var, 0)
  c0 <- simplify.expr(c0)
  
  list(c2 = c2, c1 = c1, c0 = c0)
}


#' @title Substitute Variable with Value
#' @description Replaces all occurrences of a variable with a value.
#' @param expr The expression
#' @param var The variable to replace (symbol)
#' @param val The value to substitute
#' @return Modified expression
#' @keywords internal
substitute.var <- function(expr, var, val) {
  if (is.numeric(expr) || is.logical(expr)) {
    return(expr)
  }
  
  if (is.symbol(expr)) {
    if (identical(expr, var)) {
      return(val)
    }
    return(expr)
  }
  
  if (is.call(expr)) {
    new_args <- lapply(as.list(expr)[-1], substitute.var, var = var, val = val)
    new_call <- as.call(c(expr[[1]], new_args))
    # Try to evaluate if all numeric
    tryCatch({
      if (all(vapply(new_args, is.numeric, logical(1)))) {
        return(eval(new_call))
      }
    }, error = function(e) NULL)
    return(new_call)
  }
  
  expr
}


#' @title Extract Power Base and Exponent
#' @description For expressions like x^a, extracts base and exponent.
#' @param expr The expression
#' @param var The variable symbol
#' @return List with 'base' and 'exp', or NULL if not a power of var
#' @keywords internal
extract.power <- function(expr, var) {
  # Just the variable: x = x^1
  if (is.symbol(expr) && identical(expr, var)) {
    return(list(base = var, exp = 1))
  }
  
  # Power expression
  if (is.call(expr) && as.character(expr[[1]]) == "^") {
    base <- expr[[2]]
    exp <- expr[[3]]
    
    # Check if base is just the variable
    if (is.symbol(base) && identical(base, var)) {
      return(list(base = var, exp = exp))
    }
  }
  
  NULL
}


#' @title Extract Linear Exponent
#' @description For exp(-B*x) or exp(B*x), extracts B (with sign).
#' @param expr An exp() call
#' @param var The variable symbol
#' @return The coefficient B where exponent = B*x, or NULL
#' @keywords internal
extract.linear.exp <- function(expr, var) {
  if (!is.call(expr) || as.character(expr[[1]]) != "exp") {
    return(NULL)
  }
  
  inner <- expr[[2]]
  
  # Case: exp(x) -> coefficient is 1
  if (is.symbol(inner) && identical(inner, var)) {
    return(1)
  }
  
  # Case: exp(-x) -> coefficient is -1
  if (is.call(inner) && as.character(inner[[1]]) == "-" && length(inner) == 2) {
    if (is.symbol(inner[[2]]) && identical(inner[[2]], var)) {
      return(-1)
    }
  }
  
  # Case: exp(a * x) or exp(x * a)
  if (is.call(inner) && as.character(inner[[1]]) == "*") {
    left <- inner[[2]]
    right <- inner[[3]]
    
    if (is.symbol(right) && identical(right, var) && !contains.var(left, var)) {
      return(left)
    }
    if (is.symbol(left) && identical(left, var) && !contains.var(right, var)) {
      return(right)
    }
  }
  
  # Case: exp(-a * x) or exp(-(a * x))
  if (is.call(inner) && as.character(inner[[1]]) == "-" && length(inner) == 2) {
    neg_inner <- inner[[2]]
    if (is.call(neg_inner) && as.character(neg_inner[[1]]) == "*") {
      left <- neg_inner[[2]]
      right <- neg_inner[[3]]
      
      if (is.symbol(right) && identical(right, var) && !contains.var(left, var)) {
        return(simplify.expr(call("-", left)))
      }
      if (is.symbol(left) && identical(left, var) && !contains.var(right, var)) {
        return(simplify.expr(call("-", right)))
      }
    }
    # exp(-x) was handled above
  }
  
  NULL
}


# -----------------------------------------------------------------------------
# KERNEL MATCHERS
# -----------------------------------------------------------------------------

#' @title Match Gamma Kernel
#' @description Attempts to match x^(A-1) * exp(-B*x) pattern.
#'   Returns Gamma(A) / B^A
#' @param var_part The variable-dependent part of integrand
#' @param var The variable symbol
#' @return The integral result, or NULL if no match
#' @keywords internal
match.gamma.kernel <- function(var_part, var) {
  # Expect a product of power and exponential
  if (!is.call(var_part)) return(NULL)
  
  op <- as.character(var_part[[1]])
  
  # Handle single exp (A = 1 case): integral of exp(-B*x) = 1/B
  if (op == "exp") {
    coef <- extract.linear.exp(var_part, var)
    if (!is.null(coef)) {
      # Need coefficient to be negative for convergence
      # Result: Gamma(1) / B^1 = 1/B where B = -coef
      B <- negate.expr(coef)
      return(simplify.expr(call("/", 1, B)))
    }
    return(NULL)
  }
  
  # Handle single x^a (degenerate, won't converge without exp)
  if (op == "^") {
    return(NULL)  # Need exp term for convergence
  }
  
  # Handle product
  if (op != "*") return(NULL)
  
  left <- var_part[[2]]
  right <- var_part[[3]]
  
  # Try both orderings
  power_info <- extract.power(left, var)
  exp_coef <- extract.linear.exp(right, var)
  
  if (is.null(power_info) || is.null(exp_coef)) {
    power_info <- extract.power(right, var)
    exp_coef <- extract.linear.exp(left, var)
  }
  
  if (is.null(power_info) || is.null(exp_coef)) {
    return(NULL)
  }
  
  # A = power + 1 (since kernel is x^(A-1))
  A <- simplify.expr(call("+", power_info$exp, 1))
  
  # B = -exp_coef (since kernel is exp(-B*x))
  B <- negate.expr(exp_coef)
  
  # Result: Gamma(A) / B^A
  result <- call("/", call("gamma", A), call("^", B, A))
  simplify.expr(result)
}


#' @title Match Gaussian Kernel
#' @description Attempts to match exp(-A*x^2 + B*x + C) pattern.
#'   Returns sqrt(pi/A) * exp(B^2/(4*A) + C)
#' @param var_part The variable-dependent part of integrand
#' @param var The variable symbol
#' @return The integral result, or NULL if no match
#' @keywords internal
match.gaussian.kernel <- function(var_part, var) {
  if (!is.call(var_part) || as.character(var_part[[1]]) != "exp") {
    return(NULL)
  }
  
  inner <- var_part[[2]]
  
  # Extract quadratic coefficients
  quad <- extract.quadratic(inner, var)
  if (is.null(quad)) return(NULL)
  
  c2 <- quad$c2  # coefficient of x^2
  c1 <- quad$c1  # coefficient of x
  c0 <- quad$c0  # constant
  
  # Need c2 to be negative (i.e., -A where A > 0)
  # A = -c2
  A <- simplify.expr(call("-", c2))
  B <- c1
  
  # Result: sqrt(pi/A) * exp(B^2/(4*A))
  # The c0 constant was already factored out by partition.integrand
  
  sqrt_part <- call("sqrt", call("/", quote(pi), A))
  
  # exp(B^2 / (4*A))
  exp_part <- call("exp", call("/", call("^", B, 2), call("*", 4, A)))
  
  result <- call("*", sqrt_part, exp_part)
  simplify.expr(result)
}


#' @title Match Beta Kernel
#' @description Attempts to match x^(A-1) * (1-x)^(B-1) pattern.
#'   Returns Beta(A, B) = Gamma(A)*Gamma(B)/Gamma(A+B)
#' @param var_part The variable-dependent part of integrand
#' @param var The variable symbol
#' @return The integral result, or NULL if no match
#' @keywords internal
match.beta.kernel <- function(var_part, var) {
  if (!is.call(var_part) || as.character(var_part[[1]]) != "*") {
    # Check for single terms (A=1 or B=1 cases)
    power_info <- extract.power(var_part, var)
    if (!is.null(power_info)) {
      # x^a -> Beta(a+1, 1) (if bounds are 0 to 1)
      A <- simplify.expr(call("+", power_info$exp, 1))
      return(call("beta", A, 1))
    }
    
    # Check for (1-x)^b pattern
    if (is.call(var_part) && as.character(var_part[[1]]) == "^") {
      base <- var_part[[2]]
      exp <- var_part[[3]]
      
      if (is.call(base) && as.character(base[[1]]) == "-" && length(base) == 3) {
        if (identical(base[[2]], 1) || identical(base[[2]], 1L)) {
          if (is.symbol(base[[3]]) && identical(base[[3]], var)) {
            B <- simplify.expr(call("+", exp, 1))
            return(call("beta", 1, B))
          }
        }
      }
    }
    
    return(NULL)
  }
  
  left <- var_part[[2]]
  right <- var_part[[3]]
  
  # Extract x^(a) from one side
  power_x <- NULL
  power_1mx <- NULL
  
  # Check left for x^a
  if (is.symbol(left) && identical(left, var)) {
    power_x <- 1
  } else {
    pi <- extract.power(left, var)
    if (!is.null(pi)) {
      power_x <- pi$exp
    }
  }
  
  # Check right for (1-x)^b
  if (is.call(right)) {
    if (as.character(right[[1]]) == "^") {
      base <- right[[2]]
      exp <- right[[3]]
      
      # Check if base is (1 - x)
      if (is.call(base) && as.character(base[[1]]) == "-" && length(base) == 3) {
        if ((identical(base[[2]], 1) || identical(base[[2]], 1L)) &&
            is.symbol(base[[3]]) && identical(base[[3]], var)) {
          power_1mx <- exp
        }
      }
      
      # Check if base is wrapped in parens
      if (is.call(base) && as.character(base[[1]]) == "(") {
        inner <- base[[2]]
        if (is.call(inner) && as.character(inner[[1]]) == "-" && length(inner) == 3) {
          if ((identical(inner[[2]], 1) || identical(inner[[2]], 1L)) &&
              is.symbol(inner[[3]]) && identical(inner[[3]], var)) {
            power_1mx <- exp
          }
        }
      }
    }
    
    # Check if right is just (1-x) without explicit power
    if (is.call(right) && as.character(right[[1]]) == "-" && length(right) == 3) {
      if ((identical(right[[2]], 1) || identical(right[[2]], 1L)) &&
          is.symbol(right[[3]]) && identical(right[[3]], var)) {
        power_1mx <- 1
      }
    }
    
    # Check if right is ((1-x)) - wrapped in parentheses
    if (is.call(right) && as.character(right[[1]]) == "(") {
      inner <- right[[2]]
      if (is.call(inner) && as.character(inner[[1]]) == "-" && length(inner) == 3) {
        if ((identical(inner[[2]], 1) || identical(inner[[2]], 1L)) &&
            is.symbol(inner[[3]]) && identical(inner[[3]], var)) {
          power_1mx <- 1
        }
      }
    }
  }
  
  # If we didn't find both, try swapping
  if (is.null(power_x) || is.null(power_1mx)) {
    power_x <- NULL
    power_1mx <- NULL
    
    # Check right for x^a
    if (is.symbol(right) && identical(right, var)) {
      power_x <- 1
    } else {
      pi <- extract.power(right, var)
      if (!is.null(pi)) {
        power_x <- pi$exp
      }
    }
    
    # Check left for (1-x)^b
    if (is.call(left)) {
      if (as.character(left[[1]]) == "^") {
        base <- left[[2]]
        exp <- left[[3]]
        
        if (is.call(base) && as.character(base[[1]]) == "-" && length(base) == 3) {
          if ((identical(base[[2]], 1) || identical(base[[2]], 1L)) &&
              is.symbol(base[[3]]) && identical(base[[3]], var)) {
            power_1mx <- exp
          }
        }
        
        if (is.call(base) && as.character(base[[1]]) == "(") {
          inner <- base[[2]]
          if (is.call(inner) && as.character(inner[[1]]) == "-" && length(inner) == 3) {
            if ((identical(inner[[2]], 1) || identical(inner[[2]], 1L)) &&
                is.symbol(inner[[3]]) && identical(inner[[3]], var)) {
              power_1mx <- exp
            }
          }
        }
      }
      
      if (is.call(left) && as.character(left[[1]]) == "-" && length(left) == 3) {
        if ((identical(left[[2]], 1) || identical(left[[2]], 1L)) &&
            is.symbol(left[[3]]) && identical(left[[3]], var)) {
          power_1mx <- 1
        }
      }
      
      # Check if left is ((1-x)) - wrapped in parentheses
      if (is.call(left) && as.character(left[[1]]) == "(") {
        inner <- left[[2]]
        if (is.call(inner) && as.character(inner[[1]]) == "-" && length(inner) == 3) {
          if ((identical(inner[[2]], 1) || identical(inner[[2]], 1L)) &&
              is.symbol(inner[[3]]) && identical(inner[[3]], var)) {
            power_1mx <- 1
          }
        }
      }
    }
  }
  
  if (is.null(power_x) || is.null(power_1mx)) {
    return(NULL)
  }
  
  # A = power_x + 1, B = power_1mx + 1
  A <- simplify.expr(call("+", power_x, 1))
  B <- simplify.expr(call("+", power_1mx, 1))
  
  # Result: beta(A, B)
  call("beta", A, B)
}


# -----------------------------------------------------------------------------
# MAIN INTEGRATION FUNCTION
# -----------------------------------------------------------------------------

#' @title Symbolic Definite Integration
#' @description Computes a definite integral using kernel recognition. Recognizes
#'   Gamma, Gaussian, and Beta distribution kernels and returns their known
#'   normalizing constants. Falls back to an unevaluated Integrate() call if
#'   no pattern matches.
#' @param expr The integrand (uses non-standard evaluation, no quoting needed)
#' @param var The variable of integration (no quoting needed)
#' @param lower Lower bound of integration (default -Inf)
#' @param upper Upper bound of integration (default Inf)
#' @return Simplified expression of the result OR unevaluated Integrate() call
#' @export
#' @examples
#' # Gamma kernel: integral of x^2 * exp(-x) from 0 to Inf = Gamma(3) = 2
#' integrate.sym(x^2 * exp(-x), x, 0, Inf)
#'
#' # Gaussian kernel: integral of exp(-x^2) from -Inf to Inf = sqrt(pi)
#' integrate.sym(exp(-x^2), x, -Inf, Inf)
#'
#' # Beta kernel: integral of x * (1-x) from 0 to 1 = Beta(2, 2) = 1/6
#' integrate.sym(x * (1-x), x, 0, 1)
integrate.sym <- function(expr, var, lower = -Inf, upper = Inf) {
  # Capture inputs using NSE
  expr <- substitute(expr)
  expr <- ensure.expression(expr)
  
  var <- substitute(var)
  
  # Ensure var is a symbol
  if (is.character(var)) {
    var <- as.symbol(var)
  }
  
  # If expr doesn't contain var, it's a constant integral
  if (!contains.var(expr, var)) {
    # Constant * (upper - lower)
    if (is.infinite(lower) || is.infinite(upper)) {
      if (lower == -Inf && upper == Inf) {
        # Constant times infinity - undefined unless const = 0
        if (identical(expr, 0) || identical(expr, 0L)) {
          return(0)
        }
        # Return unevaluated
        return(call("Integrate", expr, var, lower, upper))
      }
    }
    return(simplify.expr(call("*", expr, call("-", upper, lower))))
  }
  
  # Partition the integrand
  parts <- partition.integrand(expr, var)
  const_part <- parts$const
  var_part <- parts$var
  
  # If var_part is just 1, we have a constant
  if (identical(var_part, 1) || identical(var_part, 1L)) {
    if (is.infinite(lower) || is.infinite(upper)) {
      return(call("Integrate", expr, var, lower, upper))
    }
    return(simplify.expr(call("*", const_part, call("-", upper, lower))))
  }
  
  result <- NULL
  
  # Try kernels based on bounds
  if (identical(lower, 0) && is.infinite(upper) && upper > 0) {
    # (0, Inf) - Try Gamma kernel
    result <- match.gamma.kernel(var_part, var)
  } else if (is.infinite(lower) && lower < 0 && is.infinite(upper) && upper > 0) {
    # (-Inf, Inf) - Try Gaussian kernel
    result <- match.gaussian.kernel(var_part, var)
  } else if (identical(lower, 0) && identical(upper, 1)) {
    # (0, 1) - Try Beta kernel
    result <- match.beta.kernel(var_part, var)
  }
  
  # If we found a match, multiply by constant part
  if (!is.null(result)) {
    final <- simplify.expr(call("*", const_part, result))
    return(final)
  }
  
  # No match - return unevaluated
  call("Integrate", expr, var, lower, upper)
}

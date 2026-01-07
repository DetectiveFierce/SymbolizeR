#' @title Simplify Expression
#' @description Recursively simplifies an expression by removing identity
#'   elements and evaluating trivial operations.
#' @param expr An unevaluated R expression
#' @return A simplified expression
#' @details
#' Simplification rules applied:
#' \itemize{
#'   \item \code{x + 0} -> \code{x}
#'   \item \code{x * 1} -> \code{x}
#'   \item \code{x * 0} -> \code{0}
#'   \item \code{0 / x} -> \code{0}
#'   \item \code{E(c)} where c is constant -> \code{c}
#'   \item \code{a*X + b*X} -> \code{(a+b)*X}
#' }
#' @keywords internal

# ============================================================================
# HELPER FUNCTIONS FOR LIKE-TERM COMBINATION
# ============================================================================

#' @title Extract Coefficient and Base
#' @description Extracts the coefficient and base from an expression.
#'   For a*X returns list(coef=a, base=X). For X returns list(coef=1, base=X).
#' @param expr An expression
#' @return List with 'coef' and 'base', or NULL if not extractable
#' @keywords internal
extract.coef.base <- function(expr) {
  # Simple symbol: X = 1 * X
  if (is.symbol(expr)) {
    return(list(coef = 1, base = expr))
  }
  
  # Numeric: not a term we combine
  if (is.numeric(expr)) {
    return(NULL)
  }
  
  if (!is.call(expr)) {
    return(NULL)
  }
  
  op <- as.character(expr[[1]])
  
  # Handle multiplication: a * X or X * a
  if (op == "*") {
    left <- expr[[2]]
    right <- expr[[3]]
    
    # Check if left is numeric constant
    if (is.numeric(left)) {
      # Right is the base
      inner <- extract.coef.base(right)
      if (!is.null(inner)) {
        return(list(coef = call("*", left, inner$coef), base = inner$base))
      }
      return(list(coef = left, base = right))
    }
    
    # Check if right is numeric constant
    if (is.numeric(right)) {
      inner <- extract.coef.base(left)
      if (!is.null(inner)) {
        return(list(coef = call("*", inner$coef, right), base = inner$base))
      }
      return(list(coef = right, base = left))
    }
    
    # Check if left is a constant symbol (lowercase)
    if (is.symbol(left) && classify.type(left) == "const") {
      inner <- extract.coef.base(right)
      if (!is.null(inner)) {
        return(list(coef = call("*", left, inner$coef), base = inner$base))
      }
      return(list(coef = left, base = right))
    }
    
    # Check if right is a constant symbol
    if (is.symbol(right) && classify.type(right) == "const") {
      inner <- extract.coef.base(left)
      if (!is.null(inner)) {
        return(list(coef = call("*", inner$coef, right), base = inner$base))
      }
      return(list(coef = right, base = left))
    }
  }
  
  # Handle E() calls: E(X) has base E(X) and coef 1
  if (op == "E") {
    return(list(coef = 1, base = expr))
  }
  
  # For other calls, treat entire expr as base
  return(list(coef = 1, base = expr))
}


#' @title Try to Combine Like Terms
#' @description Attempts to combine like terms in addition/subtraction.
#'   a*X + b*X -> (a+b)*X, a*X - b*X -> (a-b)*X
#' @param left Left simplified expression
#' @param right Right simplified expression
#' @param op The operator ("+" or "-")
#' @return Combined expression, or NULL if terms are not alike
#' @keywords internal
try.combine.like.terms <- function(left, right, op) {
  left_parts <- extract.coef.base(left)
  right_parts <- extract.coef.base(right)
  
  if (is.null(left_parts) || is.null(right_parts)) {
    return(NULL)
  }
  
  # Check if bases are identical
  if (!identical(deparse(left_parts$base), deparse(right_parts$base))) {
    return(NULL)
  }
  
  # Combine coefficients
  new_coef <- if (op == "+") {
    call("+", left_parts$coef, right_parts$coef)
  } else {
    call("-", left_parts$coef, right_parts$coef)
  }
  
  # Simplify the coefficient
  new_coef <- simplify.coef(new_coef)
  
  # Return combined term
  if (identical(new_coef, 0) || identical(new_coef, 0L)) {
    return(0)
  }
  if (identical(new_coef, 1) || identical(new_coef, 1L)) {
    return(left_parts$base)
  }
  
  return(call("*", new_coef, left_parts$base))
}


#' @title Simplify Coefficient Expression
#' @description Simplifies numeric coefficient expressions like 1 + 1 -> 2.
#' @param coef A coefficient expression
#' @return Simplified coefficient
#' @keywords internal
simplify.coef <- function(coef) {
  # Try to evaluate if all numeric
  if (is.numeric(coef)) {
    return(coef)
  }
  
  if (is.call(coef)) {
    # Try to evaluate numeric expressions
    result <- tryCatch({
      args <- as.list(coef)[-1]
      if (all(sapply(args, is.numeric))) {
        return(eval(coef))
      }
      coef
    }, error = function(e) coef)
    return(result)
  }
  
  coef
}


# ============================================================================
# POLYNOMIAL TERM COLLECTION
# ============================================================================

#' @title Normalize Product
#' @description Recursively normalizes a product expression, extracting all
#'   numeric coefficients and combining variable powers.
#' @param expr A product expression
#' @return A list with 'coef' (numeric) and 'base' (expression with combined powers)
#' @keywords internal
normalize.product <- function(expr) {
  coef <- 1
  vars <- list()  # name -> power
  other <- NULL
  
  process <- function(e) {
    if (is.numeric(e)) {
      coef <<- coef * e
    } else if (is.symbol(e)) {
      name <- as.character(e)
      vars[[name]] <<- (vars[[name]] %||% 0) + 1
    } else if (is.call(e)) {
      op <- as.character(e[[1]])
      if (op == "^" && is.symbol(e[[2]]) && is.numeric(e[[3]])) {
        name <- as.character(e[[2]])
        vars[[name]] <<- (vars[[name]] %||% 0) + e[[3]]
      } else if (op == "*") {
        process(e[[2]])
        process(e[[3]])
      } else if (op == "(") {
        process(e[[2]])
      } else {
        # Non-decomposable - can't normalize further
        other <<- e
      }
    }
  }
  
  process(expr)
  
  if (!is.null(other)) {
    return(list(coef = 1, base = expr))  # Can't normalize
  }
  
  # Build canonical base from vars
  if (length(vars) == 0) {
    return(list(coef = coef, base = 1))
  }
  
  base <- factors.to.expr(vars)
  list(coef = coef, base = base)
}


#' @title Flatten Expression to Terms
#' @description Flattens an expression into a list of (coefficient, base) pairs.
#'   Handles nested additions and subtractions.
#' @param expr An expression
#' @param sign The sign multiplier (1 or -1)
#' @return A list of lists with 'coef' and 'base'
#' @keywords internal
flatten.to.terms <- function(expr, sign = 1) {
  if (is.numeric(expr)) {
    return(list(list(coef = sign * expr, base = 1)))
  }
  
  if (is.symbol(expr)) {
    return(list(list(coef = sign, base = expr)))
  }
  
  if (!is.call(expr)) {
    return(list(list(coef = sign, base = expr)))
  }
  
  op <- as.character(expr[[1]])
  
  # Handle addition: flatten both sides

if (op == "+") {
    if (length(expr) == 2) {
      # Unary plus
      return(flatten.to.terms(expr[[2]], sign))
    }
    left_terms <- flatten.to.terms(expr[[2]], sign)
    right_terms <- flatten.to.terms(expr[[3]], sign)
    return(c(left_terms, right_terms))
  }
  
  # Handle subtraction: negate right side
  if (op == "-") {
    if (length(expr) == 2) {
      # Unary minus
      return(flatten.to.terms(expr[[2]], -sign))
    }
    left_terms <- flatten.to.terms(expr[[2]], sign)
    right_terms <- flatten.to.terms(expr[[3]], -sign)
    return(c(left_terms, right_terms))
  }
  
  # Handle multiplication with numeric coefficient
  if (op == "*") {
    left <- expr[[2]]
    right <- expr[[3]]
    
    if (is.numeric(left)) {
      inner_terms <- flatten.to.terms(right, sign * left)
      return(inner_terms)
    }
    if (is.numeric(right)) {
      inner_terms <- flatten.to.terms(left, sign * right)
      return(inner_terms)
    }
    
    # Try to normalize the product (extract all factors, combine powers)
    normalized <- normalize.product(expr)
    if (!is.null(normalized$coef) && normalized$coef != 1) {
      return(list(list(coef = sign * normalized$coef, base = normalized$base)))
    }
  }
  
  # For other expressions, treat as single term with coefficient = sign
  return(list(list(coef = sign, base = expr)))
}


#' @title Extract Monomial Factors
#' @description Extracts all factors from a product expression and their powers.
#'   Returns a named list of powers keyed by variable name.
#' @param expr An expression (product of powers)
#' @return A named list: list(vars = c("mu" = 2, "sigma" = 2), other = NULL)
#' @keywords internal
extract.factors <- function(expr) {
  vars <- list()
  other <- NULL
  
  process <- function(e) {
    if (is.symbol(e)) {
      name <- as.character(e)
      vars[[name]] <<- (vars[[name]] %||% 0) + 1
    } else if (is.numeric(e)) {
      # Ignore numeric factors in base (handled by coefficient)
    } else if (is.call(e)) {
      op <- as.character(e[[1]])
      if (op == "^" && is.symbol(e[[2]]) && is.numeric(e[[3]])) {
        name <- as.character(e[[2]])
        vars[[name]] <<- (vars[[name]] %||% 0) + e[[3]]
      } else if (op == "*") {
        process(e[[2]])
        process(e[[3]])
      } else if (op == "(") {
        process(e[[2]])
      } else {
        # Non-decomposable expression
        other <<- e
      }
    } else {
      other <<- e
    }
  }
  
  process(expr)
  list(vars = vars, other = other)
}


#' @title Canonical Form Key
#' @description Creates a canonical string key for a polynomial term.
#'   Sorts variables alphabetically and represents as "var1^pow1*var2^pow2".
#' @param base A base expression
#' @return A character string key
#' @keywords internal
canonical.key <- function(base) {
  if (identical(base, 1) || identical(base, 1L)) {
    return("1")
  }
  
  factors <- extract.factors(base)
  
  if (!is.null(factors$other)) {
    # Has non-decomposable part, use deparse
    return(deparse(base, width.cutoff = 500))
  }
  
  if (length(factors$vars) == 0) {
    return("1")
  }
  
  # Sort by variable name and build canonical string
  sorted_names <- sort(names(factors$vars))
  parts <- vapply(sorted_names, function(name) {
    pow <- factors$vars[[name]]
    if (pow == 1) name else paste0(name, "^", pow)
  }, character(1))
  
  paste(parts, collapse = "*")
}


#' @title Reconstruct Expression from Factors
#' @description Reconstructs an R expression from a canonical factor representation.
#' @param vars Named list of variable powers
#' @return An R expression
#' @keywords internal
factors.to.expr <- function(vars) {
  if (length(vars) == 0) {
    return(1)
  }
  
  sorted_names <- sort(names(vars))
  exprs <- lapply(sorted_names, function(name) {
    pow <- vars[[name]]
    sym <- as.symbol(name)
    if (pow == 1) sym else call("^", sym, pow)
  })
  
  # Combine with multiplication
  result <- exprs[[1]]
  if (length(exprs) > 1) {
    for (i in 2:length(exprs)) {
      result <- call("*", result, exprs[[i]])
    }
  }
  result
}


#' @title Normalize Base Expression
#' @description Creates a canonical string representation of a base expression
#'   for grouping like terms.
#' @param base A base expression
#' @return A character string key
#' @keywords internal
normalize.base <- function(base) {
  canonical.key(base)
}


#' @title Collect Like Terms
#' @description Collects like terms in an expression by flattening to terms,
#'   grouping by base, summing coefficients, and reconstructing.
#' @param expr An expression
#' @return A simplified expression with like terms collected
#' @keywords internal
collect.terms <- function(expr) {
  # Flatten expression to list of (coef, base) pairs
  terms <- flatten.to.terms(expr, 1)
  
  # Group by base
  term_map <- list()
  base_order <- character(0)
  
  for (term in terms) {
    key <- normalize.base(term$base)
    if (is.null(term_map[[key]])) {
      term_map[[key]] <- list(coef = 0, base = term$base)
      base_order <- c(base_order, key)
    }
    term_map[[key]]$coef <- term_map[[key]]$coef + term$coef
  }
  
  # Filter out zero terms
  non_zero <- list()
  for (key in base_order) {
    if (term_map[[key]]$coef != 0) {
      non_zero <- c(non_zero, list(term_map[[key]]))
    }
  }
  
  # If all terms cancelled
  if (length(non_zero) == 0) {
    return(0)
  }
  
  # Reconstruct expression
  result <- NULL
  for (term in non_zero) {
    coef <- term$coef
    base <- term$base
    
    # Build term expression
    if (identical(base, 1) || identical(base, 1L)) {
      term_expr <- coef
    } else if (coef == 1) {
      term_expr <- base
    } else if (coef == -1) {
      term_expr <- call("-", base)
    } else {
      term_expr <- call("*", coef, base)
    }
    
    # Add to result
    if (is.null(result)) {
      if (is.call(term_expr) && as.character(term_expr[[1]]) == "-" && length(term_expr) == 2) {
        # Unary minus: start with negative
        result <- term_expr
      } else {
        result <- term_expr
      }
    } else {
      if (is.numeric(coef) && coef < 0) {
        # Negative coefficient: use subtraction
        if (identical(base, 1) || identical(base, 1L)) {
          result <- call("-", result, abs(coef))
        } else if (coef == -1) {
          result <- call("-", result, base)
        } else {
          result <- call("-", result, call("*", abs(coef), base))
        }
      } else {
        result <- call("+", result, term_expr)
      }
    }
  }
  
  return(result)
}


simplify.expr <- function(expr) {
  # Base case: numeric or symbol
  if (is.numeric(expr) || is.symbol(expr)) {
    return(expr)
  }
  
  # Not a call - return as-is
  if (!is.call(expr)) {
    return(expr)
  }
  
  op <- as.character(expr[[1]])
  
  # Handle E() calls - simplify E(constant) to constant
  if (op == "E") {
    inner <- expr[[2]]
    
    # E(numeric) = numeric
    if (is.numeric(inner)) {
      return(inner)
    }
    
    # E(constant symbol) = constant symbol
    if (is.symbol(inner) && classify.type(inner) == "const") {
      return(inner)
    }
    
    # Otherwise, simplify the inner expression and re-wrap
    simplified_inner <- simplify.expr(inner)
    
    # Check if simplified inner is now a constant
    if (is.numeric(simplified_inner)) {
      return(simplified_inner)
    }
    if (is.symbol(simplified_inner) && classify.type(simplified_inner) == "const") {
      return(simplified_inner)
    }
    
    return(call("E", simplified_inner))
  }
  
  # Handle addition
  if (op == "+") {
    if (length(expr) == 2) {
      # Unary plus
      return(simplify.expr(expr[[2]]))
    }
    
    left <- simplify.expr(expr[[2]])
    right <- simplify.expr(expr[[3]])
    
    # Lift 'when': when(A, cond) + B -> when(A+B, cond)
    if (is.call(left) && as.character(left[[1]]) == "when") {
      A <- left[[2]]
      cond <- left[[3]]
      new_op <- call(op, A, right)
      return(simplify.expr(call("when", new_op, cond)))
    }
    if (is.call(right) && as.character(right[[1]]) == "when") {
      B <- right[[2]]
      cond <- right[[3]]
      new_op <- call(op, left, B)
      return(simplify.expr(call("when", new_op, cond)))
    }
    
    # x + 0 = x
    if (identical(right, 0) || identical(right, 0L)) {
      return(left)
    }
    # 0 + x = x
    if (identical(left, 0) || identical(left, 0L)) {
      return(right)
    }
    
    # Try to combine like terms: a*X + b*X -> (a+b)*X
    combined <- try.combine.like.terms(left, right, "+")
    if (!is.null(combined)) {
      return(simplify.expr(combined))
    }
    
    return(call("+", left, right))
  }
  
  # Handle subtraction
  if (op == "-") {
    if (length(expr) == 2) {
      # Unary minus
      inner <- simplify.expr(expr[[2]])
      if (identical(inner, 0) || identical(inner, 0L)) {
        return(0)
      }
      return(call("-", inner))
    }
    
    left <- simplify.expr(expr[[2]])
    right <- simplify.expr(expr[[3]])
    
    # Lift 'when': when(A, cond) + B -> when(A+B, cond)
    if (is.call(left) && as.character(left[[1]]) == "when") {
      # when(A, cond) + B
      A <- left[[2]]
      cond <- left[[3]]
      new_op <- call(op, A, right)
      return(simplify.expr(call("when", new_op, cond)))
    }
    if (is.call(right) && as.character(right[[1]]) == "when") {
      # A + when(B, cond)
      B <- right[[2]]
      cond <- right[[3]]
      new_op <- call(op, left, B)
      return(simplify.expr(call("when", new_op, cond)))
    }
    
    # x - 0 = x
    if (identical(right, 0) || identical(right, 0L)) {
      return(left)
    }
    # 0 - x = -x
    if (identical(left, 0) || identical(left, 0L)) {
      return(call("-", right))
    }
    
    # x - x = 0
    if (identical(left, right)) {
      return(0)
    }

    # Handle (A + B) - B -> A
    if (is.call(left) && as.character(left[[1]]) == "+") {
      sum_left <- left[[2]]
      sum_right <- left[[3]]
      
      if (identical(sum_right, right)) {
        return(sum_left)
      }
      if (identical(sum_left, right)) {
        return(sum_right)
      }
    }

    # Try to combine like terms: a*X - b*X -> (a-b)*X
    combined <- try.combine.like.terms(left, right, "-")
    if (!is.null(combined)) {
      return(simplify.expr(combined))
    }
    
    return(call("-", left, right))

  }
  
  # Handle multiplication
  if (op == "*") {
    left <- simplify.expr(expr[[2]])
    right <- simplify.expr(expr[[3]])
    
    # Lift 'when'
    if (is.call(left) && as.character(left[[1]]) == "when") {
      A <- left[[2]]
      cond <- left[[3]]
      new_op <- call(op, A, right)
      return(simplify.expr(call("when", new_op, cond)))
    }
    if (is.call(right) && as.character(right[[1]]) == "when") {
      B <- right[[2]]
      cond <- right[[3]]
      new_op <- call(op, left, B)
      return(simplify.expr(call("when", new_op, cond)))
    }
    
    # x * 0 = 0
    if (identical(left, 0) || identical(left, 0L) ||
        identical(right, 0) || identical(right, 0L)) {
      return(0)
    }
    # x * 1 = x
    if (identical(right, 1) || identical(right, 1L)) {
      return(left)
    }
    # 1 * x = x
    if (identical(left, 1) || identical(left, 1L)) {
      return(right)
    }
    
    # Power combination: x * x = x^2
    if (identical(left, right)) {
      return(call("^", left, 2))
    }
    
    # Power combination: x * x^n = x^(n+1)
    if (is.symbol(left) && is.call(right) && as.character(right[[1]]) == "^") {
      base <- right[[2]]
      exp <- right[[3]]
      if (identical(left, base) && is.numeric(exp)) {
        return(call("^", base, exp + 1))
      }
    }
    
    # Power combination: x^n * x = x^(n+1)
    if (is.symbol(right) && is.call(left) && as.character(left[[1]]) == "^") {
      base <- left[[2]]
      exp <- left[[3]]
      if (identical(right, base) && is.numeric(exp)) {
        return(call("^", base, exp + 1))
      }
    }
    
    # Power combination: x^m * x^n = x^(m+n)
    if (is.call(left) && as.character(left[[1]]) == "^" &&
        is.call(right) && as.character(right[[1]]) == "^") {
      left_base <- left[[2]]
      left_exp <- left[[3]]
      right_base <- right[[2]]
      right_exp <- right[[3]]
      if (identical(left_base, right_base) && is.numeric(left_exp) && is.numeric(right_exp)) {
        return(call("^", left_base, left_exp + right_exp))
      }
    }
    
    return(call("*", left, right))
  }
  
  # Handle division
  if (op == "/") {
    left <- simplify.expr(expr[[2]])
    right <- simplify.expr(expr[[3]])
    
    # Lift 'when'
    if (is.call(left) && as.character(left[[1]]) == "when") {
      A <- left[[2]]
      cond <- left[[3]]
      new_op <- call(op, A, right)
      return(simplify.expr(call("when", new_op, cond)))
    }
    if (is.call(right) && as.character(right[[1]]) == "when") {
      B <- right[[2]]
      cond <- right[[3]]
      new_op <- call(op, left, B)
      return(simplify.expr(call("when", new_op, cond)))
    }
    
    # 0 / x = 0 (assuming x != 0)
    if (identical(left, 0) || identical(left, 0L)) {
      return(0)
    }
    # x / 1 = x
    if (identical(right, 1) || identical(right, 1L)) {
      return(left)
    }
    
    # x / x = 1
    if (identical(left, right)) {
      return(1)
    }
    
    # (c * x) / x = c where c is numeric
    # This must come BEFORE the canonical comparison since canonical.key ignores numeric coefficients
    if (is.call(left) && as.character(left[[1]]) == "*") {
      left_l <- left[[2]]
      left_r <- left[[3]]
      if (is.numeric(left_l) && identical(canonical.key(left_r), canonical.key(right))) {
        return(left_l)
      }
      if (is.numeric(left_r) && identical(canonical.key(left_l), canonical.key(right))) {
        return(left_r)
      }
    }
    
    # x^n / x^n = 1 (using canonical comparison)
    if (is.call(left) && is.call(right)) {
      if (identical(canonical.key(left), canonical.key(right))) {
        return(1)
      }
    }
    
    return(call("/", left, right))
  }
  
  # Handle exponentiation
  if (op == "^") {
    base <- simplify.expr(expr[[2]])
    exp <- simplify.expr(expr[[3]])
    
    # Lift 'when': when(base, cond)^exp -> when(base^exp, cond)
    # Note: exp is unlikely to be conditional, but we check base
    if (is.call(base) && as.character(base[[1]]) == "when") {
      b_inner <- base[[2]]
      cond <- base[[3]]
      new_op <- call(op, b_inner, exp)
      return(simplify.expr(call("when", new_op, cond)))
    }
    
    # x^0 = 1
    if (identical(exp, 0) || identical(exp, 0L)) {
      return(1)
    }
    # x^1 = x
    if (identical(exp, 1) || identical(exp, 1L)) {
      return(base)
    }
    # 0^x = 0 (for x > 0)
    if (identical(base, 0) || identical(base, 0L)) {
      return(0)
    }
    # 1^x = 1
    if (identical(base, 1) || identical(base, 1L)) {
      return(1)
    }
    
    # (x^m)^n = x^(m*n)
    # First unwrap parentheses if present
    inner_base_candidate <- base
    if (is.call(base) && as.character(base[[1]]) == "(") {
      inner_base_candidate <- base[[2]]
    }
    if (is.call(inner_base_candidate) && as.character(inner_base_candidate[[1]]) == "^") {
      inner_base <- inner_base_candidate[[2]]
      inner_exp <- inner_base_candidate[[3]]
      if (is.numeric(inner_exp) && is.numeric(exp)) {
        return(call("^", inner_base, inner_exp * exp))
      }
    }
    
    return(call("^", base, exp))
  }
  
  # Handle parentheses
  if (op == "(") {
    inner <- simplify.expr(expr[[2]])
    # If inner is simple, drop parens
    if (is.numeric(inner) || is.symbol(inner)) {
      return(inner)
    }
    return(call("(", inner))
  }
  
  # Default: recurse on all arguments
  args <- lapply(as.list(expr)[-1], simplify.expr)
  return(as.call(c(expr[[1]], args)))
}

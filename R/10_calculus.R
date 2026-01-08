# =============================================================================
# Symbolic Calculus Module
# Component A: Symbolic Differentiation (wrapper around stats::D)
# =============================================================================


# -----------------------------------------------------------------------------
# HELPER FUNCTIONS
# -----------------------------------------------------------------------------

#' @title Check if Expression Contains Variable
#' @description Recursively checks if an expression tree contains a specific
#'   variable symbol.
#' @param expr An unevaluated R expression
#' @param var A symbol to search for
#' @return Logical: TRUE if var appears in expr, FALSE otherwise
#' @keywords internal
contains.var <- function(expr, var) {
  if (missing(expr) || is.null(expr)) return(FALSE)
  
  # Direct symbol match
  if (is.name(expr) || is.symbol(expr)) {
    return(identical(expr, var))
  }
  
  # Numeric literals never contain variables
  if (is.atomic(expr)) return(FALSE)
  
  # Recursive check for calls
  if (is.call(expr)) {
    # Check all arguments (skip the function name at position 1)
    for (i in seq_along(expr)[-1]) {
      if (contains.var(expr[[i]], var)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  return(FALSE)
}


#' @title Protect E() Calls Before Differentiation
#' @description Replaces E(X) calls with temporary constant symbols so that
#'   stats::D treats them as constants. E() calls are replaced with symbols
#'   like __E_X__ which stats::D will ignore.
#' @param expr An unevaluated R expression
#' @return A list with 'expr' (modified expression) and 'map' (substitution map)
#' @keywords internal
protect.E <- function(expr) {
  map <- list()
  
  protect_recursive <- function(e) {
    if (is.numeric(e) || is.logical(e)) {
      return(e)
    }
    
    if (is.symbol(e)) {
      return(e)
    }
    
    if (is.call(e)) {
      op <- as.character(e[[1]])
      
      # Found an E() call - replace with temp symbol
      if (op == "E") {
        inner <- e[[2]]
        # Create a unique temp name based on the inner content
        temp_name <- paste0("__E_", gsub("[^a-zA-Z0-9]", "_", deparse(inner)), "__")
        temp_sym <- as.symbol(temp_name)
        map[[temp_name]] <<- e  # Store mapping
        return(temp_sym)
      }
      
      # Recurse on all arguments
      new_args <- lapply(as.list(e)[-1], protect_recursive)
      return(as.call(c(e[[1]], new_args)))
    }
    
    return(e)
  }
  
  protected_expr <- protect_recursive(expr)
  list(expr = protected_expr, map = map)
}


#' @title Restore E() Calls After Differentiation
#' @description Replaces temporary symbols back to their original E() calls.
#' @param expr An expression with __E_...__ symbols
#' @param map The substitution map from protect.E
#' @return The expression with E() calls restored
#' @keywords internal
restore.E <- function(expr, map) {
  if (length(map) == 0) {
    return(expr)
  }
  
  restore_recursive <- function(e) {
    if (is.numeric(e) || is.logical(e)) {
      return(e)
    }
    
    if (is.symbol(e)) {
      name <- as.character(e)
      if (name %in% names(map)) {
        return(map[[name]])
      }
      return(e)
    }
    
    if (is.call(e)) {
      new_args <- lapply(as.list(e)[-1], restore_recursive)
      return(as.call(c(e[[1]], new_args)))
    }
    
    return(e)
  }
  
  restore_recursive(expr)
}


# -----------------------------------------------------------------------------
# COMPONENT A: SYMBOLIC DIFFERENTIATION
# -----------------------------------------------------------------------------

#' @title Symbolic Differentiation
#' @description Computes the symbolic partial derivative of an expression with
#'   respect to a specified variable. Wraps \code{stats::D} with pre-processing
#'   to handle E() calls and post-processing to simplify the result.
#' @param expr An R expression (uses non-standard evaluation, no quoting needed)
#' @param var The variable symbol to differentiate by (no quoting needed)
#' @return A simplified R expression representing the derivative
#' @export deriv.sym
#' @examples
#' deriv.sym(x^3, x)             # 3 * x^2
#' deriv.sym(exp(a * x), x)      # a * exp(a * x)
#' deriv.sym(x * E(Y), x)        # E(Y) (treats E(Y) as constant)
deriv.sym <- function(expr, var) {
  # Capture inputs using NSE
  expr <- substitute(expr)
  expr <- ensure.expression(expr)
  
  var <- substitute(var)
  # Input sanitization: ensure var is a character string for stats::D
  if (is.symbol(var) || is.name(var)) {
    var_name <- as.character(var)
  } else if (is.character(var)) {
    var_name <- var
  } else {
    stop("'var' must be a symbol or character string")
  }
  
  # Convert var to symbol for contains.var check
  var_sym <- as.symbol(var_name)
  
  # Protect E() calls by replacing them with temp constants
  protection <- protect.E(expr)
  protected_expr <- protection$expr
  protection_map <- protection$map
  
  # Call stats::D for the actual differentiation
  raw_deriv <- tryCatch(
    stats::D(protected_expr, var_name),
    error = function(e) {
      # If D fails, return 0 if expr doesn't contain var, else error
      if (!contains.var(expr, var_sym)) {
        return(0)
      }
      stop("Differentiation failed: ", e$message)
    }
  )
  
  # Restore E() calls
  restored <- restore.E(raw_deriv, protection_map)
  
  # Simplify the result
  simplified <- simplify.expr(restored)
  
  return(simplified)
}

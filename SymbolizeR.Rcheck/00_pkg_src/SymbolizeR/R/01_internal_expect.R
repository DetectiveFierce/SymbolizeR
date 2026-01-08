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
expect.recursive <- function(expr) {
  # Base case: numeric literal
  if (is.numeric(expr)) {
    return(expr)
  }
  
  # Base case: symbol (variable name)
  if (is.symbol(expr)) {
    type <- classify.type(expr)
    
    if (type == "const") {
      # Constants pass through unchanged
      return(expr)
    }
    
    if (type == "rv") {
      # Check if variable has known distribution
      var_name <- as.character(expr)
      first_moment <- get.first.moment(var_name)
      if (!is.null(first_moment)) {
        return(first_moment)
      }
      # Fallback: wrap in E()
      return(call("E", expr))
    }
  }
  
  # Recursive case: it's a call (expression with operator)
  if (is.call(expr)) {
    op <- as.character(expr[[1]])
    
    # Handle addition and subtraction (linearity)
    if (op %in% c("+", "-")) {
      # Unary minus case
      if (length(expr) == 2) {
        return(call(op, expect.recursive(expr[[2]])))
      }
      
      # Binary case: E(X + Y) = E(X) + E(Y)
      left <- expect.recursive(expr[[2]])
      right <- expect.recursive(expr[[3]])
      return(call(op, left, right))
    }
    
    # Handle exp() - check for MGF patterns E[e^(t*X)]
    if (op == "exp") {
      inner <- expr[[2]]
      
      # Check if inner is t*X or X*t pattern (const * RV)
      if (is.call(inner) && as.character(inner[[1]]) == "*") {
        left_inner <- inner[[2]]
        right_inner <- inner[[3]]
        
        left_type <- classify.type(left_inner)
        right_type <- classify.type(right_inner)
        
        # Case: const * RV -> check for MGF
        if (left_type %in% c("const", "number") && right_type == "rv") {
          var_name <- as.character(right_inner)
          mgf <- get.mgf(var_name, left_inner)
          if (!is.null(mgf)) {
            return(mgf)
          }
        }
        
        # Case: RV * const -> check for MGF
        if (left_type == "rv" && right_type %in% c("const", "number")) {
          var_name <- as.character(left_inner)
          mgf <- get.mgf(var_name, right_inner)
          if (!is.null(mgf)) {
            return(mgf)
          }
        }
      }
      
      # Check if inner is just an RV (e^X with t=1 implicit)
      if (is.symbol(inner) && classify.type(inner) == "rv") {
        var_name <- as.character(inner)
        mgf <- get.mgf(var_name, 1)
        if (!is.null(mgf)) {
          return(mgf)
        }
      }
      
      # Default: wrap exp() in E()
      return(call("E", expr))
    }
    
    # Handle multiplication
    if (op == "*") {
      left_expr <- expr[[2]]
      right_expr <- expr[[3]]
      
      left_type <- classify.type(left_expr)
      right_type <- classify.type(right_expr)
      
      # Case A: Const * Const -> return as-is
      if (left_type %in% c("const", "number") && right_type %in% c("const", "number")) {
        return(expr)
      }
      
      # Case B: Const * RV -> pull constant out: a * E(X) (or a * mu if known)
      if (left_type %in% c("const", "number") && right_type == "rv") {
        return(call("*", left_expr, expect.recursive(right_expr)))
      }
      
      # Case B (reversed): RV * Const -> pull constant out: E(X) * a (or mu * a if known)
      if (left_type == "rv" && right_type %in% c("const", "number")) {
        return(call("*", expect.recursive(left_expr), right_expr))
      }
      
      # Case C: RV * RV -> check independence, otherwise wrap entire product
      if (left_type == "rv" && right_type == "rv") {
        left_name <- as.character(left_expr)
        right_name <- as.character(right_expr)
        
        # If independent: E(X * Y) = E(X) * E(Y)
        if (are.independent(left_name, right_name)) {
          return(call("*", expect.recursive(left_expr), expect.recursive(right_expr)))
        }
        
        # Not independent: cannot simplify
        return(call("E", expr))
      }
      
      # Handle nested expressions (calls)
      if (left_type == "call" || right_type == "call") {
        # Recurse into nested expressions
        left_result <- expect.recursive(left_expr)
        right_result <- expect.recursive(right_expr)
        
        # Check if results are constants or contain E()
        left_is_const <- is.numeric(left_result) || 
                         (is.symbol(left_result) && classify.type(left_result) == "const")
        right_is_const <- is.numeric(right_result) || 
                          (is.symbol(right_result) && classify.type(right_result) == "const")
        
        if (left_is_const) {
          return(call("*", left_result, right_result))
        }
        if (right_is_const) {
          return(call("*", left_result, right_result))
        }
        
        # Both contain random variables - wrap entire thing
        return(call("E", call("*", left_expr, right_expr)))
      }
    }
    
    # Handle division
    if (op == "/") {
      left_expr <- expr[[2]]
      right_expr <- expr[[3]]
      
      left_type <- classify.type(left_expr)
      right_type <- classify.type(right_expr)
      
      # Const / Const -> return as-is
      if (left_type %in% c("const", "number") && right_type %in% c("const", "number")) {
        return(expr)
      }
      
      # Anything else with division - cannot simplify, wrap it
      return(call("E", expr))
    }
    
    # Handle exponentiation
    if (op == "^") {
      base_expr <- expr[[2]]
      exp_expr <- expr[[3]]
      
      base_type <- classify.type(base_expr)
      exp_type <- classify.type(exp_expr)
      
      # Const ^ Const -> return as-is
      if (base_type %in% c("const", "number") && exp_type %in% c("const", "number")) {
        return(expr)
      }
      
      # Check for E[X^2] with known distribution
      if (base_type == "rv" && is.numeric(exp_expr) && exp_expr == 2) {
        var_name <- as.character(base_expr)
        second_moment <- get.second.moment(var_name)
        if (!is.null(second_moment)) {
          return(second_moment)
        }
      }
      
      # RV ^ something or something ^ RV -> wrap in E()
      return(call("E", expr))
    }
    
    # Handle parentheses (just recurse)
    if (op == "(") {
      return(call("(", expect.recursive(expr[[2]])))
    }
    
    # Default: wrap unknown operations in E()
    return(call("E", expr))
  }
  
  # Fallback: return as-is
  return(expr)
}

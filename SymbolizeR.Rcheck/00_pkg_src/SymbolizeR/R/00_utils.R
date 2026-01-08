#' @title Classify Expression Type
#' @description Determines if a symbol is a number, random variable, or constant
#'   based on naming conventions. Uppercase = RV, lowercase = constant.
#' @param sym A symbol or expression to classify
#' @return Character: "number", "rv", or "const"
#' @keywords internal
classify.type <- function(sym) {

  # Numeric literals

if (is.numeric(sym)) {
    return("number")
  }
  
  # Symbols (variable names)
  if (is.symbol(sym)) {
    name <- as.character(sym)
    first_char <- substr(name, 1, 1)
    
    # Uppercase first letter = Random Variable
    if (grepl("^[A-Z]", first_char)) {
      return("rv")
    }
    
    # Lowercase first letter = Constant
    return("const")
  }
  

  # Calls (expressions like a*X) - classify based on structure
  if (is.call(sym)) {
    return("call")
  }
  
  # Default fallback
  return("unknown")
}


#' @title Ensure Expression
#' @description Normalizes input to an unevaluated expression, handling both
#'   raw calls and variables holding calls.
#' @param expr An expression or variable containing an expression
#' @return An unevaluated call object
#' @keywords internal
ensure.expression <- function(expr) {
  # If it's already a call or symbol, return as-is
  if (is.call(expr) || is.symbol(expr) || is.numeric(expr)) {
    return(expr)
  }
  
  # If it's a name bound to an expression, evaluate to get the expression
  if (is.name(expr)) {
    return(eval(expr, parent.frame()))
  }
  
  return(expr)
}


#' @title Combine Powers
#' @description Combines power expressions: x*x^n = x^(n+1), x^m*x^n = x^(m+n)
#' @param left Left expression
#' @param right Right expression
#' @return Combined power expression, or NULL if not combinable
#' @keywords internal
combine.powers <- function(left, right) {
  # x * x = x^2
  if (is.symbol(left) && is.symbol(right) && identical(left, right)) {
    return(call("^", left, 2))
  }
  
  # x * x^n = x^(n+1)
  if (is.symbol(left) && is.call(right) && as.character(right[[1]]) == "^") {
    base <- right[[2]]
    exp <- right[[3]]
    if (identical(left, base) && is.numeric(exp)) {
      return(call("^", base, exp + 1))
    }
  }
  
  # x^n * x = x^(n+1)
  if (is.symbol(right) && is.call(left) && as.character(left[[1]]) == "^") {
    base <- left[[2]]
    exp <- left[[3]]
    if (identical(right, base) && is.numeric(exp)) {
      return(call("^", base, exp + 1))
    }
  }
  
  # x^m * x^n = x^(m+n)
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
  
  NULL
}


#' @title Expand Polynomial
#' @description Expands polynomial expressions with integer powers, particularly
#'   (A + B)^n patterns. This enables Var() and Cov() to handle linear
#'   combinations properly.
#' @param expr An unevaluated R expression
#' @return An expanded expression with powers distributed
#' @details
#' Currently handles:
#' \itemize{
#'   \item \code{(A + B)^2} -> \code{A^2 + 2*A*B + B^2}
#'   \item \code{(A - B)^2} -> \code{A^2 - 2*A*B + B^2}
#'   \item \code{(A * B)^n} -> \code{A^n * B^n}
#' }
#' @keywords internal
expand.poly <- function(expr) {
  # Base cases
  if (is.numeric(expr) || is.symbol(expr)) {
    return(expr)
  }
  
  if (!is.call(expr)) {
    return(expr)
  }
  
  op <- as.character(expr[[1]])
  
  # Handle exponentiation
  if (op == "^") {
    base <- expr[[2]]
    exp <- expr[[3]]
    
    # Only expand for integer powers >= 2
    if (!is.numeric(exp) || exp < 2 || exp != as.integer(exp)) {
      # Recurse on base
      return(call("^", expand.poly(base), exp))
    }
    
    exp <- as.numeric(exp)
    
    # Check if base is (A + B) or (A - B)
    if (is.call(base)) {
      base_op <- as.character(base[[1]])
      
      # Handle (A + B)^2 or (A - B)^2
      if (base_op %in% c("+", "-") && length(base) == 3 && exp == 2) {
        A <- expand.poly(base[[2]])
        B <- expand.poly(base[[3]])
        
        # (A + B)^2 = A^2 + 2*A*B + B^2
        # (A - B)^2 = A^2 - 2*A*B + B^2
        A_sq <- call("^", A, 2)
        B_sq <- call("^", B, 2)
        two_AB <- call("*", 2, call("*", A, B))
        
        if (base_op == "+") {
          # A^2 + 2*A*B + B^2
          return(call("+", call("+", A_sq, two_AB), B_sq))
        } else {
          # A^2 - 2*A*B + B^2
          return(call("+", call("-", A_sq, two_AB), B_sq))
        }
      }
      
      # Handle (A * B)^n = A^n * B^n
      if (base_op == "*" && length(base) == 3) {
        A <- expand.poly(base[[2]])
        B <- expand.poly(base[[3]])
        return(call("*", call("^", A, exp), call("^", B, exp)))
      }
      
      # Handle parentheses
      if (base_op == "(") {
        inner <- base[[2]]
        return(expand.poly(call("^", inner, exp)))
      }
    }
    
    return(call("^", expand.poly(base), exp))
  }
  
  # Handle multiplication: expand both sides and distribute
  if (op == "*") {
    left <- expand.poly(expr[[2]])
    right <- expand.poly(expr[[3]])
    
    # Unwrap parentheses for distribution check
    left_inner <- left
    if (is.call(left) && as.character(left[[1]]) == "(") {
      left_inner <- left[[2]]
    }
    right_inner <- right
    if (is.call(right) && as.character(right[[1]]) == "(") {
      right_inner <- right[[2]]
    }
    
    # Distribute: (A + B) * C = A*C + B*C
    if (is.call(left_inner) && as.character(left_inner[[1]]) == "+") {
      A <- left_inner[[2]]
      B <- left_inner[[3]]
      return(call("+", 
                  expand.poly(call("*", A, right)),
                  expand.poly(call("*", B, right))))
    }
    
    # Distribute: C * (A + B) = C*A + C*B
    if (is.call(right_inner) && as.character(right_inner[[1]]) == "+") {
      A <- right_inner[[2]]
      B <- right_inner[[3]]
      return(call("+",
                  expand.poly(call("*", left, A)),
                  expand.poly(call("*", left, B))))
    }
    
    # Combine powers: x * x^n = x^(n+1), x^m * x^n = x^(m+n)
    # This is inline since simplify.expr isn't available here
    combined <- combine.powers(left, right)
    if (!is.null(combined)) {
      return(combined)
    }
    
    return(call("*", left, right))
  }
  
  # Handle addition/subtraction: recurse on both sides
  if (op %in% c("+", "-")) {
    if (length(expr) == 2) {
      # Unary
      return(call(op, expand.poly(expr[[2]])))
    }
    left <- expand.poly(expr[[2]])
    right <- expand.poly(expr[[3]])
    return(call(op, left, right))
  }
  
  # Handle parentheses
  if (op == "(") {
    return(call("(", expand.poly(expr[[2]])))
  }
  
  # Default: return as-is
  return(expr)
}


#' @title Tag Conditional Expression
#' @description Checks if an expression is a 'when' call and tags it with 'conditional_moment' class.
#' @param expr An expression
#' @return The expression, possibly with an added class
#' @keywords internal
tag.conditional <- function(expr) {
  if (is.call(expr) && as.character(expr[[1]]) == "when") {
    class(expr) <- c("conditional_moment", class(expr))
  }
  return(expr)
}

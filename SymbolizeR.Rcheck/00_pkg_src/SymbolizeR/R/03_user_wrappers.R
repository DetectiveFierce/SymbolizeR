#' @title Symbolic Expectation
#' @description Computes the symbolic expectation of an expression using
#'   the linearity of expectation. Uses non-standard evaluation.
#' @param expr An R expression involving random variables and constants.
#'   Random variables are identified by uppercase first letter.
#' @return An unevaluated expression representing the expectation
#' @export
#' @examples
#' E(X)           # Returns: E(X)
#' E(a * X)       # Returns: a * E(X)
#' E(X + Y)       # Returns: E(X) + E(Y)
#' E(2 * X + 3)   # Returns: 2 * E(X) + 3
E <- function(expr) {
  # Capture input using NSE
  ex <- substitute(expr)
  
  # Normalize the expression
  ex <- ensure.expression(ex)
  
  # Run the recursive expectation engine
  result <- expect.recursive(ex)
  
  # Simplify the result
  clean_result <- simplify.expr(result)
  
  return(tag.conditional(clean_result))
}


#' @title Symbolic Variance
#' @description Computes the symbolic variance using the identity
#'   Var(X) = E(X^2) - E(X)^2
#' @param expr An R expression involving random variables and constants
#' @return An unevaluated expression representing the variance
#' @export
#' @examples
#' Var(X)         # Returns: E(X^2) - E(X)^2
#' Var(a * X)     # Returns: a^2 * (E(X^2) - E(X)^2)
Var <- function(expr) {
  # Capture input using NSE
  ex <- substitute(expr)
  
  # Normalize the expression
  ex <- ensure.expression(ex)
  
  # Var(X) = E(X^2) - E(X)^2
  # Build X^2
  x_squared <- call("^", ex, 2)
  
  # Expand the polynomial (a*X + b)^2 -> a^2*X^2 + 2*a*b*X + b^2
  x_squared_expanded <- expand.poly(x_squared)
  
  # E(X^2) - apply expectation to expanded form
  e_x_squared <- expect.recursive(x_squared_expanded)
  
  # E(X)
  e_x <- expect.recursive(ex)
  
  # E(X)^2
  e_x_sq <- call("^", e_x, 2)
  
  # E(X^2) - E(X)^2
  result <- call("-", e_x_squared, e_x_sq)
  
  # Simplify multiple times (expansion creates many redundant terms)
  clean_result <- simplify.expr(simplify.expr(simplify.expr(result)))
  
  return(tag.conditional(clean_result))
}


#' @title Symbolic Covariance
#' @description Computes the symbolic covariance using the identity
#'   Cov(X, Y) = E(XY) - E(X)E(Y)
#' @param x First random variable expression
#' @param y Second random variable expression
#' @return An unevaluated expression representing the covariance
#' @export
#' @examples
#' Cov(X, Y)      # Returns: E(X * Y) - E(X) * E(Y)
Cov <- function(x, y) {
  # Capture inputs using NSE
  x_ex <- substitute(x)
  y_ex <- substitute(y)
  
  # Normalize expressions
  x_ex <- ensure.expression(x_ex)
  y_ex <- ensure.expression(y_ex)
  
  # Cov(X, Y) = E(XY) - E(X)E(Y)
  # Build X * Y
  xy <- call("*", x_ex, y_ex)
  
  # Expand the polynomial product
  xy_expanded <- expand.poly(xy)
  
  # E(XY) - apply expectation to expanded form
  e_xy <- expect.recursive(xy_expanded)
  
  # E(X)
  e_x <- expect.recursive(x_ex)
  
  # E(Y)
  e_y <- expect.recursive(y_ex)
  
  # E(X) * E(Y)
  e_x_times_e_y <- call("*", e_x, e_y)
  
  # E(XY) - E(X)E(Y)
  result <- call("-", e_xy, e_x_times_e_y)
  
  # Simplify multiple times
  clean_result <- simplify.expr(simplify.expr(simplify.expr(result)))
  
  return(tag.conditional(clean_result))
}

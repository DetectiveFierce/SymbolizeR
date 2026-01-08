# ============================================================================
# HIGHER-ORDER STATISTICS
# ============================================================================

#' @title Symbolic Skewness
#' @description Computes the symbolic skewness using the third standardized moment.
#'   Skewness = E((X - mu)^3) / sigma^3 = (E(X^3) - 3*mu*sigma^2 - mu^3) / sigma^3
#' @param expr An R expression involving a random variable
#' @return An unevaluated expression representing the skewness formula
#' @export
#' @examples
#' Skewness(X)  # Returns symbolic skewness formula
#'
#' # With defined distribution
#' define(X ~ Normal(mu, sigma))
#' Skewness(X)  # Returns 0 (normal is symmetric)
Skewness <- function(expr) {
  # Capture input using NSE
  ex <- substitute(expr)
  ex <- ensure.expression(ex)
  
  # Check if this is a simple defined variable
  if (is.symbol(ex)) {
    var_name <- as.character(ex)
    m1 <- get.nth.moment(var_name, 1)
    m2 <- get.nth.moment(var_name, 2)
    m3 <- get.nth.moment(var_name, 3)
    
    if (!is.null(m1) && !is.null(m2) && !is.null(m3)) {
      # Use closed-form moments
      # Var = E[X²] - E[X]²
      var_expr <- simplify.expr(call("-", m2, call("^", m1, 2)))
      
      # E[(X-μ)³] = E[X³] - 3*μ*σ² - μ³
      mu <- m1
      third_central <- simplify.expr(
        call("-", 
             call("-", m3, call("*", 3, call("*", mu, var_expr))),
             call("^", mu, 3)))
      
      # σ³ = (σ²)^(3/2)
      sigma_cubed <- call("^", var_expr, call("/", 3, 2))
      
      result <- call("/", third_central, sigma_cubed)
      clean_result <- simplify.expr(simplify.expr(simplify.expr(result)))
      
      return(tag.conditional(clean_result))
    }
  }
  
  # Fallback: use expect.recursive for undefined variables
  e_x <- expect.recursive(ex)
  
  x_squared <- call("^", ex, 2)
  e_x2 <- expect.recursive(expand.poly(x_squared))
  
  x_cubed <- call("^", ex, 3)
  e_x3 <- expect.recursive(expand.poly(x_cubed))
  
  # σ² = E[X²] - E[X]²
  var_expr <- call("-", e_x2, call("^", e_x, 2))
  
  # σ³ = (σ²)^(3/2)
  sigma_cubed <- call("^", var_expr, call("/", 3, 2))
  
  # E[(X-μ)³] = E[X³] - 3*μ*σ² - μ³
  mu <- e_x
  third_central <- call("-", 
                        call("-", e_x3, call("*", 3, call("*", mu, var_expr))),
                        call("^", mu, 3))
  
  # Skewness = third_central / σ³
  result <- call("/", third_central, sigma_cubed)
  
  clean_result <- simplify.expr(simplify.expr(simplify.expr(result)))
  return(tag.conditional(clean_result))
}



#' @title Symbolic Kurtosis
#' @description Computes the symbolic excess kurtosis using the fourth standardized moment.
#'   Excess Kurtosis = E((X - mu)^4) / sigma^4 - 3
#' @param expr An R expression involving a random variable
#' @param excess Logical; if TRUE (default), returns excess kurtosis (subtract 3)
#' @return An unevaluated expression representing the kurtosis formula
#' @export
#' @examples
#' Kurtosis(X)  # Returns symbolic excess kurtosis formula
#'
#' # With defined distribution
#' define(X ~ Normal(mu, sigma))
#' Kurtosis(X)  # Returns 0 (normal has excess kurtosis 0)
Kurtosis <- function(expr, excess = TRUE) {
  # Capture input using NSE
  ex <- substitute(expr)
  ex <- ensure.expression(ex)
  
  # Check if this is a simple defined variable
  if (is.symbol(ex)) {
    var_name <- as.character(ex)
    m1 <- get.nth.moment(var_name, 1)
    m2 <- get.nth.moment(var_name, 2)
    m3 <- get.nth.moment(var_name, 3)
    m4 <- get.nth.moment(var_name, 4)
    
    if (!is.null(m1) && !is.null(m2) && !is.null(m3) && !is.null(m4)) {
      # Use closed-form moments
      # Var = E[X²] - E[X]²
      var_expr <- simplify.expr(call("-", m2, call("^", m1, 2)))
      
      # σ⁴ = (σ²)²
      sigma_fourth <- call("^", var_expr, 2)
      
      # E[(X-μ)⁴] = E[X⁴] - 4μE[X³] + 6μ²E[X²] - 3μ⁴
      # First expand all the products, then collect terms
      mu <- m1
      
      # Build the expression piece by piece and expand
      term1 <- expand.poly(m4)
      term2 <- expand.poly(call("*", -4, call("*", mu, m3)))
      term3 <- expand.poly(call("*", 6, call("*", call("^", mu, 2), m2)))
      term4 <- expand.poly(call("*", -3, call("^", mu, 4)))
      
      # Combine all terms
      fourth_central_raw <- call("+", call("+", call("+", term1, term2), term3), term4)
      fourth_central <- collect.terms(fourth_central_raw)
      
      # Kurtosis = fourth_central / σ⁴
      kurtosis <- call("/", fourth_central, sigma_fourth)
      
      if (excess) {
        result <- call("-", kurtosis, 3)
      } else {
        result <- kurtosis
      }
      
      clean_result <- simplify.expr(simplify.expr(simplify.expr(result)))
      return(tag.conditional(clean_result))
    }
  }
  
  # Fallback: use expect.recursive for undefined variables
  e_x <- expect.recursive(ex)
  
  x_squared <- call("^", ex, 2)
  e_x2 <- expect.recursive(expand.poly(x_squared))
  
  x_cubed <- call("^", ex, 3)
  e_x3 <- expect.recursive(expand.poly(x_cubed))
  
  x_fourth <- call("^", ex, 4)
  e_x4 <- expect.recursive(expand.poly(x_fourth))
  
  # σ² = E[X²] - E[X]²
  var_expr <- call("-", e_x2, call("^", e_x, 2))
  
  # σ⁴ = (σ²)²
  sigma_fourth <- call("^", var_expr, 2)
  
  # E[(X-μ)⁴] = E[X⁴] - 4μE[X³] + 6μ²E[X²] - 3μ⁴
  mu <- e_x
  fourth_central <- call("-",
                         call("+",
                              call("-", e_x4, call("*", 4, call("*", mu, e_x3))),
                              call("*", 6, call("*", call("^", mu, 2), e_x2))),
                         call("*", 3, call("^", mu, 4)))
  
  # Kurtosis = fourth_central / σ⁴
  kurtosis <- call("/", fourth_central, sigma_fourth)
  
  if (excess) {
    # Excess kurtosis = kurtosis - 3
    result <- call("-", kurtosis, 3)
  } else {
    result <- kurtosis
  }
  
  clean_result <- simplify.expr(simplify.expr(simplify.expr(result)))
  return(tag.conditional(clean_result))
}

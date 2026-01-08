# ============================================================================
# MOMENT FUNCTIONS (1st and 2nd Moments, MGF)
# ============================================================================

#' @title Get First Moment for Known Distribution
#' @description Returns the symbolic first moment E(X) if the variable has
#'   a registered distribution.
#' @param var_name Character name of the variable
#' @return The moment expression, or NULL if not defined
#' @keywords internal
get.first.moment <- function(var_name) {
  if (!exists(var_name, envir = pkg.env)) {
    return(NULL)
  }
  
  dist_info <- get(var_name, envir = pkg.env)
  p <- dist_info$params
  
  switch(dist_info$distribution,
    # E[X] = mu
    "Normal" = p$mu,
    
    # E[X] = (a + b) / 2
    "Uniform" = call("/", call("+", p$a, p$b), 2),
    
    # E[X] = 1 / lambda
    "Exponential" = call("/", 1, p$lambda),
    
    # E[X] = alpha / beta
    "Gamma" = call("/", p$alpha, p$beta),
    
    # E[X] = beta / (alpha - 1)  [requires alpha > 1]
    "InvGamma" = call("/", p$beta, call("-", p$alpha, 1)),
    
    # E[X] = alpha / (alpha + beta)
    "Beta" = call("/", p$alpha, call("+", p$alpha, p$beta)),
    
    # E[X] = exp(mu + sigma^2/2)
    "LogNormal" = call("exp", call("+", p$mu, call("/", call("^", p$sigma, 2), 2))),
    
    # E[X] = lambda * Gamma(1 + 1/k)
    "Weibull" = call("*", p$lambda, call("gamma", call("+", 1, call("/", 1, p$k)))),
    
    # E[X] = n * p
    "Binomial" = call("*", p$n, p$p),
    
    # E[X] = lambda
    "Poisson" = p$lambda,
    
    # E[X] = 1 / p
    "Geometric" = call("/", 1, p$p),
    
    # E[X] = r * (1 - p) / p
    "NegBinomial" = call("/", call("*", p$r, call("-", 1, p$p)), p$p),
    
    # E[X] = df for Chi-squared
    "ChiSq" = p$df,
    
    # E[X] = 0 for t-distribution (df > 1)
    "StudentT" = 0,
    
    NULL
  )
}


#' @title Get Second Moment for Known Distribution  
#' @description Returns the symbolic second moment E(X^2) if the variable has
#'   a registered distribution.
#' @param var_name Character name of the variable
#' @return The moment expression, or NULL if not defined
#' @keywords internal
get.second.moment <- function(var_name) {
  if (!exists(var_name, envir = pkg.env)) {
    return(NULL)
  }
  
  dist_info <- get(var_name, envir = pkg.env)
  p <- dist_info$params
  
  switch(dist_info$distribution,
    # E[X^2] = sigma^2 + mu^2
    "Normal" = call("+", call("^", p$sigma, 2), call("^", p$mu, 2)),
    
    # E[X^2] = (a^2 + a*b + b^2) / 3
    "Uniform" = call("/", 
                     call("+", call("+", call("^", p$a, 2), call("*", p$a, p$b)), call("^", p$b, 2)),
                     3),
    
    # E[X^2] = 2 / lambda^2
    "Exponential" = call("/", 2, call("^", p$lambda, 2)),
    
    # E[X^2] = alpha * (alpha + 1) / beta^2
    "Gamma" = call("/", call("*", p$alpha, call("+", p$alpha, 1)), call("^", p$beta, 2)),
    
    # E[X^2] = beta^2 / ((alpha - 1)^2 * (alpha - 2))  [requires alpha > 2]
    "InvGamma" = call("/", 
                      call("^", p$beta, 2),
                      call("*", call("^", call("-", p$alpha, 1), 2), call("-", p$alpha, 2))),
    
    # E[X^2] = alpha * (alpha + 1) / ((alpha + beta) * (alpha + beta + 1))
    "Beta" = call("/", 
                  call("*", p$alpha, call("+", p$alpha, 1)),
                  call("*", call("+", p$alpha, p$beta), call("+", call("+", p$alpha, p$beta), 1))),
    
    # E[X^2] = exp(2*mu + 2*sigma^2)
    "LogNormal" = call("exp", call("+", call("*", 2, p$mu), call("*", 2, call("^", p$sigma, 2)))),
    
    # E[X^2] = lambda^2 * Gamma(1 + 2/k)
    "Weibull" = call("*", call("^", p$lambda, 2), call("gamma", call("+", 1, call("/", 2, p$k)))),
    
    # E[X^2] = n*p*(1-p) + (n*p)^2 = n*p*(1-p) + n^2*p^2
    "Binomial" = call("+", 
                      call("*", call("*", p$n, p$p), call("-", 1, p$p)),
                      call("^", call("*", p$n, p$p), 2)),
    
    # E[X^2] = lambda + lambda^2
    "Poisson" = call("+", p$lambda, call("^", p$lambda, 2)),
    
    # E[X^2] = (2 - p) / p^2
    "Geometric" = call("/", call("-", 2, p$p), call("^", p$p, 2)),
    
    # E[X^2] = Var + E^2 = r(1-p)/p^2 + (r(1-p)/p)^2
    "NegBinomial" = call("+",
                         call("/", call("*", p$r, call("-", 1, p$p)), call("^", p$p, 2)),
                         call("^", call("/", call("*", p$r, call("-", 1, p$p)), p$p), 2)),
    
    # E[X^2] = df^2 + 2*df for Chi-squared (since Var = 2*df, E[X^2] = Var + E[X]^2)
    "ChiSq" = call("+", call("^", p$df, 2), call("*", 2, p$df)),
    
    # E[X^2] = df / (df - 2) for t-distribution (df > 2)
    "StudentT" = call("/", p$df, call("-", p$df, 2)),
    
    NULL
  )
}


#' @title Get Moment Generating Function for Known Distribution
#' @description Returns the symbolic MGF M_X(t) = E(e^(tX)) if the variable has
#'   a registered distribution.
#' @param var_name Character name of the variable
#' @param t_expr The expression for t in e^(tX)
#' @return The MGF expression, or NULL if not defined
#' @keywords internal
get.mgf <- function(var_name, t_expr) {
  if (!exists(var_name, envir = pkg.env)) {
    return(NULL)
  }
  
  dist_info <- get(var_name, envir = pkg.env)
  p <- dist_info$params
  
  switch(dist_info$distribution,
    # M_X(t) = exp(mu*t + sigma^2*t^2/2) for Normal(mu, sigma)
    "Normal" = call("exp", 
                    call("+", 
                         call("*", p$mu, t_expr),
                         call("/", call("*", call("^", p$sigma, 2), call("^", t_expr, 2)), 2))),
    
    # M_X(t) = (exp(t*b) - exp(t*a)) / (t*(b-a)) for Uniform(a, b)
    # This is complex, leave as E[exp(t*X)] for now
    
    # M_X(t) = lambda / (lambda - t) for Exponential(lambda), t < lambda
    "Exponential" = call("/", p$lambda, call("-", p$lambda, t_expr)),
    
    # M_X(t) = (beta / (beta - t))^alpha for Gamma(alpha, beta), t < beta
    "Gamma" = call("^", call("/", p$beta, call("-", p$beta, t_expr)), p$alpha),
    
    # M_X(t) = p*exp(t) / (1 - (1-p)*exp(t)) for Geometric(p)
    "Geometric" = call("/", 
                       call("*", p$p, call("exp", t_expr)),
                       call("-", 1, call("*", call("-", 1, p$p), call("exp", t_expr)))),
    
    # M_X(t) = exp(lambda*(exp(t) - 1)) for Poisson(lambda)
    "Poisson" = call("exp", call("*", p$lambda, call("-", call("exp", t_expr), 1))),
    
    # M_X(t) = (p*exp(t) / (1 - (1-p)*exp(t)))^r for NegBinomial(r, p)
    "NegBinomial" = call("^",
                         call("/",
                              call("*", p$p, call("exp", t_expr)),
                              call("-", 1, call("*", call("-", 1, p$p), call("exp", t_expr)))),
                         p$r),
    
    # M_X(t) = (p*exp(t) + (1-p))^n for Binomial(n, p)
    "Binomial" = call("^",
                      call("+", call("*", p$p, call("exp", t_expr)), call("-", 1, p$p)),
                      p$n),
    
    # M_X(t) = (1 - 2*t)^(-df/2) for ChiSq(df), t < 1/2
    "ChiSq" = call("^", call("-", 1, call("*", 2, t_expr)), call("-", call("/", p$df, 2))),
    
    # Note: Student's t distribution does not have a closed-form MGF
    
    NULL
  )
}


# ============================================================================
# GENERIC MOMENT FUNCTION
# ============================================================================

#' @title Print Conditional Moment
#' @description Custom print method for conditional expressions.
#'   Formats 'when(expr, cond)' as 'result when condition'.
#' @param x A conditional moment object
#' @param ... Additional arguments
#' @method print conditional_moment
#' @export
print.conditional_moment <- function(x, ...) {
  # x is a call: when(expr, cond)
  # Extract parts without evaluating
  expr_part <- x[[2]]
  cond_part <- x[[3]]
  
  # De-parse to strings
  expr_str <- deparse(expr_part)
  cond_str <- deparse(cond_part)
  
  # Print in requested format
  cat(expr_str, " when ", cond_str, "\n", sep = "")
  invisible(x)
}

#' @title Compute nth Raw Moment
#' @description Computes the nth raw moment E(X^n) of a random variable.
#'   For defined distributions, returns symbolic expressions. For undefined
#'   distributions, returns E(X^n) as-is.
#' @param X A random variable (uses non-standard evaluation)
#' @param n The moment order (positive integer)
#' @return A symbolic expression for the nth moment
#' @export
#' @examples
#' define(X ~ Normal(mu, sigma))
#' moment(X, 1)  # Returns: mu
#' moment(X, 2)  # Returns: sigma^2 + mu^2
#'
#' # For undefined variables
#' moment(Y, 3)  # Returns: E(Y^3)
moment <- function(X, n) {
  # Capture input using NSE
  x_ex <- substitute(X)
  x_ex <- ensure.expression(x_ex)
  
  if (!is.numeric(n) || n < 1 || n != as.integer(n)) {
    stop("n must be a positive integer")
  }
  
  n <- as.integer(n)
  
  result <- NULL
  
  # Handle simple RV case
  if (is.symbol(x_ex)) {
    var_name <- as.character(x_ex)
    raw_result <- get.nth.moment(var_name, n)
    if (!is.null(raw_result)) {
      result <- simplify.expr(raw_result)
    } else {
      # Fallback: return E(X^n)
      if (n == 1) {
        result <- call("E", x_ex)
      } else {
        result <- call("E", call("^", x_ex, as.numeric(n)))
      }
    }
  } else {
    # For complex expressions, build X^n and apply E
    if (n == 1) {
      result <- E(x_ex)
    } else {
      x_pow_n <- call("^", x_ex, as.numeric(n))
      raw_result <- expect.recursive(x_pow_n)
      result <- simplify.expr(raw_result)
    }
  }
  
  # Check for conditional result (when calls)
  result <- tag.conditional(result)
  
  return(result)
}

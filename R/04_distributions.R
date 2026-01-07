#' @title Package Environment for Distribution Metadata
#' @description Internal environment storing variable distribution definitions.
#'   Variables registered via define() are stored here with their distribution
#'   type and parameters.
#' @keywords internal
pkg.env <- new.env(parent = emptyenv())


#' @title Define Random Variable Distribution
#' @description Registers a random variable with its distribution, enabling
#'   automatic moment substitution in E(), Var(), etc.
#' @param formula A formula of the form X ~ Distribution(param1, param2, ...)
#' @return Invisibly returns the distribution info stored
#' @export
#' @examples
#' define(X ~ Normal(mu, sigma))
#' E(X)   # Returns: mu (instead of E(X))
#' E(X^2) # Returns: sigma^2 + mu^2
define <- function(formula) {
  # Parse the formula
  if (!inherits(formula, "formula")) {
    stop("define() requires a formula, e.g., X ~ Normal(mu, sigma)")
  }
  
  # Extract variable name (left side)
  var_name <- as.character(formula[[2]])
  
  # Extract distribution call (right side)
  dist_call <- formula[[3]]
  
  if (!is.call(dist_call)) {
    stop("Right side must be a distribution call, e.g., Normal(mu, sigma)")
  }
  
  dist_name <- as.character(dist_call[[1]])
  
  # Extract parameters based on distribution
  dist_info <- switch(dist_name,
    # === CONTINUOUS DISTRIBUTIONS ===
    
    "Normal" = {
      if (length(dist_call) != 3) {
        stop("Normal distribution requires 2 parameters: Normal(mu, sigma)")
      }
      list(
        distribution = "Normal",
        params = list(
          mu = dist_call[[2]],
          sigma = dist_call[[3]]
        )
      )
    },
    
    "Uniform" = {
      if (length(dist_call) != 3) {
        stop("Uniform distribution requires 2 parameters: Uniform(a, b)")
      }
      list(
        distribution = "Uniform",
        params = list(
          a = dist_call[[2]],
          b = dist_call[[3]]
        )
      )
    },
    
    "Exponential" = {
      if (length(dist_call) != 2) {
        stop("Exponential distribution requires 1 parameter: Exponential(lambda)")
      }
      list(
        distribution = "Exponential",
        params = list(
          lambda = dist_call[[2]]
        )
      )
    },
    
    "Gamma" = {
      if (length(dist_call) != 3) {
        stop("Gamma distribution requires 2 parameters: Gamma(alpha, beta) [shape, rate]")
      }
      list(
        distribution = "Gamma",
        params = list(
          alpha = dist_call[[2]],
          beta = dist_call[[3]]
        )
      )
    },
    
    "InvGamma" = {
      if (length(dist_call) != 3) {
        stop("InvGamma distribution requires 2 parameters: InvGamma(alpha, beta)")
      }
      list(
        distribution = "InvGamma",
        params = list(
          alpha = dist_call[[2]],
          beta = dist_call[[3]]
        )
      )
    },
    
    "Beta" = {
      if (length(dist_call) != 3) {
        stop("Beta distribution requires 2 parameters: Beta(alpha, beta)")
      }
      list(
        distribution = "Beta",
        params = list(
          alpha = dist_call[[2]],
          beta = dist_call[[3]]
        )
      )
    },
    
    "LogNormal" = {
      if (length(dist_call) != 3) {
        stop("LogNormal distribution requires 2 parameters: LogNormal(mu, sigma)")
      }
      list(
        distribution = "LogNormal",
        params = list(
          mu = dist_call[[2]],
          sigma = dist_call[[3]]
        )
      )
    },
    
    "Weibull" = {
      if (length(dist_call) != 3) {
        stop("Weibull distribution requires 2 parameters: Weibull(k, lambda) [shape, scale]")
      }
      list(
        distribution = "Weibull",
        params = list(
          k = dist_call[[2]],
          lambda = dist_call[[3]]
        )
      )
    },
    
    # === DISCRETE DISTRIBUTIONS ===
    
    "Binomial" = {
      if (length(dist_call) != 3) {
        stop("Binomial distribution requires 2 parameters: Binomial(n, p)")
      }
      list(
        distribution = "Binomial",
        params = list(
          n = dist_call[[2]],
          p = dist_call[[3]]
        )
      )
    },
    
    "Poisson" = {
      if (length(dist_call) != 2) {
        stop("Poisson distribution requires 1 parameter: Poisson(lambda)")
      }
      list(
        distribution = "Poisson",
        params = list(
          lambda = dist_call[[2]]
        )
      )
    },
    
    "Geometric" = {
      if (length(dist_call) != 2) {
        stop("Geometric distribution requires 1 parameter: Geometric(p)")
      }
      list(
        distribution = "Geometric",
        params = list(
          p = dist_call[[2]]
        )
      )
    },
    
    "NegBinomial" = {
      if (length(dist_call) != 3) {
        stop("NegBinomial distribution requires 2 parameters: NegBinomial(r, p)")
      }
      list(
        distribution = "NegBinomial",
        params = list(
          r = dist_call[[2]],
          p = dist_call[[3]]
        )
      )
    },
    
    # === ADDITIONAL DISTRIBUTIONS ===
    
    "ChiSq" = {
      if (length(dist_call) != 2) {
        stop("Chi-squared distribution requires 1 parameter: ChiSq(df)")
      }
      list(
        distribution = "ChiSq",
        params = list(
          df = dist_call[[2]]
        )
      )
    },
    
    "StudentT" = {
      if (length(dist_call) != 2) {
        stop("Student's t distribution requires 1 parameter: StudentT(df)")
      }
      list(
        distribution = "StudentT",
        params = list(
          df = dist_call[[2]]
        )
      )
    },
    
    stop(paste("Unknown distribution:", dist_name))
  )
  
  # Store in package environment
  assign(var_name, dist_info, envir = pkg.env)
  
  invisible(dist_info)
}


#' @title Clear Variable Definitions
#' @description Removes all registered variable definitions from the package
#'   environment.
#' @export
#' @examples
#' define(X ~ Normal(mu, sigma))
#' clear.definitions()
#' E(X)   # Returns: E(X) (no longer has distribution info)
clear.definitions <- function() {
  rm(list = ls(envir = pkg.env), envir = pkg.env)
  invisible(NULL)
}


#' @title Undefine a Variable
#' @description Removes the distribution definition for a specific variable.
#' @param var_name Character name of the variable to undefine
#' @export
#' @examples
#' define(X ~ Normal(mu, sigma))
#' undefine("X")
undefine <- function(var_name) {
  if (exists(var_name, envir = pkg.env)) {
    rm(list = var_name, envir = pkg.env)
  }
  invisible(NULL)
}


# ============================================================================
# INDEPENDENCE MANAGEMENT
# ============================================================================

#' @title Declare Independent Random Variables
#' @description Registers random variables as mutually independent. When
#'   independence is declared, E(X * Y) simplifies to E(X) * E(Y).
#' @param ... Random variable symbols to declare as mutually independent
#' @return Invisibly returns the list of independence pairs added
#' @export
#' @examples
#' assume.independent(X, Y)
#' E(X * Y)  # Returns: E(X) * E(Y)
#' 
#' assume.independent(X, Y, Z)  # All three are mutually independent
#' E(X * Y * Z)  # Returns: E(X) * E(Y) * E(Z)
assume.independent <- function(...) {
  # Capture variable names using NSE
  args <- as.list(substitute(list(...)))[-1]
  var_names <- sapply(args, as.character)
  
  if (length(var_names) < 2) {
    stop("assume.independent() requires at least 2 variables")
  }
  
  # Initialize independence list if not exists
  if (!exists(".independence_pairs", envir = pkg.env)) {
    assign(".independence_pairs", list(), envir = pkg.env)
  }
  
  pairs <- get(".independence_pairs", envir = pkg.env)
  new_pairs <- list()
  
  # Generate all pairs (for mutual independence of n variables)
  for (i in 1:(length(var_names) - 1)) {
    for (j in (i + 1):length(var_names)) {
      pair <- sort(c(var_names[i], var_names[j]))
      pair_key <- paste(pair, collapse = ":")
      
      # Check if pair already exists
      existing_keys <- sapply(pairs, function(p) paste(sort(p), collapse = ":"))
      if (!(pair_key %in% existing_keys)) {
        pairs[[length(pairs) + 1]] <- pair
        new_pairs[[length(new_pairs) + 1]] <- pair
      }
    }
  }
  
  assign(".independence_pairs", pairs, envir = pkg.env)
  invisible(new_pairs)
}


#' @title Check Independence
#' @description Checks if two random variables have been declared independent.
#' @param x Character name of first variable (or symbol)
#' @param y Character name of second variable (or symbol)
#' @return Logical TRUE if x and y are declared independent, FALSE otherwise
#' @keywords internal
are.independent <- function(x, y) {
  # Handle both character and symbol input
  if (is.symbol(x)) x <- as.character(x)
  if (is.symbol(y)) y <- as.character(y)
  
  if (!exists(".independence_pairs", envir = pkg.env)) {
    return(FALSE)
  }
  
  pairs <- get(".independence_pairs", envir = pkg.env)
  if (length(pairs) == 0) {
    return(FALSE)
  }
  
  pair <- sort(c(x, y))
  pair_key <- paste(pair, collapse = ":")
  
  existing_keys <- sapply(pairs, function(p) paste(sort(p), collapse = ":"))
  return(pair_key %in% existing_keys)
}


#' @title Clear Independence Assumptions
#' @description Removes all independence assumptions.
#' @export
#' @examples
#' assume.independent(X, Y)
#' clear.independence()
#' E(X * Y)  # Returns: E(X * Y) (no longer factors)
clear.independence <- function() {
  if (exists(".independence_pairs", envir = pkg.env)) {
    rm(".independence_pairs", envir = pkg.env)
  }
  invisible(NULL)
}


#' @title Show Independence Assumptions
#' @description Displays all current independence assumptions.
#' @return A character vector describing independence pairs, or a message if none
#' @export
#' @examples
#' assume.independent(X, Y)
#' assume.independent(A, B, C)
#' show.independence()
show.independence <- function() {
  if (!exists(".independence_pairs", envir = pkg.env)) {
    message("No independence assumptions declared.")
    return(invisible(character(0)))
  }
  
  pairs <- get(".independence_pairs", envir = pkg.env)
  if (length(pairs) == 0) {
    message("No independence assumptions declared.")
    return(invisible(character(0)))
  }
  
  # Format as "X ⊥ Y"
  pair_strs <- sapply(pairs, function(p) paste(p[1], "\u22A5", p[2]))
  
  cat("Independence assumptions:\n")
  for (p in pair_strs) {
    cat("  ", p, "\n")
  }
  
  invisible(pair_strs)
}


#' @title Get First Moment for Known Distribution
#' @description Returns the symbolic first moment E[X] if the variable has
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
#' @description Returns the symbolic second moment E[X^2] if the variable has
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
#' @description Returns the symbolic MGF M_X(t) = E[e^(tX)] if the variable has
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
#'   Formats 'when(expr, cond)' as '[expr] when [cond]'.
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
#' @description Computes the nth raw moment E[X^n] of a random variable.
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


#' @title Get nth Moment for Known Distribution
#' @description Returns the symbolic nth raw moment E[X^n] if the variable has
#'   a registered distribution and the moment is implemented.
#' @param var_name Character name of the variable
#' @param n The moment order (positive integer)
#' @return The moment expression, or NULL if not defined
#' @keywords internal
get.nth.moment <- function(var_name, n) {
  # Use existing moment functions for n=1,2
  if (n == 1) {
    return(get.first.moment(var_name))
  }
  if (n == 2) {
    return(get.second.moment(var_name))
  }
  
  if (!exists(var_name, envir = pkg.env)) {
    return(NULL)
  }
  
  dist_info <- get(var_name, envir = pkg.env)
  p <- dist_info$params
  
  # ============================================================================
  # THIRD MOMENTS (n = 3)
  # ============================================================================
  if (n == 3) {
    return(switch(dist_info$distribution,
      # === NORMAL: E[X^3] = mu^3 + 3*mu*sigma^2 ===
      "Normal" = call("+", call("^", p$mu, 3), 
                      call("*", 3, call("*", p$mu, call("^", p$sigma, 2)))),
      
      # === UNIFORM: E[X^3] = (a + b)(a^2 + b^2) / 4 ===
      "Uniform" = call("/", 
                       call("*", call("+", p$a, p$b), 
                            call("+", call("^", p$a, 2), call("^", p$b, 2))),
                       4),
      
      # === EXPONENTIAL: E[X^3] = 6/lambda^3 ===
      "Exponential" = call("/", 6, call("^", p$lambda, 3)),
      
      # === GAMMA: E[X^3] = alpha*(alpha+1)*(alpha+2) / beta^3 ===
      "Gamma" = call("/", 
                     call("*", call("*", p$alpha, call("+", p$alpha, 1)), 
                          call("+", p$alpha, 2)),
                     call("^", p$beta, 3)),
      
      # === INVGAMMA: E[X^3] = beta^3 / ((alpha-1)(alpha-2)(alpha-3)) [alpha > 3] ===
      "InvGamma" = call("when", 
                        call("/", call("^", p$beta, 3),
                             call("*", call("*", call("-", p$alpha, 1), 
                                           call("-", p$alpha, 2)),
                                  call("-", p$alpha, 3))),
                        call(">", p$alpha, 3)),
      
      # === BETA: E[X^3] = alpha*(alpha+1)*(alpha+2) / ((a+b)(a+b+1)(a+b+2)) ===
      "Beta" = {
        ab <- call("+", p$alpha, p$beta)
        call("/",
             call("*", call("*", p$alpha, call("+", p$alpha, 1)), call("+", p$alpha, 2)),
             call("*", call("*", ab, call("+", ab, 1)), call("+", ab, 2)))
      },
      
      # === LOGNORMAL: E[X^3] = exp(3*mu + 9*sigma^2/2) ===
      "LogNormal" = call("exp", call("+", call("*", 3, p$mu), 
                                     call("/", call("*", 9, call("^", p$sigma, 2)), 2))),
      
      # === WEIBULL: E[X^3] = lambda^3 * Gamma(1 + 3/k) ===
      "Weibull" = call("*", call("^", p$lambda, 3), 
                       call("gamma", call("+", 1, call("/", 3, p$k)))),
      
      # === BINOMIAL: E[X^3] = np*(1 + 3*(n-1)*p + (n-1)*(n-2)*p^2) ===
      "Binomial" = {
        np <- call("*", p$n, p$p)
        n_1 <- call("-", p$n, 1)
        n_2 <- call("-", p$n, 2)
        call("*", np, 
             call("+", 1,
                  call("+", call("*", 3, call("*", n_1, p$p)),
                       call("*", call("*", n_1, n_2), call("^", p$p, 2)))))
      },
      
      # === POISSON: E[X^3] = lambda + 3*lambda^2 + lambda^3 ===
      "Poisson" = call("+", call("+", p$lambda, call("*", 3, call("^", p$lambda, 2))), 
                       call("^", p$lambda, 3)),
      
      # === GEOMETRIC: E[X^3] = (p^2 - 6*p + 6) / p^3 ===
      "Geometric" = call("/", 
                         call("+", call("-", call("^", p$p, 2), call("*", 6, p$p)), 6),
                         call("^", p$p, 3)),
      
      # === NEGBINOMIAL: E[X^3] = r*(1-p)*(r^2*(1-p)^2 + 3*r*(1-p) + 1 + p) / p^3 ===
      "NegBinomial" = {
        q <- call("-", 1, p$p)  # 1-p
        rq <- call("*", p$r, q)
        call("/",
             call("*", rq, 
                  call("+", call("+", call("+", call("*", call("^", p$r, 2), call("^", q, 2)), 
                                           call("*", 3, rq)), 1), p$p)),
             call("^", p$p, 3))
      },
      
      # === CHISQ: E[X^3] = df*(df+2)*(df+4) ===
      "ChiSq" = call("*", call("*", p$df, call("+", p$df, 2)), call("+", p$df, 4)),
      
      # === STUDENTT: E[X^3] = 0 [df > 3, symmetric] ===
      "StudentT" = call("when", 0, call(">", p$df, 3)),
      
      NULL
    ))
  }
  
  # ============================================================================
  # FOURTH MOMENTS (n = 4)
  # ============================================================================
  if (n == 4) {
    return(switch(dist_info$distribution,
      # === NORMAL: E[X^4] = mu^4 + 6*mu^2*sigma^2 + 3*sigma^4 ===
      "Normal" = call("+", call("+", call("^", p$mu, 4), 
                       call("*", 6, call("*", call("^", p$mu, 2), call("^", p$sigma, 2)))),
                       call("*", 3, call("^", p$sigma, 4))),
      
      # === UNIFORM: E[X^4] = (a^4 + a^3*b + a^2*b^2 + a*b^3 + b^4) / 5 ===
      "Uniform" = call("/",
                       call("+", call("+", call("+", call("+",
                            call("^", p$a, 4),
                            call("*", call("^", p$a, 3), p$b)),
                            call("*", call("^", p$a, 2), call("^", p$b, 2))),
                            call("*", p$a, call("^", p$b, 3))),
                            call("^", p$b, 4)),
                       5),
      
      # === EXPONENTIAL: E[X^4] = 24/lambda^4 ===
      "Exponential" = call("/", 24, call("^", p$lambda, 4)),
      
      # === GAMMA: E[X^4] = alpha*(alpha+1)*(alpha+2)*(alpha+3) / beta^4 ===
      "Gamma" = call("/", 
                     call("*", call("*", call("*", p$alpha, call("+", p$alpha, 1)), 
                                    call("+", p$alpha, 2)), call("+", p$alpha, 3)),
                     call("^", p$beta, 4)),
      
      # === INVGAMMA: E[X^4] = beta^4 / ((alpha-1)(alpha-2)(alpha-3)(alpha-4)) [alpha > 4] ===
      "InvGamma" = call("when",
                        call("/", call("^", p$beta, 4),
                             call("*", call("*", call("*", call("-", p$alpha, 1), 
                                                      call("-", p$alpha, 2)),
                                            call("-", p$alpha, 3)),
                                  call("-", p$alpha, 4))),
                        call(">", p$alpha, 4)),
      
      # === BETA: E[X^4] = alpha*(alpha+1)*(alpha+2)*(alpha+3) / ((a+b)(a+b+1)(a+b+2)(a+b+3)) ===
      "Beta" = {
        ab <- call("+", p$alpha, p$beta)
        call("/",
             call("*", call("*", call("*", p$alpha, call("+", p$alpha, 1)), 
                           call("+", p$alpha, 2)), call("+", p$alpha, 3)),
             call("*", call("*", call("*", ab, call("+", ab, 1)), 
                           call("+", ab, 2)), call("+", ab, 3)))
      },
      
      # === LOGNORMAL: E[X^4] = exp(4*mu + 8*sigma^2) ===
      "LogNormal" = call("exp", call("+", call("*", 4, p$mu), 
                                     call("*", 8, call("^", p$sigma, 2)))),
      
      # === WEIBULL: E[X^4] = lambda^4 * Gamma(1 + 4/k) ===
      "Weibull" = call("*", call("^", p$lambda, 4), 
                       call("gamma", call("+", 1, call("/", 4, p$k)))),
      
      # === BINOMIAL: E[X^4] (complex formula) ===
      "Binomial" = {
        np <- call("*", p$n, p$p)
        # Use the raw moment formula: n*p*(1 + 7*(n-1)*p + 6*(n-1)*(n-2)*p^2 + (n-1)*(n-2)*(n-3)*p^3)
        n_1 <- call("-", p$n, 1)
        n_2 <- call("-", p$n, 2)
        n_3 <- call("-", p$n, 3)
        call("*", np,
             call("+", 1,
                  call("+", call("*", 7, call("*", n_1, p$p)),
                       call("+", call("*", 6, call("*", call("*", n_1, n_2), call("^", p$p, 2))),
                            call("*", call("*", call("*", n_1, n_2), n_3), call("^", p$p, 3))))))
      },
      
      # === POISSON: E[X^4] = lambda + 7*lambda^2 + 6*lambda^3 + lambda^4 ===
      "Poisson" = call("+", call("+", call("+", p$lambda, 
                                           call("*", 7, call("^", p$lambda, 2))),
                                 call("*", 6, call("^", p$lambda, 3))),
                       call("^", p$lambda, 4)),
      
      # === GEOMETRIC: E[X^4] = (-p^3 + 14*p^2 - 36*p + 24) / p^4 ===
      "Geometric" = call("/",
                         call("+", call("+", call("-", call("*", -1, call("^", p$p, 3)),
                                                  call("*", 14, call("^", p$p, 2))),
                                        call("*", -36, p$p)), 24),
                         call("^", p$p, 4)),
      
      # === NEGBINOMIAL: Complex but closed-form ===
      "NegBinomial" = {
        q <- call("-", 1, p$p)
        # E[X^4] involves r, p terms - use variance-based expansion
        # Simplified: return Var + moment relationships
        call("/",
             call("*", call("*", p$r, q),
                  call("+", 1, call("+", call("*", 7, call("*", call("-", p$r, 1), q)),
                                    call("+", call("*", 6, call("*", call("*", call("-", p$r, 1), call("-", p$r, 2)), call("^", q, 2))),
                                         call("*", call("*", call("*", call("-", p$r, 1), call("-", p$r, 2)), call("-", p$r, 3)), call("^", q, 3)))))),
             call("^", p$p, 4))
      },
      
      # === CHISQ: E[X^4] = df*(df+2)*(df+4)*(df+6) ===
      "ChiSq" = call("*", call("*", call("*", p$df, call("+", p$df, 2)), 
                               call("+", p$df, 4)), call("+", p$df, 6)),
      
      # === STUDENTT: E[X^4] = 3*df^2 / ((df-2)*(df-4)) [df > 4] ===
      "StudentT" = call("when",
                        call("/", call("*", 3, call("^", p$df, 2)),
                             call("*", call("-", p$df, 2), call("-", p$df, 4))),
                        call(">", p$df, 4)),
      
      NULL
    ))
  }
  
  NULL
}


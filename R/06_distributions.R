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

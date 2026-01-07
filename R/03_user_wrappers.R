#' @title Symbolic Expectation
#' @description Computes the symbolic expectation of an expression using
#'   the linearity of expectation. Uses non-standard evaluation.
#' @param expr An R expression involving random variables and constants.
#'   Random variables are identified by uppercase first letter.
#' @return An unevaluated expression representing E[expr]
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
#' @return An unevaluated expression representing Var[expr]
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
#' @return An unevaluated expression representing Cov[x, y]
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


#' @title Derive Expectation Step-by-Step
#' @description Shows step-by-step derivation of the expectation calculation,
#'   including all rules applied and intermediate results.
#' @param expr An R expression involving random variables and constants
#' @return A list with steps showing the derivation
#' @export
#' @examples
#' derive.E(a * X + b)
#' derive.E(X + Y)
derive.E <- function(expr) {
  # Capture input using NSE
  ex <- substitute(expr)
  ex <- ensure.expression(ex)
  
  steps <- list()
  step_num <- 1
  
  # Step 1: Input
  steps[[step_num]] <- list(
    step = step_num,
    description = "Starting expression",
    rule = "Input",
    expression = deparse(call("E", ex))
  )
  step_num <- step_num + 1
  
  # Step 2: Apply linearity and distribution rules
  result <- expect.recursive(ex)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Apply expectation rules",
    rule = "Linearity: E[aX + b] = aE[X] + b, E[X + Y] = E[X] + E[Y]",
    expression = deparse(result)
  )
  step_num <- step_num + 1
  
  # Step 3: Check for distribution substitutions
  has_substitution <- !identical(result, call("E", ex))
  if (has_substitution) {
    steps[[step_num]] <- list(
      step = step_num,
      description = "Substitute known distributions",
      rule = "If X ~ Distribution, replace E[X] with known moment",
      expression = deparse(result)
    )
    step_num <- step_num + 1
  }
  
  # Step 4: Simplify
  clean_result <- simplify.expr(result)
  if (!identical(clean_result, result)) {
    steps[[step_num]] <- list(
      step = step_num,
      description = "Simplify",
      rule = "Remove identity elements (x + 0 = x, x * 1 = x)",
      expression = deparse(clean_result)
    )
    step_num <- step_num + 1
  }
  
  # Final result
  steps[[step_num]] <- list(
    step = step_num,
    description = "Final result",
    rule = "Done",
    expression = deparse(clean_result)
  )
  
  result_obj <- list(
    input = deparse(call("E", ex)),
    steps = steps,
    result = clean_result
  )
  
  class(result_obj) <- "derivation"
  return(result_obj)
}


#' @title Derive Variance Step-by-Step
#' @description Shows step-by-step derivation of the variance calculation.
#' @param expr An R expression involving random variables and constants
#' @return A list with steps showing the derivation
#' @export
#' @examples
#' derive.Var(X)
#' derive.Var(a * X + b)
derive.Var <- function(expr) {
  # Capture input using NSE
  ex <- substitute(expr)
  ex <- ensure.expression(ex)
  
  steps <- list()
  step_num <- 1
  
  # Step 1: Input
  steps[[step_num]] <- list(
    step = step_num,
    description = "Starting expression",
    rule = "Input",
    expression = deparse(call("Var", ex))
  )
  step_num <- step_num + 1
  
  # Step 2: Definition
  x_squared <- call("^", ex, 2)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Apply variance definition",
    rule = "Var[X] = E[X²] - E[X]²",
    expression = paste0("E[", deparse(x_squared), "] - E[", deparse(ex), "]²")
  )
  step_num <- step_num + 1
  
  # Step 3: Expand polynomial if needed
  x_squared_expanded <- expand.poly(x_squared)
  if (!identical(x_squared_expanded, x_squared)) {
    steps[[step_num]] <- list(
      step = step_num,
      description = "Expand polynomial",
      rule = "(a + b)² = a² + 2ab + b²",
      expression = paste0("E[", deparse(x_squared_expanded), "] - E[", deparse(ex), "]²")
    )
    step_num <- step_num + 1
  }
  
  # Step 4: Compute E[X²]
  e_x_squared <- expect.recursive(x_squared_expanded)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Compute E[X²]",
    rule = "Apply expectation rules to X²",
    expression = paste0(deparse(e_x_squared), " - E[", deparse(ex), "]²")
  )
  step_num <- step_num + 1
  
  # Step 5: Compute E[X]
  e_x <- expect.recursive(ex)
  e_x_sq <- call("^", e_x, 2)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Compute E[X]²",
    rule = "Apply expectation rules to X, then square",
    expression = paste0(deparse(e_x_squared), " - ", deparse(e_x_sq))
  )
  step_num <- step_num + 1
  
  # Step 6: Combine
  result <- call("-", e_x_squared, e_x_sq)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Combine terms",
    rule = "E[X²] - E[X]²",
    expression = deparse(result)
  )
  step_num <- step_num + 1
  
  # Step 7: Simplify
  clean_result <- simplify.expr(simplify.expr(simplify.expr(result)))
  if (!identical(clean_result, result)) {
    steps[[step_num]] <- list(
      step = step_num,
      description = "Simplify",
      rule = "Algebraic simplification",
      expression = deparse(clean_result)
    )
    step_num <- step_num + 1
  }
  
  # Final
  steps[[step_num]] <- list(
    step = step_num,
    description = "Final result",
    rule = "Done",
    expression = deparse(clean_result)
  )
  
  result_obj <- list(
    input = deparse(call("Var", ex)),
    steps = steps,
    result = clean_result
  )
  
  class(result_obj) <- "derivation"
  return(result_obj)
}


#' @title Derive Covariance Step-by-Step
#' @description Shows step-by-step derivation of the covariance calculation.
#' @param x First random variable expression
#' @param y Second random variable expression
#' @return A list with steps showing the derivation
#' @export
#' @examples
#' derive.Cov(X, Y)
derive.Cov <- function(x, y) {
  # Capture inputs using NSE
  x_ex <- substitute(x)
  y_ex <- substitute(y)
  x_ex <- ensure.expression(x_ex)
  y_ex <- ensure.expression(y_ex)
  
  steps <- list()
  step_num <- 1
  
  # Step 1: Input
  input_expr <- call("Cov", x_ex, y_ex)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Starting expression",
    rule = "Input",
    expression = deparse(input_expr)
  )
  step_num <- step_num + 1
  
  # Step 2: Definition
  xy <- call("*", x_ex, y_ex)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Apply covariance definition",
    rule = "Cov[X, Y] = E[XY] - E[X]E[Y]",
    expression = paste0("E[", deparse(xy), "] - E[", deparse(x_ex), "] * E[", deparse(y_ex), "]")
  )
  step_num <- step_num + 1
  
  # Step 3: Expand product if needed
  xy_expanded <- expand.poly(xy)
  if (!identical(xy_expanded, xy)) {
    steps[[step_num]] <- list(
      step = step_num,
      description = "Expand product",
      rule = "Distribute multiplication",
      expression = paste0("E[", deparse(xy_expanded), "] - E[", deparse(x_ex), "] * E[", deparse(y_ex), "]")
    )
    step_num <- step_num + 1
  }
  
  # Step 4: Compute E[XY]
  e_xy <- expect.recursive(xy_expanded)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Compute E[XY]",
    rule = "Apply expectation rules to product",
    expression = paste0(deparse(e_xy), " - E[", deparse(x_ex), "] * E[", deparse(y_ex), "]")
  )
  step_num <- step_num + 1
  
  # Step 5: Compute E[X] and E[Y]
  e_x <- expect.recursive(x_ex)
  e_y <- expect.recursive(y_ex)
  e_x_times_e_y <- call("*", e_x, e_y)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Compute E[X] * E[Y]",
    rule = "Apply expectation rules to each variable",
    expression = paste0(deparse(e_xy), " - ", deparse(e_x_times_e_y))
  )
  step_num <- step_num + 1
  
  # Step 6: Combine
  result <- call("-", e_xy, e_x_times_e_y)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Combine terms",
    rule = "E[XY] - E[X]E[Y]",
    expression = deparse(result)
  )
  step_num <- step_num + 1
  
  # Step 7: Simplify
  clean_result <- simplify.expr(simplify.expr(simplify.expr(result)))
  if (!identical(clean_result, result)) {
    steps[[step_num]] <- list(
      step = step_num,
      description = "Simplify",
      rule = "Algebraic simplification",
      expression = deparse(clean_result)
    )
    step_num <- step_num + 1
  }
  
  # Final
  steps[[step_num]] <- list(
    step = step_num,
    description = "Final result",
    rule = "Done",
    expression = deparse(clean_result)
  )
  
  result_obj <- list(
    input = deparse(input_expr),
    steps = steps,
    result = clean_result
  )
  
  class(result_obj) <- "derivation"
  return(result_obj)
}


#' @title Print Derivation
#' @description Prints a derivation object in a readable format.
#' @param x A derivation object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the derivation object
#' @method print derivation
#' @export
print.derivation <- function(x, ...) {
  cat("Derivation of:", x$input, "\n")
  cat(rep("=", 50), "\n", sep = "")
  
  for (step in x$steps) {
    cat("\nStep ", step$step, ": ", step$description, "\n", sep = "")
    cat("  Rule: ", step$rule, "\n", sep = "")
    cat("  => ", step$expression, "\n", sep = "")
  }
  
  cat("\n", rep("=", 50), "\n", sep = "")
  cat("Final Answer: ", deparse(x$result), "\n", sep = "")
  
  invisible(x)
}


# ============================================================================
# HIGHER-ORDER STATISTICS
# ============================================================================

#' @title Symbolic Skewness
#' @description Computes the symbolic skewness using the third standardized moment.
#'   Skewness = E[(X - μ)³] / σ³ = (E[X³] - 3μσ² - μ³) / σ³
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
#'   Excess Kurtosis = E[(X - μ)⁴] / σ⁴ - 3
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



#' @title Derive Skewness Step-by-Step
#' @description Shows step-by-step derivation of the skewness calculation.
#' @param expr An R expression involving random variables and constants
#' @return A list with steps showing the derivation
#' @export
#' @examples
#' derive.Skewness(X)
derive.Skewness <- function(expr) {
  # Capture input using NSE
  ex <- substitute(expr)
  ex <- ensure.expression(ex)
  
  steps <- list()
  step_num <- 1
  
  # Step 1: Input
  steps[[step_num]] <- list(
    step = step_num,
    description = "Starting expression",
    rule = "Input",
    expression = deparse(call("Skewness", ex))
  )
  step_num <- step_num + 1
  
  # Step 2: Definition
  steps[[step_num]] <- list(
    step = step_num,
    description = "Apply skewness definition",
    rule = "Skewness = E[(X-μ)³] / σ³",
    expression = "E[(X - E[X])³] / (Var[X])^(3/2)"
  )
  step_num <- step_num + 1
  
  # Step 3: Compute moments
  e_x <- expect.recursive(ex)
  x_squared <- call("^", ex, 2)
  e_x2 <- expect.recursive(expand.poly(x_squared))
  x_cubed <- call("^", ex, 3)
  e_x3 <- expect.recursive(expand.poly(x_cubed))
  
  steps[[step_num]] <- list(
    step = step_num,
    description = "Compute required moments",
    rule = "E[X], E[X²], E[X³]",
    expression = paste0("E[X] = ", deparse(e_x), ", E[X²] = ", deparse(e_x2), ", E[X³] = ", deparse(e_x3))
  )
  step_num <- step_num + 1
  
  # Step 4: Show substituted expression
  # Build the skewness formula with substituted values
  var_expr <- simplify.expr(call("-", e_x2, call("^", e_x, 2)))
  sigma_cubed <- call("^", var_expr, quote(3/2))
  # Third central moment: E[X³] - 3μE[X²] + 2μ³
  third_central <- simplify.expr(
    call("+", call("-", e_x3, call("*", 3, call("*", e_x, e_x2))),
         call("*", 2, call("^", e_x, 3)))
  )
  skew_expr <- call("/", third_central, sigma_cubed)
  
  steps[[step_num]] <- list(
    step = step_num,
    description = "Substitute moments into formula",
    rule = "γ₁ = (E[X³] - 3μE[X²] + 2μ³) / σ³",
    expression = deparse(simplify.expr(skew_expr))
  )
  step_num <- step_num + 1
  
  # Step 5: Final result
  result <- eval(call("Skewness", ex))
  steps[[step_num]] <- list(
    step = step_num,
    description = "Final result",
    rule = "Done",
    expression = deparse(result)
  )
  
  result_obj <- list(
    input = deparse(call("Skewness", ex)),
    steps = steps,
    result = result
  )
  
  class(result_obj) <- "derivation"
  return(result_obj)
}


#' @title Derive Kurtosis Step-by-Step
#' @description Shows step-by-step derivation of the kurtosis calculation.
#' @param expr An R expression involving random variables and constants
#' @param excess Logical; if TRUE (default), derives excess kurtosis
#' @return A list with steps showing the derivation
#' @export
#' @examples
#' derive.Kurtosis(X)
derive.Kurtosis <- function(expr, excess = TRUE) {
  # Capture input using NSE
  ex <- substitute(expr)
  ex <- ensure.expression(ex)
  
  steps <- list()
  step_num <- 1
  
  # Step 1: Input
  steps[[step_num]] <- list(
    step = step_num,
    description = "Starting expression",
    rule = "Input",
    expression = deparse(call("Kurtosis", ex))
  )
  step_num <- step_num + 1
  
  # Step 2: Definition
  if (excess) {
    steps[[step_num]] <- list(
      step = step_num,
      description = "Apply excess kurtosis definition",
      rule = "Excess Kurtosis = E[(X-μ)⁴] / σ⁴ - 3",
      expression = "E[(X - E[X])⁴] / (Var[X])² - 3"
    )
  } else {
    steps[[step_num]] <- list(
      step = step_num,
      description = "Apply kurtosis definition",
      rule = "Kurtosis = E[(X-μ)⁴] / σ⁴",
      expression = "E[(X - E[X])⁴] / (Var[X])²"
    )
  }
  step_num <- step_num + 1
  
  # Step 3: Compute moments
  e_x <- expect.recursive(ex)
  x_squared <- call("^", ex, 2)
  e_x2 <- expect.recursive(expand.poly(x_squared))
  x_fourth <- call("^", ex, 4)
  e_x4 <- expect.recursive(expand.poly(x_fourth))
  
  steps[[step_num]] <- list(
    step = step_num,
    description = "Compute required moments",
    rule = "E[X], E[X²], E[X⁴]",
    expression = paste0("E[X] = ", deparse(e_x), ", E[X²] = ", deparse(e_x2), ", E[X⁴] = ", deparse(e_x4))
  )
  step_num <- step_num + 1
  
  # Step 4: Show substituted expression
  # Compute E[X³] for the formula
  x_cubed <- call("^", ex, 3)
  e_x3 <- expect.recursive(expand.poly(x_cubed))
  
  var_expr <- simplify.expr(call("-", e_x2, call("^", e_x, 2)))
  sigma_fourth <- call("^", var_expr, 2)
  # Fourth central moment: E[X⁴] - 4μE[X³] + 6μ²E[X²] - 3μ⁴
  fourth_central <- simplify.expr(
    call("-",
         call("+",
              call("-", e_x4, call("*", 4, call("*", e_x, e_x3))),
              call("*", 6, call("*", call("^", e_x, 2), e_x2))),
         call("*", 3, call("^", e_x, 4)))
  )
  kurt_expr <- call("/", fourth_central, sigma_fourth)
  if (excess) {
    kurt_expr <- call("-", kurt_expr, 3)
  }
  
  steps[[step_num]] <- list(
    step = step_num,
    description = "Substitute moments into formula",
    rule = "γ₂ = (E[X⁴] - 4μE[X³] + 6μ²E[X²] - 3μ⁴) / σ⁴ - 3",
    expression = deparse(simplify.expr(kurt_expr))
  )
  step_num <- step_num + 1
  
  # Step 5: Final result
  result <- eval(call("Kurtosis", ex, excess = excess))
  steps[[step_num]] <- list(
    step = step_num,
    description = "Final result",
    rule = "Done",
    expression = deparse(result)
  )
  
  result_obj <- list(
    input = deparse(call("Kurtosis", ex)),
    steps = steps,
    result = result
  )
  
  class(result_obj) <- "derivation"
  return(result_obj)
}

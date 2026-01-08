# ============================================================================
# STEP-BY-STEP DERIVATION FUNCTIONS
# ============================================================================

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
    rule = "Var[X] = E[X^2] - E[X]^2",
    expression = paste0("E[", deparse(x_squared), "] - E[", deparse(ex), "]^2")
  )
  step_num <- step_num + 1
  
  # Step 3: Expand polynomial if needed
  x_squared_expanded <- expand.poly(x_squared)
  if (!identical(x_squared_expanded, x_squared)) {
    steps[[step_num]] <- list(
      step = step_num,
      description = "Expand polynomial",
      rule = "(a + b)^2 = a^2 + 2ab + b^2",
      expression = paste0("E[", deparse(x_squared_expanded), "] - E[", deparse(ex), "]^2")
    )
    step_num <- step_num + 1
  }
  
  # Step 4: Compute E[X^2]
  e_x_squared <- expect.recursive(x_squared_expanded)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Compute E[X^2]",
    rule = "Apply expectation rules to X^2",
    expression = paste0(deparse(e_x_squared), " - E[", deparse(ex), "]^2")
  )
  step_num <- step_num + 1
  
  # Step 5: Compute E[X]
  e_x <- expect.recursive(ex)
  e_x_sq <- call("^", e_x, 2)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Compute E[X]^2",
    rule = "Apply expectation rules to X, then square",
    expression = paste0(deparse(e_x_squared), " - ", deparse(e_x_sq))
  )
  step_num <- step_num + 1
  
  # Step 6: Combine
  result <- call("-", e_x_squared, e_x_sq)
  steps[[step_num]] <- list(
    step = step_num,
    description = "Combine terms",
    rule = "E[X^2] - E[X]^2",
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
    rule = "Skewness = E[(X-mu)^3] / sigma^3",
    expression = "E[(X - E[X])^3] / (Var[X])^(3/2)"
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
    rule = "E[X], E[X^2], E[X^3]",
    expression = paste0("E[X] = ", deparse(e_x), ", E[X^2] = ", deparse(e_x2), ", E[X^3] = ", deparse(e_x3))
  )
  step_num <- step_num + 1
  
  # Step 4: Show substituted expression
  # Build the skewness formula with substituted values
  var_expr <- simplify.expr(call("-", e_x2, call("^", e_x, 2)))
  sigma_cubed <- call("^", var_expr, quote(3/2))
  # Third central moment: E[X^3] - 3*mu*E[X^2] + 2*mu^3
  third_central <- simplify.expr(
    call("+", call("-", e_x3, call("*", 3, call("*", e_x, e_x2))),
         call("*", 2, call("^", e_x, 3)))
  )
  skew_expr <- call("/", third_central, sigma_cubed)
  
  steps[[step_num]] <- list(
    step = step_num,
    description = "Substitute moments into formula",
    rule = "gamma1 = (E[X^3] - 3*mu*E[X^2] + 2*mu^3) / sigma^3",
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
      rule = "Excess Kurtosis = E[(X-mu)^4] / sigma^4 - 3",
      expression = "E[(X - E[X])^4] / (Var[X])^2 - 3"
    )
  } else {
    steps[[step_num]] <- list(
      step = step_num,
      description = "Apply kurtosis definition",
      rule = "Kurtosis = E[(X-mu)^4] / sigma^4",
      expression = "E[(X - E[X])^4] / (Var[X])^2"
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
    rule = "E[X], E[X^2], E[X^4]",
    expression = paste0("E[X] = ", deparse(e_x), ", E[X^2] = ", deparse(e_x2), ", E[X^4] = ", deparse(e_x4))
  )
  step_num <- step_num + 1
  
  # Step 4: Show substituted expression
  # Compute E[X^3] for the formula
  x_cubed <- call("^", ex, 3)
  e_x3 <- expect.recursive(expand.poly(x_cubed))
  
  var_expr <- simplify.expr(call("-", e_x2, call("^", e_x, 2)))
  sigma_fourth <- call("^", var_expr, 2)
  # Fourth central moment: E[X^4] - 4*mu*E[X^3] + 6*mu^2*E[X^2] - 3*mu^4
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
    rule = "gamma2 = (E[X^4] - 4*mu*E[X^3] + 6*mu^2*E[X^2] - 3*mu^4) / sigma^4 - 3",
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

#' @title Convert Expression to LaTeX
#' @description Converts an R expression tree to publication-ready LaTeX code.
#'   Handles standard mathematical operators, Greek letters, and statistical
#'   functions like E(), Var(), and Cov().
#' @param x An R expression, call, or the result of E(), Var(), etc.
#'   Can also be a character string representation of an expression.
#' @param delimiters Character; type of math delimiters to add.
#'   Options: "none" (default), "inline" ($...$), "display" (\\[...\\])
#' @param ... Additional arguments (for S3 method dispatch)
#' @return A latex_output object containing LaTeX code (prints prettily)
#' @export
#' @examples
#' to.latex(E(a * X + b))
#' to.latex(Var(X), delimiters = "inline")
#' to.latex(quote(sigma^2 + mu^2), delimiters = "display")
to.latex <- function(x, delimiters = "none", ...) {
  UseMethod("to.latex")
}


#' @title Default LaTeX Conversion
#' @description Converts R expressions to LaTeX using expression tree traversal.
#' @param x An R expression or call object
#' @param delimiters Character; "none", "inline" ($), or "display" (\\[\\])
#' @param ... Additional arguments (ignored)
#' @return A latex_output object containing LaTeX code
#' @export
#' @method to.latex default
to.latex.default <- function(x, delimiters = "none", ...) {
  # Handle character input - parse it first
  if (is.character(x)) {
    x <- parse(text = x)[[1]]
  }
  
  latex_code <- expr.to.latex(x)
  
  # Apply delimiters
  wrapped <- switch(delimiters,
    "inline" = paste0("$", latex_code, "$"),
    "display" = paste0("\\[\n  ", latex_code, "\n\\]"),
    latex_code  # "none" or default
  )
  
  # Return as latex_output class for pretty printing
  structure(wrapped, class = "latex_output")
}


#' @title Print LaTeX Output
#' @description Pretty prints LaTeX code with syntax highlighting intent.
#' @param x A latex_output object
#' @param ... Additional arguments (ignored)
#' @export
#' @method print latex_output
print.latex_output <- function(x, ...) {
  cat("LaTeX:\n")
  cat(x, "\n")
  invisible(x)
}




#' @title Convert Derivation to LaTeX
#' @description Converts each step of a derivation object to LaTeX format,
#'   suitable for including in homework assignments or papers.
#' @param x A derivation object (from derive.E, derive.Var, etc.)
#' @param delimiters Character; type of math delimiters (ignored for derivations)
#' @param align Logical; if TRUE, returns an align environment for step-by-step
#' @param ... Additional arguments (ignored)
#' @return A character string containing LaTeX code
#' @export
#' @method to.latex derivation
to.latex.derivation <- function(x, delimiters = "none", align = TRUE, ...) {

  if (align) {
    # Build an align environment showing each step
    lines <- character(0)
    
    for (i in seq_along(x$steps)) {
      step <- x$steps[[i]]
      # Parse and convert expression
      expr <- parse(text = step$expression)[[1]]
      latex_expr <- expr.to.latex(expr)
      
      if (i == 1) {
        lines <- c(lines, paste0("&= ", latex_expr, " && \\text{(", step$description, ")}"))
      } else {
        lines <- c(lines, paste0("&= ", latex_expr, " && \\text{(", step$description, ")}"))
      }
    }
    
    result <- paste0(
      "\\begin{align*}\n",
      "  ", to.latex(x$input), " ", 
      paste(lines, collapse = " \\\\\n  "),
      "\n\\end{align*}"
    )
    return(result)
  } else {
    # Simple: just return final result
    return(expr.to.latex(x$result))
  }
}


# ============================================================================
# INTERNAL HELPERS
# ============================================================================

# Greek letter mapping
.greek_letters <- c(
  "alpha" = "\\alpha",
  "beta" = "\\beta",
  "gamma" = "\\gamma",
  "delta" = "\\delta",
  "epsilon" = "\\epsilon",
  "zeta" = "\\zeta",
  "eta" = "\\eta",
  "theta" = "\\theta",
  "iota" = "\\iota",
  "kappa" = "\\kappa",
  "lambda" = "\\lambda",
  "mu" = "\\mu",
  "nu" = "\\nu",
  "xi" = "\\xi",
  "pi" = "\\pi",
  "rho" = "\\rho",
  "sigma" = "\\sigma",
  "tau" = "\\tau",
  "upsilon" = "\\upsilon",
  "phi" = "\\phi",
  "chi" = "\\chi",
  "psi" = "\\psi",
  "omega" = "\\omega",
  # Uppercase
  "Alpha" = "A",
  "Beta" = "B",
  "Gamma" = "\\Gamma",
  "Delta" = "\\Delta",
  "Epsilon" = "E",
  "Zeta" = "Z",
  "Eta" = "H",
  "Theta" = "\\Theta",
  "Iota" = "I",
  "Kappa" = "K",
  "Lambda" = "\\Lambda",
  "Mu" = "M",
  "Nu" = "N",
  "Xi" = "\\Xi",
  "Pi" = "\\Pi",
  "Rho" = "P",
  "Sigma" = "\\Sigma",
  "Tau" = "T",
  "Upsilon" = "\\Upsilon",
  "Phi" = "\\Phi",
  "Chi" = "X",
  "Psi" = "\\Psi",
  "Omega" = "\\Omega"
)


#' @title Convert Symbol to LaTeX
#' @description Handles Greek letter conversion and subscript notation.
#' @param name Character name of the symbol
#' @return LaTeX representation
#' @keywords internal
symbol.to.latex <- function(name) {
  # Check for subscript pattern (e.g., X_1, mu_i)
  if (grepl("_", name)) {
    parts <- strsplit(name, "_")[[1]]
    base <- symbol.to.latex(parts[1])
    subscript <- paste(parts[-1], collapse = "_")
    return(paste0(base, "_{", subscript, "}"))
  }
  
  # Check for Greek letter
  if (name %in% names(.greek_letters)) {
    return(unname(.greek_letters[name]))
  }
  
  # Return as-is
  return(name)
}


#' @title Expression to LaTeX (Recursive)
#' @description Recursively converts an R expression tree to LaTeX.
#' @param expr An R expression
#' @return LaTeX string
#' @keywords internal
expr.to.latex <- function(expr) {
  # Numeric
  if (is.numeric(expr)) {
    return(as.character(expr))
  }
  
  # Symbol
  if (is.symbol(expr)) {
    return(symbol.to.latex(as.character(expr)))
  }
  
  # Not a call - coerce to character
  if (!is.call(expr)) {
    return(as.character(expr))
  }
  
  # It's a call - handle operators and functions
  op <- as.character(expr[[1]])
  
  # Statistical functions
  if (op == "E") {
    inner <- expr.to.latex(expr[[2]])
    return(paste0("\\mathbb{E}[", inner, "]"))
  }
  
  if (op == "Var") {
    inner <- expr.to.latex(expr[[2]])
    return(paste0("\\text{Var}(", inner, ")"))
  }
  
  if (op == "Cov") {
    x <- expr.to.latex(expr[[2]])
    y <- expr.to.latex(expr[[3]])
    return(paste0("\\text{Cov}(", x, ", ", y, ")"))
  }
  
  # Addition
  if (op == "+") {
    if (length(expr) == 2) {
      # Unary plus
      return(expr.to.latex(expr[[2]]))
    }
    left <- expr.to.latex(expr[[2]])
    right <- expr.to.latex(expr[[3]])
    return(paste0(left, " + ", right))
  }
  
  # Subtraction
  if (op == "-") {
    if (length(expr) == 2) {
      # Unary minus
      inner <- expr.to.latex(expr[[2]])
      # Add parens if inner is complex
      if (is.call(expr[[2]]) && as.character(expr[[2]][[1]]) %in% c("+", "-")) {
        return(paste0("-\\left(", inner, "\\right)"))
      }
      return(paste0("-", inner))
    }
    left <- expr.to.latex(expr[[2]])
    right <- expr.to.latex(expr[[3]])
    # Add parens around right if it's an addition/subtraction
    if (is.call(expr[[3]]) && as.character(expr[[3]][[1]]) %in% c("+", "-")) {
      return(paste0(left, " - \\left(", right, "\\right)"))
    }
    return(paste0(left, " - ", right))
  }
  
  # Multiplication - use implicit multiplication for cleanliness
  if (op == "*") {
    left <- expr[[2]]
    right <- expr[[3]]
    left_latex <- expr.to.latex(left)
    right_latex <- expr.to.latex(right)
    
    # Parenthesize if needed (for addition/subtraction)
    if (is.call(left) && as.character(left[[1]]) %in% c("+", "-")) {
      left_latex <- paste0("\\left(", left_latex, "\\right)")
    }
    if (is.call(right) && as.character(right[[1]]) %in% c("+", "-")) {
      right_latex <- paste0("\\left(", right_latex, "\\right)")
    }
    
    # Use space for implicit multiplication between symbols/functions
    # Use \cdot only for number * number
    if (is.numeric(left) && is.numeric(right)) {
      return(paste0(left_latex, " \\cdot ", right_latex))
    }
    
    # Number * symbol: just concatenate (e.g., 2x not 2 x)
    if (is.numeric(left)) {
      return(paste0(left_latex, " ", right_latex))
    }
    
    # Otherwise: use space for readability
    return(paste0(left_latex, " ", right_latex))
  }
  
  # Division - use frac
  if (op == "/") {
    num <- expr.to.latex(expr[[2]])
    denom <- expr.to.latex(expr[[3]])
    return(paste0("\\frac{", num, "}{", denom, "}"))
  }
  
  # Exponentiation
  if (op == "^") {
    base <- expr[[2]]
    exp <- expr[[3]]
    base_latex <- expr.to.latex(base)
    exp_latex <- expr.to.latex(exp)
    
    # Parenthesize base if it's a complex expression
    if (is.call(base) && !(as.character(base[[1]]) %in% c("E", "Var", "Cov"))) {
      base_latex <- paste0("\\left(", base_latex, "\\right)")
    }
    
    return(paste0(base_latex, "^{", exp_latex, "}"))
  }
  
  # Parentheses
  if (op == "(") {
    inner <- expr.to.latex(expr[[2]])
    return(paste0("\\left(", inner, "\\right)"))
  }
  
  # Square root
  if (op == "sqrt") {
    inner <- expr.to.latex(expr[[2]])
    return(paste0("\\sqrt{", inner, "}"))
  }
  
  # Exponential
  if (op == "exp") {
    inner <- expr.to.latex(expr[[2]])
    return(paste0("e^{", inner, "}"))
  }
  
  # Natural log
  if (op == "log") {
    inner <- expr.to.latex(expr[[2]])
    return(paste0("\\ln(", inner, ")"))
  }
  
  # Gamma function
  if (op == "gamma") {
    inner <- expr.to.latex(expr[[2]])
    return(paste0("\\Gamma\\left(", inner, "\\right)"))
  }
  
  # Generic function call
  args <- sapply(as.list(expr)[-1], expr.to.latex)
  return(paste0("\\text{", op, "}(", paste(args, collapse = ", "), ")"))
}

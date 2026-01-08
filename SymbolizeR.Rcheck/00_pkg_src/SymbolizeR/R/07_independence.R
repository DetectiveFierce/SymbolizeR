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

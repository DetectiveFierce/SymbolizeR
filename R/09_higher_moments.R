# ============================================================================
# HIGHER-ORDER MOMENTS (3rd and 4th)
# ============================================================================

#' @title Get nth Moment for Known Distribution
#' @description Returns the symbolic nth raw moment E(X^n) if the variable has
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

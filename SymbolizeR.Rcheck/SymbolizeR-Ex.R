pkgname <- "SymbolizeR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('SymbolizeR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Cov")
### * Cov

flush(stderr()); flush(stdout())

### Name: Cov
### Title: Symbolic Covariance
### Aliases: Cov

### ** Examples

Cov(X, Y)      # Returns: E(X * Y) - E(X) * E(Y)



cleanEx()
nameEx("E")
### * E

flush(stderr()); flush(stdout())

### Name: E
### Title: Symbolic Expectation
### Aliases: E

### ** Examples

E(X)           # Returns: E(X)
E(a * X)       # Returns: a * E(X)
E(X + Y)       # Returns: E(X) + E(Y)
E(2 * X + 3)   # Returns: 2 * E(X) + 3



cleanEx()
nameEx("Kurtosis")
### * Kurtosis

flush(stderr()); flush(stdout())

### Name: Kurtosis
### Title: Symbolic Kurtosis
### Aliases: Kurtosis

### ** Examples

Kurtosis(X)  # Returns symbolic excess kurtosis formula

# With defined distribution
define(X ~ Normal(mu, sigma))
Kurtosis(X)  # Returns 0 (normal has excess kurtosis 0)



cleanEx()
nameEx("Skewness")
### * Skewness

flush(stderr()); flush(stdout())

### Name: Skewness
### Title: Symbolic Skewness
### Aliases: Skewness

### ** Examples

Skewness(X)  # Returns symbolic skewness formula

# With defined distribution
define(X ~ Normal(mu, sigma))
Skewness(X)  # Returns 0 (normal is symmetric)



cleanEx()
nameEx("Var")
### * Var

flush(stderr()); flush(stdout())

### Name: Var
### Title: Symbolic Variance
### Aliases: Var

### ** Examples

Var(X)         # Returns: E(X^2) - E(X)^2
Var(a * X)     # Returns: a^2 * (E(X^2) - E(X)^2)



cleanEx()
nameEx("assume.independent")
### * assume.independent

flush(stderr()); flush(stdout())

### Name: assume.independent
### Title: Declare Independent Random Variables
### Aliases: assume.independent

### ** Examples

assume.independent(X, Y)
E(X * Y)  # Returns: E(X) * E(Y)

assume.independent(X, Y, Z)  # All three are mutually independent
E(X * Y * Z)  # Returns: E(X) * E(Y) * E(Z)



cleanEx()
nameEx("clear.definitions")
### * clear.definitions

flush(stderr()); flush(stdout())

### Name: clear.definitions
### Title: Clear Variable Definitions
### Aliases: clear.definitions

### ** Examples

define(X ~ Normal(mu, sigma))
clear.definitions()
E(X)   # Returns: E(X) (no longer has distribution info)



cleanEx()
nameEx("clear.independence")
### * clear.independence

flush(stderr()); flush(stdout())

### Name: clear.independence
### Title: Clear Independence Assumptions
### Aliases: clear.independence

### ** Examples

assume.independent(X, Y)
clear.independence()
E(X * Y)  # Returns: E(X * Y) (no longer factors)



cleanEx()
nameEx("define")
### * define

flush(stderr()); flush(stdout())

### Name: define
### Title: Define Random Variable Distribution
### Aliases: define

### ** Examples

define(X ~ Normal(mu, sigma))
E(X)   # Returns: mu (instead of E(X))
E(X^2) # Returns: sigma^2 + mu^2



cleanEx()
nameEx("deriv.sym")
### * deriv.sym

flush(stderr()); flush(stdout())

### Name: deriv.sym
### Title: Symbolic Differentiation
### Aliases: deriv.sym

### ** Examples

deriv.sym(x^3, x)             # 3 * x^2
deriv.sym(exp(a * x), x)      # a * exp(a * x)
deriv.sym(x * E(Y), x)        # E(Y) (treats E(Y) as constant)



cleanEx()
nameEx("derive.Cov")
### * derive.Cov

flush(stderr()); flush(stdout())

### Name: derive.Cov
### Title: Derive Covariance Step-by-Step
### Aliases: derive.Cov

### ** Examples

derive.Cov(X, Y)



cleanEx()
nameEx("derive.E")
### * derive.E

flush(stderr()); flush(stdout())

### Name: derive.E
### Title: Derive Expectation Step-by-Step
### Aliases: derive.E

### ** Examples

derive.E(a * X + b)
derive.E(X + Y)



cleanEx()
nameEx("derive.Kurtosis")
### * derive.Kurtosis

flush(stderr()); flush(stdout())

### Name: derive.Kurtosis
### Title: Derive Kurtosis Step-by-Step
### Aliases: derive.Kurtosis

### ** Examples

derive.Kurtosis(X)



cleanEx()
nameEx("derive.Skewness")
### * derive.Skewness

flush(stderr()); flush(stdout())

### Name: derive.Skewness
### Title: Derive Skewness Step-by-Step
### Aliases: derive.Skewness

### ** Examples

derive.Skewness(X)



cleanEx()
nameEx("derive.Var")
### * derive.Var

flush(stderr()); flush(stdout())

### Name: derive.Var
### Title: Derive Variance Step-by-Step
### Aliases: derive.Var

### ** Examples

derive.Var(X)
derive.Var(a * X + b)



cleanEx()
nameEx("integrate.sym")
### * integrate.sym

flush(stderr()); flush(stdout())

### Name: integrate.sym
### Title: Symbolic Definite Integration
### Aliases: integrate.sym

### ** Examples

# Gamma kernel: integral of x^2 * exp(-x) from 0 to Inf = Gamma(3) = 2
integrate.sym(x^2 * exp(-x), x, 0, Inf)

# Gaussian kernel: integral of exp(-x^2) from -Inf to Inf = sqrt(pi)
integrate.sym(exp(-x^2), x, -Inf, Inf)

# Beta kernel: integral of x * (1-x) from 0 to 1 = Beta(2, 2) = 1/6
integrate.sym(x * (1-x), x, 0, 1)



cleanEx()
nameEx("moment")
### * moment

flush(stderr()); flush(stdout())

### Name: moment
### Title: Compute nth Raw Moment
### Aliases: moment

### ** Examples

define(X ~ Normal(mu, sigma))
moment(X, 1)  # Returns: mu
moment(X, 2)  # Returns: sigma^2 + mu^2

# For undefined variables
moment(Y, 3)  # Returns: E(Y^3)



cleanEx()
nameEx("show.independence")
### * show.independence

flush(stderr()); flush(stdout())

### Name: show.independence
### Title: Show Independence Assumptions
### Aliases: show.independence

### ** Examples

assume.independent(X, Y)
assume.independent(A, B, C)
show.independence()



cleanEx()
nameEx("to.latex")
### * to.latex

flush(stderr()); flush(stdout())

### Name: to.latex
### Title: Convert Expression to LaTeX
### Aliases: to.latex

### ** Examples

to.latex(E(a * X + b))
to.latex(Var(X), delimiters = "inline")
to.latex(quote(sigma^2 + mu^2), delimiters = "display")



cleanEx()
nameEx("undefine")
### * undefine

flush(stderr()); flush(stdout())

### Name: undefine
### Title: Undefine a Variable
### Aliases: undefine

### ** Examples

define(X ~ Normal(mu, sigma))
undefine("X")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

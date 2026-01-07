
library(testthat)

files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in files) {
  tryCatch(source(f), error=function(e) NULL)
}

try({
    print("Defining X ~ Normal(0, 1)")
    # Since I cannot easily replicate the environment injection of 'define' without running the package as intended,
    # I will manually populate the moments if needed or hope 'define' works if it writes to global env.
    
    # Check if define exists
    if (exists("define")) {
        define(X ~ Normal(0, 1))
    } else {
        print("define function not found. mocking moments.")
        # If define logic is hidden, I might trigger the fallback path in Skewness unless I mock ensure.expression or something.
        # Actually Skewness checks get.nth.moment.
        # I need to mock get.nth.moment to return 0, 1, 0, 3
        
        assign("get.nth.moment", function(var, n) {
            if (n==1) return(0)
            if (n==2) return(1)
            if (n==3) return(0)
            if (n==4) return(3)
            return(NULL)
        }, envir = globalenv())
    }

    print("Calculating Skewness(X)")
    res <- Skewness(X)
    
    print("Skewness Result print:")
    print(res)
    print(paste("Is List?", is.list(res)))
    print(paste("Class?", paste(class(res), collapse=", ")))

    print("Calculating Var(X)")
    # Var uses Expectation logic or similar moment usage
    res_var <- Var(X)
    print("Var Result print:")
    print(res_var)
})

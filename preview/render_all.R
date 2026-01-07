#!/usr/bin/env Rscript

# Script to render all SymbolizeR documentation Rmds
library(rmarkdown)

# List all Rmd files in the current directory
rmd_files <- list.files(".", pattern = "\\.Rmd$", full.names = TRUE)

message("Found Rmd files: ", paste(basename(rmd_files), collapse = ", "))

# Render each file
for (f in rmd_files) {
    message("Rendering: ", f)
    # Using load_all("..") because we are in the preview/ directory
    tryCatch({
        pkgload::load_all("..")
        render(f, envir = new.env())
    }, error = function(e) {
        message("Error rendering ", f, ": ", e$message)
    })
}

message("Done!")

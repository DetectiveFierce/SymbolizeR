# =============================================================================
# Generate Function Reference Pages
# Creates HTML documentation pages from Rd files for the preview site
# =============================================================================

library(tools)

# Configuration
man_dir <- "../man"
output_dir <- "reference"
template_path <- "template.html"

# Function categories (based on source file prefixes)
categories <- list(
  "Core Functions" = c("E", "Var", "Cov"),
  "Higher-Order Statistics" = c("Skewness", "Kurtosis"),
  "Step-by-Step Derivations" = c("derive.E", "derive.Var", "derive.Cov", 
                                  "derive.Skewness", "derive.Kurtosis"),
  "Distributions" = c("define", "undefine", "clear.definitions"),
  "Independence" = c("assume.independent", "clear.independence", "show.independence"),
  "Moments" = c("moment"),
  "Calculus" = c("deriv.sym", "integrate.sym"),
  "LaTeX Output" = c("to.latex")
)

# Exported functions (from NAMESPACE)
exported <- c("Cov", "E", "Kurtosis", "Skewness", "Var", 
              "assume.independent", "clear.definitions", "clear.independence",
              "define", "deriv.sym", "derive.Cov", "derive.E", "derive.Kurtosis",
              "derive.Skewness", "derive.Var", "integrate.sym", "moment",
              "show.independence", "to.latex", "undefine")

# =============================================================================
# Rd Parsing Functions
# =============================================================================

parse_rd_file <- function(rd_path) {
  rd <- parse_Rd(rd_path)
  
  result <- list(
    name = "",
    title = "",
    description = "",
    usage = "",
    arguments = list(),
    value = "",
    examples = ""
  )
  
  for (element in rd) {
    tag <- attr(element, "Rd_tag")
    
    if (tag == "\\name") {
      result$name <- paste(unlist(element), collapse = "")
    } else if (tag == "\\title") {
      result$title <- paste(unlist(element), collapse = "")
    } else if (tag == "\\description") {
      result$description <- rd_to_text(element)
    } else if (tag == "\\usage") {
      result$usage <- rd_to_code(element)
    } else if (tag == "\\arguments") {
      result$arguments <- parse_arguments(element)
    } else if (tag == "\\value") {
      result$value <- rd_to_text(element)
    } else if (tag == "\\examples") {
      result$examples <- rd_to_code(element)
    }
  }
  
  result
}

rd_to_text <- function(rd_element) {
  text <- ""
  for (item in rd_element) {
    tag <- attr(item, "Rd_tag")
    if (is.null(tag) || tag == "TEXT" || tag == "RCODE") {
      text <- paste0(text, paste(unlist(item), collapse = ""))
    } else if (tag == "\\code") {
      text <- paste0(text, "<code>", paste(unlist(item), collapse = ""), "</code>")
    } else if (tag == "\\link") {
      text <- paste0(text, paste(unlist(item), collapse = ""))
    }
  }
  # Clean up whitespace
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

rd_to_code <- function(rd_element) {
  code <- paste(unlist(rd_element), collapse = "")
  # Clean up leading/trailing whitespace
  trimws(code)
}

parse_arguments <- function(args_element) {
  args <- list()
  for (item in args_element) {
    tag <- attr(item, "Rd_tag")
    if (tag == "\\item") {
      if (length(item) >= 2) {
        arg_name <- paste(unlist(item[[1]]), collapse = "")
        arg_desc <- rd_to_text(item[-1])
        args[[arg_name]] <- arg_desc
      }
    }
  }
  args
}

# =============================================================================
# HTML Generation
# =============================================================================

escape_html <- function(text) {
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text
}

generate_function_page <- function(func_info, category) {
  html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>%s - SymbolizeR</title>
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap" rel="stylesheet">
    <link rel="stylesheet" href="../dist/output.css">
</head>
<body>
    <header class="site-header">
        <button class="mobile-menu-btn" aria-label="Toggle Menu">
            <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                <line x1="3" y1="12" x2="21" y2="12"></line>
                <line x1="3" y1="6" x2="21" y2="6"></line>
                <line x1="3" y1="18" x2="21" y2="18"></line>
            </svg>
        </button>
        <a href="../index.html" class="header-logo">
            <svg viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="1.5">
                <circle cx="16" cy="16" r="12" stroke-dasharray="4 2"/>
                <path d="M11 16c0-6 5-10 5-10s5 4 5 10-5 10-5 10-5-4-5-10z"/>
                <circle cx="16" cy="16" r="3" fill="currentColor"/>
            </svg>
            SymbolizeR
        </a>
        <nav class="header-nav">
            <a href="../index.html">Docs</a>
            <a href="index.html">Reference</a>
        </nav>
    </header>

    <div class="sidebar-overlay"></div>

    <div class="docs-layout">
        <aside class="sidebar">
            <div class="sidebar-section">
                <div class="sidebar-heading">Reference</div>
                <ul class="sidebar-nav">
                    <li><a href="index.html">← All Functions</a></li>
                </ul>
            </div>
            <div class="sidebar-section">
                <div class="sidebar-heading">%s</div>
            </div>
        </aside>

        <main class="main-content">
            <article class="content-article">
                <div class="function-header">
                    <h1 class="title">%s()</h1>
                    <span class="function-category">%s</span>
                </div>
                <p class="function-description">%s</p>
                
                <h2 id="usage">Usage</h2>
                <pre class="sourceCode r"><code>%s</code></pre>
', 
    func_info$name, category, func_info$name, category,
    func_info$description, escape_html(func_info$usage))

  # Arguments section
  if (length(func_info$arguments) > 0) {
    html <- paste0(html, '
                <h2 id="arguments">Arguments</h2>
                <table class="arguments-table">
                <thead><tr><th>Argument</th><th>Description</th></tr></thead>
                <tbody>
')
    for (arg_name in names(func_info$arguments)) {
      html <- paste0(html, sprintf('                <tr><td><code>%s</code></td><td>%s</td></tr>\n',
                                   escape_html(arg_name), func_info$arguments[[arg_name]]))
    }
    html <- paste0(html, '                </tbody></table>\n')
  }
  
  # Value section
  if (nchar(func_info$value) > 0) {
    html <- paste0(html, sprintf('
                <h2 id="value">Value</h2>
                <p>%s</p>
', func_info$value))
  }
  
  # Examples section
  if (nchar(func_info$examples) > 0) {
    html <- paste0(html, sprintf('
                <h2 id="examples">Examples</h2>
                <pre class="sourceCode r"><code>%s</code></pre>
', escape_html(func_info$examples)))
  }
  
  html <- paste0(html, '
            </article>
        </main>

        <aside class="toc-sidebar">
            <nav class="toc-container">
                <div class="toc-heading">On This Page</div>
                <ul class="toc-list">
                    <li><a href="#usage">Usage</a></li>
')
  
  if (length(func_info$arguments) > 0) {
    html <- paste0(html, '                    <li><a href="#arguments">Arguments</a></li>\n')
  }
  if (nchar(func_info$value) > 0) {
    html <- paste0(html, '                    <li><a href="#value">Value</a></li>\n')
  }
  if (nchar(func_info$examples) > 0) {
    html <- paste0(html, '                    <li><a href="#examples">Examples</a></li>\n')
  }

  html <- paste0(html, '                </ul>
            </nav>
        </aside>
    </div>

    <script src="../layout.js"></script>
</body>
</html>
')
  
  html
}

generate_index_page <- function(functions_by_category) {
  html <- '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Function Reference - SymbolizeR</title>
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap" rel="stylesheet">
    <link rel="stylesheet" href="../dist/output.css">
    <style>
        .search-container { margin-bottom: 2rem; }
        .search-input {
            width: 100%;
            padding: 0.75rem 1rem;
            font-size: 1rem;
            border: 1px solid var(--border);
            border-radius: 0.5rem;
            background: var(--bg-secondary);
            color: var(--fg-primary);
        }
        .search-input:focus {
            outline: none;
            border-color: var(--accent);
        }
        .function-grid {
            display: grid;
            gap: 0.5rem;
        }
        .function-item {
            display: flex;
            align-items: center;
            gap: 1rem;
            padding: 0.75rem 1rem;
            background: var(--bg-secondary);
            border-radius: 0.5rem;
            text-decoration: none;
            color: var(--fg-primary);
            transition: background 0.15s;
        }
        .function-item:hover {
            background: var(--bg-tertiary);
        }
        .function-item code {
            font-family: "JetBrains Mono", monospace;
            color: var(--accent);
            font-weight: 500;
        }
        .function-item .desc {
            color: var(--fg-secondary);
            font-size: 0.875rem;
        }
        .category-section { margin-bottom: 2rem; }
        .category-section h2 { 
            margin-bottom: 1rem;
            padding-bottom: 0.5rem;
            border-bottom: 1px solid var(--border);
        }
        .hidden { display: none !important; }
    </style>
</head>
<body>
    <header class="site-header">
        <button class="mobile-menu-btn" aria-label="Toggle Menu">
            <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                <line x1="3" y1="12" x2="21" y2="12"></line>
                <line x1="3" y1="6" x2="21" y2="6"></line>
                <line x1="3" y1="18" x2="21" y2="18"></line>
            </svg>
        </button>
        <a href="../index.html" class="header-logo">
            <svg viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="1.5">
                <circle cx="16" cy="16" r="12" stroke-dasharray="4 2"/>
                <path d="M11 16c0-6 5-10 5-10s5 4 5 10-5 10-5 10-5-4-5-10z"/>
                <circle cx="16" cy="16" r="3" fill="currentColor"/>
            </svg>
            SymbolizeR
        </a>
        <nav class="header-nav">
            <a href="../index.html">Docs</a>
            <a href="index.html" class="active">Reference</a>
        </nav>
    </header>

    <div class="sidebar-overlay"></div>

    <div class="docs-layout">
        <aside class="sidebar">
            <div class="sidebar-section">
                <div class="sidebar-heading">Documentation</div>
                <ul class="sidebar-nav">
                    <li><a href="../getting-started.html">Getting Started</a></li>
                    <li><a href="../distributions.html">Distributions</a></li>
                    <li><a href="../derivations.html">Derivations</a></li>
                    <li><a href="../moments.html">Moments & Statistics</a></li>
                    <li><a href="../calculus.html">Calculus</a></li>
                    <li><a href="../internals.html">Package Internals</a></li>
                </ul>
            </div>
            <div class="sidebar-section">
                <div class="sidebar-heading">Reference</div>
                <ul class="sidebar-nav">
                    <li><a href="index.html" class="active">All Functions</a></li>
                </ul>
            </div>
        </aside>

        <main class="main-content">
            <article class="content-article">
                <h1 class="title">Function Reference</h1>
                <p>Complete documentation for all exported functions in SymbolizeR.</p>

                <div class="search-container">
                    <input type="text" class="search-input" id="function-search" 
                           placeholder="Search functions..." autocomplete="off">
                </div>
'

  for (category in names(functions_by_category)) {
    funcs <- functions_by_category[[category]]
    if (length(funcs) == 0) next
    
    html <- paste0(html, sprintf('
                <div class="category-section" data-category="%s">
                    <h2>%s</h2>
                    <div class="function-grid">
', category, category))
    
    for (func in funcs) {
      html <- paste0(html, sprintf('
                        <a href="%s.html" class="function-item" data-name="%s" data-desc="%s">
                            <code>%s()</code>
                            <span class="desc">%s</span>
                        </a>
', func$name, tolower(func$name), tolower(func$title), func$name, func$title))
    }
    
    html <- paste0(html, '                    </div>
                </div>
')
  }

  html <- paste0(html, '
            </article>
        </main>
    </div>

    <script src="../layout.js"></script>
    <script>
    (function() {
        const searchInput = document.getElementById("function-search");
        const items = document.querySelectorAll(".function-item");
        const sections = document.querySelectorAll(".category-section");
        
        searchInput.addEventListener("input", function() {
            const query = this.value.toLowerCase().trim();
            
            items.forEach(item => {
                const name = item.dataset.name;
                const desc = item.dataset.desc;
                const matches = name.includes(query) || desc.includes(query);
                item.classList.toggle("hidden", !matches && query.length > 0);
            });
            
            // Hide empty sections
            sections.forEach(section => {
                const visibleItems = section.querySelectorAll(".function-item:not(.hidden)");
                section.classList.toggle("hidden", visibleItems.length === 0 && query.length > 0);
            });
        });
        
        // Focus search on Ctrl+K
        document.addEventListener("keydown", function(e) {
            if ((e.ctrlKey || e.metaKey) && e.key === "k") {
                e.preventDefault();
                searchInput.focus();
                searchInput.select();
            }
        });
    })();
    </script>
</body>
</html>
')

  html
}

# =============================================================================
# Main Execution
# =============================================================================

cat("Generating function reference pages...\n")

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Parse all Rd files for exported functions
functions_by_category <- list()
for (cat in names(categories)) {
  functions_by_category[[cat]] <- list()
}

for (func_name in exported) {
  rd_file <- file.path(man_dir, paste0(func_name, ".Rd"))
  
  if (!file.exists(rd_file)) {
    cat("  Warning: No Rd file for", func_name, "\n")
    next
  }
  
  cat("  Parsing", func_name, "...\n")
  func_info <- parse_rd_file(rd_file)
  
  # Find category
  func_category <- "Other"
  for (cat in names(categories)) {
    if (func_name %in% categories[[cat]]) {
      func_category <- cat
      break
    }
  }
  
  # Add to category
  if (is.null(functions_by_category[[func_category]])) {
    functions_by_category[[func_category]] <- list()
  }
  functions_by_category[[func_category]][[length(functions_by_category[[func_category]]) + 1]] <- func_info
  
  # Generate individual page
  html <- generate_function_page(func_info, func_category)
  output_file <- file.path(output_dir, paste0(func_name, ".html"))
  writeLines(html, output_file)
  cat("    Generated", output_file, "\n")
}

# Generate index page
cat("  Generating index page...\n")
index_html <- generate_index_page(functions_by_category)
writeLines(index_html, file.path(output_dir, "index.html"))

cat("\nDone! Generated", length(exported), "function pages.\n")

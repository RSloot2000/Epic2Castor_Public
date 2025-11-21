local_lib <- file.path(getwd(), "Rlibs")
if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE)
.libPaths(local_lib)
options(install.packages.compile.from.source = "never")
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(
  help_type = "html",
  max.print = 1000,
  digits = 4,
  warn = 1       
)
options(error = NULL) 
if (interactive()) {
  suppressWarnings({
    if (requireNamespace("magrittr", quietly = TRUE)) {
      library(magrittr)
    }
  })
}
options(prompt = "R> ", continue = "+ ")
options(crayon.enabled = TRUE)
options(stringsAsFactors = FALSE)
.First <- function() {
  cat("Project Loaded:", getwd(), "\n")
}
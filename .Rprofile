# ===== APP-V / RADBOUDUMC VM DETECTION =====
# Detect App-V virtualized environment by checking R's library path
# Phase 1 (.Rprofile): Set .libPaths() from saved config (fast, no user interaction)
# Phase 2 (.First):    Prompt for z-number if no saved config
local({
  is_appv <- any(grepl("App-V|ProgramData.*Root.*VFS", .libPaths(), ignore.case = TRUE))
  
  if (is_appv) {
    net_lib_config <- file.path("config", ".net_lib_path")
    
    if (file.exists(net_lib_config)) {
      net_lib <- trimws(readLines(net_lib_config, warn = FALSE)[1])
      if (nzchar(net_lib) && dir.exists(net_lib)) {
        .libPaths(c(net_lib, .libPaths()))
      }
    }
  }
  
  # Store detection result for .First to use
  .GlobalEnv$.epic2castor_appv <- is_appv
})
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
  
  # ===== APP-V PHASE 2: Interactive setup (rstudioapi is now available) =====
  is_appv <- isTRUE(.GlobalEnv$.epic2castor_appv)
  rm(".epic2castor_appv", envir = .GlobalEnv, inherits = FALSE)
  
  if (!is_appv) return(invisible())
  
  r_ver <- paste(R.version$major, sub("\\..*", "", R.version$minor), sep = ".")
  net_lib_config <- file.path("config", ".net_lib_path")
  
  # Check if already configured
  net_lib <- NULL
  if (file.exists(net_lib_config)) {
    net_lib <- trimws(readLines(net_lib_config, warn = FALSE)[1])
    if (!nzchar(net_lib) || !dir.exists(net_lib)) net_lib <- NULL
  }
  
  # Show status banner
  if (!is.null(net_lib)) {
    cat("\n")
    cat("  \033[44;97m                                                        \033[0m\n")
    cat("  \033[44;97m   App-V environment detected                           \033[0m\n")
    cat("  \033[44;97m   R packages are loaded from the network library:       \033[0m\n")
    cat(sprintf("  \033[44;97m   %-52s \033[0m\n", net_lib))
    cat("  \033[44;97m                                                        \033[0m\n")
    cat("\n")
    return(invisible())
  }
  
  # No saved config — prompt for z-number
  max_attempts <- 3
  
  for (attempt in seq_len(max_attempts)) {
    if (attempt == 1) {
      cat("\n")
      cat("============================================================\n")
      cat("  RADBOUDUMC APP-V ENVIRONMENT DETECTED\n")
      cat("============================================================\n")
      cat("  R packages with DLLs (e.g. Shiny) only work from the\n")
      cat("  Radboudumc network library.\n\n")
    } else {
      cat("  Z-numbers did not match. Please try again.\n\n")
    }
    cat("  Enter your z-number (e.g. z123456): ")
    utils::flush.console()
    z_number <- readline()
    if (!nzchar(trimws(z_number))) return(invisible())
    cat("  Confirm your z-number: ")
    utils::flush.console()
    z_confirm <- readline()
    
    # Check match
    if (tolower(trimws(z_confirm)) == tolower(trimws(z_number))) {
      z_number <- tolower(trimws(z_number))
      if (!grepl("^z", z_number)) z_number <- paste0("z", z_number)
      
      net_lib <- paste0("//umcn.nl/nas/APP/APPDATA/", z_number, "/R/win-library/", r_ver)
      
      # Create directory if needed
      if (!dir.exists(net_lib)) {
        cat(sprintf("  Creating network library: %s ...\n", net_lib))
        result <- tryCatch({
          system(sprintf('powershell.exe -command "mkdir \\\\umcn.nl/nas/APP/APPDATA/%s/R/win-library/%s"',
                         z_number, r_ver), intern = TRUE, invisible = TRUE)
          dir.exists(net_lib)
        }, error = function(e) FALSE)
        
        if (!isTRUE(result)) {
          cat("  WARNING: Could not create network library.\n")
          cat("  Please contact the ServiceDesk via TopDesk.\n")
          return(invisible())
        }
        cat("  Network library created successfully!\n")
      } else {
        cat(sprintf("  Using existing network library: %s ...\n", net_lib))
      }
      
      # Save path for future sessions
      tryCatch({
        if (!dir.exists("config")) dir.create("config", recursive = TRUE)
        writeLines(net_lib, net_lib_config)
      }, error = function(e) NULL)
      
      # Set library path
      .libPaths(c(net_lib, .libPaths()))
      
      cat("\n")
      cat("  \033[44;97m                                                        \033[0m\n")
      cat("  \033[44;97m   App-V environment detected                           \033[0m\n")
      cat("  \033[44;97m   R packages are loaded from the network library:       \033[0m\n")
      cat(sprintf("  \033[44;97m   %-52s \033[0m\n", net_lib))
      cat("  \033[44;97m                                                        \033[0m\n")
      cat("\n")
      cat("  Network path saved for future sessions.\n")
      cat("  You may need to install packages to this library.\n")
      cat("  See README.md or github page for instructions.\n\n")
      return(invisible())
    }
    
    # Last attempt failed
    if (attempt == max_attempts) {
      cat(sprintf("  ERROR: Z-numbers did not match after %d attempts.\n", max_attempts))
      cat("  Restart R to try again.\n")
    }
  }
}
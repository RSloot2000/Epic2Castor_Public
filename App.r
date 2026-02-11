# ============================================================================
# EPIC2CASTOR - DATA MAPPING APPLICATION
# ============================================================================
#
# Purpose:
#   Interactive Shiny application for mapping EPIC hospital data to Castor EDC
#   Provides a spreadsheet-like interface for managing field and value mappings
#
# Architecture:
#   - Shiny web application with reactive state management
#   - SQLite databases for persistent storage (mapping.db, castor_meta.db)
#   - External R scripts for data processing (baseline, follow-up, biobank)
#   - Tab-based organization mimicking Excel workbooks
#   - Copy/paste functionality for efficient data entry
#   - Auto-fill intelligence using translation APIs and fuzzy matching
#
# Core Components:
#   1. Initialization: Bootstrap config, create directories, load metadata
#   2. Database Management: SQLite connections, CSV sync, hash validation
#   3. UI Layer: Fixed header/footer, resizable table, tab navigation
#   4. Server Logic: Reactive values, observers, table editing
#   5. Data Processing: Batch scripts for baseline/follow-up/biobank export
#   6. Auto-fill Engine: Intelligent EPIC value suggestions (English -> Dutch)
#
# Data Flow:
#   EPIC CSV → Mapping Tables → Castor API → EDC System
#   ├─ elements.csv: Field mappings (EPIC columns → Castor fields)
#   ├─ waarde_radiobuttons.csv: Radio button value translations
#   └─ waarde_checkboxes.csv: Checkbox value mappings
#
# Usage:
#   Run in RStudio: shiny::runApp()
#   Or from terminal: Rscript App.r
# ============================================================================

# ===== ENVIRONMENT RESET =====
# Clear workspace and force garbage collection for clean start
rm(list = ls())
gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
gcinfo(verbose = FALSE)

# ===== LIBRARY DEPENDENCIES =====
# Core Shiny framework and UI components
library(shiny)           # Web application framework
library(data.table)      # High-performance data manipulation
library(DT)              # Interactive DataTables for Shiny
library(shinyjs)         # JavaScript operations from R
library(shinydashboard)  # Dashboard UI components

# Data import/export
library(readxl)          # Excel file reading
library(readr)           # Fast CSV reading/writing

# External process management and API
library(processx)        # Async R process execution
library(jsonlite)        # JSON parsing and generation
library(httr)            # HTTP requests (Castor API)

# Database and utilities
library(DBI)             # Database interface
library(RSQLite)         # SQLite database driver
library(digest)          # MD5 hashing for change detection
library(uuid)            # Unique identifier generation

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Safety wrapper: Capture reactive value safely with isolate()
#' 
#' Prevents errors when accessing reactive values outside reactive context
#' Commonly needed when scheduling deferred execution with later::later()
#' 
#' @param expr Expression to evaluate (reactive value or reactive function)
#' @return The value of expr, isolated from reactive context
#' 
#' @examples
#' safe_capture(input$table_select)
#' safe_capture(selectedRows())
safe_capture <- function(expr) {
  tryCatch(
    isolate(expr),
    error = function(e) {
      if (grepl("Can't access reactive", e$message)) {
        warning("Attempted to access reactive value outside reactive context. Use isolate() or capture before later::later().")
      }
      stop(e)
    }
  )
}

#' Performance timing wrapper for R operations
#' 
#' Times an operation and logs the duration to console with color coding
#' Optional: can also log to performance.log file for persistent monitoring
#' 
#' @param name Character - Operation name for logging
#' @param expr Expression to time
#' @param log_to_file Logical - Whether to also log to performance.log (default: FALSE)
#' @return Result of expr
#' 
#' @examples
#' result <- time_operation("load_data", {
#'   data <- read.csv("large_file.csv")
#'   data
#' })
time_operation <- function(name, expr, log_to_file = FALSE) {
  # Check if performance monitoring is enabled
  if (isFALSE(getOption("epic2castor.performance_monitoring", TRUE))) {
    return(expr)
  }
  
  start_time <- Sys.time()
  
  # Execute operation
  result <- tryCatch(
    expr,
    error = function(e) {
      # Log error with timing
      end_time <- Sys.time()
      duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      cat(sprintf(
        "\033[31m[Perf] ✗ %s: %.3fs (ERROR: %s)\033[0m\n",
        name,
        duration,
        e$message
      ))
      
      stop(e)
    }
  )
  
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Color code based on duration
  color_code <- if (duration < 0.1) {
    "\033[32m"  # Green (< 100ms)
  } else if (duration < 1.0) {
    "\033[33m"  # Yellow (< 1s)
  } else {
    "\033[31m"  # Red (>= 1s)
  }
  
  # Log to console
  cat(sprintf(
    "%s[Perf] ✓ %s: %.3fs\033[0m\n",
    color_code,
    name,
    duration
  ))
  
  # Optional: Log to file for persistent monitoring
  if (log_to_file && !is.null(getOption("epic2castor.performance_log_file"))) {
    tryCatch({
      log_file <- getOption("epic2castor.performance_log_file")
      log_entry <- sprintf(
        "[%s] %s: %.3fs\n",
        format(start_time, "%Y-%m-%d %H:%M:%S"),
        name,
        duration
      )
      cat(log_entry, file = log_file, append = TRUE)
    }, error = function(e) {
      # Silent fail if logging to file fails
    })
  }
  
  return(result)
}

# ============================================================================
# BOOTSTRAP: LOAD CONFIGURATION & INFRASTRUCTURE
# ============================================================================

# Load centralized path configuration from JSON
# Fallback to default paths if config file is missing or corrupted
# This allows flexible deployment without hardcoded paths
paths <- tryCatch(
  jsonlite::fromJSON(file.path("config", "paths.json")),
  error = function(e) {
    # Default paths if JSON loading fails
    list(
      scripts_dir = "scripts",
      logger_script = file.path("scripts", "Logger.r"),
      config_script = file.path("scripts", "config.R"),
      config_api = file.path("config", "APIConfig.json")
    )
  }
)

# Load logging infrastructure (creates log files, provides log_msg function)
logger_path <- if (!is.null(paths$logger_script)) {
  paths$logger_script
} else {
  file.path(paths$scripts_dir, "Logger.r")
}
source(logger_path)

# Load central configuration (provides epc_path() helper for all file paths)
config_script_path <- if (!is.null(paths$config_script)) {
  paths$config_script
} else {
  file.path(paths$scripts_dir, "config.R")
}
source(config_script_path)

# ============================================================================
# INITIALIZATION: DIRECTORY STRUCTURE & PLACEHOLDER FILES
# ============================================================================

# Track if any new files were created during initialization
# Used to trigger database rebuilds when structure changes
files_created <- FALSE

# Create required directory structure and placeholder files
# This ensures the app can start even on first run without pre-existing data
tryCatch({
  cat("[Init] Checking required directories and files...\n")
  
  # ===== DIRECTORY CREATION =====
  
  # Config directory: stores API credentials and configuration JSON files
  config_dir <- "config"
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
    cat("[Init] Created config directory\n")
  }
  
  # Database directory: stores SQLite databases (mapping.db, castor_meta.db)
  db_dir <- epc_path("db_dir")
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
    cat("[Init] Created db directory\n")
  }
  
  # Castor metadata directory: stores field options and study variable list
  # Retrieved via API and cached as CSV files
  castor_meta_dir <- epc_path("castor_meta_dir")
  if (!dir.exists(castor_meta_dir)) {
    dir.create(castor_meta_dir, recursive = TRUE)
    cat("[Init] Created castor_meta directory\n")
  }
  
  # Possible values directory: stores EPIC value options for dropdown fields
  # Generated from castor_meta data during autofill process
  pv_dir <- epc_path("mapping_possible_values_dir")
  if (!dir.exists(pv_dir)) {
    dir.create(pv_dir, recursive = TRUE)
    cat("[Init] Created mapping/possibleValues directory\n")
  }
  
  # ===== API CONFIGURATION FILE =====
  
  # Create APIConfig.json template if missing
  # User must fill in Castor API credentials for full functionality
  api_config_path <- epc_path("config_api")
  if (!file.exists(api_config_path)) {
    api_template <- list(
      client_id = "",
      client_secret = "",
      study_id = ""
    )
    jsonlite::write_json(api_template, api_config_path, pretty = TRUE, auto_unbox = TRUE)
    cat("[Init] Created APIConfig.json template at: ", api_config_path, "\n", sep = "")
    cat("[Init] Please configure your Castor API credentials in this file.\n")
  }
  
  # ===== CASTOR METADATA PLACEHOLDERS =====
  
  # Create placeholder metadata files with proper CSV structure if they don't exist
  # These will be populated by CastorRetrieval.r script when API credentials are configured
  fo <- epc_path("castor_field_options_file")  # field_options.csv
  sv <- epc_path("castor_study_variablelist_file")  # study_variablelist.csv
  
  if (!file.exists(fo)) {
    # field_options.csv: Castor dropdown/radio options metadata
    # Format: Option Group Name;Option Name;Option Value;Option Group Id
    fo_headers <- "Option Group Name;Option Name;Option Value;Option Group Id"
    writeLines(fo_headers, fo)
    cat("[Init] Created placeholder field_options.csv with proper structure\n")
  }
  
  if (!file.exists(sv)) {
    # study_variablelist.csv: Castor field definitions and types
    # Format: Form Name;Form Order;Field Option Group;Field Variable Name;Field Type
    sv_headers <- "Form Name;Form Order;Field Option Group;Field Variable Name;Field Type"
    writeLines(sv_headers, sv)
    cat("[Init] Created placeholder study_variablelist.csv with proper structure\n")
  }
  
  # ===== POSSIBLE VALUES FILE =====
  
  # Create pv_elements.csv: EPIC value options for autofill dropdowns
  # This file maps EPIC column values to available Castor field options
  pv_dir <- epc_path("mapping_possible_values_dir")
  if (dir.exists(pv_dir)) {
    pv_elements_file <- file.path(pv_dir, "pv_elements.csv")
    if (!file.exists(pv_elements_file)) {
      # Format: epic_kolom;epic_waarde;castor_kolom;castor_waarde
      # Will be populated by generate_pv_elements() during startup
      pv_headers <- "epic_kolom;epic_waarde;castor_kolom;castor_waarde"
      writeLines(pv_headers, pv_elements_file)
      cat("[Init] Created placeholder pv_elements.csv\n")
      files_created <- TRUE
    }
  }
  
  # ===== MAPPING FILES =====
  
  # Create core mapping CSV files if they don't exist
  # These are the main data files that users edit in the app
  mapping_dir <- epc_path("mapping_dir")
  if (dir.exists(mapping_dir)) {
    
    # elements.csv: Maps EPIC columns to Castor field names
    # Format: Element (Castor field ID);castor_kolom (EPIC column name)
    elements_file <- file.path(mapping_dir, "elements.csv")
    if (!file.exists(elements_file)) {
      elements_headers <- "Element;castor_kolom"
      writeLines(elements_headers, elements_file)
      cat("[Init] Created placeholder elements.csv\n")
      files_created <- TRUE
    }
    
    # waarde_checkboxes.csv: Maps checkbox values (multi-select fields)
    # Format: Element;waarde (EPIC value);kolom_toevoeging (Castor option ID)
    checkboxes_file <- file.path(mapping_dir, "waarde_checkboxes.csv")
    if (!file.exists(checkboxes_file)) {
      checkboxes_headers <- "Element;waarde;kolom_toevoeging"
      writeLines(checkboxes_headers, checkboxes_file)
      cat("[Init] Created placeholder waarde_checkboxes.csv\n")
      files_created <- TRUE
    }
    
    # waarde_radiobuttons.csv: Maps radio button values (single-select fields)
    # Format: Element;waarde (EPIC value);castor_waarde (Castor option value)
    radiobuttons_file <- file.path(mapping_dir, "waarde_radiobuttons.csv")
    if (!file.exists(radiobuttons_file)) {
      radiobuttons_headers <- "Element;waarde;castor_waarde"
      writeLines(radiobuttons_headers, radiobuttons_file)
      cat("[Init] Created placeholder waarde_radiobuttons.csv\n")
      files_created <- TRUE
    }
  }
  
  cat("[Init] Initialization complete.\n")
  flush.console()
}, error = function(e) {
  # Non-fatal errors during initialization - warn but continue
  warning(paste("[Init] Warning during initialization:", conditionMessage(e)))
})

# ============================================================================
# CASTOR METADATA RETRIEVAL
# ============================================================================
# Fetch Castor field definitions and dropdown options via API
# Uses separate R process (CastorRetrieval.r) to avoid blocking the main app
# Implements caching to avoid unnecessary API calls (default: refresh after 60 min)

# Track whether new metadata was retrieved (triggers database rebuild)
castor_meta_retrieved <- FALSE

tryCatch({
  start_time <- Sys.time()
  
  # ===== LOCATE RETRIEVAL SCRIPT =====
  castor_script <- epc_path("castor_retrieval_script")
  if (is.null(castor_script) || !file.exists(castor_script)) {
    stop(sprintf("Castor retrieval script not found: %s", as.character(castor_script)))
  }
  
  # Find Rscript executable (platform-specific)
  rscript_bin <- if (.Platform$OS.type == "windows") {
    file.path(R.home("bin"), "Rscript.exe")
  } else {
    file.path(R.home("bin"), "Rscript")
  }
  if (!file.exists(rscript_bin)) rscript_bin <- "Rscript"
  
  # ===== DETERMINE IF REFRESH IS NEEDED =====
  fo <- epc_path("castor_field_options_file")
  sv <- epc_path("castor_study_variablelist_file")
  
  # Check age of cached metadata files
  max_age_mins <- getOption("epic2castor.castor_refresh_minutes", 60)
  force_refresh <- isTRUE(getOption("epic2castor.force_retrieval", FALSE)) || 
                   identical(Sys.getenv("EPIC2CASTOR_FORCE_RETRIEVAL"), "1")
  
  info <- tryCatch(file.info(c(fo, sv)), error = function(e) NULL)
  ages_ok <- FALSE
  if (!is.null(info) && nrow(info) == 2 && all(!is.na(info$mtime))) {
    ages <- difftime(Sys.time(), info$mtime, units = "mins")
    ages_ok <- all(ages <= max_age_mins)
  }
  
  # ===== CHECK API CREDENTIALS =====
  # Verify that APIConfig.json contains valid credentials
  api_config_path <- epc_path("config_api")
  has_credentials <- FALSE
  if (file.exists(api_config_path)) {
    api_config <- tryCatch(jsonlite::fromJSON(api_config_path), error = function(e) NULL)
    if (!is.null(api_config) && 
        !is.null(api_config$client_id) && nchar(trimws(api_config$client_id)) > 0 &&
        !is.null(api_config$client_secret) && nchar(trimws(api_config$client_secret)) > 0 &&
        !is.null(api_config$study_id) && nchar(trimws(api_config$study_id)) > 0) {
      has_credentials <- TRUE
    }
  }
  
  # ===== DECIDE: SKIP, USE CACHE, OR RETRIEVE =====
  skip_retrieval <- !force_refresh && all(file.exists(c(fo, sv))) && isTRUE(ages_ok)
  
  if (skip_retrieval) {
    # Cached files are recent enough - skip API call
    cat("[Startup] Castor metadata recent (<= ", max_age_mins, " min); skipping retrieval.\n", sep = "")
    
  } else if (!has_credentials) {
    # No API credentials configured
    if (file.exists(fo) && file.exists(sv)) {
      # Use existing cached files
      cat("[Startup] API credentials not configured yet; using existing Castor metadata files.\n", sep = "")
      cat("[Startup] Please configure API credentials in the app to refresh metadata.\n", sep = "")
    } else {
      # No cache and no credentials - limited functionality mode
      cat("[Startup] API credentials not configured and no cached metadata found.\n", sep = "")
      cat("[Startup] The app will start, but Castor functionality will be limited until credentials are configured.\n", sep = "")
    }
    
  } else {
    # ===== EXECUTE RETRIEVAL SCRIPT =====
    cat("[Startup] Retrieving Castor metadata via: ", castor_script, "\n", sep = "")
    
    # Create done-flag file for process completion verification
    # Child process creates this file when successfully finished
    done_flag <- tempfile(pattern = "castor_retrieval_done_", tmpdir = tempdir(), fileext = ".flag")
    if (file.exists(done_flag)) try(file.remove(done_flag), silent = TRUE)
    
    # Run CastorRetrieval.r as separate process
    # stdout/stderr are streamed directly to console for real-time feedback
    status <- suppressWarnings(system2(
      rscript_bin,
      c("--vanilla", shQuote(castor_script)),
      stdout = "",   # Stream directly to console
      stderr = "",   # Stream directly to console
      wait = TRUE,   # Block until retrieval completes
      env = c(EPIC2CASTOR_DONE = done_flag)  # Pass done-flag path
    ))
    
    # ===== VALIDATE RETRIEVAL RESULT =====
    if (!is.null(status) && status != 0) {
      # Non-zero exit code - check if it's recoverable
      if (file.exists(fo) && file.exists(sv)) {
        # Exit code 5: "no changes needed" - not an error
        if (identical(as.integer(status), 5L)) {
          # Silently continue (metadata unchanged)
        } else {
          # Other exit code but cached files exist - warn and continue
          message(sprintf("[Startup] Castor retrieval returned exit %s, but CSVs already exist; continuing.", status))
        }
      } else {
        # No cached files and retrieval failed - fatal error
        stop(sprintf("Castor retrieval failed (exit %s)", status))
      }
    } else {
      # Exit code 0 - verify done-flag as extra safeguard
      if (!file.exists(done_flag)) {
        # Done-flag missing - verify outputs are recent
        fo_ok <- file.exists(fo)
        sv_ok <- file.exists(sv)
        recent <- function(p) {
          # Check if file was modified within last 5 seconds
          info <- tryCatch(file.info(p), error = function(e) NULL)
          if (is.null(info) || is.na(info$mtime[1])) return(FALSE)
          difftime(info$mtime[1], start_time, units = "secs") >= -5
        }
        if (!(fo_ok && sv_ok && recent(fo) && recent(sv))) {
          stop("Castor retrieval returned without done-flag and outputs are not recent. Check logs for details.")
        }
      }
      # Mark that metadata was successfully retrieved (triggers database rebuild)
      castor_meta_retrieved <- TRUE
    }
    cat("[Startup] Castor metadata retrieval completed.\n")
  }
  flush.console()
  
}, error = function(e) {
  # ===== ERROR HANDLING =====
  # Graceful degradation: use cached files if available, otherwise fail
  error_msg <- conditionMessage(e)
  
  if (grepl("Castor retrieval failed \\(exit 5\\)", error_msg, ignore.case = TRUE)) {
    # Exit 5 is special: "no changes needed" - warn but continue
    cat("[Startup] Warning: Castor metadata retrieval skipped (credentials may not be configured).\n", sep = "")
    cat("[Startup] The app will start with limited functionality. Configure API credentials to enable full features.\n", sep = "")
    
  } else {
    # Other errors: try to use cached files
    fo <- epc_path("castor_field_options_file")
    sv <- epc_path("castor_study_variablelist_file")
    
    if (file.exists(fo) && file.exists(sv)) {
      # Cached files exist - warn but continue with stale data
      cat("[Startup] Warning: Castor metadata retrieval failed, but cached files exist. Continuing with cached data.\n", sep = "")
      cat("[Startup] Error details: ", error_msg, "\n", sep = "")
    } else {
      # No cached files and retrieval failed - fatal error
      stop(paste("Castor metadata retrieval failed during startup:", error_msg))
    }
  }
})

# ============================================================================
# LOAD EXTERNAL SCRIPTS
# ============================================================================

# Load database management functions (connect_db, save_to_db, etc.)
source(file.path(epc_path("scripts_dir"), "database.r"))

# Load auto-fill intelligence engine (process_autofill, translation APIs)
source(file.path(epc_path("scripts_dir"), "autofill.r"))

# Load export functions (export_approved_data, batch processing)
source(file.path(epc_path("scripts_dir"), "export_approved.r"))

# Load combined import wizard (config, detection, mapping, transformation, templates, export)
source(epc_path("import_wizard_script"))

# Load import configs once at startup (for wizard modal)
IMPORT_CONFIGS <- get_all_import_types()
cat(sprintf("[Startup] Import wizard loaded with %d import types\n", length(IMPORT_CONFIGS)))

# Load dashboard module (patient inclusion, data completeness, biobank overview)
source(file.path(epc_path("scripts_dir"), "dashboard.r"))

# ============================================================================
# DATABASE MANAGEMENT UTILITIES
# ============================================================================

#' Compute MD5 hash of all CSV files in a folder
#' 
#' Used to detect changes in source CSV files and trigger database rebuilds
#' Recursively processes all .csv files in the given directory
#' 
#' @param dataFolder Path to folder containing CSV files
#' @return Single MD5 hash string combining all file hashes
#' 
#' @details
#' Hash format: "path1:hash1|path2:hash2|..." -> MD5 of combined string
#' This ensures any file change (content or structure) triggers rebuild
compute_csv_hash <- function(dataFolder) {
  csvs <- list.files(dataFolder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  sums <- tools::md5sum(csvs)
  digest(paste(names(sums), sums, sep = ":", collapse = "|"), algo = "md5")
}

#' Check if database needs rebuilding and rebuild if needed
#' 
#' Compares current CSV hash with stored hash to detect changes
#' Only rebuilds database when source files have been modified
#' 
#' @param dataFolder Folder containing source CSV files
#' @param dbPath Path to SQLite database file
#' @param build_fun Function to call for database building
#' 
#' @details
#' Hash file (.hash) stores the MD5 of CSV files when database was last built
#' If hash matches, skip rebuild for performance
#' If hash differs or database missing, rebuild and update hash
check_and_build <- function(dataFolder, dbPath, build_fun) {
  hashFile <- paste0(dbPath, ".hash")
  newHash  <- compute_csv_hash(dataFolder)
  if (!file.exists(dbPath) ||
      !file.exists(hashFile) ||
      readLines(hashFile, warn = FALSE, n = 1) != newHash) {
    build_fun()
    writeLines(newHash, hashFile)
  }
}

#' Validate Castor retrieval outputs
#' 
#' Checks if retrieval script completed successfully by verifying:
#' - Done-flag file exists, OR
#' - Output CSV files exist and are recent
#' 
#' @param done_flag Path to completion flag file created by retrieval script
#' @param start_time Timestamp when retrieval started
#' @param fo Path to field_options.csv
#' @param sv Path to study_variablelist.csv
#' @return TRUE if validation passed, FALSE otherwise
validate_castor_outputs <- function(done_flag, start_time, fo, sv) {
  # If done-flag exists, trust that retrieval completed successfully
  if (file.exists(done_flag)) return(TRUE)
  
  # Otherwise, verify output files are recent (modified within 5 seconds of start)
  recent <- function(p) {
    info <- tryCatch(file.info(p), error = function(e) NULL)
    if (is.null(info) || is.na(info$mtime[1])) return(FALSE)
    difftime(info$mtime[1], start_time, units = "secs") >= -5
  }
  
  fo_ok <- file.exists(fo)
  sv_ok <- file.exists(sv)
  fo_ok && sv_ok && recent(fo) && recent(sv)
}

# ============================================================================
# DATABASE INITIALIZATION
# ============================================================================
# Build SQLite databases from CSV files (if needed)
# Uses hash-based change detection to avoid unnecessary rebuilds

cat(sprintf("[Startup] (%s) Checking/building mapping database...\n", format(Sys.time(), "%H:%M:%S")))
flush.console()

# ===== MAPPING DATABASE =====
# Contains: elements.csv, waarde_radiobuttons.csv, waarde_checkboxes.csv
# User-editable mapping tables loaded from mapping/ directory
if (!isTRUE(getOption("epic2castor.mapping_built", FALSE)) || files_created) {
  if (files_created) {
    cat("[Startup] New mapping files were created; forcing database rebuild.\n")
    # Force rebuild by removing hash file (will trigger check_and_build)
    hash_file <- paste0(epc_path("mapping_db"), ".hash")
    if (file.exists(hash_file)) {
      try(file.remove(hash_file), silent = TRUE)
    }
  }
  
  # Build or update mapping database from CSV files
  check_and_build(
    dataFolder = epc_path("mapping_dir"),
    dbPath     = epc_path("mapping_db"),
    build_fun  = function() csv_to_database(
      dataFolder = epc_path("mapping_dir"),
      dbPath     = epc_path("mapping_db")
    )
  )
  
  # Set session flag to avoid redundant rebuilds
  options(epic2castor.mapping_built = TRUE)
} else {
  cat("[Startup] Mapping database already built this session; skipping rebuild.\n")
}

cat(sprintf("[Startup] (%s) Mapping database ready.\n", format(Sys.time(), "%H:%M:%S")))
flush.console()

# ===== CASTOR METADATA DATABASE =====
# Contains: field_options.csv, study_variablelist.csv
# Castor field definitions retrieved from API
cat(sprintf("[Startup] (%s) Checking/building Castor meta database...\n", format(Sys.time(), "%H:%M:%S")))
flush.console()

if (!isTRUE(getOption("epic2castor.castor_meta_built", FALSE)) || castor_meta_retrieved) {
  if (castor_meta_retrieved) {
    cat("[Startup] Castor metadata was just retrieved; forcing database rebuild.\n")
    # Force rebuild by removing hash file
    hash_file <- paste0(epc_path("castor_meta_db"), ".hash")
    if (file.exists(hash_file)) {
      try(file.remove(hash_file), silent = TRUE)
    }
  }
  
  # Build or update Castor metadata database
  check_and_build(
    dataFolder = epc_path("castor_meta_dir"),
    dbPath     = epc_path("castor_meta_db"),
    build_fun  = function() csv_to_database_meta(
      dataFolder = epc_path("castor_meta_dir"),
      dbPath     = epc_path("castor_meta_db")
    )
  )
  
  # Set session flag to avoid redundant rebuilds
  options(epic2castor.castor_meta_built = TRUE)
} else {
  cat("[Startup] Castor meta database already built this session; skipping rebuild.\n")
}

cat(sprintf("[Startup] (%s) Castor meta database ready.\n", format(Sys.time(), "%H:%M:%S")))
flush.console()

# ===== PERFORMANCE MONITORING INFO =====
# Log performance monitoring availability
cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("⚡ PERFORMANCE MONITORING ENABLED\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("📊 Client-side:\n")
cat("   • Press Ctrl+Shift+P to toggle performance panel\n")
cat("   • Press Ctrl+Shift+L to log performance summary\n")
cat("   • Console shows timing for: table render, cell edit, tab switch\n")
cat("\n")
cat("📊 Server-side:\n")
cat("   • Console shows timing for R operations\n")
cat("   • Use time_operation('name', { code }) to measure custom operations\n")
cat("\n")
cat("🔧 To disable:\n")
cat("   • Client: Set performanceMonitor.enabled = false in console\n")
cat("   • Server: options(epic2castor.performance_monitoring = FALSE)\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("\n")
flush.console()

# ===== POSSIBLE VALUES GENERATION =====
# Generate pv_elements.csv: maps EPIC values to available Castor options
# Used by autofill dropdowns to show only valid values
cat(sprintf("[Startup] (%s) Generating pv_elements...\n", format(Sys.time(), "%H:%M:%S")))
flush.console()

try({
  generate_pv_elements()
  cat(sprintf("[Startup] (%s) pv_elements done.\n", format(Sys.time(), "%H:%M:%S")))
}, silent = TRUE)

flush.console()

# ============================================================================
# LOAD MAPPING DATA INTO MEMORY
# ============================================================================
# Read all mapping tables from SQLite database into R data.tables
# This provides fast in-memory access for the Shiny reactive system

dbPath <- epc_path("mapping_db")

cat(sprintf("[Startup] (%s) Loading mapping DB into memory...\n", format(Sys.time(), "%H:%M:%S")))
flush.console()

tmp_con <- dbConnect(SQLite(), dbPath)

# Get all table names except internal possibleValues_Elements
table_names <- setdiff(dbListTables(tmp_con), "possibleValues_Elements")

# ===== TABLE VISIBILITY CONFIGURATION =====
# Define which tables are hidden from user selection
# "variabelen" is an internal lookup table not meant for direct editing
# "elements_backup_before_test" is a test backup table that should not be shown
hidden_tables <- c("variabelen", "elements_backup_before_test")

#' Check if a table name is selectable by users
#' 
#' @param name Table name to check
#' @return TRUE if table should be shown in dropdown, FALSE if hidden
is_selectable_table_name <- function(name) {
  !is.null(name) && !(name %in% hidden_tables)
}

# ===== LOAD ALL TABLES INTO MEMORY =====
# Read each table from database and store in named list
# Metadata columns (tab_name_meta, tab_order_meta) are preserved for selectable tables
# This enables the tab system to persist tab structure across sessions
mappingData <- lapply(table_names, function(tbl) {
  dt <- as.data.table(dbReadTable(tmp_con, tbl))
  
  # Remove metadata columns from non-selectable tables
  # Selectable tables (elements, waarde_radiobuttons, waarde_checkboxes) keep metadata
  # for tab management functionality
  if (!is_selectable_table_name(tbl)) {
    meta_cols <- names(dt)[grepl("tab_(name|order)_meta", names(dt))]
    if (length(meta_cols) > 0) {
      dt[, (meta_cols) := NULL]
    }
  }
  
  return(dt)
})
names(mappingData) <- table_names
dbDisconnect(tmp_con)

cat(sprintf("[Startup] (%s) Mapping data loaded.\n", format(Sys.time(), "%H:%M:%S")))
flush.console()

# ============================================================================
# HELPER FUNCTIONS - TABLE AND FILE MANAGEMENT
# ============================================================================

#' Get list of tables that users can select and edit
#' 
#' Excludes internal/hidden tables from the dropdown menu
#' 
#' @return Character vector of selectable table names
get_selectable_tables <- function() {
  setdiff(names(mappingData), hidden_tables)
}

#' Get list of EPIC input files available for processing
#' 
#' Scans epic_input_data_dir for CSV and Excel files
#' Used to populate file selection dropdown in export modals
#' 
#' @return Named character vector: display name -> filename
#'         Returns "No files found" if directory empty or missing
get_epic_input_files <- function() {
  epic_dir <- epc_path("epic_input_data_dir")
  
  # Check if input directory exists
  if (!dir.exists(epic_dir)) {
    return(c("No files found" = ""))
  }
  
  # Find all CSV and Excel files (case-insensitive)
  files <- list.files(epic_dir, pattern = "\\.(csv|xlsx)$", full.names = FALSE, ignore.case = TRUE)
  
  if (length(files) == 0) {
    return(c("No files found" = ""))
  }
  
  # Return named vector: names are display labels, values are filenames
  names(files) <- files
  return(files)
}

#' Get full path for a selected EPIC input file
#' 
#' Constructs absolute path from filename
#' 
#' @param filename Name of file in epic_input_data_dir
#' @return Full path to file, or NULL if filename is empty
get_selected_epic_file_path <- function(filename) {
  if (is.null(filename) || filename == "") return(NULL)
  epic_dir <- epc_path("epic_input_data_dir")
  file.path(epic_dir, filename)
}

#' Check if a table name is selectable (user-facing)
#' 
#' Validates that table exists and is not hidden
#' 
#' @param name Table name to check
#' @return TRUE if table can be selected by user
is_selectable_table <- function(name) {
  !is.null(name) && name %in% get_selectable_tables()
}

# ============================================================================
# CASTOR API HELPERS
# ============================================================================

#' Retrieve Castor site list for the configured study
#' 
#' Fetches available sites from Castor EDC API for the study
#' specified in APIConfig.json. Sites are used to filter patient data
#' during baseline/follow-up export operations.
#' 
#' @return Character vector of site choices in format "site_id - name"
#'         Returns empty character vector if API call fails
#' 
#' @details
#' OAuth2 Flow:
#' 1. Request access token using client credentials
#' 2. Use token to fetch sites for the configured study
#' 3. Format sites as "id - name" for dropdown display
#' 
#' Error handling: Returns empty vector on failure (graceful degradation)
get_site_choices <- function() {
  # Load API configuration
  config <- fromJSON(epc_path("config_api"))
  study_id <- config$study_id
  base_url <- "https://data.castoredc.com"
  api_base_url <- "https://data.castoredc.com/api"
  client_id <- config$client_id
  client_secret <- config$client_secret
  
  # ===== STEP 1: OBTAIN ACCESS TOKEN =====
  # OAuth2 client credentials flow
  token_url <- paste0(base_url, "/oauth/token")
  token_response <- tryCatch({
    httr::RETRY(
      "POST", token_url,
      body = list(
        client_id = client_id,
        client_secret = client_secret,
        grant_type = "client_credentials"
      ),
      encode = "form",
      httr::timeout(15),
      times = 2  # Retry once on failure
    )
  }, error = function(e) NULL)
  
  # Validate token response
  if (is.null(token_response) || status_code(token_response) != 200) {
    stop("Error obtaining access token from Castor API")
  }
  
  token_data <- content(token_response, as = "parsed", encoding = "UTF-8")
  access_token <- token_data$access_token
  
  # ===== STEP 2: RETRIEVE SITES FOR STUDY =====
  site_url <- paste0(api_base_url, "/study/", study_id, "/site")
  response <- tryCatch({
    httr::RETRY(
      "GET", site_url,
      add_headers(Authorization = paste("Bearer", access_token)),
      httr::timeout(15),
      times = 2
    )
  }, error = function(e) NULL)
  
  # Validate sites response
  if (is.null(response) || status_code(response) != 200) {
    stop("Error retrieving sites from Castor API")
  }
  
  # ===== STEP 3: PARSE AND FORMAT SITES =====
  sites <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  
  # Handle both embedded and direct response formats
  if ("_embedded" %in% names(sites)) {
    sites_data <- sites[["_embedded"]][["sites"]]
  } else {
    sites_data <- sites
  }
  
  # Format as "site_id - name" for dropdown display
  choices <- sapply(sites_data, function(x) {
    paste(x$site_id, x$name, sep = " - ")
  })
  
  return(choices)
}

# ============================================================================
# LOAD OPTION LISTS
# ============================================================================
# Load dropdown options for Castor fields (radiobuttons, checkboxes)
# Provides structured access to field options from castor_meta database
source(file.path(epc_path("scripts_dir"), "option_lists2.R"))

# ============================================================================
# CASTOR METADATA REFRESH FUNCTION
# ============================================================================

#' Reload Castor metadata and rebuild dependent databases
#' 
#' Called when user manually requests a metadata refresh (e.g., after
#' Castor field definitions have changed). This function:
#' 1. Rebuilds castor_meta.db from CSV files
#' 2. Regenerates pv_elements.csv (possible values mapping)
#' 3. Rebuilds mapping.db to pick up new possible values
#' 4. Reloads option_lists2.R to refresh dropdowns
#' 5. Reloads mappingData into memory
#' 
#' @details
#' This is a comprehensive refresh that ensures all Castor-dependent
#' data structures are synchronized. Use after:
#' - Running CastorRetrieval.r to update metadata
#' - Making changes to Castor field definitions
#' - Adding/removing fields in Castor EDC
reload_castor_metadata <- function() {
  # ===== STEP 1: REBUILD CASTOR META DATABASE =====
  cat("[CastorRefresh] Rebuilding Castor meta database...\n")
  hash_file <- paste0(epc_path("castor_meta_db"), ".hash")
  if (file.exists(hash_file)) {
    try(file.remove(hash_file), silent = TRUE)
  }
  
  # Rebuild database from field_options.csv and study_variablelist.csv
  tryCatch({
    check_and_build(
      dataFolder = epc_path("castor_meta_dir"),
      dbPath     = epc_path("castor_meta_db"),
      build_fun  = function() csv_to_database_meta(
        dataFolder = epc_path("castor_meta_dir"),
        dbPath     = epc_path("castor_meta_db")
      )
    )
    cat("[CastorRefresh] Castor meta database rebuilt successfully.\n")
  }, error = function(e) {
    cat(sprintf("[CastorRefresh] Warning: Failed to rebuild Castor meta database: %s\n", conditionMessage(e)))
  })
  
  # ===== STEP 2: REGENERATE POSSIBLE VALUES =====
  cat("[CastorRefresh] Regenerating pv_elements...\n")
  try({
    generate_pv_elements()
    cat("[CastorRefresh] pv_elements regenerated.\n")
  }, silent = TRUE)
  
  # ===== STEP 3: REBUILD MAPPING DATABASE =====
  # Mapping database depends on pv_elements for dropdown options
  cat("[CastorRefresh] Rebuilding mapping database...\n")
  hash_file_mapping <- paste0(epc_path("mapping_db"), ".hash")
  if (file.exists(hash_file_mapping)) {
    try(file.remove(hash_file_mapping), silent = TRUE)
  }
  
  # Rebuild mapping database
  tryCatch({
    check_and_build(
      dataFolder = epc_path("mapping_dir"),
      dbPath     = epc_path("mapping_db"),
      build_fun  = function() csv_to_database(
        dataFolder = epc_path("mapping_dir"),
        dbPath     = epc_path("mapping_db")
      )
    )
    cat("[CastorRefresh] Mapping database rebuilt successfully.\n")
  }, error = function(e) {
    cat(sprintf("[CastorRefresh] Warning: Failed to rebuild mapping database: %s\n", conditionMessage(e)))
  })
  
  # ===== STEP 4: RELOAD OPTION LISTS =====
  # Reload option_lists2.R in isolated environment to get fresh dropdown data
  cat("[CastorRefresh] Reloading option lists...\n")
  options(epic2castor.force_option_reload = TRUE)
  local_env <- new.env(parent = globalenv())
  sys.source(file.path(epc_path("scripts_dir"), "option_lists2.R"), envir = local_env)
  
  # Update global variables with fresh option data
  option_data <<- local_env$option_data
  checkBoxesValues <<- local_env$checkBoxesValues
  radioButtonOptionValues <<- local_env$radioButtonOptionValues
  checkboxes <<- local_env$checkboxes
  radiobuttons <<- local_env$radiobuttons
  metaRadioButtons <<- local_env$metaRadioButtons
  metaVariables <<- local_env$metaVariables
  
  # ===== STEP 5: RELOAD MAPPING DATA =====
  # Reload all mapping tables from database into memory
  cat("[CastorRefresh] Reloading mapping data...\n")
  tmp_con <- dbConnect(SQLite(), dbPath)
  on.exit(dbDisconnect(tmp_con), add = TRUE)
  
  table_list <- setdiff(dbListTables(tmp_con), "possibleValues_Elements")
  mappingData <<- lapply(table_list, function(tbl) as.data.table(dbReadTable(tmp_con, tbl)))
  names(mappingData) <<- table_list
  table_names <<- table_list
  
  cat(sprintf("[CastorRefresh] (%s) Mapping data reloaded.\n", format(Sys.time(), "%H:%M:%S")))
  flush.console()
}

# ============================================================================
# UI DEFINITION
# ============================================================================
# Shiny user interface layout
# Excel-like design with fixed header/footer, resizable table, tab navigation

ui <- fluidPage(
  # ===== ENABLE JAVASCRIPT FUNCTIONALITY =====
  useShinyjs(),
  
  # ===== LOADING SCREEN =====
  # Full-screen overlay shown during app initialization
  # Hidden via JavaScript once table is ready (see appJS.js)
  div(id = "loading-screen",
      div(class = "loading-container",
          tags$img(src = "img/logo.png", class = "loading-logo", alt = "Loading..."),
          div(id = "loading-text", "Loading")
      )
  ),
  
  # ===== MAIN APPLICATION CONTAINER =====
  div(id = "app",
      
      # ===== HEAD SECTION =====
      # Favicons, CSS, JavaScript dependencies
      tags$head(
          # Favicon configuration (multiple sizes for different devices)
          tags$link(rel = "icon", type = "image/x-icon", href = "img/favicon.ico"),
          tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "img/favicon-16x16.png"),
          tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "img/favicon-32x32.png"),
          tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "img/apple-touch-icon.png"),
          tags$link(rel = "manifest", href = "img/site.webmanifest"),
          
          # JavaScript libraries and custom scripts
          tags$link(rel = "stylesheet", href = "appCSS.css"),  # Custom styles
          tags$link(rel = "stylesheet", href = "select2.min.css"),  # Dropdown styling
          tags$script(src = "select2.min.js"),  # Enhanced dropdowns
          
          # Keyboard shortcut handler
          tags$script(src = paste0("shortcutHandler.js?v=", format(Sys.time(), "%Y%m%d%H%M%S"))),
          
          # Custom JavaScript with cache-busting timestamp
          tags$script(src = paste0("appJS.js?v=", format(Sys.time(), "%Y%m%d%H%M%S"))),
          
          # Auto-close dropdown menus after click (UX improvement)
          tags$script(HTML("$(document).on('click', '.menu-link', function(){ var $dropdown = $(this).closest('.dropdown'); if ($dropdown.length) { setTimeout(function(){ $dropdown.removeClass('open'); }, 100); }});"))
      ),
      
      # ===== FIXED HEADER =====
      # Always visible at top of page with file selector and search
      div(class = "fixed-header",
          div(class = "header-container",
              
              # Left section: file selection and search
              div(class = "header-left",
                  
                  # Top row: table selector and search box
                  div(class = "header-top",
                      # Table/file selector dropdown
                      div(style = "margin-right: 5px;",
                          selectInput("file", "File", choices = get_selectable_tables())
                      ),
                      
                      # Search box with row count warning icon
                      div(style = "margin-right: 5px; position: relative;",
                          textInput("search", label = "Search", placeholder = "Search...", width = "200px") %>%
                            tagAppendAttributes(oninput = "Shiny.setInputValue('search', this.value, {priority:'event'})")
                      ),
                      
                      # Warning icon (shown when row count exceeds threshold)
                      div(id = "row_warning_icon", 
                          style = "display: none; margin-right: 10px;",
                          uiOutput("row_warning_content")
                      ),
                      
                      # Render mode badge (Step 5 - compact icon shows pagination strategy)
                      div(id = "render_mode_badge", 
                          style = "display: none; margin-right: 10px;",
                          uiOutput("render_mode_content")
                      )
                  ),
                  
                  # Bottom row: menu bar with File, Castor, Edit, Export menus
                  div(class = "header-bottom",
                      div(class = "menu-bar",
                          
                          # ===== FILE MENU =====
                          div(class = "dropdown menu-group",
                              tags$button(
                                class = "btn btn-default dropdown-toggle menu-toggle",
                                type = "button",
                                `data-toggle` = "dropdown",
                                `aria-haspopup` = "true",
                                `aria-expanded` = "false",
                                "File ",
                                tags$span(class = "caret")
                              ),
                              tags$ul(class = "dropdown-menu",
                                      tags$li(actionLink("save", "Save changes", class = "menu-link")),
                                      tags$li(actionLink("undo", "Undo all changes", class = "menu-link")),
                                      tags$li(class = "divider"),
                                      tags$li(actionLink("select_epic_file", "Manage input files", class = "menu-link"))
                              )
                          ),
                          
                          # ===== CASTOR MENU =====
                          div(class = "dropdown menu-group",
                              tags$button(
                                class = "btn btn-default dropdown-toggle menu-toggle",
                                type = "button",
                                `data-toggle` = "dropdown",
                                `aria-haspopup` = "true",
                                `aria-expanded` = "false",
                                "Castor ",
                                tags$span(class = "caret")
                              ),
                              tags$ul(class = "dropdown-menu",
                                      tags$li(actionLink("update_credentials", "Update credentials", class = "menu-link")),
                                      tags$li(actionLink("refresh_castor", "Refresh metadata", class = "menu-link")),
                                      tags$li(class = "divider"),
                                      tags$li(actionLink("manage_medical_dict", "Medical Dictionary", class = "menu-link")),
                                      tags$li(class = "divider"),
                                      tags$li(actionLink("run_main_script", "Create CSVs", class = "menu-link")),
                                      tags$li(actionLink("run_upload_script", "Castor upload", class = "menu-link"))
                              )
                          ),
                          
                          # ===== DASHBOARD BUTTON =====
                          actionButton("open_dashboard", 
                                       tags$span(icon("chart-bar"), "Dashboard"),
                                       class = "btn btn-default menu-toggle",
                                       style = "margin-left: 5px;"),
                          
                          # ===== HELP MENU =====
                          div(class = "dropdown menu-group",
                              tags$button(
                                class = "btn btn-default dropdown-toggle menu-toggle",
                                type = "button",
                                `data-toggle` = "dropdown",
                                `aria-haspopup` = "true",
                                `aria-expanded` = "false",
                                "Help ",
                                tags$span(class = "caret")
                              ),
                              tags$ul(class = "dropdown-menu",
                                      tags$li(actionLink("show_shortcuts", "User Guide", class = "menu-link", icon = icon("book"))),
                                      tags$li(class = "divider"),
                                      tags$li(actionLink("show_about", "About", class = "menu-link", icon = icon("info-circle")))
                              )
                          )
                          
                          # Additional menu groups would go here (Edit, Export, etc.)
                          # Note: Full menu structure continues in original code
                      )
                  )
              ),
              
              # Right section: application logo
              div(
                  img(src = "img/logo.png", alt = "Logo", style = "height: 150px;")
              )
          )
      ),
      
      # ===== MAIN CONTENT AREA =====
      # Data table with horizontal scrolling
      mainPanel(
          box(
              title = "",  # No title for cleaner look
              width = NULL,  # Responsive width - controlled by JavaScript
              status = "primary",
              
              # Scrollable container for table (horizontal overflow)
              div(id = "scrollDiv",
                  DT::DTOutput('table')  # DataTables output
              )
          ),
          style = "margin-top: 10px; margin-left: 10px; width: 100%;"
      ),
      
      # ===== FIXED FOOTER =====
      # Always visible at bottom with row controls and tab navigation
      div(class = "fixed-footer",
          style = "display: block; padding: 10px 15px;",
          
          # First row: action buttons and table width control
          div(style = "margin-bottom: 10px; padding-bottom: 10px; border-bottom: 1px solid #ddd; display: flex; align-items: center; justify-content: space-between;",
              
              # Left side: New Tab button
              div(style = "flex-shrink: 0;",
                  actionButton("create_tab", "+ New Tab",
                               class = "btn btn-success btn-add-tab",
                               title = "Add new tab")
              ),
              
              # Right side: all other controls with logical grouping
              div(style = "display: flex; align-items: center; gap: 15px;",
                  
                  # GROUP 1: Row add/delete operations
                  tags$div(class = "footer-btn-group",
                           actionButton("add_row", "+",
                                        class = "btn btn-footer",
                                        style = "background-color:green; color:white; min-width:40px; height:34px; padding:6px 12px;",
                                        title = "Add new row"),
                           actionButton("delete_rows", "-",
                                        class = "btn btn-footer",
                                        style = "background-color:red; color:white; min-width:40px; height:34px; padding:6px 12px;",
                                        title = "Delete selected rows")
                  ),
                  
                  # Separator
                  tags$div(class = "footer-separator"),
                  
                  # GROUP 2: Clipboard operations
                  tags$div(class = "footer-btn-group",
                           actionButton("copy_rows", icon("copy"),
                                        class = "btn btn-footer",
                                        style = "background-color:#007bff; color:white; min-width:40px; height:34px; padding:6px 12px;",
                                        title = "Copy selected rows"),
                           actionButton("cut_rows", icon("cut"),
                                        class = "btn btn-footer",
                                        style = "background-color:#ff9800; color:white; min-width:40px; height:34px; padding:6px 12px;",
                                        title = "Cut selected rows"),
                           actionButton("paste_rows", icon("paste"),
                                        class = "btn btn-footer",
                                        style = "background-color:#28a745; color:white; min-width:40px; height:34px; padding:6px 12px;",
                                        title = "Paste rows")
                  ),
                  
                  # Separator
                  tags$div(class = "footer-separator"),
                  
                  # GROUP 3: Row movement
                  tags$div(class = "footer-btn-group",
                           actionButton("move_rows_bulk", icon("arrows-alt-v"),
                                        class = "btn btn-footer",
                                        style = "background-color:#9c27b0; color:white; min-width:40px; height:34px; padding:6px 12px;",
                                        title = "Move rows to another tab")
                  ),
                  
                  # Separator
                  tags$div(class = "footer-separator"),
                  
                  # GROUP 4: Smart features (auto-fill + auto-width)
                  tags$div(class = "footer-btn-group",
                           # Auto-fill button (dynamic UI - only shown for value tables)
                           uiOutput("autofill_button_ui"),
                           
                           # Auto-width toggle button (fits table to window width)
                           tags$button(
                             id = "reset_table_width",
                             class = "btn btn-footer",
                             style = "background-color:#17a2b8; color:white; min-width:40px; height:34px; padding:6px 12px;",
                             title = "Auto-fit table to window",
                             onclick = "console.log('Reset button clicked'); window.tableAutoWidth = true; if (typeof window.resizeTableToWindow === 'function') { window.resizeTableToWindow(); setTimeout(function() { var table = $('#table table').DataTable(); if (table) { table.columns.adjust(); console.log('DataTables columns adjusted'); } }, 50); } else { console.error('resizeTableToWindow not found'); }",
                             icon("expand")
                           )
                  )
              )
          ),
          
          # Second row: tab navigation bar (Excel-style tabs)
          div(class = "tab-navigation-bar",
              div(class = "tab-list-container", style = "text-align: left !important;",
                  uiOutput("tab_buttons", inline = TRUE)  # Dynamic tab buttons
              )
          )
      )
  )
)

# ============================================================================
# HELPER FUNCTIONS - KEYBOARD SHORTCUTS
# ============================================================================

#' Generate Keyboard Shortcuts Section for Modal
#' 
#' Creates an HTML section displaying all keyboard shortcuts from config
#' Groups shortcuts by category with icons and descriptions
#' 
#' @param config List - Parsed keyboard_shortcuts.json configuration
#' @return tags object with HTML for shortcuts section
generate_keyboard_shortcuts_section <- function(config) {
  if (is.null(config) || is.null(config$shortcuts)) {
    return(tags$p("No shortcuts configured"))
  }
  
  # Category metadata
  category_info <- list(
    clipboard = list(icon = "clipboard", label = "Clipboard Operations", color = "#e67e22"),
    data = list(icon = "save", label = "Data Operations", color = "#27ae60"),
    navigation = list(icon = "compass", label = "Navigation", color = "#3498db"),
    ui = list(icon = "window-maximize", label = "Interface", color = "#9b59b6")
  )
  
  sections <- list()
  
  for (cat_name in names(config$shortcuts)) {
    cat_config <- config$shortcuts[[cat_name]]
    cat_info <- category_info[[cat_name]]
    
    if (is.null(cat_info)) next
    
    # Build table rows for this category
    rows <- list()
    for (action_name in names(cat_config)) {
      action <- cat_config[[action_name]]
      
      # Skip if disabled
      if (!is.null(action$enabled) && !action$enabled) next
      
      # Build shortcut keys display
      keys <- action$keys
      alt_keys <- if (!is.null(action$alt_keys)) {
        paste0(" or ", tags$kbd(action$alt_keys))
      } else {
        ""
      }
      
      # Scope warning
      scope_note <- if (!is.null(action$scope) && action$scope == "elements_table_only") {
        tags$div(
          style = "margin-top: 5px;",
          tags$small(
            style = "color: #e67e22;",
            icon("exclamation-triangle"), " Elements table only"
          )
        )
      } else {
        NULL
      }
      
      rows[[action_name]] <- tags$tr(
        tags$td(
          tags$kbd(keys),
          if (nchar(alt_keys) > 0) tags$span(style = "color: #7f8c8d;", alt_keys) else NULL,
          style = "width: 250px; font-weight: 500;"
        ),
        tags$td(
          action$description,
          scope_note
        )
      )
    }
    
    # Create section if we have rows
    if (length(rows) > 0) {
      sections[[cat_name]] <- tags$div(
        style = "margin-bottom: 25px;",
        tags$h4(
          style = sprintf("color: #2c3e50; border-bottom: 2px solid %s; padding-bottom: 8px; margin-bottom: 15px;", cat_info$color),
          icon(cat_info$icon), " ", cat_info$label
        ),
        tags$table(
          class = "table table-hover",
          style = "margin-bottom: 0;",
          tags$tbody(rows)
        )
      )
    }
  }
  
  # Return wrapped sections
  tags$div(
    tags$div(
      style = "background: #e8f5e9; padding: 12px; border-radius: 4px; margin-bottom: 20px; border-left: 4px solid #27ae60;",
      icon("keyboard"), " ", 
      tags$strong("Keyboard Shortcuts:"), 
      " Use these shortcuts for faster workflow. Press ",
      tags$kbd("F1"), " anytime to view this help."
    ),
    sections
  )
}

# ============================================================================
# SERVER FUNCTION
# ============================================================================
# Reactive logic for the Shiny application
# Handles user interactions, data processing, and UI updates

server <- function(input, output, session) {
  
  # ===== PROCESS RUNNER STATE =====
  # Tracks background R processes (baseline, follow-up, biobank exports)
  # Manages process lifecycle and error handling
  runnerState <- reactiveValues(
    proc = NULL,                  # processx::process object
    observer = NULL,              # Observer for process monitoring
    kind = NULL,                  # Process type: "baseline", "follow_up", "biobank"
    canceled = FALSE,             # User requested cancellation
    error_message = NULL,         # Error text if process failed
    error_detected = FALSE,       # Error flag
    missing_file_info = NULL,     # Info about missing input files
    on_complete_handler = NULL    # Callback function when process completes
  )
  
  # ===== API CREDENTIALS CHECK =====
  # Show notification on startup if API credentials are not configured
  # Guides user to configure Castor API access
  observe({
    priority = 1000  # High priority to run early in initialization
    
    api_config_path <- epc_path("config_api")
    has_credentials <- FALSE
    
    if (file.exists(api_config_path)) {
      api_config <- tryCatch(jsonlite::fromJSON(api_config_path), error = function(e) NULL)
      
      # Check if all required credentials are present and non-empty
      if (!is.null(api_config) &&
          !is.null(api_config$client_id) && nchar(trimws(api_config$client_id)) > 0 &&
          !is.null(api_config$client_secret) && nchar(trimws(api_config$client_secret)) > 0 &&
          !is.null(api_config$study_id) && nchar(trimws(api_config$study_id)) > 0) {
        has_credentials <- TRUE
      }
    }
    
    # Display persistent warning notification if credentials missing
    if (!has_credentials) {
      showNotification(
        ui = tagList(
          tags$div(
            style = "font-size: 14px;",
            tags$strong(icon("exclamation-triangle"), " API Credentials Not Configured"),
            tags$br(),
            tags$br(),
            "Castor functionality is currently limited. Please configure your API credentials:",
            tags$br(),
            tags$ol(
              style = "margin-top: 8px; margin-bottom: 8px;",
              tags$li("Click 'Castor' → 'Update credentials' in the menu"),
              tags$li("Enter your Client ID, Client Secret, and Study ID"),
              tags$li("Click 'Castor' → 'Refresh metadata' to load study data")
            ),
            tags$small(
              style = "color: #777;",
              icon("info-circle"),
              " Find your credentials in Castor EDC under Settings → API"
            )
          )
        ),
        duration = NULL,  # Persistent until dismissed
        closeButton = TRUE,
        type = "warning",
        id = "credentials_warning"
      )
    }
  })
  
  # ============================================================================
  # TAB STATE MANAGEMENT
  # ============================================================================
  # Excel-style tab system for organizing data within each table
  # Allows users to split data into logical groups (e.g., by form or category)
  
  #' Tab State Reactive Values
  #' 
  #' @field tabs List of tab objects, each containing:
  #'   - id: Unique tab identifier (e.g., "tab_1")
  #'   - name: User-visible tab name (editable)
  #'   - order: Tab display order (determines left-to-right sequence)
  #'   - data: data.table with rows belonging to this tab
  #' @field activeTab ID of currently selected tab
  #' @field nextTabId Counter for generating unique tab IDs
  tabState <- reactiveValues(
    tabs = list(),           # List of tab objects
    activeTab = NULL,        # Currently active tab ID
    nextTabId = 1            # Auto-incrementing tab ID counter
  )
  
  # Track previous table selection for tab synchronization
  # Used to persist active tab when switching between tables with shared tab structure
  previous_table <- reactiveVal(NULL)
  
  # Track last active tab name globally across all tables
  # Tables with tab support (elements, waarde_radiobuttons, waarde_checkboxes)
  # share the same tab structure, so switching between them preserves active tab
  lastActiveTabName <- reactiveVal(NULL)
  
  # ============================================================================
  # TAB HELPER FUNCTIONS
  # ============================================================================
  
  #' Get data from currently active tab
  #' 
  #' @return data.table with rows from active tab, or NULL if no active tab
  get_active_tab_data <- function() {
    if (is.null(tabState$activeTab) || length(tabState$tabs) == 0) {
      return(NULL)
    }
    
    # Find tab by ID
    active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
    if (length(active_idx) == 0) {
      return(NULL)
    }
    
    return(tabState$tabs[[active_idx]]$data)
  }
  
  #' Initialize tab structure for a new table
  #' 
  #' Creates default "Main" tab with all data
  #' 
  #' @param data data.table to initialize with
  #' @return List with single tab object
  initialize_tabs <- function(data) {
    list(list(
      id = "tab_1",
      name = "Main",
      order = 1,
      data = data
    ))
  }
  
  #' Ensure all tabs have valid order values
  #' 
  #' Fixes any NULL or duplicate order values by reassigning sequential orders
  #' 
  #' @param tabs List of tab objects
  #' @return List of tabs with valid, unique order values
  ensure_tab_order <- function(tabs) {
    for (i in seq_along(tabs)) {
      if (is.null(tabs[[i]]$order) || is.na(tabs[[i]]$order)) {
        tabs[[i]]$order <- i
      }
    }
    return(tabs)
  }
  
  #' Consolidate all tabs into single data.table with metadata columns
  #' 
  #' Combines data from all tabs and adds metadata columns:
  #' - tab_name_meta: Name of the tab this row belongs to
  #' - tab_order_meta: Display order of the tab
  #' 
  #' This format is used for saving to CSV/database, preserving tab structure
  #' 
  #' @param tabs List of tab objects
  #' @return data.table with all rows and metadata columns
  consolidate_tabs_with_metadata <- function(tabs) {
    if (length(tabs) == 0) return(data.table())
    
    # Bind all tab data together with metadata
    all_data <- rbindlist(lapply(seq_along(tabs), function(i) {
      tab <- tabs[[i]]
      dt <- as.data.table(copy(tab$data))
      
      # Add metadata columns (without leading __ to avoid SQLite X prefix)
      dt[, tab_name_meta := as.character(tab$name)]
      dt[, tab_order_meta := as.integer(tab$order)]
      
      return(dt)
    }), fill = TRUE)
    
    return(all_data)
  }
  
  #' Restore tab structure from consolidated data with metadata
  #' 
  #' Splits consolidated data back into separate tabs based on metadata columns
  #' If metadata is missing, creates single default "Main" tab
  #' 
  #' @param data data.table with tab_name_meta and tab_order_meta columns
  #' @return List of tab objects
  restore_tabs_from_metadata <- function(data) {
    # Check for metadata columns
    has_tab_name <- "tab_name_meta" %in% names(data)
    has_tab_order <- "tab_order_meta" %in% names(data)
    
    if (nrow(data) == 0 || !has_tab_name || !has_tab_order) {
      # No metadata - create single default tab
      data_clean <- copy(data)
      
      # Remove any existing metadata columns
      meta_cols <- names(data_clean)[grepl("tab_(name|order)_meta", names(data_clean))]
      if (length(meta_cols) > 0) {
        data_clean[, (meta_cols) := NULL]
      }
      
      return(initialize_tabs(data_clean))
    }
    
    # Convert to data.table and ensure correct types
    dt <- as.data.table(copy(data))
    dt[, tab_name_meta := as.character(tab_name_meta)]
    dt[, tab_order_meta := as.integer(as.character(tab_order_meta))]
    
    # Get unique tab combinations and sort by order
    unique_tabs <- unique(dt[, .(tab_name = tab_name_meta, tab_order = tab_order_meta)])
    unique_tabs[, tab_order := as.integer(tab_order)]
    setorder(unique_tabs, tab_order)
    
    # Create tab objects from unique combinations
    tabs <- lapply(seq_len(nrow(unique_tabs)), function(i) {
      tab_name <- unique_tabs$tab_name[i]
      tab_order <- unique_tabs$tab_order[i]
      
      # Filter data for this tab
      tab_data <- dt[tab_name_meta == tab_name & tab_order_meta == tab_order]
      
      # Remove metadata columns from data
      tab_data_clean <- copy(tab_data)
      meta_cols <- names(tab_data_clean)[grepl("tab_(name|order)_meta", names(tab_data_clean))]
      if (length(meta_cols) > 0) {
        tab_data_clean[, (meta_cols) := NULL]
      }
      
      list(
        id = paste0("tab_", i),
        name = tab_name,
        order = tab_order,
        data = tab_data_clean
      )
    })
    
    return(tabs)
  }
  
  #' Synchronize tab structure from one table to another
  #' 
  #' Used when switching between tables that share tab structure
  #' (elements, waarde_radiobuttons, waarde_checkboxes)
  #' 
  #' @param table_name Name of target table to sync to
  #' @param reference_tabs Tab structure from source table
  #' @return Updated data with synced metadata
  #' 
  #' @details
  #' - For elements table: Preserves existing tabs
  #' - For value tables (radiobuttons/checkboxes): Does nothing (tabs managed by auto-fill)
  #' - For data without metadata: Places all rows in first reference tab
  sync_tab_structure_to_table <- function(table_name, reference_tabs) {
    # Get current data for the table
    current_data <- mappingData[[table_name]]
    
    # If table has no metadata, place everything in first reference tab
    if (!("tab_name_meta" %in% names(current_data))) {
      first_tab <- reference_tabs[[1]]
      current_data[, tab_name_meta := first_tab$name]
      current_data[, tab_order_meta := first_tab$order]
    }
    
    # For waarde_radiobuttons and waarde_checkboxes: do nothing
    # Tabs are managed by updateRadioMapping/updateCheckboxMapping functions
    # For elements: preserve all existing tabs (no deletion)
    
    return(current_data)
  }
  
  #' Add empty tab to consolidated data
  #' 
  #' Creates a new tab with no data rows (or a single NA placeholder row)
  #' 
  #' @param data Consolidated data.table with metadata
  #' @param tab_name Name for new tab
  #' @param tab_order Display order for new tab
  #' @return Updated data with empty tab added
  add_empty_tab_to_consolidated <- function(data, tab_name, tab_order) {
    # Handle completely empty data
    if (nrow(data) == 0) {
      dt <- data.table()
      dt[, tab_name_meta := character(0)]
      dt[, tab_order_meta := integer(0)]
      return(dt)
    }
    
    # Create template row based on first row structure
    empty_row <- data[1, ]
    
    # Set all non-metadata columns to NA
    for (col in setdiff(names(empty_row), c("tab_name_meta", "tab_order_meta"))) {
      empty_row[[col]] <- NA
    }
    
    empty_row$tab_name_meta <- tab_name
    empty_row$tab_order_meta <- tab_order
    
    # Add to data
    result <- rbind(data, empty_row, fill = TRUE)
    return(result)
  }
  
  #' Rename tab in metadata
  #' 
  #' Updates tab_name_meta column for all rows belonging to the tab
  #' 
  #' @param data data.table with metadata
  #' @param old_name Current tab name
  #' @param new_name New tab name
  #' @return Updated data with renamed tab
  rename_tab_in_metadata <- function(data, old_name, new_name) {
    if (!("tab_name_meta" %in% names(data))) return(data)
    
    data[tab_name_meta == old_name, tab_name_meta := new_name]
    return(data)
  }
  
  #' Remove tab from metadata
  #' 
  #' Deletes all rows belonging to the specified tab
  #' 
  #' @param data data.table with metadata
  #' @param tab_name Name of tab to remove
  #' @param tab_order Order of tab to remove
  #' @return Updated data with tab rows removed
  remove_tab_from_metadata <- function(data, tab_name, tab_order) {
    if (!("tab_name_meta" %in% names(data))) return(data)
    
    # Remove all rows matching this tab
    result <- data[!(tab_name_meta == tab_name & tab_order_meta == tab_order)]
    return(result)
  }
  
  # ============================================================================
  # COPY/CUT/PASTE FUNCTIONALITY
  # ============================================================================
  # Excel-like clipboard operations for efficient data entry
  # Only supported for 'elements' table to prevent data integrity issues
  # Automatically handles related checkbox and radiobutton data
  
  #' Check if copy/cut/paste operations are available for a table
  #' 
  #' Only the 'elements' table supports clipboard operations
  #' Value tables (checkboxes, radiobuttons) are excluded to maintain
  #' data integrity and avoid orphaned mappings
  #' 
  #' @param table_name Name of table to check
  #' @return TRUE if copy/paste is supported
  is_copyable_table <- function(table_name) {
    return(!is.null(table_name) && table_name == "elements")
  }
  
  #' Get name of currently active tab
  #' 
  #' @return Tab name string, or "Main" if no active tab
  get_active_tab_name <- function() {
    if (is.null(tabState$activeTab) || length(tabState$tabs) == 0) return("Main")
    
    active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
    if (length(active_idx) == 0) return("Main")
    
    return(tabState$tabs[[active_idx]]$name)
  }
  
  #' Get order of currently active tab
  #' 
  #' @return Tab order integer, or 1 if no active tab
  get_active_tab_order <- function() {
    if (is.null(tabState$activeTab) || length(tabState$tabs) == 0) return(1)
    
    active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
    if (length(active_idx) == 0) return(1)
    
    return(tabState$tabs[[active_idx]]$order)
  }
  
  #' Detect duplicate elements in current data
  #' 
  #' Identifies rows that have duplicate values in Element or castor_kolom columns (non-empty)
  #' 
  #' @param data data.table to check for duplicates
  #' @return List with duplicate_rows_element, duplicate_values_element, duplicate_rows_castor, duplicate_values_castor
  detect_duplicate_elements <- function(data) {
    result <- list(
      duplicate_rows_element = integer(0),
      duplicate_values_element = character(0),
      duplicate_rows_castor = integer(0),
      duplicate_values_castor = character(0)
    )
    
    if (is.null(data) || nrow(data) == 0) {
      return(result)
    }
    
    # Check for duplicate Elements
    if ("Element" %in% names(data)) {
      valid_elements <- data$Element[!is.na(data$Element) & data$Element != ""]
      if (length(valid_elements) > 0) {
        element_counts <- table(valid_elements)
        duplicate_values_element <- names(element_counts[element_counts > 1])
        
        if (length(duplicate_values_element) > 0) {
          result$duplicate_rows_element <- which(data$Element %in% duplicate_values_element & 
                                                  !is.na(data$Element) & data$Element != "")
          result$duplicate_values_element <- duplicate_values_element
        }
      }
    }
    
    # Check for duplicate Castor Names (castor_kolom)
    if ("castor_kolom" %in% names(data)) {
      valid_castor <- data$castor_kolom[!is.na(data$castor_kolom) & data$castor_kolom != ""]
      if (length(valid_castor) > 0) {
        castor_counts <- table(valid_castor)
        duplicate_values_castor <- names(castor_counts[castor_counts > 1])
        
        if (length(duplicate_values_castor) > 0) {
          result$duplicate_rows_castor <- which(data$castor_kolom %in% duplicate_values_castor & 
                                                !is.na(data$castor_kolom) & data$castor_kolom != "")
          result$duplicate_values_castor <- duplicate_values_castor
        }
      }
    }
    
    return(result)
  }
  
  #' Get data for selected rows from active tab
  #' 
  #' @param selected_indices Vector of row indices (1-based)
  #' @return data.table with selected rows, or NULL if invalid
  get_selected_rows_data <- function(selected_indices) {
    active_data <- get_active_tab_data()
    
    if (is.null(active_data) || length(selected_indices) == 0) return(NULL)
    if (max(selected_indices) > nrow(active_data)) return(NULL)
    
    return(active_data[selected_indices, , drop = FALSE])
  }
  
  #' Get related checkbox data for given element values
  #' 
  #' When copying/cutting elements, also copy their checkbox mappings
  #' Filters by Element values and tab metadata (if present)
  #' 
  #' @param element_values Vector of Element column values
  #' @param source_tab_name Tab name to filter by
  #' @param source_tab_order Tab order to filter by
  #' @return data.table with matching checkbox rows, or NULL if none found
  get_related_checkbox_data <- function(element_values, source_tab_name, source_tab_order) {
    checkbox_data <- mappingData[["waarde_checkboxes"]]
    
    if (is.null(checkbox_data) || nrow(checkbox_data) == 0) return(NULL)
    if (length(element_values) == 0) return(NULL)
    
    # Filter based on whether tab metadata exists
    if (!("tab_name_meta" %in% names(checkbox_data))) {
      # No tab metadata - filter by Element only
      filtered <- checkbox_data[Element %in% element_values]
    } else {
      # Has tab metadata - filter by Element AND tab info
      filtered <- checkbox_data[Element %in% element_values &
                                tab_name_meta == source_tab_name &
                                tab_order_meta == source_tab_order]
    }
    
    if (nrow(filtered) == 0) return(NULL)
    return(filtered)
  }
  
  #' Get related radiobutton data for given element values
  #' 
  #' When copying/cutting elements, also copy their radiobutton mappings
  #' Filters by Element values and tab metadata (if present)
  #' 
  #' @param element_values Vector of Element column values
  #' @param source_tab_name Tab name to filter by
  #' @param source_tab_order Tab order to filter by
  #' @return data.table with matching radiobutton rows, or NULL if none found
  get_related_radiobutton_data <- function(element_values, source_tab_name, source_tab_order) {
    radio_data <- mappingData[["waarde_radiobuttons"]]
    
    if (is.null(radio_data) || nrow(radio_data) == 0) return(NULL)
    if (length(element_values) == 0) return(NULL)
    
    # Filter based on whether tab metadata exists
    if (!("tab_name_meta" %in% names(radio_data))) {
      # No tab metadata - filter by Element only
      filtered <- radio_data[Element %in% element_values]
    } else {
      # Has tab metadata - filter by Element AND tab info
      filtered <- radio_data[Element %in% element_values & 
                             tab_name_meta == source_tab_name & 
                             tab_order_meta == source_tab_order]
    }
    
    if (nrow(filtered) == 0) return(NULL)
    return(filtered)
  }
  
  #' Clear clipboard state
  #' 
  #' Resets all clipboard reactive values to NULL. Called after paste operation
  #' completes or when user explicitly clears the clipboard.
  clear_clipboard <- function() {
    clipboardState$data <- NULL                    # Copied/cut element rows
    clipboardState$source_table <- NULL            # Source table name
    clipboardState$source_tab <- NULL              # Source tab index
    clipboardState$source_tab_name <- NULL         # Source tab name
    clipboardState$source_tab_order <- NULL        # Source tab order
    clipboardState$source_row_indices <- NULL      # Original row positions
    clipboardState$operation <- NULL               # "copy" or "cut"
    clipboardState$related_checkboxes <- NULL      # Associated checkbox mappings
    clipboardState$related_radiobuttons <- NULL    # Associated radiobutton mappings
    clipboardState$timestamp <- NULL               # Timestamp of copy/cut operation
  }
  
  # ============================================================================
  # BULK ROW MOVE - HELPER FUNCTIONS
  # ============================================================================
  # This section provides functionality to move multiple selected rows to a new
  # position within the table, maintaining their relative order. Similar to
  # copy/paste, bulk move is only available for the 'elements' table to ensure
  # data integrity with related checkbox/radiobutton tables.
  
  #' Check if bulk move is available for current table and selection
  #' 
  #' Validates that bulk move operation can be performed. Only the 'elements'
  #' table supports bulk move to prevent orphaned data in related tables.
  #' 
  #' @param table_name Name of the currently active table
  #' @param selected_indices Vector of selected row indices
  #' @return List with enabled (TRUE/FALSE) and message (error text if FALSE)
  is_bulk_move_enabled <- function(table_name, selected_indices) {
    # Check if table supports bulk move (same requirement as copy/paste)
    if (is.null(table_name) || table_name != "elements") {
      return(list(
        enabled = FALSE,
        message = "Bulk move is only available for the elements table"
      ))
    }
    
    # Check if any rows are selected
    if (is.null(selected_indices) || length(selected_indices) == 0) {
      return(list(
        enabled = FALSE,
        message = "No rows selected"
      ))
    }
    
    # Check if selection is valid
    active_data <- get_active_tab_data()
    if (is.null(active_data)) {
      return(list(
        enabled = FALSE,
        message = "No active table data found"
      ))
    }
    
    # Check if all indices are within bounds
    if (max(selected_indices) > nrow(active_data)) {
      return(list(
        enabled = FALSE,
        message = "Invalid row selection"
      ))
    }
    
    return(list(enabled = TRUE, message = NULL))
  }
  
  #' Calculate new positions for bulk move operation
  #' 
  #' Determines where each selected row will be placed after the move. The
  #' selected rows are moved as a block to the target position, maintaining
  #' their relative order. Performs bounds checking to ensure valid positions.
  #' 
  #' @param selected_indices Vector of current row indices (will be sorted)
  #' @param target_position Where the first selected row should move to
  #' @param total_rows Total number of rows in the table
  #' @return List with:
  #'   - valid: TRUE if move is possible, FALSE otherwise
  #'   - message: Error message if not valid
  #'   - new_positions: Vector of new row positions for each selected row
  #'   - target_position: Adjusted target position (may differ from input)
  #'   - final_range: String representation of new position range (e.g., "5-8")
  #'   - n_selected: Number of selected rows
  calculate_bulk_move_positions <- function(selected_indices, target_position, total_rows) {
    # Validate inputs
    if (is.null(selected_indices) || length(selected_indices) == 0) {
      return(list(valid = FALSE, message = "No rows selected"))
    }
    
    if (is.null(target_position) || is.na(target_position)) {
      return(list(valid = FALSE, message = "Invalid target position"))
    }
    
    # Ensure selected_indices is sorted
    selected_indices <- sort(unique(selected_indices))
    n_selected <- length(selected_indices)
    
    # Validate and adjust target position if needed
    target_position <- as.integer(target_position)
    if (target_position < 1) target_position <- 1
    if (target_position > total_rows) target_position <- total_rows
    
    # Calculate new positions
    # Selected rows are moved as a block to target_position
    # They maintain their relative order
    new_positions <- target_position:(target_position + n_selected - 1)
    
    # Ensure we don't go out of bounds
    if (max(new_positions) > total_rows) {
      # Adjust so last row ends at position total_rows
      new_positions <- (total_rows - n_selected + 1):total_rows
      target_position <- new_positions[1]
    }
    
    # Create display string for the new range
    if (n_selected == 1) {
      final_range <- as.character(new_positions[1])
    } else {
      final_range <- sprintf("%d-%d", new_positions[1], new_positions[n_selected])
    }
    
    return(list(
      valid = TRUE,
      message = NULL,
      new_positions = new_positions,
      target_position = target_position,
      final_range = final_range,
      n_selected = n_selected
    ))
  }
  
  #' Perform bulk move operation on data.table
  #' 
  #' Physically reorders the data.table by moving selected rows to a new position.
  #' Algorithm:
  #'   1. Extract selected rows (maintain order)
  #'   2. Remove selected rows from original data
  #'   3. Adjust target position (accounting for removed rows)
  #'   4. Insert selected rows at adjusted position
  #' 
  #' @param data data.table to reorder
  #' @param selected_indices Vector of current row indices (sorted)
  #' @param target_position Where the first row should move to (validated)
  #' @return Reordered data.table
  perform_bulk_move <- function(data, selected_indices, target_position) {
    if (is.null(data) || nrow(data) == 0) return(data)
    if (is.null(selected_indices) || length(selected_indices) == 0) return(data)
    
    # Ensure selected_indices is sorted and unique
    selected_indices <- sort(unique(selected_indices))
    n_selected <- length(selected_indices)
    total_rows <- nrow(data)
    
    # Validate target position
    target_position <- as.integer(target_position)
    if (target_position < 1) target_position <- 1
    if (target_position > total_rows) target_position <- total_rows
    
    # If target position would exceed maximum, adjust
    if (target_position + n_selected - 1 > total_rows) {
      target_position <- total_rows - n_selected + 1
    }
    
    # Step 1: Extract selected rows (preserve order)
    selected_rows <- data[selected_indices, ]
    
    # Step 2: Remove selected rows
    remaining_data <- data[-selected_indices, ]
    
    # Step 3: Calculate adjusted target position
    # If we remove rows before the target position,
    # we need to adjust the target position downward
    rows_before_target <- sum(selected_indices < target_position)
    adjusted_target <- target_position - rows_before_target
    
    # Ensure adjusted_target remains valid
    if (adjusted_target < 1) adjusted_target <- 1
    if (adjusted_target > nrow(remaining_data) + 1) adjusted_target <- nrow(remaining_data) + 1
    
    # Step 4: Insert selected rows at new position
    if (adjusted_target == 1) {
      # Insert at beginning
      result <- rbind(selected_rows, remaining_data)
    } else if (adjusted_target > nrow(remaining_data)) {
      # Insert at end
      result <- rbind(remaining_data, selected_rows)
    } else {
      # Insert in middle
      top_part <- remaining_data[1:(adjusted_target - 1), ]
      bottom_part <- remaining_data[adjusted_target:nrow(remaining_data), ]
      result <- rbind(top_part, selected_rows, bottom_part)
    }
    
    return(result)
  }
  
  # ============================================================================
  # CLIPBOARD STATE
  # ============================================================================
  # Reactive values tracking copy/cut/paste operations. Stores copied/cut data
  # along with related checkbox and radiobutton mappings for elements table.
  
  clipboardState <- reactiveValues(
    data = NULL,                    # data.table with copied rows
    source_table = NULL,            # "elements", "waarde_checkboxes", "waarde_radiobuttons"
    source_tab = NULL,              # ID of source tab (e.g., "tab_1")
    source_tab_name = NULL,         # Name of source tab (e.g., "Main")
    source_tab_order = NULL,        # Order of source tab (e.g., 1)
    source_row_indices = NULL,      # Integer vector with original row indices (for cut)
    operation = NULL,               # "copy" or "cut"
    related_checkboxes = NULL,      # data.table with related checkbox mappings (if elements)
    related_radiobuttons = NULL,    # data.table with related radiobutton mappings (if elements)
    timestamp = NULL                # Timestamp of copy/cut operation
  )
  
  # ============================================================================
  # BULK MOVE STATE
  # ============================================================================
  # Reactive values for bulk row move operations. Allows moving multiple
  # selected rows to a new position while maintaining their relative order.
  
  bulkMoveState <- reactiveValues(
    pending = FALSE,                # Whether a bulk move action is pending
    selected_indices = NULL,        # Original selected row indices
    target_position = NULL,         # Target position for first row
    preview_data = NULL,            # Preview of new ordering
    total_rows = NULL               # Total number of rows in table
  )
  
  # Trigger to force related data updates (for reactivity timing fix)
  relatedDataUpdated <- reactiveVal(0)
  
  # Trigger to force table UI refresh after data changes
  forceTableRefresh <- reactiveVal(0)
  
  # Track duplicate elements for validation
  duplicateElements <- reactiveVal(list())
  
  # ============================================================================
  # TAB BUTTONS RENDERING
  # ============================================================================
  # Renders tab navigation buttons in the footer. Each tab shows its name and
  # row count. Active tab is highlighted. Tabs can be closed (if more than 1)
  # and double-clicked to rename (handled by JavaScript).
  
  output$tab_buttons <- renderUI({
    tabs <- tabState$tabs
    if (length(tabs) == 0) return(NULL)
    
    # Create button for each tab
    tab_buttons <- lapply(seq_along(tabs), function(i) {
      tab <- tabs[[i]]
      is_active <- identical(tab$id, tabState$activeTab)
      row_count <- nrow(tab$data)
      
      tags$button(
        class = paste("tab-button", if(is_active) "active" else ""),
        `data-tab-id` = tab$id,
        # Tab click handled by JavaScript event delegation (appJS.js)
        
        # Tab name (double-click for rename handled by JavaScript)
        tags$span(class = "tab-name", tab$name),
        tags$span(class = "tab-row-count", sprintf("(%d)", row_count)),
        
        # Close button (only show if more than 1 tab exists)
        if (length(tabs) > 1) {
          tags$span(
            class = "tab-close-btn",
            onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('close_tab', '%s', {priority: 'event'})", tab$id),
            "×"
          )
        } else NULL
      )
    })
    
    # Return as div with left alignment
    tags$div(style = "display: flex; gap: 2px; justify-content: flex-start; text-align: left;", 
             tab_buttons)
  })

  # ============================================================================
  # LOADING SCREEN: Hide when app is ready
  # ============================================================================
  # Tracks application initialization. Loading screen is hidden once the
  # DataTable is fully rendered and interactive (triggered from JavaScript).
  
  app_ready <- reactiveVal(FALSE)
  
  # Hide loading screen once the table is fully initialized
  # This is triggered from JavaScript after the DataTable draw event
  observeEvent(input$table_ready, {
    # Ensure this only runs once
    if (!app_ready() && isTRUE(input$table_ready)) {
      cat("[Startup] Table ready signal received, hiding loading screen\n")
      # Send message to JavaScript to hide loading screen
      session$sendCustomMessage("hideLoadingScreen", list())
      
      # Mark app as ready
      app_ready(TRUE)
    }
  })

  # ============================================================================
  # MISSING FILE UPLOAD MODAL
  # ============================================================================
  # When export scripts (baseline/follow-up/biobank) fail due to missing input
  # files, this observer shows a modal dialog allowing users to upload the
  # missing file. File validation checks for expected columns based on script
  # type and mapping database configuration.
  
  # Observe changes in missing_file_info and show the modal when needed
  observeEvent(runnerState$missing_file_info, {
    info <- runnerState$missing_file_info
    
    # Only close modal if info is NULL AND no process is running
    if (is.null(info)) {
      # Don't close the modal if a process is running (because we just started it)
      if (is.null(runnerState$proc) || is.null(runnerState$kind)) {
        removeModalSafe()
      }
      return()
    }
    
    # Close the current runner modal first
    removeModalSafe()
    
    # Reset validation status and UI
    file_validation_result(NULL)
    output$upload_status_ui <- renderUI({ NULL })
    
    # Small delay to ensure the old modal is closed before opening the new one
    later::later(function() {
      showModalSafe(
        modalDialog(
          title = "Required file not found",
          p(
            "The script could not find the following file:",
            br(),
            code(info$expected_path)
          ),
          p("Select the correct file to continue."),
          fileInput("upload_missing_file", "Select file", accept = c(".csv", ".xlsx")),
          uiOutput("upload_status_ui"),
          footer = tagList(
            actionButton("confirm_upload_missing_file", "Upload and continue"),
            modalButton("Cancel")
          ),
          easyClose = FALSE
        )
      )
    }, delay = 0.1)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Reactive value to store file validation result
  file_validation_result <- reactiveVal(NULL)
  
  # Observe file validation result to enable/disable upload button
  observeEvent(file_validation_result(), {
    result <- file_validation_result()
    if (is.null(result) || isFALSE(result)) {
      shinyjs::disable("confirm_upload_missing_file")
    } else if (isTRUE(result)) {
      shinyjs::enable("confirm_upload_missing_file")
    }
  }, ignoreNULL = FALSE)
  
  # Validate uploaded file as soon as it's selected
  # Checks:
  #   - File is not empty
  #   - File can be read (CSV/XLSX)
  #   - File contains expected columns based on script type and mapping database
  observeEvent(input$upload_missing_file, ignoreInit = TRUE, {
    req(runnerState$missing_file_info)
    
    uploaded_file <- input$upload_missing_file
    info <- runnerState$missing_file_info
    
    if (is.null(uploaded_file)) {
      output$upload_status_ui <- renderUI({ NULL })
      file_validation_result(NULL)
      return()
    }
    
    # Show validation status
    output$upload_status_ui <- renderUI({ p("Validating...", style = "color: blue;") })
    
    # Basic validation: file must contain data
    is_valid <- is.data.frame(uploaded_file) && nrow(uploaded_file) > 0 && uploaded_file$size > 0
    
    if (is_valid) {
      # Read file to check columns
      tryCatch({
        # Determine which columns are expected based on script type
        expected_cols <- NULL
        file_ext <- tolower(tools::file_ext(uploaded_file$name))
        
        # Read the file
        if (file_ext == "csv") {
          test_data <- readr::read_csv2(uploaded_file$datapath, n_max = 1, col_types = cols(), 
                                        show_col_types = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."))
        } else if (file_ext == "xlsx") {
          test_data <- readxl::read_excel(uploaded_file$datapath, n_max = 1)
        } else {
          stop("Invalid file type. Only .csv and .xlsx are supported.")
        }
        
        # Determine expected columns based on epic_tabel from mapping database
        if (info$script_type == "baseline") {
          # For baseline we expect EpicExport columns
          variabelen_baseline <- dbGetQuery(con, 
            "SELECT DISTINCT epic_kolom FROM variabelen WHERE epic_tabel = 'EpicExport' AND epic_kolom != ''")
          expected_cols <- variabelen_baseline$epic_kolom
        } else if (info$script_type == "biobank") {
          # For biobank we expect biobank_data or MDNS columns depending on the file
          if (grepl("biobank", basename(info$expected_path), ignore.case = TRUE)) {
            variabelen_biobank <- dbGetQuery(con, 
              "SELECT DISTINCT epic_kolom FROM variabelen WHERE epic_tabel = 'biobank_data' AND epic_kolom != ''")
            expected_cols <- variabelen_biobank$epic_kolom
          } else if (grepl("MDNS", basename(info$expected_path), ignore.case = TRUE)) {
            variabelen_mdns <- dbGetQuery(con, 
              "SELECT DISTINCT epic_kolom FROM variabelen WHERE epic_tabel = 'MDNS' AND epic_kolom != ''")
            expected_cols <- variabelen_mdns$epic_kolom
          }
        }
        
        # Check if expected columns are present
        if (!is.null(expected_cols) && length(expected_cols) > 0) {
          actual_cols <- colnames(test_data)
          missing_cols <- setdiff(expected_cols, actual_cols)
          
          if (length(missing_cols) > 0) {
            # Validation failed: columns missing
            output$upload_status_ui <- renderUI({
              div(
                p("Validation failed. The following columns are missing:", style = "color: red; font-weight: bold;"),
                tags$ul(
                  lapply(missing_cols, function(col) tags$li(code(col)))
                ),
                p("Expected columns:", style = "margin-top: 10px;"),
                p(paste(expected_cols, collapse = ", "), style = "font-size: 0.9em; color: #666;")
              )
            })
            file_validation_result(FALSE)
          } else {
            # Validation succeeded
            output$upload_status_ui <- renderUI({
              p(icon("check-circle"), " File is valid and can be uploaded.", style = "color: green; font-weight: bold;")
            })
            file_validation_result(TRUE)
          }
        } else {
          # No specific columns to check, accept the file
          output$upload_status_ui <- renderUI({
            p(icon("check-circle"), " File is valid and can be uploaded.", style = "color: green; font-weight: bold;")
          })
          file_validation_result(TRUE)
        }
        
      }, error = function(e) {
        # Error reading the file
        output$upload_status_ui <- renderUI({
          p(paste("Error reading file:", conditionMessage(e)), style = "color: red;")
        })
        file_validation_result(FALSE)
      })
    } else {
      output$upload_status_ui <- renderUI({
        p("Validation failed. File appears to be empty.", style = "color: red;")
      })
      file_validation_result(FALSE)
      shinyjs::disable("confirm_upload_missing_file")
    }
  })
  
  # ============================================================================
  # FILE UPLOAD CONFIRMATION
  # ============================================================================
  # When user confirms the upload of a missing file, this observer:
  #   1. Copies uploaded file to expected location
  #   2. Closes upload modal
  #   3. Cleans up old process/observer
  #   4. Restarts the script with the newly uploaded file
  
  observeEvent(input$confirm_upload_missing_file, {
    req(input$upload_missing_file, runnerState$missing_file_info, file_validation_result())
    
    # Validation already done, only copy if valid
    if (!isTRUE(file_validation_result())) {
      return()
    }
    
    info <- runnerState$missing_file_info
    uploaded_file <- input$upload_missing_file
    
    # Copy the file to the correct location
    destination_dir <- dirname(info$expected_path)
    if (!dir.exists(destination_dir)) {
      dir.create(destination_dir, recursive = TRUE)
    }
    
    file.copy(uploaded_file$datapath, info$expected_path, overwrite = TRUE)
      
      # Close the upload modal immediately
      removeModalSafe()
      
      # Isolate reactive values before entering later() callback
      # This prevents reactive dependencies from affecting the deferred execution
      old_observer <- isolate(runnerState$observer)
      old_proc <- isolate(runnerState$proc)
      
      # Start the script with a slight delay to allow modal to close smoothly
      later::later(function() {
        # Clean up old observer if it's still running
        if (!is.null(old_observer)) {
          tryCatch({
            old_observer$destroy()
          }, error = function(e) {})
          runnerState$observer <- NULL
        }
        
        # Kill old process if it's still alive
        if (!is.null(old_proc)) {
          tryCatch({
            if (old_proc$is_alive()) {
              old_proc$kill()
            }
          }, error = function(e) {})
          runnerState$proc <- NULL
        }
        
        # ====================================================================
        # SCRIPT RESTART LOGIC AFTER FILE UPLOAD
        # ====================================================================
        # After a user uploads a missing file (e.g., EpicExport.csv, biobank_data.csv),
        # we need to restart the script that was waiting for that file.
        #
        # This section:
        # 1. Determines which script to restart based on info$script_type
        # 2. Sets up a progress modal with progress bar and status line
        # 3. Launches the script in external R process (processx)
        # 4. Creates an observer to monitor process output and status.json
        # 5. Handles completion/errors and calls on_complete handler if provided
        #
        # Supported script types:
        # - "baseline": Runs scripts/baseline/baseline.r
        # - "biobank": Runs scripts/biobank_data/biobank_data.r
        # - "follow_up": Runs scripts/follow_up/follow_up.r
        #
        # Progress monitoring:
        # - Reads status.json file written by the external script
        # - Updates progress bar based on current/total or percent
        # - Displays step, detail, and ETA information
        # - Detects "EPIC2CASTOR::DONE" marker for successful completion
        # - Handles errors and missing file detections
        
        # Replace the upload modal with the appropriate running script modal
        if (!is.null(info$script_type)) {
            if (info$script_type == "baseline") {
              # ================================================================
              # BASELINE SCRIPT RESTART
              # ================================================================
              # Show progress modal for baseline processing
              showModalSafe(modalDialog(
                title = "Running Baseline",
                tagList(
                  uiOutput("baseline_progressbar"),
                  div(style = "margin-top:8px;", textOutput("baseline_status_line"))
                ),
                footer = uiOutput("runner_footer"),
                easyClose = FALSE,
                size = "l"
              ))
              
              # Initialize runner state for baseline
              updateRunnerFooter(FALSE)
              runnerState$canceled <- FALSE
              {
                # Clean up any existing status.json from previous runs
                run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
                status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"
                if (file.exists(status_path)) try(file.remove(status_path), silent = TRUE)
              }
              
              # Launch baseline.r script in external R process
              scriptPath <- file.path(epc_path("baseline_scripts_dir"), "baseline.r")
              proc <- process$new(
                "Rscript",
                args = c(scriptPath),
                stdout = "|",
                stderr = "|",
                env = c(EPIC2CASTOR_LOGDIR = getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", "")))
              )
              runnerState$proc <- proc
              runnerState$kind <- "baseline"
              runnerState$on_complete_handler <- info$on_complete
              runnerState$error_detected <- FALSE
              runnerState$error_message <- NULL
              
              run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
              status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"
              
              # Initialize progress bar at 0%
              output$baseline_progressbar <- renderUI({
                pct <- 0
                div(class = "progress", style = "height: 20px; background:#eee;",
                    div(class = "progress-bar progress-bar-striped active", role = "progressbar",
                        style = paste0("width: ", pct, "%;"),
                        `aria-valuemin` = 0, `aria-valuemax` = 100,
                        paste0(pct, "%"))
                )
              })
              output$baseline_status_line <- renderText({ "Starting…" })
              
              # ============================================================
              # BASELINE PROGRESS MONITORING OBSERVER
              # ============================================================
              # This observer runs every 500ms to:
              # - Read process stdout/stderr for error detection
              # - Parse status.json for progress updates (current/total/percent/step/detail/eta)
              # - Update progress bar and status line
              # - Detect process completion and handle success/failure
              # - Call on_complete handler if script succeeds
              
              observerHandle <- observe({
                reactiveTimer(500)()  # Poll every 500ms
                tryCatch({
                  # Read process output for logging and error detection
                  out_lines <- character(0)
                  err_lines <- character(0)
                  try({
                    out_lines <- proc$read_output_lines()
                    err_lines <- proc$read_error_lines()
                  }, silent = TRUE)
                  if (length(out_lines)) lapply(out_lines, function(x) cat(paste0("[Baseline][OUT] ", x, "\n")))
                  if (length(err_lines)) lapply(err_lines, function(x) cat(paste0("[Baseline][ERR] ", x, "\n")))
                  
                  # Combine output for error detection
                  combined_lines <- c(out_lines, err_lines)
                  if (length(combined_lines)) {
                    err_line <- detect_error_line(combined_lines)
                    if (!is.null(err_line)) update_runner_error(err_line, "baseline_status_line", "baseline_progressbar")
                  }
                  
                  # Parse status.json for progress updates
                  if (file.exists(status_path)) {
                    st <- try(jsonlite::fromJSON(status_path), silent = TRUE)
                    if (!inherits(st, "try-error") && is.list(st)) {
                      # Defensively parse numeric fields (current/total/percent)
                      cur <- suppressWarnings(as.numeric(st$current))
                      tot <- suppressWarnings(as.numeric(st$total))
                      pct <- suppressWarnings(as.numeric(st$percent))
                      if (length(cur) != 1L || is.na(cur) || !is.finite(cur)) cur <- NA_real_
                      if (length(tot) != 1L || is.na(tot) || !is.finite(tot) || tot <= 0) tot <- NA_real_
                      if (length(pct) != 1L || is.na(pct) || !is.finite(pct)) pct <- NA_real_
                      
                      # Prefer computed percent from current/total if available
                      if (is.finite(cur) && is.finite(tot)) pct <- 100 * cur / tot
                      if (is.na(pct)) pct <- 0
                      if (pct < 0) pct <- 0
                      if (pct > 100) pct <- 100
                      
                      # Parse step and detail strings
                      step <- st$step
                      if (is.null(step) || length(step) != 1L || !nzchar(as.character(step))) step <- "running" else step <- as.character(step)
                      step_lc <- tolower(step)
                      if (!is.na(step_lc) && identical(step_lc, "done")) updateRunnerFooter(TRUE)
                      detail <- st$detail
                      if (is.null(detail) || length(detail) == 0L) detail <- "" else detail <- as.character(detail)[1]
                      
                      # Parse ETA (estimated time remaining in seconds)
                      eta_val <- suppressWarnings(as.numeric(st$eta_s))
                      if (!is.null(eta_val) && length(eta_val) == 1L && is.finite(eta_val)) {
                        eta_txt <- paste0("ETA ", format(round(eta_val), scientific = FALSE), "s")
                      } else eta_txt <- NULL
                      
                      # Update progress bar with current progress
                      if (!runnerState$error_detected) {
                        output$baseline_progressbar <- renderUI({
                          div(class = "progress", style = "height: 20px; background:#eee;",
                              div(class = "progress-bar progress-bar-striped active", role = "progressbar",
                                  style = paste0("width: ", pct, "%;"),
                                  `aria-valuemin` = 0, `aria-valuemax` = 100,
                                  paste0(round(pct), "%"))
                          )
                        })
                      }
                      
                      # Construct status line: "step - detail (ETA Xs)"
                      line <- paste(step, if (nzchar(detail)) paste("-", detail) else "", if (!is.null(eta_txt)) paste("(", eta_txt, ")") else "")
                      if (!runnerState$error_detected) output$baseline_status_line <- renderText({ line })
                    }
                  }
                  
                  # Check if process has completed
                  if (!proc$is_alive()) {
                    updateRunnerFooter(TRUE)  # Show "Close" button
                    
                    # Check for successful completion marker
                    script_completed_successfully <- FALSE
                    
                    try({
                      # Drain any remaining output
                      leftover_out <- proc$read_output_lines()
                      leftover_err <- proc$read_error_lines()
                      if (length(leftover_out)) lapply(leftover_out, function(x) cat(paste0("[Baseline][OUT] ", x, "\n")))
                      if (length(leftover_err)) lapply(leftover_err, function(x) cat(paste0("[Baseline][ERR] ", x, "\n")))
                      leftover_lines <- c(leftover_out, leftover_err)
                      
                      # Check for "EPIC2CASTOR::DONE" marker indicating success
                      if (any(grepl("EPIC2CASTOR::DONE", leftover_lines, fixed = TRUE))) {
                        script_completed_successfully <- TRUE
                      }
                      
                      # Check for errors in remaining output
                      if (length(leftover_lines)) {
                        err_line <- detect_error_line(leftover_lines)
                        if (!is.null(err_line)) update_runner_error(err_line, "baseline_status_line", "baseline_progressbar")
                      }
                    }, silent = TRUE)
                    
                    # Get process exit status
                    exit_status <- tryCatch(proc$get_exit_status(), error = function(e) NA_integer_)
                    if (is.null(exit_status)) exit_status <- NA_integer_
                    
                    # Check if a missing file was detected (upload modal will appear)
                    has_missing_file <- !is.null(isolate(runnerState$missing_file_info))
                    
                    # Report error only if:
                    # - Script didn't complete successfully AND
                    # - No error was already detected AND
                    # - No missing file was detected (that would trigger upload modal)
                    if (!script_completed_successfully && (is.na(exit_status) || exit_status != 0) && !runnerState$error_detected && !has_missing_file) {
                      msg <- sprintf("Baseline failed (exit %s). Check logs for details.", as.character(exit_status))
                      update_runner_error(msg, "baseline_status_line", "baseline_progressbar")
                    }
                    
                    # Show notification for non-zero exit (unless missing file detected)
                    if (!script_completed_successfully && !is.na(exit_status) && exit_status != 0 && !has_missing_file) {
                      safeNotify(sprintf("Baseline script exited with status %s", as.character(exit_status)), "error")
                    }
                    
                    # Clean up observer and process
                    observerHandle$destroy()
                    runnerState$proc <- NULL
                    
                    # Call on_complete handler if script succeeded
                    if (!isTRUE(runnerState$canceled) && (script_completed_successfully || (!is.na(exit_status) && exit_status == 0)) && !is.null(info$on_complete)) {
                      later::later(info$on_complete, delay = 0.05)
                    }
                  }
                }, error = function(e) {
                  output$baseline_status_line <- renderText({ paste("Monitor error:", conditionMessage(e)) })
                  updateRunnerFooter(TRUE)
                  observerHandle$destroy()
                  if (!isTRUE(runnerState$canceled) && !is.null(info$on_complete)) later::later(info$on_complete, delay = 0.05)
                })
              })
              runnerState$observer <- observerHandle
              
            } else if (info$script_type == "biobank") {
              # Replace modal with biobank running modal
              showModalSafe(modalDialog(
                title = "Running Biobank Data",
                tagList(
                  uiOutput("biobank_progressbar"),
                  div(style = "margin-top:8px;", textOutput("biobank_status_line"))
                ),
                footer = uiOutput("runner_footer"),
                easyClose = FALSE,
                size = "l"
              ))
              
              # Start the biobank script (without removeModalSafe/showModalSafe)
              updateRunnerFooter(FALSE)
              {
                run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
                status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"
                if (file.exists(status_path)) try(file.remove(status_path), silent = TRUE)
              }
              runnerState$canceled <- FALSE
              
              scriptPath <- file.path(epc_path("scripts_dir"), "biobank_data", "biobank_data.r")
              proc <- process$new(
                "Rscript",
                args = c(scriptPath),
                stdout = "|",
                stderr = "|",
                env = c(EPIC2CASTOR_LOGDIR = getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", "")))
              )
              runnerState$proc <- proc
              runnerState$kind <- "biobank_data"
              runnerState$on_complete_handler <- info$on_complete
              runnerState$error_detected <- FALSE
              runnerState$error_message <- NULL
              
              run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
              status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"
              
              output$biobank_progressbar <- renderUI({
                div(class = "progress", style = "height: 20px; background:#eee;",
                    div(class = "progress-bar progress-bar-striped active", role = "progressbar",
                        style = "width: 0%;",
                        `aria-valuemin` = 0, `aria-valuemax` = 100,
                        "0%")
                )
              })
              output$biobank_status_line <- renderText({ "Starting…" })
              
              # Create and register the observer
              observerHandle <- observe({
                reactiveTimer(500)()
                tryCatch({
                  out_lines <- character(0)
                  err_lines <- character(0)
                  try({
                    out_lines <- proc$read_output_lines()
                    err_lines <- proc$read_error_lines()
                  }, silent = TRUE)
                  if (length(out_lines)) {
                    lapply(out_lines, function(x) cat(paste0("[Biobank][OUT] ", x, "\n")))
                  }
                  if (length(err_lines)) {
                    lapply(err_lines, function(x) cat(paste0("[Biobank][ERR] ", x, "\n")))
                  }
                  
                  # ===== ERROR DETECTION =====
                  # Check combined output for error messages and update UI if found
                  combined_lines <- c(out_lines, err_lines)
                  if (length(combined_lines)) {
                    err_line <- detect_error_line(combined_lines)
                    if (!is.null(err_line)) {
                      update_runner_error(err_line, "biobank_status_line", "biobank_progressbar")
                    }
                  }
                  
                  if (file.exists(status_path)) {
                    st <- try(jsonlite::fromJSON(status_path), silent = TRUE)
                    if (!inherits(st, "try-error") && is.list(st)) {
                      cur <- suppressWarnings(as.numeric(st$current))
                      tot <- suppressWarnings(as.numeric(st$total))
                      pct <- suppressWarnings(as.numeric(st$percent))
                      if (length(cur) != 1L || is.na(cur) || !is.finite(cur)) cur <- NA_real_
                      if (length(tot) != 1L || is.na(tot) || !is.finite(tot) || tot <= 0) tot <- NA_real_
                      if (length(pct) != 1L || is.na(pct) || !is.finite(pct)) pct <- NA_real_
                      if (is.finite(cur) && is.finite(tot)) pct <- 100 * cur / tot
                      if (is.na(pct)) pct <- 0
                      if (pct < 0) pct <- 0
                      if (pct > 100) pct <- 100
                      
                      step <- st$step
                      if (is.null(step) || length(step) != 1L || !nzchar(as.character(step))) step <- "running" else step <- as.character(step)
                      step_lc <- tolower(step)
                      if (!is.na(step_lc) && identical(step_lc, "done")) updateRunnerFooter(TRUE)
                      detail <- st$detail
                      if (is.null(detail) || length(detail) == 0L) detail <- "" else detail <- as.character(detail)[1]
                      eta_val <- suppressWarnings(as.numeric(st$eta_s))
                      if (!is.null(eta_val) && length(eta_val) == 1L && is.finite(eta_val)) {
                        eta_txt <- paste0("ETA ", format(round(eta_val), scientific = FALSE), "s")
                      } else eta_txt <- NULL
                      
                      if (!runnerState$error_detected) {
                        output$biobank_progressbar <- renderUI({
                          div(class = "progress", style = "height: 20px; background:#eee;",
                              div(class = "progress-bar progress-bar-striped active", role = "progressbar",
                                  style = paste0("width: ", pct, "%;"),
                                  `aria-valuemin` = 0, `aria-valuemax` = 100,
                                  paste0(round(pct), "%"))
                          )
                        })
                      }
                      line <- paste(step, if (nzchar(detail)) paste("-", detail) else "", if (!is.null(eta_txt)) paste("(", eta_txt, ")") else "")
                      if (!runnerState$error_detected) output$biobank_status_line <- renderText({ line })
                    }
                  }
                  
                  # ===== PROCESS COMPLETION HANDLING =====
                  # When process finishes, read remaining output and check exit status
                  if (!proc$is_alive()) {
                    updateRunnerFooter(TRUE)
                    
                    # Read any remaining output lines after process completion
                    try({
                      leftover_out <- proc$read_output_lines()
                      leftover_err <- proc$read_error_lines()
                      if (length(leftover_out)) lapply(leftover_out, function(x) cat(paste0("[Biobank][OUT] ", x, "\n")))
                      if (length(leftover_err)) lapply(leftover_err, function(x) cat(paste0("[Biobank][ERR] ", x, "\n")))
                      
                      # Check leftover output for errors
                      leftover_lines <- c(leftover_out, leftover_err)
                      if (length(leftover_lines)) {
                        err_line <- detect_error_line(leftover_lines)
                        if (!is.null(err_line)) {
                          update_runner_error(err_line, "biobank_status_line", "biobank_progressbar")
                        }
                      }
                    }, silent = TRUE)
                    
                    # Check process exit status and report failures
                    exit_status <- tryCatch(proc$get_exit_status(), error = function(e) NA_integer_)
                    if (is.null(exit_status)) exit_status <- NA_integer_
                    
                    if ((is.na(exit_status) || exit_status != 0) && !runnerState$error_detected) {
                      msg <- sprintf("Biobank script failed (exit %s). Check logs for details.", as.character(exit_status))
                      update_runner_error(msg, "biobank_status_line", "biobank_progressbar")
                    }
                    if (!is.na(exit_status) && exit_status != 0) safeNotify(sprintf("Biobank script exited with status %s", as.character(exit_status)), "error")
                    observerHandle$destroy()
                    runnerState$proc <- NULL
                    if (!isTRUE(runnerState$canceled) && !is.na(exit_status) && exit_status == 0 && !is.null(info$on_complete)) {
                      later::later(info$on_complete, delay = 0.05)
                    }
                  }
                }, error = function(e) {
                  output$biobank_status_line <- renderText({ paste("Monitor error:", conditionMessage(e)) })
                  updateRunnerFooter(TRUE)
                  observerHandle$destroy()
                  if (!isTRUE(runnerState$canceled) && !is.null(info$on_complete)) later::later(info$on_complete, delay = 0.05)
                })
              })
              runnerState$observer <- observerHandle
            }
        }
        
        # Clear the missing file info AFTER showing the new modal and starting the process
        runnerState$missing_file_info <- NULL
      }, delay = 0.1)
  })

  # ===== ERROR DETECTION FUNCTION =====
  # Detects error messages in process output lines
  # Scans output for error patterns and returns the most relevant error message
  # 
  # @param lines Character vector of output lines from process
  # @return Character string with error message, or NULL if no errors found
  # 
  # Detection priority:
  # 1. ERROR:FILE_NOT_FOUND: - Missing file errors (highest priority)
  # 2. Lines starting with "Error" or "ERROR"
  # 3. Lines containing "failed" (case insensitive)
  # 4. Lines containing "error" anywhere (case insensitive, lowest priority)
  detect_error_line <- function(lines) {
    if (!length(lines)) return(NULL)
    
    # Clean and filter empty lines
    trimmed <- trimws(as.character(lines))
    trimmed <- trimmed[nzchar(trimmed)]
    if (!length(trimmed)) return(NULL)
    
    # Priority 1: Check for file not found errors
    fnf_hits <- which(grepl("^ERROR:FILE_NOT_FOUND:", trimmed))
    if (length(fnf_hits) > 0) {
      return(trimmed[fnf_hits[length(fnf_hits)]])
    }
    
    # Priority 2-4: Check for other error patterns
    hits <- which(grepl("^(Error|ERROR)\\b", trimmed) | grepl("\\bfailed\\b", trimmed, ignore.case = TRUE))
    if (!length(hits)) hits <- which(grepl("error", trimmed, ignore.case = TRUE))
    if (!length(hits)) return(NULL)
    
    # Return last error found (most recent)
    trimmed[hits[length(hits)]]
  }

  # ===== ERROR HANDLER FUNCTION =====
  # Updates UI when an error is detected in process output
  # Handles special case of file not found errors by storing info for file selection modal
  # Updates status text and progress bar to show error state
  # 
  # @param msg Character - Error message to display
  # @param status_id Character - Output ID for status text element
  # @param progress_id Character - Output ID for progress bar element
  # @param label Character - Label to show in progress bar (default: "Error")
  # 
  # Special handling:
  # - ERROR:FILE_NOT_FOUND: triggers file selection modal instead of error display
  # - Prevents duplicate error messages
  # - Forces UI updates even when hidden (suspendWhenHidden = FALSE)
  update_runner_error <- function(msg, status_id, progress_id, label = "Error") {
    msg <- trimws(paste(msg, collapse = " "))
    if (!nzchar(msg)) return()
    
    # ===== FILE NOT FOUND SPECIAL HANDLING =====
    # Check for missing file errors and trigger file selection modal
    if (grepl("ERROR:FILE_NOT_FOUND:", msg, fixed = TRUE)) {
      # Extract expected file path from error message
      match_result <- regmatches(msg, regexpr("ERROR:FILE_NOT_FOUND:[^\n\r]*", msg))
      if (length(match_result) > 0) {
        expected_path <- sub("^ERROR:FILE_NOT_FOUND:", "", match_result[1])
        expected_path <- trimws(expected_path)
        
        # Determine script type for appropriate file selection
        script_type_val <- switch(isolate(runnerState$kind), 
                           "baseline" = "baseline", 
                           "biobank_data" = "biobank",
                           NULL)
        
        # Store info for file selection modal
        runnerState$missing_file_info <- list(
          expected_path = expected_path,
          script_type = script_type_val,
          on_complete = isolate(runnerState$on_complete_handler)
        )
        
        return()
      }
    }

    # ===== STANDARD ERROR DISPLAY =====
    # Prevent duplicate error messages
    if (identical(runnerState$error_message, msg)) return()
    
    # Update error state
    runnerState$error_detected <- TRUE
    runnerState$error_message <- msg
    
    # Update status text with error message
    output[[status_id]] <- renderText({ msg })
    
    # Update progress bar to show error state (red bar at 100%)
    output[[progress_id]] <- renderUI({
      div(class = "progress", style = "height: 20px; background:#eee;",
          div(class = "progress-bar progress-bar-danger", role = "progressbar",
              style = "width: 100%;",
              `aria-valuemin` = 0, `aria-valuemax` = 100,
              label))
    })
    
    # Force UI updates even when elements are hidden
    outputOptions(output, status_id, suspendWhenHidden = FALSE)
    outputOptions(output, progress_id, suspendWhenHidden = FALSE)
  }
  
  # ============================================================================
  # MODAL & NOTIFICATION HELPER FUNCTIONS
  # ============================================================================
  # These wrapper functions provide error-safe modal and notification handling
  # They prevent crashes when session is not available or in non-interactive mode
  
  # Track table initialization state
  table_initialized <- reactiveVal(FALSE)
  current_table_name <- reactiveVal(NULL)
  
  #' Safe modal dialog display
  #' 
  #' Wrapper around showModal() that handles errors gracefully
  #' Used when session might not be fully initialized
  #' 
  #' @param ui Modal dialog UI object
  #' @param size Modal size ("s", "m", "l")
  #' @param easyClose Whether clicking outside closes modal
  #' @param footer Modal footer elements
  showModalSafe <- function(ui, size = NULL, easyClose = FALSE, footer = NULL) {
    if (is.null(session) || is.null(session$sendCustomMessage)) return(invisible(NULL))
    tryCatch({
      showModal(ui, session = session)
    }, error = function(e) {
      message("showModalSafe fallback: ", conditionMessage(e))
    })
  }
  
  #' Safe modal removal
  #' 
  #' Wrapper around removeModal() that handles errors gracefully
  removeModalSafe <- function() {
    if (is.null(session) || is.null(session$sendCustomMessage)) return(invisible(NULL))
    tryCatch(removeModal(session = session), error = function(e) {
      message("removeModalSafe fallback: ", conditionMessage(e))
    })
  }
  
  #' Safe notification display
  #' 
  #' Wrapper around showNotification() that handles errors gracefully
  #' Falls back to console message if UI is not available
  #' 
  #' @param message Text to display
  #' @param type Notification type: "default", "message", "warning", "error"
  safeNotify <- function(message, type = c("default","message","warning","error")) {
    type <- match.arg(type)
    if (!is.null(session) && is.function(session$sendCustomMessage) && is.function(shiny::showNotification)) {
      tryCatch({ shiny::showNotification(message, type = ifelse(type=="default","message", type)) },
               error = function(e) { message("safeNotify fallback: ", conditionMessage(e), " -> ", message) })
    } else {
      message(sprintf("[Notification][%s] %s", type, message))
    }
  }
  
  #' Stop background runner process
  #' 
  #' Cleanly terminates any running export/retrieval process
  #' Updates status.json and cleans up reactive state
  #' 
  #' @param detail Optional status detail message
  #' @param severity Severity level: "INFO", "WARN", "ERROR"
  #' @param status_step Status step name (default: "canceled")
  stop_runner <- function(detail = NULL, severity = "INFO", status_step = "canceled") {
    proc <- isolate(runnerState$proc)
    obs  <- isolate(runnerState$observer)
    
    # Destroy observer if exists
    if (!is.null(obs)) {
      try(obs$destroy(), silent = TRUE)
      runnerState$observer <- NULL
    }
    
    # Kill process if running
    if (!is.null(proc)) {
      try({
        if (isTRUE(proc$is_alive())) proc$kill(tree = TRUE)
      }, silent = TRUE)
      runnerState$proc <- NULL
    }
    
    # Update status.json with cancellation info
    kind_txt <- isolate(runnerState$kind)
    if (!is.null(kind_txt) && nzchar(kind_txt) && !is.null(detail)) {
      detail_txt <- paste0(detail, " (", kind_txt, ")")
      try(epic2castor_status_update(step = status_step,
                                    detail = detail_txt,
                                    severity = severity,
                                    force = TRUE), silent = TRUE)
      try(epic2castor_status_done(detail = detail_txt, severity = severity), silent = TRUE)
    }
    
    # Reset runner state
    runnerState$kind <- NULL
    runnerState$error_message <- NULL
    runnerState$error_detected <- FALSE
    
    # Clean up UI
    try(removeModalSafe(), silent = TRUE)
    try(shinyjs::enable("refresh_castor"), silent = TRUE)
  }
  
  # ============================================================================
  # REACTIVE VALUES FOR CELL EDITING
  # ============================================================================
  # Track current cell edit operation and manually entered values
  
  currentEdit <- reactiveValues(
    row = NULL,   # Row index being edited
    col = NULL,   # Column index being edited
    orig = NULL   # Original value before edit
  )
  
  manualValues <- reactiveValues(
    vals = list()  # List of manually entered values (key: row_col)
  )
  
  # ============================================================================
  # DATABASE CONNECTION
  # ============================================================================
  # Connect to mapping.db SQLite database for persistent storage
  
  con <- dbConnect(SQLite(), dbPath)
  
  #' Update runner state with new process info
  #' 
  #' Stores process object and observer for later monitoring/cleanup
  #' 
  #' @param kind Process type ("baseline", "follow_up", "biobank_data", "castor_refresh")
  #' @param proc processx::process object
  #' @param observer Observer handle (optional)
  update_runner_state <- function(kind, proc, observer = NULL) {
    runnerState$kind <- kind
    runnerState$proc <- proc
    runnerState$observer <- observer
  }
  
  # ============================================================================
  # SESSION CLEANUP
  # ============================================================================
  # Clean up resources when user closes the browser or session ends
  
  session$onSessionEnded(function() {
    runnerState$canceled <- TRUE
    stop_runner(detail = "Session ended", severity = "WARN")
    try(dbDisconnect(con), silent = TRUE)
    try(stopApp(), silent = TRUE)
  })

  # ============================================================================
  # RUNNER FOOTER MANAGEMENT
  # ============================================================================
  # Controls modal footer buttons during process execution
  # Shows "Close" when process completes, "Cancel" while running
  
  runnerCloseVisible <- reactiveVal(FALSE)
  
  #' Update runner modal footer buttons
  #' 
  #' @param show_close TRUE to show "Close" button, FALSE to show "Cancel"
  updateRunnerFooter <- function(show_close) {
    runnerCloseVisible(isTRUE(show_close))
    if (isTRUE(show_close)) {
      # Process completed: show Close button
      output$runner_footer <- renderUI({
        tagList(actionButton("close_modal", "Close", class = "btn btn-primary"))
      })
    } else {
      # Process running: show Cancel button
      output$runner_footer <- renderUI({
        tagList(actionButton("cancel_modal", "Cancel", class = "btn btn-danger"))
      })
    }
    outputOptions(output, "runner_footer", suspendWhenHidden = FALSE)
  }
  updateRunnerFooter(FALSE)

  # ============================================================================
  # TABLE SELECTION DROPDOWN UPDATE
  # ============================================================================
  # Dynamically update table selection dropdown with available tables
  # Runs whenever selectable tables change (e.g., after metadata refresh)
  
  observe({
    updateSelectInput(session, "file", choices = get_selectable_tables())
  })
  
  # ============================================================================
  # TAB SWITCHING OBSERVER
  # ============================================================================
  # Handles user clicking on a tab button to switch to a different tab
  # Updates active tab state and re-renders table with new tab's data
  #
  # Key features:
  # - Validates tab exists before switching
  # - Prevents unnecessary re-render if already on target tab
  # - Clears row selections when switching tabs
  # - Remembers active tab name globally for all tables
  # - Uses proxy mode for fast rendering
  
  observeEvent(input$switch_tab, {
    req(input$switch_tab)
    new_tab_id <- input$switch_tab
    
    # Validate that target tab exists
    tab_exists <- any(sapply(tabState$tabs, function(t) t$id == new_tab_id))
    if (!tab_exists) {
      return()
    }
    
    # Skip if already on this tab
    if (identical(tabState$activeTab, new_tab_id)) {
      return()
    }
    
    old_tab_id <- tabState$activeTab
    
    # Clear checkbox selections when switching tabs to prevent stale selections
    session$sendCustomMessage("clearCheckboxes", list())
    
    # Update active tab ID
    tabState$activeTab <- new_tab_id
    
    # Remember which tab is active GLOBALLY (for all tables)
    # This allows preserving the active tab when switching between
    # elements/radiobuttons/checkboxes tables
    if (!is.null(input$file)) {
      # Find the tab name based on tab ID
      tab_idx <- which(sapply(tabState$tabs, function(t) t$id == new_tab_id))
      if (length(tab_idx) > 0) {
        active_tab_info <- tabState$tabs[[tab_idx]]
        if (!is.null(active_tab_info)) {
          lastActiveTabName(active_tab_info$name)
        }
      }
    }
    
    # Render data from new tab using proxy mode for fast update
    active_data <- get_active_tab_data()
    if (!is.null(active_data)) {
      render_table(active_data, input$file, mode = "proxy")
      
      # Adjust Scroller after tab switch to fix viewport and row positions
      session$sendCustomMessage("adjustScroller", list())
    }
  })
  
  # ============================================================================
  # TAB CREATION & MANAGEMENT OBSERVERS
  # ============================================================================
  # These observers handle user interactions with tabs: creating new tabs,
  # closing tabs, and renaming tabs. Tab data is kept in tabState$tabs and
  # synchronized with the consolidated mapping data (which includes tab_name_meta
  # and tab_order_meta columns).
  
  # Create new tab: Show modal with options
  observeEvent(input$create_tab, {
    # Generate default name based on current number of tabs
    default_name <- sprintf("Tab %d", length(tabState$tabs) + 1)
    
    showModal(modalDialog(
      title = "Create New Tab",
      
      textInput("new_tab_name", "Tab name:", value = default_name),
      
      radioButtons("new_tab_type", "Type:",
                   choices = c(
                     "Empty tab" = "empty",
                     "Copy current tab" = "copy"
                   ),
                   selected = "empty"),
      
      checkboxInput("switch_to_new_tab", "Switch to new tab after creation", value = TRUE),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_create_tab", "Create", class = "btn-primary")
      ),
      
      easyClose = TRUE
    ))
  })
  
  # Confirm create new tab: Validate and create
  observeEvent(input$confirm_create_tab, {
    req(input$new_tab_name)
    
    new_name <- trimws(input$new_tab_name)
    
    # Validation: empty name
    if (new_name == "") {
      showNotification("Tab name cannot be empty", type = "error")
      return()
    }
    
    # Validation: max length
    if (nchar(new_name) > 50) {
      showNotification("Tab name too long (max 50 characters)", type = "error")
      return()
    }
    
    # Validation: name already exists (case-insensitive)
    existing_names <- tolower(sapply(tabState$tabs, function(t) t$name))
    if (tolower(new_name) %in% existing_names) {
      showNotification("A tab with this name already exists", type = "error")
      return()
    }
    
    # Create new tab data
    if (input$new_tab_type == "empty") {
      # Empty tab with same structure as current tab, but 0 rows
      active_data <- get_active_tab_data()
      if (!is.null(active_data) && ncol(active_data) > 0) {
        # Use structure of current tab but empty (0 rows)
        new_data <- active_data[0, ]
      } else {
        # Fallback: minimal structure with 0 rows
        new_data <- data.table()
      }
    } else {
      # Copy current tab
      new_data <- copy(get_active_tab_data())
      if (is.null(new_data)) {
        showNotification("Cannot copy current tab", type = "error")
        return()
      }
    }
    
    # Add tab to tabState
    new_tab_id <- sprintf("tab_%d", tabState$nextTabId)
    tabState$nextTabId <- tabState$nextTabId + 1
    
    # Determine order for new tab (highest current order + 1)
    max_order <- max(sapply(tabState$tabs, function(t) if(is.null(t$order)) 0 else t$order))
    
    new_tab <- list(
      id = new_tab_id,
      name = new_name,
      order = max_order + 1,
      data = new_data
    )
    
    tabState$tabs <- c(tabState$tabs, list(new_tab))
    
    # Sync new tab ONLY to current table (not to radiobuttons/checkboxes)
    # Radiobuttons/checkboxes tabs appear automatically via updateRadioMapping/updateCheckboxMapping
    if (is_selectable_table(input$file)) {
      mappingData[[input$file]] <<- add_empty_tab_to_consolidated(
        mappingData[[input$file]], 
        new_name, 
        max_order + 1
      )
    }
    
    # Switch to new tab (only if checkbox is checked)
    if (isTRUE(input$switch_to_new_tab)) {
      tabState$activeTab <- new_tab_id
      render_table(new_data, input$file, mode = "proxy")
    } else {
      # Stay on current tab, but re-render to show new tab button
      render_table(get_active_tab_data(), input$file, mode = "proxy")
    }
    
    removeModal()
    showNotification(sprintf("Tab '%s' created", new_name), type = "message")
  })
  
  # Close tab: Show confirmation modal
  observeEvent(input$close_tab, {
    req(input$close_tab)
    tab_id_to_close <- input$close_tab
    
    # Prevent closing last tab
    if (length(tabState$tabs) <= 1) {
      showNotification("Cannot close the last tab", type = "warning")
      return()
    }
    
    # Find the tab
    tab_index <- which(sapply(tabState$tabs, function(t) t$id == tab_id_to_close))
    if (length(tab_index) == 0) {
      return()
    }
    
    tab_to_close <- tabState$tabs[[tab_index]]
    
    # Show confirmation
    showModal(modalDialog(
      title = "Close Tab",
      sprintf("Are you sure you want to close tab '%s'?", tab_to_close$name),
      tags$p("This action cannot be undone."),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_close_tab", "Close", class = "btn-danger")
      ),
      
      easyClose = TRUE
    ))
    
    # Save tab_id for confirmation
    session$userData$tab_to_close <- tab_id_to_close
  })
  
  # Confirm close tab: Execute tab deletion after user confirmation
  #
  # This observer handles the actual tab deletion after user confirms in modal
  # Key steps:
  # 1. Validate that there's more than 1 tab (prevent closing last tab)
  # 2. If closing active tab, switch to another tab first
  # 3. Remove tab from tabState
  # 4. Synchronize deletion to ALL selectable tables (elements, checkboxes, radiobuttons)
  # 5. Clean up UI state
  observeEvent(input$confirm_close_tab, {
    tab_id_to_close <- session$userData$tab_to_close
    req(tab_id_to_close)
    
    # Validation: prevent deleting the last tab
    if (length(tabState$tabs) <= 1) {
      showNotification("Cannot delete the last tab", type = "error")
      removeModal()
      return()
    }
    
    # Find the tab index
    tab_index <- which(sapply(tabState$tabs, function(t) t$id == tab_id_to_close))
    if (length(tab_index) == 0) {
      removeModal()
      return()
    }
    
    # If we're closing the active tab, switch to another tab first
    if (identical(tabState$activeTab, tab_id_to_close)) {
      # Switch to first remaining tab
      other_tabs <- tabState$tabs[-tab_index]
      if (length(other_tabs) > 0) {
        new_active_id <- other_tabs[[1]]$id
        tabState$activeTab <- new_active_id
        
        # Render the new active tab
        render_table(other_tabs[[1]]$data, input$file, mode = "proxy")
      }
    }
    
    # Remember tab info for synchronization across all tables
    tab_to_delete <- tabState$tabs[[tab_index]]
    
    # Remove the tab from tabState
    tabState$tabs <- tabState$tabs[-tab_index]
    
    # Synchronize deletion to ALL selectable tables
    # This ensures elements, checkboxes, and radiobuttons all stay in sync
    for (tbl in get_selectable_tables()) {
      mappingData[[tbl]] <<- remove_tab_from_metadata(
        mappingData[[tbl]], 
        tab_to_delete$name, 
        tab_to_delete$order
      )
    }
    
    removeModal()
    showNotification("Tab closed", type = "message")
    
    # Clean up temporary storage
    session$userData$tab_to_close <- NULL
  })
  
  # Rename tab: Update tab name and synchronize across all tables
  #
  # This observer handles inline tab renaming (triggered from JavaScript double-click)
  # Receives: { tab_id: "tab_1", new_name: "New Name" }
  # 
  # Validation:
  # - Name cannot be empty
  # - Name max length is 50 characters
  # - Name must be unique (case-insensitive) across all tabs
  #
  # Synchronization:
  # - Updates tab name in tabState
  # - Renames tab_name_meta in ALL selectable tables to maintain consistency
  observeEvent(input$rename_tab, {
    req(input$rename_tab)
    
    tab_id <- input$rename_tab$tab_id
    new_name <- trimws(input$rename_tab$new_name)
    
    # Validation: empty name
    if (new_name == "") {
      showNotification("Tab name cannot be empty", type = "error")
      return()
    }
    
    # Validation: max length
    if (nchar(new_name) > 50) {
      showNotification("Tab name too long (max 50 characters)", type = "error")
      return()
    }
    
    # Validation: name already exists (case-insensitive, except current tab)
    tab_index <- which(sapply(tabState$tabs, function(t) t$id == tab_id))
    if (length(tab_index) > 0) {
      other_names <- tolower(sapply(tabState$tabs[-tab_index], function(t) t$name))
      if (tolower(new_name) %in% other_names) {
        showNotification("A tab with this name already exists", type = "error")
        return()
      }
    }
    
    # Find the tab by ID and update its name
    tab_index <- which(sapply(tabState$tabs, function(t) t$id == tab_id))
    if (length(tab_index) > 0) {
      old_name <- tabState$tabs[[tab_index]]$name
      tabState$tabs[[tab_index]]$name <- new_name
      
      # Synchronize the rename to ALL selectable tables
      # This ensures that elements, checkboxes, and radiobuttons all reflect the new tab name
      for (tbl in get_selectable_tables()) {
        mappingData[[tbl]] <<- rename_tab_in_metadata(
          mappingData[[tbl]], 
          old_name, 
          new_name
        )
      }
      
      showNotification(sprintf("Tab renamed to '%s'", new_name), type = "message", duration = 2)
    }
  })
  
  # ============================================================================
  # DYNAMIC UI OBSERVERS
  # ============================================================================
  # These observers handle dynamic UI updates in response to user input changes
  
  # Observer: Forward search input to DataTable via custom message
  # This enables real-time filtering as user types in search box
  observe({
    session$sendCustomMessage("DT_search", list(key = input$search))
  })
  
  # Observer: Adjust table container dimensions dynamically
  # Width is controlled by user input, height is calculated from viewport
  # If user hasn't manually adjusted slider, width will be set by JavaScript window resize handler
  observe({
    width <- paste0(input$width, "px")
    # Height will be calculated dynamically in JavaScript based on viewport
    height <- 700  # Default fallback
    # Send manual flag = TRUE to indicate this is a user-initiated change
    session$sendCustomMessage(type = "resizeDiv", message = list(width = width, height = height, manual = TRUE))
  })
  
  # ============================================================================
  # TABLE SWITCHING OBSERVER
  # ============================================================================
  # This is the CRITICAL observer that handles switching between different tables
  # (elements, waarde_radiobuttons, waarde_checkboxes)
  #
  # Key responsibilities:
  # 1. Save current table's tabs to mappingData before switching
  # 2. Load new table's data and restore its tabs
  # 3. Handle special case: radiobuttons/checkboxes inherit tab structure from elements
  # 4. Preserve active tab name across table switches when possible
  # 5. Synchronize tab metadata to ensure consistency
  #
  # Reactivity considerations:
  # - Uses isolate() to prevent unwanted reactive dependencies
  # - Reads relatedDataUpdated() trigger to force fresh data after paste operations
  # - Clears checkbox selections and resets JavaScript state on switch
  
  observeEvent(input$file, {
    req(is_selectable_table(input$file))
    
    # Start timing table switch operation
    perf_start <- Sys.time()
    
    # Clear checkbox selections when switching tables to prevent stale selections
    session$sendCustomMessage("clearCheckboxes", list())
    
    # Reset tab click state in JavaScript to prevent stale tab click tracking
    session$sendCustomMessage("resetTabClickState", list())
    
    # Get the previous table BEFORE doing anything else
    prev_table <- isolate(previous_table())
    
    # Remember the currently active tab name GLOBALLY before switching tables
    # This allows us to restore the same tab name when switching back
    if (!is.null(prev_table) && !is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      # Find the currently active tab name
      active_tab_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_tab_idx) > 0) {
        active_tab_name <- tabState$tabs[[active_tab_idx]]$name
        lastActiveTabName(active_tab_name)
      }
    }
    
    # Save tabs from PREVIOUS table to mappingData before switching
    if (!is.null(prev_table) && is_selectable_table(prev_table) && length(tabState$tabs) > 0) {
      # Consolidate tabs ONLY for elements table
      # For radiobuttons/checkboxes: their data is managed by auto-fill functions
      if (prev_table == "elements") {
        # Consolidate current tabs and save to mappingData
        mappingData[[prev_table]] <<- consolidate_tabs_with_metadata(tabState$tabs)
        
        # IMPORTANT: Do NOT call updateCheckboxMapping/updateRadioMapping here!
        # This causes a Shiny reactivity timing issue where the functions
        # read stale data (before paste operations are processed).
        # The copy/cut/paste functionality updates related data directly with correct values.
        # These functions are only called on other actions like data import.
      }
      # If prev_table is radiobuttons or checkboxes: do nothing, their data stays as-is
    }
    
    # REACTIVITY FIX: Force refresh of related data updates
    # IMPORTANT: Check trigger BEFORE fetching data
    trigger_value <- if (input$file %in% c("waarde_radiobuttons", "waarde_checkboxes")) {
      relatedDataUpdated()  # Read the reactive value to create dependency
    } else {
      0
    }
    
    # Load data for NEW table
    # For radiobuttons/checkboxes: use isolate to read mapping data
    # but only if trigger > 0 (after a paste operation)
    if (input$file %in% c("waarde_radiobuttons", "waarde_checkboxes")) {
      if (trigger_value > 0) {
        # AFTER a paste: force fresh data read
        # Wait briefly to let reactive flush happen
        Sys.sleep(0.1)
        
        # Read data with isolate (fresh read)
        data <- isolate(mappingData[[input$file]])
      } else {
        # No recent paste: normal read
        data <- mappingData[[input$file]]
      }
    } else {
      # For other tables: normal reactive read
      data <- mappingData[[input$file]]
    }
    
    # Restore tabs from metadata in the loaded data
    tabState$tabs <- restore_tabs_from_metadata(data)
    tabState$tabs <- ensure_tab_order(tabState$tabs)
    
    # SPECIAL CASE: When switching to radiobuttons/checkboxes, use elements' tabs as master
    # Because elements determines which tabs exist, BUT with data from the current table
    if (input$file %in% c("waarde_radiobuttons", "waarde_checkboxes")) {
      elements_data <- mappingData[["elements"]]
      if ("tab_name_meta" %in% names(elements_data)) {
        elements_tabs <- restore_tabs_from_metadata(elements_data)
        elements_tabs <- ensure_tab_order(elements_tabs)
        
        # CRITICAL: Replace the data in elements_tabs with data from the current table
        # AND remove tabs that have no data
        tabs_with_data <- list()
        
        for (i in seq_along(elements_tabs)) {
          tab_name <- elements_tabs[[i]]$name
          tab_order <- elements_tabs[[i]]$order
          
          # Filter data for this specific tab
          if ("tab_name_meta" %in% names(data)) {
            tab_data <- data[tab_name_meta == tab_name & tab_order_meta == tab_order]
          } else {
            # If there's no metadata, use all data for the first tab
            if (i == 1) {
              tab_data <- data
            } else {
              tab_data <- data[0]  # Empty data.table
            }
          }
          
          # ONLY add if there is actually data (more than 0 rows)
          if (nrow(tab_data) > 0) {
            # Replace the data in the tab
            elements_tabs[[i]]$data <- tab_data
            tabs_with_data[[length(tabs_with_data) + 1]] <- elements_tabs[[i]]
          }
        }
        
        # Use only tabs that actually have data
        # If there are no tabs with data, create one empty tab
        if (length(tabs_with_data) > 0) {
          tabState$tabs <- tabs_with_data
        } else {
          # No data: create one empty "Main" tab
          tabState$tabs <- list(list(
            id = "tab_1",
            name = "Main",
            order = 1,
            data = data[0]  # Empty data.table
          ))
        }
      }
    }
    
    # Select the active tab: try to restore the last active tab name
    last_active_tab_name <- isolate(lastActiveTabName())
    
    if (!is.null(last_active_tab_name) && length(tabState$tabs) > 0) {
      # Find the tab with this name
      matching_tab_idx <- which(sapply(tabState$tabs, function(t) t$name == last_active_tab_name))
      if (length(matching_tab_idx) > 0) {
        # Found! Use this tab
        tabState$activeTab <- tabState$tabs[[matching_tab_idx[1]]]$id
      } else {
        # Not found (e.g. tab was deleted), use first tab
        tabState$activeTab <- tabState$tabs[[1]]$id
      }
    } else {
      # No previous tab known, use first tab
      tabState$activeTab <- tabState$tabs[[1]]$id
    }
    
    tabState$nextTabId <- length(tabState$tabs) + 1
    
    # Update previous_table tracker
    previous_table(input$file)
    
    render_table(get_active_tab_data(), input$file)
    
    # Adjust Scroller after table switch to fix viewport and row positions
    session$sendCustomMessage("adjustScroller", list())
    
    # Log table switch performance
    perf_duration <- as.numeric(difftime(Sys.time(), perf_start, units = "secs"))
    row_count <- nrow(get_active_tab_data())
    color <- if (perf_duration < 0.5) "\033[32m" else if (perf_duration < 2) "\033[33m" else "\033[31m"
    cat(sprintf(
      "%s[Perf] ✓ table_switch (%s): %.3fs (%d rows)\033[0m\n",
      color,
      input$file,
      perf_duration,
      row_count
    ))
  })
  
  # ============================================================================
  # FORCE TABLE REFRESH OBSERVER
  # ============================================================================
  # This observer forces a complete table re-render when explicitly triggered
  # Used after operations that modify data in ways that require full refresh
  # (e.g., after paste operations with related data)
  
  observeEvent(forceTableRefresh(), {
    req(forceTableRefresh() > 0)  # Skip initial value
    req(input$file)
    
    # Get current tab data
    current_data <- get_active_tab_data()
    
    # Force complete table re-render (full mode, not proxy)
    render_table(current_data, input$file, mode = "full")
    
    cat(sprintf("[DEBUG] Forced table refresh for %s (trigger=%d)\n", 
                input$file, forceTableRefresh()))
  })
  
  # ============================================================================
  # DROPDOWN DOUBLE-CLICK OBSERVER
  # ============================================================================
  # Handles double-clicking on dropdown cells to edit the selected value
  # Shows modal with text input pre-filled with current value
  
  observeEvent(input$dropdown_dblclick, {
    req(input$dropdown_dblclick$id)
    showModalSafe(modalDialog(
      title = "Edit Data",
      textInput("new_option", "New value:", value = input$dropdown_dblclick$selected),
      footer = tagList(
        actionButton("modal_dbl_save", "Save", class = "btn-primary"),
        tags$button("Cancel", type = "button", class = "btn btn-default modal-cancel", `data-dismiss` = "modal")
      )
    ))
  })

  # ============================================================================
  # CASTOR METADATA REFRESH OBSERVER
  # ============================================================================
  # Handles refreshing Castor metadata from the API
  # 
  # Process:
  # 1. Check if API credentials are configured (client_id, client_secret, study_id)
  # 2. If not configured, show modal guiding user to configure credentials
  # 3. If configured, launch external R process to retrieve metadata
  # 4. Show progress modal with status updates
  # 5. Monitor process and handle completion/errors
  #
  # External process:
  # - Runs scripts/CastorRetrieval.r in separate R session
  # - Prevents blocking the Shiny UI during long-running API calls
  # - Uses processx package for robust process management
  # - Creates "done flag" file to signal completion
  
  # ============================================================================
  # KEYBOARD SHORTCUTS - F5 REFRESH
  # ============================================================================
  # Refresh current tab when F5 is pressed (soft refresh, no page reload)
  observeEvent(input$keyboard_refresh, {
    req(input$file)  # Need active tab
    
    tryCatch({
      # Re-render current tab with latest data
      active_data <- get_active_tab_data()
      render_table(active_data, input$file, mode = "proxy")
      
      showNotification("Table refreshed", type = "message", duration = 2)
    }, error = function(e) {
      showNotification(paste("Refresh failed:", e$message), type = "error")
    })
  })
  
  observeEvent(input$refresh_castor, {
    # Check if API credentials are configured
    api_config_path <- epc_path("config_api")
    has_credentials <- FALSE
    
    if (file.exists(api_config_path)) {
      api_config <- tryCatch(jsonlite::fromJSON(api_config_path), error = function(e) NULL)
      if (!is.null(api_config) && 
          !is.null(api_config$client_id) && nchar(trimws(api_config$client_id)) > 0 &&
          !is.null(api_config$client_secret) && nchar(trimws(api_config$client_secret)) > 0 &&
          !is.null(api_config$study_id) && nchar(trimws(api_config$study_id)) > 0) {
        has_credentials <- TRUE
      }
    }
    
    # If credentials are missing, show helpful modal with instructions
    if (!has_credentials) {
      showModalSafe(modalDialog(
        title = tags$div(icon("exclamation-triangle"), " API Credentials Required"),
        tagList(
          p("The Castor API credentials have not been configured yet."),
          p("To refresh metadata, you need to:"),
          tags$ol(
            tags$li(HTML("Configure your credentials by clicking <strong>'Update credentials'</strong> in the Castor menu")),
            tags$li("Fill in your Client ID, Client Secret, and Study ID"),
            tags$li("Save the credentials"),
            tags$li("Try refreshing metadata again")
          ),
          hr(),
          p(style = "color: #777; font-size: 0.9em;",
            icon("info-circle"), 
            " You can find your credentials in Castor EDC under Settings → API")
        ),
        footer = tagList(
          actionButton("go_to_credentials", "Update Credentials Now", class = "btn btn-primary"),
          modalButton("Cancel")
        ),
        easyClose = TRUE,
        size = "m"
      ))
      return()
    }
    
    # Credentials are configured, proceed with metadata refresh
    castor_script <- epc_path("castor_retrieval_script")
    if (is.null(castor_script) || !file.exists(castor_script)) {
      safeNotify(sprintf("Castor retrieval script not found: %s", as.character(castor_script)), "error")
      return()
    }
    
    # Determine Rscript executable path (OS-specific)
    rscript_bin <- if (.Platform$OS.type == "windows") file.path(R.home("bin"), "Rscript.exe") else file.path(R.home("bin"), "Rscript")
    if (!file.exists(rscript_bin)) rscript_bin <- "Rscript"

    # Prepare runner state for process monitoring
    shinyjs::disable("refresh_castor")
    runnerState$canceled <- FALSE
    runnerState$error_detected <- FALSE
    runnerState$error_message <- NULL
    updateRunnerFooter(FALSE)
    
    # Show progress modal with animated progress bar
    showModalSafe(modalDialog(
      title = "Refreshing Castor metadata",
      tagList(
        uiOutput("castor_progressbar"),
        div(style = "margin-top: 8px;", textOutput("castor_status_line"))
      ),
      footer = uiOutput("runner_footer"),
      easyClose = FALSE,
      size = "m"
    ))

    # Initialize status output
    output$castor_status_line <- renderText({ "Starting retrieval…" })
    outputOptions(output, "castor_status_line", suspendWhenHidden = FALSE)
    
    # Render animated progress bar (indeterminate/striped style)
    output$castor_progressbar <- renderUI({
      div(class = "progress", style = "height: 20px; background:#eee;",
          div(class = "progress-bar progress-bar-striped active", role = "progressbar",
              style = "width: 100%;",
              `aria-valuemin` = 0, `aria-valuemax` = 100,
              "Refreshing…"))
    })
    outputOptions(output, "castor_progressbar", suspendWhenHidden = FALSE)

    # Create temporary "done flag" file for process completion signaling
    start_time <- Sys.time()
    done_flag <- tempfile(pattern = "castor_retrieval_done_", tmpdir = tempdir(), fileext = ".flag")
    if (file.exists(done_flag)) try(file.remove(done_flag), silent = TRUE)

    # Launch external R process
    proc <- tryCatch({
      process$new(
        rscript_bin,
        args = c("--vanilla", castor_script),
        stdout = "|",
        stderr = "|",
        env = c(
          EPIC2CASTOR_DONE = done_flag,
          EPIC2CASTOR_FORCE_RETRIEVAL = "1",  # Force fresh retrieval (ignore cache)
          EPIC2CASTOR_LOGDIR = getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
        )
      )
    }, error = function(e) {
      msg <- paste("Could not start Castor retrieval:", conditionMessage(e))
      safeNotify(msg, "error")
      shinyjs::enable("refresh_castor")
      removeModalSafe()
      return(NULL)
    })
    if (is.null(proc)) return()
  runnerState$kind <- "castor_refresh"
  runnerState$proc <- proc
  runnerState$forced_completion <- FALSE

    poll_timer <- reactiveTimer(500, session)

    observerHandle <- observe({
      poll_timer()
      if (isTRUE(runnerState$canceled)) return()
      out_lines <- character(0)
      err_lines <- character(0)
      try({
        out_lines <- proc$read_output_lines()
        err_lines <- proc$read_error_lines()
      }, silent = TRUE)
      if (length(out_lines)) lapply(out_lines, function(x) cat(paste0("[CastorRefresh][OUT] ", x, "\n")))
      if (length(err_lines)) lapply(err_lines, function(x) cat(paste0("[CastorRefresh][ERR] ", x, "\n")))

      combined_lines <- c(out_lines, err_lines)
      if (length(combined_lines)) {
        err_line <- detect_error_line(combined_lines)
        if (!is.null(err_line)) update_runner_error(err_line, "castor_status_line", "castor_progressbar")
      }

      status <- NULL
      try({
        proc$poll_io(0)
        status <- proc$get_exit_status()
      }, silent = TRUE)

      outputs_ready <- FALSE
      try({
        outputs_ready <- validate_castor_outputs(done_flag, start_time, epc_path("castor_field_options_file"), epc_path("castor_study_variablelist_file"))
      }, silent = TRUE)

      if (isTRUE(outputs_ready) && is.null(status) && !isTRUE(runnerState$forced_completion)) {
        runnerState$forced_completion <- TRUE
        try(proc$kill(tree = TRUE), silent = TRUE)
        try(proc$wait(timeout = 2000), silent = TRUE)
        try({
          status <- proc$get_exit_status()
        }, silent = TRUE)
      }

      if (isTRUE(outputs_ready) && is.null(status)) {
        status <- 0L
      }

      if (is.null(status)) {
        if (!runnerState$error_detected) output$castor_status_line <- renderText({ "Retrieving metadata…" })
        return()
      }

      # Drain any remaining output after the process exits.
      try({
        proc$poll_io(0)
        leftover_out <- proc$read_output_lines()
        leftover_err <- proc$read_error_lines()
        if (length(leftover_out)) lapply(leftover_out, function(x) cat(paste0("[CastorRefresh][OUT] ", x, "\n")))
        if (length(leftover_err)) lapply(leftover_err, function(x) cat(paste0("[CastorRefresh][ERR] ", x, "\n")))
        leftover_lines <- c(leftover_out, leftover_err)
        if (length(leftover_lines)) {
          err_line <- detect_error_line(leftover_lines)
          if (!is.null(err_line)) update_runner_error(err_line, "castor_status_line", "castor_progressbar")
        }
      }, silent = TRUE)

      updateRunnerFooter(TRUE)
      observerHandle$destroy()
      runnerState$observer <- NULL

      exit_status <- tryCatch({
        if (is.null(status)) proc$get_exit_status() else status
      }, error = function(e) NA_integer_)
      if (is.null(exit_status)) exit_status <- NA_integer_
      fo <- epc_path("castor_field_options_file")
      sv <- epc_path("castor_study_variablelist_file")
      success <- FALSE
      if (!is.na(exit_status) && exit_status %in% c(0L, 5L)) {
        success <- validate_castor_outputs(done_flag, start_time, fo, sv)
      }

      if (success) {
        try({
          reload_castor_metadata()
          selected <- input$file
          choices <- get_selectable_tables()
          if (length(choices) > 0) {
            new_selection <- if (!is.null(selected) && selected %in% choices) selected else choices[1]
            updateSelectInput(session, "file", choices = choices, selected = new_selection)
            if (!is.null(new_selection) && new_selection %in% choices) {
              render_table(mappingData[[new_selection]], new_selection)
            }
          }
        }, silent = TRUE)
        output$castor_status_line <- renderText({ "Castor metadata refreshed." })
        output$castor_progressbar <- renderUI({
          div(class = "progress", style = "height: 20px; background:#eee;",
              div(class = "progress-bar progress-bar-success", role = "progressbar",
                  style = "width: 100%;",
                  `aria-valuemin` = 0, `aria-valuemax` = 100,
                  "Done"))
        })
        
        # Show success notification
        showNotification(
          ui = tagList(
            icon("check-circle"),
            " Castor metadata successfully refreshed! All study data is now up to date."
          ),
          duration = 8,
          type = "message",
          closeButton = TRUE
        )
      } else {
        err_msg <- runnerState$error_message
        if (is.null(err_msg) || !nzchar(err_msg)) {
          err_msg <- sprintf("Castor retrieval failed (exit %s). Check logs for details.", as.character(exit_status))
        }
        safeNotify(err_msg, "error")
        output$castor_status_line <- renderText({ err_msg })
        output$castor_progressbar <- renderUI({
          div(class = "progress", style = "height: 20px; background:#eee;",
              div(class = "progress-bar progress-bar-danger", role = "progressbar",
                  style = "width: 100%;",
                  `aria-valuemin` = 0, `aria-valuemax` = 100,
                  "Error"))
        })
      }

      if (file.exists(done_flag)) try(file.remove(done_flag), silent = TRUE)
      runnerState$proc <- NULL
      runnerState$kind <- NULL
      shinyjs::enable("refresh_castor")
    })
    runnerState$observer <- observerHandle
    update_runner_state("castor_refresh", proc, observerHandle)
  })
  # Runner helper: execute the Baseline script with monitoring; on_complete may trigger follow-up
  run_baseline_script <- function(on_complete = NULL) {
    removeModalSafe()
    updateRunnerFooter(FALSE)
    runnerState$canceled <- FALSE
    {
      run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
      status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"
      if (file.exists(status_path)) try(file.remove(status_path), silent = TRUE)
    }
    showModalSafe(modalDialog(
      title = "Running Baseline",
      tagList(
        uiOutput("baseline_progressbar"),
        div(style = "margin-top:8px;", textOutput("baseline_status_line"))
      ),
      footer = uiOutput("runner_footer"),
      easyClose = FALSE,
      size = "l"
    ))

    scriptPath <- file.path(epc_path("baseline_scripts_dir"), "baseline.r")
    proc <- process$new(
      "Rscript",
      args = c(scriptPath),
      stdout = "|",
      stderr = "|",
      env = c(EPIC2CASTOR_LOGDIR = getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", "")))
    )
    runnerState$proc <- proc
    runnerState$kind <- "baseline"
    runnerState$on_complete_handler <- on_complete
    runnerState$error_detected <- FALSE
    runnerState$error_message <- NULL

    run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
    status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"

    output$baseline_progressbar <- renderUI({
      pct <- 0
      div(class = "progress", style = "height: 20px; background:#eee;",
          div(class = "progress-bar progress-bar-striped active", role = "progressbar",
              style = paste0("width: ", pct, "%;"),
              `aria-valuemin` = 0, `aria-valuemax` = 100,
              paste0(pct, "%"))
      )
    })
    output$baseline_status_line <- renderText({ "Starting…" })

    observerHandle <- observe({
      reactiveTimer(500)()
      tryCatch({
        out_lines <- character(0)
        err_lines <- character(0)
        try({
          out_lines <- proc$read_output_lines()
          err_lines <- proc$read_error_lines()
        }, silent = TRUE)
        if (length(out_lines)) lapply(out_lines, function(x) cat(paste0("[Baseline][OUT] ", x, "\n")))
        if (length(err_lines)) lapply(err_lines, function(x) cat(paste0("[Baseline][ERR] ", x, "\n")))
        combined_lines <- c(out_lines, err_lines)
        if (length(combined_lines)) {
          err_line <- detect_error_line(combined_lines)
          if (!is.null(err_line)) update_runner_error(err_line, "baseline_status_line", "baseline_progressbar")
        }

        # Poll status.json
        if (file.exists(status_path)) {
          st <- try(jsonlite::fromJSON(status_path), silent = TRUE)
          if (!inherits(st, "try-error") && is.list(st)) {
            # Percent/current/total defensively parsed; compute pct from current/total when useful
            cur <- suppressWarnings(as.numeric(st$current))
            tot <- suppressWarnings(as.numeric(st$total))
            pct <- suppressWarnings(as.numeric(st$percent))
            if (length(cur) != 1L || is.na(cur) || !is.finite(cur)) cur <- NA_real_
            if (length(tot) != 1L || is.na(tot) || !is.finite(tot) || tot <= 0) tot <- NA_real_
            if (length(pct) != 1L || is.na(pct) || !is.finite(pct)) pct <- NA_real_
            # Prefer computed percent if current/total available
            if (is.finite(cur) && is.finite(tot)) pct <- 100 * cur / tot
            if (is.na(pct)) pct <- 0
            if (pct < 0) pct <- 0
            if (pct > 100) pct <- 100

            # Step/detail coerced to single strings
            step <- st$step
            if (is.null(step) || length(step) != 1L || !nzchar(as.character(step))) step <- "running" else step <- as.character(step)
            step_lc <- tolower(step)
            if (!is.na(step_lc) && identical(step_lc, "done")) updateRunnerFooter(TRUE)
            detail <- st$detail
            if (is.null(detail) || length(detail) == 0L) detail <- "" else detail <- as.character(detail)[1]

            # ETA defensively parsed to numeric seconds
            eta_val <- suppressWarnings(as.numeric(st$eta_s))
            if (!is.null(eta_val) && length(eta_val) == 1L && is.finite(eta_val)) {
              eta_txt <- paste0("ETA ", format(round(eta_val), scientific = FALSE), "s")
            } else eta_txt <- NULL

            if (!runnerState$error_detected) {
              # Check if done: green bar, no animation
              if (!is.na(step_lc) && identical(step_lc, "done")) {
                output$baseline_progressbar <- renderUI({
                  div(class = "progress", style = "height: 20px; background:#eee;",
                      div(class = "progress-bar progress-bar-success", role = "progressbar",
                          style = paste0("width: 100%; background-color: #5cb85c;"),
                          `aria-valuemin` = 0, `aria-valuemax` = 100,
                          "100%")
                  )
                })
              } else {
                output$baseline_progressbar <- renderUI({
                  div(class = "progress", style = "height: 20px; background:#eee;",
                      div(class = "progress-bar progress-bar-striped active", role = "progressbar",
                          style = paste0("width: ", pct, "%;"),
                          `aria-valuemin` = 0, `aria-valuemax` = 100,
                          paste0(round(pct), "%"))
                  )
                })
              }
            }
            line <- paste(step, if (nzchar(detail)) paste("-", detail) else "", if (!is.null(eta_txt)) paste("(", eta_txt, ")") else "")
            if (!runnerState$error_detected) output$baseline_status_line <- renderText({ line })
          }
        }

        # Done: show Close button and stop observer
        if (!proc$is_alive()) {
          updateRunnerFooter(TRUE)
          
          # Check if we have "EPIC2CASTOR::DONE" marker for successful completion
          script_completed_successfully <- FALSE
          
          try({
            leftover_out <- proc$read_output_lines()
            leftover_err <- proc$read_error_lines()
            if (length(leftover_out)) lapply(leftover_out, function(x) cat(paste0("[Baseline][OUT] ", x, "\n")))
            if (length(leftover_err)) lapply(leftover_err, function(x) cat(paste0("[Baseline][ERR] ", x, "\n")))
            leftover_lines <- c(leftover_out, leftover_err)
            
            # Check for success marker
            if (any(grepl("EPIC2CASTOR::DONE", leftover_lines, fixed = TRUE))) {
              script_completed_successfully <- TRUE
            }
            
            if (length(leftover_lines)) {
              err_line <- detect_error_line(leftover_lines)
              if (!is.null(err_line)) update_runner_error(err_line, "baseline_status_line", "baseline_progressbar")
            }
          }, silent = TRUE)
          
          exit_status <- tryCatch(proc$get_exit_status(), error = function(e) NA_integer_)
          if (is.null(exit_status)) exit_status <- NA_integer_
          
          # Check if a missing file was detected (which should trigger the upload modal)
          has_missing_file <- !is.null(isolate(runnerState$missing_file_info))
          
          # Only report error if:
          # - script didn't complete successfully AND
          # - no error was already detected AND
          # - no missing file was detected (that would trigger the upload modal)
          if (!script_completed_successfully && (is.na(exit_status) || exit_status != 0) && !runnerState$error_detected && !has_missing_file) {
            msg <- sprintf("Baseline failed (exit %s). Check logs for details.", as.character(exit_status))
            update_runner_error(msg, "baseline_status_line", "baseline_progressbar")
          }
          
          # Only show notification for non-zero exit if script didn't complete successfully and no missing file
          if (!script_completed_successfully && !is.na(exit_status) && exit_status != 0 && !has_missing_file) {
            safeNotify(sprintf("Baseline script exited with status %s", as.character(exit_status)), "error")
          }
          
          observerHandle$destroy()
          runnerState$proc <- NULL
          runnerState$observer <- NULL
          
          # Call on_complete if script completed successfully OR exit status is 0
          if (!isTRUE(runnerState$canceled) && (script_completed_successfully || (!is.na(exit_status) && exit_status == 0)) && !is.null(on_complete)) {
            later::later(on_complete, delay = 0.05)
          }
        }
      }, error = function(e) {
        output$baseline_status_line <- renderText({ paste("Monitor error:", conditionMessage(e)) })
        updateRunnerFooter(TRUE)
        observerHandle$destroy()
        runnerState$proc <- NULL
        runnerState$observer <- NULL
        if (!is.null(on_complete)) later::later(on_complete, delay = 0.05)
      })
    })
    runnerState$observer <- observerHandle
  }

  # Runner helper: execute the Follow-up script with monitoring; on_complete may trigger follow-up
  run_follow_up_script <- function(on_complete = NULL) {
    removeModalSafe()
    updateRunnerFooter(FALSE)
    runnerState$canceled <- FALSE
    {
      run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
      status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"
      if (file.exists(status_path)) try(file.remove(status_path), silent = TRUE)
    }
    showModalSafe(modalDialog(
      title = "Running Follow-up",
      tagList(
        uiOutput("follow_up_progressbar"),
        div(style = "margin-top:8px;", textOutput("follow_up_status_line"))
      ),
      footer = uiOutput("runner_footer"),
      easyClose = FALSE,
      size = "l"
    ))

    scriptPath <- file.path(epc_path("scripts_dir"), "follow_up", "follow_up.r")
    proc <- process$new(
      "Rscript",
      args = c(scriptPath),
      stdout = "|",
      stderr = "|",
      env = c(EPIC2CASTOR_LOGDIR = getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", "")))
    )
    runnerState$proc <- proc
    runnerState$kind <- "follow_up"
    runnerState$on_complete_handler <- on_complete
    runnerState$error_detected <- FALSE
    runnerState$error_message <- NULL

    run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
    status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"

    output$follow_up_progressbar <- renderUI({
      pct <- 0
      div(class = "progress", style = "height: 20px; background:#eee;",
          div(class = "progress-bar progress-bar-striped active", role = "progressbar",
              style = paste0("width: ", pct, "%;"),
              `aria-valuemin` = 0, `aria-valuemax` = 100,
              paste0(pct, "%"))
      )
    })
    output$follow_up_status_line <- renderText({ "Starting…" })
    
    # Track if we've seen the success marker
    script_success_seen <- FALSE

    observerHandle <- observe({
      reactiveTimer(500)()
      tryCatch({
        out_lines <- character(0)
        err_lines <- character(0)
        try({
          out_lines <- proc$read_output_lines()
          err_lines <- proc$read_error_lines()
        }, silent = TRUE)
        if (length(out_lines)) lapply(out_lines, function(x) cat(paste0("[Follow-up][OUT] ", x, "\n")))
        if (length(err_lines)) lapply(err_lines, function(x) cat(paste0("[Follow-up][ERR] ", x, "\n")))
        combined_lines <- c(out_lines, err_lines)
        
        # Check for success marker in current output
        if (length(combined_lines) && any(grepl("EPIC2CASTOR::DONE", combined_lines, fixed = TRUE))) {
          script_success_seen <<- TRUE
          cat("[Follow-up] SUCCESS marker detected in output\n")
        }
        
        if (length(combined_lines)) {
          err_line <- detect_error_line(combined_lines)
          if (!is.null(err_line)) update_runner_error(err_line, "follow_up_status_line", "follow_up_progressbar")
        }

        # Poll status.json
        if (file.exists(status_path)) {
          st <- try(jsonlite::fromJSON(status_path), silent = TRUE)
          if (!inherits(st, "try-error") && is.list(st)) {
            # Percent/current/total defensively parsed; compute pct from current/total when useful
            cur <- suppressWarnings(as.numeric(st$current))
            tot <- suppressWarnings(as.numeric(st$total))
            pct <- suppressWarnings(as.numeric(st$percent))
            if (length(cur) != 1L || is.na(cur) || !is.finite(cur)) cur <- NA_real_
            if (length(tot) != 1L || is.na(tot) || !is.finite(tot) || tot <= 0) tot <- NA_real_
            if (length(pct) != 1L || is.na(pct) || !is.finite(pct)) pct <- NA_real_
            # Prefer computed percent if current/total available
            if (is.finite(cur) && is.finite(tot)) pct <- 100 * cur / tot
            if (is.na(pct)) pct <- 0
            if (pct < 0) pct <- 0
            if (pct > 100) pct <- 100

            # Step/detail coerced to single strings
            step <- st$step
            if (is.null(step) || length(step) != 1L || !nzchar(as.character(step))) step <- "running" else step <- as.character(step)
            step_lc <- tolower(step)
            if (!is.na(step_lc) && identical(step_lc, "done")) updateRunnerFooter(TRUE)
            detail <- st$detail
            if (is.null(detail) || length(detail) == 0L) detail <- "" else detail <- as.character(detail)[1]

            # ETA defensively parsed to numeric seconds
            eta_val <- suppressWarnings(as.numeric(st$eta_s))
            if (!is.null(eta_val) && length(eta_val) == 1L && is.finite(eta_val)) {
              eta_txt <- paste0("ETA ", format(round(eta_val), scientific = FALSE), "s")
            } else eta_txt <- NULL

            if (!runnerState$error_detected) {
              # Check if done: green bar, no animation
              if (!is.na(step_lc) && identical(step_lc, "done")) {
                output$follow_up_progressbar <- renderUI({
                  div(class = "progress", style = "height: 20px; background:#eee;",
                      div(class = "progress-bar progress-bar-success", role = "progressbar",
                          style = paste0("width: 100%; background-color: #5cb85c;"),
                          `aria-valuemin` = 0, `aria-valuemax` = 100,
                          "100%")
                  )
                })
              } else {
                output$follow_up_progressbar <- renderUI({
                  div(class = "progress", style = "height: 20px; background:#eee;",
                      div(class = "progress-bar progress-bar-striped active", role = "progressbar",
                          style = paste0("width: ", pct, "%;"),
                          `aria-valuemin` = 0, `aria-valuemax` = 100,
                          paste0(round(pct), "%"))
                  )
                })
              }
            }
            line <- paste(step, if (nzchar(detail)) paste("-", detail) else "", if (!is.null(eta_txt)) paste("(", eta_txt, ")") else "")
            if (!runnerState$error_detected) output$follow_up_status_line <- renderText({ line })
          }
        }

        # Done: show Close button and stop observer
        if (!proc$is_alive()) {
          updateRunnerFooter(TRUE)
          
          # Check if we have "EPIC2CASTOR::DONE" marker for successful completion
          # First check if we saw it during polling
          script_completed_successfully <- script_success_seen
          
          # Also check leftover lines just in case
          try({
            leftover_out <- proc$read_output_lines()
            leftover_err <- proc$read_error_lines()
            if (length(leftover_out)) lapply(leftover_out, function(x) cat(paste0("[Follow-up][OUT] ", x, "\n")))
            if (length(leftover_err)) lapply(leftover_err, function(x) cat(paste0("[Follow-up][ERR] ", x, "\n")))
            leftover_lines <- c(leftover_out, leftover_err)
            
            # Check for success marker in leftover lines
            if (!script_completed_successfully && any(grepl("EPIC2CASTOR::DONE", leftover_lines, fixed = TRUE))) {
              script_completed_successfully <- TRUE
              cat("[Follow-up] SUCCESS marker detected in leftover lines\n")
            }
            
            if (length(leftover_lines)) {
              err_line <- detect_error_line(leftover_lines)
              if (!is.null(err_line)) update_runner_error(err_line, "follow_up_status_line", "follow_up_progressbar")
            }
          }, silent = TRUE)
          
          exit_status <- tryCatch(proc$get_exit_status(), error = function(e) NA_integer_)
          if (is.null(exit_status)) exit_status <- NA_integer_
          
          # Check if a missing file was detected (which should trigger the upload modal)
          has_missing_file <- !is.null(isolate(runnerState$missing_file_info))
          
          # Windows crash codes (0xC0000005 = -1073741819): ignore if script was successful
          is_windows_crash <- !is.na(exit_status) && exit_status == -1073741819
          
          # Only report error if:
          # - script didn't complete successfully AND
          # - no error was already detected AND
          # - no missing file was detected AND
          # - not a Windows crash after successful completion
          if (!script_completed_successfully && (is.na(exit_status) || exit_status != 0) && !runnerState$error_detected && !has_missing_file && !is_windows_crash) {
            msg <- sprintf("Follow-up failed (exit %s). Check logs for details.", as.character(exit_status))
            update_runner_error(msg, "follow_up_status_line", "follow_up_progressbar")
          }
          
          # Only show notification for non-zero exit if script didn't complete successfully and no missing file and not a Windows crash
          if (!script_completed_successfully && !is.na(exit_status) && exit_status != 0 && !has_missing_file && !is_windows_crash) {
            tryCatch({
              safeNotify(sprintf("Follow-up script exited with status %s", as.character(exit_status)), "error")
            }, error = function(e) {
              cat(sprintf("[Follow-up] Could not show notification: %s\n", conditionMessage(e)))
            })
          }
          
          observerHandle$destroy()
          runnerState$proc <- NULL
          runnerState$observer <- NULL
          
          # Debug: log completion status
          cat(sprintf("[Follow-up] Script completion status:\n"))
          cat(sprintf("  - script_completed_successfully: %s\n", script_completed_successfully))
          cat(sprintf("  - exit_status: %s\n", as.character(exit_status)))
          cat(sprintf("  - is_windows_crash: %s\n", is_windows_crash))
          cat(sprintf("  - canceled: %s\n", isTRUE(runnerState$canceled)))
          cat(sprintf("  - on_complete is null: %s\n", is.null(on_complete)))
          
          # Call on_complete if script completed successfully OR exit status is 0 (even with Windows crash)
          should_continue <- !isTRUE(runnerState$canceled) && 
                            (script_completed_successfully || (!is.na(exit_status) && exit_status == 0)) && 
                            !is.null(on_complete)
          
          cat(sprintf("  - should_continue: %s\n", should_continue))
          
          if (should_continue) {
            cat("[Follow-up] Calling on_complete callback\n")
            later::later(on_complete, delay = 0.05)
          } else {
            cat("[Follow-up] NOT calling on_complete callback\n")
          }
        }
      }, error = function(e) {
        output$follow_up_status_line <- renderText({ paste("Monitor error:", conditionMessage(e)) })
        updateRunnerFooter(TRUE)
        observerHandle$destroy()
        runnerState$proc <- NULL
        runnerState$observer <- NULL
        if (!is.null(on_complete)) later::later(on_complete, delay = 0.05)
      })
    })
    runnerState$observer <- observerHandle
  }

  # ============================================================================
  # BIOBANK SCRIPT RUNNER
  # ============================================================================
  # Executes the biobank_data.r script with real-time progress monitoring.
  # Similar to baseline and follow-up runners, monitors status.json for
  # progress updates and displays them in a modal dialog.
  #
  # Process:
  # 1. Clear previous status.json file
  # 2. Launch Rscript process with biobank_data.r
  # 3. Monitor process output and status.json every 500ms
  # 4. Update progress bar and status text based on status.json
  # 5. Handle completion, errors, and Windows crash codes
  # 6. Call on_complete callback when script finishes successfully
  #
  # @param on_complete Optional callback function to execute after successful completion
  
  run_biobank_script <- function(on_complete = NULL) {
    removeModalSafe()
    updateRunnerFooter(FALSE)
    {
      run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
      status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"
      if (file.exists(status_path)) try(file.remove(status_path), silent = TRUE)
    }
    runnerState$canceled <- FALSE
    showModalSafe(modalDialog(
      title = "Running Biobank Data",
      tagList(
        uiOutput("biobank_progressbar"),
        div(style = "margin-top:8px;", textOutput("biobank_status_line"))
      ),
      footer = uiOutput("runner_footer"),
      easyClose = FALSE,
      size = "l"
    ))

    scriptPath <- file.path(epc_path("scripts_dir"), "biobank_data", "biobank_data.r")
    proc <- process$new(
      "Rscript",
      args = c(scriptPath),
      stdout = "|",
      stderr = "|",
      env = c(EPIC2CASTOR_LOGDIR = getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", "")))
    )
    runnerState$proc <- proc
    runnerState$kind <- "biobank_data"
    runnerState$on_complete_handler <- on_complete
    runnerState$error_detected <- FALSE
    runnerState$error_message <- NULL

    run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
    status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"

    output$biobank_progressbar <- renderUI({
      div(class = "progress", style = "height: 20px; background:#eee;",
          div(class = "progress-bar progress-bar-striped active", role = "progressbar",
              style = "width: 0%;",
              `aria-valuemin` = 0, `aria-valuemax` = 100,
              "0%")
      )
    })
    output$biobank_status_line <- renderText({ "Starting…" })

    observerHandle <- observe({
      reactiveTimer(500)()
      tryCatch({
        out_lines <- character(0)
        err_lines <- character(0)
        try({
          out_lines <- proc$read_output_lines()
          err_lines <- proc$read_error_lines()
        }, silent = TRUE)
        if (length(out_lines)) lapply(out_lines, function(x) cat(paste0("[Biobank][OUT] ", x, "\n")))
        if (length(err_lines)) lapply(err_lines, function(x) cat(paste0("[Biobank][ERR] ", x, "\n")))
        combined_lines <- c(out_lines, err_lines)
        if (length(combined_lines)) {
          err_line <- detect_error_line(combined_lines)
          if (!is.null(err_line)) {
            update_runner_error(err_line, "biobank_status_line", "biobank_progressbar")
          }
        }

        if (file.exists(status_path)) {
          st <- try(jsonlite::fromJSON(status_path), silent = TRUE)
          if (!inherits(st, "try-error") && is.list(st)) {
            # Compute percent from current/total when available; clamp [0,100]
            cur <- suppressWarnings(as.numeric(st$current))
            tot <- suppressWarnings(as.numeric(st$total))
            pct <- suppressWarnings(as.numeric(st$percent))
            if (length(cur) != 1L || is.na(cur) || !is.finite(cur)) cur <- NA_real_
            if (length(tot) != 1L || is.na(tot) || !is.finite(tot) || tot <= 0) tot <- NA_real_
            if (length(pct) != 1L || is.na(pct) || !is.finite(pct)) pct <- NA_real_
            if (is.finite(cur) && is.finite(tot)) pct <- 100 * cur / tot
            if (is.na(pct)) pct <- 0
            if (pct < 0) pct <- 0
            if (pct > 100) pct <- 100

            step <- st$step
            if (is.null(step) || length(step) != 1L || !nzchar(as.character(step))) step <- "running" else step <- as.character(step)
            step_lc <- tolower(step)
            if (!is.na(step_lc) && identical(step_lc, "done")) updateRunnerFooter(TRUE)
            detail <- st$detail
            if (is.null(detail) || length(detail) == 0L) detail <- "" else detail <- as.character(detail)[1]
            eta_val <- suppressWarnings(as.numeric(st$eta_s))
            if (!is.null(eta_val) && length(eta_val) == 1L && is.finite(eta_val)) {
              eta_txt <- paste0("ETA ", format(round(eta_val), scientific = FALSE), "s")
            } else eta_txt <- NULL

            if (!runnerState$error_detected) {
              # Check if done: green bar, no animation
              if (!is.na(step_lc) && identical(step_lc, "done")) {
                output$biobank_progressbar <- renderUI({
                  div(class = "progress", style = "height: 20px; background:#eee;",
                      div(class = "progress-bar progress-bar-success", role = "progressbar",
                          style = paste0("width: 100%; background-color: #5cb85c;"),
                          `aria-valuemin` = 0, `aria-valuemax` = 100,
                          "100%")
                  )
                })
              } else {
                output$biobank_progressbar <- renderUI({
                  div(class = "progress", style = "height: 20px; background:#eee;",
                      div(class = "progress-bar progress-bar-striped active", role = "progressbar",
                          style = paste0("width: ", pct, "%;"),
                          `aria-valuemin` = 0, `aria-valuemax` = 100,
                          paste0(round(pct), "%"))
                  )
                })
              }
            }
            line <- paste(step, if (nzchar(detail)) paste("-", detail) else "", if (!is.null(eta_txt)) paste("(", eta_txt, ")") else "")
            if (!runnerState$error_detected) output$biobank_status_line <- renderText({ line })
          }
        }

        if (!proc$is_alive()) {
          updateRunnerFooter(TRUE)
          
          # Check if we have "EPIC2CASTOR::DONE" marker for successful completion
          script_completed_successfully <- FALSE
          
          try({
            leftover_out <- proc$read_output_lines()
            leftover_err <- proc$read_error_lines()
            if (length(leftover_out)) lapply(leftover_out, function(x) cat(paste0("[Biobank][OUT] ", x, "\n")))
            if (length(leftover_err)) lapply(leftover_err, function(x) cat(paste0("[Biobank][ERR] ", x, "\n")))
            leftover_lines <- c(leftover_out, leftover_err)
            
            # Check for success marker
            if (any(grepl("EPIC2CASTOR::DONE", leftover_lines, fixed = TRUE))) {
              script_completed_successfully <- TRUE
            }
            
            if (length(leftover_lines)) {
              err_line <- detect_error_line(leftover_lines)
              if (!is.null(err_line)) {
                update_runner_error(err_line, "biobank_status_line", "biobank_progressbar")
              }
            }
          }, silent = TRUE)
          
          exit_status <- tryCatch(proc$get_exit_status(), error = function(e) NA_integer_)
          if (is.null(exit_status)) exit_status <- NA_integer_
          
          # Windows crash codes: ignore if script was successful
          is_windows_crash <- !is.na(exit_status) && exit_status == -1073741819
          
          # Only report error if script didn't complete successfully and not a Windows crash
          if (!script_completed_successfully && (is.na(exit_status) || exit_status != 0) && !runnerState$error_detected && !is_windows_crash) {
            msg <- sprintf("Biobank script failed (exit %s). Check logs for details.", as.character(exit_status))
            update_runner_error(msg, "biobank_status_line", "biobank_progressbar")
          }
          
          # Only show notification for non-zero exit if script didn't complete successfully and not a Windows crash
          if (!script_completed_successfully && !is.na(exit_status) && exit_status != 0 && !is_windows_crash) {
            tryCatch({
              safeNotify(sprintf("Biobank script exited with status %s", as.character(exit_status)), "error")
            }, error = function(e) {
              cat(sprintf("[Biobank] Could not show notification: %s\n", conditionMessage(e)))
            })
          }
          
          observerHandle$destroy()
          runnerState$proc <- NULL
          runnerState$observer <- NULL
          
          # Call on_complete if script completed successfully OR exit status is 0 OR Windows crash after success
          if (!isTRUE(runnerState$canceled) && (script_completed_successfully || (!is.na(exit_status) && exit_status == 0) || (script_completed_successfully && is_windows_crash)) && !is.null(on_complete)) {
            later::later(on_complete, delay = 0.05)
          }
        }
      }, error = function(e) {
        output$biobank_status_line <- renderText({ paste("Monitor error:", conditionMessage(e)) })
        updateRunnerFooter(TRUE)
        observerHandle$destroy()
        runnerState$proc <- NULL
        runnerState$observer <- NULL
        if (!isTRUE(runnerState$canceled) && !is.null(on_complete)) later::later(on_complete, delay = 0.05)
      })
    })
    runnerState$observer <- observerHandle
  }

  # ============================================================================
  # MAIN SCRIPT RUNNER - SELECT AND EXECUTE MULTIPLE SCRIPTS
  # ============================================================================
  # Allows user to select which output scripts to run (baseline, follow-up, biobank).
  # Scripts are executed sequentially using a chaining mechanism where each
  # script's on_complete callback triggers the next script in the chain.
  
  observeEvent(input$run_main_script, {
    showModalSafe(modalDialog(
      title = "Select outputs to generate",
      tagList(
        checkboxInput("select_baseline", "Baseline", value = FALSE),
        checkboxInput("select_followup", "Follow-up", value = FALSE),
        checkboxInput("select_biobank", "BioBank Data", value = FALSE)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_run_selected_scripts", "Run", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$confirm_run_selected_scripts, {
    removeModalSafe()

    tasks <- character(0)
    if (isTRUE(input$select_baseline)) tasks <- c(tasks, "baseline")
    if (isTRUE(input$select_followup)) tasks <- c(tasks, "followup")
    if (isTRUE(input$select_biobank))  tasks <- c(tasks, "biobank")

    run_chain <- NULL
    run_chain <- function(i) {
      if (i > length(tasks)) return(invisible(NULL))
      t <- tasks[[i]]
      if (t == "baseline") {
        run_baseline_script(on_complete = function() run_chain(i + 1))
      } else if (t == "followup") {
        run_follow_up_script(on_complete = function() run_chain(i + 1))
      } else if (t == "biobank") {
        run_biobank_script(on_complete = function() run_chain(i + 1))
      } else {
        # unknown task, skip and continue
        run_chain(i + 1)
      }
    }

    if (length(tasks) > 0) {
      later::later(function() run_chain(1), delay = 0.05)
    } else {
      safeNotify("No outputs selected", type = "message")
    }
  })
  
  # ============================================================================
  # CASTOR UPLOAD RUNNER - SELECT AND UPLOAD MULTIPLE DATASETS
  # ============================================================================
  # Allows user to select which datasets to upload to Castor (baseline, follow-up, biobank)
  # and which site to upload to. Each upload script is executed sequentially with
  # progress monitoring via status.json.
  #
  # Process:
  # 1. Show modal with dataset checkboxes and site selector
  # 2. Asynchronously load available sites from Castor API
  # 3. On confirm, execute selected uploads sequentially
  # 4. Each upload monitors status.json for progress updates
  # 5. Display progress bar and status text in modal
  
  observeEvent(input$run_upload_script, {
    showModalSafe(modalDialog(
      title = "Select outputs and site for Castor Upload",
      tagList(
        fluidRow(
          column(12,
                 tags$strong("Which datasets to upload?"),
                 checkboxInput("upload_select_baseline", "Baseline", value = FALSE),
                 checkboxInput("upload_select_followup", "Follow-up", value = FALSE),
                 checkboxInput("upload_select_biobank", "BioBank Data", value = FALSE),
                 tags$hr(style = "margin-top:6px; margin-bottom:10px;")
          )
        ),
        div(id = "site_loader", "Loading sites…"),
        uiOutput("site_select_ui")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_upload", "OK", class = "btn-primary")
      ),
      easyClose = FALSE
    ))

    # Asynchronously load sites so the UI appears quickly
    later::later(function(){
      choices <- tryCatch(get_site_choices(), error = function(e) NULL)
      if (is.null(choices) || length(choices) == 0) {
        output$site_select_ui <- renderUI({ tags$div(style='color:red;', 'Error fetching sites. Please try again.') })
      } else {
        output$site_select_ui <- renderUI({ selectInput("upload_site", "Choose a site:", choices = choices) })
      }
      removeUI(selector = "#site_loader", multiple = FALSE, immediate = TRUE, session = session)
    }, delay = 0.05)
  })

  observeEvent(input$confirm_upload, {
    if (is.null(input$upload_site) || !nzchar(input$upload_site)) {
      safeNotify("Please select a site first", type = "error"); return()
    }
    tasks <- character(0)
    if (isTRUE(input$upload_select_baseline)) tasks <- c(tasks, "baseline")
    if (isTRUE(input$upload_select_followup)) tasks <- c(tasks, "followup")
    if (isTRUE(input$upload_select_biobank))  tasks <- c(tasks, "biobank")
    if (length(tasks) == 0) { safeNotify("No datasets selected", type = "warning"); return() }

    selected_site_full <- input$upload_site
    removeModal()

    run_one_upload <- function(task, on_done) {
      implemented <- task %in% c("baseline", "biobank", "followup")
      if (!implemented) {
        safeNotify(paste("Upload for", task, "not yet implemented - skipped."), type = "message")
        on_done(); return(invisible(NULL))
      }

      run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
      status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"
      if (file.exists(status_path)) try(file.remove(status_path), silent = TRUE)

  updateRunnerFooter(FALSE)
      runnerState$canceled <- FALSE
      showModalSafe(modalDialog(
        title = paste0("Castor Upload - ", task),
        tagList(
          uiOutput("upload_progressbar"),
          div(style = "margin-top:8px;", textOutput("upload_status_line"))
        ),
        footer = uiOutput("runner_footer"),
        easyClose = FALSE,
        size = "l"
      ))

      scriptPath <- switch(task,
           baseline = file.path(epc_path("baseline_scripts_dir"), "baselineExport.r"),
           biobank  = file.path(epc_path("scripts_dir"), "biobank_data", "biobankExport.r"),
           followup = file.path(epc_path("scripts_dir"), "follow_up", "follow_upExport.r"),
           file.path(epc_path("baseline_scripts_dir"), "baselineExport.r"))
      proc <- process$new(
        "Rscript",
        args = c(scriptPath, selected_site_full),
        stdout = if (.Platform$OS.type == "windows") "NUL" else "/dev/null",
        stderr = if (.Platform$OS.type == "windows") "NUL" else "/dev/null",
        env = c(EPIC2CASTOR_LOGDIR = getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", "")))
      )
      runnerState$proc <- proc
      runnerState$kind <- paste0("upload:", task)

      output$upload_progressbar <- renderUI({
        div(class = "progress", style = "height: 20px; background:#eee;",
            div(class = "progress-bar", role = "progressbar",
                style = "width: 0%;",
                `aria-valuemin` = 0, `aria-valuemax` = 100,
                "0%")
        )
      })
      output$upload_status_line <- renderText({ "Starting…" })

      observerHandle <- observe({
        reactiveTimer(500)()
        tryCatch({
          if (file.exists(status_path)) {
            st <- try(jsonlite::fromJSON(status_path), silent = TRUE)
            if (!inherits(st, "try-error") && is.list(st)) {
              cur <- suppressWarnings(as.numeric(st$current))
              tot <- suppressWarnings(as.numeric(st$total))
              pct <- suppressWarnings(as.numeric(st$percent))
              if (length(cur) != 1L || is.na(cur) || !is.finite(cur)) cur <- NA_real_
              if (length(tot) != 1L || is.na(tot) || !is.finite(tot) || tot <= 0) tot <- NA_real_
              if (length(pct) != 1L || is.na(pct) || !is.finite(pct)) pct <- NA_real_
              if (is.finite(cur) && is.finite(tot)) pct <- 100 * cur / tot
              if (is.na(pct)) pct <- 0
              if (pct < 0) pct <- 0
              if (pct > 100) pct <- 100

              step <- st$step
              if (is.null(step) || length(step) != 1L || !nzchar(as.character(step))) step <- "running" else step <- as.character(step)
              detail <- st$detail
              if (is.null(detail) || length(detail) == 0L) detail <- "" else detail <- as.character(detail)[1]
              eta_val <- suppressWarnings(as.numeric(st$eta_s))
              if (!is.null(eta_val) && length(eta_val) == 1L && is.finite(eta_val)) {
                eta_txt <- paste0("ETA ", format(round(eta_val), scientific = FALSE), "s")
              } else eta_txt <- NULL

              curtot_txt <- NULL
              if (is.finite(cur) && is.finite(tot)) {
                cur_str <- format(round(cur), big.mark = ",", trim = TRUE)
                tot_str <- format(round(tot), big.mark = ",", trim = TRUE)
                curtot_txt <- paste0("[", cur_str, "/", tot_str, "]")
              }

              output$upload_progressbar <- renderUI({
                div(class = "progress", style = "height: 20px; background:#eee;",
                    div(class = "progress-bar", role = "progressbar",
                        style = paste0("width: ", pct, "%;"),
                        `aria-valuemin` = 0, `aria-valuemax` = 100,
                        paste0(round(pct), "%"))
                )
              })
              line <- paste(
                step,
                if (nzchar(detail)) paste("-", detail) else "",
                if (!is.null(curtot_txt)) curtot_txt else "",
                if (!is.null(eta_txt)) paste("(", eta_txt, ")") else ""
              )
              output$upload_status_line <- renderText({ line })

              step_lc <- tolower(as.character(step))
              if (!is.na(step_lc) && identical(step_lc, "done")) {
                updateRunnerFooter(TRUE)
              }
            }
          }

          if (!proc$is_alive()) {
            updateRunnerFooter(TRUE)
            observerHandle$destroy()
            # Cleanup: remove process and observer from state to prevent memory leak
            runnerState$observer <- NULL
            runnerState$proc <- NULL
            # Start next task after short delay
            if (!isTRUE(runnerState$canceled) && !is.null(on_done)) later::later(on_done, delay = 0.05)
          }
        }, error = function(e) {
          output$upload_status_line <- renderText({ paste("Monitor error:", conditionMessage(e)) })
          updateRunnerFooter(TRUE)
          observerHandle$destroy()
          # Cleanup also on error
          runnerState$observer <- NULL
          runnerState$proc <- NULL
          if (!isTRUE(runnerState$canceled) && !is.null(on_done)) later::later(on_done, delay = 0.05)
        })
      })
      runnerState$observer <- observerHandle
    }

    # Chain selected upload tasks
    i_run <- function(i){
      if (i > length(tasks)) { return(invisible(NULL)) }
      run_one_upload(tasks[[i]], on_done = function() i_run(i + 1))
    }
    later::later(function() i_run(1), delay = 0.05)
  })

  # Cancel button: stop running process, close modal, log notification and block chaining
    observeEvent(input$cancel_modal, {
        kind_txt <- if (is.null(runnerState$kind)) "unknown" else runnerState$kind
        runnerState$canceled <- TRUE
        updateRunnerFooter(TRUE)
        stop_runner(detail = "Canceled by user", severity = "WARN")
        try({
          cat(sprintf("[Runner] Process '%s' canceled by user at %s\n",
                      kind_txt,
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
        }, silent = TRUE)
    })

  observeEvent(input$update_credentials, {
    config <- read_json(epc_path("config_api"), simplifyVector = TRUE)
    showModalSafe(modalDialog(
      title = "Update Castor API Credentials",
      tagList(
        div(
          tags$div(
            tags$span("Client ID"),
            tags$span(icon("question-circle"),
                      onclick = "customTogglePopover(this);",
                      `data-toggle` = "popover",
                      `data-trigger` = "manual",
                      `data-container` = "body",
                      `data-placement` = "right",
                      title = "Client ID Info",
                      `data-content` = "To find your client ID, click on the account button in the lower left corner of your Castor page. Then click 'Settings' and then 'Castor EDC API'.",
                      style = "margin-left: 5px; cursor: pointer;")
          ),
          textInput("client_id", label = NULL,
                    value = config$client_id,
                    width = "500px")
        ),
        div(
          tags$div(
            tags$span("Client Secret"),
            tags$span(icon("question-circle"),
                      onclick = "customTogglePopover(this);",
                      `data-toggle` = "popover",
                      `data-trigger` = "manual",
                      `data-container` = "body",
                      `data-placement` = "right",
                      title = "Client Secret Info",
                      `data-content` = "To find your client secret, click on the account button in the lower left corner of your Castor page. Then click 'Settings' and then 'Castor EDC API'. Note: This secret is only shown once—make sure to save it.",
                      style = "margin-left: 5px; cursor: pointer;")
          ),
          textInput("client_secret", label = NULL,
                    value = config$client_secret,
                    width = "500px")
        ),
        div(
          tags$div(
            tags$span("Study ID"),
            tags$span(icon("question-circle"),
                      onclick = "customTogglePopover(this);",
                      `data-toggle` = "popover",
                      `data-trigger` = "manual",
                      `data-container` = "body",
                      `data-placement` = "right",
                      title = "Study ID Info",
                      `data-content` = "To find your study ID, go to the study overview page in Castor. Click the study you want to use and then open 'Settings'.",
                      style = "margin-left: 5px; cursor: pointer;")
          ),
          textInput("study_id", label = NULL,
                    value = config$study_id,
                    width = "500px")
        ),
        div(
          tags$div(
            tags$span("DeepL API Key (Optional)"),
            tags$span(icon("question-circle"),
                      onclick = "customTogglePopover(this);",
                      `data-toggle` = "popover",
                      `data-trigger` = "manual",
                      `data-container` = "body",
                      `data-placement` = "right",
                      title = "DeepL API Key Info",
                      `data-content` = "DeepL provides high-quality translations for auto-fill functionality. Get a free API key at https://www.deepl.com/pro-api (500,000 characters/month free). If not provided, the system will use MyMemory as a free fallback.",
                      style = "margin-left: 5px; cursor: pointer;")
          ),
          textInput("deepl_api_key", label = NULL,
                    value = if (!is.null(config$deepl_api_key)) config$deepl_api_key else "",
                    width = "500px",
                    placeholder = "Optional: Enter DeepL API key for better translations")
        )
      ),
      footer = tagList(
        actionButton("save_credentials", "Save", class = "btn-primary"),
        tags$button("Cancel", type = "button", class = "btn btn-default modal-cancel", `data-dismiss` = "modal")
      )
    ))
    session$sendCustomMessage("initPopovers", list())
  })
  
  observeEvent(input$save_credentials, {
    req(input$client_id, input$client_secret, input$study_id)
    newConfig <- list(
      client_id = input$client_id,
      client_secret = input$client_secret,
      study_id = input$study_id,
      deepl_api_key = if (!is.null(input$deepl_api_key)) input$deepl_api_key else ""
    )
    write_json(newConfig, path = epc_path("config_api"), pretty = TRUE, auto_unbox = TRUE)
    removeModalSafe()
    
    # Remove the credentials warning notification if it exists
    removeNotification("credentials_warning")
    
    cat("API credentials have been updated successfully!\n")
    showModalSafe(modalDialog(
      title = "Success",
      tagList(
        p(icon("check-circle"), " API credentials have been updated successfully!"),
        p("You can now refresh the Castor metadata by clicking 'Refresh metadata' in the Castor menu.")
      ),
      easyClose = TRUE
    ))
  })
  
  # ============================================================================
  # KEYBOARD SHORTCUTS MODAL
  # ============================================================================
  # Show modal with all available keyboard shortcuts and their descriptions
  
  observeEvent(input$show_shortcuts, {
    # Load keyboard shortcuts configuration
    shortcuts_config <- tryCatch({
      jsonlite::fromJSON(file.path("config", "keyboard_shortcuts.json"))
    }, error = function(e) {
      NULL
    })
    
    # Generate keyboard shortcuts HTML
    keyboard_shortcuts_html <- if (!is.null(shortcuts_config)) {
      generate_keyboard_shortcuts_section(shortcuts_config)
    } else {
      tags$p(
        style = "color: #e74c3c; padding: 10px; background: #fadbd8;",
        icon("exclamation-triangle"), " Keyboard shortcuts configuration not found."
      )
    }
    
    showModalSafe(modalDialog(
      title = tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        icon("keyboard", style = "font-size: 24px;"),
        "User Guide & Keyboard Shortcuts"
      ),
      size = "l",
      easyClose = TRUE,
      footer = tags$button("Close", type = "button", class = "btn btn-primary", `data-dismiss` = "modal"),
      
      tags$div(
        style = "font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;",
        
        # Keyboard Shortcuts Section (NEW - at the top)
        keyboard_shortcuts_html,
        
        # Performance Monitoring Section
        tags$div(
          style = "margin-bottom: 25px;",
          tags$h4(
            style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px; margin-bottom: 15px;",
            icon("chart-line"), " Performance Monitoring"
          ),
          tags$table(
            class = "table table-hover",
            style = "margin-bottom: 0;",
            tags$tbody(
              tags$tr(
                tags$td(tags$kbd("Ctrl"), " + ", tags$kbd("Shift"), " + ", tags$kbd("P"), style = "width: 250px; font-weight: 500;"),
                tags$td("Toggle performance panel (shows metrics & memory usage)")
              ),
              tags$tr(
                tags$td(tags$kbd("Ctrl"), " + ", tags$kbd("Shift"), " + ", tags$kbd("L")),
                tags$td("Log performance summary to console")
              )
            )
          )
        ),
        
        # Table Navigation Section
        tags$div(
          style = "margin-bottom: 25px;",
          tags$h4(
            style = "color: #2c3e50; border-bottom: 2px solid #27ae60; padding-bottom: 8px; margin-bottom: 15px;",
            icon("table"), " Table Navigation"
          ),
          tags$table(
            class = "table table-hover",
            style = "margin-bottom: 0;",
            tags$tbody(
              tags$tr(
                tags$td(tags$kbd("Click"), " on tab", style = "width: 250px; font-weight: 500;"),
                tags$td("Switch to tab")
              ),
              tags$tr(
                tags$td(tags$kbd("Double-click"), " on tab"),
                tags$td("Rename tab (inline editing)")
              ),
              tags$tr(
                tags$td(tags$kbd("×"), " button on tab"),
                tags$td("Close tab (requires confirmation)")
              )
            )
          )
        ),
        
        # Clipboard Operations Section
        tags$div(
          style = "margin-bottom: 25px;",
          tags$h4(
            style = "color: #2c3e50; border-bottom: 2px solid #e67e22; padding-bottom: 8px; margin-bottom: 15px;",
            icon("clipboard"), " Clipboard Operations"
          ),
          tags$p(
            style = "background-color: #fff3cd; padding: 10px; border-left: 4px solid #ffc107; margin-bottom: 15px;",
            icon("info-circle"), " ", tags$strong("Note:"), " Copy/Cut/Paste only works for the 'elements' table to maintain data integrity."
          ),
          tags$table(
            class = "table table-hover",
            style = "margin-bottom: 0;",
            tags$tbody(
              tags$tr(
                tags$td(tags$button(icon("copy"), class = "btn btn-xs", style = "background-color:#007bff; color:white;"), " button", style = "width: 250px; font-weight: 500;"),
                tags$td("Copy selected rows (with related checkboxes/radiobuttons)")
              ),
              tags$tr(
                tags$td(tags$button(icon("cut"), class = "btn btn-xs", style = "background-color:#ff9800; color:white;"), " button"),
                tags$td("Cut selected rows (removes from current tab)")
              ),
              tags$tr(
                tags$td(tags$button(icon("paste"), class = "btn btn-xs", style = "background-color:#28a745; color:white;"), " button"),
                tags$td("Paste copied/cut rows to current tab")
              )
            )
          )
        ),
        
        # Row Operations Section
        tags$div(
          style = "margin-bottom: 25px;",
          tags$h4(
            style = "color: #2c3e50; border-bottom: 2px solid #9c27b0; padding-bottom: 8px; margin-bottom: 15px;",
            icon("list"), " Row Operations"
          ),
          tags$table(
            class = "table table-hover",
            style = "margin-bottom: 0;",
            tags$tbody(
              tags$tr(
                tags$td(tags$button("+", class = "btn btn-xs btn-success"), " button", style = "width: 250px; font-weight: 500;"),
                tags$td("Add new row at bottom of table")
              ),
              tags$tr(
                tags$td(tags$button("-", class = "btn btn-xs btn-danger"), " button"),
                tags$td("Delete selected rows (requires confirmation)")
              ),
              tags$tr(
                tags$td(tags$button(icon("arrows-alt-v"), class = "btn btn-xs", style = "background-color:#9c27b0; color:white;"), " button"),
                tags$td("Move selected rows to another position")
              )
            )
          )
        ),
        
        # Cell Editing Section
        tags$div(
          style = "margin-bottom: 25px;",
          tags$h4(
            style = "color: #2c3e50; border-bottom: 2px solid #e74c3c; padding-bottom: 8px; margin-bottom: 15px;",
            icon("edit"), " Cell Editing"
          ),
          tags$table(
            class = "table table-hover",
            style = "margin-bottom: 0;",
            tags$tbody(
              tags$tr(
                tags$td(tags$kbd("Click"), " on cell", style = "width: 250px; font-weight: 500;"),
                tags$td("Edit cell value (text cells)")
              ),
              tags$tr(
                tags$td(tags$kbd("Click"), " on dropdown"),
                tags$td("Open dropdown menu (lazy-loaded on first click)")
              ),
              tags$tr(
                tags$td(tags$kbd("Double-click"), " on dropdown"),
                tags$td("Edit dropdown value directly (manual entry)")
              ),
              tags$tr(
                tags$td(tags$strong("Add value..."), " option"),
                tags$td("Add custom value to dropdown options")
              )
            )
          )
        ),
        
        # Tab Management Section
        tags$div(
          style = "margin-bottom: 25px;",
          tags$h4(
            style = "color: #2c3e50; border-bottom: 2px solid #17a2b8; padding-bottom: 8px; margin-bottom: 15px;",
            icon("folder-plus"), " Tab Management"
          ),
          tags$table(
            class = "table table-hover",
            style = "margin-bottom: 0;",
            tags$tbody(
              tags$tr(
                tags$td(tags$button("+ New Tab", class = "btn btn-xs btn-success"), " button", style = "width: 250px; font-weight: 500;"),
                tags$td("Create new tab (empty or copy data from existing tab)")
              ),
              tags$tr(
                tags$td(tags$kbd("×"), " on tab"),
                tags$td("Close tab (confirmation required if more than 1 tab exists)")
              ),
              tags$tr(
                tags$td(tags$kbd("Double-click"), " on tab name"),
                tags$td("Rename tab inline (max 50 characters)")
              )
            )
          )
        ),
        
        # Auto-Fill Section
        tags$div(
          style = "margin-bottom: 25px;",
          tags$h4(
            style = "color: #2c3e50; border-bottom: 2px solid #ff5722; padding-bottom: 8px; margin-bottom: 15px;",
            icon("magic"), " Auto-Fill (waarde tables only)"
          ),
          tags$p(
            style = "background-color: #fff3cd; padding: 10px; border-left: 4px solid #ffc107; margin-bottom: 15px;",
            icon("info-circle"), " ", tags$strong("Note:"), " Auto-fill is only available for waarde_radiobuttons and waarde_checkboxes tables."
          ),
          tags$table(
            class = "table table-hover",
            style = "margin-bottom: 0;",
            tags$tbody(
              tags$tr(
                tags$td(tags$button(icon("magic"), " Auto-fill", class = "btn btn-xs btn-warning"), " button", style = "width: 250px; font-weight: 500;"),
                tags$td("Automatically suggests Dutch translations for EPIC values")
              ),
              tags$tr(
                tags$td(tags$strong("Translation source")),
                tags$td("Uses DeepL API (if configured) or MyMemory fallback + Medical Dictionary")
              ),
              tags$tr(
                tags$td(tags$strong("Fuzzy matching")),
                tags$td("Suggests similar values from existing Castor field options")
              ),
              tags$tr(
                tags$td(tags$strong("Batch processing")),
                tags$td("Processes selected rows or all empty cells in current tab")
              )
            )
          )
        ),
        
        # Menu Bar Functions Section
        tags$div(
          style = "margin-bottom: 25px;",
          tags$h4(
            style = "color: #2c3e50; border-bottom: 2px solid #607d8b; padding-bottom: 8px; margin-bottom: 15px;",
            icon("bars"), " Menu Bar Functions"
          ),
          tags$table(
            class = "table table-hover",
            style = "margin-bottom: 0;",
            tags$tbody(
              tags$tr(
                tags$td(tags$strong("File → Save changes"), style = "width: 250px; font-weight: 500;"),
                tags$td("Save all changes to database and CSV files")
              ),
              tags$tr(
                tags$td(tags$strong("File → Undo all changes")),
                tags$td("Revert to last saved state (reload from database)")
              ),
              tags$tr(
                tags$td(tags$strong("File → Manage input files")),
                tags$td("Select, view, and delete EPIC input CSV/Excel files")
              ),
              tags$tr(
                tags$td(tags$strong("Castor → Update credentials")),
                tags$td("Configure Castor API credentials and DeepL API key")
              ),
              tags$tr(
                tags$td(tags$strong("Castor → Refresh metadata")),
                tags$td("Update field definitions and options from Castor API")
              ),
              tags$tr(
                tags$td(tags$strong("Castor → Medical Dictionary")),
                tags$td("Manage common and medical term translations")
              ),
              tags$tr(
                tags$td(tags$strong("Castor → Create CSVs")),
                tags$td("Generate baseline, follow-up, and/or biobank export files")
              ),
              tags$tr(
                tags$td(tags$strong("Castor → Castor upload")),
                tags$td("Upload generated CSVs directly to Castor EDC")
              ),
              tags$tr(
                tags$td(tags$strong("Import → Import Wizard")),
                tags$td("Step-by-step wizard for importing external data files")
              ),
              tags$tr(
                tags$td(tags$strong("Help → User Guide")),
                tags$td("Show this help dialog with all shortcuts and features")
              ),
              tags$tr(
                tags$td(tags$strong("Help → About")),
                tags$td("Application information and version details")
              )
            )
          )
        ),
        
        # Import Wizard Section (NEW)
        tags$div(
          style = "margin-bottom: 25px;",
          tags$h4(
            style = "color: #2c3e50; border-bottom: 2px solid #00bcd4; padding-bottom: 8px; margin-bottom: 15px;",
            icon("file-import"), " Import Wizard"
          ),
          tags$p(
            style = "margin-bottom: 15px;",
            "The Import Wizard provides a step-by-step interface for importing external data files with automatic structure detection, interactive column mapping, and data transformation."
          ),
          tags$table(
            class = "table table-hover",
            style = "margin-bottom: 15px;",
            tags$tbody(
              tags$tr(
                tags$td(tags$strong("Step 1: Select Type"), style = "width: 250px; font-weight: 500;"),
                tags$td("Choose import type (EPIC Baseline/Follow-up, Biobank, Custom)")
              ),
              tags$tr(
                tags$td(tags$strong("Step 2: Upload File")),
                tags$td("Select CSV/Excel file; auto-detects encoding, structure, and sheets")
              ),
              tags$tr(
                tags$td(tags$strong("Step 3: Map Columns")),
                tags$td("Match required columns to your file's columns with validation")
              ),
              tags$tr(
                tags$td(tags$strong("Step 4: Review & Export")),
                tags$td("Preview transformed data and export to CSV")
              )
            )
          ),
          tags$div(
            style = "background-color: #e3f2fd; padding: 12px; border-left: 4px solid #2196f3; border-radius: 4px; margin-bottom: 15px;",
            tags$h5(
              style = "margin-top: 0; color: #1976d2;",
              icon("star"), " Key Features"
            ),
            tags$ul(
              style = "margin-bottom: 0;",
              tags$li(tags$strong("Multi-format support:"), " CSV, Excel (.xlsx/.xls), TSV with automatic detection"),
              tags$li(tags$strong("Sheet selection:"), " Choose specific sheets from Excel workbooks"),
              tags$li(tags$strong("Template system:"), " Save and reuse mapping configurations"),
              tags$li(tags$strong("Smart validation:"), " Real-time feedback on required columns and data types"),
              tags$li(tags$strong("Sample preview:"), " View actual data while mapping columns"),
              tags$li(tags$strong("Direct export:"), " Transform and export to CSV in one workflow")
            )
          ),
          tags$div(
            style = "background-color: #fff3cd; padding: 12px; border-left: 4px solid #ffc107; border-radius: 4px;",
            tags$h5(
              style = "margin-top: 0; color: #856404;",
              icon("lightbulb"), " Tips"
            ),
            tags$ul(
              style = "margin-bottom: 0;",
              tags$li("Use templates for recurring imports with the same structure"),
              tags$li("Check sample data preview to verify correct column selection"),
              tags$li("For Excel files, ensure you've selected the correct sheet"),
              tags$li("All required columns (marked with *) must be mapped before transforming"),
              tags$li("Test with small files first to validate your mapping configuration")
            )
          )
        ),
        
        # Table Controls Section
        tags$div(
          style = "margin-bottom: 25px;",
          tags$h4(
            style = "color: #2c3e50; border-bottom: 2px solid #795548; padding-bottom: 8px; margin-bottom: 15px;",
            icon("sliders-h"), " Table Controls"
          ),
          tags$table(
            class = "table table-hover",
            style = "margin-bottom: 0;",
            tags$tbody(
              tags$tr(
                tags$td(tags$strong("File selector"), style = "width: 250px; font-weight: 500;"),
                tags$td("Switch between mapping tables (elements, waarde_radiobuttons, waarde_checkboxes)")
              ),
              tags$tr(
                tags$td(tags$strong("Search box")),
                tags$td("Filter table rows in real-time (searches all columns)")
              ),
              tags$tr(
                tags$td(tags$strong("Table resize grip")),
                tags$td("Drag the grip in bottom-right corner to resize table"),
                tags$td(),
                tags$td(tags$strong("Auto-fit button")),
                tags$td("Click expand icon to auto-fit table to window width")
              ),
              tags$tr(
                tags$td(icon("exclamation-triangle"), tags$strong(" Warning icon")),
                tags$td("Appears when row count exceeds 200 (performance warning)")
              ),
              tags$tr(
                tags$td(icon("list"), " / ", icon("th"), tags$strong(" Render mode")),
                tags$td("Shows current rendering mode: virtual scrolling or pagination")
              )
            )
          )
        ),
        
        # Tips Section
        tags$div(
          style = "margin-top: 25px; padding: 15px; background-color: #e8f5e9; border-left: 4px solid #4caf50; border-radius: 4px;",
          tags$h5(
            style = "margin-top: 0; color: #2e7d32;",
            icon("lightbulb"), " Tips"
          ),
          tags$ul(
            style = "margin-bottom: 0;",
            tags$li("Use ", tags$kbd("Ctrl+Shift+P"), " to monitor performance during large operations"),
            tags$li("Tab switches preserve your active tab when switching between tables"),
            tags$li("Copy/paste operations automatically handle related checkbox and radiobutton data"),
            tags$li("The performance panel shows real-time memory usage and operation timings")
          )
        )
      )
    ))
  })
  
  # ============================================================================
  # ABOUT MODAL
  # ============================================================================
  # Show application information and version
  
  observeEvent(input$show_about, {
    showModalSafe(modalDialog(
      title = tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        icon("info-circle", style = "font-size: 24px;"),
        "About Epic2Castor"
      ),
      size = "m",
      easyClose = TRUE,
      footer = tags$button("Close", type = "button", class = "btn btn-primary", `data-dismiss` = "modal"),
      
      tags$div(
        style = "text-align: center; padding: 20px;",
        tags$img(src = "img/logo.png", style = "max-width: 200px; margin-bottom: 20px;"),
        tags$h4("Epic2Castor Data Mapping Application"),
        tags$p(
          style = "color: #666; margin-bottom: 30px;",
          "Interactive Shiny application for mapping EPIC hospital data to Castor EDC"
        ),
        tags$hr(),
        tags$div(
          style = "text-align: left;",
          tags$p(tags$strong("Features:")),
          tags$ul(
            tags$li("Excel-like spreadsheet interface for data mapping"),
            tags$li("Tab-based organization for logical data grouping"),
            tags$li("Copy/paste functionality for efficient data entry"),
            tags$li("Auto-fill intelligence with translation APIs"),
            tags$li("Performance monitoring and optimization"),
            tags$li("Direct Castor API integration")
          )
        ),
        tags$hr(),
        tags$p(
          style = "color: #999; font-size: 12px; margin-top: 20px;",
          "Built with Shiny, DataTables, and Select2"
        )
      )
    ))
  })
  
  # Handler for "Update Credentials Now" button from the refresh_castor warning modal
  observeEvent(input$go_to_credentials, {
    removeModalSafe()  # Close the warning modal first
    # Trigger the update_credentials modal
    shinyjs::delay(100, {
      shinyjs::click("update_credentials")
    })
  })
  
  observeEvent(input$close_modal, {
    removeModal()
  })
  observeEvent(input$close_modal_biobank, {
    removeModal()
  })
  observeEvent(input$close_modal_upload, {
    removeModal()
  })
  
  # ============================================================================
  # MEDICAL DICTIONARY MANAGER
  # ============================================================================
  # Provides UI and functionality for managing the medical terms dictionary
  # used by the auto-fill translation system. Users can add, edit, and delete
  # common and medical terms with their English-Dutch translations.
  #
  # Dictionary structure:
  # - common_terms: Frequently used general terms (Yes/No, Male/Female, etc.)
  # - medical_terms: Medical terminology and disease names
  #
  # The dictionary is stored in JSON format and loaded into the auto-fill system.
  
  # Reactive value to store dictionary data
  medicalDictState <- reactiveValues(
    data = NULL,
    modified = FALSE
  )
  
  # Load medical dictionary
  load_medical_dict_data <- function() {
    dict_path <- epc_path("medical_terms_dict")
    if (file.exists(dict_path)) {
      tryCatch({
        dict <- jsonlite::fromJSON(dict_path, simplifyVector = FALSE)
        
        # Convert to data.table for editing
        common_terms <- if (!is.null(dict$common_terms)) {
          data.table(
            english = names(dict$common_terms),
            dutch = unlist(dict$common_terms),
            category = "Common"
          )
        } else {
          data.table(english = character(0), dutch = character(0), category = character(0))
        }
        
        medical_terms <- if (!is.null(dict$medical_terms)) {
          data.table(
            english = names(dict$medical_terms),
            dutch = unlist(dict$medical_terms),
            category = "Medical"
          )
        } else {
          data.table(english = character(0), dutch = character(0), category = character(0))
        }
        
        combined <- rbindlist(list(common_terms, medical_terms))
        return(combined)
      }, error = function(e) {
        return(data.table(english = character(0), dutch = character(0), category = character(0)))
      })
    } else {
      return(data.table(english = character(0), dutch = character(0), category = character(0)))
    }
  }
  
  # Save medical dictionary
  save_medical_dict_data <- function(dict_data) {
    dict_path <- epc_path("medical_terms_dict")
    
    # Convert back to JSON structure
    common <- dict_data[category == "Common"]
    medical <- dict_data[category == "Medical"]
    
    common_list <- if (nrow(common) > 0) {
      setNames(as.list(common$dutch), common$english)
    } else {
      list()
    }
    
    medical_list <- if (nrow(medical) > 0) {
      setNames(as.list(medical$dutch), medical$english)
    } else {
      list()
    }
    
    dict <- list(
      common_terms = common_list,
      medical_terms = medical_list
    )
    
    jsonlite::write_json(dict, dict_path, pretty = TRUE, auto_unbox = TRUE)
  }
  
  # Open medical dictionary manager
  observeEvent(input$manage_medical_dict, {
    # Load current dictionary
    medicalDictState$data <- load_medical_dict_data()
    medicalDictState$modified <- FALSE
    
    showModalSafe(modalDialog(
      title = "Medical Dictionary Manager",
      size = "l",
      div(
        p("Manage translations used by the auto-fill feature. Add, edit, or remove English-Dutch term pairs."),
        
        # Action buttons
        div(style = "margin-bottom: 15px;",
          actionButton("add_dict_term", "Add New Term", icon = icon("plus"), 
                      class = "btn-success btn-sm"),
          actionButton("delete_dict_terms", "Delete Selected", icon = icon("trash"), 
                      class = "btn-danger btn-sm"),
          tags$span(style = "margin-left: 15px; color: #666;",
            textOutput("dict_term_count", inline = TRUE))
        ),
        
        # Dictionary table
        div(style = "max-height: 500px; overflow-y: auto;",
          DT::DTOutput("medical_dict_table")
        )
      ),
      footer = tagList(
        actionButton("save_medical_dict", "Save Changes", class = "btn-primary"),
        modalButton("Cancel")
      ),
      easyClose = FALSE
    ))
  })
  
  # Render dictionary table
  output$medical_dict_table <- DT::renderDT({
    req(medicalDictState$data)
    
    DT::datatable(
      medicalDictState$data,
      selection = 'multiple',
      editable = list(target = 'cell', disable = list(columns = c(2))),  # Make category read-only
      options = list(
        pageLength = 20,
        dom = 'ftp',
        ordering = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 2)
        )
      ),
      colnames = c("English Term", "Dutch Translation", "Category"),
      rownames = FALSE
    )
  })
  
  # Display term count
  output$dict_term_count <- renderText({
    req(medicalDictState$data)
    sprintf("Total terms: %d (Common: %d, Medical: %d)",
            nrow(medicalDictState$data),
            sum(medicalDictState$data$category == "Common"),
            sum(medicalDictState$data$category == "Medical"))
  })
  
  # Handle cell edits
  observeEvent(input$medical_dict_table_cell_edit, {
    info <- input$medical_dict_table_cell_edit
    str(info)
    
    i <- info$row
    j <- info$col + 1  # DT uses 0-based indexing
    v <- info$value
    
    medicalDictState$data[i, j] <- v
    medicalDictState$modified <- TRUE
  })
  
  # Add new term
  observeEvent(input$add_dict_term, {
    showModalSafe(modalDialog(
      title = "Add New Term",
      textInput("new_term_english", "English Term:", placeholder = "e.g., Yes"),
      textInput("new_term_dutch", "Dutch Translation:", placeholder = "e.g., Ja"),
      selectInput("new_term_category", "Category:",
                 choices = c("Common", "Medical"),
                 selected = "Common"),
      footer = tagList(
        actionButton("confirm_add_term", "Add", class = "btn-primary"),
        modalButton("Cancel")
      ),
      easyClose = FALSE
    ))
  })
  
  # Confirm add new term
  observeEvent(input$confirm_add_term, {
    req(input$new_term_english, input$new_term_dutch)
    
    # Check for duplicates
    if (input$new_term_english %in% medicalDictState$data$english) {
      showNotification("This English term already exists in the dictionary.", 
                      type = "warning", duration = 5)
      return()
    }
    
    # Add new row
    new_row <- data.table(
      english = input$new_term_english,
      dutch = input$new_term_dutch,
      category = input$new_term_category
    )
    
    medicalDictState$data <- rbindlist(list(medicalDictState$data, new_row))
    medicalDictState$modified <- TRUE
    
    removeModalSafe()
    showNotification("Term added successfully.", type = "message", duration = 3)
  })
  
  # Delete selected terms
  observeEvent(input$delete_dict_terms, {
    req(input$medical_dict_table_rows_selected)
    
    selected_rows <- input$medical_dict_table_rows_selected
    
    showModalSafe(modalDialog(
      title = "Confirm Deletion",
      sprintf("Are you sure you want to delete %d term(s)?", length(selected_rows)),
      footer = tagList(
        actionButton("confirm_delete_terms", "Delete", class = "btn-danger"),
        modalButton("Cancel")
      ),
      easyClose = FALSE
    ))
  })
  
  # Confirm delete
  observeEvent(input$confirm_delete_terms, {
    req(input$medical_dict_table_rows_selected)
    
    selected_rows <- input$medical_dict_table_rows_selected
    medicalDictState$data <- medicalDictState$data[-selected_rows]
    medicalDictState$modified <- TRUE
    
    removeModalSafe()
    showNotification(sprintf("Deleted %d term(s).", length(selected_rows)), 
                    type = "message", duration = 3)
  })
  
  # Save dictionary
  observeEvent(input$save_medical_dict, {
    req(medicalDictState$data)
    
    tryCatch({
      save_medical_dict_data(medicalDictState$data)
      medicalDictState$modified <- FALSE
      
      removeModalSafe()
      showNotification("Medical dictionary saved successfully!", 
                      type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Error saving dictionary:", e$message), 
                      type = "error", duration = 10)
    })
  })
  
  # ============================================================================
  # AUTOFILL: Auto-Fill EPIC Values Functionality
  # ============================================================================
  # Implements intelligent value matching between EPIC export data and Castor
  # field options. Uses multiple matching strategies:
  #
  # 1. Direct translation via medical dictionary (common_terms + medical_terms)
  # 2. API translation (DeepL if available, MyMemory as fallback)
  # 3. Fuzzy string matching for approximate matches
  #
  # Supports radiobuttons and checkboxes tables. Presents match suggestions
  # in a modal for user review before applying changes.
  #
  # Process:
  # 1. User clicks auto-fill button for radiobuttons/checkboxes table
  # 2. System analyzes empty "waarde" cells and their "Element" values
  # 3. Matches EPIC values against Castor options using multiple strategies
  # 4. Displays preview modal with proposed matches
  # 5. User accepts or cancels
  # 6. Accepted matches are applied to the active tab
  
  # Source autofill script using path configuration
  source(epc_path("autofill_script"), local = TRUE)
  
  # Render autofill button (always visible, disabled for non-supported tables)
  output$autofill_button_ui <- renderUI({
    req(input$file)
    
    is_supported <- input$file %in% c("waarde_radiobuttons", "waarde_checkboxes")
    
    if (is_supported) {
      # Enabled button for radiobuttons/checkboxes
      actionButton("autofill_epic_values",
                   label = icon("magic"),
                   class = "btn btn-footer",
                   style = "background-color:#17a2b8; color:white; min-width:40px; height:34px; padding:6px 12px; margin-right:10px;",
                   title = "Auto-fill EPIC values using smart matching")
    } else {
      # Disabled button for other tables
      actionButton("autofill_epic_values_disabled",
                   label = icon("magic"),
                   class = "btn btn-footer",
                   style = "background-color:#6c757d; color:white; min-width:40px; height:34px; padding:6px 12px; margin-right:10px; cursor:not-allowed; opacity:0.5;",
                   title = "Auto-fill is only available for radiobuttons and checkboxes tables",
                   onclick = "return false;")
    }
  })
  
  # State for autofill results
  autofillState <- reactiveValues(
    results = NULL,
    original_data = NULL,
    table_name = NULL,
    updated_data_to_render = NULL,  # For triggering table re-render after autofill
    active = FALSE,  # Flag to track if autofill process should continue
    pending_table_name = NULL,  # Store table info for config->confirm flow
    pending_tab_name = NULL,
    pending_data = NULL
  )
  
  # Observer: render table when autofill data is updated
  observeEvent(autofillState$updated_data_to_render, {
    req(autofillState$updated_data_to_render, input$file)
    
    render_table(autofillState$updated_data_to_render, input$file, mode = "full")
    
    # Reset after render
    autofillState$updated_data_to_render <- NULL
  })
  
  # Handle autofill configuration confirmation
  observeEvent(input$confirm_autofill_config, {
    req(input$file)
    
    # Save ML setting to config file
    tryCatch({
      settings <- list(
        use_ml_autofill = input$use_ml_autofill,
        ml_confidence_threshold = 50,
        last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      jsonlite::write_json(settings, "config/autofill_settings.json", auto_unbox = TRUE, pretty = TRUE)
    }, error = function(e) {
      cat("Warning: Could not save autofill settings:", conditionMessage(e), "\n")
    })
    
    # Get current data from pending state (set by autofill_epic_values handler)
    table_name <- autofillState$pending_table_name
    current_tab <- autofillState$pending_tab_name
    current_data <- autofillState$pending_data
    
    # Validate we have data
    req(table_name, current_data)
    
    # Capture ML setting before later() callback (reactive value not accessible in later())
    use_ml <- input$use_ml_autofill
    
    # Close config modal and show progress modal
    removeModalSafe()
    
    # Set active flag to indicate autofill is running
    autofillState$active <- TRUE
    
    showModalSafe(modalDialog(
      title = "Processing Auto-Fill...",
      div(
        style = "text-align: center; padding: 30px;",
        tags$i(class = "fa fa-spinner fa-spin fa-4x", style = "color: #007bff;"),
        tags$h4(style = "margin-top: 25px; color: #333;", "Analyzing EPIC values..."),
        tags$p(
          style = "margin-top: 15px; color: #666; font-size: 14px;",
          if (use_ml) {
            "Applying 7 matching strategies including ML predictions"
          } else {
            "Applying 6 matching strategies"
          }
        ),
        div(
          style = "margin-top: 25px; padding: 15px; background: linear-gradient(90deg, #007bff 0%, #28a745 100%); height: 4px; border-radius: 2px; animation: pulse 1.5s ease-in-out infinite;",
          tags$style(HTML("
            @keyframes pulse {
              0%, 100% { opacity: 0.6; }
              50% { opacity: 1; }
            }
          "))
        ),
        if (use_ml) {
          tags$p(
            style = "margin-top: 20px; padding: 10px; background-color: #e7f3ff; border-left: 3px solid #007bff; border-radius: 3px; font-size: 13px; color: #004085;",
            tags$i(class = "fa fa-brain", style = "margin-right: 8px;"),
            tags$strong("ML Mode Active:"),
            " Using XGBoost model for intelligent predictions"
          )
        } else {
          tags$p(
            style = "margin-top: 20px; font-size: 12px; color: #999;",
            "Tip: Enable ML predictions in settings for smarter suggestions"
          )
        }
      ),
      footer = tagList(
        modalButton("Cancel")
      ),
      easyClose = FALSE
    ))
    
    # Run autofill in background
    later::later(function() {
      tryCatch({
        # Check if autofill is still active (modal not closed)
        if (!isTRUE(isolate(autofillState$active))) {
          return(invisible(NULL))  # User cancelled, stop processing
        }
        
        # Run autofill process with current data
        results <- process_autofill(
          table_name = table_name,
          tab_name = current_tab,
          min_confidence = 70,
          current_data = current_data,
          use_ml = use_ml  # Use captured value instead of reactive input
        )
        
        # Check again if still active after processing
        if (!isTRUE(isolate(autofillState$active))) {
          return(invisible(NULL))  # User cancelled during processing
        }
        
        # Store results
        autofillState$results <- results
        autofillState$original_data <- copy(current_data)
        autofillState$table_name <- table_name
        
        # Show results modal
        removeModalSafe()
        autofillState$active <- FALSE  # Reset flag
        
        if (nrow(results[confidence > 0]) == 0) {
          # [existing no results code remains unchanged]
          total_rows <- nrow(results)
          empty_castor <- sum(results$strategy == "Skipped", na.rm = TRUE)
          all_failed <- sum(results$confidence == 0 & results$strategy != "Skipped", na.rm = TRUE)
          
          if (total_rows == 0) {
            message <- "Alle EPIC values zijn al ingevuld. Er zijn geen lege waarden om te verwerken."
            icon <- "check-circle"
            icon_color <- "green"
          } else if (empty_castor == total_rows) {
            message <- sprintf("Alle %d lege EPIC values hebben geen Castor value. Auto-fill kan alleen werken met ingevulde Castor waarden.", total_rows)
            icon <- "exclamation-triangle"
            icon_color <- "orange"
          } else if (all_failed > 0) {
            message <- sprintf("Auto-fill heeft %d lege waarde(n) geanalyseerd, maar geen betrouwbare matches gevonden. Mogelijke oorzaken:\n• Onbekende medische termen\n• Geen internetverbinding voor API vertalingen\n• Incomplete optielijst in EPIC export", all_failed)
            icon <- "info-circle"
            icon_color <- "blue"
          } else {
            message <- "Geen auto-fill suggesties gevonden."
            icon <- "info-circle"
            icon_color <- "gray"
          }
          
          showModalSafe(modalDialog(
            title = "Geen Suggesties",
            div(
              style = "text-align: center; padding: 20px;",
              tags$i(class = sprintf("fa fa-%s fa-3x", icon), style = sprintf("color: %s;", icon_color)),
              p(style = "margin-top: 20px; text-align: left; white-space: pre-line;", message)
            ),
            footer = tagList(
              modalButton("Sluiten")
            ),
            easyClose = TRUE
          ))
        } else {
          show_autofill_preview_modal(results)
        }
        
      }, error = function(e) {
        # Reset active flag on error
        autofillState$active <- FALSE
        
        removeModalSafe()
        showModalSafe(modalDialog(
          title = "Error",
          p("An error occurred during auto-fill:"),
          p(style = "color: red;", conditionMessage(e)),
          easyClose = TRUE
        ))
      })
    }, delay = 0.1)
  })
  
  # Handle autofill button click - show config modal
  observeEvent(input$autofill_epic_values, {
    req(input$file, mappingData)
    
    table_name <- input$file
    
    # Validate table type
    if (!table_name %in% c("waarde_radiobuttons", "waarde_checkboxes")) {
      showModalSafe(modalDialog(
        title = "Error",
        "Auto-fill is only available for radiobuttons and checkboxes tables.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Capture current tab name and data
    current_tab <- isolate({
      if (!is.null(input$tab_name_meta) && input$tab_name_meta != "") {
        input$tab_name_meta
      } else {
        NULL
      }
    })
    
    current_data <- isolate({
      if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
        active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
        if (length(active_idx) > 0) {
          tabState$tabs[[active_idx]]$data
        } else {
          mappingData[[table_name]]
        }
      } else {
        mappingData[[table_name]]
      }
    })
    
    # Store data in autofillState for confirm handler
    autofillState$pending_table_name <- table_name
    autofillState$pending_tab_name <- current_tab
    autofillState$pending_data <- current_data
    
    # Load ML autofill settings
    ml_settings <- tryCatch({
      jsonlite::fromJSON("config/autofill_settings.json")
    }, error = function(e) {
      list(use_ml_autofill = TRUE, ml_confidence_threshold = 50)
    })
    
    # Show configuration modal first
    showModalSafe(modalDialog(
      title = "Auto-Fill Configuration",
      div(
        style = "padding: 20px;",
        h4("Machine Learning Predictions"),
        p("Enable experimental ML-based autofill? This uses trained XGBoost model to predict EPIC values."),
        checkboxInput("use_ml_autofill", 
                     "Use ML predictions (experimental)", 
                     value = ml_settings$use_ml_autofill),
        tags$small(style = "color: #666;", 
                  sprintf("Current model: %s classes trained, confidence threshold: %d%%",
                         if (file.exists("config/ml_models/autofill_metadata.rds")) {
                           meta <- readRDS("config/ml_models/autofill_metadata.rds")
                           length(meta$epic_levels)
                         } else { "No model" },
                         ml_settings$ml_confidence_threshold))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_autofill_config", "Continue", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  })
  
  # Function to show autofill preview modal
  show_autofill_preview_modal <- function(results) {
    successful <- results[confidence > 0]
    failed <- results[confidence == 0]
    
    # Calculate autofill statistics for summary display
    summary_stats <- generate_autofill_summary(successful)
    
    # Use autofill.r function to build HTML
    preview_html <- build_autofill_preview_html(successful)
    
    # Add failed count to summary if any
    summary_html <- preview_html$summary
    if (nrow(failed) > 0) {
      summary_html <- gsub('</ul>', 
                          sprintf('<li style="color: red;">No match found: %d</li></ul>', nrow(failed)),
                          summary_html)
    }
    
    # Build enhanced summary panel with visual statistics
    enhanced_summary <- sprintf(
      '<div style="background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;">
        <h4>📊 Autofill Statistics</h4>
        <div style="display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; margin-top: 10px;">
          <div style="background: white; padding: 10px; border-radius: 3px; border-left: 3px solid #28a745;">
            <div style="font-size: 24px; font-weight: bold; color: #28a745;">%d</div>
            <div style="font-size: 12px; color: #666;">Suggestions</div>
          </div>
          <div style="background: white; padding: 10px; border-radius: 3px; border-left: 3px solid #007bff;">
            <div style="font-size: 24px; font-weight: bold; color: #007bff;">%.1f%%</div>
            <div style="font-size: 12px; color: #666;">Avg. Confidence</div>
          </div>
          <div style="background: white; padding: 10px; border-radius: 3px; border-left: 3px solid #ffc107;">
            <div style="font-size: 24px; font-weight: bold; color: #ffc107;">%d</div>
            <div style="font-size: 12px; color: #666;">High Conf. (≥90%%)</div>
          </div>
        </div>
      </div>',
      summary_stats$total_suggestions,
      summary_stats$avg_confidence,
      summary_stats$high_confidence_count
    )
    
    showModalSafe(modalDialog(
      title = "Auto-Fill Preview - Review & Export",
      size = "l",
      div(
        HTML(enhanced_summary),
        HTML(summary_html),
        
        # Interactive filter controls for suggestion review
        div(
          style = "background-color: #e9ecef; padding: 10px; border-radius: 5px; margin: 15px 0;",
          h5("🔍 Filters"),
          fluidRow(
            column(4,
              selectInput("autofill_filter_strategy", "Strategy:",
                         choices = c("All" = "all", unique(successful$strategy)),
                         selected = "all", width = "100%")
            ),
            column(4,
              selectInput("autofill_filter_confidence", "Confidence:",
                         choices = c("All" = "all", "100%" = "100", "≥90%" = "90", "≥80%" = "80", "≥70%" = "70"),
                         selected = "all", width = "100%")
            ),
            column(4,
              selectInput("autofill_filter_element", "Element:",
                         choices = c("All" = "all", unique(successful$Element)),
                         selected = "all", width = "100%")
            )
          )
        ),
        
        h4("Suggestion Preview"),
        p("Check/uncheck items to include/exclude from auto-fill:"),
        div(
          style = "max-height: 400px; overflow-y: auto; overflow-x: auto; margin-top: 10px;",
          HTML(preview_html$table)
        )
      ),
      footer = tagList(
        # Modal footer with export options and action buttons
        div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding: 10px 0;",
          # Left side: Export buttons (subtle styling)
          div(style = "display: flex; gap: 8px;",
            downloadButton("export_autofill_csv", "Export CSV", 
                          class = "btn-sm btn-outline-secondary", 
                          style = "font-size: 13px;"),
            downloadButton("export_autofill_report", "Export Report", 
                          class = "btn-sm btn-outline-secondary",
                          style = "font-size: 13px;")
          ),
          # Right side: Primary actions
          div(style = "display: flex; gap: 10px;",
            modalButton("Cancel"),
            actionButton("apply_autofill", "Apply Selected", class = "btn-success")
          )
        )
      ),
      easyClose = FALSE
    ))
  }
  
  # Live filtering for autofill preview table based on user selections
  observeEvent(c(input$autofill_filter_strategy, input$autofill_filter_confidence, input$autofill_filter_element), {
    req(autofillState$results)
    
    # Get current filter values
    filter_strategy <- input$autofill_filter_strategy
    filter_confidence <- input$autofill_filter_confidence
    filter_element <- input$autofill_filter_element
    
    # Start with successful results
    filtered_results <- autofillState$results[confidence > 0]
    
    # Apply strategy filter
    if (!is.null(filter_strategy) && filter_strategy != "all") {
      filtered_results <- filtered_results[strategy == filter_strategy]
    }
    
    # Apply confidence filter
    if (!is.null(filter_confidence) && filter_confidence != "all") {
      threshold <- as.numeric(filter_confidence)
      filtered_results <- filtered_results[confidence >= threshold]
    }
    
    # Apply element filter
    if (!is.null(filter_element) && filter_element != "all") {
      filtered_results <- filtered_results[Element == filter_element]
    }
    
    # Generate new preview HTML with filtered results
    if (nrow(filtered_results) > 0) {
      preview_html <- build_autofill_preview_html(filtered_results)
      
      # Update the preview table in the modal (using JavaScript to replace content)
      shinyjs::runjs(sprintf("
        var previewDiv = document.querySelector('.modal-body > div > div:last-child');
        if (previewDiv) {
          previewDiv.innerHTML = %s;
        }
      ", jsonlite::toJSON(preview_html$table, auto_unbox = TRUE)))
    } else {
      # Show "no results" message
      shinyjs::runjs("
        var previewDiv = document.querySelector('.modal-body > div > div:last-child');
        if (previewDiv) {
          previewDiv.innerHTML = '<p style=\"text-align:center; color:#999; padding:20px;\">Geen suggesties voldoen aan de geselecteerde filters.</p>';
        }
      ")
    }
  }, ignoreInit = TRUE)
  
  # Apply autofill results
  observeEvent(input$apply_autofill, {
    req(autofillState$results, autofillState$table_name)
    
    table_name <- autofillState$table_name
    results <- autofillState$results
    successful <- results[confidence > 0]
    
    if (nrow(successful) == 0) {
      showModalSafe(modalDialog(
        title = "Error",
        "No changes to apply.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Capture all reactive values BEFORE later() to avoid reactive context errors
    selected_indices <- isolate(input$autofill_selected_rows)
    current_related_data <- isolate(relatedDataUpdated())
    active_tab_id <- isolate(tabState$activeTab)
    current_tabs <- isolate(tabState$tabs)
    current_file <- isolate(input$file)
    current_toggle <- isolate(input$toggle)
    
    # Capture active tab data before entering later() block
    captured_active_data <- isolate(get_active_tab_data())
    
    # Wait a moment for the selected rows input to be set
    later::later(function() {
      # Use captured selected_indices
      if (is.null(selected_indices) || length(selected_indices) == 0) {
        showModalSafe(modalDialog(
          title = "No Selection",
          "Please select at least one suggestion to apply.",
          easyClose = TRUE
        ))
        return()
      }
      
      # Convert to 1-indexed and filter successful results to only selected ones
      selected_rows <- successful[selected_indices + 1]
      
      # Apply changes to mappingData using apply_approved_matches function
      tryCatch({
        # Use captured active tab data (not mappingData directly for tab isolation)
        current_data <- captured_active_data
        if (is.null(current_data)) {
          showModalSafe(modalDialog(
            title = "Error",
            "Could not retrieve active tab data.",
            easyClose = TRUE
          ))
          return()
        }
        
        # Determine target column name (checkboxes/radiobuttons use kolom_toevoeging, others use castor_kolom)
        castor_col <- if (table_name %in% c("waarde_checkboxes", "waarde_radiobuttons")) {
          "kolom_toevoeging"
        } else {
          "kolom_toevoeging"  # default
        }
        
        # Use apply_approved_matches from export_approved.r
        updated_data <- apply_approved_matches(current_data, selected_rows, castor_col)
        
        applied_count <- nrow(selected_rows)
        
        # Update active tab data with changes (mappingData sync happens on save)
        # This maintains tab isolation - only the current tab's data is modified
        if (!is.null(active_tab_id) && length(current_tabs) > 0) {
          active_idx <- which(sapply(current_tabs, function(t) t$id == active_tab_id))
          if (length(active_idx) > 0) {
            # Update the tab's data in the captured list
            current_tabs[[active_idx]]$data <- updated_data
            # Write back to the reactive value
            tabState$tabs <- current_tabs
            
            # Trigger table re-render via reactive value
            autofillState$updated_data_to_render <- updated_data
          }
        }
        
        # Add approved API translations to medical dictionary
        dict_count <- add_approved_translations_to_dictionary(
          results = successful, 
          selected_indices = selected_indices
        )
        
        # Trigger table refresh (use captured value + 1)
        relatedDataUpdated(current_related_data + 1)
        
        # The tabState$tabs is now updated. Since render_table() is reactive to
        # get_active_tab_data() (which uses tabState), the table should automatically
        # update. If not, we can add a dedicated trigger.
        
        # Close modal and show success
        removeModalSafe()
        
        success_message <- sprintf("Successfully applied %d auto-fill suggestions!", applied_count)
        if (dict_count > 0) {
          success_message <- paste0(
            success_message, 
            sprintf("\n\n%d translation(s) have been added to the medical dictionary for future use.", dict_count)
          )
        }
        
        showModalSafe(modalDialog(
          title = "Success",
          p(success_message),
          p("The EPIC values have been updated. Don't forget to save your changes."),
          easyClose = TRUE
        ))
        
        # Clear autofill state
        autofillState$results <- NULL
        autofillState$original_data <- NULL
        autofillState$table_name <- NULL
        autofillState$active <- FALSE
        autofillState$pending_table_name <- NULL
        autofillState$pending_tab_name <- NULL
        autofillState$pending_data <- NULL
        
      }, error = function(e) {
        showModalSafe(modalDialog(
          title = "Error",
          p("An error occurred while applying changes:"),
          p(style = "color: red;", conditionMessage(e)),
          easyClose = TRUE
        ))
      })
    }, delay = 0.1)
  })
  
  # CSV export handler for autofill suggestions with current filters applied
  output$export_autofill_csv <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      sprintf("autofill_suggestions_%s.csv", timestamp)
    },
    content = function(file) {
      req(autofillState$results, autofillState$table_name)
      
      tryCatch({
        # Get current filter values to export only filtered results
        filter_strategy <- input$autofill_filter_strategy
        filter_confidence <- input$autofill_filter_confidence
        filter_element <- input$autofill_filter_element
        
        # Start with successful results
        export_data <- autofillState$results[confidence > 0]
        
        # Apply same filters as preview
        if (!is.null(filter_strategy) && filter_strategy != "all") {
          export_data <- export_data[strategy == filter_strategy]
        }
        if (!is.null(filter_confidence) && filter_confidence != "all") {
          threshold <- as.numeric(filter_confidence)
          export_data <- export_data[confidence >= threshold]
        }
        if (!is.null(filter_element) && filter_element != "all") {
          export_data <- export_data[Element == filter_element]
        }
        
        # Use export function from export_approved.r
        export_approved_matches(export_data, autofillState$table_name, file)
        
        # Show success notification
        showNotification(
          sprintf("Exported %d suggesties naar %s", nrow(export_data), basename(file)),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste("Export failed:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
      })
    }
  )
  
  # Summary report export handler with statistics and strategy breakdown
  output$export_autofill_report <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      sprintf("autofill_summary_%s.txt", timestamp)
    },
    content = function(file) {
      req(autofillState$results)
      
      tryCatch({
        # Get current filter values
        filter_strategy <- input$autofill_filter_strategy
        filter_confidence <- input$autofill_filter_confidence
        filter_element <- input$autofill_filter_element
        
        # Start with successful results
        export_data <- autofillState$results[confidence > 0]
        
        # Apply same filters as preview
        if (!is.null(filter_strategy) && filter_strategy != "all") {
          export_data <- export_data[strategy == filter_strategy]
        }
        if (!is.null(filter_confidence) && filter_confidence != "all") {
          threshold <- as.numeric(filter_confidence)
          export_data <- export_data[confidence >= threshold]
        }
        if (!is.null(filter_element) && filter_element != "all") {
          export_data <- export_data[Element == filter_element]
        }
        
        # Generate summary from filtered results
        summary_stats <- generate_autofill_summary(export_data)
        
        # Export summary report
        export_summary_report(summary_stats, file)
        
        # Show success notification
        showNotification(
          sprintf("Summary report exported naar %s", basename(file)),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste("Export failed:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
      })
    }
  )
  
  # ============================================================================
  # UPDATE CHECKBOX MAPPING
  # ============================================================================
  # Synchronizes checkbox mappings with current elements table structure.
  # For each element with a Castor checkbox field, ensures corresponding
  # checkbox value rows exist in waarde_checkboxes table.
  #
  # Tab-aware operation:
  # - When tabs exist: creates checkbox rows per tab, inheriting tab metadata
  # - Without tabs: operates on full dataset
  # - Preserves existing values when adding missing rows
  # - Removes orphaned rows (elements no longer exist)
  #
  # @return NULL (side effect: updates mappingData[["waarde_checkboxes"]])
  
  updateCheckboxMapping <- function() {
    req(mappingData[["elements"]], mappingData[["waarde_checkboxes"]])

    elements_all <- mappingData[["elements"]]
    checkbox_all <- mappingData[["waarde_checkboxes"]]
    
    # Check if elements table has tab structure
    if (!("tab_name_meta" %in% names(elements_all))) {
      # Simple mode: no tabs, use entire dataset
      elements_dt <- elements_all
      current_checkbox <- checkbox_all
    } else {
      # Tab-aware mode: process each tab separately
      
      # Ensure checkboxes table has tab metadata columns
      if (!("tab_name_meta" %in% names(checkbox_all))) {
        checkbox_all$tab_name_meta <- character(nrow(checkbox_all))
        checkbox_all$tab_order_meta <- integer(nrow(checkbox_all))
      }
      
      # Start with current checkbox data
      current_checkbox <- checkbox_all
      
      # Get unique tabs from elements table
      unique_tabs <- unique(elements_all[, .(tab_name = tab_name_meta, tab_order = tab_order_meta)])
      setorder(unique_tabs, tab_order)
      
      for (i in seq_len(nrow(unique_tabs))) {
        tab_name <- unique_tabs$tab_name[i]
        tab_order <- unique_tabs$tab_order[i]
        
        # Filter elements for this tab
        elements_dt <- elements_all[tab_name_meta == tab_name & tab_order_meta == tab_order]
        
        # Remove metadata columns for processing
        elements_dt_clean <- copy(elements_dt)
        elements_dt_clean[, c("tab_name_meta", "tab_order_meta") := NULL]
        
        # Filter existing checkboxes for this tab
        checkbox_for_tab <- current_checkbox[tab_name_meta == tab_name & tab_order_meta == tab_order]
        
        # Helper function: find matching checkbox metadata rows
        # Looks up checkbox options for a given Castor field name
        find_checkcols <- function(castor_name) {
          if (is.na(castor_name) || nchar(castor_name) == 0) return(integer(0))
          idx <- which(checkBoxesValues$kolom == castor_name)
          if (length(idx) > 0) return(idx)
          castor_suffix <- sub("^[^_]+_", "", castor_name)
          idx <- which(sub("^[^_]+_", "", checkBoxesValues$kolom) == castor_suffix)
          if (length(idx) > 0) return(idx)
          idx <- which(checkBoxesValues$kolom == paste0("", castor_name))
          return(idx)
        }
        
        # For each element in this tab, ensure all checkbox options exist
        for (j in seq_len(nrow(elements_dt_clean))) {
          el <- elements_dt_clean$Element[j]
          castor <- elements_dt_clean$castor_kolom[j]
          if (is.null(castor) || is.na(castor) || nchar(castor) == 0) next
          
          idxs <- find_checkcols(castor)
          if (length(idxs) == 0) next
          
          possible_values <- unique(checkBoxesValues$toevoeging[idxs])
          
          # Add missing checkbox rows for this element+tab combination
          for (val in possible_values) {
            # Check if row already exists in current tab
            existing_row_idx <- NULL
            if (nrow(checkbox_for_tab) > 0) {
              existing_row_idx <- which(!is.na(checkbox_for_tab$Element) & 
                                        !is.na(checkbox_for_tab$kolom_toevoeging) & 
                                        checkbox_for_tab$Element == el & 
                                        checkbox_for_tab$kolom_toevoeging == val)
            }
            
            row_exists <- length(existing_row_idx) > 0
            
            if (!row_exists) {
              # Check if this row exists in checkbox_all (for ALL tabs, not just this one)
              # If yes, preserve the existing value from the first match
              existing_waarde <- ""
              if (nrow(checkbox_all) > 0) {
                existing_idx <- which(!is.na(checkbox_all$Element) & 
                                      !is.na(checkbox_all$kolom_toevoeging) & 
                                      checkbox_all$Element == el & 
                                      checkbox_all$kolom_toevoeging == val)
                if (length(existing_idx) > 0) {
                  # Take the value from the first match (regardless of which tab)
                  existing_waarde <- checkbox_all$waarde[existing_idx[1]]
                  if (is.na(existing_waarde)) existing_waarde <- ""
                }
              }
              
              # Add new row with tab metadata
              new_row <- data.table(
                Element = el, 
                waarde = existing_waarde, 
                kolom_toevoeging = val,
                tab_name_meta = tab_name,
                tab_order_meta = tab_order
              )
              current_checkbox <- rbind(current_checkbox, new_row, fill = TRUE)
            }
          }
        }
      }
    }
    
    # Cleanup: remove orphaned checkbox rows
    # Removes rows where the parent Element no longer exists in elements table
    if (nrow(current_checkbox) > 0) {
      if ("tab_name_meta" %in% names(elements_all)) {
        # Tab-aware cleanup: match on Element + tab metadata
        # Use unique() to prevent cartesian product when duplicate Elements exist
        valid_elements <- unique(elements_all[!is.na(Element) & Element != "", 
                                       .(Element, tab_name_meta, tab_order_meta)])
        
        # Ensure column types match for merge operation
        if ("tab_name_meta" %in% names(current_checkbox)) {
          current_checkbox[, tab_name_meta := as.character(tab_name_meta)]
          current_checkbox[, tab_order_meta := as.integer(tab_order_meta)]
        }
        valid_elements[, tab_name_meta := as.character(tab_name_meta)]
        valid_elements[, tab_order_meta := as.integer(tab_order_meta)]
        
        # Filter checkboxes: keep only rows where Element+tab exists in elements
        current_checkbox <- merge(current_checkbox, valid_elements, 
                                 by = c("Element", "tab_name_meta", "tab_order_meta"), 
                                 all = FALSE)
      } else {
        # Simple cleanup: match on Element only
        valid_elements <- unique(elements_all[!is.na(Element) & Element != "", .(Element)])
        current_checkbox <- current_checkbox[Element %in% valid_elements$Element]
      }
    }
    
    # Sort checkbox rows to cluster them by Element
    # Preserve row order from elements table instead of alphabetical sorting
    if (nrow(current_checkbox) > 0 && nrow(elements_all) > 0) {
      # Add position index from elements table to preserve original order
      elements_all[, element_position := .I]
      
      if ("tab_name_meta" %in% names(current_checkbox) && "tab_name_meta" %in% names(elements_all)) {
        # Merge to get element position, matching on Element + tab metadata
        current_checkbox <- merge(
          current_checkbox,
          elements_all[, .(Element, tab_name_meta, tab_order_meta, element_position)],
          by = c("Element", "tab_name_meta", "tab_order_meta"),
          all.x = TRUE,
          sort = FALSE
        )
        # Sort by: element position (preserves elements table order), then option
        setorder(current_checkbox, element_position, kolom_toevoeging, na.last = TRUE)
      } else {
        # Merge to get element position (no tab metadata)
        current_checkbox <- merge(
          current_checkbox,
          unique(elements_all[, .(Element, element_position)]),
          by = "Element",
          all.x = TRUE,
          sort = FALSE
        )
        # Sort by: element position (preserves elements table order), then option
        setorder(current_checkbox, element_position, kolom_toevoeging, na.last = TRUE)
      }
      
      # Remove temporary position column
      current_checkbox[, element_position := NULL]
      elements_all[, element_position := NULL]
    }

    mappingData[["waarde_checkboxes"]] <<- current_checkbox
  }
  
  # ============================================================================
  # UPDATE RADIOBUTTON MAPPING
  # ============================================================================
  # Synchronizes radiobutton mappings with current elements table structure.
  # For each element with a Castor radiobutton field, ensures corresponding
  # radiobutton value rows exist in waarde_radiobuttons table.
  #
  # Tab-aware operation:
  # - When tabs exist: creates radiobutton rows per tab, inheriting tab metadata
  # - Without tabs: operates on full dataset
  # - Preserves existing values when adding missing rows
  # - Removes orphaned rows (elements no longer exist)
  #
  # @return NULL (side effect: updates mappingData[["waarde_radiobuttons"]])
  
  updateRadioMapping <- function() {
    req(mappingData[["elements"]], mappingData[["waarde_radiobuttons"]])
    
    elements_all <- mappingData[["elements"]]
    radio_all <- mappingData[["waarde_radiobuttons"]]
    
    # Check if elements table has tab structure
    if (!("tab_name_meta" %in% names(elements_all))) {
      # No tabs, use old logic
      elements_dt <- elements_all
      current_radio <- radio_all
    } else {
      # Check if radiobuttons table has tab metadata
      if (!("tab_name_meta" %in% names(radio_all))) {
        # No tab metadata, add empty columns
        radio_all$tab_name_meta <- character(nrow(radio_all))
        radio_all$tab_order_meta <- integer(nrow(radio_all))
      }
      
      # Start with current radiobutton data
      current_radio <- radio_all
      
      # Get unique tabs from elements table
      unique_tabs <- unique(elements_all[, .(tab_name = tab_name_meta, tab_order = tab_order_meta)])
      setorder(unique_tabs, tab_order)
      
      for (i in seq_len(nrow(unique_tabs))) {
        tab_name <- unique_tabs$tab_name[i]
        tab_order <- unique_tabs$tab_order[i]
        
        # Filter elements for this tab
        elements_dt <- elements_all[tab_name_meta == tab_name & tab_order_meta == tab_order]
        
        # Remove metadata columns for processing
        elements_dt_clean <- copy(elements_dt)
        elements_dt_clean[, c("tab_name_meta", "tab_order_meta") := NULL]
        
        # Filter existing radiobuttons for this tab
        radio_for_tab <- current_radio[tab_name_meta == tab_name & tab_order_meta == tab_order]
        
        # Helper function: find matching radiobutton metadata rows
        # Looks up radiobutton options for a given Castor field name
        find_radio_rows <- function(castor_name) {
          if (is.na(castor_name) || nchar(castor_name) == 0) return(integer(0))
          idx <- which(radioButtonOptionValues$`Field Variable Name` == castor_name)
          if (length(idx) > 0) return(idx)
          castor_suffix <- sub("^[^_]+_", "", castor_name)
          idx <- which(sub("^[^_]+_", "", radioButtonOptionValues$`Field Variable Name`) == castor_suffix)
          return(idx)
        }
        
        # For each element in this tab, ensure all radiobutton options exist
        for (j in seq_len(nrow(elements_dt_clean))) {
          el <- elements_dt_clean$Element[j]
          castor <- elements_dt_clean$castor_kolom[j]
          if (is.null(castor) || is.na(castor) || nchar(castor) == 0) next
          
          idxs <- find_radio_rows(castor)
          if (length(idxs) == 0) next
          
          possible_values <- unique(radioButtonOptionValues$`Option Name`[idxs])
          
          # Add missing radiobutton rows for this element+tab combination
          for (val in possible_values) {
            # Check if row already exists in current tab
            existing_row_idx <- NULL
            if (nrow(radio_for_tab) > 0) {
              existing_row_idx <- which(!is.na(radio_for_tab$Element) & 
                                        !is.na(radio_for_tab$castor_waarde) & 
                                        radio_for_tab$Element == el & 
                                        radio_for_tab$castor_waarde == val)
            }
            
            row_exists <- length(existing_row_idx) > 0
            
            if (!row_exists) {
              # Check if this row exists in radio_all (for ALL tabs, not just this one)
              # If yes, preserve the existing value from the first match
              existing_waarde <- ""
              if (nrow(radio_all) > 0) {
                existing_idx <- which(!is.na(radio_all$Element) & 
                                      !is.na(radio_all$castor_waarde) & 
                                      radio_all$Element == el & 
                                      radio_all$castor_waarde == val)
                if (length(existing_idx) > 0) {
                  # Take the value from the first match (regardless of which tab)
                  existing_waarde <- radio_all$waarde[existing_idx[1]]
                  if (is.na(existing_waarde)) existing_waarde <- ""
                }
              }
              
              # Add new row with tab metadata
              new_row <- data.table(
                Element = el, 
                waarde = existing_waarde, 
                castor_waarde = val,
                tab_name_meta = tab_name,
                tab_order_meta = tab_order
              )
              current_radio <- rbind(current_radio, new_row, fill = TRUE)
            }
          }
        }
      }
    }
    
    # Cleanup: remove orphaned radiobutton rows
    # Removes rows where the parent Element no longer exists in elements table
    if (nrow(current_radio) > 0) {
      if ("tab_name_meta" %in% names(elements_all)) {
        # Tab-aware cleanup: match on Element + tab metadata
        # Use unique() to prevent cartesian product when duplicate Elements exist
        valid_elements <- unique(elements_all[!is.na(Element) & Element != "", 
                                       .(Element, tab_name_meta, tab_order_meta)])
        
        # Ensure column types match for merge operation
        if ("tab_name_meta" %in% names(current_radio)) {
          current_radio[, tab_name_meta := as.character(tab_name_meta)]
          current_radio[, tab_order_meta := as.integer(tab_order_meta)]
        }
        valid_elements[, tab_name_meta := as.character(tab_name_meta)]
        valid_elements[, tab_order_meta := as.integer(tab_order_meta)]
        
        # Filter radiobuttons: keep only rows where Element+tab exists in elements
        current_radio <- merge(current_radio, valid_elements, 
                              by = c("Element", "tab_name_meta", "tab_order_meta"), 
                              all = FALSE)
      } else {
        # Simple cleanup: match on Element only
        valid_elements <- unique(elements_all[!is.na(Element) & Element != "", .(Element)])
        current_radio <- current_radio[Element %in% valid_elements$Element]
      }
    }
    
    # Sort radiobutton rows to cluster them by Element
    # Preserve row order from elements table instead of alphabetical sorting
    if (nrow(current_radio) > 0 && nrow(elements_all) > 0) {
      # Add position index from elements table to preserve original order
      elements_all[, element_position := .I]
      
      if ("tab_name_meta" %in% names(current_radio) && "tab_name_meta" %in% names(elements_all)) {
        # Merge to get element position, matching on Element + tab metadata
        current_radio <- merge(
          current_radio,
          elements_all[, .(Element, tab_name_meta, tab_order_meta, element_position)],
          by = c("Element", "tab_name_meta", "tab_order_meta"),
          all.x = TRUE,
          sort = FALSE
        )
        # Sort by: element position (preserves elements table order), then option
        setorder(current_radio, element_position, castor_waarde, na.last = TRUE)
      } else {
        # Merge to get element position (no tab metadata)
        current_radio <- merge(
          current_radio,
          unique(elements_all[, .(Element, element_position)]),
          by = "Element",
          all.x = TRUE,
          sort = FALSE
        )
        # Sort by: element position (preserves elements table order), then option
        setorder(current_radio, element_position, castor_waarde, na.last = TRUE)
      }
      
      # Remove temporary position column
      current_radio[, element_position := NULL]
      elements_all[, element_position := NULL]
    }

    mappingData[["waarde_radiobuttons"]] <<- current_radio
  }
  
  # ============================================================================
  # UPDATE RENDER MODE BADGE (STEP 5)
  # ============================================================================
  # Shows badge indicating current pagination strategy (virtual scrolling vs pagination)
  # Provides transparency about performance optimizations being applied
  #
  # @param row_count Number of rows in current dataset
  # @param use_scroller Boolean indicating if Scroller extension is active
  # @return NULL (side effect: shows/hides render mode badge)
  
  update_render_mode_badge <- function(row_count, use_scroller) {
    if (row_count < 100) {
      # Small dataset - no need to show badge
      shinyjs::hide("render_mode_badge")
      output$render_mode_content <- renderUI({ NULL })
      return()
    }
    
    if (use_scroller) {
      # Virtual scrolling icon
      icon_symbol <- "⚡"
      tooltip_text <- sprintf("Virtual Scrolling (%d rows)", row_count)
      badge_color <- if (row_count < 200) "#1976d2" else "#e65100"  # Blue or orange
    } else {
      # Pagination icon
      icon_symbol <- "📄"
      tooltip_text <- sprintf("Pagination Mode (%d rows)", row_count)
      badge_color <- "#6a1b9a"  # Purple
    }
    
    output$render_mode_content <- renderUI({
      tags$span(
        style = sprintf("color: %s; font-size: 20px; cursor: help;", badge_color),
        title = tooltip_text,
        icon_symbol
      )
    })
    
    shinyjs::show("render_mode_badge")
  }
  
  # ============================================================================
  # UPDATE ROW WARNING
  # ============================================================================
  # Shows warning icon when tab has too many rows (>200)
  # Large tabs can impact performance, especially with pagination mode (>500 rows)
  #
  # @param data Current tab's data.table
  # @return NULL (side effect: shows/hides warning icon)
  
  update_row_warning <- function(data) {
    if (is.null(data)) {
      shinyjs::hide("row_warning_icon")
      output$row_warning_content <- renderUI({ NULL })
      return()
    }
    
    row_count <- nrow(data)
    if (row_count > 200) {
      rows_to_remove <- row_count - 200
      tooltip_text <- sprintf(
        "This tab has %d row(s), consider removing %d row(s). We recommend not adding more than 200 rows to a tab for optimal performance.",
        row_count,
        rows_to_remove
      )
      
      output$row_warning_content <- renderUI({
        tags$span(
          class = "warning-hexagon",
          style = "display: inline-flex; align-items: center; justify-content: center; width: 36px; height: 36px; background-color: #ff9800; color: white; font-weight: bold; font-size: 24px; border-radius: 6px; cursor: help; box-shadow: 0 2px 4px rgba(0,0,0,0.2); border: none; padding: 0; margin: 0; z-index: 100;",
          title = tooltip_text,
          "!"
        )
      })
      
      shinyjs::show("row_warning_icon")
    } else {
      shinyjs::hide("row_warning_icon")
      output$row_warning_content <- renderUI({ NULL })
    }
  }
  
  # ============================================================================
  # ROW-LEVEL TABLE UPDATE (STEP 4 - Performance Optimization)
  # ============================================================================
  # Updates specific rows in the table without replacing the entire dataset
  # This is significantly faster than replaceData() for single-row updates
  # Use cases: cell edits, row validation updates, single-row deletions
  #
  # @param row_indices Integer vector of row indices to update (1-based)
  # @param file Table name for context-aware processing
  # @return NULL (side effect: sends custom message to update table rows)
  
  update_table_rows <- function(row_indices, file) {
    if (!isTRUE(table_initialized()) || is.null(row_indices) || length(row_indices) == 0) {
      return()
    }
    
    # Get current active tab data
    active_data <- get_active_tab_data()
    if (is.null(active_data)) {
      return()
    }
    
    # Validate row indices
    row_indices <- row_indices[row_indices > 0 & row_indices <= nrow(active_data)]
    if (length(row_indices) == 0) {
      return()
    }
    
    # Extract only the rows that need updating
    rows_to_update <- active_data[row_indices]
    
    # Process the rows (same logic as render_table but only for selected rows)
    # Remove metadata columns
    data_for_display <- copy(rows_to_update)
    meta_cols <- names(data_for_display)[grepl("tab_(name|order)_meta", names(data_for_display))]
    if (length(meta_cols) > 0) {
      data_for_display[, (meta_cols) := NULL]
    }
    
    # Add Index column (using actual row indices from full table)
    data_for_display[, Index := row_indices]
    
    # Add Select checkboxes
    data_for_display$Select <- paste0('<input type="checkbox" class="delete-rows" id="deleterows_', 
                                       row_indices, '">')
    
    # Apply column renaming (same as render_table)
    if(file == "elements") {
      setnames(data_for_display, "castor_kolom", "Castor Name", skip_absent = TRUE)
    }
    
    col_renames <- c(
      "epic_tabel" = "Export File",
      "epic_kolom" = "EPIC Column",
      "castor_tabel" = "Main Output File",
      "castor_kolom" = "Castor Metadata",
      "vaste_waarde" = "Fixed Value",
      "key" = "Key",
      "repeating" = "Repeating",
      "gerelateerde_mapping" = "Related Mapping"
    )
    
    for (old_name in names(col_renames)) {
      if (old_name %in% names(data_for_display)) {
        setnames(data_for_display, old_name, col_renames[[old_name]], skip_absent = TRUE)
      }
    }
    
    # Send row update message to JavaScript
    # JavaScript will update only these specific rows in the DataTable
    session$sendCustomMessage("updateTableRows", list(
      rows = row_indices - 1,  # Convert to 0-based for JavaScript
      data = as.list(data_for_display)
    ))
  }
  
  # ============================================================================
  # TABLE RENDERING: Main function for displaying interactive DataTable
  # ============================================================================
  # This is the core rendering function for the application's main table.
  # It handles:
  #   - Data transformation and display (hiding metadata columns)
  #   - Column renaming for user-friendly display
  #   - Dropdown generation for option columns (with lazy loading via Select2)
  #   - Tab metadata integration for checkbox/radiobutton tables
  #   - Cell coloring based on validation state (red=invalid, blue=manual)
  #   - Delete checkboxes for row selection
  #   - Full rendering (initial load) vs proxy updates (data changes)
  #
  # PERFORMANCE OPTIMIZATION (Step 4):
  # Three rendering strategies for different use cases:
  #   1. "full" mode: Complete table rebuild (slowest, ~1-2s for 100 rows)
  #      Use when: Table changed, columns changed, or first render
  #   
  #   2. "proxy" mode: Replace all data via replaceData (medium, ~200-500ms)
  #      Use when: Multiple rows changed, tab switched, large data changes
  #   
  #   3. update_table_rows(): Update specific rows only (fastest, ~50-100ms)
  #      Use when: Single cell edit, validation update for one row
  #      NOTE: Not yet implemented in all observers, available for future optimization
  #
  # @param data data.table to display
  # @param file Table name ("elements", "waarde_checkboxes", "waarde_radiobuttons", etc.)
  # @param mode Rendering mode: "auto" (auto-detect), "full" (complete re-render), "proxy" (update existing)
  # @return NULL (side effect: updates output$table)
  render_table <- function(data, file, mode = c("auto", "full", "proxy")) {
    mode <- match.arg(mode)
    
    # Auto-detect rendering mode based on table state
    if (identical(mode, "auto")) {
      if (!isTRUE(table_initialized()) || !identical(current_table_name(), file)) {
        mode <- "full"  # First render or table changed
      } else {
        mode <- "proxy"  # Just update data
      }
    } else if (identical(mode, "proxy") && !isTRUE(table_initialized())) {
      mode <- "full"  # Can't use proxy if table not initialized
    }
    
    # Detect duplicates for elements table and update reactive value
    # Only check for duplicates in 'elements' table - duplicates are normal in radiobuttons/checkboxes
    if (file == "elements" && !is.null(data)) {
      dupes <- detect_duplicate_elements(data)
      duplicateElements(dupes)
    } else {
      # Clear duplicates for other tables
      duplicateElements(list(
        duplicate_rows_element = integer(0),
        duplicate_values_element = character(0),
        duplicate_rows_castor = integer(0),
        duplicate_values_castor = character(0)
      ))
    }
    
    # Check if input data has tab metadata (before we remove it)
    has_input_tab_meta <- "tab_name_meta" %in% names(data)
    
    # Remove metadata columns from display (tab_name_meta, tab_order_meta)
    data_for_display <- copy(data)
    meta_cols <- names(data_for_display)[grepl("tab_(name|order)_meta", names(data_for_display))]
    if (length(meta_cols) > 0) {
      data_for_display[, (meta_cols) := NULL]
    }
    
    # Add Index and Select columns
    data_with_delete <- cbind(Index = seq_len(nrow(data_for_display)), data_for_display)
    
    # Add delete checkboxes
    data_with_delete$Select <- paste0('<input type="checkbox" class="delete-rows" id="deleterows_', 
                                        seq_len(nrow(data_with_delete)), '">')
    
    display_data <- copy(data_with_delete)
    
    # ========================================================================
    # COLUMN RENAMING: User-friendly display names
    # ========================================================================
    
    # Special rename for 'elements' table
    if(file == "elements") {
      setnames(display_data, "castor_kolom", "Castor Name", skip_absent = TRUE)
    }
    
    # Standard column renames (Dutch → English)
    col_renames <- c(
      "epic_tabel" = "Export File",
      "epic_kolom" = "EPIC Column",
      "castor_tabel" = "Main Output File",
      "castor_kolom" = "Castor Metadata",
      "vaste_waarde" = "Fixed Value",
      "key" = "Key",
      "repeating" = "Repeating",
      "gerelateerde_mapping" = "Related Mapping"
    )
    
    for (old_name in names(col_renames)) {
      if (old_name %in% names(display_data)) {
        setnames(display_data, old_name, col_renames[[old_name]], skip_absent = TRUE)
      }
    }
    
    # ========================================================================
    # TAB-AWARE MERGE: For checkbox/radiobutton tables
    # ========================================================================
    # Checkbox and radiobutton tables need to show the castor_kolom_naam
    # for their associated Element. This requires a tab-aware merge when
    # tab metadata is present (to get the correct castor_kolom per tab).
    
    if(file %in% c("waarde_checkboxes", "waarde_radiobuttons")) {
      # Get elements data for merge
      elements_df <- as.data.table(copy(mappingData[["elements"]]))
      
      # Check if elements has tab metadata
      has_tab_meta <- "tab_name_meta" %in% names(elements_df)
      
      # Rename castor_kolom for merge (avoid conflict)
      setnames(elements_df, "castor_kolom", "castor_kolom_naam")
      elements_df <- elements_df[!is.na(Element) & Element != ""]
      
      # Use the check we did on original data earlier
      if (has_tab_meta && has_input_tab_meta) {
        # Tab-aware merge: match on Element AND tab metadata
        # This ensures we get the correct castor_kolom for each element PER tab
        
        # For tab-aware merge we need to use original data (with metadata)
        # Create temporary dataset with Index, Select and metadata
        temp_merge_data <- copy(data)
        temp_merge_data[, Index := seq_len(.N)]
        temp_merge_data[, Select := paste0('<input type="checkbox" class="delete-rows" id="deleterows_', seq_len(.N), '">')]
        
        # Preserve original order
        temp_merge_data[, original_order := .I]
        
        # Ensure tab_order_meta has same type in both datasets
        if ("tab_order_meta" %in% names(temp_merge_data)) {
          temp_merge_data[, tab_order_meta := as.integer(tab_order_meta)]
        }
        if ("tab_order_meta" %in% names(elements_df)) {
          elements_df[, tab_order_meta := as.integer(tab_order_meta)]
        }
        
        # Merge with tab metadata (Element + tab_name_meta + tab_order_meta)
        temp_merge_data <- merge(temp_merge_data, elements_df, 
                                by = c("Element", "tab_name_meta", "tab_order_meta"), 
                                all.x = TRUE, sort = FALSE)
        
        # Restore original order
        setorder(temp_merge_data, original_order)
        temp_merge_data[, original_order := NULL]
        
        # Remove metadata columns for display
        meta_cols <- names(temp_merge_data)[grepl("tab_(name|order)_meta", names(temp_merge_data))]
        if (length(meta_cols) > 0) {
          temp_merge_data[, (meta_cols) := NULL]
        }
        
        # Use this as display_data
        display_data <- temp_merge_data
      } else {
        # No tab metadata in one or both datasets: use Element only for merge
        # This happens when data hasn't been updated with tab metadata yet
        elements_df_unique <- copy(elements_df)
        
        # Remove tab metadata from elements if it exists
        if (has_tab_meta) {
          elements_df_unique[, c("tab_name_meta", "tab_order_meta") := NULL]
        }
        
        # Make unique by Element
        elements_df_unique <- unique(elements_df_unique, by = "Element")
        
        # Merge only on Element (display_data already has no metadata)
        display_data <- merge(display_data, elements_df_unique, 
                             by = "Element", all.x = TRUE, sort = FALSE)
        
        # Ensure any metadata columns are removed here as well
        meta_cols <- names(display_data)[grepl("tab_(name|order)_meta", names(display_data))]
        if (length(meta_cols) > 0) {
          display_data[, (meta_cols) := NULL]
        }
      }
      
      # Rename columns for display
      setnames(display_data, "castor_kolom_naam", "Castor Name", skip_absent = TRUE)
      setnames(display_data, "waarde", "EPIC Value", skip_absent = TRUE)
      
      if(file == "waarde_radiobuttons") {
        setnames(display_data, "castor_waarde", "Castor Value", skip_absent = TRUE)
      } else if(file == "waarde_checkboxes") {
        setnames(display_data, "kolom_toevoeging", "Castor Value", skip_absent = TRUE)
      }
      
      # Reorder columns: Index, Element, Castor Name, other columns, Select
      cols <- names(display_data)
      other_cols <- setdiff(cols, c("Index", "Element", "Castor Name", "Select"))
      display_data <- display_data[, c("Index", "Element", "Castor Name", other_cols, "Select"), with = FALSE]
    } else {
      # For other tables: Index, other columns, Select
      cols <- names(display_data)
      other_cols <- setdiff(cols, c("Index", "Select"))
      display_data <- display_data[, c("Index", other_cols, "Select"), with = FALSE]
    }
    
    # ========================================================================
    # DROPDOWN GENERATION: Select option lists for editable columns
    # ========================================================================
    # Filter option_data to get only columns for current table
    option_data_selected <- option_data[grepl(file, names(option_data))]
    
    # Map original column names to display names for lookup
    column_mapping <- list(
      "castor_kolom" = "Castor Name",
      "castor_kolom_naam" = "Castor Name",
      "waarde" = "EPIC Value",
      "castor_waarde" = "Castor Value",
      "kolom_toevoeging" = "Castor Value",
      
      "epic_tabel" = "Export File",
      "epic_kolom" = "EPIC Column",
      "castor_tabel" = "Main Output File",
      "castor_kolom" = "Castor Metadata",
      "vaste_waarde" = "Fixed Value",
      "key" = "Key",
      "repeating" = "Repeating",
      "gerelateerde_mapping" = "Related Mapping"
    )
    
    # For checkbox/radiobutton tables: don't show dropdown for Element column
    # (Element values come from merge, shouldn't be editable)
    if (file %in% c("waarde_checkboxes", "waarde_radiobuttons")) {
      option_data_selected <- option_data_selected[
        !grepl(paste0(file, "\\|Element"), names(option_data_selected))
      ]
    }
    
    # Remove specific non-editable columns from dropdown generation
    if (file == "waarde_checkboxes") {
      option_data_selected <- option_data_selected[
        !grepl("waarde_checkboxes\\|kolom_toevoeging", names(option_data_selected))
      ]
    }
    if (file == "waarde_radiobuttons") {
      option_data_selected <- option_data_selected[
        !grepl("waarde_radiobuttons\\|castor_waarde", names(option_data_selected))
      ]
    }
    
    # Sort Element options numerically (format: "text #123")
    if ("elements|Element" %in% names(option_data_selected)) {
      option_data_selected[["elements|Element"]]$options <-
        option_data_selected[["elements|Element"]]$options[
          order(as.numeric(sub(".*#", "", option_data_selected[["elements|Element"]]$options)))
        ]
    }
    
    # ========================================================================
    # DROPDOWN HTML GENERATION: Create Select2 dropdowns for each cell
    # ========================================================================
    # For each column with options, generate HTML select elements with:
    #   - Lazy loading (Select2 initialized on demand)
    #   - Context-aware filtering (based on toggle and match columns)
    #   - Cell coloring (red=invalid, blue=manual entry)
    #   - "Add value..." option for manual input
    
    for (i in seq_along(option_data_selected)) {
      parts <- strsplit(names(option_data_selected)[[i]], "|", fixed = TRUE)[[1]]
      if (length(parts) < 2) next
      table <- parts[1]
      column <- parts[2]
      if (column == "castor_kolom_naam") next  # Skip derived column
      
      original_column <- column
      
      # Use display column name when available
      display_column <- if (column %in% names(column_mapping)) column_mapping[[column]] else column
      
      optionsList <- option_data_selected[[i]]$options

      # Check for match column (for context-aware filtering)
      if (!is.null(option_data_selected[[i]]$match_col) && isTRUE(input$toggle)) {
        matchList <- option_data_selected[[i]]$match
        matchCol <- option_data_selected[[i]]$match_col
      }
      
      # Filter Element options when toggle is on (checkbox/radiobutton context)
      if (isTRUE(input$toggle)) {
        if (table == "waarde_checkboxes" && column == "Element") {
          # Only show Elements that have checkbox-type castor_kolom
          optionsList <- mappingData[["elements"]][["Element"]][mappingData[["elements"]][["castor_kolom"]] %in% checkboxes]
        }
        if (table == "waarde_radiobuttons" && column == "Element") {
          # Only show Elements that have radiobutton-type castor_kolom
          optionsList <- mappingData[["elements"]][["Element"]][mappingData[["elements"]][["castor_kolom"]] %in% radiobuttons]
        }
      }
      
      # Generate dropdown HTML for each cell in this column
      if (any(colnames(display_data) == display_column)) {
        display_data[[display_column]] <- sapply(seq_len(nrow(display_data)), function(valueNumber) {
          # Use original data for the value
          value <- data_with_delete[[original_column]][valueNumber]
          rowN <- valueNumber

          # Always narrow options for "waarde" column in checkbox/radiobutton tables
          always_narrow <- file %in% c("waarde_checkboxes","waarde_radiobuttons") && column=="waarde"

          # Apply context-aware filtering if match column exists
          if (!is.null(option_data_selected[[i]]$match_col)
            && ((isTRUE(input$toggle)) || always_narrow)
                  && (option_data_selected[[i]]$match_col %in% names(data_with_delete))) {
            matchCol <- option_data_selected[[i]]$match_col
            matchList <- option_data_selected[[i]]$match
            valueMatch <- data_with_delete[[ matchCol ]][valueNumber]
            # Filter options to those matching the context
            optionsListFiltered <- optionsList[matchList == valueMatch]
          } else {
            optionsListFiltered <- optionsList
          }
          optionsListFiltered <- optionsListFiltered[!is.na(optionsListFiltered)]

          # Always include the current cell value in the dropdown
          optionsListFiltered <- unique(c(value, optionsListFiltered))

          # Compute CSS class based on whether value exists in full option list
          key <- paste0(valueNumber, "_", column)
          
          # Check if this is a duplicate (Element or Castor Name column)
          is_duplicate <- FALSE
          if (file == "elements") {
            dupes <- duplicateElements()
            if (column == "Element") {
              is_duplicate <- valueNumber %in% dupes$duplicate_rows_element
            } else if (column == "castor_kolom") {
              is_duplicate <- valueNumber %in% dupes$duplicate_rows_castor
            }
          }
          
          if (is_duplicate) {
            css_class <- "orange-cell"  # Duplicate element or Castor Name
          } else if (!(value %in% optionsList)) {
            # Value not in standard options
            css_class <- if (!is.null(manualValues$vals[[key]]) && manualValues$vals[[key]] == TRUE) {
              "blue-cell"  # Manually added value
            } else if (nzchar(value)) {
              "red-cell"   # Invalid value
            } else {
              ""           # Empty cell
            }
          } else {
            css_class <- ""  # Valid value
          }
          
          # Encode options as JSON for lazy loading
          options_json <- jsonlite::toJSON(optionsListFiltered, auto_unbox = TRUE)
          
          # Escape JSON for HTML attribute (prevent XSS and parsing issues)
          options_json_escaped <- gsub("'", "&#39;", options_json, fixed = TRUE)
          options_json_escaped <- gsub('"', "&quot;", options_json_escaped, fixed = TRUE)
          
          # Build lightweight placeholder div that will become dropdown on focus
          # This dramatically reduces initial render time by deferring dropdown creation
          # The actual <select> element is generated by JavaScript when user clicks the cell
          dropdown_html <- paste0(
              '<div class="lazy-dropdown ', css_class, '" ',
              'id="dropdown_', original_column, '_', rowN, '" ',
              'data-row="', rowN, '" ',
              'data-col="', original_column, '" ',
              'data-value="', htmltools::htmlEscape(value), '" ',
              'data-options="', options_json_escaped, '" ',
              'data-cssclass="', css_class, '" ',
              'data-original-col="', original_column, '" ',
              'tabindex="0">',
              htmltools::htmlEscape(value),
              '</div>'
          )
        }, simplify = FALSE)
      }
    }
  
    # ========================================================================
    # DATATABLE INITIALIZATION / UPDATE
    # ========================================================================
    # Two rendering modes:
    #   - "full": Complete table rebuild with DataTable initialization
    #   - "proxy": Just replace data in existing table (faster for updates)
    
    if (identical(mode, "full") || !isTRUE(table_initialized())) {
      # ======================================================================
      # STEP 5: INTELLIGENT PAGINATION STRATEGY
      # ======================================================================
      # Automatically choose optimal rendering strategy based on row count:
      #   < 200 rows:   Virtual scrolling (Scroller) - best UX, smooth scrolling
      #   200-500 rows: Virtual scrolling with performance warning
      #   > 500 rows:   Traditional pagination - better performance for large datasets
      
      row_count <- nrow(display_data)
      use_scroller <- row_count < 500  # Threshold for switching to pagination
      
      # Configure extensions and options based on row count
      if (use_scroller) {
        # Virtual scrolling configuration (< 500 rows)
        extensions_list <- 'Scroller'
        dt_options <- list(
          deferRender = TRUE,        # Lazy render for performance
          autoWidth = FALSE,
          scroller = TRUE,           # Enable virtual scrolling
          scrollY = 600,             # Fixed viewport height
          scrollCollapse = TRUE,
          paging = TRUE,             # Required by Scroller
          pageLength = 50,           # Buffer size for Scroller
          displayLength = 50,
          searching = TRUE,
          ordering = FALSE,
          dom = 'rtip',              # Hide length menu and pagination controls
          info = TRUE                # Show "Showing X of Y entries"
        )
      } else {
        # Traditional pagination configuration (>= 500 rows)
        extensions_list <- character(0)  # No extensions needed
        dt_options <- list(
          deferRender = FALSE,       # Full render (pagination handles performance)
          autoWidth = FALSE,
          scroller = FALSE,          # Disable virtual scrolling
          scrollY = FALSE,           # No fixed height, use pagination
          paging = TRUE,             # Enable traditional pagination
          pageLength = 25,           # Show 25 rows per page
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
          searching = TRUE,
          ordering = FALSE,
          dom = 'lrtip',             # Show length menu + pagination
          info = TRUE
        )
      }
      
      # ======================================================================
      # STEP 6: ROW CALLBACK FOR DUPLICATE HIGHLIGHTING
      # ======================================================================
      # Add rowCallback to apply .duplicate-row class to rows with duplicates
      # This provides visual feedback for data quality issues
      
      rowCallback_js <- NULL
      if (file == "elements") {
        dupes <- duplicateElements()
        duplicate_row_indices <- unique(c(dupes$duplicate_rows_element, dupes$duplicate_rows_castor))
        
        if (length(duplicate_row_indices) > 0) {
          # JavaScript row indices are 0-based, R indices are 1-based
          duplicate_indices_js <- duplicate_row_indices - 1
          
          rowCallback_js <- JS(
            sprintf(
              "function(row, data, index) {
                var duplicateIndices = [%s];
                if (duplicateIndices.includes(index)) {
                  $(row).addClass('duplicate-row');
                } else {
                  $(row).removeClass('duplicate-row');
                }
              }",
              paste(duplicate_indices_js, collapse = ",")
            )
          )
        }
      }
      
      # Full render: Create new DataTable with adaptive configuration
      output$table <- renderDT({
        datatable(
          display_data,
          rownames = FALSE,
          escape = FALSE,  # Allow HTML in cells (for dropdowns)
          editable = list(target = 'cell', disable = list(
            # Disable editing for columns with dropdowns (last column is Select)
            columns = unname(c(
              sapply(names(option_data_selected), function(x) { 
                col_name <- strsplit(x, "|", fixed = TRUE)[[1]][2]
                # Map to the display column name when present
                if (col_name %in% names(column_mapping)) column_mapping[[col_name]] else col_name
              }),
              ncol(display_data)  # Disable Select column
            ))
          )),
          selection = "none",  # Disable row selection (we use custom checkboxes)
          extensions = extensions_list,  # Adaptive: Scroller or none
          options = c(dt_options, list(  # Merge adaptive options with callbacks
            rowCallback = rowCallback_js,  # Apply duplicate row highlighting
            initComplete = JS(
              # Initialize Select2 on all dropdowns after table loads
              "function(settings, json) {",
              "  var initStart = performance.now();",
              "  // columns.adjust() is handled by resizeTableToWindow in hideLoadingScreen",
              "  var selects = $('#table table select.lazy-load');",
              "  selects.each(function(){",
              "     var $sel = $(this);",
              "     if (!$sel.hasClass('select2-hidden-accessible')) {",
              "         $sel.select2({",
              "           width: '100%',",
              "           dropdownParent: $('#scrollDiv')",
              "         });",
              "     }",
              "  });",
              "  var initDuration = performance.now() - initStart;",
              "  var rowCount = this.api().rows().count();",
              "  if (window.performanceMonitor) {",
              "    performanceMonitor.trackTableRender('main_table', rowCount, initDuration);",
              "  }",
              "}"
            ),
            drawCallback = JS(
              # Reinitialize Select2 after table redraws (e.g., after search)
              "function(settings, json) {",
              "  $(document).trigger('initializeSelect2');",
              "}"
            )
          ))
        )
      })
      
      # Show notification about rendering mode for large datasets
      if (row_count >= 200 && row_count < 500) {
        showNotification(
          sprintf("Large dataset detected (%d rows). Using virtual scrolling. Performance may vary.", row_count),
          type = "message",
          duration = 3
        )
      } else if (row_count >= 500) {
        showNotification(
          sprintf("Very large dataset (%d rows). Switched to pagination mode for optimal performance.", row_count),
          type = "message",
          duration = 5
        )
      }
      
      # Update render mode badge
      update_render_mode_badge(row_count, use_scroller)
      
      table_initialized(TRUE)
    } else {
      # Proxy mode: Just replace data in existing table
      replaceData(
        proxy,
        as.data.frame(display_data, stringsAsFactors = FALSE),
        resetPaging = FALSE,
        clearSelection = "none",
        rownames = FALSE
      )
      
      # Update duplicate row highlighting after proxy update
      if (file == "elements") {
        dupes <- duplicateElements()
        duplicate_row_indices <- unique(c(dupes$duplicate_rows_element, dupes$duplicate_rows_castor))
        
        # Convert to 0-based indices for JavaScript
        duplicate_indices_js <- if (length(duplicate_row_indices) > 0) {
          duplicate_row_indices - 1
        } else {
          integer(0)
        }
        
        # Send custom message to update duplicate classes
        session$sendCustomMessage("updateDuplicateRows", list(
          duplicateIndices = duplicate_indices_js
        ))
      }
      
      # Update render mode badge in proxy mode too (for tab switches)
      row_count <- nrow(display_data)
      use_scroller <- row_count < 500
      update_render_mode_badge(row_count, use_scroller)
    }
    current_table_name(file)
    
    # Notify JavaScript to reload Select2 and restore scroll position
    session$sendCustomMessage("reloadSelect2", list())
    session$sendCustomMessage("restoreScroll", list())
    
    # Update row warning icon (shows if there are validation errors)
    update_row_warning(data)
  }
  
  # Create proxy for table updates (used in proxy mode)
  proxy <- DT::dataTableProxy("table")
  
  # ============================================================================
  # TABLE INTERACTION OBSERVERS
  # ============================================================================
  
  # Toggle button: Switch between filtered/unfiltered dropdown options
  observeEvent(input$toggle, {
    req(is_selectable_table(input$file))
    render_table(mappingData[[input$file]], input$file)
  })
  
  # Delete rows: Remove selected rows from active tab
  observeEvent(input$delete_rows, {
    req(is_selectable_table(input$file))
    req(!is.null(input$table_rows_selected))
    req(length(input$table_rows_selected) > 0)
    
    selected_ids <- as.numeric(input$table_rows_selected)
    
    # Remove rows from active tab
    new_data <- get_active_tab_data()
    if (!is.null(new_data) && nrow(new_data) > 0) {
      new_data <- new_data[-selected_ids, ]
      
      # Update active tab
      if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
        active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
        if (length(active_idx) > 0) {
          tabState$tabs[[active_idx]]$data <- new_data
        }
      }
      
      # Detect duplicates if this is the elements table
      if (input$file == "elements") {
        dupes <- detect_duplicate_elements(new_data)
        duplicateElements(dupes)
      }
      
      render_table(new_data, input$file)
    }
  })
  
  # Add row button: Show modal to specify number of rows to add
  observeEvent(input$add_row, {
    req(is_selectable_table(input$file))
    showModalSafe(modalDialog(
      title = "Add rows",
      tagList(
        numericInput("rows_to_add", "How many rows do you want to add?", value = 1, min = 1, step = 1, width = '200px'),
        tags$div(id = 'rows_add_error_container', uiOutput('rows_add_error'))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_rows", "Add", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  # Server-side validation for rows_to_add input
  # Only accept positive integers
  observe({
    if (is.null(input$rows_to_add)) {
      output$rows_add_error <- renderUI({ NULL })
      shinyjs::disable('confirm_add_rows')
      return()
    }

    val <- input$rows_to_add
    # Accept only integers >= 1
    ok <- is.numeric(val) || (is.character(val) && grepl('^[0-9]+$', val))
    if (ok) {
      intval <- as.integer(val)
      ok <- !is.na(intval) && intval >= 1
    }

    if (!ok) {
      output$rows_add_error <- renderUI({
        tags$div(style = 'color: red; margin-top: 8px;', 'Only numerical values are allowed!')
      })
      shinyjs::disable('confirm_add_rows')
    } else {
      output$rows_add_error <- renderUI({ NULL })
      shinyjs::enable('confirm_add_rows')
    }
  })
  
  # ============================================================================
  # COPY/CUT/PASTE BUTTON ENABLE/DISABLE LOGIC
  # ============================================================================
  # These observers dynamically enable/disable the copy/cut/paste/bulk move
  # buttons based on current selection and table state.
  
  # Enable/Disable Copy and Cut buttons based on selection
  observe({
    # Copy/Cut is only available for elements table
    # Check if rows are selected AND if it's the elements table
    has_selection <- !is.null(input$table_rows_selected) && 
                     length(input$table_rows_selected) > 0
    
    is_valid_table <- !is.null(input$file) && is_copyable_table(input$file)
    
    if (has_selection && is_valid_table) {
      shinyjs::enable("copy_rows")
      shinyjs::enable("cut_rows")
    } else {
      shinyjs::disable("copy_rows")
      shinyjs::disable("cut_rows")
    }
  })
  
  # Enable/Disable Paste button based on clipboard status
  observe({
    # Paste is only available when:
    # 1. Clipboard is not empty
    # 2. Current table matches source table (elements)
    # 3. It's the elements table
    
    clipboard_has_data <- !is.null(clipboardState$data) && 
                          nrow(clipboardState$data) > 0
    
    tables_match <- !is.null(input$file) && 
                    !is.null(clipboardState$source_table) &&
                    input$file == clipboardState$source_table
    
    is_valid_table <- !is.null(input$file) && is_copyable_table(input$file)
    
    if (clipboard_has_data && tables_match && is_valid_table) {
      shinyjs::enable("paste_rows")
    } else {
      shinyjs::disable("paste_rows")
    }
  })
  
  # Enable/Disable Bulk Move button based on selection and table
  observe({
    # Bulk Move is only available when:
    # 1. Rows are selected
    # 2. It's the elements table
    
    # Force dependencies - ensure observer reacts to these inputs
    req(input$file)  # Wait until file input is available
    
    has_selection <- !is.null(input$table_rows_selected) && 
                     length(input$table_rows_selected) > 0
    
    is_valid_table <- !is.null(input$file) && is_copyable_table(input$file)
    
    # Determine tooltip message based on current state
    # Order is important: check table first, then selection
    if (!is_valid_table) {
      # Wrong table (or no table)
      shinyjs::disable("move_rows_bulk")
      tooltip_message <- "Only available in Elements table"
    } else if (!has_selection) {
      # Correct table but no selection
      shinyjs::disable("move_rows_bulk")
      tooltip_message <- "No rows selected"
    } else {
      # Everything is OK - enabled state
      shinyjs::enable("move_rows_bulk")
      tooltip_message <- "Move selected rows in bulk"
    }
    
    # Update the tooltip via JavaScript with a small delay to ensure
    # that the JavaScript handlers are registered
    shinyjs::delay(150, {
      session$sendCustomMessage(
        type = "updateTooltip",
        message = list(
          id = "move_rows_bulk",
          title = tooltip_message
        )
      )
    })
  }, priority = -10)  # Low priority so it runs after UI rendering
  
  # ============================================================================
  # COPY/CUT/PASTE OBSERVERS
  # ============================================================================
  # These observers implement Excel-like copy/cut/paste functionality for the
  # elements table. When copying/cutting elements, all related checkbox and
  # radiobutton value mappings are automatically included. This ensures data
  # integrity when duplicating or moving elements between tabs.
  
  # ============================================================================
  # COPY ROWS: Copy selected rows to clipboard
  # ============================================================================
  
  observeEvent(input$copy_rows, {
    # Validations - only for elements table
    req(is_copyable_table(input$file))
    req(!is.null(input$table_rows_selected))
    req(length(input$table_rows_selected) > 0)
    
    # Get selected rows
    selected_indices <- as.numeric(input$table_rows_selected)
    selected_data <- get_selected_rows_data(selected_indices)
    
    if (is.null(selected_data) || nrow(selected_data) == 0) {
      showNotification(
        "No data to copy",
        type = "error",
        duration = 3
      )
      return()
    }
    
    # Clear old clipboard data
    clear_clipboard()
    
    # Get tab information
    source_tab_name <- get_active_tab_name()
    source_tab_order <- get_active_tab_order()
    
    # Copy the data (deep copy to avoid reference issues)
    clipboard_data <- copy(selected_data)
    
    # Store basic information in clipboard
    clipboardState$data <- clipboard_data
    clipboardState$source_table <- input$file
    clipboardState$source_tab <- tabState$activeTab
    clipboardState$source_tab_name <- source_tab_name
    clipboardState$source_tab_order <- source_tab_order
    clipboardState$source_row_indices <- selected_indices
    clipboardState$operation <- "copy"
    clipboardState$timestamp <- Sys.time()
    
    # If elements table, also get related checkboxes and radiobuttons
    if (input$file == "elements") {
      # Get Element values from selected rows
      if ("Element" %in% names(clipboard_data)) {
        element_values <- clipboard_data$Element
        element_values <- element_values[!is.na(element_values) & nchar(element_values) > 0]
        
        if (length(element_values) > 0) {
          # Get related checkboxes
          related_checkboxes <- get_related_checkbox_data(
            element_values, 
            source_tab_name, 
            source_tab_order
          )
          
          # Get related radiobuttons
          related_radiobuttons <- get_related_radiobutton_data(
            element_values, 
            source_tab_name, 
            source_tab_order
          )
          
          # Store related data (deep copy)
          if (!is.null(related_checkboxes)) {
            clipboardState$related_checkboxes <- copy(related_checkboxes)
          }
          if (!is.null(related_radiobuttons)) {
            clipboardState$related_radiobuttons <- copy(related_radiobuttons)
          }
        }
      }
    }
    
    # Calculate how much data was copied
    n_rows <- nrow(clipboard_data)
    n_checkboxes <- if (!is.null(clipboardState$related_checkboxes)) nrow(clipboardState$related_checkboxes) else 0
    n_radiobuttons <- if (!is.null(clipboardState$related_radiobuttons)) nrow(clipboardState$related_radiobuttons) else 0
    
    # Show notification
    msg <- sprintf("%d row%s copied", n_rows, if (n_rows > 1) "s" else "")
    if (input$file == "elements" && (n_checkboxes > 0 || n_radiobuttons > 0)) {
      msg <- sprintf("%s (with %d related checkbox%s and %d radiobutton%s)", 
                     msg,
                     n_checkboxes, if (n_checkboxes != 1) "es" else "",
                     n_radiobuttons, if (n_radiobuttons != 1) "s" else "")
    }
    
    showNotification(
      msg,
      type = "message",
      duration = 2
    )
    
    # Log for debugging
    cat(sprintf("[COPY] %s: %d rows from %s (tab: %s)\n", 
                format(Sys.time(), "%H:%M:%S"),
                n_rows, 
                input$file, 
                source_tab_name))
    if (n_checkboxes > 0 || n_radiobuttons > 0) {
      cat(sprintf("       Related: %d checkboxes, %d radiobuttons\n", 
                  n_checkboxes, n_radiobuttons))
    }
  })
  
  # ============================================================================
  # CUT ROWS: Cut selected rows to clipboard (copy + delete)
  # ============================================================================
  
  observeEvent(input$cut_rows, {
    # Validations - only for elements table
    req(is_copyable_table(input$file))
    req(!is.null(input$table_rows_selected))
    req(length(input$table_rows_selected) > 0)
    
    # Get selected rows
    selected_indices <- as.numeric(input$table_rows_selected)
    selected_data <- get_selected_rows_data(selected_indices)
    
    if (is.null(selected_data) || nrow(selected_data) == 0) {
      showNotification(
        "No data to cut",
        type = "error",
        duration = 3
      )
      return()
    }
    
    # Clear old clipboard data
    clear_clipboard()
    
    # Get tab information
    source_tab_name <- get_active_tab_name()
    source_tab_order <- get_active_tab_order()
    
    # Copy the data (deep copy to avoid reference issues)
    clipboard_data <- copy(selected_data)
    
    # Store basic information in clipboard
    clipboardState$data <- clipboard_data
    clipboardState$source_table <- input$file
    clipboardState$source_tab <- tabState$activeTab
    clipboardState$source_tab_name <- source_tab_name
    clipboardState$source_tab_order <- source_tab_order
    clipboardState$source_row_indices <- selected_indices
    clipboardState$operation <- "cut"  # DIFFERENCE FROM COPY: "cut" instead of "copy"
    clipboardState$timestamp <- Sys.time()
    
    # If elements table, also retrieve related checkboxes and radiobuttons
    if (input$file == "elements") {
      # Extract Element values from selected rows
      if ("Element" %in% names(clipboard_data)) {
        element_values <- clipboard_data$Element
        element_values <- element_values[!is.na(element_values) & nchar(element_values) > 0]
        
        if (length(element_values) > 0) {
          # Retrieve related checkboxes
          related_checkboxes <- get_related_checkbox_data(
            element_values, 
            source_tab_name, 
            source_tab_order
          )
          
          # Retrieve related radiobuttons
          related_radiobuttons <- get_related_radiobutton_data(
            element_values, 
            source_tab_name, 
            source_tab_order
          )
          
          # Store related data (deep copy)
          if (!is.null(related_checkboxes)) {
            clipboardState$related_checkboxes <- copy(related_checkboxes)
          }
          if (!is.null(related_radiobuttons)) {
            clipboardState$related_radiobuttons <- copy(related_radiobuttons)
          }
        }
      }
    }
    
    # Calculate how much data was cut
    n_rows <- nrow(clipboard_data)
    n_checkboxes <- if (!is.null(clipboardState$related_checkboxes)) nrow(clipboardState$related_checkboxes) else 0
    n_radiobuttons <- if (!is.null(clipboardState$related_radiobuttons)) nrow(clipboardState$related_radiobuttons) else 0
    
    # Show notification (warning/orange for cut)
    msg <- sprintf("%d row%s cut", n_rows, if (n_rows > 1) "s" else "")
    if (input$file == "elements" && (n_checkboxes > 0 || n_radiobuttons > 0)) {
      msg <- sprintf("%s (with %d related checkbox%s and %d radiobutton%s)", 
                     msg,
                     n_checkboxes, if (n_checkboxes != 1) "es" else "",
                     n_radiobuttons, if (n_radiobuttons != 1) "s" else "")
    }
    
    showNotification(
      msg,
      type = "warning",  # Warning type for cut (orange)
      duration = 2
    )
    
    # Log for debugging
    cat(sprintf("[CUT] %s: %d rows from %s (tab: %s)\n", 
                format(Sys.time(), "%H:%M:%S"),
                n_rows, 
                input$file, 
                source_tab_name))
    if (n_checkboxes > 0 || n_radiobuttons > 0) {
      cat(sprintf("      Related: %d checkboxes, %d radiobuttons\n", 
                  n_checkboxes, n_radiobuttons))
    }
    cat("      Note: Rows will be removed from source when pasted\n")
  })
  
  # ============================================================================
  # PASTE ROWS: Paste clipboard data to current tab
  # ============================================================================
  # Paste operation handles both copy and cut operations. For cut operations,
  # it also removes the original rows from the source tab. Tab metadata is
  # automatically updated to match the target tab, ensuring proper tab isolation.
  
  observeEvent(input$paste_rows, {
    # 1. VALIDATIONS - only for elements table
    req(is_copyable_table(input$file))
    req(!is.null(clipboardState$data))
    req(nrow(clipboardState$data) > 0)
    
    # Check that we're pasting to same table type (must be elements)
    if (clipboardState$source_table != input$file) {
      showNotification(
        sprintf("Cannot paste from %s to %s", clipboardState$source_table, input$file),
        type = "error",
        duration = 3
      )
      return()
    }
    
    # 2. GET TARGET TAB INFO
    target_tab_name <- get_active_tab_name()
    target_tab_order <- get_active_tab_order()
    
    if (is.null(target_tab_name) || is.null(target_tab_order)) {
      showNotification("No active tab found", type = "error", duration = 3)
      return()
    }
    
    # 3. GET TARGET TAB DATA
    target_data <- get_active_tab_data()
    if (is.null(target_data)) {
      showNotification("No target data found", type = "error", duration = 3)
      return()
    }
    
    # 4. COPY CLIPBOARD DATA AND UPDATE TAB METADATA
    paste_data <- copy(clipboardState$data)
    
    # Update tab metadata to target tab
    if ("tab_name_meta" %in% names(paste_data)) {
      paste_data$tab_name_meta <- target_tab_name
    }
    if ("tab_order_meta" %in% names(paste_data)) {
      paste_data$tab_order_meta <- target_tab_order
    }
    
    # 5. ADD DATA TO TARGET TAB
    target_data <- rbind(target_data, paste_data)
    
    # 6. UPDATE TARGET TAB IN TABSTATE
    if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_idx) > 0) {
        tabState$tabs[[active_idx]]$data <- target_data
      }
    }
    
    n_rows <- nrow(paste_data)
    n_checkboxes <- 0
    n_radiobuttons <- 0
    
    # 7. PASTE RELATED DATA (ONLY FOR ELEMENTS)
    if (input$file == "elements") {
      
      # 7a. PASTE RELATED CHECKBOXES
      if (!is.null(clipboardState$related_checkboxes) && nrow(clipboardState$related_checkboxes) > 0) {
        related_checkboxes <- copy(clipboardState$related_checkboxes)
        
        # Update tab metadata to target tab
        if ("tab_name_meta" %in% names(related_checkboxes)) {
          related_checkboxes$tab_name_meta <- target_tab_name
        }
        if ("tab_order_meta" %in% names(related_checkboxes)) {
          related_checkboxes$tab_order_meta <- target_tab_order
        }
        
        # Add to mappingData
        checkbox_data <- mappingData[["waarde_checkboxes"]]
        if (!is.null(checkbox_data)) {
          mappingData[["waarde_checkboxes"]] <<- rbind(checkbox_data, related_checkboxes)
          n_checkboxes <- nrow(related_checkboxes)
        }
      }
      
      # 7b. PASTE RELATED RADIOBUTTONS
      if (!is.null(clipboardState$related_radiobuttons) && nrow(clipboardState$related_radiobuttons) > 0) {
        related_radiobuttons <- copy(clipboardState$related_radiobuttons)
        
        # Update tab metadata to target tab
        if ("tab_name_meta" %in% names(related_radiobuttons)) {
          related_radiobuttons$tab_name_meta <- target_tab_name
        }
        if ("tab_order_meta" %in% names(related_radiobuttons)) {
          related_radiobuttons$tab_order_meta <- target_tab_order
        }
        
        # Add to mappingData
        radiobutton_data <- mappingData[["waarde_radiobuttons"]]
        if (!is.null(radiobutton_data)) {
          # Add new rows via rbindlist with fill option
          combined <- rbindlist(list(radiobutton_data, related_radiobuttons), fill = TRUE)
          
          # Update mappingData met GLOBAL ASSIGNMENT (<<-)
          # Dit is nodig voor reactiveValues binnen observers
          mappingData[["waarde_radiobuttons"]] <<- combined
          
          n_radiobuttons <- nrow(related_radiobuttons)
        }
      }
    }
    
    # 8. REMOVE SOURCE ROWS FOR CUT OPERATION
    if (clipboardState$operation == "cut") {
      # Find the source tab
      # Note: tabState$tabs only contains tabs for CURRENT table (input$file)
      # If we're in the same table (source == target table), search for source tab
      source_tab_idx <- which(sapply(tabState$tabs, function(t) {
        t$name == clipboardState$source_tab_name &&
        t$order == clipboardState$source_tab_order
      }))
      
      if (length(source_tab_idx) > 0) {
        source_data <- tabState$tabs[[source_tab_idx]]$data
        
        # Remove copied rows (use row indices)
        if (!is.null(clipboardState$source_row_indices) && length(clipboardState$source_row_indices) > 0) {
          indices_to_keep <- setdiff(seq_len(nrow(source_data)), clipboardState$source_row_indices)
          
          if (length(indices_to_keep) > 0) {
            source_data <- source_data[indices_to_keep, ]
          } else {
            # If all rows are removed, create empty data.frame with same structure
            source_data <- source_data[0, ]
          }
          
          tabState$tabs[[source_tab_idx]]$data <- source_data
          
          # 8b. REMOVE RELATED DATA FOR CUT OPERATION (ELEMENTS ONLY)
          if (clipboardState$source_table == "elements") {
            
            # Remove related checkboxes from mappingData
            if (!is.null(clipboardState$related_checkboxes) && nrow(clipboardState$related_checkboxes) > 0) {
              checkbox_data <- mappingData[["waarde_checkboxes"]]
              
              if (!is.null(checkbox_data) && nrow(checkbox_data) > 0) {
                # CRITICAL FIX: Match on Element + waarde + TAB METADATA
                # This prevents removing newly pasted checkboxes!
                source_tab_name <- clipboardState$source_tab_name
                source_tab_order <- clipboardState$source_tab_order

                related_keys <- paste(clipboardState$related_checkboxes$Element, 
                                     clipboardState$related_checkboxes$waarde,
                                     source_tab_name,
                                     source_tab_order,
                                     sep = "|||")
                
                existing_keys <- paste(checkbox_data$Element, 
                                      checkbox_data$waarde,
                                      checkbox_data$tab_name_meta,
                                      checkbox_data$tab_order_meta,
                                      sep = "|||")
                
                # Remove ONLY matching rows with same tab metadata
                indices_to_keep <- which(!existing_keys %in% related_keys)
                
                if (length(indices_to_keep) > 0) {
                  mappingData[["waarde_checkboxes"]] <<- checkbox_data[indices_to_keep, ]
                } else {
                  mappingData[["waarde_checkboxes"]] <<- checkbox_data[0, ]
                }
              }
            }
            
            # Remove related radiobuttons from mappingData
            if (!is.null(clipboardState$related_radiobuttons) && nrow(clipboardState$related_radiobuttons) > 0) {
              radiobutton_data <- mappingData[["waarde_radiobuttons"]]
              
              if (!is.null(radiobutton_data) && nrow(radiobutton_data) > 0) {
                # CRITICAL FIX: Match on Element + waarde + TAB METADATA
                # This prevents removing newly pasted radiobuttons!
                source_tab_name <- clipboardState$source_tab_name
                source_tab_order <- clipboardState$source_tab_order
                
                related_keys <- paste(clipboardState$related_radiobuttons$Element, 
                                     clipboardState$related_radiobuttons$waarde,
                                     source_tab_name,
                                     source_tab_order,
                                     sep = "|||")
                
                existing_keys <- paste(radiobutton_data$Element, 
                                      radiobutton_data$waarde,
                                      radiobutton_data$tab_name_meta,
                                      radiobutton_data$tab_order_meta,
                                      sep = "|||")
                
                # Remove ONLY matching rows with same tab metadata
                indices_to_keep <- which(!existing_keys %in% related_keys)
                
                if (length(indices_to_keep) > 0) {
                  mappingData[["waarde_radiobuttons"]] <<- radiobutton_data[indices_to_keep, ]
                } else {
                  mappingData[["waarde_radiobuttons"]] <<- radiobutton_data[0, ]
                }
              }
            }
          }
        }
      }
    }
    
    # 9. RENDER UPDATED TABLE
    render_table(target_data, input$file)
    
    # 9b. DETECT DUPLICATES (for elements table)
    if (input$file == "elements") {
      dupes <- detect_duplicate_elements(target_data)
      duplicateElements(dupes)
    }
    
    # 10. CONSOLIDATE ELEMENTS DATA TO MAPPINGDATA (so checkbox/radiobutton tabs load correctly)
    if (input$file == "elements") {
      # Consolidate all elements tabs to mappingData
      consolidated_elements <- consolidate_tabs_with_metadata(tabState$tabs)
      mappingData[["elements"]] <<- consolidated_elements
      
      # WARNING: Do NOT call updateCheckboxMapping/updateRadioMapping here!
      # The related data is already correctly pasted in step 7a/7b with the right values.
      # If we call updateMapping() here, values might be overwritten.
      # updateMapping() is called during table switch (line ~1769) when needed.
    }
    
    # 11. SHOW NOTIFICATION
    operation_text <- if (clipboardState$operation == "cut") "moved" else "pasted"
    msg <- sprintf("%d row%s %s", n_rows, if (n_rows > 1) "s" else "", operation_text)
    
    if (input$file == "elements" && (n_checkboxes > 0 || n_radiobuttons > 0)) {
      msg <- sprintf("%s (with %d related checkbox%s and %d radiobutton%s)", 
                     msg, n_checkboxes, if (n_checkboxes != 1) "es" else "",
                     n_radiobuttons, if (n_radiobuttons != 1) "s" else "")
    }
    
    showNotification(msg, type = "message", duration = 2)
    
    # 12. DEBUG LOG FOR PASTE OPERATION
    cat(sprintf("[PASTE] %s: %d rows %s to %s (tab: %s)\n", 
                format(Sys.time(), "%H:%M:%S"),
                n_rows,
                operation_text,
                input$file, 
                target_tab_name))
    
    if (n_checkboxes > 0 || n_radiobuttons > 0) {
      cat(sprintf("        Related: %d checkboxes, %d radiobuttons\n", 
                  n_checkboxes, n_radiobuttons))
    }
    
    if (clipboardState$operation == "cut") {
      cat(sprintf("        Removed from source: %s (tab: %s)\n",
                  clipboardState$source_table,
                  clipboardState$source_tab_name))
    }
    
    # 13. TRIGGER RELATED DATA UPDATE (voor reactivity timing fix)
    # Dit forceert een refresh van radiobuttons/checkboxes tabellen
    if (n_checkboxes > 0 || n_radiobuttons > 0) {
      relatedDataUpdated(relatedDataUpdated() + 1)
    }
    
    # 14. CLEAR CLIPBOARD
    clear_clipboard()
  })

  # ============================================================================
  # BULK ROW MOVE - MODAL & VALIDATION
  # ============================================================================
  # These observers handle bulk moving of selected rows to a new position
  # The rows maintain their relative order and move as a block
  
  # Observer: Open bulk move modal when button is clicked
  observeEvent(input$move_rows_bulk, {
    # 1. VALIDATIONS
    req(!is.null(input$file))
    req(!is.null(input$table_rows_selected))
    req(length(input$table_rows_selected) > 0)
    
    # Get selected row indices
    selected_indices <- as.numeric(input$table_rows_selected)
    
    # Check if bulk move is enabled for this table
    validation <- is_bulk_move_enabled(input$file, selected_indices)
    
    if (!validation$enabled) {
      showNotification(
        validation$message,
        type = "error",
        duration = 3
      )
      return()
    }
    
    # 2. GET ACTIVE TAB DATA
    active_data <- get_active_tab_data()
    if (is.null(active_data)) {
      showNotification("No active table data found", type = "error", duration = 3)
      return()
    }
    
    total_rows <- nrow(active_data)
    
    # 3. SORT AND PREPARE SELECTION INFO
    selected_indices <- sort(unique(selected_indices))
    n_selected <- length(selected_indices)
    
    # Create a nice display of current positions
    if (n_selected <= 5) {
      current_positions_text <- paste(selected_indices, collapse = ", ")
    } else {
      # If there are many rows, show first 3 and last 2
      current_positions_text <- sprintf("%s, ..., %s", 
                                       paste(head(selected_indices, 3), collapse = ", "),
                                       paste(tail(selected_indices, 2), collapse = ", "))
    }
    
    # 4. STORE STATE FOR LATER USE
    bulkMoveState$pending <- TRUE
    bulkMoveState$selected_indices <- selected_indices
    bulkMoveState$total_rows <- total_rows
    bulkMoveState$preview_data <- list(
      n_selected = n_selected,
      current_positions = current_positions_text
    )
    
    # 5. SHOW MODAL DIALOG
    showModal(modalDialog(
      title = "Move Rows in Bulk",
      size = "m",
      
      tags$div(
        style = "padding: 10px;",
        
        # Selection info
        tags$p(
          style = "margin-bottom: 15px; color: #333;",
          tags$strong(sprintf("Selected: %d row%s", n_selected, if(n_selected > 1) "s" else ""))
        ),
        tags$p(
          style = "margin-bottom: 20px; color: #666; font-size: 0.9em;",
          sprintf("Current positions: %s", current_positions_text)
        ),
        
        # Input for target position
        tags$div(
          style = "margin-bottom: 15px;",
          numericInput(
            "bulk_move_target",
            label = "Move first selected row to position:",
            value = 1,
            min = 1,
            max = total_rows,
            step = 1,
            width = "100%"
          )
        ),
        
        # Preview of new positions (dynamic)
        uiOutput("bulk_move_preview"),
        
        # Info text
        tags$p(
          style = "margin-top: 20px; color: #888; font-size: 0.85em; font-style: italic;",
          "The selected rows will be moved as a block to the target position, maintaining their relative order."
        )
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("bulk_move_confirm", "Move Rows", class = "btn-primary", icon = icon("arrows-alt-v"))
      ),
      easyClose = FALSE
    ))
    
    # Debug log for bulk move modal
    cat(sprintf("[BULK MOVE] %s: Modal opened - %d rows selected from %s\n", 
                format(Sys.time(), "%H:%M:%S"),
                n_selected,
                input$file))
  })
  
  # Observer: Render preview van nieuwe posities in modal
  output$bulk_move_preview <- renderUI({
    req(bulkMoveState$pending)
    req(!is.null(input$bulk_move_target))
    
    target_pos <- input$bulk_move_target
    n_selected <- bulkMoveState$preview_data$n_selected
    total_rows <- bulkMoveState$total_rows
    selected_indices <- bulkMoveState$selected_indices
    
    # Calculate position preview
    preview <- calculate_bulk_move_positions(selected_indices, target_pos, total_rows)
    
    if (!preview$valid) {
      return(tags$div(
        style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px;",
        tags$p(
          style = "margin: 0; color: #856404;",
          icon("exclamation-triangle"),
          " ",
          preview$message
        )
      ))
    }
    
    # Show preview of new positions
    tags$div(
      style = "padding: 10px; background-color: #d4edda; border: 1px solid #28a745; border-radius: 4px;",
      tags$p(
        style = "margin: 0; color: #155724;",
        icon("info-circle"),
        " ",
        tags$strong(sprintf("New positions: %s", preview$final_range))
      )
    )
  })
  
  # Observer: Confirm bulk move and execute
  observeEvent(input$bulk_move_confirm, {
    # 1. VALIDATIONS
    req(bulkMoveState$pending)
    req(!is.null(input$bulk_move_target))
    req(!is.null(bulkMoveState$selected_indices))
    req(!is.null(input$file))
    
    target_position <- as.integer(input$bulk_move_target)
    selected_indices <- bulkMoveState$selected_indices
    total_rows <- bulkMoveState$total_rows
    
    # 2. CALCULATE POSITIONS
    position_calc <- calculate_bulk_move_positions(selected_indices, target_position, total_rows)
    
    if (!position_calc$valid) {
      showNotification(
        position_calc$message,
        type = "error",
        duration = 3
      )
      return()
    }
    
    # 3. GET ACTIVE TAB DATA
    active_data <- get_active_tab_data()
    if (is.null(active_data)) {
      showNotification("No active table data found", type = "error", duration = 3)
      removeModal()
      return()
    }
    
    # 4. PERFORM BULK MOVE
    moved_data <- perform_bulk_move(active_data, selected_indices, position_calc$target_position)
    
    if (is.null(moved_data)) {
      showNotification("Failed to move rows", type = "error", duration = 3)
      removeModal()
      return()
    }
    
    # 5. UPDATE ACTIVE TAB DATA
    if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_idx) > 0) {
        tabState$tabs[[active_idx]]$data <- moved_data
      }
    }
    
    # 6. CONSOLIDATE ELEMENTS DATA TO MAPPINGDATA (if necessary)
    if (input$file == "elements") {
      consolidated_elements <- consolidate_tabs_with_metadata(tabState$tabs)
      mappingData[["elements"]] <<- consolidated_elements
    }
    
    # 7. RENDER UPDATED TABLE
    render_table(moved_data, input$file)
    
    # 7b. DETECT DUPLICATES (for elements table)
    if (input$file == "elements") {
      dupes <- detect_duplicate_elements(moved_data)
      duplicateElements(dupes)
    }
    
    # 8. SHOW SUCCESS NOTIFICATION
    n_selected <- position_calc$n_selected
    msg <- sprintf("%d row%s moved to position%s %s", 
                   n_selected, 
                   if(n_selected > 1) "s" else "",
                   if(n_selected > 1) "s" else "",
                   position_calc$final_range)
    
    showNotification(
      msg,
      type = "message",
      duration = 3
    )
    
    # 9. DEBUG LOG FOR BULK MOVE OPERATION
    cat(sprintf("[BULK MOVE] %s: %d rows moved to positions %s in %s\n", 
                format(Sys.time(), "%H:%M:%S"),
                n_selected,
                position_calc$final_range,
                input$file))
    
    # 10. CLEANUP
    bulkMoveState$pending <- FALSE
    bulkMoveState$selected_indices <- NULL
    bulkMoveState$target_position <- NULL
    bulkMoveState$preview_data <- NULL
    bulkMoveState$total_rows <- NULL
    
    # 11. CLOSE MODAL
    removeModal()
  })

  observeEvent(input$confirm_add_rows, {
    req(is_selectable_table(input$file))
    n <- as.integer(input$rows_to_add)
    if (is.na(n) || n < 1) n <- 1

    # Add rows to active tab's data (maintains tab isolation)
    active_data <- get_active_tab_data()
    if (is.null(active_data) || ncol(active_data) == 0) {
      removeModal()
      return()
    }

    # Check if total rows will exceed 50 and show warning
    current_rows <- nrow(active_data)
    total_rows_after_add <- current_rows + n
    
    if (total_rows_after_add >= 51) {
      showModalSafe(modalDialog(
        title = "Warning",
        tags$p("Adding this amount of rows will slow down the program! We recommend not adding more than 50 rows per tab."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("proceed_add_rows", "Proceed anyway", class = "btn-warning")
        ),
        easyClose = FALSE
      ))
      return()
    }

    # Create new empty rows
    new_rows <- as.data.frame(matrix("", nrow = n, ncol = ncol(active_data)), stringsAsFactors = FALSE)
    colnames(new_rows) <- colnames(active_data)
    active_data <- rbind(active_data, new_rows)
    
    # Update the active tab data
    if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_idx) > 0) {
        tabState$tabs[[active_idx]]$data <- active_data
      }
    }
    
    # Detect duplicates if this is the elements table
    if (input$file == "elements") {
      dupes <- detect_duplicate_elements(active_data)
      duplicateElements(dupes)
    }
    
    removeModal()
    render_table(active_data, input$file)
  })
  
  # Handle "Proceed anyway" button when user wants to add more than 50 rows
  observeEvent(input$proceed_add_rows, {
    req(is_selectable_table(input$file))
    n <- as.integer(input$rows_to_add)
    if (is.na(n) || n < 1) n <- 1

    # Add rows to active tab's data (maintains tab isolation)
    active_data <- get_active_tab_data()
    if (is.null(active_data) || ncol(active_data) == 0) {
      removeModal()
      return()
    }

    # Create new empty rows
    new_rows <- as.data.frame(matrix("", nrow = n, ncol = ncol(active_data)), stringsAsFactors = FALSE)
    colnames(new_rows) <- colnames(active_data)
    active_data <- rbind(active_data, new_rows)
    
    # Update the active tab data
    if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_idx) > 0) {
        tabState$tabs[[active_idx]]$data <- active_data
      }
    }
    
    # Detect duplicates if this is the elements table
    if (input$file == "elements") {
      dupes <- detect_duplicate_elements(active_data)
      duplicateElements(dupes)
    }
    
    removeModal()
    render_table(active_data, input$file)
  })
  
  observeEvent(input$table_cell_edit, {
    req(is_selectable_table(input$file))
    info <- input$table_cell_edit
    if (is.null(info$col) || length(info$col) == 0) return(NULL)
    
    col_index <- as.numeric(info$col)
    
    # Work with active tab data (maintains tab isolation)
    new_data <- get_active_tab_data()
    if (is.null(new_data) || ncol(new_data) == 0) return(NULL)
    
    if (col_index <= 1) {
      # Reordering (first column is row number)
      new_index <- suppressWarnings(as.numeric(info$value))
      if (is.na(new_index)) new_index <- info$row
      
      total <- nrow(new_data)
      if (new_index < 1) new_index <- 1
      if (new_index > total) new_index <- total
      current_pos <- info$row
      if (current_pos != new_index) {
        row_data <- new_data[current_pos, ]
        new_data <- new_data[-current_pos, ]
        if (new_index == 1) {
          new_data <- rbind(row_data, new_data)
        } else if (new_index > nrow(new_data)) {
          new_data <- rbind(new_data, row_data)
        } else {
          top <- new_data[1:(new_index - 1), , drop = FALSE]
          bottom <- new_data[new_index:nrow(new_data), , drop = FALSE]
          new_data <- rbind(top, row_data, bottom)
        }
      }
    } else {
      # Regular cell edit
      if ((col_index - 1) < 1 || (col_index - 1) > ncol(new_data)) {
        return(NULL)
      }
      new_data[info$row, col_index - 1] <- info$value
    }
    
    # Update active tab data
    if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_idx) > 0) {
        tabState$tabs[[active_idx]]$data <- new_data
      }
    }
    
    # If we edited the elements table, update checkbox/radiobutton mappings
    if (input$file == "elements" && length(tabState$tabs) > 0) {
      # First consolidate all tab data into mappingData
      mappingData[["elements"]] <<- consolidate_tabs_with_metadata(tabState$tabs)
      
      # Then update the related mappings
      updateCheckboxMapping()
      updateRadioMapping()
    }
    
    render_table(new_data, input$file)
  })
  
  observeEvent(input$dropdown_change, {
    req(is_selectable_table(input$file))
    info <- input$dropdown_change
    if(info$value == "__ADD__") {
      currentEdit$row <- as.numeric(info$row)
      currentEdit$col <- info$col
      
      # Use active tab data (maintains tab isolation)
      active_data <- get_active_tab_data()
      currentEdit$orig <- active_data[currentEdit$row, currentEdit$col, with = FALSE][[1]]
      
      showModalSafe(modalDialog(
        title = "Add new value",
        textInput("new_value", "Enter new value:", value = ""),
        footer = tagList(
          actionButton("modal_save", "Save", class = "btn-primary"),
          tags$button("Cancel", type = "button", class = "btn btn-default modal-cancel", `data-dismiss` = "modal")
        )
      ))
    } else {
      row <- as.numeric(info$row)
      colName <- info$col
      
      # Use active tab data (maintains tab isolation)
      new_data <- get_active_tab_data()
      new_data[row, (colName) := info$value]
      
      # Update active tab
      if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
        active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
        if (length(active_idx) > 0) {
          tabState$tabs[[active_idx]]$data <- new_data
        }
      }
      
      # If we edited the elements table, update checkbox/radiobutton mappings
      if (input$file == "elements" && length(tabState$tabs) > 0) {
        # First consolidate all tab data into mappingData
        mappingData[["elements"]] <<- consolidate_tabs_with_metadata(tabState$tabs)
        
        # Then update the related mappings
        updateCheckboxMapping()
        updateRadioMapping()
      }
      
      # Check for duplicate elements/castor names and update reactive value
      if (input$file == "elements" && (colName == "Element" || colName == "castor_kolom")) {
        dupes <- detect_duplicate_elements(new_data)
        duplicateElements(dupes)
        
        # Re-render table to show orange highlighting
        render_table(new_data, input$file, mode = "proxy")
        
        # Show warning notification if duplicates exist
        has_element_dupes <- length(dupes$duplicate_values_element) > 0
        has_castor_dupes <- length(dupes$duplicate_values_castor) > 0
        
        if (has_element_dupes || has_castor_dupes) {
          # Build detailed duplicate information with row numbers
          notification_parts <- c()
          
          if (has_element_dupes) {
            # Group duplicates by value and show row numbers
            element_details <- sapply(dupes$duplicate_values_element, function(val) {
              rows <- dupes$duplicate_rows_element[new_data$Element[dupes$duplicate_rows_element] == val]
              sprintf("<strong>%s</strong> (rows: %s)", val, paste(rows, collapse = ", "))
            })
            notification_parts <- c(
              notification_parts,
              sprintf("<strong>Duplicate Elements:</strong><br/>%s", 
                      paste(element_details, collapse = "<br/>"))
            )
          }
          
          if (has_castor_dupes) {
            # Group duplicates by value and show row numbers
            castor_details <- sapply(dupes$duplicate_values_castor, function(val) {
              rows <- dupes$duplicate_rows_castor[new_data$castor_kolom[dupes$duplicate_rows_castor] == val]
              sprintf("<strong>%s</strong> (rows: %s)", val, paste(rows, collapse = ", "))
            })
            notification_parts <- c(
              notification_parts,
              sprintf("<strong>Duplicate Castor Names:</strong><br/>%s", 
                      paste(castor_details, collapse = "<br/>"))
            )
          }
          
          duplicate_info <- paste(notification_parts, collapse = "<br/><br/>")
          
          # Show notification with detailed information
          showNotification(
            ui = HTML(sprintf(
              "<strong style='color: #ff9800;'><i class='fa fa-exclamation-triangle'></i> Duplicate Values Detected</strong><br/>
              Duplicates are highlighted in <span style='background-color: #ff9800; color: white; padding: 2px 4px; border-radius: 3px;'>ORANGE</span>.<br/>
              <small style='margin-top: 8px; display: block;'>%s</small>",
              duplicate_info
            )),
            duration = 15,
            closeButton = TRUE,
            type = "warning"
          )
        }
      } else {
        # For non-duplicate-checked column changes, use proxy update to reflect changes
        render_table(new_data, input$file, mode = "proxy")
      }
    }
  })
  
  observeEvent(input$modal_save, {
    # Allow empty values for testing purposes
    req(!is.null(input$new_value))
    req(is_selectable_table(input$file))
    
    # Work with active tab data (maintains tab isolation)
    new_data <- get_active_tab_data()
    if (is.null(new_data)) return(NULL)
    
    new_data[currentEdit$row, (currentEdit$col) := input$new_value]
    
    key <- paste0(currentEdit$row, "_", currentEdit$col)
    manualValues$vals[[key]] <- TRUE
    
    # Update active tab data
    if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_idx) > 0) {
        tabState$tabs[[active_idx]]$data <- new_data
      }
    }
    
    # If we edit elements, also update checkbox/radiobutton mappings
    if (input$file == "elements" && length(tabState$tabs) > 0) {
      mappingData[["elements"]] <<- consolidate_tabs_with_metadata(tabState$tabs)
      updateCheckboxMapping()
      updateRadioMapping()
    }
    
    removeModal()
    
    # Detect duplicates if this is the elements table and Element/castor_kolom was edited
    if (input$file == "elements" && (currentEdit$col == "Element" || currentEdit$col == "castor_kolom")) {
      dupes <- detect_duplicate_elements(new_data)
      duplicateElements(dupes)
    }
    
    # Send direct cell update to JavaScript (avoids full table re-render)
    session$sendCustomMessage("updateCellValue", list(
      row = currentEdit$row,
      col = currentEdit$col,
      value = input$new_value,
      cssClass = "blue-cell"  # Manual values are marked blue
    ))
  })
  
  observeEvent(input$modal_dbl_save, {
    # Allow empty values for consistency with modal_save
    req(!is.null(input$new_option))
    req(input$dropdown_dblclick$row, input$dropdown_dblclick$col)
    req(is_selectable_table(input$file))
    
    # Work with active tab data (maintains tab isolation)
    new_data <- get_active_tab_data()
    if (is.null(new_data)) return(NULL)
    
    new_data[as.numeric(input$dropdown_dblclick$row), (input$dropdown_dblclick$col) := input$new_option]
    
    # Update active tab data
    if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_idx) > 0) {
        tabState$tabs[[active_idx]]$data <- new_data
      }
    }
    
    # If we edit elements, also update checkbox/radiobutton mappings
    if (input$file == "elements" && length(tabState$tabs) > 0) {
      mappingData[["elements"]] <<- consolidate_tabs_with_metadata(tabState$tabs)
      updateCheckboxMapping()
      updateRadioMapping()
    }
    
    session$sendCustomMessage("updateSelectedOption",
                              list(id = input$dropdown_dblclick$id, new_value = input$new_option))
    
    removeModal()
    render_table(new_data, input$file)
    
    # Detect duplicates if this is the elements table
    if (input$file == "elements") {
      dupes <- detect_duplicate_elements(new_data)
      duplicateElements(dupes)
    }
  })
  
  # Save: write exclusively to the database and then update the CSV files
  observeEvent(input$save, {
    # Check for duplicates before saving (only for elements table)
    if (input$file == "elements" && length(tabState$tabs) > 0) {
      # Consolidate all tabs to check for duplicates across entire dataset
      consolidated_data <- consolidate_tabs_with_metadata(tabState$tabs)
      dupes <- detect_duplicate_elements(consolidated_data)
      
      has_element_dupes <- length(dupes$duplicate_values_element) > 0
      has_castor_dupes <- length(dupes$duplicate_values_castor) > 0
      
      if (has_element_dupes || has_castor_dupes) {
        # Build warning message with details
        warning_parts <- list()
        
        if (has_element_dupes) {
          element_list <- paste(dupes$duplicate_values_element, collapse = ", ")
          warning_parts <- c(warning_parts, list(
            tags$div(
              style = "margin-bottom: 10px;",
              tags$strong(
                style = "color: #ff9800;",
                sprintf("• %d duplicate Element value(s): ", length(dupes$duplicate_values_element))
              ),
              tags$br(),
              tags$span(style = "margin-left: 15px; font-family: monospace;", element_list)
            )
          ))
        }
        
        if (has_castor_dupes) {
          castor_list <- paste(dupes$duplicate_values_castor, collapse = ", ")
          warning_parts <- c(warning_parts, list(
            tags$div(
              style = "margin-bottom: 10px;",
              tags$strong(
                style = "color: #ff9800;",
                sprintf("• %d duplicate Castor Name value(s): ", length(dupes$duplicate_values_castor))
              ),
              tags$br(),
              tags$span(style = "margin-left: 15px; font-family: monospace;", castor_list)
            )
          ))
        }
        
        # Show confirmation modal
        showModalSafe(modalDialog(
          title = tags$div(
            style = "color: #ff9800;",
            icon("exclamation-triangle", style = "margin-right: 8px;"),
            "Save with Duplicates?"
          ),
          tags$div(
            style = "padding: 10px 0;",
            tags$p(
              style = "font-weight: bold; margin-bottom: 15px;",
              "The following duplicates were detected:"
            ),
            warning_parts,
            tags$hr(),
            tags$div(
              style = "padding: 10px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
              tags$p(
                style = "margin: 0; font-size: 14px;",
                icon("info-circle", style = "margin-right: 5px;"),
                "Duplicates may cause issues during data export. ",
                "It is recommended to fix duplicates before saving."
              )
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_save_with_duplicates", "Save Anyway", 
                         class = "btn-warning",
                         icon = icon("save"))
          ),
          size = "m",
          easyClose = FALSE
        ))
        
        # Exit early - wait for user confirmation
        return(NULL)
      }
    }
    
    # Consolidate all tabs back into mappingData before saving
    # For elements table, also update checkbox/radiobutton mappings
    if (input$file == "elements" && length(tabState$tabs) > 0) {
      # Consolidate all tabs for elements
      mappingData[[input$file]] <<- consolidate_tabs_with_metadata(tabState$tabs)
      
      # Run auto-fill to update radiobuttons/checkboxes based on new elements data
      updateCheckboxMapping()
      updateRadioMapping()
    } else if (input$file %in% c("waarde_checkboxes", "waarde_radiobuttons") && length(tabState$tabs) > 0) {
      # For checkboxes/radiobuttons: also consolidate their tabs to mappingData
      # Otherwise manual edits will be lost on save!
      mappingData[[input$file]] <<- consolidate_tabs_with_metadata(tabState$tabs)
    }
    
    for (tableName in names(mappingData)) {
      dt <- mappingData[[tableName]]
      
      # Remove empty rows for elements table
      if (tableName == "elements") {
        dt <- dt[!is.na(Element)]
      }
      
      # Remove empty rows (rows where all data columns are NA or empty, excluding metadata)
      data_cols <- setdiff(names(dt), c("tab_name_meta", "tab_order_meta"))
      if (length(data_cols) > 0 && nrow(dt) > 0) {
        # Determine which rows have at least one non-NA, non-empty value
        keep_rows <- rep(FALSE, nrow(dt))
        for (col in data_cols) {
          col_vals <- dt[[col]]
          keep_rows <- keep_rows | (!is.na(col_vals) & col_vals != "")
        }
        dt <- dt[keep_rows, ]
      }
      
      dbWriteTable(con, tableName, dt, overwrite = TRUE)
    }

    # Note: updateCheckboxMapping() and updateRadioMapping() are already called
    # in the if-statement above (line ~7434-7439) when input$file == "elements"
    # Verwijderd om dubbele aanroepen te voorkomen die dubbele rijen veroorzaken
    
    # Update the CSV files from the database (central paths)
    database_to_csv(dataFolder = epc_path("mapping_dir"), dbPath = epc_path("mapping_db"))
    
    showModalSafe(modalDialog(
      title = "Success",
      "Database and CSV files have been saved successfully!"
    ))
  })
  
  # Confirm save with duplicates - executes save after user confirmation
  observeEvent(input$confirm_save_with_duplicates, {
    # Close confirmation modal
    removeModal()
    
    # Consolidate all tabs back into mappingData before saving
    # For elements table, also update checkbox/radiobutton mappings
    if (input$file == "elements" && length(tabState$tabs) > 0) {
      # Consolidate all tabs for elements
      mappingData[[input$file]] <<- consolidate_tabs_with_metadata(tabState$tabs)
      
      # Run auto-fill to update radiobuttons/checkboxes based on new elements data
      updateCheckboxMapping()
      updateRadioMapping()
    } else if (input$file %in% c("waarde_checkboxes", "waarde_radiobuttons") && length(tabState$tabs) > 0) {
      # For checkboxes/radiobuttons: also consolidate their tabs to mappingData
      # Otherwise manual edits will be lost on save!
      mappingData[[input$file]] <<- consolidate_tabs_with_metadata(tabState$tabs)
    }
    
    for (tableName in names(mappingData)) {
      dt <- mappingData[[tableName]]
      
      # Remove empty rows for elements table
      if (tableName == "elements") {
        dt <- dt[!is.na(Element)]
      }
      
      # Remove empty rows (rows where all data columns are NA or empty, excluding metadata)
      data_cols <- setdiff(names(dt), c("tab_name_meta", "tab_order_meta"))
      if (length(data_cols) > 0 && nrow(dt) > 0) {
        # Determine which rows have at least one non-NA, non-empty value
        keep_rows <- rep(FALSE, nrow(dt))
        for (col in data_cols) {
          col_vals <- dt[[col]]
          keep_rows <- keep_rows | (!is.na(col_vals) & col_vals != "")
        }
        dt <- dt[keep_rows, ]
      }
      
      dbWriteTable(con, tableName, dt, overwrite = TRUE)
    }

    # Update the CSV files from the database (central paths)
    database_to_csv(dataFolder = epc_path("mapping_dir"), dbPath = epc_path("mapping_db"))
    
    showModalSafe(modalDialog(
      title = tags$div(
        icon("check-circle", style = "color: #4CAF50; margin-right: 8px;"),
        "Saved with Duplicates"
      ),
      tags$div(
        tags$p("Database and CSV files have been saved successfully."),
        tags$div(
          style = "padding: 10px; background-color: #fff3e0; border-left: 4px solid #ff9800; border-radius: 4px; margin-top: 10px;",
          tags$p(
            style = "margin: 0; font-size: 14px;",
            icon("exclamation-triangle", style = "margin-right: 5px; color: #ff9800;"),
            "Note: Duplicate values are still present. Please review and fix them when possible."
          )
        )
      ),
      footer = modalButton("Close")
    ))
  })

  # ===== IMPORT WIZARD MODAL =====
  # Open the import wizard when user clicks "Manage input files"
  
  observeEvent(input$select_epic_file, {
    showModalSafe(modalDialog(
      title = "Data Import Wizard - Multi-Step Data Mapping",
      tagList(
        tags$head(
          tags$style(HTML("
            .wizard-modal .modal-body {
              max-height: calc(100vh - 250px);
              overflow-y: auto;
              padding: 20px;
            }
            .wizard-step {
              background-color: #f8f9fa;
              padding: 15px;
              margin-bottom: 20px;
              border-radius: 5px;
              border-left: 4px solid #007bff;
            }
            .wizard-step h3 {
              margin-top: 0;
              color: #495057;
              font-size: 1.2rem;
            }
            .step-complete {
              border-left-color: #28a745;
              background-color: #d4edda;
            }
            .step-active {
              border-left-color: #007bff;
            }
            .confidence-high {
              color: #28a745;
              font-weight: bold;
            }
            .confidence-medium {
              color: #ffc107;
              font-weight: bold;
            }
            .confidence-low {
              color: #fd7e14;
              font-weight: bold;
            }
            .confidence-very-low {
              color: #dc3545;
              font-weight: bold;
            }
            /* Constrain mapping samples to stay within their cell */
            .wizard-modal .mapping-sample {
              word-wrap: break-word;
              overflow-wrap: break-word;
              white-space: normal;
              max-width: 100%;
              line-height: 1.3em;
              max-height: calc(1.3em * 2);
              overflow: hidden;
              text-overflow: ellipsis;
              display: -webkit-box;
              -webkit-line-clamp: 2;
              -webkit-box-orient: vertical;
            }
            .wizard-modal .mapping-sample-container {
              max-width: 100%;
              overflow: hidden;
              display: block;
              word-break: break-word;
            }
            /* Allow long dropdown items to wrap instead of widening */
            .wizard-modal .selectize-input {
              white-space: normal;
              word-wrap: break-word;
              overflow-wrap: break-word;
              word-break: break-all;
              min-height: 40px;
              height: auto;
              padding: 8px 10px;
            }
            .wizard-modal .selectize-input .item {
              white-space: normal;
              word-wrap: break-word;
              overflow-wrap: break-word;
              word-break: break-all;
              line-height: 1.3;
            }
            .wizard-modal .mapping-row .selectize-control {
              width: 100% !important;
              max-width: 100% !important;
              min-width: 0 !important;
            }
            .wizard-modal .selectize-dropdown {
              width: 100% !important;
              max-width: 100% !important;
              white-space: normal;
              word-wrap: break-word;
              overflow-wrap: break-word;
              word-break: break-all;
            }
            .wizard-modal .selectize-dropdown .option {
              white-space: normal;
              word-wrap: break-word;
              overflow-wrap: break-word;
              word-break: break-all;
              line-height: 1.3;
              padding: 8px 10px;
            }
          "))
        ),
        # Step 1: File Selection
        div(class = "wizard-step",
          h3("Step 1: Select or Upload File"),
          fluidRow(
            column(6,
              selectInput("wizard_import_type", "Data Type:", 
                choices = c("EPIC Export" = "epic_baseline", "Biobank" = "biobank_data", "Follow-up" = "follow_up"),
                width = "100%")
            ),
            column(6,
              fileInput("wizard_file_upload", "Upload File:",
                accept = c(".csv", ".xlsx"),
                buttonLabel = "Browse...",
                placeholder = "No file selected",
                width = "100%")
            )
          ),
          uiOutput("wizard_sheet_selector"),
          uiOutput("wizard_file_info_ui")
        ),
        
        # Step 2: Detection & Mapping (Dynamic)
        uiOutput("wizard_detection_ui"),
        uiOutput("wizard_mapping_ui"),
        
        # Step 3: Preview & Export
        uiOutput("wizard_preview_ui")
      ),
      footer = tagList(
        modalButton("Close"),
        actionButton("wizard_process_file", "Start Wizard", class = "btn btn-primary")
      ),
      size = "l",
      easyClose = FALSE,
      class = "wizard-modal"
    ))
  })
  
  # ===== WIZARD REACTIVE VALUES =====
  wizard_rv <- reactiveValues(
    file_path = NULL,
    file_name = NULL,
    detection_result = NULL,
    mapping_df = NULL,
    selected_sheet = NULL,
    selected_sheets = NULL,
    available_sheets = NULL,
    template_refresh = 0,
    show_preview = FALSE,
    export_result = NULL
  )
  
  # ===== WIZARD: FILE INFO =====
  output$wizard_file_info_ui <- renderUI({
    req(input$wizard_file_upload)
    
    file_info <- input$wizard_file_upload
    wizard_rv$file_path <- file_info$datapath
    wizard_rv$file_name <- file_info$name

    # Capture Excel sheets for optional selection
    ext <- tolower(tools::file_ext(wizard_rv$file_path))
    if (ext %in% c("xlsx", "xls")) {
      sheets <- tryCatch({
        readxl::excel_sheets(wizard_rv$file_path)
      }, error = function(e) NULL)
      wizard_rv$available_sheets <- sheets
      if (!is.null(sheets) && length(sheets) > 0) {
        wizard_rv$selected_sheet <- sheets[1]
        wizard_rv$selected_sheets <- c(sheets[1])
      } else {
        wizard_rv$selected_sheet <- NULL
        wizard_rv$selected_sheets <- NULL
      }
    } else {
      wizard_rv$available_sheets <- NULL
      wizard_rv$selected_sheet <- NULL
      wizard_rv$selected_sheets <- NULL
    }
    
    tags$div(
      style = "margin-top: 10px; padding: 10px; background: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;",
      p(icon("check-circle"), " File selected: ", tags$strong(file_info$name),
        style = "color: #155724; margin: 0;"),
      p(sprintf("Size: %.1f KB", file_info$size / 1024),
        style = "color: #155724; margin: 3px 0; font-size: 0.9em;")
    )
  })

  # Excel sheet selector (supports multi-sheet Excel imports)
  output$wizard_sheet_selector <- renderUI({
    req(wizard_rv$file_path)
    ext <- tolower(tools::file_ext(wizard_rv$file_path))
    sheets <- wizard_rv$available_sheets
    if (!(ext %in% c("xlsx", "xls"))) return(NULL)
    if (is.null(sheets) || length(sheets) <= 1) return(NULL)

    div(
      style = "margin-top: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 4px; background-color: #f9f9f9;",
      tags$label(
        style = "font-weight: bold; margin-bottom: 10px; display: block;",
        "Select Excel Sheets (multiple allowed):"
      ),
      checkboxGroupInput(
        "wizard_excel_sheets",
        label = NULL,
        choices = setNames(sheets, sheets),
        selected = if (is.null(wizard_rv$selected_sheets)) sheets[1] else wizard_rv$selected_sheets
      ),
      tags$small(
        class = "text-muted",
        "Tip: Select multiple sheets to combine columns from different tabs"
      )
    )
  })

  observeEvent(input$wizard_excel_sheets, {
    if (is.null(input$wizard_excel_sheets) || length(input$wizard_excel_sheets) == 0) return()
    wizard_rv$selected_sheets <- input$wizard_excel_sheets
    wizard_rv$selected_sheet <- input$wizard_excel_sheets[1]
  }, ignoreInit = TRUE)
  
  # ===== WIZARD: PROCESS FILE (DETECTION) =====
  observeEvent(input$wizard_process_file, {
    req(wizard_rv$file_path)
    
    shinyjs::disable("wizard_process_file")
    showNotification("Analyzing file structure...", type = "message", duration = NULL, id = "wizard_analyzing")
    
    tryCatch({
      det <- detect_file_structure(
        wizard_rv$file_path,
        sheet = if (!is.null(wizard_rv$selected_sheets) && length(wizard_rv$selected_sheets) == 1) wizard_rv$selected_sheets[1] else wizard_rv$selected_sheet,
        sheets = if (!is.null(wizard_rv$selected_sheets) && length(wizard_rv$selected_sheets) > 1) wizard_rv$selected_sheets else NULL,
        import_configs = IMPORT_CONFIGS
      )

      # Annotate human-friendly name based on matched type
      if (!is.null(det$structure$matched_type)) {
        mt <- det$structure$matched_type
        det$structure$matched_name <- if (!is.null(IMPORT_CONFIGS[[mt]]$name)) {
          IMPORT_CONFIGS[[mt]]$name
        } else {
          mt
        }
      }

      wizard_rv$detection_result <- det
      removeNotification("wizard_analyzing")
      showNotification("File analysis complete!", type = "message", duration = 2)
    }, error = function(e) {
      removeNotification("wizard_analyzing")
      showNotification(sprintf("Error: %s", e$message), type = "error", duration = 5)
      shinyjs::enable("wizard_process_file")
    })
  })
  
  # ===== WIZARD: DETECTION DISPLAY =====
  output$wizard_detection_ui <- renderUI({
    req(wizard_rv$detection_result)
    
    result <- wizard_rv$detection_result
    confidence_level <- result$confidence$level
    confidence_class <- paste0("confidence-", gsub("_", "-", confidence_level))
    import_type_choices <- setNames(
      names(IMPORT_CONFIGS),
      sapply(IMPORT_CONFIGS, function(x) x$name)
    )
    detected_display_name <- if (!is.null(result$structure$matched_name)) {
      result$structure$matched_name
    } else if (!is.null(result$structure$matched_type)) {
      if (!is.null(IMPORT_CONFIGS[[result$structure$matched_type]]$name)) {
        IMPORT_CONFIGS[[result$structure$matched_type]]$name
      } else {
        result$structure$matched_type
      }
    } else {
      "Onbekend"
    }
    
    div(class = "wizard-step step-complete",
      h3("Step 2: File Detection"),
      fluidRow(
        column(6,
          tags$strong("Detected Type: "),
          tags$br(),
          tags$span(detected_display_name, style = "font-size: 1.1em;")
        ),
        column(6,
          tags$strong("Confidence: "),
          tags$br(),
          tags$span(class = confidence_class, 
            sprintf("%s (%d%%)", toupper(confidence_level), result$confidence$score))
        )
      ),
      hr(),
      p(tags$strong("Columns Detected: "), length(result$structure$col_names)),
      p(tags$strong("Rows Preview: "), result$structure$row_count),
      tags$div(
        style = "max-height: 150px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px; font-size: 0.85em;",
        tags$ul(
          lapply(head(result$structure$col_names, 20), function(col) {
            tags$li(code(col))
          }),
          if (length(result$structure$col_names) > 20) {
            tags$li(sprintf("... and %d more", length(result$structure$col_names) - 20))
          }
        )
      ),
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-radius: 5px;",
        fluidRow(
          column(8,
            selectInput(
              "wizard_override_import_type",
              label = tags$span(icon("edit"), " Override Import Type:"),
              choices = import_type_choices,
              selected = result$structure$matched_type,
              width = "100%"
            )
          ),
          column(4,
            div(
              style = "margin-top: 25px;",
              actionButton(
                "wizard_apply_override",
                "Apply Override",
                class = "btn-warning",
                icon = icon("check")
              )
            )
          )
        ),
        p(
          style = "margin: 5px 0 0 0; font-size: 0.875rem; color: #856404;",
          icon("info-circle"),
          " Gebruik deze optie wanneer de automatische detectie niet klopt of als je een ander importtype wilt afdwingen."
        )
      )
    )
  })

  observeEvent(input$wizard_apply_override, {
    req(wizard_rv$detection_result, input$wizard_override_import_type)

    current_result <- wizard_rv$detection_result
    new_type <- input$wizard_override_import_type
    old_type <- current_result$structure$matched_type

    if (identical(new_type, old_type)) {
      showNotification("Import type is al ingesteld op deze waarde", type = "warning", duration = 4)
      return()
    }

    current_result$structure$matched_type <- new_type
    current_result$structure$matched_name <- if (!is.null(IMPORT_CONFIGS[[new_type]]$name)) {
      IMPORT_CONFIGS[[new_type]]$name
    } else {
      new_type
    }

    wizard_rv$detection_result <- current_result

    if (!is.null(wizard_rv$file_path)) {
      wizard_rv$mapping_df <- create_mapping_table(
        wizard_rv$file_path,
        wizard_rv$detection_result,
        import_configs = IMPORT_CONFIGS
      )
      wizard_rv$transformation_result <- NULL
    }

    showNotification(
      sprintf("Importtype gewijzigd naar: %s", current_result$structure$matched_name),
      type = "message",
      duration = 5
    )
  })

  # Mapping dropdown observers
  observe({
    req(wizard_rv$mapping_df, wizard_rv$detection_result)

    lapply(seq_len(nrow(wizard_rv$mapping_df)), function(i) {
      input_id <- paste0("wizard_map_col_", i)

      observeEvent(input[[input_id]], {
        value <- input[[input_id]]

        wizard_rv$mapping_df$file_column[i] <- value
        wizard_rv$mapping_df$is_mapped[i] <- (value != "")
        wizard_rv$mapping_df$match_type[i] <- ifelse(value == "", "", "manual")

        if (value != "" && value %in% colnames(wizard_rv$detection_result$preview_data)) {
          col_data <- wizard_rv$detection_result$preview_data[[value]]
          valid_data <- col_data[!is.na(col_data)]
          if (length(valid_data) > 0) {
            sample_values <- head(valid_data, 3)
            sample_values <- sapply(sample_values, function(val) {
              val_str <- as.character(val)
              if (grepl("\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}", val_str)) {
                val_str <- sub("\\s+\\d{2}:\\d{2}(:\\d{2})?.*$", "", val_str)
              }
              return(val_str)
            })
            wizard_rv$mapping_df$sample_data[i] <- paste(sample_values, collapse = ", ")
          } else {
            wizard_rv$mapping_df$sample_data[i] <- ""
          }
        } else {
          wizard_rv$mapping_df$sample_data[i] <- ""
        }

        if (wizard_rv$mapping_df$is_required[i] && value != "") {
          config <- get_import_type_config(wizard_rv$detection_result$structure$matched_type)
          col_config <- config$required_columns[[wizard_rv$mapping_df$required_column[i]]]

          if (!is.null(col_config$validation)) {
            is_valid <- col_config$validation(wizard_rv$detection_result$preview_data[[value]])
            wizard_rv$mapping_df$validation[i] <- ifelse(is_valid, "✓", "✗ Validation failed")
          } else {
            wizard_rv$mapping_df$validation[i] <- "✓"
          }
        } else if (value == "") {
          wizard_rv$mapping_df$validation[i] <- ""
        }
      }, ignoreInit = TRUE)
    })
  })
  
  # ===== WIZARD: CREATE MAPPING =====
  observeEvent(wizard_rv$detection_result, {
    req(wizard_rv$detection_result)

    wizard_rv$mapping_df <- create_mapping_table(
      wizard_rv$file_path,
      wizard_rv$detection_result,
      import_configs = IMPORT_CONFIGS
    )
  })
  
  # ===== WIZARD: MAPPING DISPLAY =====
  output$wizard_mapping_ui <- renderUI({
    req(wizard_rv$mapping_df, wizard_rv$detection_result)

    div(class = "wizard-step step-complete",
      h3("Step 3: Column Mapping"),
      p("Select file columns from the dropdowns. Required fields are marked with *."),

      uiOutput("wizard_template_controls"),
      uiOutput("wizard_mapping_rows"),
      uiOutput("wizard_mapping_validation"),

      hr(),
      div(style = "text-align: right;",
        actionButton("wizard_reset_mapping", "Reset to Auto-detect", 
                     class = "btn-warning btn-sm", icon = icon("undo"),
                     style = "margin-right: 10px;"),
        actionButton("wizard_apply_mapping", "Apply Mapping", 
                     class = "btn-secondary", icon = icon("check"),
                     style = "margin-right: 10px;"),
        actionButton("wizard_start_transform", "Transform Data", 
                     class = "btn-success", icon = icon("play"))
      )
    )
  })

  # Template controls UI
  output$wizard_template_controls <- renderUI({
    req(wizard_rv$detection_result)

    import_type <- wizard_rv$detection_result$structure$matched_type
    templates <- list_templates(import_type)
    wizard_rv$template_refresh  # dependency trigger

    if (nrow(templates) > 0) {
      template_choices <- setNames(templates$name, paste0(templates$name, " (", templates$created_at, ")"))
      template_choices <- c("Select a template..." = "", template_choices)
    } else {
      template_choices <- c("Select a template..." = "")
    }

    div(style = "background-color: #e9ecef; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
      fluidRow(
        column(4,
          selectInput("wizard_template_select", 
                      label = tags$span(icon("bookmark"), " Load Template:"),
                      choices = template_choices,
                      selectize = FALSE,
                      width = "100%")
        ),
        column(4,
          tags$label(icon("list"), " Manage Templates", 
                     style = "font-weight: normal; margin-bottom: 5px;"),
          uiOutput("wizard_template_manager")
        ),
        column(4,
          textInput("wizard_template_name", 
                    label = tags$span(icon("save"), " Template Name:"),
                    placeholder = "Enter template name",
                    width = "100%")
        )
      ),
      div(style = "text-align: right; margin-top: 10px;",
        actionButton("wizard_save_template", "Save Template", 
                    class = "btn-primary btn-sm", 
                    icon = icon("save"))
      )
    )
  })
  outputOptions(output, "wizard_template_controls", suspendWhenHidden = FALSE)

  # Template manager UI
  output$wizard_template_manager <- renderUI({
    req(wizard_rv$detection_result)
    wizard_rv$template_refresh

    import_type <- wizard_rv$detection_result$structure$matched_type
    templates <- list_templates(import_type)

    if (nrow(templates) == 0) {
      return(p("No templates saved", style = "color: #6c757d; font-style: italic;"))
    }

    template_items <- lapply(seq_len(nrow(templates)), function(i) {
      template_name <- templates$name[i]
      div(
        style = "margin-bottom: 8px; padding: 5px; background-color: white; border-radius: 3px;",
        span(style = "font-weight: 500;", template_name),
        div(style = "float: right;",
          actionButton(
            paste0("wizard_btn_rename_", i),
            NULL,
            icon = icon("edit"),
            class = "btn-warning btn-xs",
            style = "padding: 2px 6px; font-size: 10px; margin-left: 3px;",
            onclick = sprintf("Shiny.setInputValue('wizard_rename_template', '%s', {priority: 'event'});", template_name)
          ),
          actionButton(
            paste0("wizard_btn_delete_", i),
            NULL,
            icon = icon("trash"),
            class = "btn-danger btn-xs",
            style = "padding: 2px 6px; font-size: 10px; margin-left: 3px;",
            onclick = sprintf("Shiny.setInputValue('wizard_delete_template', '%s', {priority: 'event'});", template_name)
          )
        )
      )
    })

    do.call(div, template_items)
  })

  # Save template
  observeEvent(input$wizard_save_template, {
    req(wizard_rv$mapping_df, wizard_rv$detection_result, input$wizard_template_name)

    if (input$wizard_template_name == "") {
      showNotification("Please enter a template name", type = "warning")
      return()
    }

    import_type <- wizard_rv$detection_result$structure$matched_type

    result <- save_mapping_template(
      template_name = input$wizard_template_name,
      import_type = import_type,
      mapping_df = wizard_rv$mapping_df,
      description = paste("Created:", Sys.time())
    )

    if (result$success) {
      showNotification(result$message, type = "message")
      updateTextInput(session, "wizard_template_name", value = "")
      wizard_rv$template_refresh <- wizard_rv$template_refresh + 1
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # Load template
  observeEvent(input$wizard_template_select, {
    req(input$wizard_template_select, wizard_rv$detection_result, wizard_rv$mapping_df)

    if (input$wizard_template_select == "") return()

    import_type <- wizard_rv$detection_result$structure$matched_type
    template_name <- input$wizard_template_select

    result <- load_mapping_template(template_name, import_type)

    if (result$success) {
      wizard_rv$mapping_df <- apply_template_to_mapping(wizard_rv$mapping_df, result$template)
      showNotification(paste("Template applied:", template_name), type = "message")
      updateSelectInput(session, "wizard_template_select", selected = "")
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # Delete template
  observeEvent(input$wizard_delete_template, {
    req(wizard_rv$detection_result, input$wizard_delete_template)

    import_type <- wizard_rv$detection_result$structure$matched_type
    template_name <- input$wizard_delete_template

    result <- delete_template(template_name, import_type)

    if (result$success) {
      showNotification(result$message, type = "message")
      wizard_rv$template_refresh <- wizard_rv$template_refresh + 1
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # Rename template
  observeEvent(input$wizard_rename_template, {
    req(wizard_rv$detection_result, input$wizard_rename_template)

    import_type <- wizard_rv$detection_result$structure$matched_type
    old_name <- input$wizard_rename_template

    showModal(modalDialog(
      title = paste("Rename Template:", old_name),
      textInput("wizard_new_template_name", "New name:", value = old_name),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("wizard_confirm_rename", "Rename", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$wizard_confirm_rename, {
    req(input$wizard_new_template_name, input$wizard_rename_template, wizard_rv$detection_result)

    import_type <- wizard_rv$detection_result$structure$matched_type
    old_name <- input$wizard_rename_template
    new_name <- input$wizard_new_template_name

    if (new_name == "" || new_name == old_name) {
      showNotification("Please enter a different name", type = "warning")
      return()
    }

    load_result <- load_mapping_template(old_name, import_type)

    if (load_result$success) {
      template <- load_result$template
      temp_mapping_df <- wizard_rv$mapping_df
      temp_mapping_df <- apply_template_to_mapping(temp_mapping_df, template)

      save_result <- save_mapping_template(new_name, import_type, temp_mapping_df, 
                                          description = paste("Renamed from:", old_name))

      if (save_result$success) {
        delete_template(old_name, import_type)
        showNotification(paste("Template renamed to:", new_name), type = "message")
        wizard_rv$template_refresh <- wizard_rv$template_refresh + 1
        removeModal()
      } else {
        showNotification(save_result$message, type = "error")
      }
    } else {
      showNotification(load_result$message, type = "error")
    }
  })

  # Mapping rows UI
  output$wizard_mapping_rows <- renderUI({
    req(wizard_rv$mapping_df, wizard_rv$detection_result)

    available_cols <- c("(not mapped)" = "", wizard_rv$detection_result$structure$col_names)
    mapping_df <- wizard_rv$mapping_df

    mapping_rows <- lapply(seq_len(nrow(mapping_df)), function(i) {
      row <- mapping_df[i, ]

      row_style <- if (row$is_required) {
        "background-color: #fff3cd; padding: 10px; margin-bottom: 5px; border-radius: 3px;"
      } else {
        "background-color: #f8f9fa; padding: 10px; margin-bottom: 5px; border-radius: 3px;"
      }

      status_icon <- if (row$validation == "✓") {
        span(style = "color: green; font-size: 18px;", "✓")
      } else if (row$validation == "✗ Validation failed") {
        span(style = "color: red; font-size: 18px;", "✗")
      } else if (row$is_mapped) {
        span(style = "color: blue; font-size: 18px;", "→")
      } else {
        span(style = "color: gray; font-size: 18px;", "○")
      }

      div(
        class = "mapping-row",
        style = row_style,
        fluidRow(
          column(3,
            div(
              class = "mapping-field-name",
              strong(if (row$is_required) paste0(row$required_column, " *") else row$required_column)
            ),
            div(
              class = "mapping-description",
              style = "color: #6c757d; margin-top: 5px;",
              row$description
            )
          ),
          column(5,
            div(style = "min-width: 0; max-width: 100%; width: 100%;",
              selectInput(
                inputId = paste0("wizard_map_col_", i),
                label = NULL,
                choices = available_cols,
                selected = row$file_column,
                width = "100%"
              )
            )
          ),
          column(3,
            if (row$sample_data != "") {
              div(
                class = "mapping-sample-container",
                tags$small(style = "color: #6c757d;", "Sample:"),
                div(
                  class = "mapping-sample",
                  style = "margin-top: 3px;",
                  row$sample_data
                )
              )
            } else {
              tags$small(style = "color: #adb5bd;", "No data preview")
            }
          ),
          column(1,
            div(style = "text-align: center; padding-top: 10px;", status_icon)
          )
        )
      )
    })

    tagList(mapping_rows)
  })

  # Mapping validation status
  output$wizard_mapping_validation <- renderUI({
    req(wizard_rv$mapping_df)

    validation <- validate_mapping(wizard_rv$mapping_df)
    status_color <- ifelse(validation$is_valid, "#28a745", "#dc3545")
    bg_color <- ifelse(validation$is_valid, "#d4edda", "#f8d7da")

    div(
      style = sprintf("padding: 10px; margin-top: 15px; background-color: %s; border-left: 4px solid %s;", 
                     bg_color, status_color),
      validation$message
    )
  })
  
  # Reset mapping button
  observeEvent(input$wizard_reset_mapping, {
    req(wizard_rv$file_path, wizard_rv$detection_result)

    wizard_rv$mapping_df <- create_mapping_table(
      wizard_rv$file_path,
      wizard_rv$detection_result,
      import_configs = IMPORT_CONFIGS
    )

    showNotification("Mapping reset to auto-detected values", type = "message")
  })

  # Apply mapping button (validate only)
  observeEvent(input$wizard_apply_mapping, {
    req(wizard_rv$mapping_df)
    validation <- validate_mapping(wizard_rv$mapping_df)
    if (!validation$is_valid) {
      showNotification(validation$message, type = "error")
    } else {
      showNotification("Mapping applied", type = "message")
    }
  })

  # Trigger transformation from Step 3
  observeEvent(input$wizard_start_transform, {
    req(wizard_rv$file_path, wizard_rv$detection_result, wizard_rv$mapping_df)

    validation <- validate_mapping(wizard_rv$mapping_df)
    if (!validation$is_valid) {
      showNotification(validation$message, type = "error")
      return()
    }

    showNotification("Transforming data...", type = "message", duration = NULL, id = "wizard_transforming")
    shinyjs::disable("wizard_start_transform")

    tryCatch({
      mapping_updates <- data.frame()
      for (i in seq_len(nrow(wizard_rv$mapping_df))) {
        if (!is.null(input[[paste0("wizard_map_col_", i)]])) {
          wizard_rv$mapping_df$file_column[i] <- input[[paste0("wizard_map_col_", i)]]
          wizard_rv$mapping_df$is_mapped[i] <- (wizard_rv$mapping_df$file_column[i] != "")
          wizard_rv$mapping_df$match_type[i] <- ifelse(wizard_rv$mapping_df$file_column[i] == "", "", "manual")
        }
      }

      transform_result <- transform_import_data(
        wizard_rv$file_path,
        wizard_rv$detection_result,
        wizard_rv$mapping_df,
        sheet = if (!is.null(wizard_rv$selected_sheets) && length(wizard_rv$selected_sheets) == 1) wizard_rv$selected_sheets[1] else wizard_rv$selected_sheet,
        sheets = if (!is.null(wizard_rv$selected_sheets) && length(wizard_rv$selected_sheets) > 1) wizard_rv$selected_sheets else NULL
      )

      removeNotification("wizard_transforming")
      wizard_rv$transformation_result <- transform_result
      wizard_rv$show_preview <- TRUE
      wizard_rv$export_result <- NULL

      is_success <- isTRUE(transform_result$validation$is_valid)
      if (is_success) {
        showNotification("Transformation successful", type = "message", duration = 4)
      } else {
        err_count <- length(transform_result$validation$errors)
        warn_count <- length(transform_result$validation$warnings)
        msg <- sprintf("Transformation completed with %d errors, %d warnings", err_count, warn_count)
        showNotification(msg, type = "warning", duration = 6)
      }

      shinyjs::enable("wizard_start_transform")
    }, error = function(e) {
      removeNotification("wizard_transforming")
      showNotification(sprintf("Error: %s", e$message), type = "error", duration = 5)
      shinyjs::enable("wizard_start_transform")
    })
  })

  # Export transformed data
  observeEvent(input$wizard_export, {
    req(wizard_rv$transformation_result)

    res <- tryCatch({
      export_transformation_result(
        wizard_rv$transformation_result,
        format = "csv"
      )
    }, error = function(e) {
      list(success = FALSE, message = paste("Export error:", e$message))
    })

    wizard_rv$export_result <- res

    if (isTRUE(res$success)) {
      showNotification(
        paste(res$message, "Rows:", res$rows, "| Columns:", res$columns),
        type = "message",
        duration = 8
      )
      
      # Reload option lists to refresh dropdown options with new file data
      cat("[Import Wizard] Reloading option lists after successful export...\n")
      tryCatch({
        options(epic2castor.force_option_reload = TRUE)
        local_env <- new.env(parent = globalenv())
        sys.source(file.path(epc_path("scripts_dir"), "option_lists2.R"), envir = local_env)
        
        # Update global variables with fresh option data
        option_data <<- local_env$option_data
        checkBoxesValues <<- local_env$checkBoxesValues
        radioButtonOptionValues <<- local_env$radioButtonOptionValues
        checkboxes <<- local_env$checkboxes
        radiobuttons <<- local_env$radiobuttons
        metaRadioButtons <<- local_env$metaRadioButtons
        metaVariables <<- local_env$metaVariables
        
        # Force table re-render to update cell validation with new options
        current_file <- isolate(current_table_name())
        current_tab <- isolate(get_active_tab_name())
        if (!is.null(current_file) && !is.null(current_tab)) {
          active_data <- get_active_tab_data()
          if (!is.null(active_data)) {
            render_table(active_data, current_file, mode = "full")
            cat("[Import Wizard] Table re-rendered with updated options\n")
          }
        }
        
        cat("[Import Wizard] Option lists reloaded successfully\n")
      }, error = function(e) {
        cat(sprintf("[Import Wizard] Warning: Failed to reload option lists: %s\n", conditionMessage(e)))
      })
    } else {
      showNotification(res$message, type = "error", duration = 6)
    }
  })

  output$wizard_export_result <- renderUI({
    req(wizard_rv$export_result)
    res <- wizard_rv$export_result

    if (isTRUE(res$success)) {
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #d4edda; border-radius: 5px;",
        p(style = "margin: 0; color: #155724;",
          icon("check-circle"),
          strong(" Export successful!"),
          br(),
          tags$small(
            "File:", if (!is.null(res$path)) tags$code(basename(res$path)) else "",
            if (!is.null(res$rows)) tags$br(),
            if (!is.null(res$rows)) paste("Rows:", res$rows, "| Columns:", res$columns)
          )
        )
      )
    } else {
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #f8d7da; border-radius: 5px;",
        p(style = "margin: 0; color: #721c24;",
          icon("exclamation-circle"),
          strong(" Export failed: "),
          res$message
        )
      )
    }
  })

  # ===== WIZARD: PREVIEW =====
  output$wizard_preview_ui <- renderUI({
    req(wizard_rv$file_path, wizard_rv$detection_result)
    if (!isTRUE(wizard_rv$show_preview)) return(NULL)
    req(wizard_rv$transformation_result)
    
    div(class = "wizard-step",
      h3("Step 4: Preview & Export"),
      p("Review the transformed data below."),
      div(
        style = "background-color: #e7f3ff; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
        fluidRow(
          column(8,
            h4(icon("download"), " Export Data", style = "margin-top: 0; margin-bottom: 5px;"),
            p(style = "margin: 0; color: #6c757d;", 
              tags$small("Export transformed data to CSV (semicolon separated)"))
          ),
          column(4,
            div(style = "text-align: right; margin-top: 10px;",
              actionButton("wizard_export", "Export to CSV", 
                           class = "btn-success", 
                           icon = icon("file-csv"))
            )
          )
        ),
        uiOutput("wizard_export_result")
      ),
      tags$div(
        style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; border-radius: 4px; overflow-x: auto;",
        DT::renderDataTable({
          tryCatch({
            preview_data <- wizard_rv$transformation_result$transformed_data
            head(preview_data, 50)
          }, error = function(e) {
            data.frame("Error" = e$message)
          })
        })
      )
    )
  })
  
  # ===== WIZARD: TRANSFORMATION & EXPORT =====

  # ============================================================================
  # UNDO OBSERVER
  # ============================================================================
  # Reverts all changes by reloading data from the database
  # This discards any unsaved modifications and restores the last saved state
  #
  # Process:
  # 1. Reload all tables from mapping.db database
  # 2. Remove metadata columns from non-selectable tables
  # 3. Restore tabs from metadata for current table
  # 4. Ensure all tabs have proper order
  # 5. Reset active tab to first tab
  # 6. Re-render table with fresh data
  
  observeEvent(input$undo, {
    req(is_selectable_table(input$file))
    
    # Reload all tables from database
    mappingData <<- setNames(
      lapply(table_names, function(tbl) {
        dt <- as.data.table(dbReadTable(con, tbl))
        
        # Remove metadata columns from non-selectable tables
        # (selectable tables: elements, waarde_radiobuttons, waarde_checkboxes)
        if (!is_selectable_table_name(tbl)) {
          meta_cols <- names(dt)[grepl("tab_(name|order)_meta", names(dt))]
          if (length(meta_cols) > 0) {
            dt[, (meta_cols) := NULL]
          }
        }
        
        return(dt)
      }),
      table_names
    )
    
    # Restore tabs from database metadata
    data <- mappingData[[input$file]]
    tabState$tabs <- restore_tabs_from_metadata(data)
    
    # Ensure all tabs have order assigned
    tabState$tabs <- ensure_tab_order(tabState$tabs)
    
    # Reset to first tab
    tabState$activeTab <- tabState$tabs[[1]]$id
    tabState$nextTabId <- length(tabState$tabs) + 1
    
    # Detect duplicates if this is the elements table
    if (input$file == "elements") {
      active_data <- get_active_tab_data()
      dupes <- detect_duplicate_elements(active_data)
      duplicateElements(dupes)
    }
    
    # Re-render table with fresh data from database
    render_table(get_active_tab_data(), input$file)
  })
  
  # ===== DASHBOARD MODULE =====
  # Initialize dashboard server logic (patient inclusion, data completeness, biobank)
  dashboard_server(input, output, session)
  
} # End of server function

# ============================================================================
# APPLICATION LAUNCH
# ============================================================================
# Launch the Shiny application with UI and server components
# The launch.browser option determines whether to open in browser or RStudio viewer
# Set to TRUE for external browser, FALSE for RStudio viewer panel

shinyApp(ui, server, options = list(launch.browser = getOption("shiny.launch.browser", interactive())))
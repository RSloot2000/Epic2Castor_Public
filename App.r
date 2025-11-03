rm(list = ls())
gc()

library(shiny)
library(data.table)
library(DT)
library(shinyjs)
library(shinydashboard)
library(readxl)
library(processx)
library(jsonlite)
library(DBI)
library(RSQLite)
library(digest)
library(readr)
library(uuid)
library(httr)

# Safety wrapper: Capture reactive value safely with isolate()
# Usage: safe_capture(input$myvalue) or safe_capture(myReactive())
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

## Bootstrap: load central paths, logger, and configuration
paths <- tryCatch(
  jsonlite::fromJSON(file.path("config", "paths.json")),
  error = function(e) list(scripts_dir = "scripts",
                           logger_script = file.path("scripts", "Logger.r"),
                           config_script  = file.path("scripts", "config.R"),
                           config_api     = file.path("config", "APIConfig.json"))
)

logger_path <- if (!is.null(paths$logger_script)) paths$logger_script else file.path(paths$scripts_dir, "Logger.r")
source(logger_path)

config_script_path <- if (!is.null(paths$config_script)) paths$config_script else file.path(paths$scripts_dir, "config.R")
source(config_script_path)

## Initialize: ensure required directories and config files exist
files_created <- FALSE
tryCatch({
  cat("[Init] Checking required directories and files...\n")
  
  # Ensure config directory exists
  config_dir <- "config"
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
    cat("[Init] Created config directory\n")
  }
  
  # Ensure db directory exists (required for SQLite databases)
  db_dir <- epc_path("db_dir")
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
    cat("[Init] Created db directory\n")
  }
  
  # Ensure castor_meta directory exists
  castor_meta_dir <- epc_path("castor_meta_dir")
  if (!dir.exists(castor_meta_dir)) {
    dir.create(castor_meta_dir, recursive = TRUE)
    cat("[Init] Created castor_meta directory\n")
  }
  
  # Ensure possibleValues directory exists
  pv_dir <- epc_path("mapping_possible_values_dir")
  if (!dir.exists(pv_dir)) {
    dir.create(pv_dir, recursive = TRUE)
    cat("[Init] Created mapping/possibleValues directory\n")
  }
  
  # Create APIConfig.json template if it doesn't exist
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
  
  # Create placeholder metadata files with proper CSV structure if they don't exist
  fo <- epc_path("castor_field_options_file")
  sv <- epc_path("castor_study_variablelist_file")
  
  if (!file.exists(fo)) {
    # Create valid but empty CSV with expected headers
    fo_headers <- "Option Group Name;Option Name;Option Value;Option Group Id"
    writeLines(fo_headers, fo)
    cat("[Init] Created placeholder field_options.csv with proper structure\n")
  }
  
  if (!file.exists(sv)) {
    # Create valid but empty CSV with expected headers
    sv_headers <- "Form Name;Form Order;Field Option Group;Field Variable Name;Field Type"
    writeLines(sv_headers, sv)
    cat("[Init] Created placeholder study_variablelist.csv with proper structure\n")
  }
  
  # Create placeholder pv_elements.csv if possibleValues directory exists
  pv_dir <- epc_path("mapping_possible_values_dir")
  if (dir.exists(pv_dir)) {
    pv_elements_file <- file.path(pv_dir, "pv_elements.csv")
    if (!file.exists(pv_elements_file)) {
      # Create minimal structure - this will be populated during autofill
      pv_headers <- "epic_kolom;epic_waarde;castor_kolom;castor_waarde"
      writeLines(pv_headers, pv_elements_file)
      cat("[Init] Created placeholder pv_elements.csv\n")
      files_created <- TRUE
    }
  }
  
  # Create placeholder mapping files in mapping directory
  mapping_dir <- epc_path("mapping_dir")
  if (dir.exists(mapping_dir)) {
    # Create elements.csv
    elements_file <- file.path(mapping_dir, "elements.csv")
    if (!file.exists(elements_file)) {
      elements_headers <- "Element;castor_kolom"
      writeLines(elements_headers, elements_file)
      cat("[Init] Created placeholder elements.csv\n")
      files_created <- TRUE
    }
    
    # Create waarde_checkboxes.csv
    checkboxes_file <- file.path(mapping_dir, "waarde_checkboxes.csv")
    if (!file.exists(checkboxes_file)) {
      checkboxes_headers <- "Element;waarde;kolom_toevoeging"
      writeLines(checkboxes_headers, checkboxes_file)
      cat("[Init] Created placeholder waarde_checkboxes.csv\n")
      files_created <- TRUE
    }
    
    # Create waarde_radiobuttons.csv
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
  warning(paste("[Init] Warning during initialization:", conditionMessage(e)))
})

## Startup: fetch Castor metadata via separate Rscript and validate outputs
castor_meta_retrieved <- FALSE
tryCatch({
  start_time <- Sys.time()
  castor_script <- epc_path("castor_retrieval_script")
  if (is.null(castor_script) || !file.exists(castor_script)) {
    stop(sprintf("Castor retrieval script not found: %s", as.character(castor_script)))
  }
  rscript_bin <- if (.Platform$OS.type == "windows") file.path(R.home("bin"), "Rscript.exe") else file.path(R.home("bin"), "Rscript")
  if (!file.exists(rscript_bin)) rscript_bin <- "Rscript"

  fo <- epc_path("castor_field_options_file")
  sv <- epc_path("castor_study_variablelist_file")
  max_age_mins <- getOption("epic2castor.castor_refresh_minutes", 60)
  force_refresh <- isTRUE(getOption("epic2castor.force_retrieval", FALSE)) || identical(Sys.getenv("EPIC2CASTOR_FORCE_RETRIEVAL"), "1")
  info <- tryCatch(file.info(c(fo, sv)), error = function(e) NULL)
  ages_ok <- FALSE
  if (!is.null(info) && nrow(info) == 2 && all(!is.na(info$mtime))) {
    ages <- difftime(Sys.time(), info$mtime, units = "mins")
    ages_ok <- all(ages <= max_age_mins)
  }
  
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
  
  skip_retrieval <- !force_refresh && all(file.exists(c(fo, sv))) && isTRUE(ages_ok)

  if (skip_retrieval) {
    cat("[Startup] Castor metadata recent (<= ", max_age_mins, " min); skipping retrieval.\n", sep = "")
  } else if (!has_credentials) {
    if (file.exists(fo) && file.exists(sv)) {
      cat("[Startup] API credentials not configured yet; using existing Castor metadata files.\n", sep = "")
      cat("[Startup] Please configure API credentials in the app to refresh metadata.\n", sep = "")
    } else {
      cat("[Startup] API credentials not configured and no cached metadata found.\n", sep = "")
      cat("[Startup] The app will start, but Castor functionality will be limited until credentials are configured.\n", sep = "")
    }
  } else {
    cat("[Startup] Retrieving Castor metadata via: ", castor_script, "\n", sep = "")
    # Prepare a done-flag (unique temp path) and pass it to the child process via ENV
    done_flag <- tempfile(pattern = "castor_retrieval_done_", tmpdir = tempdir(), fileext = ".flag")
    if (file.exists(done_flag)) try(file.remove(done_flag), silent = TRUE)
    status <- suppressWarnings(system2(
      rscript_bin,
      c("--vanilla", shQuote(castor_script)),
      stdout = "",   # stream direct naar console/log
      stderr = "",
      wait   = TRUE,
      env    = c(EPIC2CASTOR_DONE = done_flag)
    ))
    if (!is.null(status) && status != 0) {
      if (file.exists(fo) && file.exists(sv)) {
        # Exit 5: interpret as "no changes needed" -> no warning
        if (identical(as.integer(status), 5L)) {
          # continue silently; optionally log an informational message
          # message("[Startup] Castor retrieval: no changes needed (exit 5).")
        } else {
          message(sprintf("[Startup] Castor retrieval returned exit %s, but CSVs already exist; continuing.", status))
        }
      } else {
        stop(sprintf("Castor retrieval failed (exit %s)", status))
      }
    } else {
      # Verify done-flag as an extra safeguard against hanging child; fallback to file timestamps
      if (!file.exists(done_flag)) {
        fo_ok <- file.exists(fo)
        sv_ok <- file.exists(sv)
        recent <- function(p) {
          info <- tryCatch(file.info(p), error = function(e) NULL)
          if (is.null(info) || is.na(info$mtime[1])) return(FALSE)
          difftime(info$mtime[1], start_time, units = "secs") >= -5
        }
        if (!(fo_ok && sv_ok && recent(fo) && recent(sv))) {
          stop("Castor retrieval returned without done-flag and outputs are not recent. Check logs for details.")
        }
      }
      # Mark that metadata was successfully retrieved
      castor_meta_retrieved <- TRUE
    }
    cat("[Startup] Castor metadata retrieval completed.\n")
  }
  flush.console()
}, error = function(e) {
  # Check if this is a credentials-related error - if so, warn but don't stop
  error_msg <- conditionMessage(e)
  if (grepl("Castor retrieval failed \\(exit 5\\)", error_msg, ignore.case = TRUE)) {
    cat("[Startup] Warning: Castor metadata retrieval skipped (credentials may not be configured).\n", sep = "")
    cat("[Startup] The app will start with limited functionality. Configure API credentials to enable full features.\n", sep = "")
  } else {
    # For other errors, try to continue if cached files exist
    fo <- epc_path("castor_field_options_file")
    sv <- epc_path("castor_study_variablelist_file")
    if (file.exists(fo) && file.exists(sv)) {
      cat("[Startup] Warning: Castor metadata retrieval failed, but cached files exist. Continuing with cached data.\n", sep = "")
      cat("[Startup] Error details: ", error_msg, "\n", sep = "")
    } else {
      stop(paste("Castor metadata retrieval failed during startup:", error_msg))
    }
  }
})

## Load database helper functions
source(file.path(epc_path("scripts_dir"), "database.r"))

## Load autofill helper functions  
source(file.path(epc_path("scripts_dir"), "autofill.r"))

## FASE 11.8: Load export functions
source(file.path(epc_path("scripts_dir"), "export_approved.r"))

## Utility: compute a single MD5 hash over all CSV files in a folder (recursively)
compute_csv_hash <- function(dataFolder) {
  csvs <- list.files(dataFolder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  sums <- tools::md5sum(csvs)
  digest(paste(names(sums), sums, sep = ":", collapse = "|"), algo = "md5")
}

## Utility: check and (re)build a database only when underlying CSVs changed
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

validate_castor_outputs <- function(done_flag, start_time, fo, sv) {
  if (file.exists(done_flag)) return(TRUE)
  recent <- function(p) {
    info <- tryCatch(file.info(p), error = function(e) NULL)
    if (is.null(info) || is.na(info$mtime[1])) return(FALSE)
    difftime(info$mtime[1], start_time, units = "secs") >= -5
  }
  fo_ok <- file.exists(fo)
  sv_ok <- file.exists(sv)
  fo_ok && sv_ok && recent(fo) && recent(sv)
}

cat(sprintf("[Startup] (%s) Checking/building mapping database...\n", format(Sys.time(), "%H:%M:%S"))); flush.console()
if (!isTRUE(getOption("epic2castor.mapping_built", FALSE)) || files_created) {
  if (files_created) {
    cat("[Startup] New mapping files were created; forcing database rebuild.\n")
    # Force rebuild by removing hash file
    hash_file <- paste0(epc_path("mapping_db"), ".hash")
    if (file.exists(hash_file)) {
      try(file.remove(hash_file), silent = TRUE)
    }
  }
  check_and_build(
    dataFolder = epc_path("mapping_dir"),
    dbPath     = epc_path("mapping_db"),
    build_fun  = function() csv_to_database(
      dataFolder = epc_path("mapping_dir"),
      dbPath     = epc_path("mapping_db")
    )
  )
  options(epic2castor.mapping_built = TRUE)
} else {
  cat("[Startup] Mapping database already built this session; skipping rebuild.\n")
}
cat(sprintf("[Startup] (%s) Mapping database ready.\n", format(Sys.time(), "%H:%M:%S"))); flush.console()

cat(sprintf("[Startup] (%s) Checking/building Castor meta database...\n", format(Sys.time(), "%H:%M:%S"))); flush.console()
if (!isTRUE(getOption("epic2castor.castor_meta_built", FALSE)) || castor_meta_retrieved) {
  if (castor_meta_retrieved) {
    cat("[Startup] Castor metadata was just retrieved; forcing database rebuild.\n")
    # Force rebuild by removing hash file
    hash_file <- paste0(epc_path("castor_meta_db"), ".hash")
    if (file.exists(hash_file)) {
      try(file.remove(hash_file), silent = TRUE)
    }
  }
  check_and_build(
    dataFolder = epc_path("castor_meta_dir"),
    dbPath     = epc_path("castor_meta_db"),
    build_fun  = function() csv_to_database_meta(
      dataFolder = epc_path("castor_meta_dir"),
      dbPath     = epc_path("castor_meta_db")
    )
  )
  options(epic2castor.castor_meta_built = TRUE)
} else {
  cat("[Startup] Castor meta database already built this session; skipping rebuild.\n")
}
cat(sprintf("[Startup] (%s) Castor meta database ready.\n", format(Sys.time(), "%H:%M:%S"))); flush.console()

cat(sprintf("[Startup] (%s) Generating pv_elements...\n", format(Sys.time(), "%H:%M:%S"))); flush.console()
try({ generate_pv_elements() ; cat(sprintf("[Startup] (%s) pv_elements done.\n", format(Sys.time(), "%H:%M:%S"))) }, silent = TRUE)
flush.console()

## Open mapping database and read mapping tables into memory
dbPath <- epc_path("mapping_db")

cat(sprintf("[Startup] (%s) Loading mapping DB into memory...\n", format(Sys.time(), "%H:%M:%S"))); flush.console()
tmp_con <- dbConnect(SQLite(), dbPath)

table_names <- setdiff(dbListTables(tmp_con), "possibleValues_Elements")

# FASE 6.5: Definieer helper functies VOOR gebruik
hidden_tables <- c("variabelen")

is_selectable_table_name <- function(name) {
  !is.null(name) && !(name %in% hidden_tables)
}

mappingData <- lapply(table_names, function(tbl) {
  dt <- as.data.table(dbReadTable(tmp_con, tbl))
  
  # FASE 6.5: Alle selectable tabellen behouden metadata kolommen
  # Alleen non-selectable tabellen (bijv. variabelen) krijgen metadata verwijderd
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
cat(sprintf("[Startup] (%s) Mapping data loaded.\n", format(Sys.time(), "%H:%M:%S"))); flush.console()

get_selectable_tables <- function() {
  setdiff(names(mappingData), hidden_tables)
}

get_epic_input_files <- function() {
  epic_dir <- epc_path("epic_input_data_dir")
  if (!dir.exists(epic_dir)) {
    return(c("No files found" = ""))
  }
  files <- list.files(epic_dir, pattern = "\\.(csv|xlsx)$", full.names = FALSE, ignore.case = TRUE)
  if (length(files) == 0) {
    return(c("No files found" = ""))
  }
  # Return named vector with display name and file path
  names(files) <- files
  return(files)
}

get_selected_epic_file_path <- function(filename) {
  if (is.null(filename) || filename == "") return(NULL)
  epic_dir <- epc_path("epic_input_data_dir")
  file.path(epic_dir, filename)
}


is_selectable_table <- function(name) {
  !is.null(name) && name %in% get_selectable_tables()
}

## API helper: retrieve Castor sites for the selected study
get_site_choices <- function() {
  config <- fromJSON(epc_path("config_api"))
  study_id <- config$study_id
  base_url <- "https://data.castoredc.com"
  api_base_url <- "https://data.castoredc.com/api"
  client_id <- config$client_id
  client_secret <- config$client_secret

  # Obtain an access token (simple pattern; could be cached)
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
      httr::timeout(15), times = 2
    )
  }, error = function(e) NULL)
  if (is.null(token_response) || status_code(token_response) != 200) {
    stop("Error obtaining access token")
  }
  token_data <- content(token_response, as = "parsed", encoding = "UTF-8")
  access_token <- token_data$access_token

  # Retrieve sites for the configured study
  site_url <- paste0(api_base_url, "/study/", study_id, "/site")
  response <- tryCatch({
    httr::RETRY("GET", site_url, add_headers(Authorization = paste("Bearer", access_token)), httr::timeout(15), times = 2)
  }, error = function(e) NULL)
  if (is.null(response) || status_code(response) != 200) {
    stop("Error retrieving sites")
  }
  sites <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  if ("_embedded" %in% names(sites)) {
    sites_data <- sites[["_embedded"]][["sites"]]
  } else {
    sites_data <- sites
  }
  # Create choices: "site_id - name"
  choices <- sapply(sites_data, function(x) { paste(x$site_id, x$name, sep = " - ") })
  return(choices)
}

source(file.path(epc_path("scripts_dir"), "option_lists2.R"))

reload_castor_metadata <- function() {
  # Force rebuild of Castor meta database by removing hash file
  cat("[CastorRefresh] Rebuilding Castor meta database...\n")
  hash_file <- paste0(epc_path("castor_meta_db"), ".hash")
  if (file.exists(hash_file)) {
    try(file.remove(hash_file), silent = TRUE)
  }
  
  # Rebuild the database using check_and_build to ensure hash file is recreated
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
  
  # Regenerate pv_elements from fresh metadata
  cat("[CastorRefresh] Regenerating pv_elements...\n")
  try({ 
    generate_pv_elements() 
    cat("[CastorRefresh] pv_elements regenerated.\n")
  }, silent = TRUE)
  
  # Force rebuild of mapping database to pick up new pv_elements
  cat("[CastorRefresh] Rebuilding mapping database...\n")
  hash_file_mapping <- paste0(epc_path("mapping_db"), ".hash")
  if (file.exists(hash_file_mapping)) {
    try(file.remove(hash_file_mapping), silent = TRUE)
  }
  
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
  
  # Reload option lists with fresh data
  options(epic2castor.force_option_reload = TRUE)
  local_env <- new.env(parent = globalenv())
  sys.source(file.path(epc_path("scripts_dir"), "option_lists2.R"), envir = local_env)
  option_data <<- local_env$option_data
  checkBoxesValues <<- local_env$checkBoxesValues
  radioButtonOptionValues <<- local_env$radioButtonOptionValues
  checkboxes <<- local_env$checkboxes
  radiobuttons <<- local_env$radiobuttons
  metaRadioButtons <<- local_env$metaRadioButtons
  metaVariables <<- local_env$metaVariables
  
  # Reload mapping data from database
  tmp_con <- dbConnect(SQLite(), dbPath)
  on.exit(dbDisconnect(tmp_con), add = TRUE)
  table_list <- setdiff(dbListTables(tmp_con), "possibleValues_Elements")
  mappingData <<- lapply(table_list, function(tbl) as.data.table(dbReadTable(tmp_con, tbl)))
  names(mappingData) <<- table_list
  table_names <<- table_list
  cat(sprintf("[CastorRefresh] (%s) Mapping data reloaded.\n", format(Sys.time(), "%H:%M:%S")))
  flush.console()
}

## UI: layout, header, controls, and data table
ui <- fluidPage(
  useShinyjs(),
  
  # Loading screen overlay
  div(id = "loading-screen",
      div(class = "loading-container",
          tags$img(src = "img/logo.png", class = "loading-logo", alt = "Loading..."),
          div(id = "loading-text", "Loading")
      )
  ),
  
  div(id = "app",
      tags$head(
          # Favicon links
          tags$link(rel = "icon", type = "image/x-icon", href = "img/favicon.ico"),
          tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "img/favicon-16x16.png"),
          tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "img/favicon-32x32.png"),
          tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "img/apple-touch-icon.png"),
          tags$link(rel = "manifest", href = "img/site.webmanifest"),
          # Scripts and stylesheets
          tags$script(src = "/colResizable-1.6.js"),
          tags$link(rel = "stylesheet", href = "appCSS.css"),
          tags$link(rel = "stylesheet", href = "select2.min.css"),
          tags$script(src = "select2.min.js"),
          tags$script(src = paste0("appJS.js?v=", format(Sys.time(), "%Y%m%d%H%M%S"))),
          tags$script(HTML("$(document).on('click', '.menu-link', function(){ var $dropdown = $(this).closest('.dropdown'); if ($dropdown.length) { setTimeout(function(){ $dropdown.removeClass('open'); }, 100); }});"))
      ),
      div(class = "fixed-header",
          div(class = "header-container",
              div(class = "header-left",
                  div(class = "header-top",
                      div(style = "margin-right: 5px;", selectInput("file", "File", choices = get_selectable_tables())),
                      div(style = "margin-right: 5px; position: relative;",
                          textInput("search", label = "Search", placeholder = "Search...", width = "200px") %>% 
                            tagAppendAttributes(oninput = "Shiny.setInputValue('search', this.value, {priority:'event'})"),
                          div(id = "row_warning_icon", style = "display: none; position: absolute; right: -44px; top: 23px;",
                              uiOutput("row_warning_content")
                          )
                      )
                  ),
                  div(class = "header-bottom",
                    div(class = "menu-bar",
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
                      )
                    )
                  )
              ),
              div(
                img(src = "img/logo.png", alt = "Logo", style = "height: 150px;")
              )
          )
      ),
      mainPanel(
        box(
          title = "", width = 2000, status = "primary",
          div(id = "scrollDiv",style = 'overflow-x: scroll', DT::DTOutput('table'))
        ),
        style = "margin-top: 10px; margin-left: 10px"
      ),
      div(class = "fixed-footer",
          # Eerste rij: New Tab knop links, bestaande controls rechts
          fluidRow(
            style = "margin-bottom: 10px; padding-bottom: 10px; border-bottom: 1px solid #ddd;",
            column(2,
              actionButton("create_tab", "+ New Tab", class = "btn btn-success btn-add-tab", 
                           title = "Add new tab")
            ),
            column(10, style = "text-align: right;",
              tags$div(style = "display: inline-flex; align-items: center; gap: 0;",
                actionButton("add_row", "+", class = "btn btn-footer", 
                             style = "background-color:green; color:white; min-width:40px; height:34px; padding:6px 12px;"),
                actionButton("delete_rows", "-", class = "btn btn-footer", 
                             style = "background-color:red; color:white; min-width:40px; height:34px; padding:6px 12px; margin-right:10px;"),
                # FASE 3: Copy/Cut/Paste knoppen
                actionButton("copy_rows", icon("copy"), class = "btn btn-footer", 
                             style = "background-color:#007bff; color:white; min-width:40px; height:34px; padding:6px 12px;",
                             title = "Copy selected rows"),
                actionButton("cut_rows", icon("cut"), class = "btn btn-footer", 
                             style = "background-color:#ff9800; color:white; min-width:40px; height:34px; padding:6px 12px;",
                             title = "Cut selected rows"),
                actionButton("paste_rows", icon("paste"), class = "btn btn-footer", 
                             style = "background-color:#28a745; color:white; min-width:40px; height:34px; padding:6px 12px;",
                             title = "Paste rows"),
                # FASE 1: Bulk Row Move knop (title wordt dynamisch gezet via observer)
                actionButton("move_rows_bulk", icon("arrows-alt-v"), class = "btn btn-footer", 
                             style = "background-color:#9c27b0; color:white; min-width:40px; height:34px; padding:6px 12px; margin-right:10px;"),
                # AUTOFILL: Auto-Fill EPIC Values knop (alleen voor radiobuttons/checkboxes)
                uiOutput("autofill_button_ui"),
                tags$label("Table Width", style = "margin: 0 0 0 15px; display: inline-block; vertical-align: middle; line-height: 34px;"),
                tags$input(id = "width", type = "range", min = "100", max = "2500", value = "1000",
                           style = "width: 200px; vertical-align: middle; display: inline-block; margin-left: 8px;",
                           oninput = "Shiny.setInputValue('width', this.value, {priority:'event'})")
              )
            )
          ),
          # Tweede rij: Tab navigatie over volledige breedte
          div(class = "tab-navigation-bar",
            div(class = "tab-list-container", style = "text-align: left !important;",
              uiOutput("tab_buttons", inline = TRUE)
            )
          )
      )
  )
)

## Server: reactive state, process runners, observers, and table editing logic
server <- function(input, output, session) {
  runnerState <- reactiveValues(
    proc = NULL,
    observer = NULL,
    kind = NULL,
    canceled = FALSE,
    error_message = NULL,
    error_detected = FALSE,
    missing_file_info = NULL,
    on_complete_handler = NULL
  )
  
  # Check API credentials on startup and show notification if not configured
  observe({
    priority = 1000  # High priority to run early
    
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
    
    if (!has_credentials) {
      # Show a persistent notification about missing credentials
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
        duration = NULL,  # Persistent notification
        closeButton = TRUE,
        type = "warning",
        id = "credentials_warning"
      )
    }
  })
  
  # Tab State Management
  tabState <- reactiveValues(
    tabs = list(),           # Lijst van tab objecten
    activeTab = NULL,        # ID van actieve tab
    nextTabId = 1            # Counter voor tab IDs
  )
  
  # FASE 6.5.2: Track vorige tabel voor tab synchronisatie
  previous_table <- reactiveVal(NULL)
  
  # Track laatst actieve tab naam GLOBAAL (voor alle tabellen die tabs delen)
  # Omdat elements, checkboxes en radiobuttons allemaal dezelfde tab structuur delen
  lastActiveTabName <- reactiveVal(NULL)
  
  # Helper functie - krijg data van actieve tab
  get_active_tab_data <- function() {
    if (is.null(tabState$activeTab) || length(tabState$tabs) == 0) {
      return(NULL)
    }
    active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
    if (length(active_idx) == 0) {
      return(NULL)
    }
    return(tabState$tabs[[active_idx]]$data)
  }
  
  # Helper functie - initialiseer tabs voor een tabel
  initialize_tabs <- function(data) {
    list(list(
      id = "tab_1",
      name = "Main",
      order = 1,
      data = data
    ))
  }
  
  # Helper functie - zorg ervoor dat alle tabs een order hebben
  ensure_tab_order <- function(tabs) {
    for (i in seq_along(tabs)) {
      if (is.null(tabs[[i]]$order) || is.na(tabs[[i]]$order)) {
        tabs[[i]]$order <- i
      }
    }
    return(tabs)
  }
  
  # Helper functie - consolideer alle tabs in één data.table met metadata
  consolidate_tabs_with_metadata <- function(tabs) {
    if (length(tabs) == 0) return(data.table())
    
    # Bind alle tab data samen
    all_data <- rbindlist(lapply(seq_along(tabs), function(i) {
      tab <- tabs[[i]]
      dt <- as.data.table(copy(tab$data))
      
      # Voeg metadata kolommen toe (zonder leading __ om SQLite X prefix te vermijden)
      dt[, tab_name_meta := as.character(tab$name)]
      dt[, tab_order_meta := as.integer(tab$order)]
      
      return(dt)
    }), fill = TRUE)
    
    return(all_data)
  }
  
  # Helper functie - splits geconsolideerde data terug in tabs
  restore_tabs_from_metadata <- function(data) {
    # Check voor metadata kolommen
    has_tab_name <- "tab_name_meta" %in% names(data)
    has_tab_order <- "tab_order_meta" %in% names(data)
    
    if (nrow(data) == 0 || !has_tab_name || !has_tab_order) {
      # Geen metadata, maak één default tab
      data_clean <- copy(data)
      # Verwijder eventuele metadata kolommen
      meta_cols <- names(data_clean)[grepl("tab_(name|order)_meta", names(data_clean))]
      if (length(meta_cols) > 0) {
        data_clean[, (meta_cols) := NULL]
      }
      return(initialize_tabs(data_clean))
    }
    
    # Converteer naar data.table en zorg voor juiste types
    dt <- as.data.table(copy(data))
    
    dt[, tab_name_meta := as.character(tab_name_meta)]
    dt[, tab_order_meta := as.integer(as.character(tab_order_meta))]
    
    # Verkrijg unieke tab combinaties en sorteer op order
    unique_tabs <- unique(dt[, .(tab_name = tab_name_meta, tab_order = tab_order_meta)])
    unique_tabs[, tab_order := as.integer(tab_order)]
    setorder(unique_tabs, tab_order)
    
    # Maak tab objecten
    tabs <- lapply(seq_len(nrow(unique_tabs)), function(i) {
      tab_name <- unique_tabs$tab_name[i]
      tab_order <- unique_tabs$tab_order[i]
      
      # Filter data voor deze tab
      tab_data <- dt[tab_name_meta == tab_name & tab_order_meta == tab_order]
      
      # Verwijder metadata kolommen
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
  
  # FASE 6.5.2: Helper functie - sync tab structuur naar een tabel
  # Voor elements: behoudt alle tabs
  # Voor waarde_radiobuttons/waarde_checkboxes: doet niks (tabs komen via auto-fill)
  sync_tab_structure_to_table <- function(table_name, reference_tabs) {
    # Haal huidige data van de tabel op
    current_data <- mappingData[[table_name]]
    
    # Als de tabel geen metadata heeft, plaats alles op "Main" tab (eerste reference tab)
    if (!("tab_name_meta" %in% names(current_data))) {
      # Data heeft geen tabs, plaats alles op de eerste reference tab
      first_tab <- reference_tabs[[1]]
      current_data[, tab_name_meta := first_tab$name]
      current_data[, tab_order_meta := first_tab$order]
    }
    # FASE 6.5.4: Voor waarde_radiobuttons en waarde_checkboxes doen we NIKS
    # Tabs worden beheerd door updateRadioMapping/updateCheckboxMapping
    # Voor elements behouden we gewoon alle bestaande tabs (geen verwijderen!)
    
    return(current_data)
  }
  
  # FASE 6.5.3: Helper - voeg lege tab toe aan geconsolideerde data
  add_empty_tab_to_consolidated <- function(data, tab_name, tab_order) {
    # Maak een lege rij met de juiste kolommen
    if (nrow(data) == 0) {
      # Als data helemaal leeg is, return lege data.table met metadata
      dt <- data.table()
      dt[, tab_name_meta := character(0)]
      dt[, tab_order_meta := integer(0)]
      return(dt)
    }
    
    # Maak template rij gebaseerd op eerste rij
    empty_row <- data[1, ]
    # Zet alle kolommen (behalve metadata) op NA
    for (col in setdiff(names(empty_row), c("tab_name_meta", "tab_order_meta"))) {
      empty_row[[col]] <- NA
    }
    empty_row$tab_name_meta <- tab_name
    empty_row$tab_order_meta <- tab_order
    
    # Voeg toe aan data
    result <- rbind(data, empty_row, fill = TRUE)
    return(result)
  }
  
  # FASE 6.5.3: Helper - hernoem tab in metadata
  rename_tab_in_metadata <- function(data, old_name, new_name) {
    if (!("tab_name_meta" %in% names(data))) return(data)
    
    data[tab_name_meta == old_name, tab_name_meta := new_name]
    return(data)
  }
  
  # FASE 6.5.3: Helper - verwijder tab uit metadata
  remove_tab_from_metadata <- function(data, tab_name, tab_order) {
    if (!("tab_name_meta" %in% names(data))) return(data)
    
    # Verwijder alle rijen met deze tab
    result <- data[!(tab_name_meta == tab_name & tab_order_meta == tab_order)]
    return(result)
  }
  
  # ============================================================================
  # FASE 2: COPY/CUT/PASTE - HELPER FUNCTIES
  # ============================================================================
  
  # Helper functie - check of copy/cut/paste beschikbaar is voor deze tabel
  # Alleen elements tabel ondersteunt copy/cut/paste
  is_copyable_table <- function(table_name) {
    return(!is.null(table_name) && table_name == "elements")
  }
  
  # Helper functie - krijg naam van actieve tab
  get_active_tab_name <- function() {
    if (is.null(tabState$activeTab) || length(tabState$tabs) == 0) return("Main")
    active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
    if (length(active_idx) == 0) return("Main")
    return(tabState$tabs[[active_idx]]$name)
  }
  
  # Helper functie - krijg order van actieve tab
  get_active_tab_order <- function() {
    if (is.null(tabState$activeTab) || length(tabState$tabs) == 0) return(1)
    active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
    if (length(active_idx) == 0) return(1)
    return(tabState$tabs[[active_idx]]$order)
  }
  
  # Helper functie - haal data op van geselecteerde rijen
  get_selected_rows_data <- function(selected_indices) {
    active_data <- get_active_tab_data()
    if (is.null(active_data) || length(selected_indices) == 0) return(NULL)
    if (max(selected_indices) > nrow(active_data)) return(NULL)
    return(active_data[selected_indices, , drop = FALSE])
  }
  
  # Helper functie - haal gerelateerde checkbox data op voor gegeven elementen
  get_related_checkbox_data <- function(element_values, source_tab_name, source_tab_order) {
    # Haal alle rijen op uit waarde_checkboxes waar Element in element_values zit
    # EN waar tab metadata overeenkomt
    checkbox_data <- mappingData[["waarde_checkboxes"]]
    if (is.null(checkbox_data) || nrow(checkbox_data) == 0) return(NULL)
    if (length(element_values) == 0) return(NULL)
    
    # Check of tab metadata bestaat
    if (!("tab_name_meta" %in% names(checkbox_data))) {
      # Geen tab metadata, filter alleen op Element
      filtered <- checkbox_data[Element %in% element_values]
    } else {
      # Filter op Element EN tab metadata
      filtered <- checkbox_data[Element %in% element_values & 
                                tab_name_meta == source_tab_name & 
                                tab_order_meta == source_tab_order]
    }
    
    if (nrow(filtered) == 0) return(NULL)
    return(filtered)
  }
  
  # Helper functie - haal gerelateerde radiobutton data op voor gegeven elementen
  get_related_radiobutton_data <- function(element_values, source_tab_name, source_tab_order) {
    # Haal alle rijen op uit waarde_radiobuttons waar Element in element_values zit
    radio_data <- mappingData[["waarde_radiobuttons"]]
    if (is.null(radio_data) || nrow(radio_data) == 0) return(NULL)
    if (length(element_values) == 0) return(NULL)
    
    # Check of tab metadata bestaat
    if (!("tab_name_meta" %in% names(radio_data))) {
      # Geen tab metadata, filter alleen op Element
      filtered <- radio_data[Element %in% element_values]
    } else {
      # Filter op Element EN tab metadata
      filtered <- radio_data[Element %in% element_values & 
                             tab_name_meta == source_tab_name & 
                             tab_order_meta == source_tab_order]
    }
    
    if (nrow(filtered) == 0) return(NULL)
    return(filtered)
  }
  
  # Helper functie - leeg het clipboard
  clear_clipboard <- function() {
    clipboardState$data <- NULL
    clipboardState$source_table <- NULL
    clipboardState$source_tab <- NULL
    clipboardState$source_tab_name <- NULL
    clipboardState$source_tab_order <- NULL
    clipboardState$source_row_indices <- NULL
    clipboardState$operation <- NULL
    clipboardState$related_checkboxes <- NULL
    clipboardState$related_radiobuttons <- NULL
    clipboardState$timestamp <- NULL
  }
  
  # ============================================================================
  # FASE 2: BULK ROW MOVE - HELPER FUNCTIES
  # ============================================================================
  
  # Helper functie - check of bulk move beschikbaar is voor deze tabel en selectie
  # Alleen elements tabel ondersteunt bulk move
  # Retourneert lijst met status en eventuele error message
  is_bulk_move_enabled <- function(table_name, selected_indices) {
    # Check of tabel bulk move ondersteunt (zelfde als copy/paste)
    if (is.null(table_name) || table_name != "elements") {
      return(list(
        enabled = FALSE,
        message = "Bulk move is only available for the elements table"
      ))
    }
    
    # Check of er rijen geselecteerd zijn
    if (is.null(selected_indices) || length(selected_indices) == 0) {
      return(list(
        enabled = FALSE,
        message = "No rows selected"
      ))
    }
    
    # Check of selectie geldig is
    active_data <- get_active_tab_data()
    if (is.null(active_data)) {
      return(list(
        enabled = FALSE,
        message = "No active table data found"
      ))
    }
    
    # Check of alle indices binnen bereik zijn
    if (max(selected_indices) > nrow(active_data)) {
      return(list(
        enabled = FALSE,
        message = "Invalid row selection"
      ))
    }
    
    return(list(enabled = TRUE, message = NULL))
  }
  
  # Helper functie - bereken nieuwe posities voor bulk move
  # Input: 
  #   - selected_indices: vector van huidige row indices (gesorteerd)
  #   - target_position: waar de eerste geselecteerde rij naar toe moet
  #   - total_rows: totaal aantal rijen in de tabel
  # Output:
  #   - list met:
  #     - valid: TRUE/FALSE
  #     - message: error message indien niet valid
  #     - new_positions: vector met nieuwe posities voor elke geselecteerde rij
  #     - final_range: character string met nieuwe positie range voor display
  calculate_bulk_move_positions <- function(selected_indices, target_position, total_rows) {
    # Validaties
    if (is.null(selected_indices) || length(selected_indices) == 0) {
      return(list(valid = FALSE, message = "No rows selected"))
    }
    
    if (is.null(target_position) || is.na(target_position)) {
      return(list(valid = FALSE, message = "Invalid target position"))
    }
    
    # Zorg dat selected_indices gesorteerd is
    selected_indices <- sort(unique(selected_indices))
    n_selected <- length(selected_indices)
    
    # Valideer target position
    target_position <- as.integer(target_position)
    if (target_position < 1) target_position <- 1
    if (target_position > total_rows) target_position <- total_rows
    
    # Bereken nieuwe posities
    # De geselecteerde rijen worden als blok verplaatst naar target_position
    # Ze behouden hun onderlinge volgorde
    new_positions <- target_position:(target_position + n_selected - 1)
    
    # Zorg dat we niet buiten de grenzen gaan
    if (max(new_positions) > total_rows) {
      # Pas aan zodat laatste rij op positie total_rows eindigt
      new_positions <- (total_rows - n_selected + 1):total_rows
      target_position <- new_positions[1]
    }
    
    # Maak een display string voor de nieuwe range
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
  
  # Helper functie - voer bulk move uit op data.table
  # Input:
  #   - data: data.table om te herordenen
  #   - selected_indices: vector van huidige row indices (gesorteerd)
  #   - target_position: waar de eerste rij naar toe moet (al gevalideerd)
  # Output:
  #   - Herordende data.table
  perform_bulk_move <- function(data, selected_indices, target_position) {
    if (is.null(data) || nrow(data) == 0) return(data)
    if (is.null(selected_indices) || length(selected_indices) == 0) return(data)
    
    # Zorg dat selected_indices gesorteerd is en uniek
    selected_indices <- sort(unique(selected_indices))
    n_selected <- length(selected_indices)
    total_rows <- nrow(data)
    
    # Valideer target position
    target_position <- as.integer(target_position)
    if (target_position < 1) target_position <- 1
    if (target_position > total_rows) target_position <- total_rows
    
    # Als target positie het maximum overschrijdt, pas aan
    if (target_position + n_selected - 1 > total_rows) {
      target_position <- total_rows - n_selected + 1
    }
    
    # Algoritme:
    # 1. Extract de geselecteerde rijen (in volgorde)
    # 2. Verwijder de geselecteerde rijen uit de data
    # 3. Insert de rijen op de target positie
    
    # Stap 1: Extract geselecteerde rijen (behoud volgorde)
    selected_rows <- data[selected_indices, ]
    
    # Stap 2: Verwijder geselecteerde rijen
    remaining_data <- data[-selected_indices, ]
    
    # Stap 3: Bereken aangepaste target position
    # Als we rijen verwijderen die vóór de target position staan,
    # moeten we de target position aanpassen
    rows_before_target <- sum(selected_indices < target_position)
    adjusted_target <- target_position - rows_before_target
    
    # Zorg dat adjusted_target geldig blijft
    if (adjusted_target < 1) adjusted_target <- 1
    if (adjusted_target > nrow(remaining_data) + 1) adjusted_target <- nrow(remaining_data) + 1
    
    # Stap 4: Insert geselecteerde rijen op nieuwe positie
    if (adjusted_target == 1) {
      # Insert aan het begin
      result <- rbind(selected_rows, remaining_data)
    } else if (adjusted_target > nrow(remaining_data)) {
      # Insert aan het einde
      result <- rbind(remaining_data, selected_rows)
    } else {
      # Insert in het midden
      top_part <- remaining_data[1:(adjusted_target - 1), ]
      bottom_part <- remaining_data[adjusted_target:nrow(remaining_data), ]
      result <- rbind(top_part, selected_rows, bottom_part)
    }
    
    return(result)
  }
  
  # ============================================================================
  # FASE 2: CLIPBOARD STATE
  # ============================================================================
  
  # Clipboard state voor copy/cut/paste operaties
  clipboardState <- reactiveValues(
    data = NULL,                    # Data.table met gekopieerde rijen
    source_table = NULL,            # "elements", "waarde_checkboxes", "waarde_radiobuttons"
    source_tab = NULL,              # ID van bron tab (bijv. "tab_1")
    source_tab_name = NULL,         # Naam van bron tab (bijv. "Main")
    source_tab_order = NULL,        # Order van bron tab (bijv. 1)
    source_row_indices = NULL,      # Integer vector met originele row indices (voor cut)
    operation = NULL,               # "copy" of "cut"
    related_checkboxes = NULL,      # Data.table met gerelateerde checkboxes (indien elements)
    related_radiobuttons = NULL,    # Data.table met gerelateerde radiobuttons (indien elements)
    timestamp = NULL                # Timestamp van copy/cut operatie
  )
  
  # ============================================================================
  # FASE 2: BULK MOVE STATE
  # ============================================================================
  
  # Bulk Move state voor bulk row move operaties
  bulkMoveState <- reactiveValues(
    pending = FALSE,                # Of er een bulk move actie pending is
    selected_indices = NULL,        # Oorspronkelijke geselecteerde indices
    target_position = NULL,         # Doel positie voor eerste rij
    preview_data = NULL,            # Preview van nieuwe volgorde
    total_rows = NULL               # Totaal aantal rijen in tabel
  )
  
  # Trigger om related data updates te forceren (voor reactivity timing fix)
  relatedDataUpdated <- reactiveVal(0)
  
  # Trigger om tabel UI refresh te forceren na data wijzigingen
  forceTableRefresh <- reactiveVal(0)
  
  # Render tab buttons in footer
  output$tab_buttons <- renderUI({
    tabs <- tabState$tabs
    if (length(tabs) == 0) return(NULL)
    
    tab_buttons <- lapply(seq_along(tabs), function(i) {
      tab <- tabs[[i]]
      is_active <- identical(tab$id, tabState$activeTab)
      row_count <- nrow(tab$data)
      
      tags$button(
        class = paste("tab-button", if(is_active) "active" else ""),
        `data-tab-id` = tab$id,
        # Remove onclick from button - will be handled by JavaScript
        
        # Tab name (double-click handled by JavaScript event delegation)
        tags$span(class = "tab-name", tab$name),
        tags$span(class = "tab-row-count", sprintf("(%d)", row_count)),
        
        # Close button (alleen tonen als er meer dan 1 tab is)
        if (length(tabs) > 1) {
          tags$span(
            class = "tab-close-btn",
            onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('close_tab', '%s', {priority: 'event'})", tab$id),
            "×"
          )
        } else NULL
      )
    })
    
    # Return as div with explicit left alignment
    tags$div(style = "display: flex; gap: 2px; justify-content: flex-start; text-align: left;", 
             tab_buttons)
  })

  # ============================================================================
  # LOADING SCREEN: Hide when app is ready
  # ============================================================================
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
  
  # Validate the file as soon as it's selected
  file_validation_result <- reactiveVal(NULL)
  
  # Observe file_validation_result to enable/disable button
  observeEvent(file_validation_result(), {
    result <- file_validation_result()
    if (is.null(result) || isFALSE(result)) {
      shinyjs::disable("confirm_upload_missing_file")
    } else if (isTRUE(result)) {
      shinyjs::enable("confirm_upload_missing_file")
    }
  }, ignoreNULL = FALSE)
  
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
        
        # Determine expected columns based on epic_tabel from mapping
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
  
  # Handle the upload of the missing file
  observeEvent(input$confirm_upload_missing_file, {
    req(input$upload_missing_file, runnerState$missing_file_info, file_validation_result())
    
    # Validatie is al gedaan, alleen kopiëren als valid
    if (!isTRUE(file_validation_result())) {
      return()
    }
    
    info <- runnerState$missing_file_info
    uploaded_file <- input$upload_missing_file
    
    # Kopieer het bestand naar de juiste locatie
    destination_dir <- dirname(info$expected_path)
    if (!dir.exists(destination_dir)) {
      dir.create(destination_dir, recursive = TRUE)
    }
    
    file.copy(uploaded_file$datapath, info$expected_path, overwrite = TRUE)
      
      # Close the upload modal immediately
      removeModalSafe()
      
      # Isolate reactive values before entering later() callback
      old_observer <- isolate(runnerState$observer)
      old_proc <- isolate(runnerState$proc)
      
      # Start the script with a slight delay to allow modal to close
      later::later(function() {
        # Stop the old observer if it's still running
        if (!is.null(old_observer)) {
          tryCatch({
            old_observer$destroy()
          }, error = function(e) {})
          runnerState$observer <- NULL
        }
        
        # Kill the old process if it's still alive
        if (!is.null(old_proc)) {
          tryCatch({
            if (old_proc$is_alive()) {
              old_proc$kill()
            }
          }, error = function(e) {})
          runnerState$proc <- NULL
        }
        
        # Replace the modal content with the running script modal FIRST
        if (!is.null(info$script_type)) {
            if (info$script_type == "baseline") {
              # Replace modal with baseline running modal
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
              
              # Start the baseline script (without removeModalSafe/showModalSafe)
              updateRunnerFooter(FALSE)
              runnerState$canceled <- FALSE
              {
                run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
                status_path <- if (nzchar(run_dir)) file.path(run_dir, "status.json") else "status.json"
                if (file.exists(status_path)) try(file.remove(status_path), silent = TRUE)
              }
              
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
                  if (length(out_lines)) lapply(out_lines, function(x) cat(paste0("[Baseline][OUT] ", x, "\n")))
                  if (length(err_lines)) lapply(err_lines, function(x) cat(paste0("[Baseline][ERR] ", x, "\n")))
                  combined_lines <- c(out_lines, err_lines)
                  if (length(combined_lines)) {
                    err_line <- detect_error_line(combined_lines)
                    if (!is.null(err_line)) update_runner_error(err_line, "baseline_status_line", "baseline_progressbar")
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
                        output$baseline_progressbar <- renderUI({
                          div(class = "progress", style = "height: 20px; background:#eee;",
                              div(class = "progress-bar progress-bar-striped active", role = "progressbar",
                                  style = paste0("width: ", pct, "%;"),
                                  `aria-valuemin` = 0, `aria-valuemax` = 100,
                                  paste0(round(pct), "%"))
                          )
                        })
                      }
                      line <- paste(step, if (nzchar(detail)) paste("-", detail) else "", if (!is.null(eta_txt)) paste("(", eta_txt, ")") else "")
                      if (!runnerState$error_detected) output$baseline_status_line <- renderText({ line })
                    }
                  }
                  
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
                    
                    # Call on_complete if script completed successfully OR exit status is 0
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
  table_initialized <- reactiveVal(FALSE)
  current_table_name <- reactiveVal(NULL)
  showModalSafe <- function(ui, size = NULL, easyClose = FALSE, footer = NULL) {
    if (is.null(session) || is.null(session$sendCustomMessage)) return(invisible(NULL))
    tryCatch({
      showModal(ui, session = session)
    }, error = function(e) {
      message("showModalSafe fallback: ", conditionMessage(e))
    })
  }
  removeModalSafe <- function() {
    if (is.null(session) || is.null(session$sendCustomMessage)) return(invisible(NULL))
    tryCatch(removeModal(session = session), error = function(e) {
      message("removeModalSafe fallback: ", conditionMessage(e))
    })
  }
  safeNotify <- function(message, type = c("default","message","warning","error")) {
    type <- match.arg(type)
    if (!is.null(session) && is.function(session$sendCustomMessage) && is.function(shiny::showNotification)) {
      tryCatch({ shiny::showNotification(message, type = ifelse(type=="default","message", type)) },
               error = function(e) { message("safeNotify fallback: ", conditionMessage(e), " -> ", message) })
    } else {
      message(sprintf("[Notification][%s] %s", type, message))
    }
  }
  stop_runner <- function(detail = NULL, severity = "INFO", status_step = "canceled") {
    proc <- isolate(runnerState$proc)
    obs  <- isolate(runnerState$observer)
    if (!is.null(obs)) {
      try(obs$destroy(), silent = TRUE)
      runnerState$observer <- NULL
    }
    if (!is.null(proc)) {
      try({
        if (isTRUE(proc$is_alive())) proc$kill(tree = TRUE)
      }, silent = TRUE)
      runnerState$proc <- NULL
    }
    kind_txt <- isolate(runnerState$kind)
    if (!is.null(kind_txt) && nzchar(kind_txt) && !is.null(detail)) {
      detail_txt <- paste0(detail, " (", kind_txt, ")")
      try(epic2castor_status_update(step = status_step,
                                    detail = detail_txt,
                                    severity = severity,
                                    force = TRUE), silent = TRUE)
      try(epic2castor_status_done(detail = detail_txt, severity = severity), silent = TRUE)
    }
    runnerState$kind <- NULL
    runnerState$error_message <- NULL
    runnerState$error_detected <- FALSE
    try(removeModalSafe(), silent = TRUE)
    try(shinyjs::enable("refresh_castor"), silent = TRUE)
  }
  currentEdit <- reactiveValues(row = NULL, col = NULL, orig = NULL)
  manualValues <- reactiveValues(vals = list())
  con <- dbConnect(SQLite(), dbPath)
  update_runner_state <- function(kind, proc, observer = NULL) {
    runnerState$kind <- kind
    runnerState$proc <- proc
    runnerState$observer <- observer
  }
  session$onSessionEnded(function() {
    runnerState$canceled <- TRUE
    stop_runner(detail = "Session ended", severity = "WARN")
    try(dbDisconnect(con), silent = TRUE)
    try(stopApp(), silent = TRUE)
  })

  runnerCloseVisible <- reactiveVal(FALSE)
  updateRunnerFooter <- function(show_close) {
    runnerCloseVisible(isTRUE(show_close))
    if (isTRUE(show_close)) {
      output$runner_footer <- renderUI({
        tagList(actionButton("close_modal", "Close", class = "btn btn-primary"))
      })
    } else {
      output$runner_footer <- renderUI({
        tagList(actionButton("cancel_modal", "Cancel", class = "btn btn-danger"))
      })
    }
    outputOptions(output, "runner_footer", suspendWhenHidden = FALSE)
  }
  updateRunnerFooter(FALSE)

  observe({
    updateSelectInput(session, "file", choices = get_selectable_tables())
  })
  
  # FASE 3: Tab switching
  observeEvent(input$switch_tab, {
    req(input$switch_tab)
    new_tab_id <- input$switch_tab
    
    # Check of tab bestaat
    tab_exists <- any(sapply(tabState$tabs, function(t) t$id == new_tab_id))
    if (!tab_exists) {
      return()
    }
    
    # Check of we al op deze tab zijn
    if (identical(tabState$activeTab, new_tab_id)) {
      return()
    }
    
    old_tab_id <- tabState$activeTab
    
    # Clear checkbox selection bij tab switch
    session$sendCustomMessage("clearCheckboxes", list())
    
    # Update active tab
    tabState$activeTab <- new_tab_id
    
    # Onthoud welke tab actief is GLOBAAL (voor alle tabellen)
    if (!is.null(input$file)) {
      # Zoek de tab naam op basis van tab ID
      tab_idx <- which(sapply(tabState$tabs, function(t) t$id == new_tab_id))
      if (length(tab_idx) > 0) {
        active_tab_info <- tabState$tabs[[tab_idx]]
        if (!is.null(active_tab_info)) {
          lastActiveTabName(active_tab_info$name)
        }
      }
    }
    
    # Render data van nieuwe tab
    active_data <- get_active_tab_data()
    if (!is.null(active_data)) {
      render_table(active_data, input$file, mode = "proxy")
    }
  })
  
  # FASE 4: Create new tab
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
  
  # FASE 4: Confirm create new tab
  observeEvent(input$confirm_create_tab, {
    req(input$new_tab_name)
    
    new_name <- trimws(input$new_tab_name)
    
    # Validatie: lege naam
    if (new_name == "") {
      showNotification("Tab name cannot be empty", type = "error")
      return()
    }
    
    # Validatie: max lengte
    if (nchar(new_name) > 50) {
      showNotification("Tab name too long (max 50 characters)", type = "error")
      return()
    }
    
    # Validatie: naam bestaat al (case-insensitive)
    existing_names <- tolower(sapply(tabState$tabs, function(t) t$name))
    if (tolower(new_name) %in% existing_names) {
      showNotification("A tab with this name already exists", type = "error")
      return()
    }
    
    # Maak nieuwe tab data
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
    
    # Voeg tab toe
    new_tab_id <- sprintf("tab_%d", tabState$nextTabId)
    tabState$nextTabId <- tabState$nextTabId + 1
    
    # Bepaal de order voor de nieuwe tab (hoogste huidige order + 1)
    max_order <- max(sapply(tabState$tabs, function(t) if(is.null(t$order)) 0 else t$order))
    
    new_tab <- list(
      id = new_tab_id,
      name = new_name,
      order = max_order + 1,
      data = new_data
    )
    
    tabState$tabs <- c(tabState$tabs, list(new_tab))
    
    # FASE 6.5.4: Sync nieuwe tab ALLEEN naar de huidige tabel (niet naar radiobuttons/checkboxes)
    # Radiobuttons/checkboxes tabs verschijnen automatisch via updateRadioMapping/updateCheckboxMapping
    if (is_selectable_table(input$file)) {
      mappingData[[input$file]] <<- add_empty_tab_to_consolidated(
        mappingData[[input$file]], 
        new_name, 
        max_order + 1
      )
    }
    
    # Switch naar nieuwe tab (alleen als checkbox aangevinkt is)
    if (isTRUE(input$switch_to_new_tab)) {
      tabState$activeTab <- new_tab_id
      render_table(new_data, input$file, mode = "proxy")
    } else {
      # Blijf op huidige tab, maar re-render om nieuwe tab button te tonen
      render_table(get_active_tab_data(), input$file, mode = "proxy")
    }
    
    removeModal()
    showNotification(sprintf("Tab '%s' created", new_name), type = "message")
  })
  
  # FASE 4: Close tab
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
  
  # FASE 4: Confirm close tab
  observeEvent(input$confirm_close_tab, {
    tab_id_to_close <- session$userData$tab_to_close
    req(tab_id_to_close)
    
    # Validatie: blokkeer verwijderen van laatste tab
    if (length(tabState$tabs) <= 1) {
      showNotification("Cannot delete the last tab", type = "error")
      removeModal()
      return()
    }
    
    # Zoek de tab index
    tab_index <- which(sapply(tabState$tabs, function(t) t$id == tab_id_to_close))
    if (length(tab_index) == 0) {
      removeModal()
      return()
    }
    
    # Als we de actieve tab sluiten, switch naar een andere tab
    if (identical(tabState$activeTab, tab_id_to_close)) {
      # Switch naar eerste andere tab
      other_tabs <- tabState$tabs[-tab_index]
      if (length(other_tabs) > 0) {
        new_active_id <- other_tabs[[1]]$id
        tabState$activeTab <- new_active_id
        
        # Render nieuwe actieve tab
        render_table(other_tabs[[1]]$data, input$file, mode = "proxy")
      }
    }
    
    # FASE 6.5.3: Onthoud tab info voor sync
    tab_to_delete <- tabState$tabs[[tab_index]]
    
    # Verwijder de tab
    tabState$tabs <- tabState$tabs[-tab_index]
    
    # FASE 6.5.3: Sync delete naar ALLE selectable tabellen
    for (tbl in get_selectable_tables()) {
      mappingData[[tbl]] <<- remove_tab_from_metadata(
        mappingData[[tbl]], 
        tab_to_delete$name, 
        tab_to_delete$order
      )
    }
    
    removeModal()
    showNotification("Tab closed", type = "message")
    
    # Clean up
    session$userData$tab_to_close <- NULL
  })
  
  # FASE 5: Rename tab
  observeEvent(input$rename_tab, {
    req(input$rename_tab)
    
    tab_id <- input$rename_tab$tab_id
    new_name <- trimws(input$rename_tab$new_name)
    
    # Validatie: lege naam
    if (new_name == "") {
      showNotification("Tab name cannot be empty", type = "error")
      return()
    }
    
    # Validatie: max lengte
    if (nchar(new_name) > 50) {
      showNotification("Tab name too long (max 50 characters)", type = "error")
      return()
    }
    
    # Validatie: naam bestaat al (case-insensitive, behalve huidige tab)
    tab_index <- which(sapply(tabState$tabs, function(t) t$id == tab_id))
    if (length(tab_index) > 0) {
      other_names <- tolower(sapply(tabState$tabs[-tab_index], function(t) t$name))
      if (tolower(new_name) %in% other_names) {
        showNotification("A tab with this name already exists", type = "error")
        return()
      }
    }
    
    # Find tab and update name
    tab_index <- which(sapply(tabState$tabs, function(t) t$id == tab_id))
    if (length(tab_index) > 0) {
      old_name <- tabState$tabs[[tab_index]]$name
      tabState$tabs[[tab_index]]$name <- new_name
      
      # FASE 6.5.3: Sync rename naar ALLE selectable tabellen
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
  
  observe({
    session$sendCustomMessage("DT_search", list(key = input$search))
  })
  
  observe({
    width <- paste0(input$width, "px")
    height <- 700
    session$sendCustomMessage(type = "resizeDiv", message = list(width = width, height = height))
  })
  
  observeEvent(input$file, {
    req(is_selectable_table(input$file))
    
    # Clear checkbox selection bij tabel switch
    session$sendCustomMessage("clearCheckboxes", list())
    
    # Reset tab click state in JavaScript to prevent stale tab click tracking
    session$sendCustomMessage("resetTabClickState", list())
    
    # Haal prev_table op VOORDAT we iets doen
    prev_table <- isolate(previous_table())
    
    # NIEUWE CODE: Onthoud de actieve tab naam GLOBAAL voordat we switchen
    if (!is.null(prev_table) && !is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      # Zoek de huidige actieve tab naam
      active_tab_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_tab_idx) > 0) {
        active_tab_name <- tabState$tabs[[active_tab_idx]]$name
        lastActiveTabName(active_tab_name)
      }
    }
    
    # FASE 6.5.2: Sla tabs van VORIGE tabel op
    if (!is.null(prev_table) && is_selectable_table(prev_table) && length(tabState$tabs) > 0) {
      # FASE 6.5.4: Consolideer ALLEEN voor elements
      # Voor radiobuttons/checkboxes: hun data wordt beheerd door auto-fill functies
      if (prev_table == "elements") {
        # Consolideer huidige tabs en sla op in mappingData
        mappingData[[prev_table]] <<- consolidate_tabs_with_metadata(tabState$tabs)
        
        # LET OP: Roep updateCheckboxMapping/updateRadioMapping NIET meer aan hier!
        # Dit veroorzaakt een Shiny reactivity timing probleem waarbij de functies
        # verouderde data lezen (voordat paste operaties zijn verwerkt).
        # De copy/cut/paste functionaliteit past de related data direct aan met correcte waarden.
        # Deze functies worden alleen aangeroepen bij andere acties zoals data import.
      }
      # Als prev_table radiobuttons of checkboxes is: doe niks, hun data blijft zoals het is
    }
    
    # REACTIVITY FIX: Forceer refresh van related data updates
    # BELANGRIJK: Check trigger VOORDAT we data ophalen
    trigger_value <- if (input$file %in% c("waarde_radiobuttons", "waarde_checkboxes")) {
      relatedDataUpdated()  # Read the reactive value to create dependency
    } else {
      0
    }
    
    # Laad data voor NIEUWE tabel
    # Voor radiobuttons/checkboxes: gebruik isolate om mapping data te lezen
    # maar alleen als trigger > 0 (na een paste operatie)
    if (input$file %in% c("waarde_radiobuttons", "waarde_checkboxes")) {
      if (trigger_value > 0) {
        # NA een paste: forceer verse data lezen
        # Wacht kort om reactive flush te laten gebeuren
        Sys.sleep(0.1)
        
        # Lees data met isolate (verse read)
        data <- isolate(mappingData[[input$file]])
      } else {
        # Geen recente paste: normale read
        data <- mappingData[[input$file]]
      }
    } else {
      # Voor andere tabellen: gewone reactive read
      data <- mappingData[[input$file]]
    }
    
    # FASE 6.5.2: Herstel tabs vanuit metadata
    tabState$tabs <- restore_tabs_from_metadata(data)
    tabState$tabs <- ensure_tab_order(tabState$tabs)
    
    # FASE 6.5.4: Als we naar radiobuttons/checkboxes gaan, gebruik elements' tabs als master
    # Want elements bepaalt welke tabs er zijn, MAAR met data van de huidige tabel
    if (input$file %in% c("waarde_radiobuttons", "waarde_checkboxes")) {
      elements_data <- mappingData[["elements"]]
      if ("tab_name_meta" %in% names(elements_data)) {
        elements_tabs <- restore_tabs_from_metadata(elements_data)
        elements_tabs <- ensure_tab_order(elements_tabs)
        
        # KRITIEK: Vervang de data in elements_tabs met data van de huidige tabel
        # EN verwijder tabs die geen data hebben
        tabs_with_data <- list()
        
        for (i in seq_along(elements_tabs)) {
          tab_name <- elements_tabs[[i]]$name
          tab_order <- elements_tabs[[i]]$order
          
          # Filter data voor deze specifieke tab
          if ("tab_name_meta" %in% names(data)) {
            tab_data <- data[tab_name_meta == tab_name & tab_order_meta == tab_order]
          } else {
            # Als er geen metadata is, gebruik alle data voor de eerste tab
            if (i == 1) {
              tab_data <- data
            } else {
              tab_data <- data[0]  # Lege data.table
            }
          }
          
          # ALLEEN toevoegen als er daadwerkelijk data is (meer dan 0 rijen)
          if (nrow(tab_data) > 0) {
            # Vervang de data in de tab
            elements_tabs[[i]]$data <- tab_data
            tabs_with_data[[length(tabs_with_data) + 1]] <- elements_tabs[[i]]
          }
        }
        
        # Gebruik alleen tabs die daadwerkelijk data hebben
        # Als er geen tabs met data zijn, maak dan één lege tab
        if (length(tabs_with_data) > 0) {
          tabState$tabs <- tabs_with_data
        } else {
          # Geen data: maak één lege "Main" tab
          tabState$tabs <- list(list(
            id = "tab_1",
            name = "Main",
            order = 1,
            data = data[0]  # Lege data.table
          ))
        }
      }
    }
    
    # Selecteer de actieve tab: probeer de laatst actieve tab naam te herstellen
    last_active_tab_name <- isolate(lastActiveTabName())
    
    if (!is.null(last_active_tab_name) && length(tabState$tabs) > 0) {
      # Zoek de tab met deze naam
      matching_tab_idx <- which(sapply(tabState$tabs, function(t) t$name == last_active_tab_name))
      if (length(matching_tab_idx) > 0) {
        # Gevonden! Gebruik deze tab
        tabState$activeTab <- tabState$tabs[[matching_tab_idx[1]]]$id
      } else {
        # Niet gevonden (bijv. tab is verwijderd), gebruik eerste tab
        tabState$activeTab <- tabState$tabs[[1]]$id
      }
    } else {
      # Geen vorige tab bekend, gebruik eerste tab
      tabState$activeTab <- tabState$tabs[[1]]$id
    }
    
    tabState$nextTabId <- length(tabState$tabs) + 1
    
    # Update previous_table tracker
    previous_table(input$file)
    
    render_table(get_active_tab_data(), input$file)
  })
  
  # Observer: Force tabel refresh wanneer forceTableRefresh wordt getriggerd
  observeEvent(forceTableRefresh(), {
    req(forceTableRefresh() > 0)  # Skip initiële waarde
    req(input$file)
    
    # Haal huidige tab data op
    current_data <- get_active_tab_data()
    
    # Forceer volledige tabel re-render
    render_table(current_data, input$file, mode = "full")
    
    cat(sprintf("[DEBUG] Forced table refresh for %s (trigger=%d)\n", 
                input$file, forceTableRefresh()))
  })
  
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
    
    castor_script <- epc_path("castor_retrieval_script")
    if (is.null(castor_script) || !file.exists(castor_script)) {
      safeNotify(sprintf("Castor retrieval script not found: %s", as.character(castor_script)), "error")
      return()
    }
    rscript_bin <- if (.Platform$OS.type == "windows") file.path(R.home("bin"), "Rscript.exe") else file.path(R.home("bin"), "Rscript")
    if (!file.exists(rscript_bin)) rscript_bin <- "Rscript"

    shinyjs::disable("refresh_castor")
    runnerState$canceled <- FALSE
  runnerState$error_detected <- FALSE
  runnerState$error_message <- NULL
    updateRunnerFooter(FALSE)
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

    output$castor_status_line <- renderText({ "Starting retrieval…" })
  outputOptions(output, "castor_status_line", suspendWhenHidden = FALSE)
    output$castor_progressbar <- renderUI({
      div(class = "progress", style = "height: 20px; background:#eee;",
          div(class = "progress-bar progress-bar-striped active", role = "progressbar",
              style = "width: 100%;",
              `aria-valuemin` = 0, `aria-valuemax` = 100,
              "Refreshing…"))
    })
  outputOptions(output, "castor_progressbar", suspendWhenHidden = FALSE)

    start_time <- Sys.time()
    done_flag <- tempfile(pattern = "castor_retrieval_done_", tmpdir = tempdir(), fileext = ".flag")
    if (file.exists(done_flag)) try(file.remove(done_flag), silent = TRUE)

    proc <- tryCatch({
      process$new(
        rscript_bin,
        args = c("--vanilla", castor_script),
        stdout = "|",
        stderr = "|",
        env = c(
          EPIC2CASTOR_DONE = done_flag,
          EPIC2CASTOR_FORCE_RETRIEVAL = "1",
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

      try(file.remove(done_flag), silent = TRUE)
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
          
          # Windows crash codes (0xC0000005 = -1073741819): negeer als script succesvol was
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

  # Runner helper: execute the Biobank Data script with status-driven monitoring
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
          
          # Windows crash codes: negeer als script succesvol was
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
            # Cleanup: verwijder process en observer uit state om geheugenlek te voorkomen
            runnerState$observer <- NULL
            runnerState$proc <- NULL
            # Start volgende taak na korte delay
            if (!isTRUE(runnerState$canceled) && !is.null(on_done)) later::later(on_done, delay = 0.05)
          }
        }, error = function(e) {
          output$upload_status_line <- renderText({ paste("Monitor error:", conditionMessage(e)) })
          updateRunnerFooter(TRUE)
          observerHandle$destroy()
          # Cleanup ook bij error
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
    updated_data_to_render = NULL  # Voor het triggeren van table re-render na autofill
  )
  
  # Observer: render tabel wanneer autofill data is bijgewerkt
  observeEvent(autofillState$updated_data_to_render, {
    req(autofillState$updated_data_to_render, input$file)
    
    render_table(autofillState$updated_data_to_render, input$file, mode = "full")
    
    # Reset na render
    autofillState$updated_data_to_render <- NULL
  })
  
  # Handle autofill button click
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
    
    # Capture current tab name and data before entering later() context
    current_tab <- isolate({
      if (!is.null(input$tab_name_meta) && input$tab_name_meta != "") {
        input$tab_name_meta
      } else {
        NULL
      }
    })
    
    # Capture current data from active tab
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
    
    # Show progress modal
    showModalSafe(modalDialog(
      title = "Processing Auto-Fill...",
      div(
        style = "text-align: center; padding: 20px;",
        tags$i(class = "fa fa-spinner fa-spin fa-3x"),
        p(style = "margin-top: 20px;", "Analyzing data and generating suggestions...")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Run autofill in background
    later::later(function() {
      tryCatch({
        # Run autofill process with current data
        results <- process_autofill(
          table_name = table_name,
          tab_name = current_tab,
          min_confidence = 70,
          current_data = current_data
        )
        
        # Store results
        autofillState$results <- results
        autofillState$original_data <- copy(current_data)
        autofillState$table_name <- table_name
        
        # Show results modal
        removeModalSafe()
        
        if (nrow(results[confidence > 0]) == 0) {
          # FASE 11.1: Specifiekere meldingen bij geen suggesties
          total_rows <- nrow(results)
          empty_castor <- sum(results$strategy == "Skipped", na.rm = TRUE)
          all_failed <- sum(results$confidence == 0 & results$strategy != "Skipped", na.rm = TRUE)
          
          # Bepaal de juiste melding
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
  
  # Function to show autofill preview modal
  show_autofill_preview_modal <- function(results) {
    successful <- results[confidence > 0]
    failed <- results[confidence == 0]
    
    # FASE 11.9: Generate summary statistics
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
    
    # FASE 11.10: Enhanced summary with statistics
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
        
        # FASE 11.11: Filter controls
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
        # FASE 11.11: Cleaner footer layout
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
  
  # FASE 11.11: Reactive filtering voor autofill preview
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
        # FASE 6 FIX: Gebruik captured actieve tab data
        current_data <- captured_active_data
        if (is.null(current_data)) {
          showModalSafe(modalDialog(
            title = "Error",
            "Could not retrieve active tab data.",
            easyClose = TRUE
          ))
          return()
        }
        
        # Bepaal de kolom naam voor de castor waarde (kolom_toevoeging voor checkboxes/radiobuttons)
        castor_col <- if (table_name %in% c("waarde_checkboxes", "waarde_radiobuttons")) {
          "kolom_toevoeging"
        } else {
          "kolom_toevoeging"  # default
        }
        
        # Use apply_approved_matches from export_approved.r
        updated_data <- apply_approved_matches(current_data, selected_rows, castor_col)
        
        applied_count <- nrow(selected_rows)
        
        # FASE 6 FIX: Update actieve tab data (NIET mappingData direct)
        # mappingData wordt pas bijgewerkt bij save via consolidate_tabs_with_metadata
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
        
        # De tabState$tabs is nu bijgewerkt, en omdat render_table() reactief is
        # op get_active_tab_data() (die tabState gebruikt), zou de tabel automatisch
        # moeten updaten. Als dat niet werkt, kunnen we een dedicated trigger toevoegen.
        
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
  
  # FASE 11.11: Export CSV download handler
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
  
  # FASE 11.11: Export Summary Report download handler
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
  
  # Mapping maintenance: ensure checkbox/radiobutton value rows exist based on Castor metadata
  updateCheckboxMapping <- function() {
    req(mappingData[["elements"]], mappingData[["waarde_checkboxes"]])

    # FASE 6.5.4: Tab-aware checkbox mapping
    elements_all <- mappingData[["elements"]]
    checkbox_all <- mappingData[["waarde_checkboxes"]]
    
    # Check of elements tabs heeft
    if (!("tab_name_meta" %in% names(elements_all))) {
      # Geen tabs, gebruik oude logica
      elements_dt <- elements_all
      current_checkbox <- checkbox_all
    } else {
      # Check of checkboxes ook tab metadata heeft
      if (!("tab_name_meta" %in% names(checkbox_all))) {
        # Checkboxes heeft geen tab metadata, voeg lege kolommen toe
        checkbox_all$tab_name_meta <- character(nrow(checkbox_all))
        checkbox_all$tab_order_meta <- integer(nrow(checkbox_all))
      }
      
      # FASE 6.5.4: Loop door alle tabs en voeg checkboxes toe per tab
      # Start met huidige checkbox data
      current_checkbox <- checkbox_all
      
      # Haal unieke tabs op uit elements
      unique_tabs <- unique(elements_all[, .(tab_name = tab_name_meta, tab_order = tab_order_meta)])
      setorder(unique_tabs, tab_order)
      
      for (i in seq_len(nrow(unique_tabs))) {
        tab_name <- unique_tabs$tab_name[i]
        tab_order <- unique_tabs$tab_order[i]
        
        # Filter elements voor deze tab
        elements_dt <- elements_all[tab_name_meta == tab_name & tab_order_meta == tab_order]
        
        # Verwijder metadata kolommen voor processing
        elements_dt_clean <- copy(elements_dt)
        elements_dt_clean[, c("tab_name_meta", "tab_order_meta") := NULL]
        
        # Filter bestaande checkboxes voor deze tab
        checkbox_for_tab <- current_checkbox[tab_name_meta == tab_name & tab_order_meta == tab_order]
        
        # Helper to find matching rows in checkBoxesValues
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
        
        # Voor elk element in deze tab, check of checkboxes bestaan
        for (j in seq_len(nrow(elements_dt_clean))) {
          el <- elements_dt_clean$Element[j]
          castor <- elements_dt_clean$castor_kolom[j]
          if (is.null(castor) || is.na(castor) || nchar(castor) == 0) next
          
          idxs <- find_checkcols(castor)
          if (length(idxs) == 0) next
          
          possible_values <- unique(checkBoxesValues$toevoeging[idxs])
          
          # Voeg ontbrekende checkboxes toe voor deze tab
          for (val in possible_values) {
            # FASE 6.5.4: Check of deze rij al bestaat (handle lege data)
            existing_row_idx <- NULL
            if (nrow(checkbox_for_tab) > 0) {
              existing_row_idx <- which(!is.na(checkbox_for_tab$Element) & 
                                        !is.na(checkbox_for_tab$kolom_toevoeging) & 
                                        checkbox_for_tab$Element == el & 
                                        checkbox_for_tab$kolom_toevoeging == val)
            }
            
            row_exists <- length(existing_row_idx) > 0
            
            if (!row_exists) {
              # Check of deze rij bestaat in checkbox_all (voor ALLE tabs, niet alleen deze tab)
              # Zo ja, behoud de bestaande waarde uit de eerste match
              existing_waarde <- ""
              if (nrow(checkbox_all) > 0) {
                existing_idx <- which(!is.na(checkbox_all$Element) & 
                                      !is.na(checkbox_all$kolom_toevoeging) & 
                                      checkbox_all$Element == el & 
                                      checkbox_all$kolom_toevoeging == val)
                if (length(existing_idx) > 0) {
                  # Neem de waarde van de eerste match (ongeacht welke tab)
                  existing_waarde <- checkbox_all$waarde[existing_idx[1]]
                  if (is.na(existing_waarde)) existing_waarde <- ""
                }
              }
              
              # Voeg nieuwe rij toe met tab metadata
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
    
    # FASE 6.5.6: Cleanup - verwijder orphaned checkbox rijen
    # (rijen waarvan het Element niet meer bestaat in elements_all)
    if (nrow(current_checkbox) > 0) {
      # Maak lijst van bestaande elements per tab (of zonder tab)
      if ("tab_name_meta" %in% names(elements_all)) {
        # Tab-aware cleanup
        valid_elements <- elements_all[!is.na(Element) & Element != "", 
                                       .(Element, tab_name_meta, tab_order_meta)]
        
        # FASE 6.5.6: Zorg ervoor dat de kolom types overeenkomen
        # Forceer correcte types in beide datasets voor de merge
        if ("tab_name_meta" %in% names(current_checkbox)) {
          current_checkbox[, tab_name_meta := as.character(tab_name_meta)]
          current_checkbox[, tab_order_meta := as.integer(tab_order_meta)]
        }
        valid_elements[, tab_name_meta := as.character(tab_name_meta)]
        valid_elements[, tab_order_meta := as.integer(tab_order_meta)]
        
        # Filter checkboxes: behoud alleen rijen waarvan Element+tab bestaat in elements
        current_checkbox <- merge(current_checkbox, valid_elements, 
                                 by = c("Element", "tab_name_meta", "tab_order_meta"), 
                                 all = FALSE)
      } else {
        # Non-tab cleanup
        valid_elements <- unique(elements_all[!is.na(Element) & Element != "", .(Element)])
        # Filter checkboxes: behoud alleen rijen waarvan Element bestaat in elements
        current_checkbox <- current_checkbox[Element %in% valid_elements$Element]
      }
    }

    mappingData[["waarde_checkboxes"]] <<- current_checkbox
  }
  
  updateRadioMapping <- function() {
    req(mappingData[["elements"]], mappingData[["waarde_radiobuttons"]])
    
    # FASE 6.5.4: Tab-aware radio button mapping
    elements_all <- mappingData[["elements"]]
    radio_all <- mappingData[["waarde_radiobuttons"]]
    
    # Check of elements tabs heeft
    if (!("tab_name_meta" %in% names(elements_all))) {
      # Geen tabs, gebruik oude logica
      elements_dt <- elements_all
      current_radio <- radio_all
    } else {
      # Check of radiobuttons ook tab metadata heeft
      if (!("tab_name_meta" %in% names(radio_all))) {
        # Radiobuttons heeft geen tab metadata, voeg lege kolommen toe
        radio_all$tab_name_meta <- character(nrow(radio_all))
        radio_all$tab_order_meta <- integer(nrow(radio_all))
      }
      
      # FASE 6.5.4: Loop door alle tabs en voeg radiobuttons toe per tab
      current_radio <- radio_all
      
      # Haal unieke tabs op uit elements
      unique_tabs <- unique(elements_all[, .(tab_name = tab_name_meta, tab_order = tab_order_meta)])
      setorder(unique_tabs, tab_order)
      
      for (i in seq_len(nrow(unique_tabs))) {
        tab_name <- unique_tabs$tab_name[i]
        tab_order <- unique_tabs$tab_order[i]
        
        # Filter elements voor deze tab
        elements_dt <- elements_all[tab_name_meta == tab_name & tab_order_meta == tab_order]
        
        # Verwijder metadata kolommen voor processing
        elements_dt_clean <- copy(elements_dt)
        elements_dt_clean[, c("tab_name_meta", "tab_order_meta") := NULL]
        
        # Filter bestaande radiobuttons voor deze tab
        radio_for_tab <- current_radio[tab_name_meta == tab_name & tab_order_meta == tab_order]
        
        # Helper to find matching rows in radioButtonOptionValues
        find_radio_rows <- function(castor_name) {
          if (is.na(castor_name) || nchar(castor_name) == 0) return(integer(0))
          idx <- which(radioButtonOptionValues$`Field Variable Name` == castor_name)
          if (length(idx) > 0) return(idx)
          castor_suffix <- sub("^[^_]+_", "", castor_name)
          idx <- which(sub("^[^_]+_", "", radioButtonOptionValues$`Field Variable Name`) == castor_suffix)
          return(idx)
        }
        
        # Voor elk element in deze tab, check of radiobuttons bestaan
        for (j in seq_len(nrow(elements_dt_clean))) {
          el <- elements_dt_clean$Element[j]
          castor <- elements_dt_clean$castor_kolom[j]
          if (is.null(castor) || is.na(castor) || nchar(castor) == 0) next
          
          idxs <- find_radio_rows(castor)
          if (length(idxs) == 0) next
          
          possible_values <- unique(radioButtonOptionValues$`Option Name`[idxs])
          
          # Voeg ontbrekende radiobuttons toe voor deze tab
          for (val in possible_values) {
            # FASE 6.5.4: Check of deze rij al bestaat (handle lege data)
            existing_row_idx <- NULL
            if (nrow(radio_for_tab) > 0) {
              existing_row_idx <- which(!is.na(radio_for_tab$Element) & 
                                        !is.na(radio_for_tab$castor_waarde) & 
                                        radio_for_tab$Element == el & 
                                        radio_for_tab$castor_waarde == val)
            }
            
            row_exists <- length(existing_row_idx) > 0
            
            if (!row_exists) {
              # Check of deze rij bestaat in radio_all (voor ALLE tabs, niet alleen deze tab)
              # Zo ja, behoud de bestaande waarde uit de eerste match
              existing_waarde <- ""
              if (nrow(radio_all) > 0) {
                existing_idx <- which(!is.na(radio_all$Element) & 
                                      !is.na(radio_all$castor_waarde) & 
                                      radio_all$Element == el & 
                                      radio_all$castor_waarde == val)
                if (length(existing_idx) > 0) {
                  # Neem de waarde van de eerste match (ongeacht welke tab)
                  existing_waarde <- radio_all$waarde[existing_idx[1]]
                  if (is.na(existing_waarde)) existing_waarde <- ""
                }
              }
              
              # Voeg nieuwe rij toe met tab metadata
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
    
    # FASE 6.5.6: Cleanup - verwijder orphaned radiobutton rijen
    # (rijen waarvan het Element niet meer bestaat in elements_all)
    if (nrow(current_radio) > 0) {
      # Maak lijst van bestaande elements per tab (of zonder tab)
      if ("tab_name_meta" %in% names(elements_all)) {
        # Tab-aware cleanup
        valid_elements <- elements_all[!is.na(Element) & Element != "", 
                                       .(Element, tab_name_meta, tab_order_meta)]
        
        # FASE 6.5.6: Zorg ervoor dat de kolom types overeenkomen
        # Forceer correcte types in beide datasets voor de merge
        if ("tab_name_meta" %in% names(current_radio)) {
          current_radio[, tab_name_meta := as.character(tab_name_meta)]
          current_radio[, tab_order_meta := as.integer(tab_order_meta)]
        }
        valid_elements[, tab_name_meta := as.character(tab_name_meta)]
        valid_elements[, tab_order_meta := as.integer(tab_order_meta)]
        
        # Filter radiobuttons: behoud alleen rijen waarvan Element+tab bestaat in elements
        current_radio <- merge(current_radio, valid_elements, 
                              by = c("Element", "tab_name_meta", "tab_order_meta"), 
                              all = FALSE)
      } else {
        # Non-tab cleanup
        valid_elements <- unique(elements_all[!is.na(Element) & Element != "", .(Element)])
        # Filter radiobuttons: behoud alleen rijen waarvan Element bestaat in elements
        current_radio <- current_radio[Element %in% valid_elements$Element]
      }
    }

    mappingData[["waarde_radiobuttons"]] <<- current_radio
  }
  
  # Update row warning icon visibility based on row count
  update_row_warning <- function(data) {
    if (is.null(data)) {
      shinyjs::hide("row_warning_icon")
      output$row_warning_content <- renderUI({ NULL })
      return()
    }
    
    row_count <- nrow(data)
    if (row_count > 50) {
      rows_to_remove <- row_count - 50
      tooltip_text <- sprintf(
        "This tab has %d row(s), consider removing %d row(s). We recommend not adding more than 50 rows to a tab as this will slow down the program.",
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
  
  render_table <- function(data, file, mode = c("auto", "full", "proxy")) {
    mode <- match.arg(mode)
    if (identical(mode, "auto")) {
      if (!isTRUE(table_initialized()) || !identical(current_table_name(), file)) {
        mode <- "full"
      } else {
        mode <- "proxy"
      }
    } else if (identical(mode, "proxy") && !isTRUE(table_initialized())) {
      mode <- "full"
    }
    
    # FASE 6.5.6: Check voor tab metadata VOOR we ze verwijderen
    has_input_tab_meta <- "tab_name_meta" %in% names(data)
    
    # FASE 6: Verwijder metadata kolommen uit weergave
    data_for_display <- copy(data)
    meta_cols <- names(data_for_display)[grepl("tab_(name|order)_meta", names(data_for_display))]
    if (length(meta_cols) > 0) {
      data_for_display[, (meta_cols) := NULL]
    }
    
    data_with_delete <- cbind(Index = seq_len(nrow(data_for_display)), data_for_display)
    
    data_with_delete$Select <- paste0('<input type="checkbox" class="delete-rows" id="deleterows_', 
                                        seq_len(nrow(data_with_delete)), '">')
    
    display_data <- copy(data_with_delete)
    
    # Column renames for display: specific rename for 'elements'
    if(file == "elements") {
      setnames(display_data, "castor_kolom", "Castor Name", skip_absent = TRUE)
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
      if (old_name %in% names(display_data)) {
        setnames(display_data, old_name, col_renames[[old_name]], skip_absent = TRUE)
      }
    }
    
    if(file %in% c("waarde_checkboxes", "waarde_radiobuttons")) {
      # FASE 6.5.6: Gebruik mappingData[["elements"]] met tab-aware merge
      elements_df <- as.data.table(copy(mappingData[["elements"]]))
      
      # Check of elements tab metadata heeft
      has_tab_meta <- "tab_name_meta" %in% names(elements_df)
      
      # Hernoem castor_kolom voor merge
      setnames(elements_df, "castor_kolom", "castor_kolom_naam")
      elements_df <- elements_df[!is.na(Element) & Element != ""]
      
      # FASE 6.5.6: Gebruik de check die we eerder hebben gedaan op originele data
      if (has_tab_meta && has_input_tab_meta) {
        # FASE 6.5.6: Tab-aware merge: match op Element EN tab metadata
        # Zo krijgen we de juiste castor_kolom voor elk element PER tab
        
        # Voor tab-aware merge moeten we de originele data gebruiken (met metadata)
        # Maak een tijdelijke dataset met Index, Select en metadata
        temp_merge_data <- copy(data)
        temp_merge_data[, Index := seq_len(.N)]
        temp_merge_data[, Select := paste0('<input type="checkbox" class="delete-rows" id="deleterows_', seq_len(.N), '">')]
        
        # Bewaar originele volgorde
        temp_merge_data[, original_order := .I]
        
        # Zorg ervoor dat tab_order_meta hetzelfde type heeft in beide datasets
        if ("tab_order_meta" %in% names(temp_merge_data)) {
          temp_merge_data[, tab_order_meta := as.integer(tab_order_meta)]
        }
        if ("tab_order_meta" %in% names(elements_df)) {
          elements_df[, tab_order_meta := as.integer(tab_order_meta)]
        }
        
        # Merge met tab metadata
        temp_merge_data <- merge(temp_merge_data, elements_df, 
                                by = c("Element", "tab_name_meta", "tab_order_meta"), 
                                all.x = TRUE, sort = FALSE)
        
        # Herstel originele volgorde
        setorder(temp_merge_data, original_order)
        temp_merge_data[, original_order := NULL]
        
        # Verwijder metadata kolommen voor display
        meta_cols <- names(temp_merge_data)[grepl("tab_(name|order)_meta", names(temp_merge_data))]
        if (length(meta_cols) > 0) {
          temp_merge_data[, (meta_cols) := NULL]
        }
        
        # Gebruik deze als display_data
        display_data <- temp_merge_data
      } else {
        # Geen tab metadata in één van beide: gebruik alleen Element voor merge
        # Dit gebeurt als de data nog niet geüpdatet is met tab metadata
        elements_df_unique <- copy(elements_df)
        
        # Verwijder tab metadata uit elements als die bestaat
        if (has_tab_meta) {
          elements_df_unique[, c("tab_name_meta", "tab_order_meta") := NULL]
        }
        
        # Maak uniek op Element
        elements_df_unique <- unique(elements_df_unique, by = "Element")
        
        # Merge alleen op Element (display_data heeft al geen metadata meer)
        display_data <- merge(display_data, elements_df_unique, 
                             by = "Element", all.x = TRUE, sort = FALSE)
        
        # Zorg ervoor dat eventuele metadata kolommen ook hier verwijderd zijn
        meta_cols <- names(display_data)[grepl("tab_(name|order)_meta", names(display_data))]
        if (length(meta_cols) > 0) {
          display_data[, (meta_cols) := NULL]
        }
      }
      
      setnames(display_data, "castor_kolom_naam", "Castor Name", skip_absent = TRUE)
      setnames(display_data, "waarde", "EPIC Value", skip_absent = TRUE)
      
      if(file == "waarde_radiobuttons") {
        setnames(display_data, "castor_waarde", "Castor Value", skip_absent = TRUE)
      } else if(file == "waarde_checkboxes") {
        setnames(display_data, "kolom_toevoeging", "Castor Value", skip_absent = TRUE)
      }
      
      # Metadata kolommen zijn al verwijderd in de merge logica hierboven
      
      cols <- names(display_data)
      other_cols <- setdiff(cols, c("Index", "Element", "Castor Name", "Select"))
      display_data <- display_data[, c("Index", "Element", "Castor Name", other_cols, "Select"), with = FALSE]
    } else {
      cols <- names(display_data)
      other_cols <- setdiff(cols, c("Index", "Select"))
      display_data <- display_data[, c("Index", other_cols, "Select"), with = FALSE]
    }
    
  option_data_selected <- option_data[grepl(file, names(option_data))]
    
    # Map original column names to display names
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
    
    if (file %in% c("waarde_checkboxes", "waarde_radiobuttons")) {
      option_data_selected <- option_data_selected[
        !grepl(paste0(file, "\\|Element"), names(option_data_selected))
      ]
    }
    
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
    
    if ("elements|Element" %in% names(option_data_selected)) {
      option_data_selected[["elements|Element"]]$options <-
        option_data_selected[["elements|Element"]]$options[
          order(as.numeric(sub(".*#", "", option_data_selected[["elements|Element"]]$options)))
        ]
    }
    
    for (i in seq_along(option_data_selected)) {
      parts <- strsplit(names(option_data_selected)[[i]], "|", fixed = TRUE)[[1]]
      if (length(parts) < 2) next
      table <- parts[1]
      column <- parts[2]
      if (column == "castor_kolom_naam") next
      
      original_column <- column
      
      # Use display column name when available
      display_column <- if (column %in% names(column_mapping)) column_mapping[[column]] else column
      
      optionsList <- option_data_selected[[i]]$options

      if (!is.null(option_data_selected[[i]]$match_col) && isTRUE(input$toggle)) {
        matchList <- option_data_selected[[i]]$match
        matchCol <- option_data_selected[[i]]$match_col
      }
      
      if (isTRUE(input$toggle)) {
        if (table == "waarde_checkboxes" && column == "Element") {
          optionsList <- mappingData[["elements"]][["Element"]][mappingData[["elements"]][["castor_kolom"]] %in% checkboxes]
        }
        if (table == "waarde_radiobuttons" && column == "Element") {
          optionsList <- mappingData[["elements"]][["Element"]][mappingData[["elements"]][["castor_kolom"]] %in% radiobuttons]
        }
      }
      
      if (any(colnames(display_data) == display_column)) {
        display_data[[display_column]] <- sapply(seq_len(nrow(display_data)), function(valueNumber) {
          # Use original data for the value
          value <- data_with_delete[[original_column]][valueNumber]
          rowN <- valueNumber

          always_narrow <- file %in% c("waarde_checkboxes","waarde_radiobuttons") && column=="waarde"

      if (!is.null(option_data_selected[[i]]$match_col)
        && ((isTRUE(input$toggle)) || always_narrow)
              && (option_data_selected[[i]]$match_col %in% names(data))) {
            matchCol <- option_data_selected[[i]]$match_col
            matchList <- option_data_selected[[i]]$match
            valueMatch <- data[[ matchCol ]][valueNumber]
            optionsListFiltered <- optionsList[matchList == valueMatch]
          } else {
            optionsListFiltered <- optionsList
          }
          optionsListFiltered <- optionsListFiltered[!is.na(optionsListFiltered)]

          # Always include the current cell value in the dropdown
          optionsListFiltered <- unique(c(value, optionsListFiltered))

          # Compute CSS class based on whether value exists in full option list
          key <- paste0(valueNumber, "_", column)
          if (!(value %in% optionsList)) {
            css_class <- if (!is.null(manualValues$vals[[key]]) && manualValues$vals[[key]] == TRUE) {
              "blue-cell"
            } else if (nzchar(value)) {
              "red-cell"
            } else {
              ""
            }
          } else {
            css_class <- ""
          }
          
          # Encode options as JSON (used by client JS on demand)
          options_json <- jsonlite::toJSON(optionsListFiltered, auto_unbox = TRUE)

          # Build full dropdown: include all options and mark the current value as selected
          options_html <- paste(
            sapply(optionsListFiltered, function(opt) {
              # FASE 6.5: Handle NA values in comparison
              is_selected <- !is.na(opt) && !is.na(value) && opt == value
              if(is_selected) {
                paste0('<option value="', opt, '" selected>', opt, '</option>')
              } else {
                paste0('<option value="', opt, '">', opt, '</option>')
              }
            }),
            collapse = ""
          )

          # Add the special "Add value..." option
          options_html <- paste0(options_html, '<option value="__ADD__">Add value...</option>')

          dropdown_html <- paste0(
              '<select id="dropdown_', original_column, '_', rowN, 
              '" data-row="', rowN, '" data-col="', original_column, 
              '" data-options=\'', options_json, '\' data-cssclass="', css_class, 
              '" class="lazy-load" onchange="Shiny.setInputValue(\'dropdown_change\', {row: ', rowN, 
              ', col: \'', original_column, '\', id: this.id, value: this.value});">',
              options_html,
              '</select>'
          )
        }, simplify = FALSE)
      }
    }
  
    if (identical(mode, "full") || !isTRUE(table_initialized())) {
      output$table <- renderDT({
        datatable(
          display_data,
          rownames = FALSE,
          escape = FALSE,
          editable = list(target = 'cell', disable = list(
            columns = unname(c(
              sapply(names(option_data_selected), function(x) { 
                col_name <- strsplit(x, "|", fixed = TRUE)[[1]][2]
                # Map to the display column name when present
                if (col_name %in% names(column_mapping)) column_mapping[[col_name]] else col_name
              }),
              ncol(display_data)
            ))
          )),
          selection = "none",
          options = list(
            deferRender = TRUE,
            autoWidth = FALSE,
            scroller = TRUE,
            paging = FALSE,
            searching = TRUE,
            ordering = FALSE,
            dom = 'lrtip',
            initComplete = JS(
              "function(settings, json) {",
              "  this.api().columns.adjust();",
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
              "}"
            ),
            drawCallback = JS(
              "function(settings, json) {",
              "  $(document).trigger('initializeSelect2');",
              "}"
            )
          )
        )
      })
      table_initialized(TRUE)
    } else {
      replaceData(
        proxy,
        as.data.frame(display_data, stringsAsFactors = FALSE),
        resetPaging = FALSE,
        clearSelection = "none",
        rownames = FALSE
      )
    }
    current_table_name(file)
    session$sendCustomMessage("reloadSelect2", list())
    session$sendCustomMessage("restoreScroll", list())
    
    # Update row warning icon
    update_row_warning(data)
  }
  
  proxy <- DT::dataTableProxy("table")
  
  observeEvent(input$toggle, {
    req(is_selectable_table(input$file))
    render_table(mappingData[[input$file]], input$file)
  })
  
  # Delete rows observer - triggert alleen bij delete_rows button click
  observeEvent(input$delete_rows, {
    req(is_selectable_table(input$file))
    req(!is.null(input$table_rows_selected))
    req(length(input$table_rows_selected) > 0)
    
    selected_ids <- as.numeric(input$table_rows_selected)
    
    # FASE 6 FIX: Verwijder rijen van actieve tab
    new_data <- get_active_tab_data()
    if (!is.null(new_data) && nrow(new_data) > 0) {
      new_data <- new_data[-selected_ids, ]
      
      # Update actieve tab
      if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
        active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
        if (length(active_idx) > 0) {
          tabState$tabs[[active_idx]]$data <- new_data
        }
      }
      
      render_table(new_data, input$file)
    }
  })
  
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

  # Server-side validation for rows_to_add: show error message if non-numeric or not integer >0
  observe({
    # if input$rows_to_add is null then nothing to show
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
  # FASE 3: ENABLE/DISABLE LOGIC VOOR COPY/CUT/PASTE KNOPPEN
  # ============================================================================
  
  # Observer: Enable/Disable Copy en Cut knoppen op basis van selectie
  observe({
    # Copy/Cut is alleen beschikbaar voor elements tabel
    # Check of er rijen geselecteerd zijn EN of het de elements tabel is
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
  
  # Observer: Enable/Disable Paste knop op basis van clipboard status
  observe({
    # Paste is alleen beschikbaar als:
    # 1. Clipboard niet leeg is
    # 2. Huidige tabel overeenkomt met bron tabel (elements)
    # 3. Het de elements tabel is
    
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
  
  # Observer: Enable/Disable Bulk Move knop op basis van selectie en tabel
  observe({
    # Bulk Move is alleen beschikbaar als:
    # 1. Er rijen geselecteerd zijn
    # 2. Het de elements tabel is
    
    # Force dependencies - zorg dat observer reageert op deze inputs
    req(input$file)  # Wacht tot file input beschikbaar is
    
    has_selection <- !is.null(input$table_rows_selected) && 
                     length(input$table_rows_selected) > 0
    
    is_valid_table <- !is.null(input$file) && is_copyable_table(input$file)
    
    # Bepaal de tooltip message op basis van de huidige staat
    # Volgorde is belangrijk: check eerst de tabel, dan de selectie
    if (!is_valid_table) {
      # Verkeerde tabel (of geen tabel)
      shinyjs::disable("move_rows_bulk")
      tooltip_message <- "Only available in Elements table"
    } else if (!has_selection) {
      # Correcte tabel maar geen selectie
      shinyjs::disable("move_rows_bulk")
      tooltip_message <- "No rows selected"
    } else {
      # Alles is OK - enabled state
      shinyjs::enable("move_rows_bulk")
      tooltip_message <- "Move selected rows in bulk"
    }
    
    # Update de tooltip via JavaScript met een kleine delay om zeker te zijn
    # dat de JavaScript handlers geregistreerd zijn
    shinyjs::delay(150, {
      session$sendCustomMessage(
        type = "updateTooltip",
        message = list(
          id = "move_rows_bulk",
          title = tooltip_message
        )
      )
    })
  }, priority = -10)  # Lage prioriteit zodat het na UI rendering gebeurt
  
  # ============================================================================
  # FASE 4: COPY FUNCTIONALITEIT
  # ============================================================================
  
  # Observer: Copy geselecteerde rijen naar clipboard
  observeEvent(input$copy_rows, {
    # Validaties - alleen voor elements tabel
    req(is_copyable_table(input$file))
    req(!is.null(input$table_rows_selected))
    req(length(input$table_rows_selected) > 0)
    
    # Haal geselecteerde rijen op
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
    
    # Clear oude clipboard data
    clear_clipboard()
    
    # Haal tab informatie op
    source_tab_name <- get_active_tab_name()
    source_tab_order <- get_active_tab_order()
    
    # Kopieer de data (deep copy om referenties te vermijden)
    clipboard_data <- copy(selected_data)
    
    # Sla basisinformatie op in clipboard
    clipboardState$data <- clipboard_data
    clipboardState$source_table <- input$file
    clipboardState$source_tab <- tabState$activeTab
    clipboardState$source_tab_name <- source_tab_name
    clipboardState$source_tab_order <- source_tab_order
    clipboardState$source_row_indices <- selected_indices
    clipboardState$operation <- "copy"
    clipboardState$timestamp <- Sys.time()
    
    # Als het elements tabel is, haal ook gerelateerde checkboxes en radiobuttons op
    if (input$file == "elements") {
      # Haal Element waarden op uit geselecteerde rijen
      if ("Element" %in% names(clipboard_data)) {
        element_values <- clipboard_data$Element
        element_values <- element_values[!is.na(element_values) & nchar(element_values) > 0]
        
        if (length(element_values) > 0) {
          # Haal gerelateerde checkboxes op
          related_checkboxes <- get_related_checkbox_data(
            element_values, 
            source_tab_name, 
            source_tab_order
          )
          
          # Haal gerelateerde radiobuttons op
          related_radiobuttons <- get_related_radiobutton_data(
            element_values, 
            source_tab_name, 
            source_tab_order
          )
          
          # Sla gerelateerde data op (deep copy)
          if (!is.null(related_checkboxes)) {
            clipboardState$related_checkboxes <- copy(related_checkboxes)
          }
          if (!is.null(related_radiobuttons)) {
            clipboardState$related_radiobuttons <- copy(related_radiobuttons)
          }
        }
      }
    }
    
    # Bereken hoeveel data is gekopieerd
    n_rows <- nrow(clipboard_data)
    n_checkboxes <- if (!is.null(clipboardState$related_checkboxes)) nrow(clipboardState$related_checkboxes) else 0
    n_radiobuttons <- if (!is.null(clipboardState$related_radiobuttons)) nrow(clipboardState$related_radiobuttons) else 0
    
    # Toon notificatie
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
    
    # Log voor debugging
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
  # FASE 5: CUT FUNCTIONALITEIT
  # ============================================================================
  
  # Observer: Cut geselecteerde rijen naar clipboard
  observeEvent(input$cut_rows, {
    # Validaties - alleen voor elements tabel
    req(is_copyable_table(input$file))
    req(!is.null(input$table_rows_selected))
    req(length(input$table_rows_selected) > 0)
    
    # Haal geselecteerde rijen op
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
    
    # Clear oude clipboard data
    clear_clipboard()
    
    # Haal tab informatie op
    source_tab_name <- get_active_tab_name()
    source_tab_order <- get_active_tab_order()
    
    # Kopieer de data (deep copy om referenties te vermijden)
    clipboard_data <- copy(selected_data)
    
    # Sla basisinformatie op in clipboard
    clipboardState$data <- clipboard_data
    clipboardState$source_table <- input$file
    clipboardState$source_tab <- tabState$activeTab
    clipboardState$source_tab_name <- source_tab_name
    clipboardState$source_tab_order <- source_tab_order
    clipboardState$source_row_indices <- selected_indices
    clipboardState$operation <- "cut"  # VERSCHIL MET COPY: "cut" in plaats van "copy"
    clipboardState$timestamp <- Sys.time()
    
    # Als het elements tabel is, haal ook gerelateerde checkboxes en radiobuttons op
    if (input$file == "elements") {
      # Haal Element waarden op uit geselecteerde rijen
      if ("Element" %in% names(clipboard_data)) {
        element_values <- clipboard_data$Element
        element_values <- element_values[!is.na(element_values) & nchar(element_values) > 0]
        
        if (length(element_values) > 0) {
          # Haal gerelateerde checkboxes op
          related_checkboxes <- get_related_checkbox_data(
            element_values, 
            source_tab_name, 
            source_tab_order
          )
          
          # Haal gerelateerde radiobuttons op
          related_radiobuttons <- get_related_radiobutton_data(
            element_values, 
            source_tab_name, 
            source_tab_order
          )
          
          # Sla gerelateerde data op (deep copy)
          if (!is.null(related_checkboxes)) {
            clipboardState$related_checkboxes <- copy(related_checkboxes)
          }
          if (!is.null(related_radiobuttons)) {
            clipboardState$related_radiobuttons <- copy(related_radiobuttons)
          }
        }
      }
    }
    
    # Bereken hoeveel data is geknipt
    n_rows <- nrow(clipboard_data)
    n_checkboxes <- if (!is.null(clipboardState$related_checkboxes)) nrow(clipboardState$related_checkboxes) else 0
    n_radiobuttons <- if (!is.null(clipboardState$related_radiobuttons)) nrow(clipboardState$related_radiobuttons) else 0
    
    # Toon notificatie (oranje voor cut)
    msg <- sprintf("%d row%s cut", n_rows, if (n_rows > 1) "s" else "")
    if (input$file == "elements" && (n_checkboxes > 0 || n_radiobuttons > 0)) {
      msg <- sprintf("%s (with %d related checkbox%s and %d radiobutton%s)", 
                     msg,
                     n_checkboxes, if (n_checkboxes != 1) "es" else "",
                     n_radiobuttons, if (n_radiobuttons != 1) "s" else "")
    }
    
    showNotification(
      msg,
      type = "warning",  # Warning type voor cut (oranje)
      duration = 2
    )
    
    # Log voor debugging
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
  # FASE 6: PASTE FUNCTIONALITEIT
  # ============================================================================
  
  observeEvent(input$paste_rows, {
    # 1. VALIDATIES - alleen voor elements tabel
    req(is_copyable_table(input$file))
    req(!is.null(clipboardState$data))
    req(nrow(clipboardState$data) > 0)
    
    # Controleer dat we naar dezelfde tabel type pasten (moet elements zijn)
    if (clipboardState$source_table != input$file) {
      showNotification(
        sprintf("Cannot paste from %s to %s", clipboardState$source_table, input$file),
        type = "error",
        duration = 3
      )
      return()
    }
    
    # 2. HAAL TARGET TAB INFO OP
    target_tab_name <- get_active_tab_name()
    target_tab_order <- get_active_tab_order()
    
    if (is.null(target_tab_name) || is.null(target_tab_order)) {
      showNotification("No active tab found", type = "error", duration = 3)
      return()
    }
    
    # 3. HAAL TARGET TAB DATA OP
    target_data <- get_active_tab_data()
    if (is.null(target_data)) {
      showNotification("No target data found", type = "error", duration = 3)
      return()
    }
    
    # 4. KOPIEER CLIPBOARD DATA EN UPDATE TAB METADATA
    paste_data <- copy(clipboardState$data)
    
    # Update tab metadata naar target tab
    if ("tab_name_meta" %in% names(paste_data)) {
      paste_data$tab_name_meta <- target_tab_name
    }
    if ("tab_order_meta" %in% names(paste_data)) {
      paste_data$tab_order_meta <- target_tab_order
    }
    
    # 5. VOEG DATA TOE AAN TARGET TAB
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
    
    # 7. PASTE GERELATEERDE DATA (ALLEEN VOOR ELEMENTS)
    if (input$file == "elements") {
      
      # 7a. PASTE RELATED CHECKBOXES
      if (!is.null(clipboardState$related_checkboxes) && nrow(clipboardState$related_checkboxes) > 0) {
        related_checkboxes <- copy(clipboardState$related_checkboxes)
        
        # Update tab metadata naar target tab
        if ("tab_name_meta" %in% names(related_checkboxes)) {
          related_checkboxes$tab_name_meta <- target_tab_name
        }
        if ("tab_order_meta" %in% names(related_checkboxes)) {
          related_checkboxes$tab_order_meta <- target_tab_order
        }
        
        # Voeg toe aan mappingData
        checkbox_data <- mappingData[["waarde_checkboxes"]]
        if (!is.null(checkbox_data)) {
          mappingData[["waarde_checkboxes"]] <<- rbind(checkbox_data, related_checkboxes)
          n_checkboxes <- nrow(related_checkboxes)
        }
      }
      
      # 7b. PASTE RELATED RADIOBUTTONS
      if (!is.null(clipboardState$related_radiobuttons) && nrow(clipboardState$related_radiobuttons) > 0) {
        related_radiobuttons <- copy(clipboardState$related_radiobuttons)
        
        # Update tab metadata naar target tab
        if ("tab_name_meta" %in% names(related_radiobuttons)) {
          related_radiobuttons$tab_name_meta <- target_tab_name
        }
        if ("tab_order_meta" %in% names(related_radiobuttons)) {
          related_radiobuttons$tab_order_meta <- target_tab_order
        }
        
        # Voeg toe aan mappingData
        radiobutton_data <- mappingData[["waarde_radiobuttons"]]
        if (!is.null(radiobutton_data)) {
          # Voeg nieuwe rijen toe via rbindlist met fill
          combined <- rbindlist(list(radiobutton_data, related_radiobuttons), fill = TRUE)
          
          # Update mappingData met GLOBAL ASSIGNMENT (<<-)
          # Dit is nodig voor reactiveValues binnen observers
          mappingData[["waarde_radiobuttons"]] <<- combined
          
          n_radiobuttons <- nrow(related_radiobuttons)
        }
      }
    }
    
    # 8. VERWIJDER BRON RIJEN BIJ CUT OPERATIE
    if (clipboardState$operation == "cut") {
      # Vind de source tab
      # Let op: tabState$tabs bevat alleen tabs van de HUIDIGE tabel (input$file)
      # Als we in dezelfde tabel zijn (source == target table), zoeken we de source tab
      source_tab_idx <- which(sapply(tabState$tabs, function(t) {
        t$name == clipboardState$source_tab_name &&
        t$order == clipboardState$source_tab_order
      }))
      
      if (length(source_tab_idx) > 0) {
        source_data <- tabState$tabs[[source_tab_idx]]$data
        
        # Verwijder de gekopieerde rijen (gebruik row indices)
        if (!is.null(clipboardState$source_row_indices) && length(clipboardState$source_row_indices) > 0) {
          indices_to_keep <- setdiff(seq_len(nrow(source_data)), clipboardState$source_row_indices)
          
          if (length(indices_to_keep) > 0) {
            source_data <- source_data[indices_to_keep, ]
          } else {
            # Als alle rijen verwijderd worden, maak lege data.frame met zelfde structuur
            source_data <- source_data[0, ]
          }
          
          tabState$tabs[[source_tab_idx]]$data <- source_data
          
          # 8b. VERWIJDER GERELATEERDE DATA BIJ CUT (ALLEEN VOOR ELEMENTS)
          if (clipboardState$source_table == "elements") {
            
            # Verwijder related checkboxes uit mappingData
            if (!is.null(clipboardState$related_checkboxes) && nrow(clipboardState$related_checkboxes) > 0) {
              checkbox_data <- mappingData[["waarde_checkboxes"]]
              
              if (!is.null(checkbox_data) && nrow(checkbox_data) > 0) {
                # CRITICAL FIX: Match op Element + waarde + TAB METADATA
                # Dit voorkomt dat we net geplakte checkboxes verwijderen!
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
                
                # Verwijder ALLEEN matchende rijen met dezelfde tab metadata
                indices_to_keep <- which(!existing_keys %in% related_keys)
                
                if (length(indices_to_keep) > 0) {
                  mappingData[["waarde_checkboxes"]] <<- checkbox_data[indices_to_keep, ]
                } else {
                  mappingData[["waarde_checkboxes"]] <<- checkbox_data[0, ]
                }
              }
            }
            
            # Verwijder related radiobuttons uit mappingData
            if (!is.null(clipboardState$related_radiobuttons) && nrow(clipboardState$related_radiobuttons) > 0) {
              radiobutton_data <- mappingData[["waarde_radiobuttons"]]
              
              if (!is.null(radiobutton_data) && nrow(radiobutton_data) > 0) {
                # CRITICAL FIX: Match op Element + waarde + TAB METADATA
                # Dit voorkomt dat we net geplakte radiobuttons verwijderen!
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
                
                # Verwijder ALLEEN matchende rijen met dezelfde tab metadata
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
    
    # 9. RENDER BIJGEWERKTE TABEL
    render_table(target_data, input$file)
    
    # 10. CONSOLIDEER ELEMENTS DATA NAAR MAPPINGDATA (zodat checkbox/radiobutton tabs correct laden)
    if (input$file == "elements") {
      # Consolideer alle elements tabs naar mappingData
      consolidated_elements <- consolidate_tabs_with_metadata(tabState$tabs)
      mappingData[["elements"]] <<- consolidated_elements
      
      # LET OP: Roep updateCheckboxMapping/updateRadioMapping NIET aan hier!
      # De related data is al correct geplakt in stap 7a/7b met de juiste waarden.
      # Als we hier updateMapping() aanroepen, worden mogelijk waarden overschreven.
      # updateMapping() wordt aangeroepen bij table switch (regel ~1769) wanneer nodig.
    }
    
    # 11. TOON NOTIFICATIE
    operation_text <- if (clipboardState$operation == "cut") "moved" else "pasted"
    msg <- sprintf("%d row%s %s", n_rows, if (n_rows > 1) "s" else "", operation_text)
    
    if (input$file == "elements" && (n_checkboxes > 0 || n_radiobuttons > 0)) {
      msg <- sprintf("%s (with %d related checkbox%s and %d radiobutton%s)", 
                     msg, n_checkboxes, if (n_checkboxes != 1) "es" else "",
                     n_radiobuttons, if (n_radiobuttons != 1) "s" else "")
    }
    
    showNotification(msg, type = "message", duration = 2)
    
    # 12. LOG VOOR DEBUGGING
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
  # FASE 3: BULK ROW MOVE - OBSERVER EVENTS
  # ============================================================================
  
  # Observer: Open bulk move modal wanneer knop wordt geklikt
  observeEvent(input$move_rows_bulk, {
    # 1. VALIDATIES
    req(!is.null(input$file))
    req(!is.null(input$table_rows_selected))
    req(length(input$table_rows_selected) > 0)
    
    # Haal geselecteerde indices op
    selected_indices <- as.numeric(input$table_rows_selected)
    
    # Check of bulk move enabled is voor deze tabel
    validation <- is_bulk_move_enabled(input$file, selected_indices)
    
    if (!validation$enabled) {
      showNotification(
        validation$message,
        type = "error",
        duration = 3
      )
      return()
    }
    
    # 2. HAAL ACTIVE TAB DATA OP
    active_data <- get_active_tab_data()
    if (is.null(active_data)) {
      showNotification("No active table data found", type = "error", duration = 3)
      return()
    }
    
    total_rows <- nrow(active_data)
    
    # 3. SORTEER EN BEREID SELECTIE INFO VOOR
    selected_indices <- sort(unique(selected_indices))
    n_selected <- length(selected_indices)
    
    # Maak een mooie display van huidige posities
    if (n_selected <= 5) {
      current_positions_text <- paste(selected_indices, collapse = ", ")
    } else {
      # Als er veel rijen zijn, toon eerste 3 en laatste 2
      current_positions_text <- sprintf("%s, ..., %s", 
                                       paste(head(selected_indices, 3), collapse = ", "),
                                       paste(tail(selected_indices, 2), collapse = ", "))
    }
    
    # 4. STORE STATE VOOR LATER GEBRUIK
    bulkMoveState$pending <- TRUE
    bulkMoveState$selected_indices <- selected_indices
    bulkMoveState$total_rows <- total_rows
    bulkMoveState$preview_data <- list(
      n_selected = n_selected,
      current_positions = current_positions_text
    )
    
    # 5. TOON MODAL DIALOG
    showModal(modalDialog(
      title = "Move Rows in Bulk",
      size = "m",
      
      tags$div(
        style = "padding: 10px;",
        
        # Info over selectie
        tags$p(
          style = "margin-bottom: 15px; color: #333;",
          tags$strong(sprintf("Selected: %d row%s", n_selected, if(n_selected > 1) "s" else ""))
        ),
        tags$p(
          style = "margin-bottom: 20px; color: #666; font-size: 0.9em;",
          sprintf("Current positions: %s", current_positions_text)
        ),
        
        # Input voor target position
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
        
        # Preview van nieuwe posities (dynamisch)
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
    
    # Log voor debugging
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
    
    # Bereken preview
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
    
    # Toon preview van nieuwe posities
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
  
  # Observer: Bevestig bulk move en voer uit
  observeEvent(input$bulk_move_confirm, {
    # 1. VALIDATIES
    req(bulkMoveState$pending)
    req(!is.null(input$bulk_move_target))
    req(!is.null(bulkMoveState$selected_indices))
    req(!is.null(input$file))
    
    target_position <- as.integer(input$bulk_move_target)
    selected_indices <- bulkMoveState$selected_indices
    total_rows <- bulkMoveState$total_rows
    
    # 2. BEREKEN POSITIES
    position_calc <- calculate_bulk_move_positions(selected_indices, target_position, total_rows)
    
    if (!position_calc$valid) {
      showNotification(
        position_calc$message,
        type = "error",
        duration = 3
      )
      return()
    }
    
    # 3. HAAL ACTIVE TAB DATA OP
    active_data <- get_active_tab_data()
    if (is.null(active_data)) {
      showNotification("No active table data found", type = "error", duration = 3)
      removeModal()
      return()
    }
    
    # 4. VOER BULK MOVE UIT
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
    
    # 6. CONSOLIDEER ELEMENTS DATA NAAR MAPPINGDATA (indien nodig)
    if (input$file == "elements") {
      consolidated_elements <- consolidate_tabs_with_metadata(tabState$tabs)
      mappingData[["elements"]] <<- consolidated_elements
    }
    
    # 7. RENDER BIJGEWERKTE TABEL
    render_table(moved_data, input$file)
    
    # 8. TOON SUCCESS NOTIFICATIE
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
    
    # 9. LOG VOOR DEBUGGING
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
    
    # 11. SLUIT MODAL
    removeModal()
  })

  observeEvent(input$confirm_add_rows, {
    req(is_selectable_table(input$file))
    n <- as.integer(input$rows_to_add)
    if (is.na(n) || n < 1) n <- 1

    # FASE 6 FIX: Voeg rijen toe aan actieve tab, niet aan mappingData
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

    # Maak nieuwe lege rijen
    new_rows <- as.data.frame(matrix("", nrow = n, ncol = ncol(active_data)), stringsAsFactors = FALSE)
    colnames(new_rows) <- colnames(active_data)
    active_data <- rbind(active_data, new_rows)
    
    # Update de actieve tab data
    if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_idx) > 0) {
        tabState$tabs[[active_idx]]$data <- active_data
      }
    }
    
    removeModal()
    render_table(active_data, input$file)
  })
  
  # Handle "Proceed anyway" button when user wants to add more than 50 rows
  observeEvent(input$proceed_add_rows, {
    req(is_selectable_table(input$file))
    n <- as.integer(input$rows_to_add)
    if (is.na(n) || n < 1) n <- 1

    # FASE 6 FIX: Voeg rijen toe aan actieve tab, niet aan mappingData
    active_data <- get_active_tab_data()
    if (is.null(active_data) || ncol(active_data) == 0) {
      removeModal()
      return()
    }

    # Maak nieuwe lege rijen
    new_rows <- as.data.frame(matrix("", nrow = n, ncol = ncol(active_data)), stringsAsFactors = FALSE)
    colnames(new_rows) <- colnames(active_data)
    active_data <- rbind(active_data, new_rows)
    
    # Update de actieve tab data
    if (!is.null(tabState$activeTab) && length(tabState$tabs) > 0) {
      active_idx <- which(sapply(tabState$tabs, function(t) t$id == tabState$activeTab))
      if (length(active_idx) > 0) {
        tabState$tabs[[active_idx]]$data <- active_data
      }
    }
    
    removeModal()
    render_table(active_data, input$file)
  })
  
  observeEvent(input$table_cell_edit, {
    req(is_selectable_table(input$file))
    info <- input$table_cell_edit
    if (is.null(info$col) || length(info$col) == 0) return(NULL)
    
    col_index <- as.numeric(info$col)
    
    # FASE 6 FIX: Werk met actieve tab data
    new_data <- get_active_tab_data()
    if (is.null(new_data) || ncol(new_data) == 0) return(NULL)
    
    if (col_index <= 1) {
      # Reordering (eerste kolom is row number)
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
    
    # Update actieve tab data
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
      
      # FASE 6 FIX: Gebruik actieve tab data
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
      
      # FASE 6 FIX: Gebruik actieve tab data
      new_data <- get_active_tab_data()
      new_data[row, (colName) := info$value]
      
      # Update actieve tab
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
      
      session$sendCustomMessage("updateSelectedOption", list(id = info$id, new_value = info$value))
    }
  })
  
  observeEvent(input$modal_save, {
    # Allow empty values for testing purposes
    req(!is.null(input$new_value))
    req(is_selectable_table(input$file))
    
    # FASE 6 FIX: Werk met actieve tab data, niet met volledige mappingData
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
    
    # Als we elements bewerken, update ook de checkbox/radiobutton mappings
    if (input$file == "elements" && length(tabState$tabs) > 0) {
      mappingData[["elements"]] <<- consolidate_tabs_with_metadata(tabState$tabs)
      updateCheckboxMapping()
      updateRadioMapping()
    }
    
    removeModal()
    render_table(new_data, input$file)
  })
  
  observeEvent(input$modal_dbl_save, {
    # Allow empty values for consistency with modal_save
    req(!is.null(input$new_option))
    req(input$dropdown_dblclick$row, input$dropdown_dblclick$col)
    req(is_selectable_table(input$file))
    
    # FASE 6 FIX: Werk met actieve tab data, niet met volledige mappingData
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
    
    # Als we elements bewerken, update ook de checkbox/radiobutton mappings
    if (input$file == "elements" && length(tabState$tabs) > 0) {
      mappingData[["elements"]] <<- consolidate_tabs_with_metadata(tabState$tabs)
      updateCheckboxMapping()
      updateRadioMapping()
    }
    
    session$sendCustomMessage("updateSelectedOption",
                              list(id = input$dropdown_dblclick$id, new_value = input$new_option))
    
    removeModal()
    render_table(new_data, input$file)
  })
  
  # Save: write exclusively to the database and then update the CSV files
  observeEvent(input$save, {
    # FASE 6.5.4: Update mappingData voor elements, checkboxes en radiobuttons
    if (input$file == "elements" && length(tabState$tabs) > 0) {
      # Consolideer alle tabs voor elements
      mappingData[[input$file]] <<- consolidate_tabs_with_metadata(tabState$tabs)
      
      # Run auto-fill om radiobuttons/checkboxes bij te werken op basis van nieuwe elements data
      updateCheckboxMapping()
      updateRadioMapping()
    } else if (input$file %in% c("waarde_checkboxes", "waarde_radiobuttons") && length(tabState$tabs) > 0) {
      # Voor checkboxes/radiobuttons: consolideer ook hun tabs naar mappingData
      # Anders gaan handmatige edits verloren bij save!
      mappingData[[input$file]] <<- consolidate_tabs_with_metadata(tabState$tabs)
    }
    
    for (tableName in names(mappingData)) {
      dt <- mappingData[[tableName]]
      
      # Verwijder lege rijen voor elements tabel
      if (tableName == "elements") {
        dt <- dt[!is.na(Element)]
      }
      
      # Verwijder lege rijen (rijen waar alle data kolommen NA of leeg zijn, exclusief metadata)
      data_cols <- setdiff(names(dt), c("tab_name_meta", "tab_order_meta"))
      if (length(data_cols) > 0 && nrow(dt) > 0) {
        # Bepaal welke rijen tenminste één non-NA, non-empty waarde hebben
        keep_rows <- rep(FALSE, nrow(dt))
        for (col in data_cols) {
          col_vals <- dt[[col]]
          keep_rows <- keep_rows | (!is.na(col_vals) & col_vals != "")
        }
        dt <- dt[keep_rows, ]
      }
      
      dbWriteTable(con, tableName, dt, overwrite = TRUE)
    }

    # Note: updateCheckboxMapping() en updateRadioMapping() worden al aangeroepen
    # in de if-statement hierboven (regel 5623-5624) wanneer input$file == "elements"
    # Verwijderd om dubbele aanroepen te voorkomen die dubbele rijen veroorzaken
    
    # Update the CSV files from the database (central paths)
    database_to_csv(dataFolder = epc_path("mapping_dir"), dbPath = epc_path("mapping_db"))
    
    showModalSafe(modalDialog(
      title = "Success",
      "Database and CSV files have been saved successfully!"
    ))
  })

  # Select Epic input file
  observeEvent(input$select_epic_file, {
    showModalSafe(modalDialog(
      title = "Manage Input Files",
      tagList(
        h4("Select File Type:"),
        selectInput(
          "input_file_type",
          NULL,
          choices = c(
            "Epic Export" = "epic_export",
            "Biobank Data" = "biobank_data",
            "Follow-up Data" = "follow_up"
          ),
          width = "100%"
        ),
        hr(),
        h4("Available Files:"),
        uiOutput("available_files_ui"),
        hr(),
        h4("Upload New File:"),
        fileInput(
          "upload_input_file",
          NULL,
          accept = c(".csv", ".xlsx"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        uiOutput("upload_validation_ui")  # Add validation UI here
      ),
      footer = tagList(
        modalButton("Close"),
        actionButton("confirm_file_selection", "Select & Close", class = "btn btn-primary",
                     title = "Upload new file (if selected) and/or select file from list")
      ),
      size = "l",
      easyClose = FALSE
    ))
  })
  
  # Render available files based on selected type
  output$available_files_ui <- renderUI({
    req(input$input_file_type)
    
    # Get directory path based on type
    dir_path <- switch(input$input_file_type,
      "epic_export" = epc_path("epic_input_data_dir"),
      "biobank_data" = epc_path("biobank_input_data_dir"),
      "follow_up" = file.path("input_data", "follow_up"),
      epc_path("epic_input_data_dir")
    )
    
    if (!dir.exists(dir_path)) {
      return(tags$div(
        style = "padding: 10px; background: #fff3cd; border: 1px solid #ffc107; border-radius: 4px;",
        icon("exclamation-triangle"), " Directory not found: ", dir_path
      ))
    }
    
    files <- list.files(dir_path, pattern = "\\.(csv|xlsx)$", full.names = FALSE, ignore.case = TRUE)
    
    if (length(files) == 0) {
      return(tags$div(
        style = "padding: 10px; background: #e7f3ff; border: 1px solid #0066cc; border-radius: 4px;",
        icon("info-circle"), " No files found in this directory"
      ))
    }
    
    # Create a list of files with select buttons
    file_items <- lapply(files, function(file) {
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; padding: 8px; border-bottom: 1px solid #eee;",
        tags$div(
          style = "flex-grow: 1;",
          icon("file-alt"), 
          tags$span(style = "margin-left: 8px;", file)
        ),
        tags$div(
          style = "display: flex; gap: 5px;",
          actionButton(
            paste0("select_file_", gsub("[^A-Za-z0-9]", "_", file)),
            "Select",
            class = "btn btn-sm btn-primary",
            onclick = sprintf("Shiny.setInputValue('file_to_select', '%s', {priority: 'event'}); Shiny.setInputValue('file_type_context', '%s', {priority: 'event'});", file, input$input_file_type)
          ),
          actionButton(
            paste0("delete_file_", gsub("[^A-Za-z0-9]", "_", file)),
            icon("trash"),
            class = "btn btn-sm btn-danger",
            onclick = sprintf("Shiny.setInputValue('file_to_delete', '%s', {priority: 'event'}); Shiny.setInputValue('file_type_context', '%s', {priority: 'event'});", file, input$input_file_type),
            title = "Delete file"
          )
        )
      )
    })
    
    tags$div(
      style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; border-radius: 4px;",
      file_items
    )
  })
  
  # Store pending upload info
  pending_upload_info <- reactiveVal(NULL)
  
  # Handle file upload with validation - show inline feedback
  observeEvent(input$upload_input_file, {
    req(input$upload_input_file, input$input_file_type)
    
    uploaded <- input$upload_input_file
    
    # Show validation in progress
    output$upload_validation_ui <- renderUI({
      div(
        style = "margin-top: 10px; padding: 10px; background: #e7f3ff; border: 1px solid #0066cc; border-radius: 4px;",
        icon("spinner", class = "fa-spin"), " Validating file..."
      )
    })
    
    target_dir <- switch(input$input_file_type,
      "epic_export" = epc_path("epic_input_data_dir"),
      "biobank_data" = epc_path("biobank_input_data_dir"),
      "follow_up" = file.path("input_data", "follow_up"),
      epc_path("epic_input_data_dir")
    )
    
    # Create directory if it doesn't exist
    if (!dir.exists(target_dir)) {
      dir.create(target_dir, recursive = TRUE)
    }
    
    target_path <- file.path(target_dir, uploaded$name)
    
    # Check if file already exists
    if (file.exists(target_path)) {
      output$upload_validation_ui <- renderUI({
        div(
          style = "margin-top: 10px; padding: 10px; background: #fff3cd; border: 1px solid #ffc107; border-radius: 4px;",
          p(icon("exclamation-triangle"), " File already exists", style = "color: #856404; font-weight: bold; margin: 0;"),
          p(sprintf("A file named '%s' already exists in this directory.", uploaded$name),
            style = "margin: 5px 0 0 0; color: #856404;")
        )
      })
      return()
    }
    
    # Validate file columns before uploading
    tryCatch({
      file_ext <- tolower(tools::file_ext(uploaded$name))
      
      # Read the file to check columns
      test_data <- NULL
      if (file_ext == "csv") {
        test_data <- readr::read_csv2(uploaded$datapath, n_max = 1, col_types = cols(), 
                                      show_col_types = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."))
      } else if (file_ext == "xlsx") {
        test_data <- readxl::read_excel(uploaded$datapath, n_max = 1)
      } else {
        output$upload_validation_ui <- renderUI({
          div(
            style = "margin-top: 10px; padding: 10px; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;",
            p(icon("times-circle"), " Invalid file type", style = "color: #721c24; font-weight: bold; margin: 0;"),
            p("Only .csv and .xlsx files are supported.", style = "margin: 5px 0 0 0; color: #721c24;")
          )
        })
        return()
      }
      
      # Determine expected columns based on file type
      expected_cols <- NULL
      epic_tabel <- NULL
      
      if (input$input_file_type == "epic_export") {
        epic_tabel <- "EpicExport"
      } else if (input$input_file_type == "biobank_data") {
        # Check filename to determine if it's biobank_data or MDNS
        if (grepl("biobank", uploaded$name, ignore.case = TRUE)) {
          epic_tabel <- "biobank_data"
        } else if (grepl("MDNS", uploaded$name, ignore.case = TRUE)) {
          epic_tabel <- "MDNS"
        } else {
          epic_tabel <- "biobank_data"  # Default to biobank_data
        }
      } else if (input$input_file_type == "follow_up") {
        epic_tabel <- "FollowUp"
      }
      
      # Query expected columns from variabelen table
      if (!is.null(epic_tabel)) {
        variabelen_query <- sprintf(
          "SELECT DISTINCT epic_kolom FROM variabelen WHERE epic_tabel = '%s' AND epic_kolom != ''",
          epic_tabel
        )
        variabelen_result <- tryCatch(
          dbGetQuery(con, variabelen_query),
          error = function(e) NULL
        )
        
        if (!is.null(variabelen_result) && nrow(variabelen_result) > 0) {
          expected_cols <- variabelen_result$epic_kolom
        }
      }
      
      # Validate columns if we have expected columns
      if (!is.null(expected_cols) && length(expected_cols) > 0) {
        actual_cols <- colnames(test_data)
        missing_cols <- setdiff(expected_cols, actual_cols)
        
        if (length(missing_cols) > 0) {
          # Validation failed - show inline warning
          output$upload_validation_ui <- renderUI({
            div(
              style = "margin-top: 10px; padding: 10px; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;",
              p(icon("exclamation-triangle"), " Column validation failed", 
                style = "color: #721c24; font-weight: bold; margin: 0;"),
              p(sprintf("The file is missing %d required column(s). You can still upload it using 'Select & Close'.", 
                length(missing_cols)),
                style = "margin: 5px 0; color: #721c24;"),
              tags$details(
                style = "margin-top: 8px;",
                tags$summary(
                  sprintf("Show missing columns (%d)", length(missing_cols)), 
                  style = "cursor: pointer; color: #721c24; font-weight: bold;"
                ),
                tags$div(
                  style = "margin-top: 8px; max-height: 150px; overflow-y: auto; background: #fff; padding: 8px; border-radius: 3px;",
                  tags$ul(
                    style = "margin: 0; padding-left: 20px;",
                    lapply(head(missing_cols, 30), function(col) tags$li(code(col)))
                  ),
                  if (length(missing_cols) > 30) {
                    tags$p(sprintf("... and %d more", length(missing_cols) - 30), 
                           style = "color: #721c24; margin: 5px 0 0 0;")
                  }
                )
              )
            )
          })
          
          # Store upload info for later upload via Select & Close
          pending_upload_info(list(
            datapath = uploaded$datapath,
            target_path = target_path,
            name = uploaded$name,
            type = input$input_file_type,
            has_warnings = TRUE
          ))
          
          return()
        }
      }
      
      # Validation passed - show success
      output$upload_validation_ui <- renderUI({
        div(
          style = "margin-top: 10px; padding: 10px; background: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;",
          p(icon("check-circle"), " File is valid and ready to upload", 
            style = "color: #155724; font-weight: bold; margin: 0;"),
          p(sprintf("File: %s - Click 'Select & Close' to upload.", uploaded$name), 
            style = "margin: 5px 0 0 0; color: #155724; font-size: 0.9em;")
        )
      })
      
      # Store upload info for upload via Select & Close
      pending_upload_info(list(
        datapath = uploaded$datapath,
        target_path = target_path,
        name = uploaded$name,
        type = input$input_file_type,
        has_warnings = FALSE
      ))
      
    }, error = function(e) {
      output$upload_validation_ui <- renderUI({
        div(
          style = "margin-top: 10px; padding: 10px; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px;",
          p(icon("times-circle"), " Validation error", style = "color: #721c24; font-weight: bold; margin: 0;"),
          p(sprintf("Error: %s", e$message), style = "margin: 5px 0 0 0; color: #721c24;")
        )
      })
    })
  })
  
  # Store selected Epic file and its type (persistent)
  selected_epic_file <- reactiveVal(NULL)
  selected_file_type <- reactiveVal(NULL)
  
  # Temporary selection (before confirmation)
  temp_selected_file <- reactiveVal(NULL)
  temp_selected_type <- reactiveVal(NULL)
  
  # Handle file selection (temporary, not confirmed yet)
  observeEvent(input$file_to_select, {
    req(input$file_to_select, input$file_type_context)
    
    # Store temporarily
    temp_selected_file(input$file_to_select)
    temp_selected_type(input$file_type_context)
    
    # Visual feedback that file is selected (highlight in UI would go here if needed)
    # For now just store it
  })
  
  # Confirm file selection and close modal (also handles file upload if present)
  observeEvent(input$confirm_file_selection, {
    upload_info <- pending_upload_info()
    
    # Step 1: Handle file upload if there's a pending upload
    if (!is.null(upload_info)) {
      tryCatch({
        # Perform the upload
        file.copy(upload_info$datapath, upload_info$target_path, overwrite = FALSE)
        
        notification_type <- if (isTRUE(upload_info$has_warnings)) "warning" else "message"
        notification_icon <- if (isTRUE(upload_info$has_warnings)) "exclamation-triangle" else "check-circle"
        
        showNotification(
          ui = tagList(
            icon(notification_icon),
            sprintf(" File '%s' uploaded%s", 
                    upload_info$name,
                    if (isTRUE(upload_info$has_warnings)) " with validation warnings" else " successfully")
          ),
          type = notification_type,
          duration = 5
        )
        
        # Auto-select the uploaded file
        selected_epic_file(upload_info$name)
        selected_file_type(upload_info$type)
        
        # Reload option lists if an epic_export file was uploaded
        # This ensures dropdown options reflect the new data
        if (upload_info$type == "epic_export") {
          tryCatch({
            options(epic2castor.force_option_reload = TRUE)
            local_env <- new.env(parent = globalenv())
            sys.source(file.path(epc_path("scripts_dir"), "option_lists2.R"), envir = local_env)
            option_data <<- local_env$option_data
            checkBoxesValues <<- local_env$checkBoxesValues
            radioButtonOptionValues <<- local_env$radioButtonOptionValues
            checkboxes <<- local_env$checkboxes
            radiobuttons <<- local_env$radiobuttons
            metaRadioButtons <<- local_env$metaRadioButtons
            metaVariables <<- local_env$metaVariables
            
            # Re-render current table to update existing dropdowns with new options
            if (!is.null(input$file) && is_selectable_table(input$file)) {
              active_data <- get_active_tab_data()
              if (!is.null(active_data) && nrow(active_data) > 0) {
                render_table(active_data, input$file, mode = "full")
              }
            }
          }, error = function(e) {
            cat(sprintf("[FileUpload] Warning: Failed to reload option lists: %s\n", conditionMessage(e)))
          })
        }
        
        # Clear pending upload and temporary selection
        pending_upload_info(NULL)
        temp_selected_file(NULL)
        temp_selected_type(NULL)
        
        removeModalSafe()
        return()
        
      }, error = function(e) {
        showNotification(
          sprintf("Error uploading file: %s", e$message),
          type = "error",
          duration = 8
        )
        return()
      })
    }
    
    # Step 2: If no upload, check if a file from the list was selected
    if (is.null(temp_selected_file())) {
      showNotification(
        "Please select a file from the list or upload a new file.",
        type = "warning",
        duration = 4
      )
      return()
    }
    
    # Confirm the selection from the list
    selected_epic_file(temp_selected_file())
    selected_file_type(temp_selected_type())
    
    # Clear temporary selection
    temp_selected_file(NULL)
    temp_selected_type(NULL)
    
    removeModalSafe()
    
    showNotification(
      ui = tagList(
        icon("check-circle"),
        sprintf(" Selected: %s (%s)", selected_epic_file(), 
                switch(selected_file_type(),
                  "epic_export" = "Epic Export",
                  "biobank_data" = "Biobank Data",
                  "follow_up" = "Follow-up Data",
                  "Unknown"))
      ),
      duration = 5,
      type = "message"
    )
  })
  
  # Clear temporary selection when modal is dismissed without confirmation
  observe({
    # This will trigger whenever the modal state changes
    # We use input$shiny_modal to detect when modal is closed
    if (is.null(input$shiny_modal) || isFALSE(input$shiny_modal)) {
      temp_selected_file(NULL)
      temp_selected_type(NULL)
    }
  })
  
  # Handle file deletion
  observeEvent(input$file_to_delete, {
    req(input$file_to_delete, input$file_type_context)
    
    dir_path <- switch(input$file_type_context,
      "epic_export" = epc_path("epic_input_data_dir"),
      "biobank_data" = epc_path("biobank_input_data_dir"),
      "follow_up" = file.path("input_data", "follow_up"),
      epc_path("epic_input_data_dir")
    )
    
    file_path <- file.path(dir_path, input$file_to_delete)
    
    showModalSafe(modalDialog(
      title = "Confirm Deletion",
      tagList(
        p(icon("exclamation-triangle", style = "color: #dc3545;"), 
          " Are you sure you want to delete this file?"),
        tags$div(
          style = "padding: 10px; background: #f8f9fa; border-radius: 4px; margin: 10px 0;",
          tags$strong("File: "), input$file_to_delete,
          tags$br(),
          tags$strong("Type: "), switch(input$file_type_context,
            "epic_export" = "Epic Export",
            "biobank_data" = "Biobank Data",
            "follow_up" = "Follow-up Data",
            "Unknown")
        ),
        p(style = "color: #dc3545;", "This action cannot be undone!")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_file", "Delete", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_delete_file, {
    req(input$file_to_delete, input$file_type_context)
    
    dir_path <- switch(input$file_type_context,
      "epic_export" = epc_path("epic_input_data_dir"),
      "biobank_data" = epc_path("biobank_input_data_dir"),
      "follow_up" = file.path("input_data", "follow_up"),
      epc_path("epic_input_data_dir")
    )
    
    file_path <- file.path(dir_path, input$file_to_delete)
    file_to_show <- input$file_to_delete
    
    removeModalSafe()
    
    tryCatch({
      if (file.exists(file_path)) {
        file.remove(file_path)
        
        # If this was the selected file, clear selection
        if (!is.null(selected_epic_file()) && selected_epic_file() == input$file_to_delete) {
          selected_epic_file(NULL)
          selected_file_type(NULL)
        }
        
        showNotification(
          ui = tagList(
            icon("check-circle"),
            sprintf(" File '%s' deleted successfully", file_to_show)
          ),
          type = "message",
          duration = 5
        )
        
        # Reopen the file manager modal
        shinyjs::delay(200, {
          shinyjs::click("select_epic_file")
        })
      } else {
        showNotification("File not found", type = "warning", duration = 3)
      }
    }, error = function(e) {
      showNotification(
        sprintf("Error deleting file: %s", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  # Undo: reload the data exclusively from the database
  observeEvent(input$undo, {
    req(is_selectable_table(input$file))
    
    mappingData <<- setNames(
      lapply(table_names, function(tbl) {
        dt <- as.data.table(dbReadTable(con, tbl))
        
        # FASE 6.5: Behoud metadata voor alle selectable tabellen
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
    
    # FASE 6: Herstel tabs vanuit database metadata
    data <- mappingData[[input$file]]
    tabState$tabs <- restore_tabs_from_metadata(data)
    
    # Zorg ervoor dat alle tabs een order hebben
    tabState$tabs <- ensure_tab_order(tabState$tabs)
    
    tabState$activeTab <- tabState$tabs[[1]]$id
    tabState$nextTabId <- length(tabState$tabs) + 1
    
    render_table(get_active_tab_data(), input$file)
  })
}

shinyApp(ui, server, options = list(launch.browser = getOption("shiny.launch.browser", interactive())))

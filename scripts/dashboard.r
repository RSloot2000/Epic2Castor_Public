# ============================================================================
# EPIC2CASTOR - DASHBOARD MODULE
# ============================================================================
#
# Purpose:
#   Provides dashboard functionality for the Epic2Castor application:
#   1. Patient Inclusion: Track inclusion over time, per site, totals
#   2. Data Completeness: Per-record field completion, per-form heatmap
#   3. Biobank Samples: Sample counts by type, status, per patient
#
# Architecture:
#   - Uses Castor REST API (same OAuth2 flow as CastorRetrieval.r)
#   - Caches API responses to avoid excessive calls
#   - Provides UI-generating functions and server-side logic
#   - Integrated into App.r via source() + dashboard modal
#
# Dependencies:
#   - httr, jsonlite (API calls)
#   - data.table (data manipulation)
#   - ggplot2 (static plots fallback)
#   - plotly (interactive plots, optional)
#   - DT (data tables)
#   - config.R (epc_path helper)
#
# Usage:
#   source("scripts/dashboard.r")  # from App.r
#   Then call dashboard_server(input, output, session) in server function
# ============================================================================

# ============================================================================
# CONSTANTS
# ============================================================================

DASHBOARD_CACHE_DIR <- file.path("db", "dashboard_cache")
DASHBOARD_CACHE_MAX_AGE_MINS <- 30
CASTOR_API_BASE <- "https://data.castoredc.com"

# Lab reference ranges extracted from lab_data.csv (109,193 rows, 341 patients, 32 lab tests)
# Two sex-specific ranges per test (F = female, M = male) because sex is unavailable in Castor.
# Three-level warning: green (within both), orange (within one), red (outside all).
# type: "range" = low-high, "upper" = normal < X, "lower" = normal > X
LAB_REFERENCE_RANGES <- list(
  lab_haemoglobin   = list(label = "Hemoglobin",     unit = "mmol/L",   type = "range", low_f = 7.4,  high_f = 9.9,   low_m = 8.4,  high_m = 10.8,  sex_dep = TRUE),
  lab_haematocrit   = list(label = "Hematocrit",     unit = "L/L",      type = "range", low_f = 0.36, high_f = 0.46,  low_m = 0.41, high_m = 0.53,  sex_dep = TRUE),
  lab_erythrocytes  = list(label = "Erythrocytes",   unit = "10^12/L",  type = "range", low_f = 4.00, high_f = 5.20,  low_m = 4.50, high_m = 5.90,  sex_dep = TRUE),
  lab_mcv           = list(label = "MCV",            unit = "fL",       type = "range", low_f = 80,   high_f = 100,   low_m = 80,   high_m = 100,   sex_dep = FALSE),
  lab_mch           = list(label = "MCH",            unit = "fmol",     type = "range", low_f = 1.70, high_f = 2.00,  low_m = 1.70, high_m = 2.10,  sex_dep = TRUE),
  lab_mchc          = list(label = "MCHC",           unit = "mmol/L",   type = "range", low_f = 20.0, high_f = 21.5,  low_m = 20.0, high_m = 21.5,  sex_dep = FALSE),
  lab_rdw           = list(label = "RDW",            unit = "%",        type = "range", low_f = 12.1, high_f = 14.3,  low_m = 12.0, high_m = 13.6,  sex_dep = TRUE),
  lab_leucocytes    = list(label = "Leukocytes",     unit = "10^9/L",   type = "range", low_f = 4.0,  high_f = 10.0,  low_m = 4.0,  high_m = 10.0,  sex_dep = FALSE),
  lab_thrombocytes  = list(label = "Thrombocytes",   unit = "10^9/L",   type = "range", low_f = 150,  high_f = 400,   low_m = 150,  high_m = 400,   sex_dep = FALSE),
  lab_neutrophils   = list(label = "Neutrophils",    unit = "10^9/L",   type = "range", low_f = 1.50, high_f = 7.50,  low_m = 1.50, high_m = 7.50,  sex_dep = FALSE),
  lab_lymphocytes   = list(label = "Lymphocytes",    unit = "10^9/L",   type = "range", low_f = 1.00, high_f = 3.50,  low_m = 1.00, high_m = 3.50,  sex_dep = FALSE),
  lab_monocytes     = list(label = "Monocytes",      unit = "10^9/L",   type = "range", low_f = 0.10, high_f = 1.00,  low_m = 0.10, high_m = 1.00,  sex_dep = FALSE),
  lab_basophils_abs = list(label = "Basophils",      unit = "10^9/L",   type = "upper", high_f = 0.20, high_m = 0.20, sex_dep = FALSE),
  lab_creatinine    = list(label = "Creatinine",     unit = "\u00b5mol/L", type = "range", low_f = 45,  high_f = 90,   low_m = 60,   high_m = 110,  sex_dep = TRUE),
  lab_urea          = list(label = "Urea",           unit = "mmol/L",   type = "range", low_f = 2.5,  high_f = 7.0,   low_m = 2.5,  high_m = 7.0,   sex_dep = FALSE),
  lab_mdrd_gfr      = list(label = "MDRD-GFR",      unit = "ml/min",   type = "lower", low_f = 60,   low_m = 60,     sex_dep = FALSE),
  lab_cdk_epi_gfr   = list(label = "CKD-EPI-GFR",   unit = "ml/min",   type = "lower", low_f = 90,   low_m = 90,     sex_dep = FALSE),
  lab_asat          = list(label = "ASAT",           unit = "U/L",      type = "upper", high_f = 30,  high_m = 35,    sex_dep = TRUE),
  lab_alat          = list(label = "ALAT",           unit = "U/L",      type = "upper", high_f = 35,  high_m = 45,    sex_dep = TRUE),
  lab_gamma_gt      = list(label = "Gamma-GT",       unit = "U/L",      type = "upper", high_f = 40,  high_m = 55,    sex_dep = TRUE),
  lab_ldh           = list(label = "LDH",            unit = "U/L",      type = "upper", high_f = 250, high_m = 250,   sex_dep = FALSE),
  lab_sodium        = list(label = "Sodium",         unit = "mmol/L",   type = "range", low_f = 135,  high_f = 145,   low_m = 135,  high_m = 145,   sex_dep = FALSE),
  lab_potassium     = list(label = "Potassium",      unit = "mmol/L",   type = "range", low_f = 3.5,  high_f = 4.7,   low_m = 3.5,  high_m = 4.7,   sex_dep = FALSE),
  lab_albumin       = list(label = "Albumin",        unit = "g/L",      type = "range", low_f = 35,   high_f = 50,    low_m = 35,   high_m = 50,    sex_dep = FALSE),
  lab_glucose       = list(label = "Glucose",        unit = "mmol/L",   type = "range", low_f = 4.0,  high_f = 5.6,   low_m = 4.0,  high_m = 5.6,   sex_dep = FALSE),
  lab_hba1c         = list(label = "HbA1c",          unit = "mmol/mol", type = "range", low_f = 20,   high_f = 42,    low_m = 20,   high_m = 42,    sex_dep = FALSE),
  lab_ferritin      = list(label = "Ferritin",       unit = "\u00b5g/L",type = "range", low_f = 10,   high_f = 200,   low_m = 20,   high_m = 300,   sex_dep = TRUE),
  lab_crp           = list(label = "CRP",            unit = "mg/L",     type = "upper", high_f = 10,  high_m = 10,    sex_dep = FALSE),
  lab_esr           = list(label = "ESR",            unit = "mm/hr",    type = "upper", high_f = 30,  high_m = 15,    sex_dep = TRUE),
  lab_triglycerides = list(label = "Triglycerides",  unit = "mmol/L",   type = "range", low_f = 0.80, high_f = 2.00,  low_m = 0.80, high_m = 2.00,  sex_dep = FALSE),
  lab_fibrinogen    = list(label = "Fibrinogen",     unit = "mg/L",     type = "range", low_f = 1600, high_f = 3200,  low_m = 1826, high_m = 4205,  sex_dep = TRUE)
)

# Clinical grouping for display
LAB_GROUPS <- list(
  "Hematology"      = c("lab_haemoglobin", "lab_leucocytes", "lab_thrombocytes",
                         "lab_neutrophils", "lab_lymphocytes", "lab_monocytes", "lab_basophils_abs"),
  "Renal Function"  = c("lab_creatinine", "lab_cdk_epi_gfr", "lab_mdrd_gfr", "lab_urea"),
  "Liver Function"  = c("lab_asat", "lab_alat", "lab_gamma_gt", "lab_ldh", "lab_albumin"),
  "Infection"       = c("lab_crp", "lab_esr", "lab_fibrinogen"),
  "Electrolytes"    = c("lab_sodium", "lab_potassium"),
  "Metabolism"      = c("lab_glucose", "lab_hba1c", "lab_triglycerides", "lab_ferritin")
)

# Primary lab variables shown in summary table with sparklines
LAB_PRIMARY_VARS <- c("lab_haemoglobin", "lab_leucocytes", "lab_thrombocytes",
                       "lab_creatinine", "lab_cdk_epi_gfr", "lab_crp")

#' Check a lab value against reference ranges (three-level: green/orange/red)
#'
#' @param value Numeric lab value
#' @param ref_name Character: key in LAB_REFERENCE_RANGES
#' @return List with status ("normal", "possibly_abnormal", "abnormal"), color, and message
lab_check_value <- function(value, ref_name) {
  result <- list(status = "unknown", color = "#999999", message = "")
  ref <- LAB_REFERENCE_RANGES[[ref_name]]
  if (is.null(ref) || is.na(value)) return(result)
  
  val <- suppressWarnings(as.numeric(value))
  if (is.na(val)) return(result)
  
  in_range <- function(v, ref, sex) {
    if (ref$type == "range") {
      low_key  <- paste0("low_", sex)
      high_key <- paste0("high_", sex)
      return(v >= ref[[low_key]] && v <= ref[[high_key]])
    } else if (ref$type == "upper") {
      high_key <- paste0("high_", sex)
      return(v <= ref[[high_key]])
    } else if (ref$type == "lower") {
      low_key <- paste0("low_", sex)
      return(v >= ref[[low_key]])
    }
    return(TRUE)
  }
  
  in_f <- in_range(val, ref, "f")
  in_m <- in_range(val, ref, "m")
  
  if (in_f && in_m) {
    result$status  <- "normal"
    result$color   <- "#2ecc71"
    result$message <- ""
  } else if (in_f || in_m) {
    result$status  <- "possibly_abnormal"
    result$color   <- "#f39c12"
    within_sex  <- if (in_f) "female" else "male"
    outside_sex <- if (in_f) "male" else "female"
    result$message <- sprintf("Possibly outside reference range (sex-dependent): within %s range, outside %s range", within_sex, outside_sex)
  } else {
    result$status  <- "abnormal"
    result$color   <- "#e74c3c"
    result$message <- "Outside normal range for all references"
  }
  return(result)
}

# ============================================================================
# AUTHENTICATION
# ============================================================================

#' Obtain OAuth2 access token for Castor API
#'
#' Uses client credentials flow from APIConfig.json
#' Follows same pattern as CastorRetrieval.r
#'
#' @return Character string: Bearer access token
#' @throws Error if credentials are missing or invalid
dashboard_get_token <- function() {
  config_path <- epc_path("config_api")
  if (!file.exists(config_path)) {
    stop("APIConfig.json not found. Configure credentials first.", call. = FALSE)
  }
  
  config <- jsonlite::fromJSON(config_path)
  
  # Validate required fields
  required <- c("client_id", "client_secret", "study_id")
  missing <- required[!required %in% names(config) | sapply(required, function(k) is.null(config[[k]]) || config[[k]] == "")]
  if (length(missing) > 0) {
    stop(sprintf("Missing API credentials: %s. Update config/APIConfig.json.", 
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  
  token_url <- paste0(CASTOR_API_BASE, "/oauth/token")
  token_response <- httr::RETRY(
    "POST", token_url,
    body = list(
      client_id     = config$client_id,
      client_secret = config$client_secret,
      grant_type    = "client_credentials"
    ),
    encode = "form",
    httr::timeout(30),
    times = 3
  )
  
  if (httr::status_code(token_response) != 200) {
    stop("Failed to obtain Castor API token. Check your credentials.", call. = FALSE)
  }
  
  token_data <- httr::content(token_response, as = "parsed", encoding = "UTF-8")
  return(token_data$access_token)
}

#' Get study_id from API config
#'
#' @return Character string: Castor study ID
dashboard_get_study_id <- function() {
  config <- jsonlite::fromJSON(epc_path("config_api"))
  config$study_id
}

# ============================================================================
# CACHING LAYER
# ============================================================================

#' Ensure dashboard cache directory exists
dashboard_ensure_cache_dir <- function() {
  dir.create(DASHBOARD_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
}

#' Read cached data if still fresh
#'
#' @param cache_key Character: unique name for this data (e.g., "records", "biobank")
#' @param max_age_mins Numeric: maximum age in minutes before refresh (default: 30)
#' @return Cached data (list/data.frame), or NULL if expired/missing
dashboard_read_cache <- function(cache_key, max_age_mins = DASHBOARD_CACHE_MAX_AGE_MINS) {
  dashboard_ensure_cache_dir()
  cache_file <- file.path(DASHBOARD_CACHE_DIR, paste0(cache_key, ".rds"))
  
  if (!file.exists(cache_file)) return(NULL)
  
  file_age_mins <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "mins"))
  if (file_age_mins > max_age_mins) return(NULL)
  
  tryCatch(readRDS(cache_file), error = function(e) NULL)
}

#' Write data to cache
#'
#' @param cache_key Character: unique name for this data
#' @param data Data to cache (any serializable R object)
dashboard_write_cache <- function(cache_key, data) {
  dashboard_ensure_cache_dir()
  cache_file <- file.path(DASHBOARD_CACHE_DIR, paste0(cache_key, ".rds"))
  tryCatch(saveRDS(data, cache_file), error = function(e) {
    cat(sprintf("[Dashboard] Warning: Failed to write cache '%s': %s\n", cache_key, e$message))
  })
}

#' Clear all dashboard cache files
dashboard_clear_cache <- function() {
  if (dir.exists(DASHBOARD_CACHE_DIR)) {
    files <- list.files(DASHBOARD_CACHE_DIR, full.names = TRUE)
    file.remove(files)
    cat("[Dashboard] Cache cleared.\n")
  }
}

# ============================================================================
# API DATA FETCHING
# ============================================================================

#' Helper: Make authenticated GET request to Castor API with pagination
#'
#' Handles paginated responses automatically, collecting all pages
#'
#' @param endpoint Character: API endpoint path (e.g., "/api/study/{id}/record")
#' @param token Character: Bearer access token
#' @param query List: additional query parameters
#' @return List with all collected items from paginated response
dashboard_api_get_all <- function(endpoint, token, query = list()) {
  headers <- httr::add_headers(Authorization = paste("Bearer", token))
  all_items <- list()
  page <- 1
  
  repeat {
    query$page <- page
    url <- paste0(CASTOR_API_BASE, endpoint)
    
    response <- httr::RETRY(
      "GET", url,
      headers,
      query = query,
      httr::timeout(60),
      times = 3
    )
    
    if (httr::status_code(response) != 200) {
      warning(sprintf("[Dashboard] API error %d on %s (page %d)", 
                       httr::status_code(response), endpoint, page))
      break
    }
    
    parsed <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    
    # Handle embedded response format
    if ("_embedded" %in% names(parsed)) {
      items_key <- setdiff(names(parsed[["_embedded"]]), character(0))
      if (length(items_key) > 0) {
        items <- parsed[["_embedded"]][[items_key[1]]]
        all_items <- c(all_items, items)
      }
    } else if (is.list(parsed) && length(parsed) > 0) {
      # Direct list response
      all_items <- c(all_items, list(parsed))
      break
    }
    
    # Check for next page
    total_pages <- parsed$page_count %||% 1
    if (page >= total_pages) break
    page <- page + 1
  }
  
  return(all_items)
}

#' Fetch all records (patients) from Castor
#'
#' Retrieves record list with metadata: id, creation date, site, status, etc.
#' Uses caching to avoid repeated API calls.
#'
#' @param force_refresh Logical: bypass cache and fetch fresh data
#' @return data.table with columns: record_id, participant_id, site_id, 
#'         created_on, status, archived
dashboard_fetch_records <- function(force_refresh = FALSE) {
  if (!force_refresh) {
    cached <- dashboard_read_cache("records")
    if (!is.null(cached)) {
      cat("[Dashboard] Using cached records data.\n")
      return(cached)
    }
  }
  
  cat("[Dashboard] Fetching records from Castor API...\n")
  token <- dashboard_get_token()
  study_id <- dashboard_get_study_id()
  
  endpoint <- sprintf("/api/study/%s/record", study_id)
  raw_records <- dashboard_api_get_all(endpoint, token)
  
  if (length(raw_records) == 0) {
    cat("[Dashboard] No records found.\n")
    return(data.table(
      record_id = character(0), participant_id = character(0),
      site_id = character(0), created_on = character(0),
      status = character(0), archived = logical(0)
    ))
  }
  
  # Parse into data.table
  records_dt <- rbindlist(lapply(raw_records, function(r) {
    data.table(
      record_id      = as.character(r$id %||% r$record_id %||% NA),
      participant_id = as.character(r$participant_id %||% NA),
      site_id        = as.character(r$`_embedded`$site$id %||% r$site_id %||% NA),
      site_name      = as.character(r$`_embedded`$site$name %||% r$site_name %||% NA),
      created_on     = as.character(r$created_on$date %||% r$created_on %||% NA),
      status         = as.character(r$status %||% NA),
      archived       = isTRUE(r$archived)
    )
  }), fill = TRUE)
  
  # Parse dates
  records_dt[, created_date := tryCatch(
    as.Date(substr(created_on, 1, 10), format = "%Y-%m-%d"),
    error = function(e) as.Date(NA)
  ), by = record_id]
  
  dashboard_write_cache("records", records_dt)
  cat(sprintf("[Dashboard] Fetched %d records.\n", nrow(records_dt)))
  return(records_dt)
}

#' Fetch field ID to variable name mapping from Castor API
#'
#' Uses the /export/structure endpoint to build a mapping from Field ID (UUID)
#' to Field Variable Name. Also returns form name and field type per field.
#'
#' @param token Character: access token
#' @param study_id Character: study ID
#' @return data.table with columns: field_id, field_variable_name, form_name, field_type, form_collection_name
dashboard_fetch_field_mapping <- function(token, study_id) {
  cached <- dashboard_read_cache("field_mapping")
  if (!is.null(cached)) return(cached)
  
  headers <- httr::add_headers(Authorization = paste("Bearer", token))
  url <- paste0(CASTOR_API_BASE, "/api/study/", study_id, "/export/structure")
  
  response <- tryCatch(
    httr::RETRY("GET", url, headers, httr::timeout(60), times = 3),
    error = function(e) NULL
  )
  
  if (is.null(response) || httr::status_code(response) != 200) {
    cat("[Dashboard] Failed to fetch field structure from API.\n")
    return(data.table())
  }
  
  csv_text <- httr::content(response, as = "text", encoding = "UTF-8")
  struct_dt <- tryCatch(
    data.table::fread(text = csv_text, sep = ";", header = TRUE, fill = TRUE, encoding = "UTF-8"),
    error = function(e) {
      tryCatch(
        data.table::fread(text = csv_text, sep = ",", header = TRUE, fill = TRUE, encoding = "UTF-8"),
        error = function(e2) data.table()
      )
    }
  )
  
  if (nrow(struct_dt) == 0) return(data.table())
  
  # Extract relevant columns
  fid_col <- grep("Field ID", names(struct_dt), value = TRUE, ignore.case = TRUE)
  fvar_col <- grep("Field Variable Name", names(struct_dt), value = TRUE, ignore.case = TRUE)
  form_col <- grep("^Form Name$", names(struct_dt), value = TRUE, ignore.case = TRUE)
  type_col <- grep("Field Type", names(struct_dt), value = TRUE, ignore.case = TRUE)
  coll_col <- grep("Form Collection Name", names(struct_dt), value = TRUE, ignore.case = TRUE)
  
  if (length(fid_col) == 0 || length(fvar_col) == 0) return(data.table())
  
  mapping <- data.table(
    field_id = struct_dt[[fid_col[1]]],
    field_variable_name = struct_dt[[fvar_col[1]]],
    form_name = if (length(form_col) > 0) struct_dt[[form_col[1]]] else NA_character_,
    field_type = if (length(type_col) > 0) struct_dt[[type_col[1]]] else NA_character_,
    form_collection_name = if (length(coll_col) > 0) struct_dt[[coll_col[1]]] else NA_character_
  )
  
  dashboard_write_cache("field_mapping", mapping)
  cat(sprintf("[Dashboard] Fetched field mapping: %d fields.\n", nrow(mapping)))
  return(mapping)
}

#' Fetch data points for all records (for completeness analysis)
#'
#' Uses the Castor API /export/data endpoint which returns data in LONG format:
#' one row per field per record, with columns: Study ID, Record ID, Form Type,
#' Form Instance ID, Form Instance Name, Field ID, Value, Date, User ID, Is Archived.
#'
#' Maps Field IDs (UUIDs) to Field Variable Names via /export/structure,
#' then pivots to WIDE format: one row per record, columns = field variable names.
#'
#' @param force_refresh Logical: bypass cache
#' @return data.table with one row per record, columns = field variable names
dashboard_fetch_data_points <- function(force_refresh = FALSE) {
  if (!force_refresh) {
    cached <- dashboard_read_cache("data_points")
    if (!is.null(cached)) {
      cat("[Dashboard] Using cached data points.\n")
      return(cached)
    }
  }
  
  cat("[Dashboard] Fetching study data from Castor API...\n")
  token <- dashboard_get_token()
  study_id <- dashboard_get_study_id()
  headers <- httr::add_headers(Authorization = paste("Bearer", token))
  
  # ===== STEP 1: Get field ID -> variable name mapping =====
  field_mapping <- dashboard_fetch_field_mapping(token, study_id)
  if (nrow(field_mapping) == 0) {
    cat("[Dashboard] No field mapping available, cannot process data.\n")
    return(data.table())
  }
  
  # ===== STEP 2: Get data export (long format CSV) =====
  url_export <- paste0(CASTOR_API_BASE, "/api/study/", study_id, "/export/data")
  response <- tryCatch(
    httr::RETRY("GET", url_export, headers, httr::timeout(120), times = 3),
    error = function(e) NULL
  )
  
  if (is.null(response) || httr::status_code(response) != 200) {
    cat(sprintf("[Dashboard] Export endpoint failed (status %s).\n",
                if (!is.null(response)) httr::status_code(response) else "NULL"))
    return(data.table())
  }
  
  csv_text <- httr::content(response, as = "text", encoding = "UTF-8")
  dt_long <- tryCatch(
    data.table::fread(text = csv_text, sep = ";", header = TRUE, fill = TRUE, encoding = "UTF-8"),
    error = function(e) {
      tryCatch(
        data.table::fread(text = csv_text, sep = ",", header = TRUE, fill = TRUE, encoding = "UTF-8"),
        error = function(e2) data.table()
      )
    }
  )
  
  if (nrow(dt_long) == 0) {
    cat("[Dashboard] Export returned empty data.\n")
    return(data.table())
  }
  
  cat(sprintf("[Dashboard] Export loaded: %d rows (long format).\n", nrow(dt_long)))
  
  # ===== STEP 3: Map Field IDs to variable names =====
  # Columns from export: "Record ID", "Field ID", "Value"
  rec_col <- grep("Record.ID", names(dt_long), value = TRUE, ignore.case = TRUE)
  fid_col <- grep("Field.ID", names(dt_long), value = TRUE, ignore.case = TRUE)
  val_col <- grep("^Value$", names(dt_long), value = TRUE, ignore.case = TRUE)
  
  if (length(rec_col) == 0 || length(fid_col) == 0 || length(val_col) == 0) {
    cat(sprintf("[Dashboard] Unexpected CSV columns: %s\n", paste(names(dt_long), collapse = ", ")))
    return(data.table())
  }
  
  # Filter to rows with actual field data (non-empty Field ID)
  dt_data <- dt_long[get(fid_col[1]) != ""]
  
  # Exclude archived records
  arch_col <- grep("Is.Archived|Archived", names(dt_data), value = TRUE, ignore.case = TRUE)
  if (length(arch_col) > 0) {
    dt_data <- dt_data[get(arch_col[1]) == 0 | get(arch_col[1]) == "0" | get(arch_col[1]) == FALSE]
  }
  
  # Join with field mapping to get variable names
  dt_data[, field_id_join := get(fid_col[1])]
  dt_data <- merge(dt_data, field_mapping[, .(field_id, field_variable_name)],
                   by.x = "field_id_join", by.y = "field_id", all.x = FALSE)
  
  cat(sprintf("[Dashboard] Matched %d data points to %d unique fields.\n", 
              nrow(dt_data), uniqueN(dt_data$field_variable_name)))
  
  # ===== STEP 4: Pivot to wide format =====
  # Aggregate: take first non-empty value per record + field
  dt_wide <- tryCatch(
    dcast(dt_data, get(rec_col[1]) ~ field_variable_name, 
          value.var = val_col[1], 
          fun.aggregate = function(x) {
            non_empty <- x[!is.na(x) & x != ""]
            if (length(non_empty) > 0) non_empty[1] else NA_character_
          }),
    error = function(e) {
      cat(sprintf("[Dashboard] Pivot failed: %s\n", e$message))
      data.table()
    }
  )
  
  if (nrow(dt_wide) == 0) return(data.table())
  
  # Rename the record ID column
  if (names(dt_wide)[1] != "record_id") {
    setnames(dt_wide, names(dt_wide)[1], "record_id")
  }
  
  dashboard_write_cache("data_points", dt_wide)
  cat(sprintf("[Dashboard] Data export pivoted: %d records x %d fields.\n", nrow(dt_wide), ncol(dt_wide) - 1))
  return(dt_wide)
}

#' Fetch biobank report instances from Castor API
#'
#' Uses the /export/data endpoint (which includes report data) combined with
#' report instance metadata to extract biobank samples. The export data contains
#' all form types including reports, with Form Instance Name identifying the
#' report type (e.g., "Biobank - 1", "Biobank - 2").
#'
#' @param force_refresh Logical: bypass cache
#' @return data.table with biobank sample information
dashboard_fetch_biobank <- function(force_refresh = FALSE) {
  if (!force_refresh) {
    cached <- dashboard_read_cache("biobank")
    if (!is.null(cached)) {
      cat("[Dashboard] Using cached biobank data.\n")
      return(cached)
    }
  }
  
  empty_result <- data.table(
    record_id = character(0), instance_name = character(0),
    iles_sample_id = character(0), tube_sample_id = character(0),
    sample_type = character(0), status = character(0),
    collection_date = character(0), finished_date = character(0)
  )
  
  cat("[Dashboard] Fetching biobank data from Castor API...\n")
  token <- dashboard_get_token()
  study_id <- dashboard_get_study_id()
  headers <- httr::add_headers(Authorization = paste("Bearer", token))
  
  # ===== STEP 1: Get field mapping (needed for Field ID -> variable name) =====
  field_mapping <- dashboard_fetch_field_mapping(token, study_id)
  if (nrow(field_mapping) == 0) {
    cat("[Dashboard] No field mapping available.\n")
    return(empty_result)
  }
  
  # Identify biobank field IDs from the mapping
  bb_fields <- field_mapping[form_collection_name %like% "(?i)sampling|biobank"]
  if (nrow(bb_fields) == 0) {
    # Fallback: look for bb_ prefixed fields
    bb_fields <- field_mapping[field_variable_name %like% "^bb_"]
  }
  
  if (nrow(bb_fields) == 0) {
    cat("[Dashboard] No biobank fields found in field mapping.\n")
    return(empty_result)
  }
  
  cat(sprintf("[Dashboard] Found %d biobank fields: %s\n", 
              nrow(bb_fields), paste(bb_fields$field_variable_name, collapse = ", ")))
  
  bb_field_ids <- bb_fields$field_id
  
  # ===== STEP 2: Get data from export endpoint =====
  url_export <- paste0(CASTOR_API_BASE, "/api/study/", study_id, "/export/data")
  response <- tryCatch(
    httr::RETRY("GET", url_export, headers, httr::timeout(120), times = 3),
    error = function(e) NULL
  )
  
  if (is.null(response) || httr::status_code(response) != 200) {
    cat("[Dashboard] Export endpoint failed for biobank data.\n")
    return(empty_result)
  }
  
  csv_text <- httr::content(response, as = "text", encoding = "UTF-8")
  dt_long <- tryCatch(
    data.table::fread(text = csv_text, sep = ";", header = TRUE, fill = TRUE, encoding = "UTF-8"),
    error = function(e) data.table()
  )
  
  if (nrow(dt_long) == 0) return(empty_result)
  
  # Get column names
  rec_col <- grep("Record.ID", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  fid_col <- grep("Field.ID", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  val_col <- grep("^Value$", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  inst_col <- grep("Form.Instance.Name", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  inst_id_col <- grep("Form.Instance.ID", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  arch_col <- grep("Is.Archived|Archived", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  
  # Filter to biobank field IDs only
  bb_data <- dt_long[get(fid_col) %in% bb_field_ids]
  
  # Exclude archived
  if (!is.na(arch_col)) {
    bb_data <- bb_data[get(arch_col) == 0 | get(arch_col) == "0" | get(arch_col) == FALSE]
  }
  
  if (nrow(bb_data) == 0) {
    cat("[Dashboard] No biobank data points found in export.\n")
    return(empty_result)
  }
  
  # Map Field IDs to variable names
  bb_data[, field_id_join := get(fid_col)]
  bb_data <- merge(bb_data, bb_fields[, .(field_id, field_variable_name)],
                   by.x = "field_id_join", by.y = "field_id", all.x = FALSE)
  
  # Pivot: one row per record+instance, columns = field variable names
  rec_inst_cols <- c(rec_col, inst_id_col)
  bb_wide <- tryCatch(
    dcast(bb_data, get(rec_col) + get(inst_id_col) ~ field_variable_name,
          value.var = val_col,
          fun.aggregate = function(x) { ne <- x[!is.na(x) & x != ""]; if (length(ne) > 0) ne[1] else NA_character_ }),
    error = function(e) {
      cat(sprintf("[Dashboard] Biobank pivot failed: %s\n", e$message))
      data.table()
    }
  )
  
  if (nrow(bb_wide) == 0) return(empty_result)
  
  # Rename cast columns
  cast_names <- names(bb_wide)
  if (cast_names[1] %in% c(rec_col, paste0("rec_col"))) setnames(bb_wide, 1, "record_id")
  if (grepl("^rec_col$|^get$", cast_names[1])) setnames(bb_wide, 1, "record_id")
  setnames(bb_wide, 1, "record_id")
  setnames(bb_wide, 2, "instance_id")
  
  # Add instance name from original data
  inst_names <- unique(bb_data[, .(inst = get(inst_id_col), iname = get(inst_col))])
  bb_wide <- merge(bb_wide, inst_names, by.x = "instance_id", by.y = "inst", all.x = TRUE)
  if ("iname" %in% names(bb_wide)) setnames(bb_wide, "iname", "instance_name")
  
  # Build clean output using actual field names from mapping
  # The field variable names from mapping are like: bb_iles_sampleid, bb_tube_sampleid, etc.
  get_bb_col <- function(dt, patterns) {
    for (pat in patterns) {
      cols <- grep(pat, names(dt), value = TRUE, ignore.case = TRUE)
      if (length(cols) > 0) return(dt[[cols[1]]])
    }
    return(rep(NA_character_, nrow(dt)))
  }
  
  biobank_dt <- data.table(
    record_id       = bb_wide$record_id,
    instance_name   = if ("instance_name" %in% names(bb_wide)) bb_wide$instance_name else NA_character_,
    iles_sample_id  = get_bb_col(bb_wide, c("iles_sampleid", "iles_sample")),
    tube_sample_id  = get_bb_col(bb_wide, c("tube_sampleid", "tube_sample")),
    sample_type     = get_bb_col(bb_wide, c("sample_type")),
    status          = get_bb_col(bb_wide, c("bb_status", "status")),
    collection_date = get_bb_col(bb_wide, c("collection_date", "date_time")),
    finished_date   = get_bb_col(bb_wide, c("bb_finished", "finished"))
  )
  
  dashboard_write_cache("biobank", biobank_dt)
  cat(sprintf("[Dashboard] Fetched %d biobank samples from API.\n", nrow(biobank_dt)))
  return(biobank_dt)
}

#' Fetch lab data repeating instances from Castor API
#'
#' Uses the /export/data endpoint to extract "MyCoS Lab" repeating data instances.
#' Returns wide-format data: one row per record + instance, columns = lab variable names.
#'
#' @param force_refresh Logical: bypass cache
#' @return data.table with lab data (wide format) including record_id, instance_id, collection_date, and lab_ columns
dashboard_fetch_lab_data <- function(force_refresh = FALSE) {
  if (!force_refresh) {
    cached <- dashboard_read_cache("lab_data")
    if (!is.null(cached)) {
      cat("[Dashboard] Using cached lab data.\n")
      return(cached)
    }
  }
  
  empty_result <- data.table(record_id = character(0), instance_id = character(0))
  
  cat("[Dashboard] Fetching lab data from Castor API...\n")
  token <- dashboard_get_token()
  study_id <- dashboard_get_study_id()
  headers <- httr::add_headers(Authorization = paste("Bearer", token))
  
  # ===== STEP 1: Get field mapping =====
  field_mapping <- dashboard_fetch_field_mapping(token, study_id)
  if (nrow(field_mapping) == 0) {
    cat("[Dashboard] No field mapping available.\n")
    return(empty_result)
  }
  
  # Identify lab fields (prefixed with lab_)
  lab_fields <- field_mapping[field_variable_name %like% "^lab_"]
  if (nrow(lab_fields) == 0) {
    # Fallback: look for form collection matching lab
    lab_fields <- field_mapping[form_collection_name %like% "(?i)lab|mycos"]
  }
  
  if (nrow(lab_fields) == 0) {
    cat("[Dashboard] No lab fields found in field mapping.\n")
    return(empty_result)
  }
  
  cat(sprintf("[Dashboard] Found %d lab fields.\n", nrow(lab_fields)))
  lab_field_ids <- lab_fields$field_id
  
  # ===== STEP 2: Get data from export endpoint =====
  url_export <- paste0(CASTOR_API_BASE, "/api/study/", study_id, "/export/data")
  response <- tryCatch(
    httr::RETRY("GET", url_export, headers, httr::timeout(120), times = 3),
    error = function(e) NULL
  )
  
  if (is.null(response) || httr::status_code(response) != 200) {
    cat("[Dashboard] Export endpoint failed for lab data.\n")
    return(empty_result)
  }
  
  csv_text <- httr::content(response, as = "text", encoding = "UTF-8")
  dt_long <- tryCatch(
    data.table::fread(text = csv_text, sep = ";", header = TRUE, fill = TRUE, encoding = "UTF-8"),
    error = function(e) data.table()
  )
  
  if (nrow(dt_long) == 0) return(empty_result)
  
  # Get column names
  rec_col  <- grep("Record.ID", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  fid_col  <- grep("Field.ID", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  val_col  <- grep("^Value$", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  inst_col <- grep("Form.Instance.Name", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  inst_id_col <- grep("Form.Instance.ID", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  arch_col <- grep("Is.Archived|Archived", names(dt_long), value = TRUE, ignore.case = TRUE)[1]
  
  # Filter to lab field IDs only
  lab_data <- dt_long[get(fid_col) %in% lab_field_ids]
  
  # Also filter on form instance name matching Lab/MyCoS Lab
  if (!is.na(inst_col)) {
    lab_data <- lab_data[get(inst_col) %like% "(?i)lab|mycos"]
  }
  
  # Exclude archived
  if (!is.na(arch_col)) {
    lab_data <- lab_data[get(arch_col) == 0 | get(arch_col) == "0" | get(arch_col) == FALSE]
  }
  
  if (nrow(lab_data) == 0) {
    cat("[Dashboard] No lab data points found in export.\n")
    return(empty_result)
  }
  
  # Map Field IDs to variable names
  lab_data[, field_id_join := get(fid_col)]
  lab_data <- merge(lab_data, lab_fields[, .(field_id, field_variable_name)],
                    by.x = "field_id_join", by.y = "field_id", all.x = FALSE)
  
  cat(sprintf("[Dashboard] Found %d lab data points for %d unique fields.\n",
              nrow(lab_data), uniqueN(lab_data$field_variable_name)))
  
  # ===== STEP 3: Pivot to wide format (one row per record + instance) =====
  lab_wide <- tryCatch(
    dcast(lab_data, get(rec_col) + get(inst_id_col) ~ field_variable_name,
          value.var = val_col,
          fun.aggregate = function(x) { ne <- x[!is.na(x) & x != ""]; if (length(ne) > 0) ne[1] else NA_character_ }),
    error = function(e) {
      cat(sprintf("[Dashboard] Lab data pivot failed: %s\n", e$message))
      data.table()
    }
  )
  
  if (nrow(lab_wide) == 0) return(empty_result)
  
  setnames(lab_wide, 1, "record_id")
  setnames(lab_wide, 2, "instance_id")
  
  # Add instance name (contains collection info)
  if (!is.na(inst_col)) {
    inst_names <- unique(lab_data[, .(inst = get(inst_id_col), iname = get(inst_col))])
    lab_wide <- merge(lab_wide, inst_names, by.x = "instance_id", by.y = "inst", all.x = TRUE)
    if ("iname" %in% names(lab_wide)) setnames(lab_wide, "iname", "instance_name")
  }
  
  # Extract collection date if available (lab_date_labs field)
  date_col <- grep("lab_date_labs|lab_collection_date|collection.date", names(lab_wide), value = TRUE, ignore.case = TRUE)
  if (length(date_col) > 0) {
    lab_wide[, collection_date := tryCatch(
      as.Date(get(date_col[1]), format = "%d-%m-%Y"),
      error = function(e) as.Date(NA)
    ), by = seq_len(nrow(lab_wide))]
    # Try alternate format
    lab_wide[is.na(collection_date), collection_date := tryCatch(
      as.Date(get(date_col[1]), format = "%Y-%m-%d"),
      error = function(e) as.Date(NA)
    ), by = seq_len(nrow(lab_wide))]
  } else {
    lab_wide[, collection_date := as.Date(NA)]
  }
  
  dashboard_write_cache("lab_data", lab_wide)
  cat(sprintf("[Dashboard] Fetched %d lab instances for %d patients.\n",
              nrow(lab_wide), uniqueN(lab_wide$record_id)))
  return(lab_wide)
}

# ============================================================================
# STUDY STRUCTURE HELPERS
# ============================================================================

#' Load study variable list (field definitions per form)
#'
#' Fetches the study structure from the Castor API (/export/structure endpoint)
#' which provides field definitions per form. Falls back to cached field mapping.
#'
#' @return data.table with field definitions
dashboard_get_study_fields <- function() {
  # Use the field mapping which is fetched from the API
  token <- tryCatch(dashboard_get_token(), error = function(e) NULL)
  study_id <- tryCatch(dashboard_get_study_id(), error = function(e) NULL)
  
  if (!is.null(token) && !is.null(study_id)) {
    mapping <- dashboard_fetch_field_mapping(token, study_id)
    if (nrow(mapping) > 0) {
      # Return in the format expected by compute_completeness_stats
      result <- data.table(
        `Field Variable Name` = mapping$field_variable_name,
        `Form Name` = mapping$form_name,
        `Field Type` = mapping$field_type,
        `Field ID` = mapping$field_id
      )
      return(result)
    }
  }
  
  # Fallback to local cached CSV if API fails
  sv_file <- epc_path("castor_study_variablelist_file")
  if (file.exists(sv_file)) {
    dt <- tryCatch(
      data.table::fread(sv_file, sep = ";", header = TRUE, fill = TRUE, encoding = "UTF-8"),
      error = function(e) {
        tryCatch(
          data.table::fread(sv_file, sep = ",", header = TRUE, fill = TRUE, encoding = "UTF-8"),
          error = function(e2) data.table()
        )
      }
    )
    return(dt)
  }
  
  warning("[Dashboard] No field definitions available.")
  return(data.table())
}

# ============================================================================
# STATISTICS COMPUTATION
# ============================================================================

#' Compute patient inclusion statistics
#'
#' Calculates:
#' - Total included patients (non-archived)
#' - Inclusion over time (cumulative curve)
#' - Inclusion per site
#'
#' @param records data.table from dashboard_fetch_records()
#' @return List with inclusion stats:
#'   - total: total active records
#'   - total_archived: total archived records
#'   - by_date: data.table with date, count, cumulative
#'   - by_site: data.table with site_name, count
#'   - by_month: data.table with month, count
dashboard_compute_inclusion_stats <- function(records) {
  empty_date <- data.table(date = as.Date(character(0)), count = integer(0), cumulative = integer(0))
  empty_site <- data.table(site_name = character(0), active = integer(0), archived = integer(0), total = integer(0))
  empty_month <- data.table(month = character(0), active = integer(0), archived = integer(0), total = integer(0))
  
  if (is.null(records) || nrow(records) == 0) {
    return(list(
      total = 0, total_archived = 0,
      by_date_active = empty_date,
      by_date_archived = empty_date,
      by_date = empty_date,
      by_site = empty_site,
      by_month = empty_month
    ))
  }
  
  active <- records[archived == FALSE]
  arch <- records[archived == TRUE]
  
  # Total counts
  total_active <- nrow(active)
  total_archived <- nrow(arch)
  
  # By date - Active (cumulative inclusion curve)
  by_date_active <- active[!is.na(created_date), .(count = .N), by = .(date = created_date)]
  setorder(by_date_active, date)
  by_date_active[, cumulative := cumsum(count)]
  
  # By date - Archived (cumulative)
  by_date_archived <- arch[!is.na(created_date), .(count = .N), by = .(date = created_date)]
  setorder(by_date_archived, date)
  if (nrow(by_date_archived) > 0) {
    by_date_archived[, cumulative := cumsum(count)]
  }
  
  # By date - All combined (for backward compat)
  by_date <- records[!is.na(created_date), .(count = .N), by = .(date = created_date)]
  setorder(by_date, date)
  by_date[, cumulative := cumsum(count)]
  
  # By site (with active/archived breakdown)
  by_site <- records[, .(
    active = sum(archived == FALSE),
    archived = sum(archived == TRUE),
    total = .N
  ), by = .(site_name)]
  by_site[is.na(site_name), site_name := "Unknown"]
  setorder(by_site, -total)
  
  # By month (with active/archived breakdown)
  by_month <- records[!is.na(created_date), .(
    active = sum(archived == FALSE),
    archived = sum(archived == TRUE),
    total = .N
  ), by = .(month = format(created_date, "%Y-%m"))]
  setorder(by_month, month)
  
  list(
    total = total_active,
    total_archived = total_archived,
    by_date_active = by_date_active,
    by_date_archived = by_date_archived,
    by_date = by_date,
    by_site = by_site,
    by_month = by_month
  )
}

#' Compute data completeness statistics
#'
#' For each record and form, calculates percentage of fields filled in.
#' A field is considered "complete" if its value is not empty/NA/null.
#'
#' @param data_export data.table from dashboard_fetch_data_points()
#' @param study_fields data.table from dashboard_get_study_fields()
#' @return List with completeness stats:
#'   - overall_pct: overall percentage complete
#'   - by_form: data.table with form_name, total_fields, filled, pct
#'   - by_record: data.table with record_id, pct_complete
#'   - by_field: data.table with field_name, pct_filled
dashboard_compute_completeness_stats <- function(data_export, study_fields) {
  if (is.null(data_export) || nrow(data_export) == 0) {
    return(list(
      overall_pct = 0,
      by_form = data.table(form_name = character(0), total_fields = integer(0), filled = integer(0), pct = numeric(0)),
      by_record = data.table(record_id = character(0), pct_complete = numeric(0)),
      by_field = data.table(field_name = character(0), pct_filled = numeric(0))
    ))
  }
  
  # Get field-to-form mapping from study variable list
  field_form_map <- data.table()
  if (nrow(study_fields) > 0) {
    # Identify column names (may vary by locale/version)
    var_col <- grep("Field Variable Name|variable_name", names(study_fields), value = TRUE, ignore.case = TRUE)
    form_col <- grep("Form Name|form_name", names(study_fields), value = TRUE, ignore.case = TRUE)
    type_col <- grep("Field Type|field_type", names(study_fields), value = TRUE, ignore.case = TRUE)
    
    if (length(var_col) > 0 && length(form_col) > 0) {
      field_form_map <- study_fields[, .SD, .SDcols = c(var_col[1], form_col[1])]
      setnames(field_form_map, c("field_variable_name", "form_name"))
    }
    
    # Exclude calculation fields (auto-computed, not user-filled)
    if (length(type_col) > 0) {
      calc_fields <- study_fields[get(type_col[1]) == "calculation", get(var_col[1])]
      field_form_map <- field_form_map[!field_variable_name %in% calc_fields]
    }
  }
  
  # Determine which columns are data fields (exclude metadata columns)
  meta_cols <- c("Participant Id", "Participant.Id", "Site Abbreviation", 
                 "Record Id", "Record.Id", "record_id",
                 "Participant Status", "Participant.Status",
                 "Randomization Group", "Randomization Datetime",
                 "Site.Abbreviation",
                 "date_baseline_chr", "parsed_date")
  data_cols <- setdiff(names(data_export), meta_cols)
  
  # Also exclude columns not in study fields (if we have field map)
  if (nrow(field_form_map) > 0) {
    known_fields <- field_form_map$field_variable_name
    data_cols <- intersect(data_cols, known_fields)
  }
  
  if (length(data_cols) == 0) {
    return(list(
      overall_pct = 0,
      by_form = data.table(),
      by_record = data.table(),
      by_field = data.table()
    ))
  }
  
  # ===== BY RECORD: percentage of fields filled =====
  record_id_col <- intersect(c("record_id", "Participant Id", "Participant.Id", "Record Id", "Record.Id"), names(data_export))
  rec_col <- if (length(record_id_col) > 0) record_id_col[1] else names(data_export)[1]
  
  by_record <- data_export[, {
    vals <- unlist(.SD, use.names = FALSE)
    filled <- sum(!is.na(vals) & vals != "" & vals != "null", na.rm = TRUE)
    total <- length(vals)
    .(pct_complete = if (total > 0) round(100 * filled / total, 1) else 0,
      n_missing = total - sum(!is.na(vals) & vals != "" & vals != "null", na.rm = TRUE),
      total_fields = total)
  }, by = rec_col, .SDcols = data_cols]
  setnames(by_record, rec_col, "record_id")
  
  # ===== BY FIELD: percentage of records that have this field filled =====
  n_records <- nrow(data_export)
  by_field <- data.table(
    field_name = data_cols,
    pct_filled = sapply(data_cols, function(col) {
      vals <- data_export[[col]]
      filled <- sum(!is.na(vals) & vals != "" & vals != "null", na.rm = TRUE)
      round(100 * filled / n_records, 1)
    })
  )
  
  # ===== BY FORM: aggregate field completeness per form =====
  by_form <- data.table()
  if (nrow(field_form_map) > 0) {
    by_field_with_form <- merge(by_field, field_form_map, 
                                by.x = "field_name", by.y = "field_variable_name",
                                all.x = TRUE)
    by_field_with_form[is.na(form_name), form_name := "Other"]
    
    by_form <- by_field_with_form[, .(
      total_fields = .N,
      avg_pct = round(mean(pct_filled, na.rm = TRUE), 1)
    ), by = form_name]
    setorder(by_form, -avg_pct)
  }
  
  # ===== OVERALL =====
  overall_pct <- round(mean(by_record$pct_complete, na.rm = TRUE), 1)
  
  list(
    overall_pct = overall_pct,
    by_form = by_form,
    by_record = by_record,
    by_field = by_field
  )
}

#' Compute biobank sample statistics
#'
#' @param biobank data.table from dashboard_fetch_biobank()
#' @return List with biobank stats:
#'   - total_samples: total count
#'   - by_type: data.table with sample_type, count
#'   - by_status: data.table with status, count
#'   - by_patient: data.table with participant_id, sample_count
#'   - unique_patients: number of unique patients with samples
dashboard_compute_biobank_stats <- function(biobank) {
  if (is.null(biobank) || nrow(biobank) == 0) {
    return(list(
      total_samples = 0,
      by_type = data.table(sample_type = character(0), count = integer(0)),
      by_status = data.table(status = character(0), count = integer(0)),
      by_patient = data.table(record_id = character(0), sample_count = integer(0)),
      unique_patients = 0
    ))
  }
  
  # By sample type
  by_type <- biobank[, .(count = .N), by = .(sample_type)]
  by_type[is.na(sample_type), sample_type := "Unknown"]
  setorder(by_type, -count)
  
  # By status
  by_status <- biobank[, .(count = .N), by = .(status)]
  by_status[is.na(status), status := "Unknown"]
  setorder(by_status, -count)
  
  # By patient (use record_id)
  by_patient <- biobank[, .(sample_count = .N), by = .(record_id)]
  by_patient[is.na(record_id), record_id := "Unknown"]
  setorder(by_patient, -sample_count)
  
  list(
    total_samples = nrow(biobank),
    by_type = by_type,
    by_status = by_status,
    by_patient = by_patient,
    unique_patients = uniqueN(biobank$record_id, na.rm = TRUE)
  )
}

#' Compute lab data statistics
#'
#' @param lab_data data.table from dashboard_fetch_lab_data()
#' @return List with lab statistics
dashboard_compute_lab_stats <- function(lab_data) {
  empty <- list(
    total_measurements = 0,
    unique_patients    = 0,
    avg_per_patient    = 0,
    last_date          = NA,
    completeness       = data.table(variable = character(0), label = character(0),
                                    group = character(0), pct = numeric(0)),
    by_month           = data.table(month = character(0), count = integer(0)),
    patient_latest     = data.table(),
    patient_sparklines = list()
  )
  
  if (is.null(lab_data) || nrow(lab_data) == 0) return(empty)
  
  # ===== KPIs =====
  total_measurements <- nrow(lab_data)
  unique_patients    <- uniqueN(lab_data$record_id, na.rm = TRUE)
  avg_per_patient    <- round(total_measurements / max(unique_patients, 1), 1)
  
  last_date <- NA
  if ("collection_date" %in% names(lab_data) && any(!is.na(lab_data$collection_date))) {
    last_date <- max(lab_data$collection_date, na.rm = TRUE)
  }
  
  # ===== Completeness per variable =====
  lab_cols <- intersect(names(lab_data), names(LAB_REFERENCE_RANGES))
  
  # Build group lookup
  group_lookup <- character(0)
  for (g in names(LAB_GROUPS)) {
    for (v in LAB_GROUPS[[g]]) group_lookup[v] <- g
  }
  
  completeness <- rbindlist(lapply(lab_cols, function(col) {
    vals <- lab_data[[col]]
    filled <- sum(!is.na(vals) & vals != "" & vals != "null", na.rm = TRUE)
    ref <- LAB_REFERENCE_RANGES[[col]]
    data.table(
      variable = col,
      label    = if (!is.null(ref)) ref$label else col,
      group    = if (col %in% names(group_lookup)) group_lookup[col] else "Other",
      pct      = round(100 * filled / nrow(lab_data), 1)
    )
  }))
  
  # ===== Measurements over time =====
  by_month <- data.table(month = character(0), count = integer(0))
  if ("collection_date" %in% names(lab_data) && any(!is.na(lab_data$collection_date))) {
    by_month <- lab_data[!is.na(collection_date), .(count = .N),
                          by = .(month = format(collection_date, "%Y-%m"))]
    setorder(by_month, month)
  }
  
  # ===== Latest values per patient (for ALL lab variables) =====
  all_lab_cols <- intersect(names(LAB_REFERENCE_RANGES), names(lab_data))
  
  patient_latest <- data.table()
  if (length(all_lab_cols) > 0) {
    # Sort by collection date descending, take first per patient
    if ("collection_date" %in% names(lab_data) && any(!is.na(lab_data$collection_date))) {
      latest <- lab_data[!is.na(collection_date)]
      setorder(latest, record_id, -collection_date)
      patient_latest <- latest[, .SD[1], by = record_id, .SDcols = c(all_lab_cols, "collection_date")]
    } else {
      # No date info: take last row per patient
      patient_latest <- lab_data[, .SD[.N], by = record_id, .SDcols = all_lab_cols]
    }
    
    # Add warning colors for each lab variable
    for (pc in all_lab_cols) {
      if (pc %in% names(patient_latest)) {
        ref_name <- pc
        patient_latest[, paste0(pc, "_status") := {
          sapply(get(pc), function(v) lab_check_value(v, ref_name)$status)
        }]
        patient_latest[, paste0(pc, "_color") := {
          sapply(get(pc), function(v) lab_check_value(v, ref_name)$color)
        }]
        patient_latest[, paste0(pc, "_msg") := {
          sapply(get(pc), function(v) lab_check_value(v, ref_name)$message)
        }]
      }
    }
  }
  
  # ===== Sparkline data per patient (time series for all lab vars) =====
  patient_sparklines <- list()
  if (length(all_lab_cols) > 0 && "collection_date" %in% names(lab_data) &&
      any(!is.na(lab_data$collection_date))) {
    spark_data <- lab_data[!is.na(collection_date), c("record_id", "collection_date", all_lab_cols), with = FALSE]
    setorder(spark_data, record_id, collection_date)
    
    for (pid in unique(spark_data$record_id)) {
      pdata <- spark_data[record_id == pid]
      patient_sparklines[[pid]] <- list()
      for (pc in all_lab_cols) {
        vals <- suppressWarnings(as.numeric(pdata[[pc]]))
        if (any(!is.na(vals))) {
          patient_sparklines[[pid]][[pc]] <- vals[!is.na(vals)]
        }
      }
    }
  }
  
  list(
    total_measurements = total_measurements,
    unique_patients    = unique_patients,
    avg_per_patient    = avg_per_patient,
    last_date          = last_date,
    completeness       = completeness,
    by_month           = by_month,
    patient_latest     = patient_latest,
    patient_sparklines = patient_sparklines
  )
}

# ============================================================================
# UI GENERATION
# ============================================================================

#' Generate the dashboard modal UI
#'
#' Creates a full-screen modal with sidebar navigation and 5 sections:
#' Overview, Patient Inclusion, Data Completeness, Lab Data, Biobank Samples
#'
#' @return Shiny modal dialog
dashboard_modal_ui <- function() {
  
  # Helper: KPI card with consistent styling
  kpi_card <- function(output_id, label, bg_color, icon_name) {
    div(class = "col-sm-3",
      div(class = "dashboard-kpi-card",
          style = sprintf("background: %s; color: white; padding: 12px 15px; border-radius: 8px; text-align: center; box-shadow: 0 2px 6px rgba(0,0,0,0.1);", bg_color),
          tags$i(class = paste("fa", icon_name), style = "font-size: 18px; opacity: 0.8;"),
          tags$h3(tags$span(id = output_id, "--"), style = "margin: 5px 0 2px 0; font-size: 22px;"),
          tags$p(label, style = "margin: 0; font-size: 12px; opacity: 0.9;")
      )
    )
  }
  
  modalDialog(
    title = tags$div(
      style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
      tags$span(
        tags$i(class = "fa fa-chart-bar", style = "margin-right: 8px;"),
        "Study Dashboard"
      ),
      actionButton("dashboard_refresh", "Refresh Data", 
                    class = "btn btn-sm btn-info",
                    icon = icon("sync"))
    ),
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),
    
    # CSS for dashboard layout
    tags$style(HTML("
      /* Wider modal for dashboard */
      .modal-dialog.modal-lg {
        width: 90vw;
        max-width: 1400px;
      }
      /* Horizontal top navigation */
      .dashboard-nav {
        display: flex;
        border-bottom: 2px solid #e0e0e0;
        margin-bottom: 15px;
        gap: 0;
      }
      .dashboard-nav .nav-item {
        padding: 10px 18px;
        color: #555;
        text-decoration: none;
        cursor: pointer;
        border-bottom: 3px solid transparent;
        font-size: 13px;
        transition: all 0.15s;
        white-space: nowrap;
        margin-bottom: -2px;
      }
      .dashboard-nav .nav-item:hover {
        background: #f0f4f8;
        color: #2c3e50;
      }
      .dashboard-nav .nav-item.active {
        color: #2980b9;
        border-bottom-color: #2980b9;
        font-weight: 600;
      }
      .dashboard-nav .nav-item i {
        margin-right: 6px;
      }
      .dashboard-content {
        padding: 0 5px;
      }
      .dashboard-section { display: none; }
      .dashboard-section.active { display: block; }
      /* KPI cards */
      .dashboard-kpi-card h3 { font-weight: 700; }
      /* Charts */
      .modal-body .row { overflow: hidden; }
      .modal-body .col-sm-3, .modal-body .col-sm-4, .modal-body .col-sm-6, .modal-body .col-sm-12 {
        overflow: hidden; box-sizing: border-box;
      }
      .modal-body .plotly { width: 100% !important; }
      /* Lab table status colors */
      .lab-green  { color: #27ae60; font-weight: 600; }
      .lab-orange { color: #e67e22; font-weight: 600; }
      .lab-red    { color: #c0392b; font-weight: 600; }
      /* Sparkline container */
      .spark-cell { display: inline-block; vertical-align: middle; }
      /* Protect modal DataTable horizontal scroll from external resize handlers */
      .modal .dataTables_scrollBody {
        overflow-x: auto !important;
      }
      .modal .dataTables_scroll table.dataTable,
      .modal .dataTables_scrollHead table.dataTable {
        width: max-content !important;
        min-width: 100% !important;
      }
      .modal .dataTables_scroll table.dataTable th,
      .modal .dataTables_scroll table.dataTable td {
        min-width: 130px;
        white-space: nowrap;
      }
      .modal .dataTables_scroll table.dataTable td:first-child,
      .modal .dataTables_scroll table.dataTable th:first-child {
        min-width: 90px;
      }
      /* Nested lab column selector modal */
      #lab_col_modal {
        z-index: 1060 !important;
      }
      #lab_col_modal .modal-backdrop {
        z-index: 1055 !important;
      }
      #lab_col_modal .modal-dialog {
        z-index: 1065;
      }
      .lab-col-checkbox:disabled + span,
      .lab-col-checkbox:disabled ~ * {
        opacity: 0.45;
      }
      label:has(.lab-col-checkbox:disabled) {
        opacity: 0.45;
        cursor: not-allowed !important;
      }
      #lab_col_limit_msg.at-limit {
        color: #e67e22 !important;
        font-weight: 600;
      }
    ")),
    
    # jQuery sparkline library (for inline trend charts in lab table)
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-sparklines/2.1.2/jquery.sparkline.min.js"),
    
    # JS for lab column selector modal
    tags$script(HTML("
      $(document).on('click', '#dash_lab_col_select_btn', function(e) {
        e.preventDefault();
        $('#lab_col_modal').modal('show');
      });

      function labColUpdateCheckboxes() {
        var MAX_COLS = 6;
        var checked = $('.lab-col-checkbox:checked');
        var unchecked = $('.lab-col-checkbox:not(:checked)');
        var msg = $('#lab_col_limit_msg');
        if (checked.length >= MAX_COLS) {
          unchecked.prop('disabled', true);
          msg.text('Maximum of ' + MAX_COLS + ' columns reached.').addClass('at-limit');
        } else {
          unchecked.prop('disabled', false);
          msg.text('Select up to ' + MAX_COLS + ' columns (' + checked.length + ' selected).').removeClass('at-limit');
        }
      }

      $(document).on('change', '.lab-col-checkbox', function() {
        labColUpdateCheckboxes();
      });

      $(document).on('shown.bs.modal', '#lab_col_modal', function() {
        labColUpdateCheckboxes();
      });

      $(document).on('click', '#lab_col_apply_btn', function() {
        var selected = [];
        $('.lab-col-checkbox:checked').each(function() {
          selected.push($(this).val());
        });
        Shiny.setInputValue('dash_lab_selected_cols', selected, {priority: 'event'});
        $('#lab_col_modal').modal('hide');
      });
    ")),

    # JS for tab switching
    tags$script(HTML("
      $(document).on('click', '.dashboard-nav .nav-item', function() {
        var target = $(this).data('target');
        $('.dashboard-nav .nav-item').removeClass('active');
        $(this).addClass('active');
        $('.dashboard-section').removeClass('active');
        $('#' + target).addClass('active');
        Shiny.setInputValue('dashboard_active_tab', target, {priority: 'event'});
        setTimeout(function() {
          var $section = $('#' + target);
          $section.find('.shiny-bound-output').trigger('resize');
          $section.find('.plotly-graph-div').each(function() {
            if (this._fullLayout) { Plotly.Plots.resize(this); }
          });
          $(window).trigger('resize');
        }, 150);
      });
    ")),
    
    # Loading indicator
    div(id = "dashboard_loading",
        style = "text-align: center; padding: 40px; display: none;",
        tags$i(class = "fa fa-spinner fa-spin fa-2x"),
        tags$p("Loading dashboard data...", style = "margin-top: 10px;")
    ),
    
    # ===== LAYOUT: TOP NAV + CONTENT =====
    div(
      
      # HORIZONTAL NAV BAR
      div(class = "dashboard-nav",
        div(class = "nav-item active", `data-target` = "dash_overview",
            tags$i(class = "fa fa-home"), "Overview"),
        div(class = "nav-item", `data-target` = "dash_inclusion",
            tags$i(class = "fa fa-user-plus"), "Inclusion"),
        div(class = "nav-item", `data-target` = "dash_completeness",
            tags$i(class = "fa fa-tasks"), "Completeness"),
        div(class = "nav-item", `data-target` = "dash_labdata",
            tags$i(class = "fa fa-flask"), "Lab Data"),
        div(class = "nav-item", `data-target` = "dash_biobank",
            tags$i(class = "fa fa-vial"), "Biobank")
      ),
      
      # CONTENT AREA
      div(class = "dashboard-content",
        
        # ===== SECTION 1: OVERVIEW =====
        div(id = "dash_overview", class = "dashboard-section active",
          tags$h4("Overview", style = "margin-top: 0;"),
          
          # Summary KPIs
          div(class = "row", style = "margin-bottom: 15px;",
            kpi_card("dash_ov_patients",     "Total Patients",  "#3498db", "fa-users"),
            kpi_card("dash_ov_completeness", "Data Complete",   "#f39c12", "fa-check-circle"),
            kpi_card("dash_ov_lab_measures", "Lab Measurements","#1abc9c", "fa-flask"),
            kpi_card("dash_ov_bio_samples",  "Biobank Samples", "#e67e22", "fa-vial")
          ),
          
          # Mini charts row
          div(class = "row", style = "margin-top: 10px;",
            div(class = "col-sm-4",
              tags$h5("Inclusion Trend"),
              plotly::plotlyOutput("dash_ov_inclusion_mini", height = "200px", width = "100%")
            ),
            div(class = "col-sm-4",
              tags$h5("Completeness by Form"),
              plotly::plotlyOutput("dash_ov_completeness_mini", height = "200px", width = "100%")
            ),
            div(class = "col-sm-4",
              tags$h5("Lab Measurements / Month"),
              plotly::plotlyOutput("dash_ov_lab_mini", height = "200px", width = "100%")
            )
          )
        ),
        
        # ===== SECTION 2: PATIENT INCLUSION =====
        div(id = "dash_inclusion", class = "dashboard-section",
          tags$h4("Patient Inclusion", style = "margin-top: 0;"),
          
          div(class = "row", style = "margin-bottom: 15px;",
            kpi_card("dash_total_patients", "Total Patients", "#3498db", "fa-users"),
            kpi_card("dash_total_active",   "Active",         "#2ecc71", "fa-user-check"),
            kpi_card("dash_total_archived", "Archived",       "#e74c3c", "fa-user-times"),
            div(class = "col-sm-3") # spacer
          ),
          
          div(class = "row",
            div(class = "col-sm-12",
              tags$h5("Inclusion Over Time"),
              plotly::plotlyOutput("dash_inclusion_curve", height = "240px", width = "100%")
            )
          ),
          
          div(class = "row", style = "margin-top: 15px;",
            div(class = "col-sm-6",
              tags$h5("Monthly Inclusion Rate"),
              plotly::plotlyOutput("dash_monthly_rate", height = "240px", width = "100%")
            ),
            div(class = "col-sm-6",
              tags$h5("Active vs Archived"),
              plotly::plotlyOutput("dash_active_vs_archived", height = "240px", width = "100%")
            )
          ),
          
          div(class = "row", style = "margin-top: 15px;",
            div(class = "col-sm-12",
              tags$h5("Monthly Inclusion Table"),
              DT::DTOutput("dash_monthly_table", height = "250px")
            )
          )
        ),
        
        # ===== SECTION 3: DATA COMPLETENESS =====
        div(id = "dash_completeness", class = "dashboard-section",
          tags$h4("Data Completeness", style = "margin-top: 0;"),
          
          div(class = "row", style = "margin-bottom: 15px;",
            kpi_card("dash_overall_completeness", "Overall Complete", "#f39c12", "fa-check-circle"),
            kpi_card("dash_total_forms",          "Forms",            "#9b59b6", "fa-file-alt"),
            kpi_card("dash_total_fields",         "Fields Tracked",   "#1abc9c", "fa-list"),
            div(class = "col-sm-3")
          ),
          
          div(class = "row",
            div(class = "col-sm-6",
              tags$h5("Completeness by Form"),
              plotly::plotlyOutput("dash_completeness_by_form", height = "240px", width = "100%")
            ),
            div(class = "col-sm-6",
              tags$h5("Completeness per Record"),
              plotly::plotlyOutput("dash_completeness_histogram", height = "240px", width = "100%")
            )
          ),
          
          div(class = "row", style = "margin-top: 15px;",
            div(class = "col-sm-6",
              tags$h5("Completeness by Record Creation Date"),
              plotly::plotlyOutput("dash_completeness_over_time", height = "240px", width = "100%")
            ),
            div(class = "col-sm-6",
              tags$h5("Missing Fields per Record"),
              plotly::plotlyOutput("dash_missing_fields_box", height = "240px", width = "100%")
            )
          ),
          
          div(class = "row", style = "margin-top: 15px;",
            div(class = "col-sm-12",
              tags$h5("Least Complete Fields (Bottom 20)"),
              DT::DTOutput("dash_incomplete_fields_table", height = "250px")
            )
          )
        ),
        
        # ===== SECTION 4: LAB DATA =====
        div(id = "dash_labdata", class = "dashboard-section",
          tags$h4("Lab Data", style = "margin-top: 0;"),
          
          div(class = "row", style = "margin-bottom: 15px;",
            kpi_card("dash_lab_total",     "Total Measurements", "#1abc9c", "fa-flask"),
            kpi_card("dash_lab_patients",  "Patients with Labs", "#3498db", "fa-user"),
            kpi_card("dash_lab_avg",       "Avg / Patient",      "#9b59b6", "fa-chart-line"),
            kpi_card("dash_lab_last_date", "Last Measurement",   "#e67e22", "fa-calendar")
          ),
          
          div(class = "row",
            div(class = "col-sm-12",
              tags$h5("Lab Completeness by Variable"),
              plotly::plotlyOutput("dash_lab_completeness", height = "300px", width = "100%")
            )
          ),
          div(class = "row", style = "margin-top: 15px;",
            div(class = "col-sm-12",
              tags$h5("Measurements Over Time"),
              plotly::plotlyOutput("dash_lab_timeline", height = "260px", width = "100%")
            )
          ),
          
          div(class = "row", style = "margin-top: 15px;",
            div(class = "col-sm-12",
              div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 4px;",
                tags$h5("Latest Values per Patient", style = "margin: 0;"),
                tags$button(
                  id = "dash_lab_col_select_btn", type = "button",
                  class = "btn btn-sm btn-default",
                  style = "padding: 2px 8px; font-size: 12px;",
                  title = "Select columns",
                  tags$i(class = "fa fa-columns", style = "margin-right: 4px;"),
                  "Columns"
                )
              ),
              tags$p("Color coding: ",
                tags$span(style = "color: #27ae60; font-weight: 600;", "Green"), " = within all reference ranges, ",
                tags$span(style = "color: #e67e22; font-weight: 600;", "Orange"), " = possibly outside (sex-dependent), ",
                tags$span(style = "color: #c0392b; font-weight: 600;", "Red"), " = outside all reference ranges",
                style = "font-size: 11px; color: #666; margin-bottom: 8px;"
              ),
              div(style = "width: 100%; overflow-x: auto; -webkit-overflow-scrolling: touch;",
                DT::DTOutput("dash_lab_patient_table", width = "auto")
              ),
              # Column selector modal (nested inside dashboard modal)
              div(id = "lab_col_modal", class = "modal fade", tabindex = "-1", role = "dialog",
                  `data-backdrop` = "false",
                div(class = "modal-dialog modal-sm", role = "document", style = "margin-top: 80px;",
                  div(class = "modal-content",
                    div(class = "modal-header", style = "padding: 10px 15px;",
                      tags$button(type = "button", class = "close", `data-dismiss` = "modal",
                                  tags$span(HTML("&times;"))),
                      tags$h4(class = "modal-title", style = "font-size: 15px;",
                              tags$i(class = "fa fa-columns", style = "margin-right: 6px;"),
                              "Select Lab Columns")
                    ),
                    div(class = "modal-body", style = "padding: 10px 15px; max-height: 400px; overflow-y: auto;",
                      tags$p(id = "lab_col_limit_msg",
                             style = "font-size: 11px; color: #888; margin-bottom: 8px;",
                             "Select up to 6 columns to display."),
                      # Generate checkbox groups from LAB_GROUPS
                      tagList(lapply(names(LAB_GROUPS), function(grp) {
                        grp_vars <- LAB_GROUPS[[grp]]
                        tags$div(style = "margin-bottom: 10px;",
                          tags$div(style = "font-weight: 600; font-size: 12px; color: #555; margin-bottom: 4px; border-bottom: 1px solid #eee; padding-bottom: 2px;", grp),
                          tagList(lapply(grp_vars, function(v) {
                            ref <- LAB_REFERENCE_RANGES[[v]]
                            lbl <- if (!is.null(ref)) paste0(ref$label, " (", ref$unit, ")") else v
                            is_default <- v %in% LAB_PRIMARY_VARS
                            tags$div(style = "margin-left: 4px; margin-bottom: 2px;",
                              tags$label(style = "font-weight: normal; font-size: 12px; cursor: pointer;",
                                tags$input(type = "checkbox", class = "lab-col-checkbox",
                                           value = v,
                                           checked = if (is_default) "checked" else NULL,
                                           style = "margin-right: 5px;"),
                                lbl
                              )
                            )
                          }))
                        )
                      }))
                    ),
                    div(class = "modal-footer", style = "padding: 8px 15px;",
                      tags$button(id = "lab_col_apply_btn", type = "button",
                                  class = "btn btn-sm btn-primary",
                                  "Apply")
                    )
                  )
                )
              )
            )
          )
        ),
        
        # ===== SECTION 5: BIOBANK SAMPLES =====
        div(id = "dash_biobank", class = "dashboard-section",
          tags$h4("Biobank Samples", style = "margin-top: 0;"),
          
          div(class = "row", style = "margin-bottom: 15px;",
            kpi_card("dash_total_samples",       "Total Samples",          "#e67e22", "fa-vial"),
            kpi_card("dash_unique_patients_bio",  "Patients with Samples", "#16a085", "fa-user"),
            kpi_card("dash_complete_samples",     "Complete",              "#8e44ad", "fa-check"),
            div(class = "col-sm-3")
          ),
          
          div(class = "row",
            div(class = "col-sm-6",
              tags$h5("Samples by Type"),
              plotly::plotlyOutput("dash_biobank_by_type", height = "240px", width = "100%")
            ),
            div(class = "col-sm-6",
              tags$h5("Samples by Status"),
              plotly::plotlyOutput("dash_biobank_by_status", height = "240px", width = "100%")
            )
          ),
          
          div(class = "row", style = "margin-top: 15px;",
            div(class = "col-sm-12",
              tags$h5("Samples per Patient"),
              DT::DTOutput("dash_biobank_per_patient", height = "250px")
            )
          )
        )
        
      ) # end dashboard-content
    ) # end outer container
  )
}

# ============================================================================
# SERVER LOGIC
# ============================================================================

#' Dashboard server module
#'
#' Registers all dashboard-related observers and renderers in the Shiny server.
#' Call this function from the main server() function in App.r.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
dashboard_server <- function(input, output, session) {
  
  # Helper: empty chart placeholder
  # Uses explicit type + empty data to avoid "No trace type" warnings at build time
  .empty_chart <- function(msg = "No data", font_size = 16) {
    plotly::plot_ly(type = "scatter", mode = "markers",
                    x = numeric(0), y = numeric(0)) %>%
      plotly::layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          text = msg, showarrow = FALSE,
          xref = "paper", yref = "paper", x = 0.5, y = 0.5,
          font = list(size = font_size, color = "gray")
        ))
      ) %>% plotly::config(displayModeBar = FALSE)
  }
  
  # ===== REACTIVE VALUES =====
  dashboard_data <- reactiveValues(
    records     = NULL,
    data_export = NULL,
    biobank     = NULL,
    lab_data    = NULL,
    inclusion_stats    = NULL,
    completeness_stats = NULL,
    biobank_stats      = NULL,
    lab_stats          = NULL,
    loading     = FALSE,
    error       = NULL
  )
  
  # Selected lab columns for patient table (default = primary vars)
  selected_lab_cols <- reactiveVal(LAB_PRIMARY_VARS)
  
  observeEvent(input$dash_lab_selected_cols, {
    selected_lab_cols(input$dash_lab_selected_cols)
  })
  
  # ===== OPEN DASHBOARD =====
  observeEvent(input$open_dashboard, {
    # 1. Show the modal first — let client create DOM + output bindings
    showModal(dashboard_modal_ui())
    
    # 2. After client has processed the modal and registered bindings,
    #    load data (or re-trigger if already cached). shinyjs::delay does
    #    a JS→server round-trip, guaranteeing client is ready.
    shinyjs::delay(500, {
      if (is.null(dashboard_data$records)) {
        cat("[Dashboard] Loading data (post-modal)...\n")
        dashboard_load_data(force_refresh = FALSE)
      } else {
        cat("[Dashboard] Data cached, re-triggering renders...\n")
        # Bump all reactive stats to force output re-evaluation
        dashboard_data$inclusion_stats    <- isolate(dashboard_data$inclusion_stats)
        dashboard_data$completeness_stats <- isolate(dashboard_data$completeness_stats)
        dashboard_data$biobank_stats      <- isolate(dashboard_data$biobank_stats)
        dashboard_data$lab_stats          <- isolate(dashboard_data$lab_stats)
      }
    })
  })
  
  # ===== REFRESH BUTTON =====
  observeEvent(input$dashboard_refresh, {
    dashboard_clear_cache()
    dashboard_load_data(force_refresh = TRUE)
  })
  
  # ===== DATA LOADING FUNCTION =====
  dashboard_load_data <- function(force_refresh = FALSE) {
    dashboard_data$loading <- TRUE
    dashboard_data$error <- NULL
    
    # Show loading indicator
    shinyjs::show("dashboard_loading")
    
    tryCatch({
      # Fetch data (uses cache if available)
      dashboard_data$records <- dashboard_fetch_records(force_refresh)
      dashboard_data$biobank <- dashboard_fetch_biobank(force_refresh)
      dashboard_data$lab_data <- dashboard_fetch_lab_data(force_refresh)
      
      # Data completeness: only fetch if on that tab or if forced
      dashboard_data$data_export <- dashboard_fetch_data_points(force_refresh)
      study_fields <- dashboard_get_study_fields()
      
      # Compute statistics
      dashboard_data$inclusion_stats <- dashboard_compute_inclusion_stats(dashboard_data$records)
      dashboard_data$completeness_stats <- dashboard_compute_completeness_stats(
        dashboard_data$data_export, study_fields
      )
      dashboard_data$biobank_stats <- dashboard_compute_biobank_stats(dashboard_data$biobank)
      dashboard_data$lab_stats <- dashboard_compute_lab_stats(dashboard_data$lab_data)
      
      cat("[Dashboard] All data loaded and statistics computed.\n")
    }, error = function(e) {
      dashboard_data$error <- e$message
      cat(sprintf("[Dashboard] Error loading data: %s\n", e$message))
      showNotification(
        paste("Dashboard error:", e$message),
        type = "error", duration = 10
      )
    })
    
    dashboard_data$loading <- FALSE
    shinyjs::hide("dashboard_loading")
  }
  
  # ============================================================================
  # RENDER: PATIENT INCLUSION
  # ============================================================================
  
  # KPI outputs — use observe + shinyjs::html for reliable updates inside modals
  observe({
    stats <- dashboard_data$inclusion_stats
    val <- if (is.null(stats)) "--" else as.character(stats$total + stats$total_archived)
    shinyjs::html("dash_total_patients", val)
  })
  
  observe({
    stats <- dashboard_data$inclusion_stats
    val <- if (is.null(stats)) "--" else as.character(stats$total)
    shinyjs::html("dash_total_active", val)
  })
  
  observe({
    stats <- dashboard_data$inclusion_stats
    val <- if (is.null(stats)) "--" else as.character(stats$total_archived)
    shinyjs::html("dash_total_archived", val)
  })
  
  # Inclusion curve (Active vs Archived cumulative over time)
  output$dash_inclusion_curve <- plotly::renderPlotly({
    stats <- dashboard_data$inclusion_stats
    if (is.null(stats) || (nrow(stats$by_date_active) == 0 && nrow(stats$by_date_archived) == 0)) {
      return(.empty_chart("No inclusion data available"))
    }
    
    # Combine active + archived into one data.table to avoid discrete/non-discrete conflict
    parts <- list()
    if (nrow(stats$by_date_active) > 0) {
      parts[["Active"]] <- stats$by_date_active[, .(date, cumulative, series = "Active")]
    }
    if (nrow(stats$by_date_archived) > 0) {
      parts[["Archived"]] <- stats$by_date_archived[, .(date, cumulative, series = "Archived")]
    }
    combined <- rbindlist(parts)
    
    line_styles <- list(
      Active   = list(color = "#2ecc71", width = 2),
      Archived = list(color = "#e74c3c", width = 2, dash = "dot")
    )
    marker_styles <- list(
      Active   = list(color = "#2ecc71", size = 4),
      Archived = list(color = "#e74c3c", size = 4)
    )
    
    p <- plotly::plot_ly(combined, x = ~date, y = ~cumulative,
                         type = "scatter", mode = "lines+markers",
                         color = ~series,
                         colors = c("Active" = "#2ecc71", "Archived" = "#e74c3c"))
    # Override line/marker styles per series
    for (s in unique(combined$series)) {
      p <- plotly::style(p, traces = which(unique(combined$series) == s),
                         line = line_styles[[s]], marker = marker_styles[[s]])
    }
    
    p <- p %>%
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Cumulative Patients"),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center"),
        margin = list(l = 50, r = 20, t = 10, b = 70)
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    p
  })
  
  # Monthly inclusion rate (grouped bar chart: active vs archived per month)
  output$dash_monthly_rate <- plotly::renderPlotly({
    stats <- dashboard_data$inclusion_stats
    if (is.null(stats) || nrow(stats$by_month) == 0) {
      return(.empty_chart("No monthly data"))
    }
    
    dt <- stats$by_month
    
    p <- plotly::plot_ly(dt) %>%
      plotly::add_trace(x = ~month, y = ~active, type = "bar", name = "Active",
                        marker = list(color = "#2ecc71"),
                        hovertemplate = "%{x}<br>Active: %{y}<extra></extra>") %>%
      plotly::add_trace(x = ~month, y = ~archived, type = "bar", name = "Archived",
                        marker = list(color = "#e74c3c"),
                        hovertemplate = "%{x}<br>Archived: %{y}<extra></extra>") %>%
      plotly::layout(
        barmode = "stack",
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "New Patients"),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.35, x = 0.5, xanchor = "center"),
        margin = list(l = 50, r = 20, t = 10, b = 90)
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    p
  })
  
  # Active vs Archived donut chart
  output$dash_active_vs_archived <- plotly::renderPlotly({
    stats <- dashboard_data$inclusion_stats
    if (is.null(stats) || (stats$total == 0 && stats$total_archived == 0)) {
      return(.empty_chart("No data"))
    }
    
    dt <- data.table(
      status = c("Active", "Archived"),
      count = c(stats$total, stats$total_archived)
    )
    
    p <- plotly::plot_ly(dt, labels = ~status, values = ~count, type = "pie",
                         marker = list(
                           colors = c("#2ecc71", "#e74c3c"),
                           line = list(color = "white", width = 2)
                         ),
                         textinfo = "label+value+percent",
                         textposition = "inside",
                         hovertemplate = "%{label}<br>Count: %{value}<br>%{percent}<extra></extra>",
                         hole = 0.4) %>%
      plotly::layout(
        margin = list(l = 20, r = 20, t = 10, b = 30),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.1, x = 0.5, xanchor = "center")
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    p
  })
  
  # Monthly table
  output$dash_monthly_table <- DT::renderDT({
    stats <- dashboard_data$inclusion_stats
    req(stats)     # block render until data is ready (avoids empty widget)
    req(nrow(stats$by_month) > 0)
    
    DT::datatable(
      stats$by_month,
      colnames = c("Month", "Active", "Archived", "Total"),
      options = list(
        pageLength = 6, dom = "tp", ordering = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE
    )
  }, server = FALSE)
  
  # ============================================================================
  # RENDER: DATA COMPLETENESS
  # ============================================================================
  
  observe({
    stats <- dashboard_data$completeness_stats
    val <- if (is.null(stats)) "--" else paste0(stats$overall_pct, "%")
    shinyjs::html("dash_overall_completeness", val)
  })
  
  observe({
    stats <- dashboard_data$completeness_stats
    val <- if (is.null(stats) || nrow(stats$by_form) == 0) "--" else as.character(nrow(stats$by_form))
    shinyjs::html("dash_total_forms", val)
  })
  
  observe({
    stats <- dashboard_data$completeness_stats
    val <- if (is.null(stats) || nrow(stats$by_field) == 0) "--" else as.character(nrow(stats$by_field))
    shinyjs::html("dash_total_fields", val)
  })
  
  # Completeness by form (horizontal bar chart)
  output$dash_completeness_by_form <- plotly::renderPlotly({
    stats <- dashboard_data$completeness_stats
    if (is.null(stats) || nrow(stats$by_form) == 0) {
      return(.empty_chart("No form data"))
    }
    
    dt <- stats$by_form
    colors <- ifelse(dt$avg_pct >= 80, "#2ecc71",
              ifelse(dt$avg_pct >= 50, "#f39c12", "#e74c3c"))
    
    # Order by percentage for better readability
    dt <- dt[order(avg_pct)]
    colors <- ifelse(dt$avg_pct >= 80, "#2ecc71",
              ifelse(dt$avg_pct >= 50, "#f39c12", "#e74c3c"))
    
    p <- plotly::plot_ly(dt, y = ~form_name, x = ~avg_pct, type = "bar",
                         orientation = "h",
                         marker = list(color = colors),
                         text = ~paste0(avg_pct, "%"),
                         textposition = "outside",
                         hovertemplate = "%{y}<br>Completeness: %{x}%<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = "Completeness (%)", range = c(0, 110)),
        yaxis = list(title = "", categoryorder = "array", categoryarray = dt$form_name,
                     ticksuffix = "  "),
        margin = list(l = 130, r = 30, t = 10, b = 50),
        shapes = list(
          list(type = "line", x0 = 50, x1 = 50, y0 = -0.5, y1 = nrow(dt) - 0.5,
               line = list(color = "#cccccc", dash = "dash", width = 1)),
          list(type = "line", x0 = 80, x1 = 80, y0 = -0.5, y1 = nrow(dt) - 0.5,
               line = list(color = "#cccccc", dash = "dash", width = 1))
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    p
  })
  
  # Completeness histogram (distribution across records)
  output$dash_completeness_histogram <- plotly::renderPlotly({
    stats <- dashboard_data$completeness_stats
    if (is.null(stats) || nrow(stats$by_record) == 0) {
      return(.empty_chart("No record data"))
    }
    
    mean_pct <- mean(stats$by_record$pct_complete, na.rm = TRUE)
    
    p <- plotly::plot_ly(x = ~stats$by_record$pct_complete, type = "histogram",
                         nbinsx = 20,
                         marker = list(color = "#3498db", line = list(color = "white", width = 1)),
                         hovertemplate = "Range: %{x}<br>Records: %{y}<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = "Completeness (%)", range = c(0, 100)),
        yaxis = list(title = "Number of Records", standoff = 15, ticksuffix = "  "),
        margin = list(l = 60, r = 20, t = 20, b = 50),
        shapes = list(list(
          type = "line", x0 = mean_pct, x1 = mean_pct, y0 = 0, y1 = 0.9,
          yref = "paper",
          line = list(color = "#e74c3c", width = 2, dash = "dash")
        )),
        annotations = list(list(
          x = mean_pct, y = 0.9, yref = "paper",
          text = paste0("Mean: ", round(mean_pct, 1), "%"),
          showarrow = FALSE, yanchor = "bottom",
          font = list(color = "#e74c3c", size = 11)
        ))
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    p
  })
  
  # Completeness by record creation date (scatter + trend)
  output$dash_completeness_over_time <- plotly::renderPlotly({
    stats <- dashboard_data$completeness_stats
    records <- dashboard_data$records
    if (is.null(stats) || nrow(stats$by_record) == 0 || is.null(records) || nrow(records) == 0) {
      return(.empty_chart("No data"))
    }
    
    # Join with record creation dates
    rec_dates <- records[!is.na(created_date), .(record_id = as.character(record_id), created_date)]
    dt <- merge(stats$by_record, rec_dates, by = "record_id", all.x = FALSE)
    
    if (nrow(dt) == 0) {
      return(.empty_chart("No date data available"))
    }
    
    setorder(dt, created_date)
    
    # Compute rolling average (window of 5 records or less)
    window <- min(5, nrow(dt))
    dt[, rolling_avg := frollmean(pct_complete, n = window, align = "center", na.rm = TRUE)]
    
    p <- plotly::plot_ly(dt) %>%
      plotly::add_trace(x = ~created_date, y = ~pct_complete, type = "scatter", mode = "markers",
                        marker = list(color = "#3498db", size = 5, opacity = 0.5),
                        name = "Record",
                        hovertemplate = "ID: %{text}<br>Date: %{x}<br>Complete: %{y}%<extra></extra>",
                        text = ~record_id) %>%
      plotly::add_trace(x = ~created_date, y = ~rolling_avg, type = "scatter", mode = "lines",
                        line = list(color = "#e74c3c", width = 2),
                        name = "Trend",
                        hovertemplate = "Date: %{x}<br>Avg: %{y:.1f}%<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Completeness (%)", range = c(0, 105), standoff = 15),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center"),
        margin = list(l = 60, r = 20, t = 10, b = 60)
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    p
  })
  
  # Missing fields per record (box plot)
  output$dash_missing_fields_box <- plotly::renderPlotly({
    stats <- dashboard_data$completeness_stats
    if (is.null(stats) || nrow(stats$by_record) == 0) {
      return(.empty_chart("No data"))
    }
    
    dt <- stats$by_record
    median_missing <- median(dt$n_missing, na.rm = TRUE)
    mean_missing <- round(mean(dt$n_missing, na.rm = TRUE), 1)
    
    p <- plotly::plot_ly(y = dt$n_missing, type = "box",
                        boxpoints = "outliers",
                        marker = list(color = "#3498db", size = 4),
                        line = list(color = "#2c3e50"),
                        fillcolor = "rgba(52, 152, 219, 0.3)",
                        name = "Missing Fields",
                        hovertemplate = "Missing: %{y}<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = "", showticklabels = FALSE),
        yaxis = list(title = "Missing Fields", standoff = 15),
        margin = list(l = 60, r = 20, t = 10, b = 30),
        showlegend = FALSE,
        annotations = list(list(
          x = 0.95, y = 0.95, xref = "paper", yref = "paper",
          text = paste0("Median: ", median_missing, "<br>Mean: ", mean_missing),
          showarrow = FALSE, xanchor = "right", yanchor = "top",
          font = list(size = 11, color = "#2c3e50"),
          bgcolor = "rgba(255,255,255,0.8)", bordercolor = "#ccc", borderwidth = 1
        ))
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    p
  })
  
  # Least complete fields table
  output$dash_incomplete_fields_table <- DT::renderDT({
    stats <- dashboard_data$completeness_stats
    req(stats)
    req(nrow(stats$by_field) > 0)
    
    # Bottom 20 by completion
    dt <- head(stats$by_field[order(pct_filled)], 20)
    
    DT::datatable(
      dt,
      colnames = c("Field", "Filled (%)"),
      options = list(
        pageLength = 10, dom = "tp", ordering = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 1))
      ),
      rownames = FALSE
    ) %>% DT::formatStyle(
      "pct_filled",
      backgroundColor = DT::styleInterval(c(30, 60), c("#f8d7da", "#fff3cd", "#d4edda"))
    )
  }, server = FALSE)
  
  # ============================================================================
  # RENDER: BIOBANK SAMPLES
  # ============================================================================
  
  observe({
    stats <- dashboard_data$biobank_stats
    val <- if (is.null(stats)) "--" else as.character(stats$total_samples)
    shinyjs::html("dash_total_samples", val)
  })
  
  observe({
    stats <- dashboard_data$biobank_stats
    val <- if (is.null(stats)) "--" else as.character(stats$unique_patients)
    shinyjs::html("dash_unique_patients_bio", val)
  })
  
  observe({
    stats <- dashboard_data$biobank_stats
    if (is.null(stats)) {
      shinyjs::html("dash_complete_samples", "--")
    } else {
      complete <- stats$by_status[tolower(status) == "complete", sum(count)]
      val <- as.character(if (length(complete) == 0) 0 else complete)
      shinyjs::html("dash_complete_samples", val)
    }
  })
  
  # Samples by type (bar chart)
  output$dash_biobank_by_type <- plotly::renderPlotly({
    stats <- dashboard_data$biobank_stats
    if (is.null(stats) || nrow(stats$by_type) == 0) {
      return(.empty_chart("No biobank data"))
    }
    
    dt <- stats$by_type
    colors <- substr(rainbow(nrow(dt), s = 0.7, v = 0.8), 1, 7)
    
    p <- plotly::plot_ly(dt, y = ~sample_type, x = ~count, type = "bar",
                         orientation = "h",
                         marker = list(color = colors),
                         text = ~count, textposition = "outside",
                         cliponaxis = FALSE,
                         hovertemplate = "%{y}<br>Count: %{x}<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = "Count", range = c(0, max(dt$count) * 1.15)),
        yaxis = list(title = "", categoryorder = "total ascending", ticksuffix = "  "),
        margin = list(l = 130, r = 50, t = 10, b = 50)
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    p
  })
  
  # Samples by status (pie/donut chart)
  output$dash_biobank_by_status <- plotly::renderPlotly({
    stats <- dashboard_data$biobank_stats
    if (is.null(stats) || nrow(stats$by_status) == 0) {
      return(.empty_chart("No status data"))
    }
    
    dt <- stats$by_status
    colors <- c("#2ecc71", "#f39c12", "#e74c3c", "#3498db", "#9b59b6")[seq_len(nrow(dt))]
    
    p <- plotly::plot_ly(dt, labels = ~status, values = ~count, type = "pie",
                         marker = list(colors = colors, line = list(color = "white", width = 2)),
                         textinfo = "label+percent",
                         textposition = "inside",
                         hovertemplate = "%{label}<br>Count: %{value}<br>%{percent}<extra></extra>",
                         hole = 0.3) %>%
      plotly::layout(
        margin = list(l = 20, r = 20, t = 10, b = 10),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.1, x = 0.5, xanchor = "center")
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    p
  })
  
  # Per-patient table
  output$dash_biobank_per_patient <- DT::renderDT({
    stats <- dashboard_data$biobank_stats
    req(stats)
    req(nrow(stats$by_patient) > 0)
    
    DT::datatable(
      stats$by_patient,
      colnames = c("Patient", "Samples"),
      options = list(
        pageLength = 10, dom = "tfp", ordering = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 1))
      ),
      rownames = FALSE
    )
  }, server = FALSE)
  
  # ============================================================================
  # RENDER: OVERVIEW
  # ============================================================================
  
  observe({
    stats <- dashboard_data$inclusion_stats
    val <- if (is.null(stats)) "--" else as.character(stats$total)
    shinyjs::html("dash_ov_patients", val)
  })
  
  observe({
    stats <- dashboard_data$completeness_stats
    val <- if (is.null(stats)) "--" else paste0(stats$overall_pct, "%")
    shinyjs::html("dash_ov_completeness", val)
  })
  
  observe({
    stats <- dashboard_data$lab_stats
    val <- if (is.null(stats)) "--" else as.character(stats$total_measurements)
    shinyjs::html("dash_ov_lab_measures", val)
  })
  
  observe({
    stats <- dashboard_data$biobank_stats
    val <- if (is.null(stats)) "--" else as.character(stats$total_samples)
    shinyjs::html("dash_ov_bio_samples", val)
  })
  
  # Mini inclusion trend
  output$dash_ov_inclusion_mini <- plotly::renderPlotly({
    stats <- dashboard_data$inclusion_stats
    if (is.null(stats) || nrow(stats$by_date_active) == 0) {
      return(.empty_chart("No data", 14))
    }
    plotly::plot_ly(stats$by_date_active, x = ~date, y = ~cumulative,
                    type = "scatter", mode = "lines",
                    line = list(color = "#2ecc71", width = 2),
                    hovertemplate = "%{x}: %{y}<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = "", showgrid = FALSE),
        yaxis = list(title = "", showgrid = TRUE),
        margin = list(l = 35, r = 10, t = 5, b = 30)
      ) %>% plotly::config(displayModeBar = FALSE)
  })
  
  # Mini completeness bar
  output$dash_ov_completeness_mini <- plotly::renderPlotly({
    stats <- dashboard_data$completeness_stats
    if (is.null(stats) || nrow(stats$by_form) == 0) {
      return(.empty_chart("No data", 14))
    }
    dt <- head(stats$by_form[order(-avg_pct)], 8)
    colors <- ifelse(dt$avg_pct >= 80, "#2ecc71", ifelse(dt$avg_pct >= 50, "#f39c12", "#e74c3c"))
    plotly::plot_ly(dt, y = ~form_name, x = ~avg_pct, type = "bar", orientation = "h",
                    marker = list(color = colors),
                    hovertemplate = "%{y}: %{x}%<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = "", range = c(0, 105), showgrid = FALSE),
        yaxis = list(title = "", categoryorder = "total ascending", ticksuffix = "  ",
                     tickfont = list(size = 10)),
        margin = list(l = 100, r = 10, t = 5, b = 30)
      ) %>% plotly::config(displayModeBar = FALSE)
  })
  
  # Mini lab timeline
  output$dash_ov_lab_mini <- plotly::renderPlotly({
    stats <- dashboard_data$lab_stats
    if (is.null(stats) || nrow(stats$by_month) == 0) {
      return(.empty_chart("No data", 14))
    }
    plotly::plot_ly(stats$by_month, x = ~month, y = ~count, type = "bar",
                    marker = list(color = "#1abc9c"),
                    hovertemplate = "%{x}: %{y}<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = "", tickangle = -45, tickfont = list(size = 9)),
        yaxis = list(title = "", showgrid = TRUE),
        margin = list(l = 35, r = 10, t = 5, b = 50)
      ) %>% plotly::config(displayModeBar = FALSE)
  })
  
  # ============================================================================
  # RENDER: LAB DATA
  # ============================================================================
  
  observe({
    stats <- dashboard_data$lab_stats
    val <- if (is.null(stats)) "--" else as.character(stats$total_measurements)
    shinyjs::html("dash_lab_total", val)
  })
  
  observe({
    stats <- dashboard_data$lab_stats
    val <- if (is.null(stats)) "--" else as.character(stats$unique_patients)
    shinyjs::html("dash_lab_patients", val)
  })
  
  observe({
    stats <- dashboard_data$lab_stats
    val <- if (is.null(stats)) "--" else as.character(stats$avg_per_patient)
    shinyjs::html("dash_lab_avg", val)
  })
  
  observe({
    stats <- dashboard_data$lab_stats
    val <- if (is.null(stats) || is.na(stats$last_date)) "--" else format(stats$last_date, "%Y-%m-%d")
    shinyjs::html("dash_lab_last_date", val)
  })
  
  # Lab completeness by variable (horizontal bar chart grouped by category)
  output$dash_lab_completeness <- plotly::renderPlotly({
    stats <- dashboard_data$lab_stats
    if (is.null(stats) || nrow(stats$completeness) == 0) {
      return(.empty_chart("No lab data available", 14))
    }
    
    dt <- stats$completeness[order(group, -pct)]
    
    # Color per group
    group_palette <- c(
      "Hematology" = "#e74c3c", "Renal Function" = "#3498db",
      "Liver Function" = "#f39c12", "Infection" = "#2ecc71",
      "Electrolytes" = "#9b59b6", "Metabolism" = "#1abc9c",
      "Other" = "#95a5a6"
    )
    dt[, bar_color := ifelse(group %in% names(group_palette),
                             group_palette[group], "#95a5a6")]
    
    # Single plot_ly call with all data to avoid discrete/non-discrete axis conflict
    plotly::plot_ly(dt, y = ~label, x = ~pct, type = "bar", orientation = "h",
                    color = ~group,
                    colors = group_palette,
                    text = ~paste0(pct, "%"), textposition = "outside",
                    hovertemplate = "%{y}<br>%{x}% filled<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = "Completeness (%)", range = c(0, 110)),
        yaxis = list(title = "", categoryorder = "array", categoryarray = rev(dt$label),
                     ticksuffix = "  ", tickfont = list(size = 10)),
        margin = list(l = 110, r = 40, t = 10, b = 70),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.3, x = 0.5, xanchor = "center",
                      font = list(size = 10)),
        barmode = "stack"
      ) %>% plotly::config(displayModeBar = FALSE)
  })
  
  # Lab measurements timeline
  output$dash_lab_timeline <- plotly::renderPlotly({
    stats <- dashboard_data$lab_stats
    if (is.null(stats) || nrow(stats$by_month) == 0) {
      return(.empty_chart("No timeline data", 14))
    }
    
    plotly::plot_ly(stats$by_month, x = ~month, y = ~count, type = "bar",
                    marker = list(color = "#1abc9c", line = list(color = "white", width = 1)),
                    hovertemplate = "%{x}<br>Measurements: %{y}<extra></extra>") %>%
      plotly::layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Measurements"),
        margin = list(l = 50, r = 20, t = 10, b = 70)
      ) %>% plotly::config(displayModeBar = FALSE)
  })
  
  # Latest lab values per patient table with color coding and sparklines
  output$dash_lab_patient_table <- DT::renderDT({
    stats <- dashboard_data$lab_stats
    req(stats)
    req(nrow(stats$patient_latest) > 0)
    
    pl <- copy(stats$patient_latest)
    sparklines <- stats$patient_sparklines
    
    # Build display table
    display_cols <- c("record_id")
    col_labels <- c("Patient")
    
    primary_vars <- intersect(selected_lab_cols(), names(pl))
    
    for (pc in primary_vars) {
      ref <- LAB_REFERENCE_RANGES[[pc]]
      lab_label <- if (!is.null(ref)) ref$label else pc
      
      # Format value with unit
      pl[, (paste0(pc, "_display")) := {
        vals <- suppressWarnings(as.numeric(get(pc)))
        ifelse(is.na(vals), "--", sprintf("%.1f", vals))
      }]
      display_cols <- c(display_cols, paste0(pc, "_display"))
      col_labels <- c(col_labels, lab_label)
      
      # Add sparkline column only if there is actual sparkline data for this variable
      has_spark_data <- any(sapply(sparklines, function(s) {
        sp <- s[[pc]]
        !is.null(sp) && length(sp) >= 2
      }))
      
      if (has_spark_data) {
        spark_col <- paste0(pc, "_spark")
        pl[, (spark_col) := {
          sapply(record_id, function(pid) {
            sp <- sparklines[[pid]][[pc]]
            if (is.null(sp) || length(sp) < 2) return("")
            paste0('<span class="spark-cell" values="', paste(sp, collapse = ","), '"></span>')
          })
        }]
        display_cols <- c(display_cols, spark_col)
        col_labels <- c(col_labels, paste(lab_label, "Trend"))
      }
    }
    
    # Add collection date
    if ("collection_date" %in% names(pl)) {
      pl[, date_display := ifelse(is.na(collection_date), "--", format(collection_date, "%Y-%m-%d"))]
      display_cols <- c(display_cols, "date_display")
      col_labels <- c(col_labels, "Last Date")
    }
    
    dt_display <- pl[, ..display_cols]
    
    # Create DT with conditional formatting
    tbl <- DT::datatable(
      dt_display,
      colnames = col_labels,
      escape = FALSE,
      options = list(
        pageLength = 15,
        dom = "tfp",
        ordering = TRUE,
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all"),
          list(width = "120px", targets = "_all")
        ),
        drawCallback = DT::JS("function(settings) {
          var api = this.api();
          var body = $(api.table().body());
          var sparks = body.find('span.spark-cell[values]');
          sparks.each(function() {
            var raw = $(this).attr('values');
            if (raw && raw.length > 0) {
              var vals = raw.split(',').map(Number);
              if (vals.length >= 2 && typeof $.fn.sparkline === 'function') {
                $(this).sparkline(vals, {
                  type: 'line', width: '80px', height: '20px',
                  lineColor: '#3498db', fillColor: 'rgba(52,152,219,0.1)',
                  spotColor: '#e74c3c', minSpotColor: '#e74c3c', maxSpotColor: '#2ecc71',
                  spotRadius: 2
                });
              }
            }
          });
        }")
      ),
      rownames = FALSE
    )
    
    # Apply three-level color coding per primary variable column
    for (i in seq_along(primary_vars)) {
      pc <- primary_vars[i]
      status_col <- paste0(pc, "_status")
      if (status_col %in% names(pl)) {
        value_col_name <- col_labels[which(display_cols == paste0(pc, "_display"))]
        # Map status to colors
        color_map <- pl[[paste0(pc, "_color")]]
        tbl <- tbl %>% DT::formatStyle(
          paste0(pc, "_display"),
          backgroundColor = DT::styleEqual(
            unique(dt_display[[paste0(pc, "_display")]]),
            sapply(unique(dt_display[[paste0(pc, "_display")]]), function(v) {
              idx <- which(dt_display[[paste0(pc, "_display")]] == v)[1]
              if (is.na(idx)) return("transparent")
              status <- pl[[status_col]][idx]
              if (status == "normal") return("rgba(46, 204, 113, 0.15)")
              if (status == "possibly_abnormal") return("rgba(243, 156, 18, 0.2)")
              if (status == "abnormal") return("rgba(231, 76, 60, 0.2)")
              return("transparent")
            })
          )
        )
      }
    }
    
    tbl
  }, server = FALSE)

  # ===== Ensure all outputs render even when their tab is hidden =====
  # Shiny suspends outputs in display:none containers by default.
  # We need them to render eagerly so data shows when tabs are switched.
  hidden_outputs <- c(
    # Overview tab
    "dash_ov_inclusion_mini", "dash_ov_completeness_mini", "dash_ov_lab_mini",
    # Inclusion tab
    "dash_inclusion_curve", "dash_monthly_rate", "dash_active_vs_archived",
    "dash_monthly_table",
    # Completeness tab
    "dash_completeness_by_form", "dash_completeness_histogram",
    "dash_completeness_over_time", "dash_missing_fields_box",
    "dash_incomplete_fields_table",
    # Lab Data tab
    "dash_lab_completeness", "dash_lab_timeline", "dash_lab_patient_table",
    # Biobank tab
    "dash_biobank_by_type", "dash_biobank_by_status", "dash_biobank_per_patient"
  )
  for (oid in hidden_outputs) {
    outputOptions(output, oid, suspendWhenHidden = FALSE)
  }
}

cat("[Startup] Dashboard module loaded.\n")

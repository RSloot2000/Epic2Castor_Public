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
  if (is.null(records) || nrow(records) == 0) {
    return(list(
      total = 0, total_archived = 0,
      by_date = data.table(date = as.Date(character(0)), count = integer(0), cumulative = integer(0)),
      by_site = data.table(site_name = character(0), count = integer(0)),
      by_month = data.table(month = character(0), count = integer(0))
    ))
  }
  
  active <- records[archived == FALSE]
  
  # Total counts
  total_active <- nrow(active)
  total_archived <- sum(records$archived, na.rm = TRUE)
  
  # By date (cumulative inclusion curve)
  by_date <- active[!is.na(created_date), .(count = .N), by = .(date = created_date)]
  setorder(by_date, date)
  by_date[, cumulative := cumsum(count)]
  
  # By site
  by_site <- active[, .(count = .N), by = .(site_name)]
  by_site[is.na(site_name), site_name := "Unknown"]
  setorder(by_site, -count)
  
  # By month
  by_month <- active[!is.na(created_date), .(
    count = .N
  ), by = .(month = format(created_date, "%Y-%m"))]
  setorder(by_month, month)
  
  list(
    total = total_active,
    total_archived = total_archived,
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
    .(pct_complete = if (total > 0) round(100 * filled / total, 1) else 0)
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

# ============================================================================
# UI GENERATION
# ============================================================================

#' Generate the dashboard modal UI
#'
#' Creates a full-screen modal with 3 tabs: Inclusion, Completeness, Biobank
#' Called when user clicks the Dashboard button in the menu bar
#'
#' @return Shiny modal dialog
dashboard_modal_ui <- function() {
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
    
    # Loading indicator
    div(id = "dashboard_loading",
        style = "text-align: center; padding: 20px; display: none;",
        tags$i(class = "fa fa-spinner fa-spin fa-2x"),
        tags$p("Loading dashboard data...")
    ),
    
    # Tab panel with 3 dashboard views
    tabsetPanel(
      id = "dashboard_tabs",
      type = "pills",
      
      # ===== TAB 1: Patient Inclusion =====
      tabPanel(
        title = tags$span(icon("user-plus"), "Patient Inclusion"),
        value = "inclusion",
        div(style = "padding: 15px;",
            
            # KPI boxes row
            div(class = "row", style = "margin-bottom: 20px;",
                div(class = "col-sm-4",
                    div(class = "well text-center",
                        style = "background: #3498db; color: white; padding: 15px; border-radius: 8px;",
                        tags$h2(textOutput("dash_total_patients", inline = TRUE)),
                        tags$p("Total Patients")
                    )
                ),
                div(class = "col-sm-4",
                    div(class = "well text-center",
                        style = "background: #2ecc71; color: white; padding: 15px; border-radius: 8px;",
                        tags$h2(textOutput("dash_total_active", inline = TRUE)),
                        tags$p("Active")
                    )
                ),
                div(class = "col-sm-4",
                    div(class = "well text-center",
                        style = "background: #e74c3c; color: white; padding: 15px; border-radius: 8px;",
                        tags$h2(textOutput("dash_total_archived", inline = TRUE)),
                        tags$p("Archived")
                    )
                )
            ),
            
            # Inclusion curve plot
            div(class = "row",
                div(class = "col-sm-12",
                    tags$h4("Inclusion Over Time"),
                    plotOutput("dash_inclusion_curve", height = "300px")
                )
            ),
            
            # Monthly breakdown table
            div(class = "row", style = "margin-top: 20px;",
                div(class = "col-sm-12",
                    tags$h4("Monthly Inclusion"),
                    DT::DTOutput("dash_monthly_table", height = "200px")
                )
            )
        )
      ),
      
      # ===== TAB 2: Data Completeness =====
      tabPanel(
        title = tags$span(icon("tasks"), "Data Completeness"),
        value = "completeness",
        div(style = "padding: 15px;",
            
            # Overall completeness KPI
            div(class = "row", style = "margin-bottom: 20px;",
                div(class = "col-sm-4",
                    div(class = "well text-center",
                        style = "background: #f39c12; color: white; padding: 15px; border-radius: 8px;",
                        tags$h2(textOutput("dash_overall_completeness", inline = TRUE)),
                        tags$p("Overall Completeness")
                    )
                ),
                div(class = "col-sm-4",
                    div(class = "well text-center",
                        style = "background: #9b59b6; color: white; padding: 15px; border-radius: 8px;",
                        tags$h2(textOutput("dash_total_forms", inline = TRUE)),
                        tags$p("Forms")
                    )
                ),
                div(class = "col-sm-4",
                    div(class = "well text-center",
                        style = "background: #1abc9c; color: white; padding: 15px; border-radius: 8px;",
                        tags$h2(textOutput("dash_total_fields", inline = TRUE)),
                        tags$p("Fields Tracked")
                    )
                )
            ),
            
            # Completeness by form
            div(class = "row",
                div(class = "col-sm-6",
                    tags$h4("Completeness by Form"),
                    plotOutput("dash_completeness_by_form", height = "300px")
                ),
                div(class = "col-sm-6",
                    tags$h4("Completeness per Record"),
                    plotOutput("dash_completeness_histogram", height = "300px")
                )
            ),
            
            # Fields with lowest completion
            div(class = "row", style = "margin-top: 20px;",
                div(class = "col-sm-12",
                    tags$h4("Least Complete Fields (Bottom 20)"),
                    DT::DTOutput("dash_incomplete_fields_table", height = "250px")
                )
            )
        )
      ),
      
      # ===== TAB 3: Biobank Samples =====
      tabPanel(
        title = tags$span(icon("flask"), "Biobank Samples"),
        value = "biobank",
        div(style = "padding: 15px;",
            
            # KPI boxes
            div(class = "row", style = "margin-bottom: 20px;",
                div(class = "col-sm-4",
                    div(class = "well text-center",
                        style = "background: #e67e22; color: white; padding: 15px; border-radius: 8px;",
                        tags$h2(textOutput("dash_total_samples", inline = TRUE)),
                        tags$p("Total Samples")
                    )
                ),
                div(class = "col-sm-4",
                    div(class = "well text-center",
                        style = "background: #16a085; color: white; padding: 15px; border-radius: 8px;",
                        tags$h2(textOutput("dash_unique_patients_bio", inline = TRUE)),
                        tags$p("Patients with Samples")
                    )
                ),
                div(class = "col-sm-4",
                    div(class = "well text-center",
                        style = "background: #8e44ad; color: white; padding: 15px; border-radius: 8px;",
                        tags$h2(textOutput("dash_complete_samples", inline = TRUE)),
                        tags$p("Complete")
                    )
                )
            ),
            
            # Charts
            div(class = "row",
                div(class = "col-sm-6",
                    tags$h4("Samples by Type"),
                    plotOutput("dash_biobank_by_type", height = "300px")
                ),
                div(class = "col-sm-6",
                    tags$h4("Samples by Status"),
                    plotOutput("dash_biobank_by_status", height = "300px")
                )
            ),
            
            # Per patient table
            div(class = "row", style = "margin-top: 20px;",
                div(class = "col-sm-12",
                    tags$h4("Samples per Patient"),
                    DT::DTOutput("dash_biobank_per_patient", height = "250px")
                )
            )
        )
      )
    )
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
  
  # ===== REACTIVE VALUES =====
  dashboard_data <- reactiveValues(
    records     = NULL,
    data_export = NULL,
    biobank     = NULL,
    inclusion_stats    = NULL,
    completeness_stats = NULL,
    biobank_stats      = NULL,
    loading     = FALSE,
    error       = NULL
  )
  
  # ===== OPEN DASHBOARD =====
  observeEvent(input$open_dashboard, {
    showModal(dashboard_modal_ui())
    
    # Trigger data load if not already loaded
    if (is.null(dashboard_data$records)) {
      dashboard_load_data(force_refresh = FALSE)
    }
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
      
      # Data completeness: only fetch if on that tab or if forced
      dashboard_data$data_export <- dashboard_fetch_data_points(force_refresh)
      study_fields <- dashboard_get_study_fields()
      
      # Compute statistics
      dashboard_data$inclusion_stats <- dashboard_compute_inclusion_stats(dashboard_data$records)
      dashboard_data$completeness_stats <- dashboard_compute_completeness_stats(
        dashboard_data$data_export, study_fields
      )
      dashboard_data$biobank_stats <- dashboard_compute_biobank_stats(dashboard_data$biobank)
      
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
  
  # KPI outputs
  output$dash_total_patients <- renderText({
    stats <- dashboard_data$inclusion_stats
    if (is.null(stats)) return("--")
    stats$total + stats$total_archived
  })
  
  output$dash_total_active <- renderText({
    stats <- dashboard_data$inclusion_stats
    if (is.null(stats)) return("--")
    stats$total
  })
  
  output$dash_total_archived <- renderText({
    stats <- dashboard_data$inclusion_stats
    if (is.null(stats)) return("--")
    stats$total_archived
  })
  
  # Inclusion curve (cumulative over time)
  output$dash_inclusion_curve <- renderPlot({
    stats <- dashboard_data$inclusion_stats
    if (is.null(stats) || nrow(stats$by_date) == 0) {
      plot.new()
      text(0.5, 0.5, "No inclusion data available", cex = 1.2, col = "gray")
      return()
    }
    
    dt <- stats$by_date
    par(mar = c(4, 4, 2, 1))
    plot(dt$date, dt$cumulative, type = "l", lwd = 2, col = "#3498db",
         xlab = "Date", ylab = "Cumulative Patients",
         main = "", las = 1, bty = "l")
    points(dt$date, dt$cumulative, pch = 19, cex = 0.5, col = "#3498db")
    grid(col = "#eeeeee", lty = 1)
    
    # Add target line if configured (optional)
    abline(h = nrow(dashboard_data$records), lty = 2, col = "#2ecc71")
  })
  
  # Monthly table
  output$dash_monthly_table <- DT::renderDT({
    stats <- dashboard_data$inclusion_stats
    if (is.null(stats) || nrow(stats$by_month) == 0) return(data.table())
    
    DT::datatable(
      stats$by_month,
      colnames = c("Month", "Count"),
      options = list(
        pageLength = 6, dom = "tp", ordering = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE
    )
  })
  
  # ============================================================================
  # RENDER: DATA COMPLETENESS
  # ============================================================================
  
  output$dash_overall_completeness <- renderText({
    stats <- dashboard_data$completeness_stats
    if (is.null(stats)) return("--")
    paste0(stats$overall_pct, "%")
  })
  
  output$dash_total_forms <- renderText({
    stats <- dashboard_data$completeness_stats
    if (is.null(stats) || nrow(stats$by_form) == 0) return("--")
    nrow(stats$by_form)
  })
  
  output$dash_total_fields <- renderText({
    stats <- dashboard_data$completeness_stats
    if (is.null(stats) || nrow(stats$by_field) == 0) return("--")
    nrow(stats$by_field)
  })
  
  # Completeness by form (horizontal bar chart)
  output$dash_completeness_by_form <- renderPlot({
    stats <- dashboard_data$completeness_stats
    if (is.null(stats) || nrow(stats$by_form) == 0) {
      plot.new()
      text(0.5, 0.5, "No form data", cex = 1.2, col = "gray")
      return()
    }
    
    dt <- stats$by_form
    colors <- ifelse(dt$avg_pct >= 80, "#2ecc71",
              ifelse(dt$avg_pct >= 50, "#f39c12", "#e74c3c"))
    
    par(mar = c(4, 12, 2, 2))
    bp <- barplot(dt$avg_pct, names.arg = dt$form_name, horiz = TRUE,
                  col = colors, border = NA, las = 1,
                  xlab = "Completeness (%)", xlim = c(0, 100), main = "")
    text(dt$avg_pct + 2, bp, paste0(dt$avg_pct, "%"), adj = 0, cex = 0.8)
    abline(v = c(50, 80), lty = 2, col = "#cccccc")
  })
  
  # Completeness histogram (distribution across records)
  output$dash_completeness_histogram <- renderPlot({
    stats <- dashboard_data$completeness_stats
    if (is.null(stats) || nrow(stats$by_record) == 0) {
      plot.new()
      text(0.5, 0.5, "No record data", cex = 1.2, col = "gray")
      return()
    }
    
    par(mar = c(4, 4, 2, 1))
    hist(stats$by_record$pct_complete, breaks = 20,
         col = "#3498db", border = "white",
         xlab = "Completeness (%)", ylab = "Number of Records",
         main = "", las = 1, xlim = c(0, 100))
    abline(v = mean(stats$by_record$pct_complete, na.rm = TRUE),
           lty = 2, col = "#e74c3c", lwd = 2)
  })
  
  # Least complete fields table
  output$dash_incomplete_fields_table <- DT::renderDT({
    stats <- dashboard_data$completeness_stats
    if (is.null(stats) || nrow(stats$by_field) == 0) return(data.table())
    
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
  })
  
  # ============================================================================
  # RENDER: BIOBANK SAMPLES
  # ============================================================================
  
  output$dash_total_samples <- renderText({
    stats <- dashboard_data$biobank_stats
    if (is.null(stats)) return("--")
    stats$total_samples
  })
  
  output$dash_unique_patients_bio <- renderText({
    stats <- dashboard_data$biobank_stats
    if (is.null(stats)) return("--")
    stats$unique_patients
  })
  
  output$dash_complete_samples <- renderText({
    stats <- dashboard_data$biobank_stats
    if (is.null(stats)) return("--")
    complete <- stats$by_status[tolower(status) == "complete", sum(count)]
    if (length(complete) == 0) 0 else complete
  })
  
  # Samples by type (bar chart)
  output$dash_biobank_by_type <- renderPlot({
    stats <- dashboard_data$biobank_stats
    if (is.null(stats) || nrow(stats$by_type) == 0) {
      plot.new()
      text(0.5, 0.5, "No biobank data", cex = 1.2, col = "gray")
      return()
    }
    
    dt <- stats$by_type
    colors <- rainbow(nrow(dt), s = 0.7, v = 0.8)
    par(mar = c(4, 8, 2, 1))
    barplot(dt$count, names.arg = dt$sample_type, horiz = TRUE,
            col = colors, border = NA, las = 1,
            xlab = "Count", main = "")
  })
  
  # Samples by status (pie chart)
  output$dash_biobank_by_status <- renderPlot({
    stats <- dashboard_data$biobank_stats
    if (is.null(stats) || nrow(stats$by_status) == 0) {
      plot.new()
      text(0.5, 0.5, "No status data", cex = 1.2, col = "gray")
      return()
    }
    
    dt <- stats$by_status
    colors <- c("#2ecc71", "#f39c12", "#e74c3c", "#3498db", "#9b59b6")[seq_len(nrow(dt))]
    labels <- paste0(dt$status, "\n(", dt$count, ")")
    
    par(mar = c(1, 1, 2, 1))
    pie(dt$count, labels = labels, col = colors, border = "white", main = "")
  })
  
  # Per-patient table
  output$dash_biobank_per_patient <- DT::renderDT({
    stats <- dashboard_data$biobank_stats
    if (is.null(stats) || nrow(stats$by_patient) == 0) return(data.table())
    
    DT::datatable(
      stats$by_patient,
      colnames = c("Patient", "Samples"),
      options = list(
        pageLength = 10, dom = "tfp", ordering = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 1))
      ),
      rownames = FALSE
    )
  })
}

cat("[Startup] Dashboard module loaded.\n")

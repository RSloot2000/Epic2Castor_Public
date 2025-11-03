## Ensure CRAN mirror and required packages in non-interactive runs
if (is.null(getOption("repos")) || isTRUE(getOption("repos")["CRAN"] %in% c("", "@CRAN@"))) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

invisible(lapply(c("httr", "jsonlite", "uuid", "RSQLite", "DBI"), ensure_pkg))

# Paden centraliseren via scripts/paths.json
paths <- jsonlite::fromJSON(file.path("config", "paths.json"))

# Auto logger en config (creates logs/<date_time> and <CastorAPIExport>_log.txt)
logger_path <- if (!is.null(paths$logger_script)) paths$logger_script else file.path(paths$scripts_dir, "Logger.r")
config_script_path <- if (!is.null(paths$config_script)) paths$config_script else file.path(paths$scripts_dir, "config.R")
batch_helper_path <- if (!is.null(paths$batch_upload_helper_script)) paths$batch_upload_helper_script else file.path(paths$scripts_dir, "batch_upload_helper.r")
try(source(logger_path), silent = TRUE)
try(source(config_script_path), silent = TRUE)
try(source(batch_helper_path), silent = TRUE)

# Initialize structured status (progress bar in Shiny)
if (exists("epic2castor_status_init")) {
  try(epic2castor_status_init(total = NA_integer_, title = "Follow-up Upload", step = "init", detail = "Starting"), silent = TRUE)
}

# Helper for getting error messages
getErrorMessage <- function(token_content) {
  error_info <- tryCatch(fromJSON(token_content, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(error_info) && is.list(error_info)) {
    if ("error" %in% names(error_info)) {
      return(error_info$error)
    }
    if ("message" %in% names(error_info)) {
      return(error_info$message)
    }
    if ("error_description" %in% names(error_info)) {
      return(error_info$error_description)
    }
  }
  return(NULL)
}

# ======== CONFIGURATION ===========
config_path <- epc_path("config_api")
config <- fromJSON(config_path)

base_url <- "https://data.castoredc.com"  # Base URL for the Castor API
api_base_url <- "https://data.castoredc.com/api"  # Base URL for the Castor API
client_id <- config$client_id             # Retrieve from APIConfig.json
client_secret <- config$client_secret     # Retrieve from APIConfig.json
study_id <- config$study_id               # Retrieve from APIConfig.json

# Load field mapping from castor_meta.db (same as baseline/biobank)
# Field Variable Name = Castor field name without prefix (e.g., fup_visit_location)
# Field ID = Castor Field ID (PBAIG#xxx)
db_path <- epc_path("castor_meta_db")
con <- dbConnect(SQLite(), dbname = db_path)
query <- "SELECT [Field Variable Name] as varname, [Field ID] as field_id FROM study_variablelist"
mapping_df <- dbGetQuery(con, query)
mapping_list <- setNames(mapping_df$field_id, mapping_df$varname)
cat("Loaded", length(mapping_list), "field mappings from castor_meta.db\n")
dbDisconnect(con)

# ======== STEP 1: Obtain an access token ===========
token_url <- paste0(base_url, "/oauth/token")
token_response <- POST(
  url = token_url,
  body = list(
    client_id = client_id,
    client_secret = client_secret,
    grant_type = "client_credentials"
  ),
  encode = "form"
)

if (status_code(token_response) != 200) {
  token_content <- content(token_response, as = "text", encoding = "UTF-8")
  error_message <- getErrorMessage(token_content)
  
  if (!is.null(error_message)) {
    stop("Failed to obtain access token: ", error_message)
  } else {
    stop("Failed to obtain access token. Status code: ", status_code(token_response))
  }
}

token_data <- content(token_response, as = "parsed", encoding = "UTF-8")
access_token <- token_data$access_token

cat("Access token successfully obtained.\n\n")

# Status: auth ok
if (exists("epic2castor_status_update")) {
  try(epic2castor_status_update(step = "auth", detail = "Access token obtained", force = TRUE), silent = TRUE)
}

headers <- add_headers(Authorization = paste("Bearer", access_token))

# ======== STEP 2: Read CSV and build JSON payload ===========
csv_file <- file.path(epc_path("follow_up_output_data_dir"), "follow_up.csv")
if (!file.exists(csv_file)) {
  stop("File not found:", csv_file)
}
data <- read.csv2(csv_file, stringsAsFactors = FALSE)

# Estimate total datapoints (all non-meta fields across all records)
unwanted_fields <- c("Participant.Id", "Participant.Status", "Repeating.data.Name.Custom",
                     "Site.Abbreviation", "Repeating.Data.Creation.Date")
num_fields_per_record <- length(setdiff(names(data), unwanted_fields))
dp_total_estimate <- as.numeric(nrow(data)) * as.numeric(num_fields_per_record)
dp_done <- 0
if (exists("epic2castor_status_update")) {
  try(epic2castor_status_update(step = "prepare", total = dp_total_estimate,
                                 detail = sprintf("Preparing %d record(s)", nrow(data)),
                                 force = TRUE), silent = TRUE)
}

records <- lapply(seq_len(nrow(data)), function(i) {
  list(
    participant_id = data$`Participant.Id`[i],
    fields = as.list(data[i, ])
  )
})
json_payload_object <- list(records = records)
json_file <- file.path(epc_path("castor_export_dir"), "follow_up.json")
write(toJSON(json_payload_object, pretty = TRUE, auto_unbox = TRUE), file = json_file)
cat("JSON file has been saved as", json_file, "\n")

if (exists("epic2castor_status_update")) {
  try(epic2castor_status_update(step = "prepare", detail = paste("JSON saved:", basename(json_file))), silent = TRUE)
}

# ======== STEP 3: Retrieve all sites and let the user (or a default) select a site ===========
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  site_parts <- strsplit(args[1], " - ")[[1]]
  selected_site <- site_parts[1]
  selected_site_name <- if(length(site_parts) >= 2) site_parts[2] else ""
  cat("Selected site:", selected_site, "-", selected_site_name, "\n\n")
} else {
  get_sites <- function(access_token, api_base_url, study_id) {
    sites_url <- paste0(api_base_url, "/study/", study_id, "/site")
    response <- GET(sites_url, add_headers(Authorization = paste("Bearer", access_token)))
    if (status_code(response) != 200) {
      stop("Error retrieving sites: ", content(response, as = "text", encoding = "UTF-8"))
    }
    return(content(response, as = "parsed", type = "application/json", encoding = "UTF-8"))
  }
  sites_raw <- get_sites(access_token, api_base_url, study_id)
  if ("_embedded" %in% names(sites_raw)) {
    sites_data <- sites_raw[["_embedded"]][["sites"]]
  } else {
    sites_data <- sites_raw
  }
  choices <- sapply(sites_data, function(x) { paste(x$site_id, x$name, sep = " - ") })
  if (exists("epic2castor_status_update")) {
    try(epic2castor_status_update(step = "site_select", detail = "Waiting for site selection"), silent = TRUE)
  }
  
  if (interactive() && requireNamespace("tcltk", quietly = TRUE)) {
    selected_choice <- tcltk::tk_select.list(choices, title = "Select a site")
  } else if (interactive()) {
    cat("\nAvailable sites:\n")
    for (i in seq_along(choices)) {
      cat(sprintf("%d: %s\n", i, choices[i]))
    }
    cat("Enter the number of your choice: ")
    index <- as.integer(readline())
    if (is.na(index) || index < 1 || index > length(choices)) {
      stop("Invalid choice.")
    }
    selected_choice <- choices[index]
  } else {
    selected_choice <- choices[1]
    cat("Non-interactive mode: selecting first site:", selected_choice, "\n")
  }

  if (selected_choice == "") { stop("No site selected.") }
  selected_site <- strsplit(selected_choice, " - ")[[1]][1]
  cat("Selected site:", selected_site, "\n\n")
}

# ======== STEP 4: Create participants ===========
check_or_create_participant <- function(access_token, api_base_url, study_id, participant_id, site_id) {
  participant_url <- paste0(api_base_url, "/study/", study_id, "/participant/", participant_id)
  check_response <- GET(participant_url, add_headers(Authorization = paste("Bearer", access_token)))
  cat("Check participant HTTP status code:", status_code(check_response), "\n")
  
  if (status_code(check_response) == 404) {
    cat("Participant not found. Creating new participant:", participant_id, "\n")
    create_participant_url <- paste0(api_base_url, "/study/", study_id, "/participant")
    create_response <- POST(
      url = create_participant_url,
      add_headers(Authorization = paste("Bearer", access_token)),
      body = toJSON(list(
        participant_id = participant_id,
        site_id = site_id
      ), auto_unbox = TRUE),
      content_type("application/json")
    )
    if (status_code(create_response) %in% c(200, 201)) {
      cat("Participant created successfully:", participant_id, "\n")
    } else {
      cat("Error creating participant:", content(create_response, as = "text", encoding = "UTF-8"), "\n")
    }
  } else {
    cat("Participant already exists:", participant_id, "\n")
  }
}

unique_participants <- unique(data$`Participant.Id`)
for (participant_id in unique_participants) {
  if (exists("epic2castor_status_update")) {
    try(epic2castor_status_update(step = "participants", detail = paste("Checking participant:", participant_id)), silent = TRUE)
  }
  check_or_create_participant(access_token, api_base_url, study_id, participant_id, selected_site)
}

# ======== STEP 5: Retrieve existing repeating data ===========
cat("\n=== Retrieving repeating data structures ===\n")
if (exists("epic2castor_status_update")) {
  try(epic2castor_status_update(step = "repeating_data", detail = "Fetching repeating data structures"), silent = TRUE)
}

repeating_instance_url <- paste0(api_base_url, "/study/", study_id, "/repeating-data")
cat("URL:", repeating_instance_url, "\n")

repeating_instance_response <- tryCatch({
  GET(repeating_instance_url, headers)
}, error = function(e) {
  cat("ERROR in GET request:", conditionMessage(e), "\n")
  stop("Failed to retrieve repeating data: ", conditionMessage(e))
})

cat("HTTP status code:", status_code(repeating_instance_response), "\n")

if (status_code(repeating_instance_response) != 200) {
  error_content <- content(repeating_instance_response, as = "text", encoding = "UTF-8")
  cat("Error response:", error_content, "\n")
  stop("Error retrieving repeating data structures (HTTP ", status_code(repeating_instance_response), "): ", error_content)
}

repeating_data_list <- content(repeating_instance_response, as = "parsed", type = "application/json", encoding = "UTF-8")
cat("Successfully retrieved repeating data structures\n")

# Kies de juiste repeating data structure op basis van de CSV-bestandsnaam in output_data
cat("\n=== Selecting repeating data structure ===\n")
rd_items <- repeating_data_list$`_embedded`$repeatingData
cat("Found", length(rd_items), "repeating data structure(s)\n")

if (is.null(rd_items) || length(rd_items) == 0) {
  stop("No repeating data structures found in study.")
}

rd_target_name <- tools::file_path_sans_ext(basename(csv_file))
cat("Target name from CSV:", rd_target_name, "\n")

rd_names <- vapply(rd_items, function(x) {
  n <- x$name
  if (is.null(n) || is.na(n)) "" else as.character(n)
}, character(1))
cat("Available repeating data names:", paste(rd_names, collapse = ", "), "\n")

# Normaliseer beide strings: lowercase en vervang - door _
normalize_name <- function(name) {
  tolower(gsub("-", "_", name))
}

normalized_target <- normalize_name(rd_target_name)
normalized_names <- sapply(rd_names, normalize_name)
cat("Normalized target:", normalized_target, "\n")
cat("Normalized available:", paste(normalized_names, collapse = ", "), "\n")

match_idx <- which(normalized_names == normalized_target)
cat("Match indices:", if(length(match_idx) == 0) "NONE" else paste(match_idx, collapse = ", "), "\n")

if (length(match_idx) == 1) {
  repeating_data_id <- rd_items[[match_idx]]$id
  repeating_data_name <- rd_items[[match_idx]]$name
  cat("Selected repeating data:", repeating_data_name, "(", repeating_data_id, ")\n")
  if (exists("epic2castor_status_update")) {
    try(epic2castor_status_update(step = "repeating_data", detail = paste("Using:", repeating_data_name)), silent = TRUE)
  }
} else {
  stop(paste0(
    "Could not uniquely match repeating data structure for target name '", rd_target_name,
    "'. Available: ", paste(rd_names, collapse = ", ")
  ))
}

# ======== STEP 6: Create a new repeating data instance ===========
cat("\n=== Loading follow-up records ===\n")
follow_up_json <- fromJSON(json_file, simplifyVector = FALSE)
records <- follow_up_json$records
cat("Total records loaded:", length(records), "\n")

cat("Grouping records by participant...\n")
records_by_participant <- split(records, sapply(records, function(rec) rec$fields$`Participant.Id`))
cat("Number of participants:", length(records_by_participant), "\n")

# Helper: normaliseer waarden voor Castor API
format_castor_value <- function(field_var, value) {
  # Lege / NULL -> NA_char, zodat we kunnen skippen
  if (is.null(value) || length(value) == 0) return(NA_character_)

  x <- as.character(value)
  if (length(x) == 0) return(NA_character_)
  if (identical(x, "") || identical(x, "NA")) return(NA_character_)

  # Datumvelden naar d-m-Y (Castor verwacht dag-maand-jaar)
  if (identical(field_var, "fup_visit_date")) {
    parsed <- tryCatch(as.Date(x), error = function(e) NA)
    if (is.na(parsed)) {
      maybe_ymd <- tryCatch(as.Date(x, format = "%Y-%m-%d"), error = function(e) NA)
      if (!is.na(maybe_ymd)) {
        return(format(maybe_ymd, "%d-%m-%Y"))
      }
      maybe_dmy <- tryCatch(as.Date(x, format = "%d-%m-%Y"), error = function(e) NA)
      if (!is.na(maybe_dmy)) {
        return(format(maybe_dmy, "%d-%m-%Y"))
      }
      return(NA_character_)
    } else {
      return(format(parsed, "%d-%m-%Y"))
    }
  }

  return(x)
}

cat("\n=== Processing instances for each participant ===\n")
for (participant_id in names(records_by_participant)) {
  aantal_nodig <- length(records_by_participant[[participant_id]])
  cat("\n--- Participant:", participant_id, "---\n")
  cat("  Records needed:", aantal_nodig, "\n")

  # Stap 6a: Haal bestaande instances op
  instances_url <- paste0(api_base_url, "/study/", study_id, "/participant/", participant_id, "/repeating-data-instance")
  cat("  Fetching instances from:", instances_url, "\n")
  
  instances_response <- tryCatch({
    GET(instances_url, headers)
  }, error = function(e) {
    cat("  ERROR in GET request:", conditionMessage(e), "\n")
    stop("Failed to retrieve instances for participant ", participant_id, ": ", conditionMessage(e))
  })
  
  cat("  HTTP status code:", status_code(instances_response), "\n")

  if (status_code(instances_response) == 200) {
    instances_payload <- content(instances_response, as = "parsed", type = "application/json", encoding = "UTF-8")
    all_instances_list <- instances_payload$`_embedded`$repeatingDataInstance
    if (is.null(all_instances_list)) all_instances_list <- list()
    
    # Filter alleen instances die bij deze repeating data structure (follow-up) horen
    # Let op: repeating_data_id is NULL in de API response, gebruik repeating_data_name
    instances_list <- Filter(function(inst) {
      !is.null(inst$repeating_data_name) && inst$repeating_data_name == repeating_data_name
    }, all_instances_list)
    
    aantal_bestaand <- length(instances_list)
    cat("  Total instances:", length(all_instances_list), "| Follow-up instances:", aantal_bestaand, "\n")
  } else if (status_code(instances_response) == 404) {
    aantal_bestaand <- 0
    instances_list <- list()
    cat("  No instances found (404)\n")
  } else {
    stop("Error retrieving repeating data instances for participant ", participant_id, ": ",
         content(instances_response, as = "text", encoding = "UTF-8"))
  }
  
  cat("  Existing follow-up instances:", aantal_bestaand, "\n")

  # Stap 6b: Creëer nieuwe instances als nodig
  if (aantal_nodig > aantal_bestaand) {
    aantal_aanmaken <- aantal_nodig - aantal_bestaand
    cat("  Need to create", aantal_aanmaken, "new instances.\n")

    # Bepaal het hoogste bestaande instance nummer uit de namen
    max_existing_index <- 0
    if (aantal_bestaand > 0) {
      for (inst in instances_list) {
        inst_name <- inst$name
        if (!is.null(inst_name) && grepl(" - [0-9]+$", inst_name)) {
          suffix <- sub("^.* - ([0-9]+)$", "\\1", inst_name)
          idx <- as.integer(suffix)
          if (!is.na(idx) && idx > max_existing_index) {
            max_existing_index <- idx
          }
        }
      }
    }
    cat("  Highest existing follow-up instance index:", max_existing_index, "\n")

    for (i in seq_len(aantal_aanmaken)) {
      instance_index <- max_existing_index + i
      instance_name <- paste0("Follow-up - ", instance_index)
      
      if (exists("epic2castor_status_update")) {
        try(epic2castor_status_update(step = "create_instances",
                                       detail = sprintf("Participant %s: creating instance %s (%d/%d)",
                                                        participant_id, instance_name, i, aantal_aanmaken)), silent = TRUE)
      }

      create_instance_url <- paste0(base_url, "/api/study/", study_id, "/participant/", participant_id, "/repeating-data-instance")
      new_instance_body <- list(
        repeating_data_id = repeating_data_id,
        repeating_data_name_custom = instance_name
      )
      create_instance_response <- POST(
        url = create_instance_url,
        add_headers(Authorization = paste("Bearer", access_token),
                    `Content-Type` = "application/json",
                    Accept = "application/hal+json"),
        body = toJSON(new_instance_body, auto_unbox = TRUE),
        encode = "json"
      )

      if (status_code(create_instance_response) %in% c(200, 201)) {
        cat("    Instance created successfully:", instance_name, "(", i, "/", aantal_aanmaken, ")\n")
      } else {
        error_text <- content(create_instance_response, as = "text", encoding = "UTF-8")
        cat("    Error creating instance:", error_text, "\n")
      }
    }

    # Haal de instances opnieuw op om ze te updaten
    instances_response <- GET(instances_url, headers)
    if (status_code(instances_response) != 200) {
      stop("Error retrieving repeating data instances for participant ", participant_id, " after creation: ",
           content(instances_response, as = "text", encoding = "UTF-8"))
    }
    instances_payload <- content(instances_response, as = "parsed", type = "application/json", encoding = "UTF-8")
    all_instances_list <- instances_payload$`_embedded`$repeatingDataInstance
    if (is.null(all_instances_list)) all_instances_list <- list()
    
    # Filter follow-up instances op basis van naam
    instances_list <- Filter(function(inst) {
      inst_name <- inst$name
      !is.null(inst_name) && grepl("^Follow-up -", inst_name)
    }, all_instances_list)
    
    cat("  After creation, total instances:", length(all_instances_list), "| Follow-up instances:", length(instances_list), "\n")
  }
}

# ======== STEP 7: Add data to the repeating data instance ===========
# Stream JSON: open bestand nu al en voeg elke datapoint direct toe
datastructure_path <- if (exists("epc_path")) epc_path("castor_datastructure_file") else {
  file.path("castor_export", "Datastructure.json")
}
datastructure_con <- file(datastructure_path, open = "w", encoding = "UTF-8")
# Start JSON array en flush direct naar disk zodat het bestand niet leeg blijft bij vroege fouten
writeLines("[", datastructure_con)
flush(datastructure_con)
first_item <- TRUE

# Helper: maak een index->instance_id mapping op basis van naam-suffix (.. - n) of volgorde
build_instance_index_map <- function(instances_payload) {
  instances_list <- instances_payload$`_embedded`$repeatingDataInstance
  if (is.null(instances_list) || length(instances_list) == 0) {
    return(list())
  }

  # Probeer eerst via naam-suffix (bv "Follow-up - 2" -> index 2)
  indexed_map <- list()
  for (inst in instances_list) {
    inst_name <- inst$name
    if (is.null(inst_name) || !grepl(" - [0-9]+$", inst_name)) next
    suffix <- sub("^.* - ([0-9]+)$", "\\1", inst_name)
    idx <- as.integer(suffix)
    if (!is.na(idx)) {
      indexed_map[[as.character(idx)]] <- inst$id
    }
  }

  # Als er geen gesuffixeerde namen zijn, gebruik dan de volgorde
  if (length(indexed_map) == 0) {
    for (i in seq_along(instances_list)) {
      indexed_map[[as.character(i)]] <- instances_list[[i]]$id
    }
  }

  return(indexed_map)
}

tryCatch({
for (participant_id in names(records_by_participant)) {
  cat("\n=== Processing participant:", participant_id, "===\n")
  participant_records <- records_by_participant[[participant_id]]

  # Haal instances opnieuw op (voor het geval ze net zijn aangemaakt)
  instances_url <- paste0(api_base_url, "/study/", study_id, "/participant/", participant_id, "/repeating-data-instance")
  instances_response <- GET(instances_url, headers)

  if (status_code(instances_response) != 200) {
    stop("Error retrieving repeating data instances for participant ", participant_id, ": ",
         content(instances_response, as = "text", encoding = "UTF-8"))
  }

  instances_payload <- content(instances_response, as = "parsed", type = "application/json", encoding = "UTF-8")
  all_instances_list <- instances_payload$`_embedded`$repeatingDataInstance
  if (is.null(all_instances_list)) all_instances_list <- list()
  
  # Filter follow-up instances op basis van repeating_data_name (repeating_data_id is NULL in API)
  instances_list <- Filter(function(inst) {
    !is.null(inst$repeating_data_name) && inst$repeating_data_name == repeating_data_name
  }, all_instances_list)
  
  cat("  Total instances:", length(all_instances_list), "| Follow-up instances:", length(instances_list), "\n")

  # Build index->instance_id mapping (alleen voor follow-up instances)
  # Maak een aangepaste payload met alleen follow-up instances
  filtered_payload <- list(`_embedded` = list(repeatingDataInstance = instances_list))
  instance_map <- build_instance_index_map(filtered_payload)

  if (length(participant_records) > length(instance_map)) {
    cat("  Warning: Participant", participant_id, "has", length(participant_records),
        "records, but only", length(instance_map), "instances found.\n")
  }

  # Loop door de records en update de corresponderende instance
  for (rec_idx in seq_along(participant_records)) {
    record <- participant_records[[rec_idx]]
    fields <- record$fields

    instance_id <- instance_map[[as.character(rec_idx)]]
    if (is.null(instance_id)) {
      cat("  Warning: No instance found for record index", rec_idx, "- skipping.\n")
      next
    }

    cat("\n  Record", rec_idx, "-> Instance ID:", instance_id, "\n")
    rep_name <- paste0("Follow-up - ", rec_idx)

    if (exists("epic2castor_status_update")) {
      try(epic2castor_status_update(step = "upload",
                                     detail = sprintf("Participant %s: record %d/%d",
                                                      participant_id, rec_idx, length(participant_records))), silent = TRUE)
    }

    # Remove unwanted meta fields
    unwanted_fields <- c("Participant.Id", "Participant.Status", "Site.Abbreviation",
                         "Repeating.data.Name.Custom", "Repeating.Data.Creation.Date")
    record_data <- fields[!(names(fields) %in% unwanted_fields)]

    # ======== BATCH UPLOAD: Verzamel alle velden voor deze instance ========
    fields_to_upload <- list()  # field_id -> field_value
    fields_metadata <- list()   # field_id -> field_name (voor logging)

    for (field_name in names(record_data)) {
      # Special case: date_baseline maps to fup_visit_date for follow-up
      if (field_name == "date_baseline") {
        clean_field_var <- "fup_visit_date"
      } else {
        # Strip the foll_ prefix to get Castor field name (e.g., foll_fup_visit_location -> fup_visit_location)
        clean_field_var <- sub("^foll_", "", field_name)
      }
      
      # Lookup field ID in mapping
      field_id <- tryCatch({
        if (clean_field_var %in% names(mapping_list)) {
          mapping_list[[clean_field_var]]
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (is.null(field_id) || is.na(field_id)) {
        cat("    No field ID found for:", field_name, "(mapped as:", clean_field_var, ")\n")
        dp_done <- dp_done + 1
        next
      }

      raw_value <- record_data[[field_name]]
      field_value <- format_castor_value(clean_field_var, raw_value)

      # Behandel placeholders als leeg
      if (!is.na(field_value) && tolower(trimws(field_value)) %in% c("not set", "n/a", "na")) {
        field_value <- NA_character_
      }

      if (is.na(field_value)) {
        cat("    Skipping field", field_name, "because value is NA/empty\n")
        dp_done <- dp_done + 1
        next
      }

      fields_to_upload[[field_id]] <- field_value
      fields_metadata[[field_id]] <- field_name
    }

    # ======== BATCH UPLOAD UITVOEREN (1 API call) ========
    if (length(fields_to_upload) > 0) {
      cat("  === Batch uploading", length(fields_to_upload), "fields for", participant_id, rep_name, "===\n")

      result <- batch_upload_with_fallback(
        access_token = access_token,
        base_url = base_url,
        api_base_url = api_base_url,
        study_id = study_id,
        participant_id = participant_id,
        instance_id = instance_id,
        fields_list = fields_to_upload,
        fields_metadata = fields_metadata,
        rep_name = rep_name,
        existing_map = list()  # No existing data check for now
      )

      # Update progress counter
      dp_done <- dp_done + result$uploaded + result$failed + result$skipped

      if (exists("epic2castor_status_update")) {
        detail_msg <- sprintf("Batch: %d uploaded, %d skipped, %d failed (%s %s)",
                             result$uploaded, result$skipped, result$failed,
                             participant_id, rep_name)
        try(epic2castor_status_update(
          step = "datapoints",
          current = dp_done,
          detail = detail_msg
        ), silent = TRUE)
      }

    } else {
      cat("  No fields to upload for", participant_id, rep_name, "\n")
    }
  }
}
}, finally = {
  # Sluit JSON array
  writeLines("\n]", datastructure_con)
  close(datastructure_con)
  cat("\nDatapoint log saved to:", datastructure_path, "\n")
  
  # Final status: mark as done with log file location
  if (exists("epic2castor_status_done")) {
    log_file <- getOption("epic2castor.logger.file", default = NA_character_)
    if (is.null(log_file) || is.na(log_file)) {
      # Fallback to run_dir/script name
      run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
      script_base <- "follow_upExport"
      if (nzchar(run_dir)) log_file <- file.path(run_dir, paste0(script_base, "_log.txt")) else log_file <- paste0(script_base, "_log.txt")
    }
    try(epic2castor_status_done(detail = paste("Done. Log:", log_file)), silent = TRUE)
  }
})

cat("\n=== Follow-up upload completed ===\n")
cat("EPIC2CASTOR::DONE\n")
flush(stdout())
flush(stderr())

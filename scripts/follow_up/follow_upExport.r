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

invisible(lapply(c("httr", "httr2", "jsonlite", "uuid", "RSQLite", "DBI"), ensure_pkg))

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

# ======== httr2 parallel helpers =========
build_post_req <- function(url, token, payload_json) {
	httr2::request(url) |>
		httr2::req_headers(
			Authorization = paste("Bearer", token),
			`Content-Type` = "application/json",
			Accept = "application/hal+json"
		) |>
		httr2::req_body_raw(payload_json, type = "application/json") |>
		httr2::req_retry(max_tries = 3, is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 503L)) |>
		httr2::req_error(is_error = function(resp) FALSE)
}

build_get_req <- function(url, token) {
	httr2::request(url) |>
		httr2::req_headers(
			Authorization = paste("Bearer", token),
			Accept = "application/hal+json"
		) |>
		httr2::req_retry(max_tries = 3, is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 503L)) |>
		httr2::req_error(is_error = function(resp) FALSE)
}

perform_parallel <- function(reqs, max_active = 6L) {
	if (length(reqs) == 0L) return(list())
	httr2::req_perform_parallel(reqs, max_active = max_active, on_error = "continue")
}

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

# Helper: haal alle repeating data instances op (alle pagina's)
fetch_all_instances <- function(url) {
  all_items <- list(); next_url <- url; page_count <- 0
  repeat {
    page_count <- page_count + 1
    resp <- GET(next_url, headers)
    if (status_code(resp) != 200) break
    cont <- content(resp, as = "parsed", type = "application/json", encoding = "UTF-8")
    items <- cont$`_embedded`$repeatingDataInstance
    if (!is.null(items) && length(items) > 0) all_items <- c(all_items, items)
    next_link <- tryCatch({ cont$`_links`$`next`$href }, error = function(e) NULL)
    if (is.null(next_link) || identical(next_link, "")) break
    next_url <- next_link
    if (page_count > 100) break
  }
  list(`_embedded` = list(repeatingDataInstance = all_items))
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

# Cache: sla per participant de instances op + welke indices nieuw zijn
cached_instances   <- list()
new_instance_ids   <- list()

for (participant_id in names(records_by_participant)) {
  aantal_nodig <- length(records_by_participant[[participant_id]])
  cat("\n--- Participant:", participant_id, "---\n")
  cat("  Records needed:", aantal_nodig, "\n")

  # Stap 6a: Haal bestaande instances op
  instances_url <- paste0(api_base_url, "/study/", study_id, "/participant/", participant_id, "/repeating-data-instance")
  cat("  Fetching instances from:", instances_url, "\n")
  
  instances_payload <- fetch_all_instances(instances_url)
  all_instances_list <- instances_payload$`_embedded`$repeatingDataInstance
  if (is.null(all_instances_list)) all_instances_list <- list()
  
  # Filter alleen instances die bij deze repeating data structure (follow-up) horen
  instances_list <- Filter(function(inst) {
    !is.null(inst$repeating_data_name) && inst$repeating_data_name == repeating_data_name
  }, all_instances_list)
  
  aantal_bestaand <- length(instances_list)
  cat("  Total instances:", length(all_instances_list), "| Follow-up instances:", aantal_bestaand, "\n")

  # Stap 6b: Creëer nieuwe instances als nodig (parallel)
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

    if (exists("epic2castor_status_update")) {
      try(epic2castor_status_update(step = "instances", detail = paste("Creating", aantal_aanmaken, "instances for", participant_id)), silent = TRUE)
    }

    # Bouw parallel requests voor instance creatie
    create_reqs <- lapply(seq_len(aantal_aanmaken), function(i) {
      instance_index <- max_existing_index + i
      instance_name <- paste0("Follow-up - ", instance_index)
      payload <- jsonlite::toJSON(list(repeating_data_id = repeating_data_id, repeating_data_name_custom = instance_name), auto_unbox = TRUE)
      url <- paste0(api_base_url, "/study/", study_id, "/participant/", participant_id, "/repeating-data-instance")
      build_post_req(url, access_token, payload)
    })

    # Voer parallel uit
    create_resps <- perform_parallel(create_reqs)

    # Verwerk resultaten
    new_ids_for_p <- character(0)
    for (k in seq_along(create_resps)) {
      resp <- create_resps[[k]]
      instance_index <- max_existing_index + k
      instance_name <- paste0("Follow-up - ", instance_index)
      if (inherits(resp, "httr2_response") && httr2::resp_status(resp) %in% c(200L, 201L)) {
        resp_body <- httr2::resp_body_json(resp)
        new_id <- resp_body$id
        if (!is.null(new_id)) {
          new_ids_for_p <- c(new_ids_for_p, new_id)
          instances_list <- c(instances_list, list(list(
            id = new_id, name = instance_name,
            repeating_data_name = repeating_data_name
          )))
        }
        cat("    Instance created successfully:", instance_name, "(", k, "/", aantal_aanmaken, ")\n")
      } else {
        status <- if (inherits(resp, "httr2_response")) httr2::resp_status(resp) else "error"
        error_text <- tryCatch(httr2::resp_body_string(resp), error = function(e) as.character(e))
        cat("    Error creating instance:", instance_name, "- status:", status, "\n")
        cat("    Response:", error_text, "\n")
      }
    }
    new_instance_ids[[participant_id]] <- new_ids_for_p
    cat("  After creation, follow-up instances:", length(instances_list), "\n")
  } else {
    new_instance_ids[[participant_id]] <- character(0)
  }

  # Cache de volledige (gefilterde) instances payload voor Stap 7
  cached_instances[[participant_id]] <- list(`_embedded` = list(repeatingDataInstance = instances_list))
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

  if (exists("epic2castor_status_update")) {
    try(epic2castor_status_update(step = "datapoints", detail = paste("Participant", participant_id, "- preparing upload")), silent = TRUE)
  }

  # ---- Gebruik GECACHTE instances (geen re-fetch!) ----
  instances_content <- cached_instances[[participant_id]]
  if (is.null(instances_content)) {
    cat("Warning: no cached instances for", participant_id, "- fetching now\n")
    instances_url <- paste0(api_base_url, "/study/", study_id, "/participant/", participant_id, "/repeating-data-instance")
    instances_content <- fetch_all_instances(instances_url)
    all_tmp <- instances_content$`_embedded`$repeatingDataInstance
    if (is.null(all_tmp)) all_tmp <- list()
    instances_content <- list(`_embedded` = list(repeatingDataInstance = Filter(function(inst) {
      !is.null(inst$repeating_data_name) && inst$repeating_data_name == repeating_data_name
    }, all_tmp)))
  }

  instance_map <- build_instance_index_map(instances_content)

  new_ids <- new_instance_ids[[participant_id]]
  if (is.null(new_ids)) new_ids <- character(0)

  cat("  Follow-up instances:", length(instance_map), "| New:", length(new_ids), "\n")
  cat("  Instance index mapping:", paste(sprintf("%s=%s", names(instance_map), unlist(instance_map)), collapse = ", "), "\n")

  normalize_for_compare <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (is.list(x)) x <- unlist(x, use.names = FALSE)
    x <- as.character(x); x <- x[!is.na(x)]; if (length(x) == 0) return(NA_character_)
    x <- gsub(";", ",", x, fixed = TRUE); trimws(paste(x, collapse = ","))
  }
  normalize_date <- function(s) {
    if (is.null(s) || is.na(s) || identical(s, "")) return(NA_character_)
    s <- gsub("[./]", "-", trimws(as.character(s)))
    if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", s)) { d <- strsplit(s, "-")[[1]]; return(sprintf("%02d-%02d-%04d", as.integer(d[3]), as.integer(d[2]), as.integer(d[1]))) }
    if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", s)) { d <- strsplit(s, "-")[[1]]; return(sprintf("%02d-%02d-%04d", as.integer(d[1]), as.integer(d[2]), as.integer(d[3]))) }
    s
  }
  values_equal <- function(field_var, old_val, new_val) {
    if (is.null(old_val) && is.null(new_val)) return(TRUE)
    if (length(old_val) == 0 && length(new_val) == 0) return(TRUE)
    if (is.null(old_val) || is.null(new_val)) return(FALSE)
    a <- trimws(as.character(old_val)); b <- trimws(as.character(new_val))
    if (tolower(a) %in% c("not set","n/a","na")) a <- ""; if (tolower(b) %in% c("not set","n/a","na")) b <- ""
    if (!is.null(field_var) && grepl("date|datum", field_var, ignore.case = TRUE)) {
      na <- normalize_date(a); nb <- normalize_date(b); return(!is.na(na) && !is.na(nb) && identical(na, nb))
    }
    if (grepl("[,;]", a) || grepl("[,;]", b)) {
      sa <- sort(tolower(trimws(unlist(strsplit(a, "[,;]"))))); sa <- sa[sa != ""]
      sb <- sort(tolower(trimws(unlist(strsplit(b, "[,;]"))))); sb <- sb[sb != ""]
      return(identical(sa, sb))
    }
    is_num <- function(x) grepl("^-?\\d+(,?\\d+)*(\\.\\d+)?$", gsub(" ", "", x))
    if (is_num(a) && is_num(b)) {
      an <- suppressWarnings(as.numeric(gsub(",", ".", a))); bn <- suppressWarnings(as.numeric(gsub(",", ".", b)))
      if (!is.na(an) && !is.na(bn)) return(abs(an - bn) < 1e-9)
    }
    identical(tolower(a), tolower(b))
  }

  datapoints_to_value_map <- function(dp_payload) {
    items <- NULL
    if (!is.null(dp_payload$`_embedded`)) {
      emb <- dp_payload$`_embedded`
      if (!is.null(emb$dataPoints)) items <- emb$dataPoints
      if (is.null(items) && !is.null(emb$data_points)) items <- emb$data_points
      if (is.null(items) && !is.null(emb$dataPoint)) items <- emb$dataPoint
      if (is.null(items) && !is.null(emb$data_point)) items <- emb$data_point
      if (is.null(items) && !is.null(emb$items)) items <- emb$items
    }
    if (is.null(items) && is.list(dp_payload) && length(dp_payload) > 0) {
      first <- dp_payload[[1]]
      if (is.list(first) && ("field_id" %in% names(first) || "field" %in% names(first))) items <- dp_payload
    }
    map <- list(); if (is.null(items) || length(items) == 0) return(map)
    extract_opts <- function(lst) {
      if (is.null(lst)) return(NULL)
      if (is.list(lst) && !is.null(lst[[1]]) && is.list(lst[[1]])) {
        return(unlist(lapply(lst, function(o) { if (!is.null(o$value)) return(o$value); if (!is.null(o$option_value)) return(o$option_value); if (!is.null(o$id)) return(o$id); if (!is.null(o$label)) return(o$label); return(NA_character_) }), use.names = FALSE))
      }
      lst
    }
    for (dp in items) {
      fid <- NULL
      if (!is.null(dp$field_id)) fid <- dp$field_id else if (!is.null(dp$field) && !is.null(dp$field$id)) fid <- dp$field$id else if (!is.null(dp$field) && !is.null(dp$field$field_id)) fid <- dp$field$field_id else if (!is.null(dp$field) && !is.null(dp$field$variable_name)) {
        varnm <- as.character(dp$field$variable_name); if (!is.null(mapping_list[[varnm]])) fid <- as.character(mapping_list[[varnm]])
      }
      if (is.null(fid)) next
      val <- NULL
      if (!is.null(dp$field_value)) val <- dp$field_value
      if (is.null(val) && !is.null(dp$value)) val <- dp$value
      if (is.null(val) && !is.null(dp$raw_value)) val <- dp$raw_value
      if (is.null(val) && !is.null(dp$option_value)) val <- dp$option_value
      if (is.null(val) && !is.null(dp$options)) val <- extract_opts(dp$options)
      if (is.null(val) && !is.null(dp$selected_options)) val <- extract_opts(dp$selected_options)
      norm <- normalize_for_compare(val); fid_chr <- as.character(fid)
      if (!is.null(map[[fid_chr]]) && !is.na(map[[fid_chr]]) && !is.na(norm)) {
        combined <- paste0(map[[fid_chr]], ",", norm); map[[fid_chr]] <- normalize_for_compare(combined)
      } else { map[[fid_chr]] <- norm }
    }
    map
  }

  # ==== FASE A: Parallel pre-fetch datapoints voor BESTAANDE instances ====
  old_instance_indices <- character(0)
  old_dp_urls <- list()
  for (rec_idx in seq_along(participant_records)) {
    inst_id <- instance_map[[as.character(rec_idx)]]
    if (!is.null(inst_id) && !(inst_id %in% new_ids)) {
      key <- as.character(rec_idx)
      old_instance_indices <- c(old_instance_indices, key)
      old_dp_urls[[key]] <- paste0(
        api_base_url, "/study/", study_id,
        "/participant/", participant_id,
        "/data-points/repeating-data-instance/", inst_id
      )
    }
  }

  existing_maps <- list()
  if (length(old_dp_urls) > 0) {
    cat("  Parallel pre-fetching datapoints for", length(old_dp_urls), "existing instances...\n")
    if (exists("epic2castor_status_update")) {
      try(epic2castor_status_update(step = "datapoints", detail = paste("Pre-fetch", length(old_dp_urls), "old instances for", participant_id)), silent = TRUE)
    }
    dp_reqs <- lapply(old_dp_urls, function(url) build_get_req(url, access_token))
    dp_resps <- perform_parallel(dp_reqs)
    for (k in seq_along(dp_resps)) {
      idx_key <- names(old_dp_urls)[k]
      resp <- dp_resps[[k]]
      dp_data <- list(`_embedded` = list(dataPoints = list()))
      if (inherits(resp, "httr2_response") && httr2::resp_status(resp) == 200L) {
        cont <- httr2::resp_body_json(resp)
        if (!is.null(cont$`_embedded`$items)) {
          dp_data <- list(`_embedded` = list(dataPoints = cont$`_embedded`$items))
        } else if (!is.null(cont$`_embedded`$dataPoints)) {
          dp_data <- list(`_embedded` = list(dataPoints = cont$`_embedded`$dataPoints))
        }
      }
      existing_maps[[idx_key]] <- datapoints_to_value_map(dp_data)
    }
    cat("  Pre-fetch done:", length(existing_maps), "instance(s) loaded\n")
  } else {
    cat("  All instances are new - skipping pre-fetch of existing datapoints\n")
  }

  # ==== FASE B: Bouw batch payloads voor ALLE instances ====
  batch_jobs <- list()
  for (rec_idx in seq_along(participant_records)) {
    record <- participant_records[[rec_idx]]
    fields <- record$fields
    rep_name <- paste0("Follow-up - ", rec_idx)

    instance_id <- instance_map[[as.character(rec_idx)]]
    if (is.null(instance_id)) {
      cat("  Warning: No instance found for record index", rec_idx, "- skipping.\n")
      if (exists("dp_done") && exists("num_fields_per_record")) {
        dp_done <- dp_done + num_fields_per_record
        if (exists("epic2castor_status_update")) try(epic2castor_status_update(step = "datapoints", current = dp_done), silent = TRUE)
      }
      next
    }

    is_new_instance <- instance_id %in% new_ids
    existing_map <- if (is_new_instance) list() else (existing_maps[[as.character(rec_idx)]])
    if (is.null(existing_map)) existing_map <- list()

    # Write to datastructure file
    datapoints <- list(`_embedded` = list(dataPoints = list()))
    item <- list(participant_id = participant_id, instance_id = instance_id, instance_name = rep_name, datapoints = datapoints)
    item_json <- toJSON(item, pretty = TRUE, auto_unbox = TRUE)
    if (!first_item) writeLines(",", datastructure_con)
    writeLines(item_json, datastructure_con); flush(datastructure_con); first_item <- FALSE

    # Remove unwanted meta fields
    unwanted_fields <- c("Participant.Id", "Participant.Status", "Site.Abbreviation",
                         "Repeating.data.Name.Custom", "Repeating.Data.Creation.Date")
    record_data <- fields[!(names(fields) %in% unwanted_fields)]

    # Collect fields for batch upload
    fields_to_upload <- list()
    fields_metadata <- list()

    for (field_name in names(record_data)) {
      if (field_name == "date_baseline") {
        clean_field_var <- "fup_visit_date"
      } else {
        clean_field_var <- sub("^foll_", "", field_name)
      }
      
      field_id <- tryCatch({
        if (clean_field_var %in% names(mapping_list)) mapping_list[[clean_field_var]] else NULL
      }, error = function(e) NULL)

      if (is.null(field_id) || is.na(field_id)) {
        cat("    No field ID found for:", field_name, "(mapped as:", clean_field_var, ")\n")
        dp_done <- dp_done + 1
        next
      }

      raw_value <- record_data[[field_name]]
      field_value <- format_castor_value(clean_field_var, raw_value)
      if (!is.na(field_value) && tolower(trimws(field_value)) %in% c("not set", "n/a", "na")) field_value <- NA_character_
      if (is.na(field_value)) {
        dp_done <- dp_done + 1
        next
      }

      fields_to_upload[[field_id]] <- field_value
      fields_metadata[[field_id]] <- field_name
    }

    if (length(fields_to_upload) > 0) {
      # Filter unchanged fields lokaal
      skipped_count <- 0
      if (length(existing_map) > 0) {
        filtered <- list()
        for (fid in names(fields_to_upload)) {
          existing_val_norm <- existing_map[[fid]]
          new_val_norm <- normalize_for_compare(fields_to_upload[[fid]])
          if (!is.null(existing_val_norm) && !is.na(existing_val_norm) &&
              !is.na(new_val_norm) &&
              tolower(trimws(existing_val_norm)) == tolower(trimws(new_val_norm))) {
            skipped_count <- skipped_count + 1
            next
          }
          filtered[[fid]] <- fields_to_upload[[fid]]
        }
        fields_to_upload <- filtered
      }

      if (length(fields_to_upload) > 0) {
        datapoints_array <- lapply(names(fields_to_upload), function(fid) {
          list(field_id = fid, instance_id = instance_id, field_value = fields_to_upload[[fid]])
        })
        payload_json <- jsonlite::toJSON(list(data = datapoints_array), auto_unbox = TRUE)
        batch_url <- paste0(api_base_url, "/study/", study_id,
          "/participant/", participant_id,
          "/data-points/repeating-data-instance/", instance_id)

        batch_jobs[[length(batch_jobs) + 1]] <- list(
          index = rec_idx,
          instance_id = instance_id,
          participant_id = participant_id,
          rep_name = rep_name,
          fields_count = length(fields_to_upload),
          skipped = skipped_count,
          fields_list = fields_to_upload,
          fields_metadata = fields_metadata,
          url = batch_url,
          payload = payload_json
        )
      } else {
        dp_done <- dp_done + skipped_count
        cat("  All fields unchanged for", participant_id, rep_name, "- skipped", skipped_count, "\n")
      }
    }
  }

  # ==== FASE C: Parallel batch uploads ====
  if (length(batch_jobs) > 0) {
    cat("  Firing", length(batch_jobs), "batch uploads in parallel for", participant_id, "\n")
    if (exists("epic2castor_status_update")) {
      try(epic2castor_status_update(step = "datapoints", detail = paste("Parallel upload:", length(batch_jobs), "batches for", participant_id)), silent = TRUE)
    }

    post_reqs <- lapply(batch_jobs, function(job) build_post_req(job$url, access_token, job$payload))
    post_resps <- perform_parallel(post_reqs)

    for (k in seq_along(post_resps)) {
      resp <- post_resps[[k]]
      job <- batch_jobs[[k]]

      if (inherits(resp, "httr2_response") && httr2::resp_status(resp) %in% c(200L, 201L, 207L)) {
        resp_body <- httr2::resp_body_json(resp)
        n_ok <- resp_body$total_success
        if (is.null(n_ok)) n_ok <- 0
        n_fail <- resp_body$total_failed
        if (is.null(n_fail)) n_fail <- 0

        dp_done <- dp_done + n_ok + n_fail + job$skipped
        if (n_fail > 0) {
          cat("  Partial:", n_ok, "ok,", n_fail, "fail for", job$participant_id, job$rep_name, "\n")
          failed_items <- resp_body$failed
          if (!is.null(failed_items) && length(failed_items) > 0) {
            for (fi in failed_items) {
              fid <- fi$field_id
              if (!is.null(fid) && fid %in% names(job$fields_list)) {
                clean_var <- job$fields_metadata[[fid]]
                if (is.null(clean_var)) clean_var <- fid
                success <- individual_upload_datapoint(
                  access_token = access_token, base_url = base_url,
                  api_base_url = api_base_url, study_id = study_id,
                  participant_id = job$participant_id, instance_id = job$instance_id,
                  field_id = fid, field_value = job$fields_list[[fid]],
                  clean_field_var = clean_var, rep_name = job$rep_name
                )
                if (success) { n_ok <- n_ok + 1; n_fail <- n_fail - 1 }
              }
            }
          }
        } else {
          cat("  Batch OK:", n_ok, "fields for", job$participant_id, job$rep_name, "\n")
        }
      } else {
        status_txt <- if (inherits(resp, "httr2_response")) httr2::resp_status(resp) else "error"
        cat("  Batch failed (status:", status_txt, ") for", job$participant_id, job$rep_name, "- retrying individually\n")
        n_ok <- 0; n_fail <- 0
        for (fid in names(job$fields_list)) {
          clean_var <- job$fields_metadata[[fid]]
          if (is.null(clean_var)) clean_var <- fid
          success <- individual_upload_datapoint(
            access_token = access_token, base_url = base_url,
            api_base_url = api_base_url, study_id = study_id,
            participant_id = job$participant_id, instance_id = job$instance_id,
            field_id = fid, field_value = job$fields_list[[fid]],
            clean_field_var = clean_var, rep_name = job$rep_name
          )
          if (success) n_ok <- n_ok + 1 else n_fail <- n_fail + 1
        }
        dp_done <- dp_done + n_ok + n_fail + job$skipped
      }

      if (exists("epic2castor_status_update")) {
        try(epic2castor_status_update(step = "datapoints", current = dp_done,
          detail = paste("Done:", job$participant_id, job$rep_name)), silent = TRUE)
      }
    }
  } else {
    cat("  No batch jobs to upload for", participant_id, "\n")
  }
  cat("\n")
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

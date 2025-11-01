## Ensure CRAN mirror and required packages in non-interactive runs
if (is.null(getOption("repos")) || isTRUE(getOption("repos")["CRAN"] %in% c("", "@CRAN@"))) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
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
  try(epic2castor_status_init(total = NA_integer_, title = "Baseline Upload", step = "init", detail = "Starting"), silent = TRUE)
}

# Helper for getting error messages
getErrorMessage <- function(token_content) {
  error_info <- tryCatch(fromJSON(token_content, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(error_info) && is.list(error_info)) {
    if ("errors" %in% names(error_info)) {
      err_field <- error_info[["errors"]]
      if (is.list(err_field)) {
        return(err_field[[1]]$message)
      } else {
        return(err_field[1])
      }
    } else if ("error" %in% names(error_info)) {
      if ("error_description" %in% names(error_info)) {
        return(error_info$error_description)
      } else {
        return(error_info$error)
      }
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
db_path <- epc_path("castor_meta_db")  # Centralized
con <- dbConnect(SQLite(), dbname = db_path)
query <- "SELECT [Field Variable Name] as varname, [Field ID] as field_id FROM study_variablelist"
mapping_df <- dbGetQuery(con, query)
mapping_list <- setNames(mapping_df$field_id, mapping_df$varname)
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
    if (grepl("Client with id", error_message, ignore.case = TRUE)) {
      stop(paste0(error_message, ". \nMake sure your credentials are exactly the same as in Castor!"), call. = FALSE)
    } else if (grepl("client_secret", error_message, ignore.case = TRUE) ||
               grepl("client credentials", error_message, ignore.case = TRUE)) {
      stop(paste0("The client secret: ", client_secret, " is invalid. \nMake sure your credentials are exactly the same as in Castor!"),
           call. = FALSE)
    } else {
      stop("Error in obtaining the access token: ", error_message)
    }
  } else {
    stop("Error in obtaining the access token: ", status_code(token_response),
         ". Response: ", token_content)
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
csv_file <- file.path(epc_path("baseline_output_data_dir"), "baseline.csv")
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
                                detail = paste("Loaded", nrow(data), "records,", num_fields_per_record, "fields/record"),
                                force = TRUE), silent = TRUE)
}

records <- lapply(seq_len(nrow(data)), function(i) {
  list(
    record_id = as.character(i),
    fields = as.list(data[i, , drop = FALSE])
  )
})
json_payload_object <- list(records = records)
json_file <- file.path(epc_path("castor_export_dir"), "baseline.json")
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
    url <- paste0(api_base_url, "/study/", study_id, "/site")
    response <- GET(url, add_headers(Authorization = paste("Bearer", access_token)))
    if (status_code(response) != 200) {
      stop("Error retrieving sites: ", 
           content(response, as = "text", encoding = "UTF-8"))
    }
    sites <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    return(sites)
  }
  sites_raw <- get_sites(access_token, api_base_url, study_id)
  if ("_embedded" %in% names(sites_raw)) {
    sites_data <- sites_raw[["_embedded"]][["sites"]]
  } else {
    sites_data <- sites_raw
  }
  choices <- sapply(sites_data, function(x) { paste(x$site_id, x$name, sep = " - ") })
  if (exists("epic2castor_status_update")) {
    try(epic2castor_status_update(step = "sites", detail = paste("Loaded", length(choices), "sites")), silent = TRUE)
  }
  
  if (interactive() && requireNamespace("tcltk", quietly = TRUE)) {
    selected_choice <- tcltk::tk_select.list(choices, title = "Select a site")
  } else if (interactive()) {
    cat("Available sites:\n")
    for (i in seq_along(choices)) {
      cat(i, ":", choices[i], "\n")
    }
    index <- as.integer(readline(prompt = "Select a site (enter the number): "))
    if (is.na(index) || index < 1 || index > length(choices)) {
      stop("Invalid selection.")
    }
    selected_choice <- choices[index]
  } else {
    cat("Non-interactive mode: using default site:", choices[1], "\n")
    selected_choice <- choices[1]
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
    cat("Participant", participant_id, "not found. Creating participant...\n")
    create_url <- paste0(api_base_url, "/study/", study_id, "/participant")
    body <- list(
      participant_id = participant_id,
      site_id = site_id
    )
    create_response <- POST(create_url,
                            add_headers(
                              Authorization = paste("Bearer", access_token),
                              `Content-Type` = "application/json"
                            ),
                            body = body,
                            encode = "json")
    cat("Create participant status code:", status_code(create_response), "\n")
    if (status_code(create_response) %in% c(200, 201)) {
      cat("Participant", participant_id, "successfully created.\n")
    } else {
      stop("Failed to create participant ", participant_id, ". Response: ", 
           content(create_response, as = "text", encoding = "UTF-8"))
    }
  } else {
    cat("Participant", participant_id, "already exists.\n")
  }
}

unique_participants <- unique(data$`Participant.Id`)
for (participant_id in unique_participants) {
  if (exists("epic2castor_status_update")) {
    try(epic2castor_status_update(step = "participants", detail = paste("Ensure participant:", participant_id)), silent = TRUE)
  }
  check_or_create_participant(access_token, api_base_url, study_id, participant_id, selected_site)
}

# ======== STEP 5: Retrieve existing repeating data ===========
repeating_instance_url <- paste0(api_base_url, "/study/", study_id, "/repeating-data")
repeating_instance_response <- GET(repeating_instance_url, headers)

if (status_code(repeating_instance_response) != 200) {
    stop("Error retrieving sites: ", content(repeating_instance_response, as = "text", encoding = "UTF-8"))
}
repeating_data_list <- content(repeating_instance_response, as = "parsed", type = "application/json", encoding = "UTF-8")

# Kies de juiste repeating data structure op basis van de CSV-bestandsnaam in output_data
rd_items <- repeating_data_list$`_embedded`$repeatingData
if (is.null(rd_items) || length(rd_items) == 0) {
  stop("No repeating data structures found in study.")
}
rd_target_name <- tools::file_path_sans_ext(basename(csv_file))
rd_names <- vapply(rd_items, function(x) {
  n <- x$name
  if (is.null(n) || is.na(n)) "" else as.character(n)
}, character(1))
match_idx <- which(tolower(rd_names) == tolower(rd_target_name))
if (length(match_idx) == 1) {
  repeating_data_id <- rd_items[[match_idx]]$id
  repeating_data_name <- rd_items[[match_idx]]$name
  cat("Selected repeating data:", repeating_data_name, "(", repeating_data_id, ")\n")
  if (exists("epic2castor_status_update")) {
    try(epic2castor_status_update(step = "repeatingdata", detail = paste("Selected:", repeating_data_name)), silent = TRUE)
  }
} else {
  stop(paste0(
    "Could not find repeating data structure named '", rd_target_name,
    "'. Available: ", paste(rd_names, collapse = ", ")
  ))
}

# ======== STEP 6: Create a new repeating data instance ===========
baseline_json <- fromJSON(json_file, simplifyVector = FALSE)
records <- baseline_json$records
records_by_participant <- split(records, sapply(records, function(rec) rec$fields$`Participant.Id`))

# Helper: normaliseer waarden voor Castor API
format_castor_value <- function(field_var, value) {
  # Lege / NULL -> NA_char, zodat we kunnen skippen
  if (is.null(value) || length(value) == 0) return(NA_character_)

  x <- as.character(value)
  if (length(x) == 0) return(NA_character_)
  if (identical(x, "") || identical(x, "NA")) return(NA_character_)

  # Datumvelden naar d-m-Y (Castor verwacht dag-maand-jaar)
  if (identical(field_var, "visit_date")) {
    x2 <- gsub("[./]", "-", x)
    # yyyy-mm-dd -> dd-mm-yyyy
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x2)) {
      parts <- strsplit(x2, "-")[[1]]
      if (length(parts) == 3) {
        x <- sprintf("%02d-%02d-%04d", as.integer(parts[3]), as.integer(parts[2]), as.integer(parts[1]))
      }
    } else if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", x2)) {
      # d-m-Y of dd-mm-yyyy -> pad tot twee cijfers
      parts <- strsplit(x2, "-")[[1]]
      if (length(parts) == 3) {
        x <- sprintf("%02d-%02d-%04d", as.integer(parts[1]), as.integer(parts[2]), as.integer(parts[3]))
      }
    } else {
      # Laat overige formaten ongemoeid; server zal valideren
      x <- x2
    }
  }

  return(x)
}

for (participant_id in names(records_by_participant)) {
  aantal_nodig <- length(records_by_participant[[participant_id]])
  cat("Participant:", participant_id, "has", aantal_nodig, "record(s)\n")

  instance_get_url <- paste0(api_base_url, "/study/", study_id, "/participant/", participant_id, "/repeating-data-instance")
  existing_response <- GET(instance_get_url, headers)
  
  if (status_code(existing_response) == 200) {
    existing_content <- content(existing_response, as = "parsed", type = "application/json", encoding = "UTF-8")
    if (!is.null(existing_content$total_items)) {
      aantal_bestaand <- length(seq_len(existing_content$total_items))
    } else {
      aantal_bestaand <- 0
    }
  } else if (status_code(existing_response) == 404) {
    aantal_bestaand <- 0
  } else {
    stop("Error retrieving repeating data instances for participant ", participant_id,
         ": ", content(existing_response, "text", encoding = "UTF-8"))
  }

  cat("Existing instances for participant", participant_id, ":", aantal_bestaand, "\n")

  if (aantal_bestaand < aantal_nodig) {
    for (j in (aantal_bestaand + 1):aantal_nodig) {
      rep_name <- paste("Visit -", j)
      if (exists("epic2castor_status_update")) {
        try(epic2castor_status_update(step = "instances", detail = paste("Create instance", rep_name, "for", participant_id)), silent = TRUE)
      }
      # Castor API expects the custom instance name as 'name' (not 'repeating_data_name_custom')
      instance_payload <- list(
        repeating_data_id = repeating_data_id,
        name = rep_name
      )
      
      instance_url <- paste0(base_url, "/api/study/", study_id, "/participant/", participant_id, "/repeating-data-instance")
      instance_response <- POST(instance_url, 
                                  add_headers(Authorization = paste("Bearer", access_token),
                                              `Content-Type` = "application/json",
                                              Accept = "application/hal+json"),
                                  body = toJSON(instance_payload, auto_unbox = TRUE),
                                  encode = "json")
      
      if (status_code(instance_response) != 201) {
        cat("Error creating repeating data instance for participant", participant_id, ":\n")
        err_text <- content(instance_response, "text", encoding = "UTF-8")
        print(err_text)
        stop("Error in Repeating Data Instance Creation")
      } else {
        # Log the actual name returned by the API so we can verify it's applied
        inst_created <- tryCatch(
          content(instance_response, as = "parsed", type = "application/json", encoding = "UTF-8"),
          error = function(e) NULL
        )
        returned_name <- if (!is.null(inst_created) && !is.null(inst_created$name)) inst_created$name else "<unknown>"
        cat("Repeating data instance successfully created for participant", participant_id, 
            "with requested name:", rep_name, "| API returned name:", returned_name, "\n")

        # If the API ignored the requested custom name, try to update the instance name via PATCH
        if (!identical(returned_name, rep_name)) {
          inst_id <- if (!is.null(inst_created) && !is.null(inst_created$id)) inst_created$id else NULL
          if (!is.null(inst_id)) {
            rename_url <- paste0(api_base_url, "/study/", study_id, "/repeating-data-instance/", inst_id)
            rename_body <- list(name = rep_name)
            rename_resp <- PATCH(
              rename_url,
              add_headers(
                Authorization = paste("Bearer", access_token),
                `Content-Type` = "application/json",
                Accept = "application/hal+json"
              ),
              body = toJSON(rename_body, auto_unbox = TRUE),
              encode = "json"
            )
            if (status_code(rename_resp) %in% c(200, 204)) {
              cat("Instance name updated via PATCH to:", rep_name, "\n")
              if (exists("epic2castor_status_update")) {
                try(epic2castor_status_update(step = "instances", detail = paste("Renamed to", rep_name)), silent = TRUE)
              }
            } else {
              cat("Failed to update instance name via PATCH. Response:", content(rename_resp, "text", encoding = "UTF-8"), "\n")
            }
          } else {
            cat("No instance id returned; cannot rename instance after creation.\n")
          }
        }
      }
    }
  } else {
    cat("For participant", participant_id, "there are already enough repeating data instances present.\n")
  }
}

# ======== STEP 7: Add data to the repeating data instance ===========
# Stream JSON: open bestand nu al en voeg elke datapoint direct toe
datastructure_path <- if (exists("epc_path")) epc_path("castor_datastructure_file") else {
  # Fallback indien epc_path nog niet bestaat
  if (!is.null(paths$castor_datastructure_file)) paths$castor_datastructure_file else file.path(paths$castor_meta_dir, "Datastructure.json")
}
datastructure_con <- file(datastructure_path, open = "w", encoding = "UTF-8")
# Start JSON array en flush direct naar disk zodat het bestand niet leeg blijft bij vroege fouten
writeLines("[", datastructure_con)
flush(datastructure_con)
first_item <- TRUE

# Helper: maak een index->instance_id mapping op basis van naam-suffix (.. - n) of volgorde
build_instance_index_map <- function(instances_payload) {
  id_by_index <- list()
  idx_fallback <- 1
  if (!is.null(instances_payload$`_embedded`) && !is.null(instances_payload$`_embedded`$repeatingDataInstance)) {
    for (inst in instances_payload$`_embedded`$repeatingDataInstance) {
      inst_id <- inst$id
      inst_name <- if (!is.null(inst$name)) inst$name else ""
      m <- regexec(".* - ([0-9]+)$", inst_name)
      reg <- regmatches(inst_name, m)[[1]]
      if (length(reg) >= 2) {
        idx <- suppressWarnings(as.integer(reg[2]))
        if (!is.na(idx)) {
          id_by_index[[as.character(idx)]] <- inst_id
        } else {
          id_by_index[[as.character(idx_fallback)]] <- inst_id
          idx_fallback <- idx_fallback + 1
        }
      } else {
        id_by_index[[as.character(idx_fallback)]] <- inst_id
        idx_fallback <- idx_fallback + 1
      }
    }
  }
  id_by_index
}

tryCatch({
for (participant_id in names(records_by_participant)) {
  rec_list <- records_by_participant[[participant_id]]
  if (exists("epic2castor_status_update")) {
    try(epic2castor_status_update(step = "datapoints", detail = paste("Participant", participant_id, "- fetch instances")), silent = TRUE)
  }
  
  instance_get_url <- paste0(api_base_url, "/study/", study_id, "/participant/", participant_id, "/repeating-data-instance")
  instances_response <- GET(instance_get_url, headers)
  if (status_code(instances_response) != 200) {
    cat("Error retrieving instances for participant", participant_id, "\n")
    next
  }
  instances <- content(instances_response, as = "parsed", type = "application/json", encoding = "UTF-8")
  id_by_index <- build_instance_index_map(instances)
  cat("Instance index mapping for participant", participant_id, ":",
      paste(sprintf("%s=%s", names(id_by_index), unlist(id_by_index)), collapse = ", "), "\n")
  
  # Helper: normaliseer en map bestaande datapoints naar field_id -> value
  normalize_for_compare <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (is.list(x)) x <- unlist(x, use.names = FALSE)
    x <- as.character(x)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
  # Normaliseer scheidingstekens binnen multiselects: vervang ; door ,
  x <- gsub(";", ",", x, fixed = TRUE)
  trimws(paste(x, collapse = ","))
  }

  # Datum normalisatie naar dd-mm-yyyy
  normalize_date <- function(s) {
    if (is.null(s) || is.na(s) || identical(s, "")) return(NA_character_)
    s <- gsub("[./]", "-", trimws(as.character(s)))
    # yyyy-mm-dd
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) {
      parts <- strsplit(s, "-")[[1]]
      return(sprintf("%02d-%02d-%04d", as.integer(parts[3]), as.integer(parts[2]), as.integer(parts[1])))
    }
    # d-m-Y of dd-mm-yyyy
    if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", s)) {
      parts <- strsplit(s, "-")[[1]]
      return(sprintf("%02d-%02d-%04d", as.integer(parts[1]), as.integer(parts[2]), as.integer(parts[3])))
    }
    # ongewijzigd teruggeven
    s
  }

  # Vergelijk bestaande en nieuwe waarde, met speciale handling voor datums, numeriek en multiselect
  values_equal <- function(field_var, old_val, new_val) {
    if (is.null(old_val) && is.null(new_val)) return(TRUE)
    if (length(old_val) == 0 && length(new_val) == 0) return(TRUE)
    if (is.null(old_val) || is.null(new_val)) return(FALSE)
  a <- trimws(as.character(old_val))
  b <- trimws(as.character(new_val))
  # Behandel placeholders als leeg
  if (tolower(a) %in% c("not set","n/a","na")) a <- ""
  if (tolower(b) %in% c("not set","n/a","na")) b <- ""

    # Datumvelden
    if (!is.null(field_var) && grepl("date|datum", field_var, ignore.case = TRUE)) {
      na <- normalize_date(a)
      nb <- normalize_date(b)
      return(!is.na(na) && !is.na(nb) && identical(na, nb))
    }

    # Multiselect: vergelijk als sets (komma- of puntkomma-gescheiden)
    if (grepl("[,;]", a) || grepl("[,;]", b)) {
      sa <- sort(tolower(trimws(unlist(strsplit(a, "[,;]")))))
      sa <- sa[sa != ""]
      sb <- sort(tolower(trimws(unlist(strsplit(b, "[,;]")))))
      sb <- sb[sb != ""]
      return(identical(sa, sb))
    }

    # Numeriek tolerant
    is_num <- function(x) grepl("^-?\\d+(,?\\d+)*(\\.\\d+)?$", gsub(" ", "", x))
    if (is_num(a) && is_num(b)) {
      an <- suppressWarnings(as.numeric(gsub(",", ".", a)))
      bn <- suppressWarnings(as.numeric(gsub(",", ".", b)))
      if (!is.na(an) && !is.na(bn)) return(abs(an - bn) < 1e-9)
    }

    # Case-insensitive string-vergelijking
    identical(tolower(a), tolower(b))
  }

  datapoints_to_value_map <- function(dp_payload) {
    # Zoek items in diverse mogelijke vormen
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
      # fallback: misschien zelf de lijst
      first <- dp_payload[[1]]
      if (is.list(first) && ("field_id" %in% names(first) || "field" %in% names(first))) {
        items <- dp_payload
      }
    }
    map <- list()
    if (is.null(items) || length(items) == 0) {
      cat("No datapoints found in payload to map existing values.\n")
      return(map)
    }
    for (dp in items) {
      # Vind field_id
      fid <- NULL
      if (!is.null(dp$field_id)) {
        fid <- dp$field_id
      } else if (!is.null(dp$field) && !is.null(dp$field$id)) {
        fid <- dp$field$id
      } else if (!is.null(dp$field) && !is.null(dp$field$field_id)) {
        fid <- dp$field$field_id
      } else if (!is.null(dp$field) && !is.null(dp$field$variable_name)) {
        # Map via variable_name naar field_id
        varnm <- as.character(dp$field$variable_name)
        if (!is.null(mapping_list[[varnm]])) {
          fid <- as.character(mapping_list[[varnm]])
        }
      }
      if (is.null(fid)) next
      # Vind value uit diverse velden
      # Voor repeating-data-instance endpoint: field_value is direct beschikbaar
      val <- NULL
      if (!is.null(dp$field_value)) val <- dp$field_value
      if (is.null(val) && !is.null(dp$value)) val <- dp$value
      if (is.null(val) && !is.null(dp$raw_value)) val <- dp$raw_value
      if (is.null(val) && !is.null(dp$option_value)) val <- dp$option_value
      # selected_options / options kunnen lijsten met objecten zijn; pak code/value of id/label
      extract_opts <- function(lst) {
        if (is.null(lst)) return(NULL)
        if (is.list(lst) && !is.null(lst[[1]]) && is.list(lst[[1]])) {
          return(unlist(lapply(lst, function(o) {
            if (!is.null(o$value)) return(o$value)
            if (!is.null(o$option_value)) return(o$option_value)
            if (!is.null(o$id)) return(o$id)
            if (!is.null(o$label)) return(o$label)
            return(NA_character_)
          }), use.names = FALSE))
        }
        lst
      }
      if (is.null(val) && !is.null(dp$options)) val <- extract_opts(dp$options)
      if (is.null(val) && !is.null(dp$selected_options)) val <- extract_opts(dp$selected_options)

      norm <- normalize_for_compare(val)
      fid_chr <- as.character(fid)
      if (!is.null(map[[fid_chr]]) && !is.na(map[[fid_chr]]) && !is.na(norm)) {
        # Combineer bij dubbele entries (bv. multi-select gespreid)
        combined <- paste0(map[[fid_chr]], ",", norm)
        map[[fid_chr]] <- normalize_for_compare(combined)
      } else {
        map[[fid_chr]] <- norm
      }
    }
    map
  }

  # Haal alle datapoints met paginering op (volg _links$next)
  fetch_all_datapoints <- function(url) {
    all_items <- list()
    next_url <- url
    page_count <- 0
    repeat {
      page_count <- page_count + 1
      if (exists("epic2castor_status_update")) {
        try(epic2castor_status_update(step = "datapoints", detail = paste("Fetching datapoints page", page_count)), silent = TRUE)
      }
      resp <- GET(next_url, add_headers(Authorization = paste("Bearer", access_token), Accept = "application/hal+json"))
      if (status_code(resp) != 200) {
        stop("Error retrieving datapoints: ", content(resp, as = "text", encoding = "UTF-8"))
      }
      cont <- content(resp, as = "parsed", type = "application/json", encoding = "UTF-8")
      items <- NULL
      if (!is.null(cont$`_embedded`) && !is.null(cont$`_embedded`$dataPoints)) {
        items <- cont$`_embedded`$dataPoints
      }
      if (!is.null(items) && length(items) > 0) {
        all_items <- c(all_items, items)
      }
      # Zoek next link
      next_link <- tryCatch({ cont$`_links`$`next`$href }, error = function(e) NULL)
      if (is.null(next_link) || identical(next_link, "")) break
      next_url <- next_link
      # Safety: voorkom oneindige lus
      if (page_count > 100) break
    }
    # Bouw payload-achtig object terug voor hergebruik
    list(`_embedded` = list(dataPoints = all_items))
  }

  for (i in seq_along(rec_list)) {
    rep_name <- paste("Baseline -", i)
    instance_id <- id_by_index[[as.character(i)]]
    if (is.null(instance_id)) {
      cat("No instance found for participant", participant_id, "for index", i, "(expected ", rep_name, ")\n")
      if (exists("epic2castor_status_update")) {
        try(epic2castor_status_update(step = "datapoints", detail = paste("Missing instance for", participant_id, rep_name)), silent = TRUE)
      }
      # Count all fields of this record as processed to keep progress realistic
      if (exists("dp_done") && exists("num_fields_per_record")) {
        dp_done <- dp_done + num_fields_per_record
        if (exists("epic2castor_status_update")) {
          try(epic2castor_status_update(step = "datapoints", current = dp_done), silent = TRUE)
        }
      }
      next
    }
    
    # Haal bestaande datapoints op voor deze instance via het repeating data instance endpoint
    datapoints_url <- paste0(
      api_base_url, "/study/", study_id, 
      "/participant/", participant_id,
      "/data-points/repeating-data-instance/", instance_id
    )
    
    # GET request naar het endpoint (unpaged volgens API docs)
    resp <- GET(datapoints_url, add_headers(
      Authorization = paste("Bearer", access_token), 
      Accept = "application/hal+json"
    ))
    
    datapoints <- list(`_embedded` = list(dataPoints = list()))
    if (status_code(resp) == 200) {
      cont <- content(resp, as = "parsed", type = "application/json", encoding = "UTF-8")
      # Response heeft _embedded$items (niet dataPoints)
      if (!is.null(cont$`_embedded`) && !is.null(cont$`_embedded`$items)) {
        # Converteer naar dataPoints formaat voor compatibiliteit met bestaande functies
        datapoints <- list(`_embedded` = list(dataPoints = cont$`_embedded`$items))
      }
    } else {
      cat("Warning: Could not fetch existing datapoints (status", status_code(resp), "). All fields will be uploaded.\n")
    }
    
    existing_map <- datapoints_to_value_map(datapoints)
    cat("Fetched", length(existing_map), "existing datapoint(s) for instance", instance_id, "(", rep_name, ")\n")
    if (length(existing_map) == 0) {
      cat("Note: no existing datapoints found; all fields will be posted for this instance.\n")
    }
    # Schrijf datapoint direct weg naar JSON (streaming array)
    item <- list(
      participant_id = participant_id,
      instance_id = instance_id,
      instance_name = rep_name,
      datapoints = datapoints
    )
  item_json <- toJSON(item, pretty = TRUE, auto_unbox = TRUE)
  if (!first_item) writeLines(",", datastructure_con)
  writeLines(item_json, datastructure_con)
  flush(datastructure_con)
  first_item <- FALSE
    
    record_data <- rec_list[[i]]$fields
    unwanted_fields <- c("Participant.Id", "Participant.Status", "Repeating.data.Name.Custom", 
                       "Site.Abbreviation", "Repeating.Data.Creation.Date")
    record_data <- record_data[!(names(record_data) %in% unwanted_fields)]
  
    # ======== BATCH UPLOAD: Verzamel alle velden voor deze instance ========
    fields_to_upload <- list()  # field_id -> field_value
    fields_metadata <- list()   # field_id -> clean_field_var (voor logging)
    
    for (field_id in names(record_data)) {
      if (field_id == "date_baseline") {
        clean_field_var <- "visit_dates"
      } else {
        clean_field_var <- sub("^[^_]*_", "", field_id)
      }

      # mapping_list is een named vector; gebruik enkele [] i.p.v. [[ ]] om NA bij missende sleutel te krijgen
      new_field_id <- unname(mapping_list[clean_field_var])
      if (length(new_field_id) == 0 || is.na(new_field_id)) {
        cat("No Field ID found in castor_meta.db for field variable name:", clean_field_var, "\n")
        dp_done <- dp_done + 1
        if (exists("epic2castor_status_update")) {
          try(epic2castor_status_update(step = "datapoints", current = dp_done,
                                        detail = paste("Unmapped", clean_field_var, "(", participant_id, rep_name, ")")), silent = TRUE)
        }
        next
      }
      new_field_id <- as.character(new_field_id)

      raw_value <- record_data[[field_id]]
      field_value <- format_castor_value(clean_field_var, raw_value)
      
      # Behandel placeholders als leeg
      if (!is.na(field_value) && tolower(trimws(field_value)) %in% c("not set", "n/a", "na")) {
        field_value <- NA_character_
      }
      
      if (is.na(field_value)) {
        cat("Skipping field", clean_field_var, "because value is NA/empty\n")
        dp_done <- dp_done + 1
        if (exists("epic2castor_status_update")) {
          try(epic2castor_status_update(step = "datapoints", current = dp_done,
                                        detail = paste("Empty", clean_field_var, "(", participant_id, rep_name, ")")), silent = TRUE)
        }
        next
      }

      # Voeg toe aan batch (filtering van ongewijzigde waarden gebeurt in batch_upload_datapoints)
      fields_to_upload[[new_field_id]] <- field_value
      fields_metadata[[new_field_id]] <- clean_field_var
      # Logging verplaatst naar batch_upload_helper voor alleen daadwerkelijk geüploade velden
    }
    
    # ======== BATCH UPLOAD UITVOEREN (1 API call) ========
    if (length(fields_to_upload) > 0) {
      cat("\n=== Batch uploading", length(fields_to_upload), "fields for", participant_id, rep_name, "===\n")
      
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
        existing_map = existing_map
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
      cat("No fields to upload for", participant_id, rep_name, "\n")
    }
    
    # Lege regel tussen deelnemers voor leesbaarheid
    cat("\n")
  }
}
}, finally = {
  # Sluit JSON-array en bestand netjes af, ook bij errors
  writeLines("\n]", datastructure_con)
  flush(datastructure_con)
  close(datastructure_con)
  cat("Datastructure saved to", datastructure_path, "\n")
  if (exists("epic2castor_status_done")) {
    # Report log file location instead of datastructure
    log_file <- getOption("epic2castor.logger.file", default = NA_character_)
    if (is.null(log_file) || is.na(log_file)) {
      # Fallback to run_dir/script name
      run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
      script_base <- "baselineExport"
      if (nzchar(run_dir)) log_file <- file.path(run_dir, paste0(script_base, "_log.txt")) else log_file <- paste0(script_base, "_log.txt")
    }
    try(epic2castor_status_done(detail = paste("Done. Log:", log_file)), silent = TRUE)
  }
})
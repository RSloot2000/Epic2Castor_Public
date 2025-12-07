# Zorg dat subprocess dezelfde projectpackages vindt (bv. jsonlite in ./Rlibs)
project_lib <- tryCatch(
  normalizePath(file.path(getwd(), "Rlibs"), winslash = "/", mustWork = FALSE),
  error = function(e) ""
)
if (nzchar(project_lib) && dir.exists(project_lib)) {
  .libPaths(c(project_lib, .libPaths()))
}

# Paden centraliseren via scripts/paths.json
paths <- jsonlite::fromJSON(file.path("config", "paths.json"))

# Auto logger (creates logs/<date_time> and <CastorAPIRetrieval>_log.txt)
logger_path <- if (!is.null(paths$logger_script)) paths$logger_script else file.path(paths$scripts_dir, "Logger.r")
config_script_path <- if (!is.null(paths$config_script)) paths$config_script else file.path(paths$scripts_dir, "config.R")
try(source(logger_path), silent = TRUE)
try(source(config_script_path), silent = TRUE)

# Laad packages zonder runtime-installaties (installeer ze vooraf in de omgeving)
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

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

main <- function(){
  # ======== CONFIGURATION ===========
  config_path <- epc_path("config_api")
  config <- fromJSON(config_path)

  base_url <- "https://data.castoredc.com"  # Base URL for the Castor API
  client_id <- config$client_id             # Retrieve from APIConfig.json
  client_secret <- config$client_secret     # Retrieve from APIConfig.json
  study_id <- config$study_id               # Retrieve from APIConfig.json

  # ======== STEP 1: Obtain an access token ===========
  token_url <- paste0(base_url, "/oauth/token")
  token_response <- httr::RETRY(
    "POST", token_url,
    body = list(
      client_id = client_id,
      client_secret = client_secret,
      grant_type = "client_credentials"
    ),
    encode = "form",
    httr::timeout(30),
    times = 3
  )
  
  if (status_code(token_response) != 200) {
    token_content <- content(token_response, as = "text", encoding = "UTF-8")
    error_message <- getErrorMessage(token_content)
    
    if (!is.null(error_message)) {
      if (grepl("Client with id", error_message, ignore.case = TRUE)) {
        stop(paste0(error_message, ". \nMake sure your credentials are exactly the same as in Castor!"), call. = FALSE)
      } else if (grepl("client_secret", error_message, ignore.case = TRUE) || grepl("client credentials", error_message, ignore.case = TRUE)) {
        stop(paste0("The client secret: ", client_secret, " is invalid. \nMake sure your credentials are exactly the same as in Castor!"), call. = FALSE)
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
  
  headers <- add_headers(Authorization = paste("Bearer", access_token))

  response_study <- httr::RETRY(
    "GET", paste0(base_url, "/api/study/", study_id),
    headers,
    httr::timeout(30),
    times = 3
  )
  if (status_code(response_study) != 200) {
    study_content <- content(response_study, as = "text", encoding = "UTF-8")
    error_message <- tolower(getErrorMessage(study_content))
    if (!is.null(error_message)) {
      stop(paste0("Error in Study ID: ", study_id, " ", error_message, "\nMake sure your credentials are exactly the same as in Castor!"))
    } else {
      stop("Error in obtaining study: ", status_code(response_study),
           ". Response: ", study_content)
    }
  }

  # ======== STEP 2: Retrieve the CSV files ===========
  url_optiongroups <- paste0(base_url, "/api/study/", study_id, "/export/optiongroups")
  url_structure <- paste0(base_url, "/api/study/", study_id, "/export/structure")
  
  output_field_options <- if (!is.null(paths$castor_field_options_file)) paths$castor_field_options_file else file.path(epc_path("castor_meta_dir"), "field_options.csv")
  output_study_variablelist <- if (!is.null(paths$castor_study_variablelist_file)) paths$castor_study_variablelist_file else file.path(epc_path("castor_meta_dir"), "study_variablelist.csv")
  
  # Zorg dat outputdirectories bestaan
  dir.create(dirname(output_field_options), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(output_study_variablelist), recursive = TRUE, showWarnings = FALSE)

  response_optiongroups <- httr::RETRY("GET", url_optiongroups, headers, httr::timeout(60), times = 3)
  if (status_code(response_optiongroups) != 200) {
    og_txt <- content(response_optiongroups, as = "text", encoding = "UTF-8")
    stop("Error retrieving optiongroups: ", status_code(response_optiongroups), " Response: ", og_txt)
  }
  csv_optiongroups <- content(response_optiongroups, as = "text", encoding = "UTF-8")
  writeLines(csv_optiongroups, output_field_options)
  cat("Field options CSV successfully saved as:", output_field_options, "\n")
  
  response_structure <- httr::RETRY("GET", url_structure, headers, httr::timeout(60), times = 3)
  if (status_code(response_structure) != 200) {
    st_txt <- content(response_structure, as = "text", encoding = "UTF-8")
    stop("Error retrieving structure: ", status_code(response_structure), " Response: ", st_txt)
  }
  csv_structure <- content(response_structure, as = "text", encoding = "UTF-8")
  writeLines(csv_structure, output_study_variablelist)
  cat("Study variablelist CSV successfully saved as:", output_study_variablelist, "\n\n")

  # ======== STEP 3: Signal completion to caller ==========
  done_env <- Sys.getenv("EPIC2CASTOR_DONE", unset = "")
  done_flag <- if (nzchar(done_env)) done_env else file.path(epc_path("castor_meta_dir"), ".castor_retrieval_done")
  try(writeLines(as.character(Sys.time()), done_flag), silent = TRUE)
  cat("EPIC2CASTOR_CASTOR_RETRIEVAL_DONE\n")
  flush.console()
}

initial_sink_count <- sink.number()
initial_message_sink_count <- sink.number(type = "message")

exit_code <- tryCatch({
  main()
  0L
}, error = function(e) {
  msg <- sub("^Error: ", "", conditionMessage(e))
  cat(msg, "\n")
  1L
}, finally = {
  # Probeer eventuele open sinks (logger) te sluiten, zodat processen niet blijven hangen
  try({ while (sink.number(type = "message") > initial_message_sink_count) sink(type = "message") }, silent = TRUE)
  try({ while (sink.number() > initial_sink_count) sink() }, silent = TRUE)
  flush.console()
})

# Alleen afsluiten wanneer dit bestand als zelfstandig script draait (niet wanneer het gesourced wordt)
if (sys.parent() == 0 && !interactive()) {
  q(status = exit_code, runLast = FALSE)
} else if (exit_code != 0L) {
  stop("Castor retrieval failed", call. = FALSE)
}

invisible(exit_code)
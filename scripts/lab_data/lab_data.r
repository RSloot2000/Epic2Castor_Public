## Lab data processing script (dependency-light, base R)
## - Reads lab_data CSV from input_data/lab_data/ (semicolon-separated, long format)
## - Pivots long-to-wide: one row per ParticipantId + CollectionInstant (date)
## - Maps Epic LabName -> Castor variable names
## - Filters out non-result rows (status messages like "Niet ontvangen")
## - Writes output to output_data/lab_data/lab_data.csv

tryCatch({
  ## ---- Load config & logger ------------------------------------------------
  source(file.path("scripts", "config.R"), local = TRUE)
  paths <- epc_paths()

  log_script <- tryCatch(epc_path("logger_script"), error = function(e) NULL)
  if (!is.null(log_script) && file.exists(log_script)) {
    try(source(log_script), silent = TRUE)
  }

  try(epic2castor_status_init(
    total = 5, title = "Lab Data", step = "init", detail = "Starting..."
  ), silent = TRUE)

  # ---- Paths ----------------------------------------------------------------
  in_dir  <- tryCatch(epc_path("lab_input_data_dir"),
                       error = function(e) "input_data/lab_data")
  out_dir <- tryCatch(epc_path("lab_output_data_dir"),
                       error = function(e) "output_data/lab_data")

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # Find input file (accept any .csv in the input directory)
  csv_files <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE,
                          ignore.case = TRUE)
  if (length(csv_files) == 0) {
    stop(paste0("ERROR:FILE_NOT_FOUND: No CSV files found in ", in_dir))
  }
  input_file <- csv_files[1]
  out_file   <- file.path(out_dir, "lab_data.csv")

  options(warn = 1)
  try(sink(stdout(), type = "message"), silent = TRUE)

  # ---- Helpers --------------------------------------------------------------
  read_semicolon_csv <- function(path) {
    if (!file.exists(path)) {
      stop(paste0("ERROR:FILE_NOT_FOUND:", path))
    }
    utils::read.csv2(
      file             = path,
      sep              = ";",
      dec              = ",",
      stringsAsFactors = FALSE,
      na.strings       = c("", "NA"),
      fileEncoding     = "UTF-8-BOM",
      check.names      = TRUE
    )
  }

  pick_col <- function(df, candidates, label) {
    for (nm in candidates) if (nm %in% names(df)) return(nm)
    stop(sprintf("Column not found (%s). Tried: %s",
                 label, paste(candidates, collapse = ", ")))
  }

  # ---- Mapping: Epic LabName -> Castor variable name ------------------------
  lab_name_mapping <- c(
    "Hemoglobine"                                          = "lab_haemoglobin",
    "Hematocriet"                                          = "lab_haematocrit",
    "Erytrocyten"                                          = "lab_erythrocytes",
    "Leukocyten"                                           = "lab_leucocytes",
    "Trombocyten"                                          = "lab_thrombocytes",
    "MCV"                                                  = "lab_mcv",
    "MCH"                                                  = "lab_mch",
    "MCHC"                                                 = "lab_mchc",
    "Red Cell Distribution Width (RDW)"                    = "lab_rdw",
    "Neutrofiel segm.granulocyten (absoluut)"              = "lab_neutrophils",
    "Lymfocyten (Absoluut)"                                = "lab_lymphocytes",
    "Monocyten (absoluut)"                                 = "lab_monocytes",
    "Eosinofiele granulocyten (absoluut)"                  = "lab_eosinophils",
    "Basofiele granulocyten (absoluut)"                    = "lab_basophils_abs",
    "Basofielen"                                           = "lab_basophils_perc",
    "Neutrofiele segmentkernige granulocyten md absoluut"  = "lab_neutrophils_md",
    "Lymfocyten md absoluut"                               = "lab_lymphocytes_md",
    "Monocyten md absoluut"                                = "lab_monocytes_md",
    "Eosinofiele granulocyten md absoluut"                 = "lab_eosinophils_md",
    "Myelocyten md absoluut"                               = "lab_myelocytes_md",
    "Myelocyten"                                           = "lab_myelocytes",
    "Afwijkende lymfocyten suspect reactief"               = "lab_atypical_lymphocytes",
    "Vacuolisatie"                                         = "lab_vacuolisation",
    "Abnormaal kernchromatine"                             = "lab_abnormal_chromatin",
    "Reusvormen"                                           = "lab_giant_forms",
    "Kreatinine"                                           = "lab_creatinine",
    "Ureum"                                                = "lab_urea",
    "MDRD-GFR"                                             = "lab_mdrd_gfr",
    "CKD-EPI-GFR"                                          = "lab_cdk_epi_gfr",
    "Kreatinine (dialysaat)"                               = "lab_creatinine_dialysate",
    "ASAT"                                                 = "lab_asat",
    "ALAT"                                                 = "lab_alat",
    "Gamma GT"                                             = "lab_gamma_gt",
    "LDH"                                                  = "lab_ldh",
    "Natrium"                                              = "lab_sodium",
    "Kalium"                                               = "lab_potassium",
    "Albumine"                                             = "lab_albumin",
    "Glucose"                                              = "lab_glucose",
    "HbA1c"                                                = "lab_hba1c",
    "Ferritine"                                            = "lab_ferritin",
    "Triglyceriden"                                        = "lab_triglycerides",
    "Fibrinogeen"                                          = "lab_fibrinogen",
    "CRP"                                                  = "lab_crp",
    "Bezinking"                                            = "lab_esr",
    "IgG Antistoffen Aspergillus type III"                 = "lab_igg_aspergillus"
  )

  # Apply overrides from elements.csv (tab_name_meta = "Lab Data")
  # Entries in elements.csv override the hardcoded defaults above
  lab_name_mapping <- epc_load_column_mapping("Lab Data", lab_name_mapping)
  message(sprintf("Lab mapping: %d entries (incl. elements.csv overrides)", length(lab_name_mapping)))

  # Status messages that should be filtered out (not actual lab results)
  # These are matched case-insensitively against the Value column
  status_patterns <- c(
    "niet ontvangen", "geannuleerd", "foutief", "niet honoreerbaar",
    "niet rapporteerbaar", "monster niet", "te weinig materiaal",
    "dubbele afname", "zie opmerking"
  )

  # ---- Step 1: Read input ---------------------------------------------------
  message("Reading: ", input_file)
  raw <- read_semicolon_csv(input_file)
  try(epic2castor_status_update(
    step = "read", detail = sprintf("Read %s (%d rows)", basename(input_file), NROW(raw)),
    current = 1, force = TRUE
  ), silent = TRUE)

  # ---- Step 2: Identify columns ---------------------------------------------
  pid_col        <- pick_col(raw, c("ParticipantId", "Participant.Id",
                                     "Participant ID", "Participant.ID",
                                     "participantId"), "ParticipantId")
  labname_col    <- pick_col(raw, c("LabName", "Lab.Name", "labname"), "LabName")
  value_col      <- pick_col(raw, c("Value", "value"), "Value")
  collection_col <- pick_col(raw, c("CollectionInstant", "Collection.Instant",
                                     "CollectionDate", "collection_instant"),
                             "CollectionInstant")

  message(sprintf("Columns found: pid=%s, lab=%s, value=%s, date=%s",
                  pid_col, labname_col, value_col, collection_col))

  # ---- Step 3: Filter & map -------------------------------------------------
  try(epic2castor_status_update(
    step = "filter", detail = "Filtering and mapping lab values...",
    current = 2, force = TRUE
  ), silent = TRUE)

  df <- raw[, c(pid_col, labname_col, value_col, collection_col), drop = FALSE]
  names(df) <- c("pid", "labname", "value", "collection_dt")
  n_before_filter <- NROW(df)

  # Remove rows with missing essential data
  df <- df[!is.na(df$pid) & !is.na(df$labname) & !is.na(df$value), ]

  # Remove status messages (non-result entries)
  is_status <- Reduce(`|`, lapply(status_patterns, function(pat) {
    grepl(pat, df$value, ignore.case = TRUE)
  }))
  df <- df[!is_status, ]
  n_after_filter <- NROW(df)
  message(sprintf("Filtered: %d -> %d rows (%d status messages removed)",
                  n_before_filter, n_after_filter, n_before_filter - n_after_filter))

  # Map LabName to Castor variable name
  df$castor_var <- lab_name_mapping[df$labname]
  unmapped <- unique(df$labname[is.na(df$castor_var)])
  if (length(unmapped) > 0) {
    warning(sprintf("Unmapped lab names (%d): %s",
                    length(unmapped), paste(unmapped, collapse = ", ")),
            call. = FALSE)
  }
  # Keep only mapped values
  df <- df[!is.na(df$castor_var), ]

  try(epic2castor_status_update(
    step = "filter", detail = sprintf("Mapped %d rows to %d Castor variables",
                                       NROW(df), length(unique(df$castor_var))),
    current = 3, force = TRUE
  ), silent = TRUE)

  # ---- Step 4: Extract date from collection_dt & pivot ----------------------
  try(epic2castor_status_update(
    step = "pivot", detail = "Pivoting long to wide...",
    current = 4, force = TRUE
  ), silent = TRUE)

  # Extract date portion (YYYY-MM-DD) from datetime string
  df$collection_date <- sub(" .*$", "", trimws(df$collection_dt))

  # Convert to Castor date format (DD-MM-YYYY)
  normalize_date_iso <- function(x) {
    s <- trimws(as.character(x))
    s[s == "" | is.na(s)] <- NA_character_
    conv <- function(v) {
      if (is.na(v)) return(NA_character_)
      # Expect YYYY-MM-DD
      if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", v)) {
        parts <- strsplit(v, "-")[[1]]
        return(sprintf("%02d-%02d-%04d",
                       as.integer(parts[3]),
                       as.integer(parts[2]),
                       as.integer(parts[1])))
      }
      # Already DD-MM-YYYY
      if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", v)) return(v)
      v
    }
    vapply(s, conv, character(1), USE.NAMES = FALSE)
  }

  df$collection_date_castor <- normalize_date_iso(df$collection_date)

  # Handle duplicate lab values on the same date for the same patient:
  # If the same lab test appears multiple times on the same date,
  # keep the last one (by row order, which reflects ResultInstant)
  df <- df[!duplicated(df[, c("pid", "collection_date", "castor_var")],
                        fromLast = TRUE), ]

  # Pivot: one row per participant + collection date, one column per castor var
  # Use reshape() from base R
  wide <- reshape(
    df[, c("pid", "collection_date_castor", "castor_var", "value")],
    idvar     = c("pid", "collection_date_castor"),
    timevar   = "castor_var",
    v.names   = "value",
    direction = "wide"
  )

  # Clean up column names: "value.lab_xxx" -> "lab_xxx"
  names(wide) <- sub("^value\\.", "", names(wide))

  # Rename pid -> Participant Id, collection_date_castor -> collection_date
  names(wide)[names(wide) == "pid"]                     <- "Participant Id"
  names(wide)[names(wide) == "collection_date_castor"]  <- "collection_date"

  # Ensure all Castor lab columns exist (add empty ones if missing)
  all_castor_vars <- unique(lab_name_mapping)
  for (cv in all_castor_vars) {
    if (!cv %in% names(wide)) wide[[cv]] <- NA_character_
  }

  # Reorder: Participant Id, collection_date, then lab columns alphabetically
  lab_cols <- sort(intersect(names(wide), all_castor_vars))
  wide <- wide[, c("Participant Id", "collection_date", lab_cols), drop = FALSE]

  # Sort by participant (numeric part) then date
  num_part <- suppressWarnings(
    as.integer(gsub("\\D", "", wide[["Participant Id"]]))
  )
  # Parse DD-MM-YYYY for proper date sorting
  sort_date <- as.Date(wide[["collection_date"]], format = "%d-%m-%Y")
  ord <- order(num_part, sort_date, na.last = TRUE)
  wide <- wide[ord, , drop = FALSE]

  # Replace NA with empty string for clean CSV output
  wide[is.na(wide)] <- ""

  # ---- Step 5: Write output -------------------------------------------------
  message(sprintf("Writing %d rows (%d participants, %d unique dates) to %s",
                  NROW(wide),
                  length(unique(wide[["Participant Id"]])),
                  NROW(wide),
                  out_file))

  write.table(
    wide,
    out_file,
    sep          = ";",
    dec          = ",",
    row.names    = FALSE,
    col.names    = TRUE,
    na           = "",
    quote        = FALSE,
    fileEncoding = "UTF-8"
  )

  try(epic2castor_status_update(
    step = "done", detail = sprintf("Done: %d rows written", NROW(wide)),
    current = 5, force = TRUE
  ), silent = TRUE)
  try(epic2castor_status_done(), silent = TRUE)

  cat("EPIC2CASTOR::DONE\n")
  message("Lab data processing complete.")

}, error = function(e) {
  msg <- conditionMessage(e)
  message("ERROR in lab_data processing: ", msg)
  try(epic2castor_status_update(
    step = "error", detail = paste("Error:", msg),
    current = NA, force = TRUE
  ), silent = TRUE)
  try(epic2castor_status_done(), silent = TRUE)
})

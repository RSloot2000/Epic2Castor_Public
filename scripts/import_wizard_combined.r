# ============================================================================
# IMPORT WIZARD - COMBINED MODULE
# ============================================================================
#
# Purpose:
#   All-in-one import wizard functionality combining:
#   - Import Configuration (type definitions, column requirements)
#   - File Detection (auto-detect file type, structure, encoding)
#   - Column Mapping (interactive UI for mapping columns)
#   - Data Transformation (apply mappings, conversions, validation)
#   - Template Management (save/load mapping templates)
#   - Data Export (export to CSV/Excel)
#
# Usage:
#   source("scripts/import_wizard_combined.r")
#   
#   # Detection
#   result <- detect_file_structure("path/to/file.csv")
#   
#   # Configuration
#   config <- get_import_type_config("epic_baseline")
#   
#   # Transformation
#   transformed <- transform_import_data(file_path, detection_result, mapping_df)
#   
#   # Export
#   export_to_csv(data, "epic_baseline")
#
# ============================================================================

if (!exists("IMPORT_WIZARD_COMBINED_LOADED")) {
  cat("✓ Loading import_wizard_combined.r - Complete Import Wizard Module\n")
  IMPORT_WIZARD_COMBINED_LOADED <- TRUE
}

# ============================================================================
# REQUIRED PACKAGES
# ============================================================================

required_packages <- c("data.table", "readxl", "readr", "DT", "jsonlite")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required for import_wizard_combined.r", pkg))
  }
}

# ============================================================================
# SECTION 1: IMPORT CONFIGURATION
# ============================================================================
# Central configuration for file import types, column requirements, and
# validation rules. Used by the import wizard to detect file types and
# guide users through the import process.
# ============================================================================

#' Get configuration for specific import type
#' 
#' @param type_name Character - Import type identifier
#' @return List with import type configuration
#' @export
get_import_type_config <- function(type_name) {
  all_configs <- get_all_import_types()
  
  if (!type_name %in% names(all_configs)) {
    stop(sprintf("Unknown import type: %s. Available types: %s", 
                 type_name, 
                 paste(names(all_configs), collapse = ", ")))
  }
  
  return(all_configs[[type_name]])
}

#' Get all available import type configurations
#' 
#' @return Named list of all import type configs
#' @export
get_all_import_types <- function() {
  list(
    
    # ========================================================================
    # EPIC EXPORT BASELINE - ELEMENT-VALUE FORMAT
    # ========================================================================
    epic_baseline = list(
      id = "epic_baseline",
      name = "EPIC Export (Baseline)",
      description = "Patient baseline data in Element-Value format (vertical structure)",
      icon = "list-alt",
      structure = "element_value_pairs",
      category = "clinical_data",
      
      # File pattern hints for auto-detection
      file_patterns = c("epic.*export", "baseline", "epic.*baseline"),
      
      # Required columns with aliases and validation
      required_columns = list(
        Element = list(
          required = TRUE,
          aliases = c("Element", "element", "ELEMENT"),
          description = "EPIC element code (e.g., PBAIG#043)",
          example = "PBAIG#043",
          pattern = "^[A-Z]+#\\d+",
          validation = function(x) {
            # Check if at least some values match EPIC pattern
            x_clean <- x[!is.na(x)]
            if (length(x_clean) == 0) return(FALSE)
            any(grepl("^[A-Z]+#\\d+", x_clean))
          }
        ),
        
        waarde = list(
          required = TRUE,
          aliases = c("waarde", "value", "EPIC_waarde", "Value", "WAARDE"),
          description = "Value for the element",
          example = "Yes",
          validation = function(x) {
            # Any value accepted, but not all NA
            !all(is.na(x))
          }
        ),
        
        studiealias = list(
          required = TRUE,
          aliases = c("studiealias", "Participant Id", "ParticipantId", 
                     "Patient_ID", "patient_id", "PatientID", "STUDIEALIAS"),
          description = "Patient identifier",
          example = "0001",
          validation = function(x) {
            # Not all values can be NA or empty
            char_x <- trimws(as.character(x))
            sum(!is.na(char_x) & nzchar(char_x)) > 0
          }
        ),
        
        datum = list(
          required = TRUE,
          aliases = c("datum", "date", "Date", "visit_date", "Visit_Date", 
                     "measurement_date", "DATUM"),
          description = "Date of measurement",
          example = "2023-01-15",
          validation = function(x) {
            # Check if can be converted to date
            !all(is.na(x))
          }
        ),
        
        pseudo_id = list(
          required = FALSE,
          aliases = c("pseudo_id", "PseudoID", "Pseudo ID", "pseudo id", 
                     "PSEUDO_ID", "pseudoID", "Pseudo_ID"),
          description = "Pseudo identifier to be replaced by studiealias",
          example = "ABC123",
          validation = function(x) {
            # Optional - can be all NA
            TRUE
          }
        )
      ),
      
      # No optional columns - all others are ignored
      optional_columns = list(),
      
      # Configuration flags
      ignore_extra_columns = TRUE,  # Extra columns are silently ignored
      flexible_detection = FALSE,   # Use exact/alias matching only
      
      # Column transformation map (source -> target)
      transformation_map = list(
        studiealias = "Participant Id",
        Element = "Element",
        waarde = "waarde",
        datum = "date_baseline",
        pseudo_id = "pseudo_id"
      ),
      
      # Processing notes
      processing_notes = c(
        "Data will be used in Element-Value format",
        "Extra columns in the file will be ignored",
        "Date format will be automatically detected and converted"
      )
    ),
    
    # ========================================================================
    # EPIC EXPORT FOLLOW-UP - ELEMENT-VALUE FORMAT
    # ========================================================================
    follow_up = list(
      id = "follow_up",
      name = "EPIC Export (Follow-up)",
      description = "Patient follow-up data in Element-Value format (vertical structure)",
      icon = "calendar-check",
      structure = "element_value_pairs",
      category = "clinical_data",
      
      # File pattern hints for auto-detection
      file_patterns = c("follow.*up", "followup", "fu", "epic.*follow"),
      
      # Required columns with aliases and validation (same as baseline)
      required_columns = list(
        Element = list(
          required = TRUE,
          aliases = c("Element", "element", "ELEMENT"),
          description = "EPIC element code (e.g., PBAIG#043)",
          example = "PBAIG#043",
          pattern = "^[A-Z]+#\\d+",
          validation = function(x) {
            x_clean <- x[!is.na(x)]
            if (length(x_clean) == 0) return(FALSE)
            any(grepl("^[A-Z]+#\\d+", x_clean))
          }
        ),
        
        waarde = list(
          required = TRUE,
          aliases = c("waarde", "value", "EPIC_waarde", "Value", "WAARDE"),
          description = "Value for the element",
          example = "Yes",
          validation = function(x) {
            !all(is.na(x))
          }
        ),
        
        studiealias = list(
          required = TRUE,
          aliases = c("studiealias", "Participant Id", "ParticipantId", 
                     "Patient_ID", "patient_id", "PatientID", "STUDIEALIAS"),
          description = "Patient identifier",
          example = "0001",
          validation = function(x) {
            char_x <- trimws(as.character(x))
            sum(!is.na(char_x) & nzchar(char_x)) > 0
          }
        ),
        
        datum = list(
          required = TRUE,
          aliases = c("datum", "date", "Date", "visit_date", "Visit_Date", 
                     "measurement_date", "DATUM"),
          description = "Date of measurement",
          example = "2023-01-15",
          validation = function(x) {
            !all(is.na(x))
          }
        ),
        
        pseudo_id = list(
          required = FALSE,
          aliases = c("pseudo_id", "PseudoID", "Pseudo ID", "pseudo id", 
                     "PSEUDO_ID", "pseudoID", "Pseudo_ID"),
          description = "Pseudo identifier to be replaced by studiealias",
          example = "ABC123",
          validation = function(x) {
            # Optional - can be all NA
            TRUE
          }
        )
      ),
      
      # No optional columns
      optional_columns = list(),
      
      # Configuration flags
      ignore_extra_columns = TRUE,
      flexible_detection = FALSE,
      
      # Column transformation map
      transformation_map = list(
        studiealias = "Participant Id",
        Element = "Element",
        waarde = "waarde",
        datum = "date_follow_up",
        pseudo_id = "pseudo_id"
      ),
      
      # Processing notes
      processing_notes = c(
        "Follow-up data will be used in Element-Value format",
        "Extra columns in the file will be ignored",
        "Date format will be automatically detected and converted"
      )
    ),
    
    # ========================================================================
    # BIOBANK DATA - WIDE FORMAT
    # ========================================================================
    biobank_data = list(
      id = "biobank_data",
      name = "Biobank Sample Data",
      description = "Laboratory sample information (wide format with fixed columns)",
      icon = "vial",
      structure = "wide_format",
      category = "biobank",
      
      # File pattern hints
      file_patterns = c("biobank", "sample", "laboratory", "lab.*data"),
      
      # Required columns
      required_columns = list(
        Identificatie = list(
          required = TRUE,
          aliases = c("Identificatie", "identificatie", "ID", "Patient_ID", 
                     "PatientID", "patient_id", "IDENTIFICATIE"),
          description = "Patient identification number",
          example = "1768227",
          validation = function(x) {
            # Not all values can be NA or empty
            char_x <- trimws(as.character(x))
            sum(!is.na(char_x) & nzchar(char_x)) > 0
          }
        )
      ),
      
      # Optional columns with flexible detection
      optional_columns = list(
        `iLES sampleId` = list(
          aliases = c("iLES sampleId", "iLES_sampleId", "iles_sample_id", 
                     "iLES.sampleId", "ILES_SAMPLEID"),
          description = "iLES sample identifier",
          example = "8",
          detect_pattern = "iles.*sample"
        ),
        
        `Tube sampleId` = list(
          aliases = c("Tube sampleId", "Tube_sampleId", "tube_sample_id",
                     "Tube.sampleId", "TUBE_SAMPLEID"),
          description = "Physical tube identifier",
          example = "1976305",
          detect_pattern = "tube.*sample"
        ),
        
        Monstersoort = list(
          aliases = c("Monstersoort", "Sample_Type", "sample_type", 
                     "SampleType", "MONSTERSOORT"),
          description = "Sample type (e.g., Plasma EDTA, PBMC)",
          example = "Plasma EDTA",
          detect_pattern = "monster|sample.*type"
        ),
        
        Status = list(
          aliases = c("Status", "status", "STATUS"),
          description = "Sample processing status",
          example = "Complete"
        ),
        
        SubCollectie = list(
          aliases = c("SubCollectie", "Sub_Collectie", "subcollection", 
                     "Sub.Collectie", "SUBCOLLECTIE"),
          description = "Sub-collection name",
          example = "MYCOS",
          detect_pattern = "sub.*collect"
        ),
        
        `Afname datumtijd` = list(
          aliases = c("Afname datumtijd", "Afname.datumtijd", "collection_datetime",
                     "CollectionDateTime", "AFNAME_DATUMTIJD"),
          description = "Sample collection date/time",
          example = "2-1-2023 09:00",
          detect_pattern = "afname.*(datum|date).*(tijd|time)"
        ),
        
        Afgerond = list(
          aliases = c("Afgerond", "afgerond", "completed", "finished",
                     "Finished", "Completed", "AFGEROND"),
          description = "Completion date",
          example = "14-7-2023",
          detect_pattern = "afgerond|finished|completion|completed"
        ),
        
        Protocol = list(
          aliases = c("Protocol", "protocol", "PROTOCOL"),
          description = "Collection protocol",
          example = "Plasma EDTA 10ml"
        )
      ),
      
      # Configuration flags
      ignore_extra_columns = FALSE,  # Keep all columns
      flexible_detection = TRUE,     # Use pattern matching for optional columns
      
      # No transformation - columns keep their names
      transformation_map = NULL,
      
      processing_notes = c(
        "All columns will be included in the output",
        "Column names will be preserved as-is",
        "Optional columns will be auto-detected using flexible matching"
      )
    ),
    
    # ========================================================================
    # MDNS MAPPING FILE - ID MAPPING
    # ========================================================================
    mdns_mapping = list(
      id = "mdns_mapping",
      name = "MDNS Mapping File",
      description = "Maps MDN identifiers to Participant IDs (required for biobank processing)",
      icon = "exchange-alt",
      structure = "mapping_file",
      category = "biobank",
      
      # File pattern hints
      file_patterns = c("mdns", "mdn.*map", "id.*map"),
      
      # Required columns
      required_columns = list(
        MDN = list(
          required = TRUE,
          aliases = c("MDN", "mdn"),
          description = "MDN identifier",
          example = "12345",
          validation = function(x) {
            char_x <- trimws(as.character(x))
            sum(!is.na(char_x) & nzchar(char_x)) > 0
          }
        ),
        
        `Participant ID` = list(
          required = TRUE,
          aliases = c("Participant ID", "Participant.ID", "participant_id",
                     "ParticipantId", "participantID", "PatientID", "Patient_ID"),
          description = "Patient identifier",
          example = "0001",
          validation = function(x) {
            char_x <- trimws(as.character(x))
            sum(!is.na(char_x) & nzchar(char_x)) > 0
          }
        )
      ),
      
      optional_columns = list(),
      
      # Configuration flags
      ignore_extra_columns = TRUE,
      flexible_detection = FALSE,
      
      transformation_map = NULL,
      
      processing_notes = c(
        "Used to map MDN to Participant ID in biobank data",
        "Both columns must contain unique, non-empty values",
        "Extra columns will be ignored"
      )
    ),
    
    # ========================================================================
    # MAPPING FILE (ELEMENTS.CSV) - FOR FUTURE USE
    # ========================================================================
    mapping_elements = list(
      id = "mapping_elements",
      name = "Mapping File (elements.csv)",
      description = "EPIC element to Castor field mappings",
      icon = "map",
      structure = "mapping_file",
      category = "configuration",
      enabled = FALSE,  # Not yet implemented
      
      required_columns = list(
        Element = list(
          required = TRUE,
          aliases = c("Element", "element"),
          description = "EPIC element code",
          pattern = "^[A-Z]+#\\d+"
        ),
        
        castor_kolom = list(
          required = TRUE,
          aliases = c("castor_kolom", "castor_column", "CastorColumn"),
          description = "Castor field name"
        )
      ),
      
      optional_columns = list(),
      
      processing_notes = c(
        "NOT YET AVAILABLE IN IMPORT WIZARD",
        "Tab metadata (tab_name_meta, tab_order_meta) is managed automatically by the app",
        "Use the app interface to manage mappings"
      )
    )
  )
}

#' Get list of enabled import types for UI display
#' 
#' @return Named character vector: type_id -> display_name
#' @export
get_enabled_import_types <- function() {
  all_types <- get_all_import_types()
  
  # Filter enabled types
  enabled <- Filter(function(x) {
    is.null(x$enabled) || isTRUE(x$enabled)
  }, all_types)
  
  # Return as named vector for selectInput
  choices <- sapply(enabled, function(x) x$name)
  names(choices) <- sapply(enabled, function(x) x$id)
  
  return(choices)
}

#' Get user-friendly description for import type
#' 
#' @param type_id Character - Import type ID
#' @param include_notes Logical - Include processing notes
#' @return Character vector or list with description
#' @export
get_import_type_description <- function(type_id, include_notes = FALSE) {
  config <- get_import_type_config(type_id)
  
  if (!include_notes) {
    return(config$description)
  }
  
  list(
    description = config$description,
    structure = config$structure,
    required_count = length(config$required_columns),
    optional_count = length(config$optional_columns),
    notes = config$processing_notes
  )
}

#' Check if import type requires specific file structure
#' 
#' @param type_id Character - Import type ID
#' @return Character - structure type
#' @export
get_import_structure <- function(type_id) {
  config <- get_import_type_config(type_id)
  return(config$structure)
}

cat("  ✓ Section 1: Import Configuration loaded\n")

# ============================================================================
# SECTION 2: FILE DETECTION
# ============================================================================
# Automatically detect file type, structure, and appropriate import configuration
# Supports CSV, Excel (single/multi-sheet), and various data structures
# ============================================================================

#' Detect file type and basic properties
#' 
#' @param file_path Character - Path to file
#' @return List with file info
#' @export
detect_file_type <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(sprintf("File not found: %s", file_path))
  }
  
  ext <- tolower(tools::file_ext(file_path))
  file_size <- file.info(file_path)$size
  
  if (ext == "csv") {
    return(list(
      type = "csv",
      extension = ext,
      size = file_size,
      delimiter = detect_csv_delimiter(file_path),
      encoding = detect_encoding(file_path),
      sheets = NULL,
      multi_sheet = FALSE
    ))
  }
  
  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(file_path)
    
    return(list(
      type = "excel",
      extension = ext,
      size = file_size,
      sheets = sheets,
      n_sheets = length(sheets),
      multi_sheet = length(sheets) > 1,
      sheet_info = lapply(sheets, function(sheet) {
        preview <- tryCatch(
          readxl::read_excel(file_path, sheet = sheet, n_max = 5),
          error = function(e) NULL
        )
        
        if (is.null(preview)) {
          return(list(name = sheet, error = TRUE))
        }
        
        list(
          name = sheet,
          n_cols = ncol(preview),
          n_rows = nrow(readxl::read_excel(file_path, sheet = sheet, n_max = 0)) + 
                   nrow(readxl::read_excel(file_path, sheet = sheet)),
          col_names = colnames(preview),
          error = FALSE
        )
      })
    ))
  }
  
  stop(sprintf("Unsupported file type: %s (supported: csv, xlsx, xls)", ext))
}

#' Detect CSV delimiter
#' 
#' @param file_path Character - Path to CSV file
#' @return Character - Detected delimiter (";" or ",")
detect_csv_delimiter <- function(file_path) {
  lines <- readLines(file_path, n = 3, warn = FALSE)
  
  if (length(lines) == 0) return(";")
  
  semicolon_count <- sum(gregexpr(";", lines, fixed = TRUE)[[1]] > 0)
  comma_count <- sum(gregexpr(",", lines, fixed = TRUE)[[1]] > 0)
  
  if (semicolon_count > comma_count) return(";")
  return(",")
}

#' Detect file encoding
#' 
#' @param file_path Character - Path to file
#' @return Character - Detected encoding
detect_encoding <- function(file_path) {
  raw_bytes <- readBin(file_path, "raw", n = 3)
  
  if (length(raw_bytes) >= 3 && 
      raw_bytes[1] == as.raw(0xEF) && 
      raw_bytes[2] == as.raw(0xBB) && 
      raw_bytes[3] == as.raw(0xBF)) {
    return("UTF-8")
  }
  
  return("UTF-8")
}

#' Detect data structure pattern
#' 
#' @param data data.frame - Preview data (first 50-100 rows)
#' @param file_info List - File info from detect_file_type
#' @param import_configs List - Import type configs to check against
#' @return List with structure analysis
#' @export
detect_structure_pattern <- function(data, file_info = NULL, import_configs = NULL) {
  if (is.null(import_configs)) {
    import_configs <- get_all_import_types()
  }
  
  col_names <- colnames(data)
  n_cols <- ncol(data)
  n_rows <- nrow(data)
  
  scores <- list()
  
  for (type_id in names(import_configs)) {
    config <- import_configs[[type_id]]
    
    if (!is.null(config$enabled) && !config$enabled) next
    
    score <- calculate_match_score(data, col_names, config)
    scores[[type_id]] <- score
  }
  
  if (length(scores) == 0) {
    return(list(
      type = "unknown",
      subtype = "unclassified",
      confidence = 0,
      matched_type = NULL,
      scores = scores,
      col_names = col_names,
      n_cols = n_cols,
      n_rows = n_rows
    ))
  }
  
  best_type <- names(which.max(sapply(scores, function(x) x$total_score)))
  best_score <- scores[[best_type]]
  
  return(list(
    type = import_configs[[best_type]]$structure,
    subtype = best_type,
    confidence = best_score$total_score,
    matched_type = best_type,
    scores = scores,
    col_names = col_names,
    n_cols = n_cols,
    n_rows = n_rows,
    indicators = best_score$indicators
  ))
}

#' Calculate match score for import type
#' 
#' @param data data.frame - Data to analyze
#' @param col_names Character vector - Column names
#' @param config List - Import type config
#' @return List with score breakdown
calculate_match_score <- function(data, col_names, config) {
  score <- 0
  max_score <- 100
  indicators <- list()
  
  required_cols <- config$required_columns
  optional_cols <- config$optional_columns
  
  required_matches <- 0
  required_total <- length(required_cols)
  
  for (col_name in names(required_cols)) {
    col_config <- required_cols[[col_name]]
    match_result <- find_column_match(col_names, col_config, data)
    
    if (!is.null(match_result$matched_col)) {
      required_matches <- required_matches + 1
      indicators[[paste0("has_", col_name)]] <- TRUE
      
      if (!is.null(col_config$validation)) {
        if (col_config$validation(data[[match_result$matched_col]])) {
          score <- score + 5
        }
      }
    } else {
      indicators[[paste0("has_", col_name)]] <- FALSE
    }
  }
  
  if (required_total > 0) {
    score <- score + (required_matches / required_total) * 60
  }
  
  if (!is.null(optional_cols) && length(optional_cols) > 0) {
    optional_matches <- 0
    optional_total <- length(optional_cols)
    
    for (col_name in names(optional_cols)) {
      col_config <- optional_cols[[col_name]]
      match_result <- find_column_match(col_names, col_config, data)
      
      if (!is.null(match_result$matched_col)) {
        optional_matches <- optional_matches + 1
      }
    }
    
    score <- score + (optional_matches / optional_total) * 20
  }
  
  if (!is.null(config$pattern)) {
    score <- score + 10
  } else {
    for (col_name in names(required_cols)) {
      col_config <- required_cols[[col_name]]
      if (!is.null(col_config$pattern)) {
        match_result <- find_column_match(col_names, col_config, data)
        if (!is.null(match_result$matched_col)) {
          col_data <- data[[match_result$matched_col]]
          valid_data <- col_data[!is.na(col_data)]
          if (length(valid_data) > 0 && any(grepl(col_config$pattern, valid_data))) {
            score <- score + 5
            break
          }
        }
      }
    }
  }
  
  if (nrow(data) > 0) score <- score + 5
  if (ncol(data) >= required_total) score <- score + 5
  
  return(list(
    total_score = min(score, max_score),
    required_matches = required_matches,
    required_total = required_total,
    indicators = indicators
  ))
}

#' Find matching column for config
#' 
#' @param col_names Character vector - Available column names
#' @param col_config List - Column configuration
#' @param data data.frame - Data for validation
#' @return List with matched_col and match_type
find_column_match <- function(col_names, col_config, data = NULL) {
  for (alias in col_config$aliases) {
    exact <- col_names[tolower(col_names) == tolower(alias)]
    if (length(exact) > 0) {
      return(list(matched_col = exact[1], match_type = "exact"))
    }
  }
  
  if (!is.null(col_config$detect_pattern)) {
    pattern_matches <- col_names[grepl(col_config$detect_pattern, col_names, ignore.case = TRUE)]
    if (length(pattern_matches) > 0) {
      return(list(matched_col = pattern_matches[1], match_type = "pattern"))
    }
  }
  
  if (isTRUE(col_config$required) && length(col_names) > 0) {
    if (!requireNamespace("stringdist", quietly = TRUE)) {
      return(list(matched_col = NULL, match_type = "none"))
    }
    primary_name <- col_config$aliases[1]
    distances <- stringdist::stringdist(tolower(primary_name), tolower(col_names), method = "lv")
    
    if (length(distances) > 0 && !all(is.na(distances))) {
      min_dist <- min(distances, na.rm = TRUE)
      threshold <- max(3, nchar(primary_name) * 0.3)
      
      if (min_dist <= threshold) {
        best_match <- col_names[which.min(distances)]
        return(list(matched_col = best_match, match_type = "fuzzy"))
      }
    }
  }
  
  return(list(matched_col = NULL, match_type = "none"))
}

#' Main detection function - analyzes file and returns detection result
#' 
#' @param file_path Character - Path to file
#' @param sheet Character - Excel sheet name (optional, single sheet)
#' @param sheets Character vector - Multiple Excel sheet names (optional)
#' @param max_preview_rows Integer - Max rows to read for detection (default: 100)
#' @param import_configs List - Pre-loaded import configs (optional)
#' @return List with complete detection result
#' @export
detect_file_structure <- function(file_path, sheet = NULL, sheets = NULL, max_preview_rows = 100, import_configs = NULL) {
  file_info <- detect_file_type(file_path)
  
  if (file_info$type == "csv") {
    delimiter <- file_info$delimiter
    preview_data <- tryCatch({
      if (delimiter == ";") {
        readr::read_csv2(file_path, n_max = max_preview_rows, 
                        col_types = cols(), show_col_types = FALSE,
                        locale = locale(decimal_mark = ",", grouping_mark = "."))
      } else {
        readr::read_csv(file_path, n_max = max_preview_rows,
                       col_types = cols(), show_col_types = FALSE,
                       locale = locale(decimal_mark = ".", grouping_mark = ","))
      }
    }, error = function(e) {
      stop(sprintf("Failed to read CSV: %s", conditionMessage(e)))
    })
  } else if (file_info$type == "excel") {
    selected_sheets <- NULL
    
    if (!is.null(sheets) && length(sheets) > 0) {
      selected_sheets <- sheets
    } else if (!is.null(sheet)) {
      selected_sheets <- c(sheet)
    } else {
      selected_sheets <- c(file_info$sheets[1])
    }
    
    if (length(selected_sheets) == 1) {
      preview_data <- tryCatch({
        readxl::read_excel(file_path, sheet = selected_sheets[1], n_max = max_preview_rows)
      }, error = function(e) {
        stop(sprintf("Failed to read Excel sheet '%s': %s", selected_sheets[1], conditionMessage(e)))
      })
    } else {
      sheet_data_list <- list()
      
      for (sheet_name in selected_sheets) {
        sheet_data <- tryCatch({
          readxl::read_excel(file_path, sheet = sheet_name, n_max = max_preview_rows)
        }, error = function(e) {
          warning(sprintf("Failed to read sheet '%s': %s", sheet_name, conditionMessage(e)))
          NULL
        })
        
        if (!is.null(sheet_data) && ncol(sheet_data) > 0) {
          sheet_data_list[[sheet_name]] <- sheet_data
        }
      }
      
      if (length(sheet_data_list) == 0) {
        stop("Failed to read any of the selected sheets")
      }
      
      preview_data <- tryCatch({
        max_rows <- max(sapply(sheet_data_list, nrow))
        
        padded_sheets <- lapply(names(sheet_data_list), function(sheet_name) {
          df <- sheet_data_list[[sheet_name]]
          if (nrow(df) < max_rows) {
            empty_rows <- matrix(NA, nrow = max_rows - nrow(df), ncol = ncol(df))
            colnames(empty_rows) <- colnames(df)
            empty_df <- as.data.frame(empty_rows, stringsAsFactors = FALSE)
            df <- rbind(df, empty_df)
          }
          
          original_names <- colnames(df)
          colnames(df) <- paste0(original_names, "_", sheet_name)
          
          return(df)
        })
        
        do.call(cbind, padded_sheets)
      }, error = function(e) {
        stop(sprintf("Failed to merge sheets: %s", conditionMessage(e)))
      })
    }
  }
  
  structure_info <- detect_structure_pattern(preview_data, file_info, import_configs)
  
  confidence_level <- classify_confidence(structure_info$confidence)
  
  return(list(
    file_info = file_info,
    structure = structure_info,
    preview_data = preview_data,
    confidence = list(
      score = structure_info$confidence,
      level = confidence_level,
      message = generate_confidence_message(structure_info$confidence, confidence_level)
    ),
    recommended_action = recommend_action(confidence_level, structure_info)
  ))
}

#' Classify confidence score into level
#' 
#' @param score Numeric - Confidence score (0-100)
#' @return Character - Confidence level
classify_confidence <- function(score) {
  if (score >= 85) return("high")
  if (score >= 70) return("medium")
  if (score >= 50) return("low")
  return("very_low")
}

#' Generate user-friendly confidence message
#' 
#' @param score Numeric - Confidence score
#' @param level Character - Confidence level
#' @return Character - Message
generate_confidence_message <- function(score, level) {
  switch(level,
    high = sprintf("✓ Detected with high confidence (%d%%)", round(score)),
    medium = sprintf("⚠ Detected with medium confidence (%d%%). Please verify.", round(score)),
    low = sprintf("⚠ Low confidence detection (%d%%). Manual review recommended.", round(score)),
    very_low = sprintf("✗ Detection uncertain (%d%%). Manual classification required.", round(score))
  )
}

#' Recommend action based on confidence
#' 
#' @param level Character - Confidence level
#' @param structure_info List - Structure detection result
#' @return Character - Recommended action
recommend_action <- function(level, structure_info) {
  switch(level,
    high = "proceed_with_auto",
    medium = "show_confirmation",
    low = "show_manual_selector",
    very_low = "require_manual_selection"
  )
}

cat("  ✓ Section 2: File Detection loaded\n")

# ============================================================================
# SECTION 3: COLUMN MAPPING
# ============================================================================
# Interactive column mapping interface for import wizard
# ============================================================================

#' Create column mapping data frame for UI
#' 
#' @param file_path Character - Path to the file
#' @param detection_result List - Result from detect_file_structure()
#' @param import_configs List - Pre-loaded import configs (optional)
#' @return data.frame - Mapping table for UI
create_mapping_table <- function(file_path, detection_result, import_configs = NULL) {
  
  if (is.null(detection_result$structure$matched_type)) {
    stop("No matched type found in detection result")
  }
  
  if (is.null(import_configs)) {
    config <- get_import_type_config(detection_result$structure$matched_type)
  } else {
    config <- import_configs[[detection_result$structure$matched_type]]
  }
  
  file_cols <- detection_result$structure$col_names
  preview_data <- detection_result$preview_data
  
  mapping_rows <- list()
  
  # Required columns
  for (col_name in names(config$required_columns)) {
    col_config <- config$required_columns[[col_name]]
    
    match_result <- find_column_match(file_cols, col_config, preview_data)
    
    sample_data <- ""
    if (!is.null(match_result$matched_col)) {
      col_data <- preview_data[[match_result$matched_col]]
      valid_data <- col_data[!is.na(col_data)]
      if (length(valid_data) > 0) {
        sample_values <- head(valid_data, 3)
        sample_data <- paste(sample_values, collapse = ", ")
      }
    }
    
    validation_status <- "pending"
    validation_message <- ""
    if (!is.null(match_result$matched_col) && !is.null(col_config$validation)) {
      is_valid <- col_config$validation(preview_data[[match_result$matched_col]])
      validation_status <- ifelse(is_valid, "valid", "invalid")
      validation_message <- ifelse(is_valid, "✓", "✗ Validation failed")
    }
    
    mapping_rows[[length(mapping_rows) + 1]] <- list(
      required_column = col_name,
      description = col_config$description,
      file_column = ifelse(is.null(match_result$matched_col), "", match_result$matched_col),
      match_type = ifelse(is.null(match_result$matched_col), "", match_result$match_type),
      sample_data = sample_data,
      validation = validation_message,
      is_required = TRUE,
      is_mapped = !is.null(match_result$matched_col)
    )
  }
  
  # Optional columns
  if (!is.null(config$optional_columns)) {
    for (col_name in names(config$optional_columns)) {
      col_config <- config$optional_columns[[col_name]]
      
      match_result <- find_column_match(file_cols, col_config, preview_data)
      
      sample_data <- ""
      if (!is.null(match_result$matched_col)) {
        col_data <- preview_data[[match_result$matched_col]]
        valid_data <- col_data[!is.na(col_data)]
        if (length(valid_data) > 0) {
          sample_values <- head(valid_data, 3)
          sample_data <- paste(sample_values, collapse = ", ")
        }
      }
      
      mapping_rows[[length(mapping_rows) + 1]] <- list(
        required_column = col_name,
        description = col_config$description,
        file_column = ifelse(is.null(match_result$matched_col), "", match_result$matched_col),
        match_type = ifelse(is.null(match_result$matched_col), "", match_result$match_type),
        sample_data = sample_data,
        validation = "",
        is_required = FALSE,
        is_mapped = !is.null(match_result$matched_col)
      )
    }
  }
  
  mapping_df <- do.call(rbind, lapply(mapping_rows, as.data.frame, stringsAsFactors = FALSE))
  
  return(mapping_df)
}

#' Get available file columns for dropdown
#' 
#' @param file_path Character - Path to the file
#' @param detection_result List - Detection result
#' @return Character vector - Available column names
get_available_columns <- function(file_path, detection_result) {
  return(c("", detection_result$structure$col_names))
}

#' Validate mapping completeness
#' 
#' @param mapping_df data.frame - Current mapping table
#' @return List with is_valid (logical) and message (character)
validate_mapping <- function(mapping_df) {
  required_rows <- mapping_df[mapping_df$is_required == TRUE, ]
  unmapped_required <- required_rows[required_rows$file_column == "", ]
  
  if (nrow(unmapped_required) > 0) {
    return(list(
      is_valid = FALSE,
      message = sprintf("✗ %d required column(s) not mapped: %s", 
                       nrow(unmapped_required),
                       paste(unmapped_required$required_column, collapse = ", "))
    ))
  }
  
  invalid_rows <- required_rows[required_rows$validation == "✗ Validation failed", ]
  if (nrow(invalid_rows) > 0) {
    return(list(
      is_valid = FALSE,
      message = sprintf("✗ %d column(s) failed validation: %s",
                       nrow(invalid_rows),
                       paste(invalid_rows$required_column, collapse = ", "))
    ))
  }
  
  mapped_cols <- mapping_df$file_column[mapping_df$file_column != ""]
  if (any(duplicated(mapped_cols))) {
    duplicates <- unique(mapped_cols[duplicated(mapped_cols)])
    return(list(
      is_valid = FALSE,
      message = sprintf("✗ Column(s) mapped multiple times: %s",
                       paste(duplicates, collapse = ", "))
    ))
  }
  
  return(list(
    is_valid = TRUE,
    message = sprintf("✓ All mappings valid (%d required, %d optional)",
                     sum(required_rows$is_mapped),
                     sum(!mapping_df$is_required & mapping_df$is_mapped))
  ))
}

#' Apply column mapping to data
#' 
#' @param data data.frame - Original data
#' @param mapping_df data.frame - Mapping table
#' @return data.frame - Data with renamed columns
apply_column_mapping <- function(data, mapping_df) {
  mapped_rows <- mapping_df[mapping_df$file_column != "", ]
  
  if (nrow(mapped_rows) == 0) {
    return(data.frame(row.names = seq_len(nrow(data))))
  }
  
  result_cols <- list()
  
  for (i in seq_len(nrow(mapped_rows))) {
    file_col <- mapped_rows$file_column[i]
    required_col <- mapped_rows$required_column[i]
    
    if (file_col %in% colnames(data)) {
      result_cols[[required_col]] <- data[[file_col]]
    }
  }
  
  if (length(result_cols) > 0) {
    result <- as.data.frame(result_cols, stringsAsFactors = FALSE)
  } else {
    result <- data.frame(row.names = seq_len(nrow(data)))
  }
  
  return(result)
}

#' Column Mapper UI
#' 
#' @param id Character - Module namespace ID
#' @return Shiny UI elements
columnMapperUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "background-color: #f8f9fa; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
      h4("Column Mapping", style = "margin: 0; color: #495057;"),
      p("Map file columns to required fields. Required columns are marked in bold.",
        style = "margin: 5px 0 0 0; color: #6c757d;")
    ),
    
    uiOutput(ns("detection_info")),
    
    div(
      style = "margin-bottom: 20px;",
      DTOutput(ns("mapping_table"))
    ),
    
    uiOutput(ns("validation_status")),
    
    div(
      style = "margin-top: 30px;",
      h5("Data Preview (after mapping)"),
      DTOutput(ns("preview_table"))
    ),
    
    div(
      style = "margin-top: 20px; text-align: right;",
      actionButton(ns("btn_save_template"), "Save as Template", 
                   icon = icon("save"), class = "btn-secondary"),
      actionButton(ns("btn_reset"), "Reset to Auto-detect", 
                   icon = icon("undo"), class = "btn-warning"),
      actionButton(ns("btn_apply"), "Apply Mapping", 
                   icon = icon("check"), class = "btn-success")
    )
  )
}

#' Column Mapper Server
#' 
#' @param id Character - Module namespace ID
#' @param file_path Reactive - Path to the file
#' @param detection_result Reactive - Detection result from detect_file_structure()
#' @return Reactive list with mapped_data and mapping_df
columnMapperServer <- function(id, file_path, detection_result) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      mapping_df = NULL,
      available_cols = NULL,
      mapped_data = NULL
    )
    
    observe({
      req(detection_result())
      
      rv$mapping_df <- create_mapping_table(file_path(), detection_result())
      rv$available_cols <- get_available_columns(file_path(), detection_result())
    })
    
    output$detection_info <- renderUI({
      req(detection_result())
      result <- detection_result()
      
      confidence_color <- switch(
        result$confidence$level,
        "high" = "#28a745",
        "medium" = "#ffc107",
        "low" = "#fd7e14",
        "very_low" = "#dc3545",
        "#6c757d"
      )
      
      div(
        style = "background-color: #e9ecef; padding: 10px; margin-bottom: 15px; border-left: 4px solid",
        style = paste0("border-left-color: ", confidence_color, ";"),
        tags$strong(sprintf("Detected Type: %s", result$structure$matched_type)),
        tags$br(),
        sprintf("Confidence: %d%% (%s)", 
               round(result$confidence$score), 
               result$confidence$level),
        tags$br(),
        sprintf("Structure: %s | Columns: %d", 
               result$structure$type,
               result$structure$n_cols)
      )
    })
    
    output$mapping_table <- renderDT({
      req(rv$mapping_df)
      
      datatable(
        rv$mapping_df[, c("required_column", "description", "file_column", 
                         "match_type", "sample_data", "validation")],
        colnames = c("Required Field", "Description", "File Column", 
                    "Match Type", "Sample Data", "Status"),
        selection = "none",
        editable = list(target = "cell", disable = list(columns = c(0, 1, 3, 4, 5))),
        options = list(
          pageLength = 20,
          dom = "t",
          scrollX = TRUE,
          columnDefs = list(
            list(width = "150px", targets = 0),
            list(width = "200px", targets = 1),
            list(width = "150px", targets = 2),
            list(width = "100px", targets = 3),
            list(width = "250px", targets = 4),
            list(width = "80px", targets = 5)
          )
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          "required_column",
          target = "row",
          fontWeight = styleEqual(
            rv$mapping_df$required_column[rv$mapping_df$is_required],
            rep("bold", sum(rv$mapping_df$is_required))
          )
        ) %>%
        formatStyle(
          "validation",
          color = styleEqual(c("✓", "✗ Validation failed"), c("green", "red"))
        )
    })
    
    observeEvent(input$mapping_table_cell_edit, {
      info <- input$mapping_table_cell_edit
      row <- info$row
      col <- info$col
      value <- info$value
      
      if (col == 2) {
        rv$mapping_df$file_column[row] <- value
        rv$mapping_df$is_mapped[row] <- (value != "")
        
        if (value != "" && rv$mapping_df$is_required[row]) {
          col_config <- get_import_type_config(detection_result()$structure$matched_type)$required_columns[[rv$mapping_df$required_column[row]]]
          
          if (!is.null(col_config$validation)) {
            is_valid <- col_config$validation(detection_result()$preview_data[[value]])
            rv$mapping_df$validation[row] <- ifelse(is_valid, "✓", "✗ Validation failed")
          }
        }
        
        if (value != "") {
          col_data <- detection_result()$preview_data[[value]]
          valid_data <- col_data[!is.na(col_data)]
          if (length(valid_data) > 0) {
            sample_values <- head(valid_data, 3)
            rv$mapping_df$sample_data[row] <- paste(sample_values, collapse = ", ")
          }
        } else {
          rv$mapping_df$sample_data[row] <- ""
        }
      }
    })
    
    output$validation_status <- renderUI({
      req(rv$mapping_df)
      
      validation <- validate_mapping(rv$mapping_df)
      
      status_color <- ifelse(validation$is_valid, "#28a745", "#dc3545")
      
      div(
        style = paste0("padding: 10px; background-color: ", 
                      ifelse(validation$is_valid, "#d4edda", "#f8d7da"),
                      "; border-left: 4px solid ", status_color, ";"),
        validation$message
      )
    })
    
    output$preview_table <- renderDT({
      req(rv$mapping_df)
      req(detection_result())
      
      mapped_data <- apply_column_mapping(detection_result()$preview_data, rv$mapping_df)
      
      if (ncol(mapped_data) == 0) {
        return(data.frame(Message = "No columns mapped yet"))
      }
      
      datatable(
        head(mapped_data, 10),
        options = list(
          pageLength = 10,
          dom = "t",
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
    
    observeEvent(input$btn_reset, {
      rv$mapping_df <- create_mapping_table(file_path(), detection_result())
      showNotification("Mapping reset to auto-detected values", type = "info")
    })
    
    observeEvent(input$btn_apply, {
      req(rv$mapping_df)
      
      validation <- validate_mapping(rv$mapping_df)
      
      if (!validation$is_valid) {
        showNotification(validation$message, type = "error")
        return()
      }
      
      rv$mapped_data <- apply_column_mapping(detection_result()$preview_data, rv$mapping_df)
      
      showNotification("Mapping applied successfully!", type = "message")
    })
    
    return(reactive({
      list(
        mapped_data = rv$mapped_data,
        mapping_df = rv$mapping_df
      )
    }))
  })
}

cat("  ✓ Section 3: Column Mapping loaded\n")

# ============================================================================
# SECTION 4: DATA TRANSFORMATION
# ============================================================================
# Transform and validate imported data according to import type requirements
# ============================================================================

#' Read full file with correct settings
#' 
#' @param file_path Character - Path to file
#' @param detection_result List - Result from detect_file_structure()
#' @param sheet Character - Excel sheet name (optional)
#' @param sheets Character vector - Multiple Excel sheet names (optional)
#' @return data.frame - Full data
read_full_file <- function(file_path, detection_result, sheet = NULL, sheets = NULL) {
  file_info <- detection_result$file_info
  
  if (file_info$type == "csv") {
    delimiter <- file_info$delimiter
    
    if (delimiter == ";") {
      data <- readr::read_csv2(file_path, col_types = cols(), show_col_types = FALSE,
                              locale = locale(decimal_mark = ",", grouping_mark = "."))
    } else {
      data <- readr::read_csv(file_path, col_types = cols(), show_col_types = FALSE,
                             locale = locale(decimal_mark = ".", grouping_mark = ","))
    }
  } else if (file_info$type == "excel") {
    selected_sheets <- NULL
    
    if (!is.null(sheets) && length(sheets) > 0) {
      selected_sheets <- sheets
    } else if (!is.null(sheet) && length(sheet) > 0) {
      selected_sheets <- c(sheet)
    } else if (!is.null(file_info$sheets) && length(file_info$sheets) > 0) {
      selected_sheets <- c(file_info$sheets[1])
    } else {
      stop("No sheet information available")
    }
    
    if (length(selected_sheets) == 1) {
      data <- readxl::read_excel(file_path, sheet = selected_sheets[1])
    } else {
      sheet_data_list <- list()
      
      for (sheet_name in selected_sheets) {
        sheet_data <- tryCatch({
          readxl::read_excel(file_path, sheet = sheet_name)
        }, error = function(e) {
          warning(sprintf("Failed to read sheet '%s': %s", sheet_name, conditionMessage(e)))
          NULL
        })
        
        if (!is.null(sheet_data) && ncol(sheet_data) > 0) {
          sheet_data_list[[sheet_name]] <- sheet_data
        }
      }
      
      if (length(sheet_data_list) == 0) {
        stop("Failed to read any of the selected sheets")
      }
      
      has_pseudo_id <- any(sapply(sheet_data_list, function(df) {
        any(grepl("pseudo_?id", colnames(df), ignore.case = TRUE))
      }))
      
      if (has_pseudo_id && length(sheet_data_list) == 2) {
        tryCatch({
          sheet_row_counts <- sapply(sheet_data_list, nrow)
          
          if (length(sheet_row_counts) != 2) {
            stop("Expected exactly 2 sheets for pseudo_id join")
          }
          
          data_sheet_idx <- which.max(sheet_row_counts)
          mapping_sheet_idx <- which.min(sheet_row_counts)
          
          data_df <- sheet_data_list[[data_sheet_idx]]
          mapping_df <- sheet_data_list[[mapping_sheet_idx]]
          
          pseudo_col_data <- grep("pseudo_?id", colnames(data_df), ignore.case = TRUE, value = TRUE)
          pseudo_col_mapping <- grep("pseudo_?id", colnames(mapping_df), ignore.case = TRUE, value = TRUE)
          
          if (length(pseudo_col_data) > 0 && length(pseudo_col_mapping) > 0) {
            pseudo_col_data <- pseudo_col_data[1]
            pseudo_col_mapping <- pseudo_col_mapping[1]
            
            colnames(data_df)[colnames(data_df) == pseudo_col_data] <- "pseudo_id"
            colnames(mapping_df)[colnames(mapping_df) == pseudo_col_mapping] <- "pseudo_id"
            
            mapping_df_unique <- mapping_df[!duplicated(mapping_df$pseudo_id), ]
            
            data <- merge(data_df, mapping_df_unique, by = "pseudo_id", all.x = TRUE, suffixes = c("", "_mapping"))
          } else {
            data <- do.call(cbind, sheet_data_list)
          }
        }, error = function(e) {
          max_rows <- max(sapply(sheet_data_list, nrow))
          padded_sheets <- lapply(names(sheet_data_list), function(sheet_name) {
            df <- sheet_data_list[[sheet_name]]
            if (nrow(df) < max_rows) {
              empty_rows <- matrix(NA, nrow = max_rows - nrow(df), ncol = ncol(df))
              colnames(empty_rows) <- colnames(df)
              empty_df <- as.data.frame(empty_rows, stringsAsFactors = FALSE)
              df <- rbind(df, empty_df)
            }
            original_names <- colnames(df)
            colnames(df) <- paste0(original_names, "_", sheet_name)
            return(df)
          })
          data <- do.call(cbind, padded_sheets)
        })
      } else {
        max_rows <- max(sapply(sheet_data_list, nrow))
        
        padded_sheets <- lapply(names(sheet_data_list), function(sheet_name) {
          df <- sheet_data_list[[sheet_name]]
          if (nrow(df) < max_rows) {
            empty_rows <- matrix(NA, nrow = max_rows - nrow(df), ncol = ncol(df))
            colnames(empty_rows) <- colnames(df)
            empty_df <- as.data.frame(empty_rows, stringsAsFactors = FALSE)
            df <- rbind(df, empty_df)
          }
          
          original_names <- colnames(df)
          colnames(df) <- paste0(original_names, "_", sheet_name)
          
          return(df)
        })
        
        data <- do.call(cbind, padded_sheets)
      }
    }
  } else {
    stop(sprintf("Unsupported file type: %s", file_info$type))
  }
  
  return(as.data.frame(data))
}

#' Transform imported data according to mapping
#' 
#' @param file_path Character - Path to source file
#' @param detection_result List - Detection result
#' @param mapping_df data.frame - Column mapping table
#' @param sheet Character - Excel sheet (optional, single sheet)
#' @param sheets Character vector - Excel sheets (optional, multiple sheets)
#' @return List with transformed_data and transformation_log
transform_import_data <- function(file_path, detection_result, mapping_df, sheet = NULL, sheets = NULL) {
  
  log <- list(
    timestamp = Sys.time(),
    file_path = file_path,
    import_type = detection_result$structure$matched_type,
    steps = list()
  )
  
  log$steps[[length(log$steps) + 1]] <- list(
    step = "read_file",
    status = "started",
    message = sprintf("Reading file: %s", basename(file_path))
  )
  
  tryCatch({
    raw_data <- read_full_file(file_path, detection_result, sheet, sheets)
    log$steps[[length(log$steps)]]$status <- "success"
    log$steps[[length(log$steps)]]$rows_read <- nrow(raw_data)
    log$steps[[length(log$steps)]]$cols_read <- ncol(raw_data)
  }, error = function(e) {
    log$steps[[length(log$steps)]]$status <- "error"
    log$steps[[length(log$steps)]]$error <- conditionMessage(e)
    return(list(transformed_data = NULL, log = log))
  })
  
  log$steps[[length(log$steps) + 1]] <- list(
    step = "apply_mapping",
    status = "started",
    message = "Applying column mapping"
  )
  
  mapped_rows <- mapping_df[mapping_df$file_column != "", ]
  
  if (nrow(mapped_rows) > 0) {
    for (i in seq_len(nrow(mapped_rows))) {
      file_col <- mapped_rows$file_column[i]
      
      if (!file_col %in% colnames(raw_data)) {
        test_col <- file_col
        found <- FALSE
        
        while (grepl("_", test_col) && !found) {
          test_col <- sub("_[^_]*$", "", test_col)
          
          if (test_col %in% colnames(raw_data)) {
            mapped_rows$file_column[i] <- test_col
            found <- TRUE
          }
        }
      }
    }
  }
  
  result_cols <- list()
  for (i in seq_len(nrow(mapped_rows))) {
    file_col <- mapped_rows$file_column[i]
    required_col <- mapped_rows$required_column[i]
    
    if (file_col %in% colnames(raw_data)) {
      result_cols[[required_col]] <- raw_data[[file_col]]
    }
  }
  
  if (length(result_cols) == 0) {
    log$steps[[length(log$steps)]]$status <- "error"
    log$steps[[length(log$steps)]]$error <- "No columns mapped"
    return(list(transformed_data = NULL, log = log))
  }
  
  mapped_data <- as.data.frame(result_cols, stringsAsFactors = FALSE)
  log$steps[[length(log$steps)]]$status <- "success"
  log$steps[[length(log$steps)]]$columns_mapped <- ncol(mapped_data)
  
  log$steps[[length(log$steps) + 1]] <- list(
    step = "type_conversion",
    status = "started",
    message = "Converting data types"
  )
  
  config <- get_import_type_config(detection_result$structure$matched_type)
  converted_data <- apply_type_conversions(mapped_data, config, log)
  
  log$steps[[length(log$steps)]]$status <- "success"
  
  log$steps[[length(log$steps) + 1]] <- list(
    step = "data_cleaning",
    status = "started",
    message = "Cleaning data"
  )
  
  cleaned_data <- clean_data(converted_data, config)
  
  log$steps[[length(log$steps)]]$status <- "success"
  log$steps[[length(log$steps)]]$rows_before <- nrow(converted_data)
  log$steps[[length(log$steps)]]$rows_after <- nrow(cleaned_data)
  log$steps[[length(log$steps)]]$rows_removed <- nrow(converted_data) - nrow(cleaned_data)
  
  log$steps[[length(log$steps) + 1]] <- list(
    step = "pseudo_id_handling",
    status = "started",
    message = "Processing pseudo_id and studiealias"
  )
  
  tryCatch({
    if ("pseudo_id" %in% colnames(cleaned_data)) {
      rows_before <- nrow(cleaned_data)
      
      if ("studiealias" %in% colnames(cleaned_data)) {
        has_valid_alias <- !is.na(cleaned_data$studiealias) & 
                           (nchar(trimws(as.character(cleaned_data$studiealias))) > 0)
        
        rows_removed <- sum(!has_valid_alias)
        cleaned_data <- cleaned_data[has_valid_alias, , drop = FALSE]
        
        log$steps[[length(log$steps)]]$details <- sprintf(
          "Removed %d rows without valid studiealias mapping", 
          rows_removed
        )
      } else {
        log$steps[[length(log$steps)]]$details <- "Warning: pseudo_id found but no studiealias column available"
      }
      
      cleaned_data$pseudo_id <- NULL
      
      mapping_cols <- grep("_mapping$", colnames(cleaned_data), value = TRUE)
      if (length(mapping_cols) > 0) {
        cleaned_data <- cleaned_data[, !colnames(cleaned_data) %in% mapping_cols, drop = FALSE]
      }
      
      log$steps[[length(log$steps)]]$status <- "success"
      log$steps[[length(log$steps)]]$rows_before <- rows_before
      log$steps[[length(log$steps)]]$rows_after <- nrow(cleaned_data)
      log$steps[[length(log$steps)]]$rows_removed <- rows_removed
      log$steps[[length(log$steps)]]$message <- sprintf("Removed %d rows without valid studiealias mapping", rows_removed)
    } else if ("pseudo_id" %in% colnames(cleaned_data)) {
      cleaned_data$pseudo_id <- NULL
      log$steps[[length(log$steps)]]$status <- "success"
      log$steps[[length(log$steps)]]$message <- "Removed pseudo_id column (no studiealias column)"
    } else {
      log$steps[[length(log$steps)]]$status <- "skipped"
      log$steps[[length(log$steps)]]$message <- "No pseudo_id column found"
    }
  }, error = function(e) {
    log$steps[[length(log$steps)]]$status <- "error"
    log$steps[[length(log$steps)]]$error <- conditionMessage(e)
  })
  
  log$steps[[length(log$steps) + 1]] <- list(
    step = "validation",
    status = "started",
    message = "Validating data"
  )
  
  validation_result <- validate_transformed_data(cleaned_data, config)
  
  log$steps[[length(log$steps)]]$status <- ifelse(validation_result$is_valid, "success", "warning")
  log$steps[[length(log$steps)]]$validation_errors <- validation_result$errors
  log$steps[[length(log$steps)]]$validation_warnings <- validation_result$warnings
  
  log$summary <- list(
    total_rows_input = nrow(raw_data),
    total_rows_output = nrow(cleaned_data),
    total_columns = ncol(cleaned_data),
    is_valid = validation_result$is_valid,
    error_count = length(validation_result$errors),
    warning_count = length(validation_result$warnings)
  )
  
  return(list(
    transformed_data = cleaned_data,
    log = log,
    validation = validation_result
  ))
}

#' Apply data type conversions
#' 
#' @param data data.frame - Mapped data
#' @param config List - Import type config
#' @param log List - Transformation log
#' @return data.frame - Data with converted types
apply_type_conversions <- function(data, config, log) {
  result <- data
  
  all_cols <- c(config$required_columns, config$optional_columns)
  
  for (col_name in names(all_cols)) {
    if (!(col_name %in% colnames(result))) next
    
    col_config <- all_cols[[col_name]]
    col_data <- result[[col_name]]
    
    if (!is.null(col_config$type)) {
      result[[col_name]] <- tryCatch({
        convert_column_type(col_data, col_config$type)
      }, error = function(e) {
        warning(sprintf("Type conversion failed for %s: %s", col_name, conditionMessage(e)))
        col_data
      })
    }
  }
  
  return(result)
}

#' Convert column to specified type
#' 
#' @param col_data Vector - Column data
#' @param target_type Character - Target type
#' @return Vector - Converted data
convert_column_type <- function(col_data, target_type) {
  switch(target_type,
    "character" = as.character(col_data),
    "numeric" = as.numeric(col_data),
    "integer" = as.integer(col_data),
    "logical" = as.logical(col_data),
    "date" = parse_date_column(col_data),
    "datetime" = parse_datetime_column(col_data),
    col_data
  )
}

#' Parse date column with multiple format attempts
#' 
#' @param col_data Vector - Column data
#' @return Date vector
parse_date_column <- function(col_data) {
  if (is.character(col_data)) {
    col_data <- sapply(col_data, function(val) {
      if (is.na(val) || val == "") return(val)
      val <- sub("T\\d{2}:\\d{2}:\\d{2}.*$", "", val)
      val <- sub("\\s+\\d{2}:\\d{2}(:\\d{2})?.*$", "", val)
      return(val)
    }, USE.NAMES = FALSE)
  }
  
  formats <- c(
    "%Y-%m-%d",
    "%d-%m-%Y",
    "%d/%m/%Y",
    "%Y/%m/%d",
    "%d.%m.%Y",
    "%d-%b-%Y",
    "%d %B %Y"
  )
  
  for (fmt in formats) {
    result <- tryCatch({
      as.Date(col_data, format = fmt)
    }, error = function(e) NULL)
    
    if (!is.null(result) && sum(!is.na(result)) > length(col_data) * 0.5) {
      return(result)
    }
  }
  
  return(as.Date(col_data))
}

#' Parse datetime column
#' 
#' @param col_data Vector - Column data
#' @return POSIXct vector
parse_datetime_column <- function(col_data) {
  formats <- c(
    "%d-%m-%Y %H:%M:%S",
    "%Y-%m-%d %H:%M:%S",
    "%d/%m/%Y %H:%M:%S",
    "%d-%m-%Y %H:%M",
    "%Y-%m-%d %H:%M"
  )
  
  for (fmt in formats) {
    result <- tryCatch({
      as.POSIXct(col_data, format = fmt)
    }, error = function(e) NULL)
    
    if (!is.null(result) && sum(!is.na(result)) > length(col_data) * 0.5) {
      return(result)
    }
  }
  
  return(as.POSIXct(col_data))
}

#' Clean data (remove empty rows, trim whitespace, etc.)
#' 
#' @param data data.frame - Data to clean
#' @param config List - Import type config
#' @return data.frame - Cleaned data
clean_data <- function(data, config) {
  result <- data
  
  for (col in names(result)) {
    col_data <- result[[col]]
    
    if (inherits(col_data, c("POSIXct", "POSIXlt", "POSIXt"))) {
      result[[col]] <- format(as.Date(col_data), "%Y-%m-%d")
    }
    else if (inherits(col_data, "Date")) {
      result[[col]] <- format(col_data, "%Y-%m-%d")
    }
  }
  
  char_cols <- sapply(result, is.character)
  for (col in names(which(char_cols))) {
    result[[col]] <- trimws(result[[col]])
  }
  
  empty_rows <- apply(result, 1, function(row) {
    all(is.na(row) | row == "" | row == " ")
  })
  
  if (sum(empty_rows) > 0) {
    result <- result[!empty_rows, , drop = FALSE]
  }
  
  required_cols <- names(config$required_columns)
  required_cols_present <- required_cols[required_cols %in% colnames(result)]
  
  if (length(required_cols_present) > 0) {
    required_all_empty <- apply(result[, required_cols_present, drop = FALSE], 1, function(row) {
      all(is.na(row) | row == "" | row == " ")
    })
    
    if (sum(required_all_empty) > 0) {
      result <- result[!required_all_empty, , drop = FALSE]
    }
  }
  
  rownames(result) <- NULL
  
  return(result)
}

#' Validate transformed data
#' 
#' @param data data.frame - Transformed data
#' @param config List - Import type config
#' @return List with is_valid, errors, and warnings
validate_transformed_data <- function(data, config) {
  errors <- list()
  warnings <- list()
  
  required_cols <- names(config$required_columns)
  
  actually_required <- character(0)
  for (col_name in required_cols) {
    col_config <- config$required_columns[[col_name]]
    if (is.null(col_config$required) || isTRUE(col_config$required)) {
      actually_required <- c(actually_required, col_name)
    }
  }
  
  missing_cols <- setdiff(actually_required, colnames(data))
  
  if (length(missing_cols) > 0) {
    errors[[length(errors) + 1]] <- list(
      type = "missing_columns",
      severity = "error",
      message = sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", "))
    )
  }
  
  if (nrow(data) == 0) {
    errors[[length(errors) + 1]] <- list(
      type = "no_data",
      severity = "error",
      message = "No data rows found after transformation"
    )
  }
  
  for (col_name in names(config$required_columns)) {
    if (!(col_name %in% colnames(data))) next
    
    col_config <- config$required_columns[[col_name]]
    col_data <- data[[col_name]]
    
    if (!is.null(col_config$validation)) {
      is_valid <- col_config$validation(col_data)
      
      if (!is_valid) {
        errors[[length(errors) + 1]] <- list(
          type = "validation_failed",
          column = col_name,
          severity = "error",
          message = sprintf("Validation failed for column: %s", col_name)
        )
      }
    }
    
    na_count <- sum(is.na(col_data))
    na_percent <- (na_count / length(col_data)) * 100
    
    if (na_percent > 50) {
      warnings[[length(warnings) + 1]] <- list(
        type = "high_na_rate",
        column = col_name,
        severity = "warning",
        message = sprintf("Column %s has %.1f%% missing values", col_name, na_percent),
        na_count = na_count,
        na_percent = na_percent
      )
    }
  }
  
  duplicate_count <- sum(duplicated(data))
  if (duplicate_count > 0) {
    warnings[[length(warnings) + 1]] <- list(
      type = "duplicates",
      severity = "warning",
      message = sprintf("Found %d duplicate rows", duplicate_count),
      duplicate_count = duplicate_count
    )
  }
  
  for (col_name in colnames(data)) {
    col_data <- data[[col_name]]
    
    if (is.character(col_data)) {
      as_numeric <- suppressWarnings(as.numeric(col_data))
      numeric_success_rate <- sum(!is.na(as_numeric)) / length(col_data)
      
      if (numeric_success_rate > 0.8 && numeric_success_rate < 1) {
        warnings[[length(warnings) + 1]] <- list(
          type = "mixed_types",
          column = col_name,
          severity = "warning",
          message = sprintf("Column %s appears mostly numeric but contains non-numeric values", col_name)
        )
      }
    }
  }
  
  return(list(
    is_valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  ))
}

#' Format validation result for display
#' 
#' @param validation_result List - Result from validate_transformed_data()
#' @return Character - Formatted message
format_validation_message <- function(validation_result) {
  if (validation_result$is_valid && length(validation_result$warnings) == 0) {
    return("✓ All validation checks passed")
  }
  
  messages <- c()
  
  if (length(validation_result$errors) > 0) {
    messages <- c(messages, sprintf("✗ %d error(s):", length(validation_result$errors)))
    for (err in validation_result$errors) {
      messages <- c(messages, sprintf("  - %s", err$message))
    }
  }
  
  if (length(validation_result$warnings) > 0) {
    messages <- c(messages, sprintf("⚠ %d warning(s):", length(validation_result$warnings)))
    for (warn in validation_result$warnings) {
      messages <- c(messages, sprintf("  - %s", warn$message))
    }
  }
  
  return(paste(messages, collapse = "\n"))
}

#' Get transformation summary
#' 
#' @param transformation_result List - Result from transform_import_data()
#' @return Character - Summary message
get_transformation_summary <- function(transformation_result) {
  log <- transformation_result$log
  
  if (is.null(log$summary)) {
    return("Transformation incomplete")
  }
  
  summary <- log$summary
  
  msg <- sprintf(
    "Transformation complete:\n  Input: %d rows, Output: %d rows (%d removed)\n  Columns: %d\n  Status: %s",
    summary$total_rows_input,
    summary$total_rows_output,
    summary$total_rows_input - summary$total_rows_output,
    summary$total_columns,
    ifelse(summary$is_valid, "✓ Valid", sprintf("✗ %d errors, %d warnings", summary$error_count, summary$warning_count))
  )
  
  return(msg)
}

cat("  ✓ Section 4: Data Transformation loaded\n")

# ============================================================================
# SECTION 5: TEMPLATE MANAGEMENT
# ============================================================================
# Functions voor het opslaan, laden en beheren van column mapping templates
# ============================================================================

# Template directory
TEMPLATE_DIR <- "config/mapping_templates"

#' Ensure template directory exists
#' 
#' @return Path to template directory
ensure_template_dir <- function() {
  if (!dir.exists(TEMPLATE_DIR)) {
    dir.create(TEMPLATE_DIR, recursive = TRUE)
  }
  return(TEMPLATE_DIR)
}

#' Get template file path
#' 
#' @param import_type Character. Type of import
#' @param template_name Character. Name of the template
#' @return Character path to template file
get_template_path <- function(import_type, template_name) {
  ensure_template_dir()
  
  safe_type <- gsub("[^a-zA-Z0-9_-]", "_", import_type)
  safe_name <- gsub("[^a-zA-Z0-9_-]", "_", template_name)
  
  filename <- paste0(safe_type, "_", safe_name, ".json")
  return(file.path(TEMPLATE_DIR, filename))
}

#' Save a mapping template
#' 
#' @param template_name Character. Name for the template
#' @param import_type Character. Type of import
#' @param mapping_df Data frame. The mapping table from create_mapping_table()
#' @param description Character. Optional description of the template
#' @return List with success status and message
save_mapping_template <- function(template_name, import_type, mapping_df, description = "") {
  tryCatch({
    if (is.null(template_name) || template_name == "") {
      return(list(success = FALSE, message = "Template name is required"))
    }
    
    if (is.null(import_type) || import_type == "") {
      return(list(success = FALSE, message = "Import type is required"))
    }
    
    if (!is.data.frame(mapping_df)) {
      return(list(success = FALSE, message = "mapping_df must be a data frame"))
    }
    
    template <- list(
      name = template_name,
      import_type = import_type,
      description = description,
      created_at = Sys.time(),
      mapping = list(
        required_columns = mapping_df[mapping_df$is_required, c("required_column", "file_column", "match_type")],
        optional_columns = mapping_df[!mapping_df$is_required, c("required_column", "file_column", "match_type")]
      )
    )
    
    file_path <- get_template_path(import_type, template_name)
    write_json(template, file_path, pretty = TRUE, auto_unbox = TRUE)
    
    return(list(
      success = TRUE,
      message = paste("Template saved:", template_name),
      path = file_path
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error saving template:", e$message)
    ))
  })
}

#' Load a mapping template
#' 
#' @param template_name Character. Name of the template to load
#' @param import_type Character. Type of import
#' @return List with success status, message, and template data
load_mapping_template <- function(template_name, import_type) {
  tryCatch({
    file_path <- get_template_path(import_type, template_name)
    
    if (!file.exists(file_path)) {
      return(list(
        success = FALSE,
        message = paste("Template not found:", template_name)
      ))
    }
    
    template <- read_json(file_path, simplifyVector = TRUE)
    
    return(list(
      success = TRUE,
      message = paste("Template loaded:", template_name),
      template = template
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error loading template:", e$message)
    ))
  })
}

#' Apply a template to a mapping data frame
#' 
#' @param mapping_df Data frame. Current mapping table
#' @param template List. Template loaded with load_mapping_template()
#' @return Data frame. Updated mapping table with template applied
apply_template_to_mapping <- function(mapping_df, template) {
  tryCatch({
    template_mappings <- as.data.frame(template$mapping$required_columns)
    
    if (!is.null(template$mapping$optional_columns) && 
        length(template$mapping$optional_columns) > 0 &&
        nrow(as.data.frame(template$mapping$optional_columns)) > 0) {
      template_mappings <- rbind(
        template_mappings,
        as.data.frame(template$mapping$optional_columns)
      )
    }
    
    for (i in seq_len(nrow(mapping_df))) {
      req_col <- mapping_df$required_column[i]
      
      template_row <- template_mappings[template_mappings$required_column == req_col, ]
      
      if (nrow(template_row) > 0) {
        mapping_df$file_column[i] <- template_row$file_column[1]
        mapping_df$is_mapped[i] <- (template_row$file_column[1] != "")
        mapping_df$match_type[i] <- template_row$match_type[1]
      }
    }
    
    return(mapping_df)
    
  }, error = function(e) {
    warning("Error applying template:", e$message)
    return(mapping_df)
  })
}

#' List all templates for an import type
#' 
#' @param import_type Character. Type of import (optional, if NULL returns all)
#' @return Data frame with columns: name, import_type, description, created_at, file_path
list_templates <- function(import_type = NULL) {
  tryCatch({
    ensure_template_dir()
    
    all_files <- list.files(TEMPLATE_DIR, pattern = "\\.json$", full.names = TRUE)
    
    if (length(all_files) == 0) {
      return(data.frame(
        name = character(),
        import_type = character(),
        description = character(),
        created_at = character(),
        file_path = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    templates_list <- lapply(all_files, function(file_path) {
      tryCatch({
        template <- read_json(file_path, simplifyVector = TRUE)
        
        data.frame(
          name = template$name,
          import_type = template$import_type,
          description = ifelse(is.null(template$description), "", template$description),
          created_at = as.character(template$created_at),
          file_path = file_path,
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        NULL
      })
    })
    
    templates_df <- do.call(rbind, Filter(Negate(is.null), templates_list))
    
    if (!is.null(import_type) && nrow(templates_df) > 0) {
      templates_df <- templates_df[templates_df$import_type == import_type, ]
    }
    
    return(templates_df)
    
  }, error = function(e) {
    warning("Error listing templates:", e$message)
    return(data.frame(
      name = character(),
      import_type = character(),
      description = character(),
      created_at = character(),
      file_path = character(),
      stringsAsFactors = FALSE
    ))
  })
}

#' Delete a template
#' 
#' @param template_name Character. Name of the template to delete
#' @param import_type Character. Type of import
#' @return List with success status and message
delete_template <- function(template_name, import_type) {
  tryCatch({
    file_path <- get_template_path(import_type, template_name)
    
    if (!file.exists(file_path)) {
      return(list(
        success = FALSE,
        message = paste("Template not found:", template_name)
      ))
    }
    
    file.remove(file_path)
    
    return(list(
      success = TRUE,
      message = paste("Template deleted:", template_name)
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error deleting template:", e$message)
    ))
  })
}

#' Get template info without loading full mapping
#' 
#' @param template_name Character. Name of the template
#' @param import_type Character. Type of import
#' @return List with template metadata
get_template_info <- function(template_name, import_type) {
  tryCatch({
    file_path <- get_template_path(import_type, template_name)
    
    if (!file.exists(file_path)) {
      return(list(
        success = FALSE,
        message = "Template not found"
      ))
    }
    
    template <- read_json(file_path, simplifyVector = TRUE)
    
    req_mapped <- sum(template$mapping$required_columns$file_column != "")
    req_total <- nrow(template$mapping$required_columns)
    
    if (!is.null(template$mapping$optional_columns) && 
        length(template$mapping$optional_columns) > 0 &&
        nrow(as.data.frame(template$mapping$optional_columns)) > 0) {
      opt_mapped <- sum(template$mapping$optional_columns$file_column != "")
      opt_total <- nrow(as.data.frame(template$mapping$optional_columns))
    } else {
      opt_mapped <- 0
      opt_total <- 0
    }
    
    return(list(
      success = TRUE,
      name = template$name,
      import_type = template$import_type,
      description = template$description,
      created_at = template$created_at,
      required_mapped = req_mapped,
      required_total = req_total,
      optional_mapped = opt_mapped,
      optional_total = opt_total
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error reading template:", e$message)
    ))
  })
}

cat("  ✓ Section 5: Template Management loaded\n")

# ============================================================================
# SECTION 6: DATA EXPORT
# ============================================================================
# Functions voor het exporteren van getransformeerde data naar CSV/Excel
# ============================================================================

#' Get export directory for import type
#' 
#' @param import_type Character. Type of import
#' @return Character path to export directory
get_export_dir <- function(import_type) {
  base_dir <- "input_data"
  
  dir_map <- list(
    epic_baseline = file.path(base_dir, "epic_export"),
    biobank_data = file.path(base_dir, "biobank_data"),
    follow_up = file.path(base_dir, "follow_up"),
    mdns_mapping = file.path(base_dir, "mdns_mapping")
  )
  
  export_dir <- dir_map[[import_type]]
  
  if (is.null(export_dir)) {
    export_dir <- file.path(base_dir, import_type)
  }
  
  if (!dir.exists(export_dir)) {
    dir.create(export_dir, recursive = TRUE)
  }
  
  return(export_dir)
}

#' Generate export filename based on import type
#' 
#' @param import_type Character. Type of import
#' @param original_filename Character. Original input filename (not used)
#' @return Character filename for export
generate_export_filename <- function(import_type, original_filename = NULL) {
  filename_map <- list(
    epic_baseline = "EpicExport.csv",
    follow_up = "EpicExport.csv",
    biobank_data = "biobank_data.csv",
    mdns_mapping = "MDNS.csv"
  )
  
  filename <- filename_map[[import_type]]
  
  if (is.null(filename)) {
    filename <- paste0(import_type, "_export.csv")
  }
  
  return(filename)
}

#' Export data to CSV
#' 
#' @param data Data frame or data.table. The transformed data
#' @param import_type Character. Type of import
#' @param original_filename Character. Original input filename (optional)
#' @param custom_path Character. Custom export path (optional)
#' @return List with success status, message, and file path
export_to_csv <- function(data, import_type, original_filename = NULL, custom_path = NULL) {
  tryCatch({
    if (!is.data.frame(data)) {
      return(list(
        success = FALSE,
        message = "Data must be a data frame"
      ))
    }
    
    if (nrow(data) == 0) {
      return(list(
        success = FALSE,
        message = "No data to export"
      ))
    }
    
    if (!is.null(custom_path)) {
      export_path <- custom_path
    } else {
      export_dir <- get_export_dir(import_type)
      filename <- generate_export_filename(import_type, original_filename)
      export_path <- file.path(export_dir, filename)
    }
    
    if (is.null(export_path) || !is.character(export_path) || nchar(export_path) == 0) {
      return(list(
        success = FALSE,
        message = "Failed to generate export path"
      ))
    }
    
    write_csv2(data, export_path)
    
    export_filename <- tryCatch({
      basename(export_path)
    }, error = function(e) {
      "exported_file.csv"
    })
    
    return(list(
      success = TRUE,
      message = paste("Data exported successfully to:", export_filename),
      path = export_path,
      rows = nrow(data),
      columns = ncol(data)
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error exporting data:", e$message)
    ))
  })
}

#' Export data to Excel
#' 
#' @param data Data frame. The transformed data
#' @param import_type Character. Type of import
#' @param original_filename Character. Original input filename (optional)
#' @param custom_path Character. Custom export path (optional)
#' @return List with success status, message, and file path
export_to_excel <- function(data, import_type, original_filename = NULL, custom_path = NULL) {
  tryCatch({
    if (!requireNamespace("writexl", quietly = TRUE)) {
      return(list(
        success = FALSE,
        message = "Package 'writexl' is required for Excel export"
      ))
    }
    
    if (!is.data.frame(data)) {
      return(list(
        success = FALSE,
        message = "Data must be a data frame"
      ))
    }
    
    if (!is.null(custom_path)) {
      export_path <- custom_path
    } else {
      export_dir <- get_export_dir(import_type)
      filename <- generate_export_filename(import_type, original_filename)
      export_path <- file.path(export_dir, sub("\\.csv$", ".xlsx", filename))
    }
    
    writexl::write_xlsx(data, export_path)
    
    return(list(
      success = TRUE,
      message = paste("Data exported successfully to:", basename(export_path)),
      path = export_path,
      rows = nrow(data),
      columns = ncol(data)
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error exporting data:", e$message)
    ))
  })
}

#' Preview export data (first N rows)
#' 
#' @param data Data frame. The data to preview
#' @param n Integer. Number of rows to preview
#' @return Data frame with preview
preview_export <- function(data, n = 10) {
  if (nrow(data) <= n) {
    return(data)
  }
  return(head(data, n))
}

#' Get export summary statistics
#' 
#' @param data Data frame. The transformed data
#' @return List with summary statistics
get_export_summary <- function(data) {
  list(
    total_rows = nrow(data),
    total_columns = ncol(data),
    column_names = names(data),
    missing_values = sum(is.na(data)),
    data_types = sapply(data, class),
    file_size_estimate = format(object.size(data), units = "KB")
  )
}

#' Validate export data for specific import type
#' 
#' @param data Data frame. The data to validate
#' @param import_type Character. Type of import
#' @return List with validation results
validate_export_data <- function(data, import_type) {
  issues <- character()
  
  if (import_type == "epic_baseline") {
    required <- c("Element", "waarde", "studiealias", "datum")
    missing <- setdiff(required, names(data))
    if (length(missing) > 0) {
      issues <- c(issues, paste("Missing required columns:", paste(missing, collapse = ", ")))
    }
  } else if (import_type == "biobank_data") {
    if (!"Identificatie" %in% names(data)) {
      issues <- c(issues, "Missing required column: Identificatie")
    }
  }
  
  empty_cols <- names(data)[sapply(data, function(x) all(is.na(x) | x == ""))]
  if (length(empty_cols) > 0) {
    issues <- c(issues, paste("Empty columns detected:", paste(empty_cols, collapse = ", ")))
  }
  
  warnings <- character()
  if (anyDuplicated(data) > 0) {
    dup_count <- sum(duplicated(data))
    warnings <- c(warnings, paste("Duplicate rows detected:", dup_count))
  }
  
  list(
    is_valid = length(issues) == 0,
    issues = issues,
    warnings = warnings
  )
}

#' Export with automatic format detection and validation
#' 
#' @param transformation_result List. Result from transform_import_data()
#' @param format Character. Export format ("csv" or "excel")
#' @param custom_path Character. Custom export path (optional)
#' @return List with export results
export_transformation_result <- function(transformation_result, format = "csv", custom_path = NULL) {
  tryCatch({
    if (is.null(transformation_result) || !is.list(transformation_result)) {
      return(list(
        success = FALSE,
        message = "Invalid transformation result"
      ))
    }
    
    if (!is.null(transformation_result$transformed_data)) {
      data <- transformation_result$transformed_data
      import_type <- transformation_result$log$import_type
      original_file <- transformation_result$log$file_path
      is_valid <- transformation_result$validation$is_valid
    } else if (!is.null(transformation_result$data)) {
      if (!transformation_result$success) {
        return(list(
          success = FALSE,
          message = "Cannot export: transformation was not successful"
        ))
      }
      data <- transformation_result$data
      import_type <- transformation_result$import_type
      original_file <- transformation_result$file_path
      is_valid <- TRUE
    } else {
      return(list(
        success = FALSE,
        message = "No data found in transformation result"
      ))
    }
    
    original_filename <- NULL
    if (!is.null(original_file) && is.character(original_file) && nchar(original_file) > 0) {
      original_filename <- basename(original_file)
    }
    
    validation <- validate_export_data(data, import_type)
    if (!validation$is_valid) {
      return(list(
        success = FALSE,
        message = paste("Export validation failed:", paste(validation$issues, collapse = "; "))
      ))
    }
    
    if (format == "excel") {
      result <- export_to_excel(data, import_type, original_filename, custom_path)
    } else {
      result <- export_to_csv(data, import_type, original_filename, custom_path)
    }
    
    if (length(validation$warnings) > 0) {
      result$warnings <- validation$warnings
    }
    
    return(result)
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error exporting transformation result:", e$message)
    ))
  })
}

cat("  ✓ Section 6: Data Export loaded\n")

# ============================================================================
# MODULE COMPLETE
# ============================================================================

cat("✓ Import Wizard Combined Module fully loaded\n")
cat("\n")

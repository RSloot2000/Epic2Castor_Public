# ============================================================================
# OPTION LISTS GENERATOR - DROPDOWN OPTIONS FOR SHINY UI
# ============================================================================
# Version: 2025-11-05 (Production)
#
# Purpose:
#   Generate dropdown options for Shiny UI elements based on:
#   - Castor metadata (field options, study variables)
#   - EPIC input data (tables, columns, elements, values)
#   - Mapping configuration files
#
# Architecture:
#   - Centralized path configuration via config/paths.json
#   - Performance caching to avoid redundant processing
#   - Automatic detection of field types (checkbox, radiobutton)
#   - Dynamic option generation based on file content
#
# Outputs:
#   - option_data: Named list of dropdown options for each UI element
#   - checkboxes: Vector of checkbox field names
#   - radiobuttons: Vector of radiobutton field names
#   - checkBoxesValues: Data frame mapping checkbox fields to option labels
#   - radioButtonOptionValues: Data frame mapping radiobutton fields to option values
#   - metaRadioButtons: Castor radiobutton metadata
#   - metaVariables: Castor variable metadata
#
# Usage:
#   source("scripts/option_lists2.R")
#   # Access via: option_data[["table_name|column_name"]]
# ============================================================================

# ===== LOAD CONFIGURATION =====
# Centralize paths via config/paths.json
paths <- jsonlite::fromJSON("config/paths.json")

# Helper function to build paths (OS-independent)
path_join <- function(...) file.path(...)

# ===== DERIVED PATH VARIABLES FROM PATHS.JSON =====
dir_scripts <- paths$scripts_dir
dir_input_epic <- paths$epic_input_data_dir
dir_output_example <- paths$output_data_example_dir
dir_mapping <- paths$mapping_dir
dir_castor_meta <- paths$castor_meta_dir
dir_castor_export <- paths$castor_export_dir

# ===== OPTIONAL/NEWLY ADDED KEYS (WITH SAFE FALLBACK IF MISSING) =====
# These paths may not exist in older versions of paths.json
# Fallback ensures backward compatibility with legacy configurations

file_castor_field_options <- if (!is.null(paths$castor_field_options_file)) paths$castor_field_options_file else path_join(dir_castor_meta, "field_options.csv")
file_castor_study_variablelist <- if (!is.null(paths$castor_study_variablelist_file)) paths$castor_study_variablelist_file else path_join(dir_castor_meta, "study_variablelist.csv")
file_mapping_variabelen <- if (!is.null(paths$mapping_variabelen_file)) paths$mapping_variabelen_file else path_join(dir_mapping, "variabelen.csv")
dir_mapping_possible_values <- if (!is.null(paths$mapping_possible_values_dir)) paths$mapping_possible_values_dir else path_join(dir_mapping, "possibleValues")

# ============================================================================
# PERFORMANCE CACHING
# ============================================================================
# Cache option_data to avoid redundant processing on every reload
# Force reload can be triggered via: options(epic2castor.force_option_reload = TRUE)
#
# Performance Impact:
#   - Without cache: ~2-5 seconds to generate all options (file I/O intensive)
#   - With cache: <0.1 seconds (instant reload from memory)
#
# Cache Invalidation:
#   - Manual: options(epic2castor.force_option_reload = TRUE)
#   - Automatic: When App.r calls reload_castor_metadata()
#   - Session: Cache lost when R session restarts

force_option_reload <- isTRUE(getOption("epic2castor.force_option_reload", FALSE))
cache <- getOption("epic2castor.option_cache", NULL)
cached_loaded <- !force_option_reload && !is.null(cache) && 
  !is.null(cache$option_data) && !is.null(cache$checkBoxesValues)

# ============================================================================
# INITIALIZE VARIABLES WITH SAFE DEFAULTS
# ============================================================================
# Ensures script can run even if Castor metadata is missing

option_data <- NULL
checkBoxesValues <- data.frame(kolom = character(0), toevoeging = character(0))
radioButtonOptionValues <- data.frame(kolom = character(0), waarde = character(0))
checkboxes <- character(0)
radiobuttons <- character(0)
metaRadioButtons <- data.frame(
  `Option Group Name` = character(0),
  `Option Name` = character(0),
  `Option Value` = integer(0),
  `Option Group Id` = character(0),
  check.names = FALSE
)
metaVariables <- data.frame(
  `Form Name` = character(0),
  `Form Order` = integer(0),
  `Field Option Group` = character(0),
  `Field Variable Name` = character(0),
  `Field Type` = character(0),
  check.names = FALSE
)

# ===== LOAD FROM CACHE IF AVAILABLE =====
if (cached_loaded) {
  option_data <- cache$option_data
  checkBoxesValues <- cache$checkBoxesValues
  radioButtonOptionValues <- cache$radioButtonOptionValues
  checkboxes <- cache$checkboxes
  radiobuttons <- cache$radiobuttons
  metaRadioButtons <- cache$metaRadioButtons
  metaVariables <- cache$metaVariables
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Capitalize first letter of a string
#' 
#' Converts first character to uppercase while preserving the rest
#' Used for normalizing EPIC values to ensure consistent capitalization
#' 
#' @param x String to capitalize
#' @return String with capitalized first letter
#' 
#' @examples
#' capitalize_first("male") # Returns "Male"
#' capitalize_first("MALE") # Returns "MALE"
capitalize_first <- function(x) {
  if (is.na(x) || length(x) == 0 || nchar(x) == 0) return(x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

#' Read data from CSV or Excel files with normalization
#' 
#' Automatically detects file type and applies consistent formatting:
#' - Reads as character type to preserve leading zeros
#' - Strips whitespace from all columns
#' - Capitalizes first letter of value columns (waarde, Waarde, Element)
#' 
#' @param file_path Path to CSV or XLSX file
#' @param ... Additional arguments passed to fread() or read_excel()
#' @return data.table with normalized data
#' 
#' @details
#' Normalization Strategy:
#'   - "male" -> "Male" (capitalize first letter only)
#'   - "MALE" -> "MALE" (preserve all-caps)
#'   - "  text  " -> "text" (strip whitespace)
#'   - Leading zeros preserved: "007" stays "007" (colClasses = "character")
#' 
#' Why normalize?
#'   - EPIC data often has inconsistent capitalization
#'   - Castor requires exact matches for dropdown values
#'   - Prevents mapping errors due to "Male" vs "male" mismatches
read_data <- function(file_path, ...) {
  dots <- list(...)
  result <- NULL
  if (grepl("\\.csv$", file_path)) {
    result <- fread(file_path, colClasses = "character", strip.white = TRUE, ...)
  } else if (grepl("\\.xlsx$", file_path)) {
    # read_excel expects n_max instead of nrows; map if provided
    if (!is.null(dots$nrows)) {
      dots$n_max <- dots$nrows
      dots$nrows <- NULL
    }
    result <- as.data.table(do.call(readxl::read_excel, c(list(file_path), dots)))
  }
  
  if (!is.null(result)) {
    # Normalize values in specific columns where capitalization is important
    # Focus on relevant columns for dropdown options
    value_columns <- c("waarde", "Waarde", "Element")
    for (col in names(result)) {
      if (col %in% value_columns && is.character(result[[col]])) {
        result[[col]] <- sapply(result[[col]], capitalize_first)
      }
    }
  }
  
  return(result)
}

# ============================================================================
# CONFIGURATION BUILDER
# ============================================================================
# Build configuration matrix defining dropdown options for each column
# Maps each column to its data source (file, column, matching rules, etc.)

if (!cached_loaded) {
  # ===== STEP 1: SCAN ALL MAPPING FILES =====
  # Create configuration entry for every column in every mapping CSV file
  # This builds a master config that defines how to generate dropdown options
  # for each column across all mapping tables (elements.csv, waarde_radiobuttons.csv, etc.)
  config <- do.call(rbind, lapply(
    list.files(dir_mapping, full.names = TRUE, pattern = "\\.csv$"), 
    function(file_path) {
      cols <- colnames(read_data(file_path, nrows = 0))
      data.frame(
        table = rep(file_path, length(cols)),           # Source table for this column
        col = cols,                                     # Column name
        option_file = NA_character_,                    # Where to load options from
        option_col = NA_character_,                     # Which column contains the options
        option_col_match_list = NA_character_,          # Column for dependent matching (e.g., Element)
        option_col_match_table = NA_character_,         # Table column to match against
        type = NA_character_,                           # Type hint: "tabellen", "kolommen", "waarden", "elementen"
        custom = NA_character_,                         # Hardcoded custom values
        stringsAsFactors = FALSE
      )
    }
  ))

  # ===== STEP 2: CONFIGURE SPECIFIC COLUMNS =====
  
  # --- castor_kolom: Castor field variable names ---
  # Source: study_variablelist.csv (Castor metadata)
  config[config$col == "castor_kolom", "option_file"] <- file_castor_study_variablelist
  config[config$col == "castor_kolom", "option_col"] <- "Field Variable Name"

  # --- epic_tabel: EPIC table names ---
  # Source: Filenames in epic_input_data_dir
  config[config$col == "epic_tabel", "option_file"] <- dir_input_epic
  config[config$col == "epic_tabel", "type"] <- "elementen"

  # --- epic_kolom: EPIC column names ---
  # Source: All column names across EPIC input files
  config[config$col == "epic_kolom", "option_file"] <- dir_input_epic
  config[config$col == "epic_kolom", "type"] <- "kolommen"

  # --- castor_tabel: Castor table names ---
  # Source: Filenames in castor_export_dir
  config[config$col == "castor_tabel", "option_file"] <- dir_castor_export
  config[config$col == "castor_tabel", "type"] <- "tabellen"

  # Note: No fallback to output_data_example for castor_kolom; only use Castor metadata

  # --- Custom configuration columns ---
  config[config$col == "key", "custom"] <- "TRUE"
  config[config$col == "repeating", "custom"] <- "TRUE"
  config[config$col == "gerelateerde_mapping", "custom"] <- "waarde_checkboxes|waarde_radiobuttons/elements"

  # ===== STEP 3: CONFIGURE ELEMENT AND VALUE COLUMNS =====
  # These appear in waarde_radiobuttons.csv and waarde_checkboxes.csv
  # Complex mapping: Element -> waarde (EPIC values depend on selected Element)
  # 
  # Example: If user selects Element "Gender", waarde dropdown should show ["Male", "Female"]
  #          If user selects Element "Smoking", waarde dropdown should show ["Yes", "No", "Former"]
  # 
  # Implementation: Store element-value pairs with matching logic
  
  element_rows <- which(config$col == "Element")
  waarde_rows <- which(config$col == "waarde")
  castor_waarde_rows <- which(config$col == "castor_waarde")
  kolom_toevoeging_rows <- which(config$col == "kolom_toevoeging")

  # First Element column (waarde_radiobuttons)
  if (length(element_rows) >= 1) {
    config[element_rows[1], "option_file"] <- dir_input_epic
    config[element_rows[1], "type"] <- "elementen"
  }

  # First waarde column (waarde_radiobuttons)
  # Values depend on selected Element (dynamic filtering)
  if (length(waarde_rows) >= 1) {
    config[waarde_rows[1], "option_file"] <- dir_input_epic
    config[waarde_rows[1], "type"] <- "waarden"
    config[waarde_rows[1], "option_col_match_table"] <- "Element"
    config[waarde_rows[1], "option_col_match_list"] <- "Element"
  }

  # castor_waarde: Castor option names (radiobutton/checkbox values)
  if (length(castor_waarde_rows) >= 1) {
    config[castor_waarde_rows[1], "option_file"] <- file_castor_field_options
    config[castor_waarde_rows[1], "option_col"] <- "Option Name"
  }

  # Second Element column (waarde_checkboxes)
  if (length(element_rows) >= 2) {
    config[element_rows[2], "option_file"] <- dir_input_epic
    config[element_rows[2], "type"] <- "elementen"
  }

  # Second waarde column (waarde_checkboxes)
  if (length(waarde_rows) >= 2) {
    config[waarde_rows[2], "option_file"] <- dir_input_epic
    config[waarde_rows[2], "type"] <- "waarden"
    config[waarde_rows[2], "option_col_match_table"] <- "Element"
    config[waarde_rows[2], "option_col_match_list"] <- "Element"
  }

  # kolom_toevoeging: Checkbox option names
  if (length(kolom_toevoeging_rows) >= 1) {
    config[kolom_toevoeging_rows[1], "option_file"] <- file_castor_field_options
    config[kolom_toevoeging_rows[1], "option_col"] <- "Option Name"
  }

  # ============================================================================
  # OPTION GENERATOR FUNCTION
  # ============================================================================
  
  #' Generate dropdown options based on configuration
  #' 
  #' Main logic for building option lists for each UI dropdown element
  #' Handles multiple data sources and matching strategies:
  #' 1. Custom options (hardcoded values)
  #' 2. File-based tables (castor_tabel)
  #' 3. Column names (epic_kolom)
  #' 4. Element-value pairs (waarden with matching)
  #' 5. Input data options (elements from EPIC files)
  #' 6. Specific file options (from Castor metadata)
  #' 
  #' @param config_row Single row from config data.frame
  #' @return List with:
  #'   - options: Character vector of dropdown choices
  #'   - match: Optional matching values for dependent dropdowns
  #'   - match_col: Column name for matching
  #'   - table: Table name (usually NA)
  generate_options <- function(config_row) {
    # ===== CUSTOM OPTIONS =====
    # Hardcoded values like "TRUE", "waarde_checkboxes|waarde_radiobuttons"
    if (!is.na(config_row$custom)) {
      if (grepl("/elements", config_row$custom)) {
        return(list(table = NA, match = NA, options = NA))
      } else {
        return(list(
          table = NA,
          match = NA,
          options = c("", strsplit(config_row$custom, "/")[[1]])
        ))
      }
    }
    
    # ===== TYPE: TABELLEN (TABLE NAMES FROM FILES) =====
    # Extract filenames without extension from a directory
    if (!is.na(config_row$type) && config_row$type == "tabellen") {
      files <- list.files(config_row$option_file, full.names = TRUE)
      return(list(
        table = NA,
        match = NA,
        options = tools::file_path_sans_ext(basename(files))
      ))
    } 
    
    # ===== TYPE: KOLOMMEN (COLUMN NAMES FROM FILES) =====
    # Collect all unique column names across multiple files
    if (!is.na(config_row$type) && config_row$type == "kolommen") {
      all_columns <- c()
      for (file in list.files(config_row$option_file, full.names = TRUE)) {
        all_columns <- c(all_columns, colnames(read_data(file, nrows = 0)))
      }
      return(list(
        table = NA,
        match = NA,
        options = unique(all_columns)
      ))
    }
    
    # ===== TYPE: WAARDEN (VALUES WITH ELEMENT MATCHING) =====
    # Build element-value pairs from EPIC input data
    # Used for dependent dropdowns where values depend on selected element
    # 
    # Data Flow:
    #   1. Read variabelen.csv to find which EPIC tables have element-value relationships
    #   2. For each matching table, extract all Element-Value pairs
    #   3. Store with matching metadata so UI can filter values by selected Element
    # 
    # Example Output:
    #   options: ["Male", "Female", "Yes", "No", "Former", ...]  (all values)
    #   match:   ["Gender", "Gender", "Smoking", "Smoking", "Smoking", ...]  (corresponding elements)
    #   match_col: "Element"  (column to match on in the UI table)
    if (!is.na(config_row$type) && config_row$type == "waarden") {
      # Read variables mapping to identify relevant tables/columns
      vars <- read_data(file_mapping_variabelen, 
                      select = c("epic_tabel", "gerelateerde_mapping", "epic_kolom"))
      vars <- unique(vars[vars$gerelateerde_mapping == "waarde_checkboxes|waarde_radiobuttons", ])
      
      element_value_pairs <- data.frame(element = character(0), value = character(0))

      for (file in list.files(dir_input_epic, full.names = TRUE)) {
        file_name <- tools::file_path_sans_ext(basename(file))
        column <- vars$epic_kolom[vars$epic_tabel == file_name]
        
        if (length(column) != 1) next
        
        data <- read_data(file)
        
        # Check if required columns exist
        if (!all(c(column, "Element") %in% colnames(data))) next
        
        # Extract all element-value pairs
        pairs <- data[, c("Element", column), with = FALSE]
        colnames(pairs) <- c("element", "value")
        
        # Normalize values explicitly (in case not already normalized)
        pairs$value <- sapply(pairs$value, capitalize_first)
        
        # Add to collection
        element_value_pairs <- rbind(element_value_pairs, pairs)
      }
      
      # Remove duplicates
      element_value_pairs <- unique(element_value_pairs)
      
      return(list(
        options = element_value_pairs$value,
        match = element_value_pairs$element,
        match_col = "Element",
        table = NA
      ))
    }
    
    # ===== INPUT_DATA OPTIONS (ELEMENTS FROM EPIC FILES) =====
    # Extract elements or other columns from EPIC input files
    # 
    # Path Matching Logic:
    #   - Uses normalizePath() to compare paths reliably across OS
    #   - Checks if config points to epic_input_data_dir
    #   - If match: scan all EPIC files for relevant column values
    # 
    # Why tryCatch?
    #   - normalizePath() fails if path doesn't exist yet
    #   - Fallback to string comparison ensures robustness
    if (!is.na(config_row$option_file) && 
        tryCatch(normalizePath(config_row$option_file, winslash = "/", mustWork = FALSE), error = function(e) config_row$option_file) == 
        tryCatch(normalizePath(dir_input_epic, winslash = "/", mustWork = FALSE), error = function(e) dir_input_epic)) {
      vars <- read_data(file_mapping_variabelen, 
                      select = c("epic_tabel", "gerelateerde_mapping", "epic_kolom"))
      
      if (!is.na(config_row$type) && config_row$type == "elementen") {
        vars <- unique(vars[vars$gerelateerde_mapping == "elements", ])
      }
      
      all_options <- c()
      all_matches <- c()

      for (file in list.files(dir_input_epic, full.names = TRUE)) {
        file_name <- tools::file_path_sans_ext(basename(file))
        column <- vars$epic_kolom[vars$epic_tabel == file_name]
        
        if (length(column) != 1) next
        
        data <- read_data(file)
        sel_cols <- na.omit(c(column, config_row$option_col_match_list))
        
        if (all(sel_cols %in% colnames(data))) {
          unique_data <- unique(data[, sel_cols, with = FALSE])
          all_options <- c(all_options, unique_data[[column]])
          
          if (!is.na(config_row$option_col_match_list)) {
            all_matches <- c(all_matches, unique_data[[config_row$option_col_match_list]])
          }
        }
      }
      
      result <- list(options = unique(all_options), table = NA)
      
      if (!is.na(config_row$option_col_match_list)) {
        result$match <- all_matches
        result$match_col <- config_row$option_col_match_table
      } else {
        result$match <- NA
      }
      
      return(result)
    }
    
    # ===== SPECIFIC FILE OPTIONS (CASTOR METADATA) =====
    # Read options from a specific file (e.g., field_options.csv)
    if (!is.na(config_row$option_file)) {
      data <- read_data(config_row$option_file)
      if (!is.na(config_row$option_col) && config_row$option_col %in% names(data)) {
        result <- list(
          options = unique(data[[config_row$option_col]]),
          table = NA,
          match = NA
        )
        
        if (!is.na(config_row$option_col_match_list) && 
            config_row$option_col_match_list %in% names(data)) {
          result$match <- data[[config_row$option_col_match_list]]
        }
        
        return(result)
      }
    }
    
    # ===== DEFAULT RETURN (NO MATCH) =====
    return(list(table = NA, match = NA, options = character(0)))
  }

  # ============================================================================
  # GENERATE ALL OPTIONS
  # ============================================================================
  # Apply generate_options() to all configured columns with error handling
  # 
  # Output Format:
  #   option_data is a named list where names follow pattern: "table_name|column_name"
  #   Example: option_data[["waarde_radiobuttons|castor_waarde"]] = list(options = c("Yes", "No"), ...)
  # 
  # Error Handling:
  #   - Individual column failures don't stop entire process
  #   - Warnings logged but execution continues
  #   - Allows app to function even with partial option data

  option_rows <- which(!is.na(config$option_file) | !is.na(config$custom))
  option_data <- list()

  for (idx in option_rows) {
    tryCatch({
      option_name <- paste0(
        tools::file_path_sans_ext(basename(config$table[idx])),
        "|",
        config$col[idx]
      )
      option_data[[option_name]] <- generate_options(config[idx, ])
    }, error = function(e) {
      warning(paste("Error in row", idx, ":", e$message))
    })
  }


  # ===== ADD ELEMENTS|CASTOR_KOLOM (SPECIAL CASE) =====
  # Load castor_kolom options for elements table from possibleValues
  # 
  # Why Special Case?
  #   - elements.csv needs to show which Castor fields are available for mapping
  #   - pv_elements.csv is generated by batch_upload_helper.r from Castor metadata
  #   - Contains filtered list of valid Castor field names for element mapping
  # 
  # Fallback: If file doesn't exist, dropdown will be empty (graceful degradation)
  tryCatch({
    option_data[["elements|castor_kolom"]] <- list(
      table = "elements",
      match = NA,
      options = read_data(path_join(dir_mapping_possible_values, "pv_elements.csv"), sep = ";")$castor_kolom
    )
  }, error = function(e) {
    warning(paste("Error adding elements|castor_kolom:", e$message))
  })

  # ============================================================================
  # LOAD CASTOR METADATA
  # ============================================================================
  # Read Castor field options and study variables from metadata files
  # These files are retrieved via CastorRetrieval.r script using Castor API
  # 
  # File Structure:
  #   field_options.csv: Contains all dropdown options for radio/checkbox fields
  #     - Option Group Name: Human-readable group name
  #     - Option Name: Display text for the option
  #     - Option Value: Numeric value stored in Castor
  #     - Option Group Id: Internal Castor ID (converted to Name later)
  # 
  #   study_variablelist.csv: Contains all field definitions
  #     - Form Name: Which form the field belongs to (e.g., "Baseline")
  #     - Field Variable Name: Internal field name
  #     - Field Type: "radio", "checkbox", "text", "number", etc.
  #     - Field Option Group: Link to option group in field_options.csv
  # 
  # Error Handling:
  #   - If files missing/corrupt: Return empty data.frames with correct structure
  #   - Allows app to start even without Castor metadata
  #   - User can still edit mappings, just without Castor-specific dropdowns
  
  # --- Radio Button Options ---
  metaRadioButtons <- tryCatch({
    read_delim(
      file_castor_field_options, 
      delim = ";", 
      col_select = c("Option Group Name", "Option Name", "Option Value", "Option Group Id"),
      col_types = cols(
        `Option Group Name` = col_character(), 
        `Option Name` = col_character(), 
        `Option Value` = col_integer(), 
        `Option Group Id` = col_character()
      ), 
      trim_ws = TRUE
    )
  }, error = function(e) {
    warning(paste("Error reading field_options.csv:", e$message))
    data.frame(
      `Option Group Name` = character(0),
      `Option Name` = character(0),
      `Option Value` = integer(0),
      `Option Group Id` = character(0),
      check.names = FALSE
    )
  })

  # --- Study Variables ---
  metaVariables <- tryCatch({
    read_delim(
      file_castor_study_variablelist, 
      delim = ";",
      col_select = c("Form Name", "Form Order", "Field Option Group", "Field Variable Name", "Field Type"),
      col_types = cols(
        `Form Name` = col_character(), 
        `Form Order` = col_integer(), 
        `Field Option Group` = col_character(), 
        `Field Variable Name` = col_character(), 
        `Field Type` = col_character()
      ), 
      trim_ws = TRUE
    )
  }, error = function(e) {
    warning(paste("Error reading study_variablelist.csv:", e$message))
    data.frame(
      `Form Name` = character(0),
      `Form Order` = integer(0),
      `Field Option Group` = character(0),
      `Field Variable Name` = character(0),
      `Field Type` = character(0),
      check.names = FALSE
    )
  })

  # ============================================================================
  # IDENTIFY CHECKBOX AND RADIOBUTTON FIELDS
  # ============================================================================
  # Extract field names for checkboxes and radiobuttons from study variables
  # 
  # Purpose:
  #   - Identify which Castor fields are checkboxes vs radiobuttons
  #   - Different handling: checkboxes allow multiple values, radiobuttons only one
  # 
  # Output Format:
  #   - Prefix form abbreviation (first 4 chars lowercase) + "_" + field name
  #   - Example: "base_gender" (from Baseline form, field "gender")
  #   - Why? Ensures uniqueness across forms (multiple forms may have "gender" field)

  dt <- tryCatch({
    read_data(file_castor_study_variablelist, sep = ";")
  }, error = function(e) {
    warning(paste("Error reading study_variablelist.csv for checkboxes:", e$message))
    data.table()
  })

  checkboxes <- character(0)
  radiobuttons <- character(0)

  if (nrow(dt) > 0 && "Field Type" %in% names(dt)) {
    # Extract checkbox fields
    checkbox_dt <- dt[dt$`Field Type` == "checkbox", ]
    if (nrow(checkbox_dt) > 0 && "Form Name" %in% names(checkbox_dt) && "Field Variable Name" %in% names(checkbox_dt)) {
      checkboxes <- paste0(
        tolower(substr(checkbox_dt$`Form Name`, 1, 4)), 
        "_", 
        checkbox_dt$`Field Variable Name`
      )
    }
    
    # Extract radiobutton fields
    radio_dt <- dt[dt$`Field Type` == "radio", ]
    if (nrow(radio_dt) > 0 && "Form Name" %in% names(radio_dt) && "Field Variable Name" %in% names(radio_dt)) {
      radiobuttons <- paste0(
        tolower(substr(radio_dt$`Form Name`, 1, 4)), 
        "_", 
        radio_dt$`Field Variable Name`
      )
    }
  }

  # ============================================================================
  # PROCESS METADATA: OPTION GROUP ID TO NAME CONVERSION
  # ============================================================================
  # Convert Option Group IDs to human-readable names
  # Build radiobutton and checkbox value mappings
  # 
  # Why This Is Needed:
  #   - Castor API returns Option Group IDs (e.g., "A1B2C3D4")
  #   - App needs human-readable names (e.g., "Yes/No Options")
  #   - Must link metaVariables (fields) to metaRadioButtons (options)
  # 
  # Transformation Steps:
  #   1. Create ID->Name lookup table from metaRadioButtons
  #   2. Replace all IDs in metaVariables with Names
  #   3. Add form prefix to all field names (uniqueness)
  #   4. Sort by form order (preserve Castor display order)
  #   5. Extract radiobutton and checkbox specific mappings
  # 
  # Output:
  #   - radioButtonOptionValues: field_name -> [option_name, option_value] pairs
  #   - checkBoxesValues: field_name -> [option_name] pairs (option labels only)
  
  if (nrow(metaRadioButtons) > 0 && nrow(metaVariables) > 0 && 
      "Option Group Id" %in% names(metaRadioButtons) && 
      "Field Option Group" %in% names(metaVariables)) {
    
    # Create lookup table for Option Group ID -> Name
    lookup <- metaRadioButtons[, c("Option Group Id", "Option Group Name")]
    lookup <- lookup[order(lookup$`Option Group Id`), ]
    metaVariables$`Field Option Group` <- lookup$`Option Group Name`[
      match(metaVariables$`Field Option Group`, lookup$`Option Group Id`)
    ]
    
    # Add form prefix to field variable names (e.g., "base_" for baseline form)
    if ("Form Name" %in% names(metaVariables) && "Field Variable Name" %in% names(metaVariables)) {
      metaVariables$`Field Variable Name` <- paste0(
        tolower(substr(metaVariables$`Form Name`, 1, 4)), 
        "_", 
        metaVariables$`Field Variable Name`
      )
    }
    
    # Sort and remove unnecessary columns
    if ("Form Order" %in% names(metaVariables)) {
      metaVariables <- metaVariables[order(metaVariables$`Form Order`), ]
    }
    
    if ("Option Group Id" %in% names(metaRadioButtons)) {
      metaRadioButtons <- metaRadioButtons[, -which(names(metaRadioButtons) == "Option Group Id")]
    }
    
    if (all(c("Form Name", "Form Order") %in% names(metaVariables))) {
      metaVariables <- metaVariables[, -which(names(metaVariables) %in% c("Form Name", "Form Order"))]
    }
    
    # ===== BUILD RADIOBUTTON VALUE MAPPINGS =====
    # Maps field names to their available option values
    # 
    # Data Structure:
    #   Field Variable Name | Option Name | Option Value
    #   base_gender         | Male        | 1
    #   base_gender         | Female      | 2
    #   base_smoking        | Yes         | 1
    #   base_smoking        | No          | 0
    # 
    # Used By: baseline.r and follow_up.r to convert text values to numeric codes
    radioButtonOptionValues <- data.frame()
    if ("Field Type" %in% names(metaVariables)) {
      radioButtonOptionValues <- metaVariables[metaVariables$`Field Type` == "radio", ]
      if (nrow(radioButtonOptionValues) > 0 && 
          "Field Option Group" %in% names(radioButtonOptionValues) && 
          "Option Group Name" %in% names(metaRadioButtons)) {
        
        radioButtonOptionValues <- merge(
          x = radioButtonOptionValues, 
          y = metaRadioButtons, 
          by.x = "Field Option Group", 
          by.y = "Option Group Name",
          all.x = TRUE
        )
        
        if (all(c("Field Variable Name", "Option Name", "Option Value") %in% names(radioButtonOptionValues))) {
          radioButtonOptionValues <- radioButtonOptionValues[, 
            c("Field Variable Name", "Option Name", "Option Value")
          ]
        }
      }
    }
    
    # ===== BUILD CHECKBOX VALUE MAPPINGS =====
    # Maps checkbox field names to their available option labels
    # 
    # Data Structure:
    #   kolom (field name)  | toevoeging (option name) | Option Value
    #   base_symptoms       | Fever                    | 1
    #   base_symptoms       | Cough                    | 2
    #   base_symptoms       | Fatigue                  | 3
    # 
    # Difference From Radiobuttons:
    #   - Radiobuttons: Single selection only - one value per field (e.g., value = 1 OR 2)
    #   - Checkboxes: Multiple selection allowed - multiple values per field (e.g., values = 1 AND 2 AND 3)
    #   - Both use same field with numeric option values (1, 2, 3, etc.)
    #   - Export scripts need to handle multiple selected values for checkbox fields
    # 
    # Example:
    #   Radiobutton "Gender": User selects "Male" (value=1) → field stores: 1
    #   Checkbox "Symptoms": User selects "Fever" (1) and "Cough" (2) → field stores: [1, 2]
    # 
    # Note: Currently stored but not actively used by export scripts
    # Export scripts detect checkboxes through waarde_checkboxes mapping table
    checkBoxesValues <- data.frame(kolom = character(0), toevoeging = character(0))
    if ("Field Type" %in% names(metaVariables)) {
      checkboxVars <- metaVariables[metaVariables$`Field Type` == "checkbox", ]
      if (nrow(checkboxVars) > 0 && 
          "Field Option Group" %in% names(checkboxVars) && 
          "Option Group Name" %in% names(metaRadioButtons)) {
        
        checkBoxesValues <- merge(
          checkboxVars,
          metaRadioButtons,
          by.x = "Field Option Group",
          by.y = "Option Group Name",
          all.x = TRUE
        )
        
        if (all(c("Field Variable Name", "Option Name") %in% names(checkBoxesValues))) {
          checkBoxesValues <- data.frame(
            kolom = checkBoxesValues$`Field Variable Name`,
            toevoeging = checkBoxesValues$`Option Name`
          )
        }
      }
    }
  }

} # End of if (!cached_loaded)

# ============================================================================
# CACHE RESULTS
# ============================================================================
# Store generated options in global cache for reuse within the same R session
# Cache can be invalidated by setting: options(epic2castor.force_option_reload = TRUE)
# 
# What Gets Cached:
#   - option_data: All dropdown options (largest data structure)
#   - checkBoxesValues: Checkbox field->option mappings
#   - radioButtonOptionValues: Radiobutton field->value mappings
#   - checkboxes: Vector of checkbox field names
#   - radiobuttons: Vector of radiobutton field names
#   - metaRadioButtons: Raw Castor radiobutton metadata
#   - metaVariables: Raw Castor variable metadata
# 
# Cache Lifetime:
#   - Persists for entire R session (in memory)
#   - Survives script reloads (source() doesn't clear options)
#   - Cleared when R session restarts or force_option_reload = TRUE
#   - NOT saved between R sessions (options are session-specific)
# 
# Memory Impact: Depends on number of fields and options in project

options(
  epic2castor.option_cache = if (!cached_loaded) list(
    option_data = option_data,
    checkBoxesValues = checkBoxesValues,
    radioButtonOptionValues = radioButtonOptionValues,
    checkboxes = checkboxes,
    radiobuttons = radiobuttons,
    metaRadioButtons = metaRadioButtons,
    metaVariables = metaVariables
  ) else cache
)
options(epic2castor.force_option_reload = FALSE)
# ============================================================================
# IMPORT WIZARD - COMBINED MODULE
# ============================================================================
#
# Purpose:
#   All-in-one import wizard functionality combining:
#   - Import Configuration (type definitions, column requirements)
#   - File Detection (auto-detect file type, structure, encoding)
#   - Column Mapping (mapping logic for column matching)
#   - Data Transformation (apply mappings, conversions, validation)
#   - Template Management (save/load mapping templates)
#   - Data Export (export to CSV/Excel)
#
# Table of Contents:
#   SECTION 1: Import Configuration ........... line ~55
#     - base_epic_columns()    : Shared column defs for baseline/follow-up
#     - get_all_import_types() : All type configs (epic_baseline, follow_up,
#                                biobank_data, lab_data, mdns_mapping,
#                                mapping_elements)
#     - get_import_type_config(), get_enabled_import_types(), etc.
#   SECTION 2: File Detection ................. search "SECTION 2"
#     - detect_file_type(), detect_csv_delimiter(), detect_encoding()
#     - detect_structure_pattern(), calculate_match_score()
#     - detect_file_structure() [main entry point]
#   SECTION 3: Column Mapping ................. search "SECTION 3"
#     - create_mapping_table(), get_available_columns()
#     - validate_mapping(), apply_column_mapping()
#   SECTION 4: Data Transformation ............ search "SECTION 4"
#     - read_full_file(), transform_import_data()
#     - apply_type_conversions(), clean_data(), validate_transformed_data()
#   SECTION 5: Template Management ............ search "SECTION 5"
#     - save/load/delete/list_templates(), apply_template_to_mapping()
#   SECTION 6: Data Export .................... search "SECTION 6"
#     - export_to_csv(), export_to_excel(), export_transformation_result()
#
# Supported import types:
#   - epic_baseline   : EPIC element-value (baseline visits)
#   - follow_up       : EPIC element-value (follow-up visits)
#   - biobank_data    : Biobank sample data (wide format)
#   - lab_data        : Lab results (long format)
#   - mdns_mapping    : MDN-to-ParticipantID mapping
#   - mapping_elements: EPIC element-to-Castor field mapping
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

#' Build shared EPIC Element-Value column definitions
#' 
#' @return Named list of column configs shared by baseline and follow-up
#' @keywords internal
base_epic_columns <- function() {
  list(
    Element = list(
      required = TRUE,
      aliases = c("Element", "element", "ELEMENT", "question_id", "question"),
      detect_pattern = "form_entries_question_id_\\d+|form_entries_question_\\d+",
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
      aliases = c("waarde", "value", "EPIC_waarde", "Value", "WAARDE",
                 "value_text", "value_number"),
      detect_pattern = "form_entries_value_(text|number|multiple_choice)_\\d+",
      description = "Value for the element",
      example = "Yes",
      validation = function(x) {
        !all(is.na(x))
      }
    ),
    
    studiealias = list(
      required = TRUE,
      aliases = c("studiealias", "Participant Id", "ParticipantId", 
                 "Patient_ID", "patient_id", "PatientID", "STUDIEALIAS",
                 "pseudo_id_overview",
                 "studie identificatiecode", "Studie Identificatiecode",
                 "studie_identificatiecode"),
      detect_pattern = "pseudo_id.*overview|pseudo_id_\\d+|studie.?identificatie",
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
                 "measurement_date", "DATUM", "start_date"),
      detect_pattern = "form_entries_(start_date|value_date)_\\d+|age_at_time_of_event",
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
      detect_pattern = "pseudo_id",
      description = "Pseudo identifier to be replaced by studiealias",
      example = "ABC123",
      validation = function(x) TRUE
    )
  )
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
      file_patterns = c("epic.*export", "baseline", "epic.*baseline"),
      required_columns = base_epic_columns(),
      optional_columns = list(),
      ignore_extra_columns = TRUE,
      flexible_detection = FALSE,
      transformation_map = list(
        studiealias = "Participant Id",
        Element = "Element",
        waarde = "waarde",
        datum = "date_baseline",
        pseudo_id = "pseudo_id"
      ),
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
      file_patterns = c("follow.*up", "followup", "fu", "epic.*follow"),
      required_columns = base_epic_columns(),
      optional_columns = list(),
      ignore_extra_columns = TRUE,
      flexible_detection = FALSE,
      transformation_map = list(
        studiealias = "Participant Id",
        Element = "Element",
        waarde = "waarde",
        datum = "date_follow_up",
        pseudo_id = "pseudo_id"
      ),
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
      file_patterns = c("biobank", "sample"),
      required_columns = list(
        Identificatie = list(
          required = TRUE,
          aliases = c("Identificatie", "identificatie", "ID", "Patient_ID", 
                     "PatientID", "patient_id", "IDENTIFICATIE",
                     "Identificatie / Identification", "Identification"),
          description = "Patient identification number",
          example = "1768227",
          detect_pattern = "identificatie|identification",
          validation = function(x) {
            char_x <- trimws(as.character(x))
            sum(!is.na(char_x) & nzchar(char_x)) > 0
          }
        )
      ),
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
                     "SampleType", "MONSTERSOORT",
                     "Monstersoort /SampleType", "Monstersoort / SampleType",
                     "SampleType"),
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
                     "Sub.Collectie", "SUBCOLLECTIE",
                     "SubCollectie /SubCollection", "SubCollectie / SubCollection",
                     "SubCollection"),
          description = "Sub-collection name",
          example = "MYCOS",
          detect_pattern = "sub.*collect"
        ),
        `Afname datumtijd` = list(
          aliases = c("Afname datumtijd", "Afname.datumtijd", "collection_datetime",
                     "CollectionDateTime", "AFNAME_DATUMTIJD",
                     "Afname datumtijd / Draw datetime", "Draw datetime"),
          description = "Sample collection date/time",
          example = "2-1-2023 09:00",
          detect_pattern = "afname.*(datum|date).*(tijd|time)|draw.*date"
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
        ),
        `Beschrijving Moederbuis` = list(
          aliases = c("Beschrijving Moederbuis", "Beschrijving.Moederbuis",
                     "beschrijving_moederbuis"),
          description = "Mother tube description",
          example = "EDTA 10ml",
          detect_pattern = "beschrijving.*moeder|mother.*tube"
        ),
        `Beschrijving Aliquot` = list(
          aliases = c("Beschrijving Aliquot", "Beschrijving.Aliquot",
                     "beschrijving_aliquot"),
          description = "Aliquot description",
          example = "Plasma EDTA 500ul",
          detect_pattern = "beschrijving.*aliquot|aliquot.*desc"
        ),
        `Order` = list(
          aliases = c("Order", "order", "ORDER"),
          description = "Order number",
          example = "1"
        ),
        `Afgerond door` = list(
          aliases = c("Afgerond door", "Afgerond.door", "afgerond_door",
                     "completed_by", "Completed By"),
          description = "Completed by whom",
          example = "Lab technician",
          detect_pattern = "afgerond.*door|completed.*by"
        )
      ),
      ignore_extra_columns = FALSE,
      flexible_detection = TRUE,
      transformation_map = NULL,
      processing_notes = c(
        "All columns will be included in the output",
        "Column names will be preserved as-is",
        "Optional columns will be auto-detected using flexible matching"
      )
    ),
    
    # ========================================================================
    # LAB DATA - LONG FORMAT
    # ========================================================================
    lab_data = list(
      id = "lab_data",
      name = "Lab Data (Long Format)",
      description = "Laboratory results in long format (one row per lab value per patient)",
      icon = "flask",
      structure = "long_format",
      category = "clinical_data",
      file_patterns = c("lab.*data", "lab.*result", "laborator"),
      required_columns = list(
        ParticipantId = list(
          required = TRUE,
          aliases = c("ParticipantId", "Participant.Id", "Participant ID",
                     "Participant.ID", "participantId", "participant_id",
                     "PatientID", "Patient_ID"),
          description = "Patient identifier",
          example = "0001",
          validation = function(x) {
            char_x <- trimws(as.character(x))
            sum(!is.na(char_x) & nzchar(char_x)) > 0
          }
        ),
        LabName = list(
          required = TRUE,
          aliases = c("LabName", "Lab.Name", "labname", "Lab Name",
                     "lab_name", "LABNAME"),
          description = "Laboratory test name",
          example = "Hemoglobine",
          validation = function(x) {
            !all(is.na(x))
          }
        ),
        Value = list(
          required = TRUE,
          aliases = c("Value", "value", "VALUE", "Result", "result"),
          description = "Lab result value",
          example = "7.5",
          validation = function(x) {
            !all(is.na(x))
          }
        ),
        CollectionInstant = list(
          required = TRUE,
          aliases = c("CollectionInstant", "Collection.Instant", "CollectionDate",
                     "collection_instant", "Collection Instant", "collection_date"),
          description = "Date/time of sample collection",
          example = "2023-01-15 09:30:00",
          validation = function(x) {
            !all(is.na(x))
          }
        )
      ),
      optional_columns = list(
        NumericValue = list(
          aliases = c("NumericValue", "Numeric.Value", "numeric_value", "NumericResult"),
          description = "Numeric lab result value",
          example = "7.5"
        ),
        Abbreviation = list(
          aliases = c("Abbreviation", "abbreviation", "Abbr", "abbr"),
          description = "Lab test abbreviation",
          example = "Hb"
        ),
        DefaultUnit = list(
          aliases = c("DefaultUnit", "Default.Unit", "Unit", "unit", "default_unit"),
          description = "Unit of measurement",
          example = "mmol/L"
        ),
        ResultInstant = list(
          aliases = c("ResultInstant", "Result.Instant", "ResultDate", "result_instant"),
          description = "Date/time of result",
          example = "2023-01-15 10:00:00"
        ),
        ResultStatus = list(
          aliases = c("ResultStatus", "Result.Status", "result_status", "Status"),
          description = "Result status",
          example = "Final"
        ),
        Sex = list(
          aliases = c("Sex", "sex", "Gender", "gender"),
          description = "Patient sex",
          example = "M"
        ),
        BirthYear = list(
          aliases = c("BirthYear", "Birth.Year", "birth_year", "YearOfBirth"),
          description = "Patient birth year",
          example = "1985"
        ),
        AgeInYears = list(
          aliases = c("AgeInYears", "Age.In.Years", "age_in_years", "Age"),
          description = "Patient age in years",
          example = "38"
        ),
        ReferenceValues = list(
          aliases = c("ReferenceValues", "Reference.Values", "reference_values",
                     "RefValues", "NormalRange"),
          description = "Reference range for the lab test",
          example = "4.0-10.0"
        )
      ),
      ignore_extra_columns = TRUE,
      flexible_detection = TRUE,
      transformation_map = NULL,
      processing_notes = c(
        "Lab data in long format (one row per result per patient)",
        "Pivot/transformation to wide format is handled by scripts/lab_data/lab_data.r",
        "Import wizard handles file detection and column mapping only"
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
      file_patterns = c("mdns", "mdn.*map", "id.*map"),
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
                     "ParticipantId", "participantID", "PatientID", "Patient_ID",
                     "studie identificatiecode", "Studie Identificatiecode",
                     "studie_identificatiecode"),
          detect_pattern = "studie.?identificatie|participant.?id|patient.?id",
          description = "Patient identifier",
          example = "0001",
          validation = function(x) {
            char_x <- trimws(as.character(x))
            sum(!is.na(char_x) & nzchar(char_x)) > 0
          }
        )
      ),
      optional_columns = list(),
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
    # MAPPING FILE (ELEMENTS.CSV) - ELEMENT TO CASTOR MAPPING
    # ========================================================================
    mapping_elements = list(
      id = "mapping_elements",
      name = "Mapping File (elements.csv)",
      description = "EPIC element to Castor field mappings",
      icon = "map",
      structure = "mapping_file",
      category = "configuration",
      file_patterns = c("elements", "element.*map", "mapping.*element"),
      required_columns = list(
        Element = list(
          required = TRUE,
          aliases = c("Element", "element", "ELEMENT"),
          description = "EPIC element code",
          example = "PBAIG#043",
          pattern = "^[A-Z]+#\\d+",
          validation = function(x) {
            x_clean <- x[!is.na(x)]
            if (length(x_clean) == 0) return(FALSE)
            any(grepl("^[A-Z]+#\\d+", x_clean))
          }
        ),
        castor_kolom = list(
          required = TRUE,
          aliases = c("castor_kolom", "castor_column", "CastorColumn",
                     "castor_field", "CastorField"),
          description = "Castor field name",
          example = "medi_visit_location",
          validation = function(x) {
            char_x <- trimws(as.character(x))
            sum(!is.na(char_x) & nzchar(char_x)) > 0
          }
        )
      ),
      optional_columns = list(
        tab_name_meta = list(
          aliases = c("tab_name_meta", "tab_name", "TabName"),
          description = "Castor tab name for the field"
        ),
        tab_order_meta = list(
          aliases = c("tab_order_meta", "tab_order", "TabOrder"),
          description = "Castor tab display order"
        )
      ),
      ignore_extra_columns = TRUE,
      flexible_detection = FALSE,
      transformation_map = NULL,
      processing_notes = c(
        "Maps EPIC element codes to Castor field names",
        "Tab metadata (tab_name_meta, tab_order_meta) is optional",
        "Used by the app to resolve element-to-field mappings"
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
  
  # Return as named vector for selectInput: display_name = type_id
  # Shiny uses names as labels and values as returned input
  choices <- sapply(enabled, function(x) x$id)
  names(choices) <- sapply(enabled, function(x) x$name)
  
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
# Includes SharePoint/OneDrive integration and password-protected Excel support
# ============================================================================

# --- SharePoint/OneDrive helpers ---

#' Detect locally synced SharePoint/OneDrive root folders
#'
#' Scans the user's home directory for OneDrive folders synced with an
#' organisation (e.g. "Radboudumc").  Two common path patterns exist:
#'   C:/Users/<user>/Radboudumc/...
#'   C:/Users/<user>/OneDrive - Radboudumc/...
#'
#' @return Named character vector (display_name -> path). Empty if none found.
#' @export
detect_sharepoint_roots <- function() {
  user_home <- normalizePath(Sys.getenv("USERPROFILE"), winslash = "/", mustWork = FALSE)
  if (!nzchar(user_home) || !dir.exists(user_home)) return(character(0))

  children <- list.dirs(user_home, recursive = FALSE, full.names = TRUE)
  children <- normalizePath(children, winslash = "/", mustWork = FALSE)

  # Keep folders that look like OneDrive org-sync roots
  is_onedrive <- grepl("OneDrive|Radboudumc|SharePoint", basename(children), ignore.case = TRUE)
  candidates <- children[is_onedrive]

  if (length(candidates) == 0) return(character(0))

  # Build display names from folder basename
  names(candidates) <- basename(candidates)
  candidates
}

# --- Password-protected Excel via COM automation ---

#' Read a password-protected Excel file using PowerShell COM automation
#'
#' Opens the workbook via the Excel COM object, saves it to a temporary
#' unprotected xlsx, then reads with readxl.  Requires Excel installed.
#'
#' @param file_path Character – Path to the protected .xlsx/.xls file
#' @param password  Character – Workbook password
#' @param ...       Extra arguments forwarded to readxl::read_excel()
#' @return data.frame
#' @export
read_protected_excel <- function(file_path, password, ...) {
  if (!nzchar(password)) stop("Password must not be empty.", call. = FALSE)

  # Copy locally first to avoid OneDrive/SharePoint file locks
  tmp_input <- tempfile(fileext = paste0(".", tools::file_ext(file_path)))
  ok <- tryCatch(file.copy(file_path, tmp_input, overwrite = TRUE), error = function(e) FALSE)
  if (!isTRUE(ok)) {
    tmp_inter <- tempfile(fileext = paste0(".", tools::file_ext(file_path)))
    on.exit(unlink(tmp_inter, force = TRUE), add = TRUE)
    ok2 <- tryCatch(file.copy(file_path, tmp_inter, overwrite = TRUE), error = function(e) FALSE)
    if (isTRUE(ok2)) {
      file.copy(tmp_inter, tmp_input, overwrite = TRUE)
    } else {
      stop(sprintf("Cannot copy protected file (locked by OneDrive sync?): %s", basename(file_path)), call. = FALSE)
    }
  }

  tmp_xlsx <- tempfile(fileext = ".xlsx")
  tmp_ps   <- tempfile(fileext = ".ps1")
  tmp_log  <- tempfile(fileext = ".log")
  on.exit(unlink(c(tmp_xlsx, tmp_ps, tmp_log, tmp_input), force = TRUE), add = TRUE)

  abs_path     <- normalizePath(tmp_input, winslash = "\\")
  abs_tmp_xlsx <- normalizePath(tmp_xlsx, winslash = "\\", mustWork = FALSE)
  abs_tmp_log  <- normalizePath(tmp_log,  winslash = "\\", mustWork = FALSE)
  escaped_pw   <- gsub("'", "''", password)

  ps_lines <- c(
    "$m = [System.Reflection.Missing]::Value",
    "try {",
    "  $xl = New-Object -ComObject Excel.Application",
    "  $xl.Visible = $false",
    "  $xl.DisplayAlerts = $false",
    sprintf("  $wb = $xl.Workbooks.Open('%s', $m, $true, $m, '%s')", abs_path, escaped_pw),
    sprintf("  $wb.SaveAs('%s', 51, '')", abs_tmp_xlsx),
    "  $wb.Close($false)",
    "  $xl.Quit()",
    "  [System.Runtime.Interopservices.Marshal]::ReleaseComObject($wb) | Out-Null",
    "  [System.Runtime.Interopservices.Marshal]::ReleaseComObject($xl) | Out-Null",
    "  [GC]::Collect()",
    "  [GC]::WaitForPendingFinalizers()",
    sprintf("  'OK' | Out-File -FilePath '%s' -Encoding utf8", abs_tmp_log),
    "} catch {",
    sprintf("  $_.Exception.Message | Out-File -FilePath '%s' -Encoding utf8", abs_tmp_log),
    "  try { if ($xl) { $xl.Quit() } } catch {}",
    "  exit 1",
    "}"
  )
  writeLines(ps_lines, tmp_ps)

  # Run with 30-second timeout
  proc <- tryCatch(
    processx::run(
      "powershell",
      args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", tmp_ps),
      timeout = 30,
      error_on_status = FALSE
    ),
    error = function(e) {
      # Kill any lingering Excel spawned by COM
      try(system2("taskkill", c("/F", "/IM", "EXCEL.EXE"), stdout = FALSE, stderr = FALSE), silent = TRUE)
      if (grepl("timeout", conditionMessage(e), ignore.case = TRUE)) {
        stop("Excel COM process timed out after 30 seconds. Is Excel showing a dialog?", call. = FALSE)
      }
      stop(paste("PowerShell error:", conditionMessage(e)), call. = FALSE)
    }
  )

  log_lines <- if (file.exists(tmp_log)) readLines(tmp_log, warn = FALSE, encoding = "UTF-8") else character(0)
  success   <- any(grepl("OK", log_lines, fixed = TRUE))

  if (!success || !file.exists(tmp_xlsx) || file.info(tmp_xlsx)$size == 0) {
    err_msg <- log_lines[!grepl("OK", log_lines, fixed = TRUE)]
    err_msg <- err_msg[nchar(trimws(err_msg)) > 0]
    ps_stderr <- if (is.list(proc) && !is.null(proc$stderr)) trimws(proc$stderr) else ""
    detail <- if (length(err_msg) > 0) {
      paste(" Error:", err_msg[1])
    } else if (nzchar(ps_stderr)) {
      paste(" PowerShell:", substr(ps_stderr, 1, 200))
    } else {
      " (no details available — check if Excel is installed)"
    }
    stop(paste0("Could not open protected Excel file.", detail), call. = FALSE)
  }

  # Keep temp xlsx alive until caller is done; on.exit will clean up
  # Remove it from on.exit so the caller can use the path for multi-sheet reads
  on.exit(NULL)  # clear previous
  on.exit(unlink(c(tmp_ps, tmp_log), force = TRUE), add = TRUE)
  # Return both the data AND the temp path for potential multi-sheet use
  attr_path <- tmp_xlsx
  result <- readxl::read_excel(tmp_xlsx, .name_repair = "minimal", ...)
  attr(result, ".tmp_xlsx") <- attr_path
  result
}

#' Get sheet names from a password-protected Excel file
#'
#' Uses COM automation to open the workbook and write sheet names to a temp file.
#'
#' @param file_path Character – Path to protected file
#' @param password  Character – Workbook password
#' @return Character vector of sheet names
#' @export
get_protected_excel_sheets <- function(file_path, password) {
  if (!nzchar(password)) stop("Password must not be empty.", call. = FALSE)

  # Copy locally first to avoid OneDrive/SharePoint file locks
  tmp_input <- tempfile(fileext = paste0(".", tools::file_ext(file_path)))
  ok <- tryCatch(file.copy(file_path, tmp_input, overwrite = TRUE), error = function(e) FALSE)
  if (!isTRUE(ok)) {
    tmp_inter <- tempfile(fileext = paste0(".", tools::file_ext(file_path)))
    on.exit(unlink(tmp_inter, force = TRUE), add = TRUE)
    ok2 <- tryCatch(file.copy(file_path, tmp_inter, overwrite = TRUE), error = function(e) FALSE)
    if (isTRUE(ok2)) {
      file.copy(tmp_inter, tmp_input, overwrite = TRUE)
    } else {
      stop(sprintf("Cannot copy protected file (locked by OneDrive sync?): %s", basename(file_path)), call. = FALSE)
    }
  }

  tmp_ps  <- tempfile(fileext = ".ps1")
  tmp_out <- tempfile(fileext = ".txt")
  tmp_log <- tempfile(fileext = ".log")
  on.exit(unlink(c(tmp_ps, tmp_out, tmp_log, tmp_input), force = TRUE), add = TRUE)

  abs_path    <- normalizePath(tmp_input, winslash = "\\")
  abs_tmp_out <- normalizePath(tmp_out, winslash = "\\", mustWork = FALSE)
  abs_tmp_log <- normalizePath(tmp_log, winslash = "\\", mustWork = FALSE)
  escaped_pw  <- gsub("'", "''", password)

  ps_lines <- c(
    "$m = [System.Reflection.Missing]::Value",
    "try {",
    "  $xl = New-Object -ComObject Excel.Application",
    "  $xl.Visible = $false",
    "  $xl.DisplayAlerts = $false",
    sprintf("  $wb = $xl.Workbooks.Open('%s', $m, $true, $m, '%s')", abs_path, escaped_pw),
    sprintf("  $wb.Sheets | ForEach-Object { $_.Name } | Out-File -FilePath '%s' -Encoding utf8", abs_tmp_out),
    "  $wb.Close($false)",
    "  $xl.Quit()",
    "  [System.Runtime.Interopservices.Marshal]::ReleaseComObject($wb) | Out-Null",
    "  [System.Runtime.Interopservices.Marshal]::ReleaseComObject($xl) | Out-Null",
    "  [GC]::Collect()",
    "  [GC]::WaitForPendingFinalizers()",
    sprintf("  'OK' | Out-File -FilePath '%s' -Encoding utf8", abs_tmp_log),
    "} catch {",
    sprintf("  $_.Exception.Message | Out-File -FilePath '%s' -Encoding utf8", abs_tmp_log),
    "  try { if ($xl) { $xl.Quit() } } catch {}",
    "  exit 1",
    "}"
  )
  writeLines(ps_lines, tmp_ps)

  proc <- tryCatch(
    processx::run(
      "powershell",
      args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", tmp_ps),
      timeout = 30,
      error_on_status = FALSE
    ),
    error = function(e) {
      try(system2("taskkill", c("/F", "/IM", "EXCEL.EXE"), stdout = FALSE, stderr = FALSE), silent = TRUE)
      if (grepl("timeout", conditionMessage(e), ignore.case = TRUE)) {
        stop("Excel COM process timed out after 30 seconds.", call. = FALSE)
      }
      stop(paste("PowerShell error:", conditionMessage(e)), call. = FALSE)
    }
  )

  log_lines <- if (file.exists(tmp_log)) readLines(tmp_log, warn = FALSE, encoding = "UTF-8") else character(0)
  success   <- any(grepl("OK", log_lines, fixed = TRUE))

  if (!success) {
    err_msg <- log_lines[!grepl("OK", log_lines, fixed = TRUE)]
    err_msg <- err_msg[nchar(trimws(err_msg)) > 0]
    ps_stderr <- if (is.list(proc) && !is.null(proc$stderr)) trimws(proc$stderr) else ""
    detail <- if (length(err_msg) > 0) {
      paste(" Error:", err_msg[1])
    } else if (nzchar(ps_stderr)) {
      paste(" PowerShell:", substr(ps_stderr, 1, 200))
    } else {
      ""
    }
    stop(paste0("Could not read sheets from protected Excel file.", detail), call. = FALSE)
  }

  sheet_lines <- if (file.exists(tmp_out)) readLines(tmp_out, warn = FALSE, encoding = "UTF-8") else character(0)
  sheet_names <- trimws(sheet_lines)
  sheet_names <- sheet_names[nchar(sheet_names) > 0]
  sheet_names
}

# --- File lock retry helper ---

#' Attempt to read a file; on sharing-violation / lock error, copy to temp first
#'
#' @param file_path Character – Path to file
#' @param read_fn   Function – The reader function (e.g. readxl::read_excel)
#' @param ...       Extra args forwarded to read_fn
#' @return Result of read_fn
read_with_lock_retry <- function(file_path, read_fn, ...) {
  result <- tryCatch(read_fn(file_path, ...), error = function(e) e)
  if (!inherits(result, "error")) return(result)

  # Check if it's a lock/sharing-violation error
  msg <- conditionMessage(result)
  is_lock <- grepl("lock|sharing|permission|access|denied|used by another", msg, ignore.case = TRUE)
  if (!is_lock) stop(result)

  # Retry via temp copy
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(file_path)))
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  file.copy(file_path, tmp, overwrite = TRUE)
  read_fn(tmp, ...)
}

#' Decrypt a protected Excel file to a temporary path for multi-use reads
#'
#' Returns the path to a temporary unprotected xlsx.  Caller is responsible
#' for calling unlink() on the returned path when done.
#'
#' @param file_path Character – Path to protected file
#' @param password  Character – Workbook password
#' @return Character – Path to temporary unprotected xlsx
#' @export
decrypt_excel_to_temp <- function(file_path, password) {
  if (!nzchar(password)) stop("Password must not be empty.", call. = FALSE)

  # Copy the source file locally first to avoid OneDrive/SharePoint file locks
  tmp_input <- tempfile(fileext = paste0(".", tools::file_ext(file_path)))
  ok <- tryCatch(file.copy(file_path, tmp_input, overwrite = TRUE), error = function(e) FALSE)
  if (!isTRUE(ok)) {
    # Retry via intermediate temp (lock-aware)
    tmp_inter <- tempfile(fileext = paste0(".", tools::file_ext(file_path)))
    on.exit(unlink(tmp_inter, force = TRUE), add = TRUE)
    ok2 <- tryCatch(file.copy(file_path, tmp_inter, overwrite = TRUE), error = function(e) FALSE)
    if (isTRUE(ok2)) {
      file.copy(tmp_inter, tmp_input, overwrite = TRUE)
    } else {
      stop(sprintf("Cannot copy protected file (locked by OneDrive sync?): %s", basename(file_path)), call. = FALSE)
    }
  }

  tmp_xlsx <- tempfile(fileext = ".xlsx")
  tmp_ps   <- tempfile(fileext = ".ps1")
  tmp_log  <- tempfile(fileext = ".log")
  on.exit(unlink(c(tmp_ps, tmp_log, tmp_input), force = TRUE), add = TRUE)

  abs_path     <- normalizePath(tmp_input, winslash = "\\")
  abs_tmp_xlsx <- normalizePath(tmp_xlsx, winslash = "\\", mustWork = FALSE)
  abs_tmp_log  <- normalizePath(tmp_log,  winslash = "\\", mustWork = FALSE)
  escaped_pw   <- gsub("'", "''", password)

  ps_lines <- c(
    "$m = [System.Reflection.Missing]::Value",
    "try {",
    "  $xl = New-Object -ComObject Excel.Application",
    "  $xl.Visible = $false",
    "  $xl.DisplayAlerts = $false",
    sprintf("  $wb = $xl.Workbooks.Open('%s', $m, $true, $m, '%s')", abs_path, escaped_pw),
    sprintf("  $wb.SaveAs('%s', 51, '')", abs_tmp_xlsx),
    "  $wb.Close($false)",
    "  $xl.Quit()",
    "  [System.Runtime.Interopservices.Marshal]::ReleaseComObject($wb) | Out-Null",
    "  [System.Runtime.Interopservices.Marshal]::ReleaseComObject($xl) | Out-Null",
    "  [GC]::Collect()",
    "  [GC]::WaitForPendingFinalizers()",
    sprintf("  'OK' | Out-File -FilePath '%s' -Encoding utf8", abs_tmp_log),
    "} catch {",
    sprintf("  $_.Exception.Message | Out-File -FilePath '%s' -Encoding utf8", abs_tmp_log),
    "  try { if ($xl) { $xl.Quit() } } catch {}",
    "  exit 1",
    "}"
  )
  writeLines(ps_lines, tmp_ps)

  proc <- tryCatch(
    processx::run(
      "powershell",
      args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", tmp_ps),
      timeout = 30,
      error_on_status = FALSE
    ),
    error = function(e) {
      try(system2("taskkill", c("/F", "/IM", "EXCEL.EXE"), stdout = FALSE, stderr = FALSE), silent = TRUE)
      if (grepl("timeout", conditionMessage(e), ignore.case = TRUE)) {
        stop("Excel COM process timed out after 30 seconds.", call. = FALSE)
      }
      stop(paste("PowerShell error:", conditionMessage(e)), call. = FALSE)
    }
  )

  log_lines <- if (file.exists(tmp_log)) readLines(tmp_log, warn = FALSE, encoding = "UTF-8") else character(0)
  success   <- any(grepl("OK", log_lines, fixed = TRUE))

  if (!success || !file.exists(tmp_xlsx) || file.info(tmp_xlsx)$size == 0) {
    unlink(tmp_xlsx, force = TRUE)
    # Collect all available error info
    err_msg <- log_lines[!grepl("OK", log_lines, fixed = TRUE)]
    err_msg <- err_msg[nchar(trimws(err_msg)) > 0]
    ps_stderr <- if (is.list(proc) && !is.null(proc$stderr)) trimws(proc$stderr) else ""
    detail <- if (length(err_msg) > 0) {
      paste(" Error:", err_msg[1])
    } else if (nzchar(ps_stderr)) {
      paste(" PowerShell:", substr(ps_stderr, 1, 200))
    } else {
      " (no details available — check if Excel is installed)"
    }
    stop(paste0("Could not open protected Excel file.", detail), call. = FALSE)
  }

  tmp_xlsx
}

#' Copy a source file to a session-temp directory
#'
#' For password-protected Excel: decrypts to an unprotected temp copy.
#' For other files: does a lock-aware file.copy.
#'
#' @param source_path Character - Original file path (e.g. SharePoint/OneDrive)
#' @param password    Character - Excel password (NULL for unprotected files)
#' @param session_temp_dir Character - Base temp dir (default: tempdir()/epc_sharepoint/source)
#' @return Character - Path to the local temp copy
copy_to_session_temp <- function(source_path, password = NULL, session_temp_dir = NULL) {
  if (!file.exists(source_path)) {
    stop(sprintf("Source file not found: %s", source_path), call. = FALSE)
  }

  if (is.null(session_temp_dir)) {
    session_temp_dir <- file.path(tempdir(), "epc_sharepoint", "source")
  }
  if (!dir.exists(session_temp_dir)) dir.create(session_temp_dir, recursive = TRUE)

  dest_path <- file.path(session_temp_dir, basename(source_path))

  ext <- tolower(tools::file_ext(source_path))
  use_password <- !is.null(password) && nzchar(password) && ext %in% c("xlsx", "xls")

  if (use_password) {
    # Decrypt to temp, then move to session dir with original filename
    tmp_decrypted <- decrypt_excel_to_temp(source_path, password)
    # Move (rename if same volume, copy+delete otherwise)
    if (file.exists(dest_path)) unlink(dest_path, force = TRUE)
    ok <- file.rename(tmp_decrypted, dest_path)
    if (!ok) {
      file.copy(tmp_decrypted, dest_path, overwrite = TRUE)
      unlink(tmp_decrypted, force = TRUE)
    }
  } else {
    # Lock-aware copy: try direct copy first, fallback via intermediate temp
    ok <- tryCatch({
      file.copy(source_path, dest_path, overwrite = TRUE)
    }, error = function(e) FALSE)

    if (!isTRUE(ok)) {
      msg <- tryCatch(conditionMessage(attr(ok, "condition")), error = function(e) "")
      is_lock <- grepl("lock|sharing|permission|access|denied|used by another", msg, ignore.case = TRUE)
      if (!is_lock && isFALSE(ok)) {
        # file.copy returns FALSE without error on some lock scenarios
        is_lock <- TRUE
      }
      if (is_lock) {
        # Copy via intermediate temp file
        tmp_inter <- tempfile(fileext = paste0(".", tools::file_ext(source_path)))
        on.exit(unlink(tmp_inter, force = TRUE), add = TRUE)
        ok2 <- tryCatch(file.copy(source_path, tmp_inter, overwrite = TRUE), error = function(e) FALSE)
        if (!isTRUE(ok2)) {
          stop(sprintf("Cannot copy file (locked by OneDrive sync?): %s", basename(source_path)), call. = FALSE)
        }
        file.copy(tmp_inter, dest_path, overwrite = TRUE)
      } else {
        stop(sprintf("Failed to copy file: %s", basename(source_path)), call. = FALSE)
      }
    }
  }

  if (!file.exists(dest_path)) {
    stop("Copy failed: destination file does not exist after copy.", call. = FALSE)
  }

  dest_path
}

#' Reload a file from its SharePoint/OneDrive source
#'
#' Re-copies the source file to the session temp directory, overwriting the old copy.
#'
#' @param source_path     Character - Original SharePoint/OneDrive file path
#' @param password        Character - Excel password (NULL for unprotected)
#' @param session_temp_dir Character - Base temp dir (same as used in copy_to_session_temp)
#' @return Character - Path to the refreshed temp copy
reload_from_source <- function(source_path, password = NULL, session_temp_dir = NULL) {
  copy_to_session_temp(source_path, password = password, session_temp_dir = session_temp_dir)
}

#' Detect which row contains the header in an Excel/CSV file
#'
#' Reads the first `max_scan_rows` rows without headers and scores each row
#' by how many values look like known column names from the import configs.
#' Returns the 0-based skip value (number of rows to skip before the header).
#'
#' @param file_path Character - Path to the file
#' @param sheet Character - Sheet name for Excel (NULL for first sheet / CSV)
#' @param file_type Character - "excel" or "csv"
#' @param delimiter Character - CSV delimiter (only used for csv)
#' @param max_scan_rows Integer - How many rows to scan (default 25)
#' @param import_configs List - Import configs (NULL = load default)
#' @return Integer - Number of rows to skip (0 means header is on row 1)
#' @keywords internal
detect_header_row <- function(file_path, sheet = NULL, file_type = "excel",
                              delimiter = ";", max_scan_rows = 25,
                              import_configs = NULL) {
  if (is.null(import_configs)) {
    import_configs <- tryCatch(get_all_import_types(), error = function(e) list())
  }

  # Collect all known column name aliases across all configs
  known_names <- character(0)
  for (cfg in import_configs) {
    for (col_cfg in c(cfg$required_columns, cfg$optional_columns)) {
      known_names <- c(known_names, col_cfg$aliases)
    }
  }
  known_names <- unique(tolower(trimws(known_names)))
  if (length(known_names) == 0) return(0L)

  # Read raw rows without treating any row as header
  raw <- tryCatch({
    if (file_type == "excel") {
      readxl::read_excel(file_path, sheet = sheet, col_names = FALSE,
                         n_max = max_scan_rows, col_types = "text",
                         .name_repair = "minimal")
    } else {
      if (delimiter == ";") {
        readr::read_csv2(file_path, col_names = FALSE, n_max = max_scan_rows,
                         col_types = readr::cols(.default = "c"),
                         show_col_types = FALSE)
      } else {
        readr::read_csv(file_path, col_names = FALSE, n_max = max_scan_rows,
                        col_types = readr::cols(.default = "c"),
                        show_col_types = FALSE)
      }
    }
  }, error = function(e) NULL)

  if (is.null(raw) || nrow(raw) == 0) return(0L)

  # Score each row by how many cells match a known column name
  best_row <- 1L
  best_score <- 0L
  for (i in seq_len(nrow(raw))) {
    vals <- tolower(trimws(as.character(unlist(raw[i, ]))))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    hits <- sum(vals %in% known_names)
    if (hits > best_score) {
      best_score <- hits
      best_row <- i
    }
  }

  # Only return a skip value if the best row had at least 2 matches
  # and is not the first row (which readxl would pick by default)
  if (best_score >= 2 && best_row > 1) {
    return(as.integer(best_row - 1))
  }
  return(0L)
}

#' Detect file type and basic properties
#' 
#' @param file_path Character - Path to file
#' @param password  Character - Excel password (NULL for unprotected files)
#' @return List with file info
#' @export
detect_file_type <- function(file_path, password = NULL) {
  if (!file.exists(file_path)) {
    stop(sprintf("File not found: %s", file_path))
  }
  
  ext <- tolower(tools::file_ext(file_path))
  file_size <- file.info(file_path)$size
  
  if (ext == "csv") {
    csv_reader <- function(p) readLines(p, n = 3, warn = FALSE)
    tryCatch(read_with_lock_retry(file_path, csv_reader), error = function(e) NULL)
    delim <- detect_csv_delimiter(file_path)
    skip <- detect_header_row(file_path, file_type = "csv", delimiter = delim)
    return(list(
      type = "csv",
      extension = ext,
      size = file_size,
      delimiter = delim,
      encoding = detect_encoding(file_path),
      sheets = NULL,
      multi_sheet = FALSE,
      password_protected = FALSE,
      skip_rows = skip
    ))
  }
  
  if (ext %in% c("xlsx", "xls")) {
    # Try reading sheets; detect password-protection on failure
    sheets <- tryCatch(
      read_with_lock_retry(file_path, readxl::excel_sheets),
      error = function(e) e
    )

    if (inherits(sheets, "error")) {
      # If we have a password, try COM automation
      if (!is.null(password) && nzchar(password)) {
        sheets <- get_protected_excel_sheets(file_path, password)
      } else {
        # Likely password-protected — signal to UI to ask for password
        return(list(
          type = "excel",
          extension = ext,
          size = file_size,
          sheets = NULL,
          n_sheets = 0,
          multi_sheet = FALSE,
          sheet_info = list(),
          password_protected = TRUE,
          password_error = conditionMessage(sheets)
        ))
      }
    }

    # Determine the read function for sheet info based on password
    effective_path_for_preview <- file_path
    read_sheet_fn <- if (!is.null(password) && nzchar(password)) {
      # Decrypt once, read multiple sheets from temp
      tmp_path <- decrypt_excel_to_temp(file_path, password)
      effective_path_for_preview <- tmp_path
      function(sheet, skip = 0) {
        tryCatch(readxl::read_excel(tmp_path, sheet = sheet, n_max = 5, skip = skip, col_types = "text", .name_repair = "minimal"), error = function(e) NULL)
      }
    } else {
      function(sheet, skip = 0) {
        tryCatch(
          read_with_lock_retry(file_path, readxl::read_excel, sheet = sheet, n_max = 5, skip = skip, col_types = "text", .name_repair = "minimal"),
          error = function(e) NULL
        )
      }
    }

    # Detect header row using first sheet
    skip <- tryCatch(
      detect_header_row(effective_path_for_preview, sheet = sheets[1], file_type = "excel"),
      error = function(e) 0L
    )

    sheet_info <- lapply(sheets, function(sheet) {
      preview <- read_sheet_fn(sheet, skip = skip)
      if (is.null(preview)) return(list(name = sheet, error = TRUE))
      list(
        name = sheet,
        n_cols = ncol(preview),
        col_names = colnames(preview),
        error = FALSE
      )
    })

    # Clean up decrypted temp if it was created
    if (exists("tmp_path", inherits = FALSE)) {
      on.exit(unlink(tmp_path, force = TRUE), add = TRUE)
    }

    return(list(
      type = "excel",
      extension = ext,
      size = file_size,
      sheets = sheets,
      n_sheets = length(sheets),
      multi_sheet = length(sheets) > 1,
      sheet_info = sheet_info,
      password_protected = !is.null(password) && nzchar(password),
      skip_rows = skip
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
#' @param password Character - Excel password (NULL for unprotected files)
#' @return List with complete detection result
#' @export
detect_file_structure <- function(file_path, sheet = NULL, sheets = NULL, max_preview_rows = 100, import_configs = NULL, password = NULL) {
  file_info <- detect_file_type(file_path, password = password)
  
  # If password-protected and no password provided, return early so UI can ask
  if (isTRUE(file_info$password_protected) && (is.null(password) || !nzchar(password))) {
    return(list(
      file_info = file_info,
      password_required = TRUE
    ))
  }
  
  if (file_info$type == "csv") {
    delimiter <- file_info$delimiter
    skip <- if (!is.null(file_info$skip_rows)) file_info$skip_rows else 0L
    preview_data <- tryCatch({
      csv_reader <- if (delimiter == ";") {
        function(fp, ...) readr::read_csv2(fp, n_max = max_preview_rows, skip = skip,
                        col_types = cols(), show_col_types = FALSE,
                        locale = locale(decimal_mark = ",", grouping_mark = "."))
      } else {
        function(fp, ...) readr::read_csv(fp, n_max = max_preview_rows, skip = skip,
                       col_types = cols(), show_col_types = FALSE,
                       locale = locale(decimal_mark = ".", grouping_mark = ","))
      }
      read_with_lock_retry(file_path, csv_reader)
    }, error = function(e) {
      stop(sprintf("Failed to read CSV: %s", conditionMessage(e)))
    })
  } else if (file_info$type == "excel") {
    # For password-protected Excel, decrypt once and read from temp
    use_password <- !is.null(password) && nzchar(password)
    tmp_decrypted <- NULL
    effective_path <- file_path
    if (use_password) {
      tmp_decrypted <- decrypt_excel_to_temp(file_path, password)
      effective_path <- tmp_decrypted
    }
    on.exit(if (!is.null(tmp_decrypted)) unlink(tmp_decrypted, force = TRUE), add = TRUE)

    selected_sheets <- NULL
    
    if (!is.null(sheets) && length(sheets) > 0) {
      selected_sheets <- sheets
    } else if (!is.null(sheet)) {
      selected_sheets <- c(sheet)
    } else {
      selected_sheets <- c(file_info$sheets[1])
    }
    
    read_excel_safe <- function(fp, ...) {
      read_with_lock_retry(fp, readxl::read_excel, col_types = "text", .name_repair = "minimal", ...)
    }

    skip <- if (!is.null(file_info$skip_rows)) file_info$skip_rows else 0L

    if (length(selected_sheets) == 1) {
      preview_data <- tryCatch({
        read_excel_safe(effective_path, sheet = selected_sheets[1], n_max = max_preview_rows, skip = skip)
      }, error = function(e) {
        stop(sprintf("Failed to read Excel sheet '%s': %s", selected_sheets[1], conditionMessage(e)))
      })
    } else {
      sheet_data_list <- list()
      
      for (sheet_name in selected_sheets) {
        sheet_data <- tryCatch({
          read_excel_safe(effective_path, sheet = sheet_name, n_max = max_preview_rows, skip = skip)
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
#' @param password Character - Excel password (NULL for unprotected files)
#' @return data.frame - Full data
read_full_file <- function(file_path, detection_result, sheet = NULL, sheets = NULL, password = NULL) {
  file_info <- detection_result$file_info
  skip <- if (!is.null(file_info$skip_rows)) file_info$skip_rows else 0L
  
  if (file_info$type == "csv") {
    delimiter <- file_info$delimiter
    
    if (delimiter == ";") {
      csv_reader <- function(fp, ...) readr::read_csv2(fp, skip = skip, col_types = cols(), show_col_types = FALSE,
                              locale = locale(decimal_mark = ",", grouping_mark = "."))
    } else {
      csv_reader <- function(fp, ...) readr::read_csv(fp, skip = skip, col_types = cols(), show_col_types = FALSE,
                             locale = locale(decimal_mark = ".", grouping_mark = ","))
    }
    data <- read_with_lock_retry(file_path, csv_reader)
  } else if (file_info$type == "excel") {
    # For password-protected files, decrypt once and read all sheets from temp
    use_password <- !is.null(password) && nzchar(password)
    tmp_decrypted <- NULL
    effective_path <- file_path
    if (use_password) {
      tmp_decrypted <- decrypt_excel_to_temp(file_path, password)
      effective_path <- tmp_decrypted
    }
    on.exit(if (!is.null(tmp_decrypted)) unlink(tmp_decrypted, force = TRUE), add = TRUE)

    read_excel_safe <- function(fp, ...) {
      read_with_lock_retry(fp, readxl::read_excel, col_types = "text", .name_repair = "minimal", ...)
    }

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
      data <- read_excel_safe(effective_path, sheet = selected_sheets[1], skip = skip)
    } else {
      sheet_data_list <- list()
      
      for (sheet_name in selected_sheets) {
        sheet_data <- tryCatch({
          read_excel_safe(effective_path, sheet = sheet_name, skip = skip)
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
transform_import_data <- function(file_path, detection_result, mapping_df, sheet = NULL, sheets = NULL, password = NULL) {
  
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
    raw_data <- read_full_file(file_path, detection_result, sheet, sheets, password = password)
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
  
  mapped_data <- as.data.frame(result_cols, stringsAsFactors = FALSE, check.names = FALSE)
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
        # Find which rows fail validation
        row_valid <- sapply(col_data, function(x) {
          tryCatch(col_config$validation(x), error = function(e) FALSE)
        })
        bad_rows <- which(!row_valid & !is.na(col_data))
        sample_bad <- head(bad_rows, 5)
        sample_vals <- col_data[sample_bad]
        detail <- if (length(sample_bad) > 0) {
          sprintf(" (rows: %s, values: %s)",
                  paste(sample_bad, collapse = ", "),
                  paste(head(sample_vals, 5), collapse = ", "))
        } else ""
        
        errors[[length(errors) + 1]] <- list(
          type = "validation_failed",
          column = col_name,
          severity = "error",
          message = sprintf("Validation failed for column: %s%s", col_name, detail),
          failed_rows = bad_rows
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
    lab_data = file.path(base_dir, "lab_data"),
    mdns_mapping = file.path(base_dir, "biobank_data"),
    mapping_elements = "mapping"
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
    lab_data = "lab_data.csv",
    mdns_mapping = "MDNS.csv",
    mapping_elements = "elements.csv"
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
  
  # Use config-driven validation: check required columns for any type
  config <- tryCatch(get_import_type_config(import_type), error = function(e) NULL)
  
  if (!is.null(config)) {
    req_cols <- names(config$required_columns)
    actually_required <- character(0)
    for (col_name in req_cols) {
      col_config <- config$required_columns[[col_name]]
      if (is.null(col_config$required) || isTRUE(col_config$required)) {
        actually_required <- c(actually_required, col_name)
      }
    }
    missing <- setdiff(actually_required, names(data))
    if (length(missing) > 0) {
      issues <- c(issues, paste("Missing required columns:", paste(missing, collapse = ", ")))
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

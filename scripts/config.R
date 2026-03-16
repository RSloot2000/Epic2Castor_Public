# Central path configuration for Epic2Castor
# - Loads paths from scripts/paths.json (if present), else uses defaults
# - Exposes epc_path(name) and epc_paths() helpers

epc_load_paths <- function() {
  # Defaults relative to project root
  defaults <- list(
    scripts_dir                      = "scripts",
    baseline_scripts_dir             = "scripts/baseline",
    epic_input_data_dir              = "input_data/epic_export",
  # Biobank data IO (nieuw)
    biobank_input_data_dir           = "input_data/biobank_data",
    biobank_output_data_dir          = "output_data/biobank_data",
  # Follow-up data IO
    follow_up_input_data_dir         = "input_data/follow_up",
    follow_up_output_data_dir        = "output_data/follow_up",
  # Lab data IO
    lab_input_data_dir               = "input_data/lab_data",
    lab_output_data_dir              = "output_data/lab_data",
    baseline_output_data_dir         = "output_data/baseline",
    mapping_dir                      = "mapping",
    mapping_db                       = "db/mapping_data.db",
    castor_meta_dir                  = "castor_meta",
    castor_meta_db                   = "db/castor_meta.db",
    castor_export_dir                = "castor_export",
    logs_dir                         = "logs",
    db_dir                           = "db",
    www_dir                          = "www",
    references_dir                   = "References",
    config_api                       = "config/APIConfig.json",
    # Extra defaults zodat scripts altijd werken ook zonder paths.json
    logger_script                    = "scripts/Logger.r",
    config_script                    = "scripts/config.R",
    import_wizard_script             = "scripts/import_wizard_combined.r",
    batch_upload_helper_script       = "scripts/batch_upload_helper.r",
    castor_field_options_file        = "castor_meta/field_options.csv",
    castor_study_variablelist_file   = "castor_meta/study_variablelist.csv",
    mapping_variabelen_file          = "mapping/variabelen.csv",
    mapping_possible_values_dir      = "mapping/possibleValues",
    castor_datastructure_file        = "castor_meta/Datastructure.json",
    castor_retrieval_script          = "scripts/CastorRetrieval.r",
    # Autofill paths
    autofill_script                  = "scripts/autofill.r",
    medical_terms_dict               = "config/medical_terms.json",
    mapping_radiobuttons_file        = "mapping/waarde_radiobuttons.csv",
    mapping_checkboxes_file          = "mapping/waarde_checkboxes.csv",
    # Dashboard
    dashboard_script                 = "scripts/dashboard.r",
    dashboard_cache_dir              = "db/dashboard_cache"
  )

  cfg_path <- file.path("config", "paths.json")
  paths <- defaults
  if (file.exists(cfg_path)) {
    jp <- tryCatch(jsonlite::fromJSON(cfg_path, simplifyVector = TRUE), error = function(e) NULL)
    if (is.list(jp)) {
      for (nm in names(jp)) paths[[nm]] <- jp[[nm]]
    }
  }
  # Normalize (keep as relative where possible)
  normalize_rel <- function(p) {
    # Don't force absolute; keep existing relative to project root
    # but replace backslashes on Windows for consistency
    gsub("\\\\", "/", p)
  }
  paths <- lapply(paths, normalize_rel)
  options(epic2castor.paths = paths)
  invisible(paths)
}

epc_paths <- function() {
  p <- getOption("epic2castor.paths", NULL)
  if (is.null(p)) p <- epc_load_paths()
  p
}

epc_path <- function(name) {
  p <- epc_paths()
  if (!name %in% names(p)) stop(sprintf("Onbekende pad-sleutel: %s", name))
  path <- p[[name]]
  # When EPC_TEMP_INPUT_DIR is set (SharePoint mode), redirect input_data/* paths to temp
  temp_dir <- Sys.getenv("EPC_TEMP_INPUT_DIR", "")
  if (nzchar(temp_dir) && grepl("^input_data/", path)) {
    temp_path <- file.path(temp_dir, path)
    if (dir.exists(temp_path) || file.exists(temp_path)) {
      # Copy any local files missing in temp so scripts find all required inputs
      if (dir.exists(temp_path) && dir.exists(path)) {
        local_files <- list.files(path, full.names = FALSE)
        temp_files  <- list.files(temp_path, full.names = FALSE)
        missing     <- setdiff(local_files, temp_files)
        for (f in missing) {
          try(file.copy(file.path(path, f), file.path(temp_path, f)), silent = TRUE)
        }
      }
      return(temp_path)
    }
  }
  path
}

# ---------------------------------------------------------------------------
# Column mapping helper: load from elements.csv with optional defaults
# tab_filter   : value to match in tab_name_meta (e.g. "Lab Data", "Biobank")
# default_mapping : named character vector (Epic name = Castor var) used as fallback
# Returns: named character vector Epic -> Castor (elements.csv entries override defaults)
# ---------------------------------------------------------------------------
epc_load_column_mapping <- function(tab_filter, default_mapping = character(0)) {
  mapping <- default_mapping
  elements_path <- tryCatch(
    file.path(epc_path("mapping_dir"), "elements.csv"),
    error = function(e) file.path("mapping", "elements.csv")
  )
  if (file.exists(elements_path)) {
    el <- tryCatch(
      utils::read.csv2(elements_path, stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8-BOM", check.names = TRUE),
      error = function(e) NULL
    )
    if (!is.null(el) && "Element" %in% names(el) && "castor_kolom" %in% names(el)
        && "tab_name_meta" %in% names(el)) {
      el$Element      <- trimws(el$Element)
      el$castor_kolom <- trimws(el$castor_kolom)
      el$tab_name_meta <- trimws(el$tab_name_meta)
      rows <- el[tolower(el$tab_name_meta) == tolower(tab_filter) &
                   nzchar(el$Element) & nzchar(el$castor_kolom), , drop = FALSE]
      if (nrow(rows) > 0) {
        override <- setNames(rows$castor_kolom, rows$Element)
        # Merge: override wins for matching keys, add new keys from override
        for (nm in names(override)) {
          mapping[nm] <- override[nm]
        }
      }
    }
  }
  mapping
}

# Auto-load on source
try(epc_load_paths(), silent = TRUE)

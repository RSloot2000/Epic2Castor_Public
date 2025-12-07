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
    mapping_checkboxes_file          = "mapping/waarde_checkboxes.csv"
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
  p[[name]]
}

# Auto-load on source
try(epc_load_paths(), silent = TRUE)

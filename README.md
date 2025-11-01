# Epic2Castor

Convert Epic SmartForm exports into Castor EDC import files. The toolkit offers an ETL pipeline and a Shiny UI to manage mappings, retrieve Castor metadata, transform data, and generate Castor-ready CSV and JSON payloads.

# Team
Supervisor:
  - [A. van Laarhoven](https://github.com/ArjanvL)

Developers:
  - [R. Sloot](https://github.com/RSloot2000)
  - [W. Peeters](https://github.com/wouterpeeters)
  - [R. Elbers](https://github.com/relbersradboudumc)
  - V. Aukes

Funding:
  - [ZonMw](https://www.zonmw.nl/en), a clinical fellowship to A. van Laarhoven (09032212110006)

---

## Table of contents

- Overview
- Features
- Project structure
- Installation
- Configuration
- Usage
  - Run the Shiny app
  - Run the ETL (baseline.r)
  - Generate Castor upload payloads
- Data flow
- Logging & status tracking
- Troubleshooting
- Contributing
- License

---

## Overview

Epic2Castor combines mapping definitions, Castor metadata, and Epic source files into Castor import datasets. It ships a Shiny mapping editor, reproducible transformation scripts, and optional Castor upload helpers for baseline and biobank datasets.

---

## Features

- **Shiny mapping editor**
  - Select, edit, filter, and validate mapping CSVs.
  - Dropdowns resolve valid values via `option_lists2.R`.
  - Integrated credential editor and task runner.

- **Automated Castor metadata retrieval**
  - OAuth2 flow managed by [`scripts/CastorRetrieval.r`](scripts/CastorRetrieval.r).
  - Cached CSV/SQLite copies in [`castor_meta`](castor_meta) and [`db`](db).

- **ETL pipeline**
  - Reads Epic exports (`.xlsx`/`.csv`), applies mappings, normalizes dates, and resolves radio/checkbox values.
  - Produces Castor-ready CSVs in [`output_data`](output_data).

- **Upload helpers**
  - Baseline: [`scripts/baseline/baselineExport.r`](scripts/baseline/baselineExport.r).
  - Biobank: [`scripts/biobank_data/biobankExport.r`](scripts/biobank_data/biobankExport.r).
  - Support streaming JSON payload creation and Castor API submission.

- **Structured logging and progress reporting**
  - Central logger ([`scripts/Logger.r`](scripts/Logger.r)) creates per-run directories in [`logs`](logs).
  - Status JSON enables real-time UI progress bars.

---

## Project structure

```
.
├── App.r                               # Main Shiny app for mapping management, ETL, uploads
├── app.R                               # Legacy/alternative Shiny entry point
├── main.R                              # Legacy main script (deprecated)
├── README.md
├── .gitignore                          # Git ignore rules (excludes config/APIConfig.json!)
├── .Rhistory
├── .lintr
├── VictorAukes_EpicToCastor.Rproj     # RStudio project file
├── .github/                            # GitHub workflows and actions
├── .Rproj.user/                        # RStudio user settings
├── config/                             # 🔐 Configuration files (add to .gitignore!)
│   ├── paths.json                      # Path configuration & overrides
│   ├── APIConfig.json                  # Castor API credentials & DeepL key (NEVER COMMIT!)
│   └── medical_terms.json              # Medical terminology dictionary (EN→NL)
├── scripts/
│   ├── config.R                        # Path helpers (epc_path, epc_paths)
│   ├── Logger.r                        # Run directory + structured logging
│   ├── CastorRetrieval.r               # Fetch Castor metadata via API
│   ├── autofill.r                      # Auto-fill EPIC values from Castor
│   ├── database.r                      # CSV ⇄ SQLite loaders
│   ├── option_lists2.R                 # Dropdown option generation
│   ├── export_approved.r               # Export approved autofill suggestions
│   ├── batch_upload_helper.r           # Batch upload utilities
│   ├── baseline/
│   │   ├── baseline.r                  # Baseline ETL orchestrator
│   │   └── baselineExport.r            # Baseline Castor upload helper
│   └── biobank_data/
│       ├── biobank_data.r              # MDN→Participant mapping + CSV writer
│       └── biobankExport.r             # Biobank Castor upload helper
├── mapping/
│   ├── elements.csv                    # EPIC→Castor element mapping
│   ├── variabelen.csv                  # Variable name mapping
│   ├── waarde_checkboxes.csv           # Checkbox value mapping
│   ├── waarde_radiobuttons.csv         # Radio button value mapping
│   └── possibleValues/
│       └── pv_elements.csv             # Possible values per element
├── castor_meta/                        # Castor study metadata (from API)
│   ├── field_options.csv               # All field options
│   ├── study_variablelist.csv          # Study variable list
│   ├── Datastructure.json              # Complete study structure cache
│   └── .castor_retrieval_done          # Flag file for metadata retrieval
├── castor_export/                      # Generated JSON payloads for Castor API
│   ├── baseline.json                   # Baseline upload payload
│   └── biobank.json                    # Biobank upload payload
├── input_data/
│   ├── epic_export/                    # EPIC baseline & follow-up exports
│   │   └── EpicExport.csv
│   └── biobank_data/                   # Biobank CSV inputs
│       ├── biobank_data.csv
│       └── MDNS.csv
├── output_data/
│   ├── baseline/                       # Processed baseline data
│   │   └── baseline.csv
│   └── biobank_data/                   # Processed biobank data
│       └── biobank.csv
├── db/                                 # SQLite databases (mapping & metadata)
│   ├── mapping_data.db
│   └── castor_meta.db
├── logs/                               # Timestamped run directories with detailed logs
├── Backup/                             # Backup files and legacy code
│   ├── App_backup.r
│   ├── appCSS.css
│   ├── appJS.js
│   ├── baseline.r
│   ├── baselineExport.r
│   ├── baselineRetrieval.r
│   ├── basic app backup.R
│   ├── biobank_data.r
│   ├── config.R
│   ├── database.r
│   ├── Logger.r
│   ├── option_lists2.R
│   ├── paths.json
│   └── README.md
├── funtions/                           # Legacy helpers (deprecated)
├── www/                                # Static assets for Shiny UI
│   ├── appCSS.css
│   ├── appJS.js
│   └── img/
│       └── logo.png
└── References/                         # Reference files & documentation
```

---

## Installation

Prerequisites:

- R ≥ 4.1
- Internet access for Castor API calls (unless cached metadata is provided)

Install required packages:

```r
install.packages(c(
  "shiny", "data.table", "DT", "shinyjs", "shinydashboard",
  "readxl", "readr", "processx", "jsonlite", "httr",
  "DBI", "RSQLite", "digest", "uuid", "stringdist", "later"
))
```

Scripts invoked via `Rscript` auto-create writable user libraries and install missing packages when necessary.

---

## Configuration

### Path Configuration

Central paths are managed via [`scripts/config.R`](scripts/config.R) which provides `epc_path(name)` and `epc_paths()` helpers. Default paths match the structure above. 

**Override paths** via [`config/paths.json`](config/paths.json):

```json
{
  "scripts_dir": "scripts",
  "baseline_scripts_dir": "scripts/baseline",
  "epic_input_data_dir": "input_data/epic_export",
  "biobank_input_data_dir": "input_data/biobank_data",
  "biobank_output_data_dir": "output_data/biobank_data",
  "baseline_output_data_dir": "output_data/baseline",
  "mapping_dir": "mapping",
  "mapping_db": "db/mapping_data.db",
  "castor_meta_dir": "castor_meta",
  "castor_meta_db": "db/castor_meta.db",
  "castor_export_dir": "castor_export",
  "logs_dir": "logs",
  "db_dir": "db",
  "www_dir": "www",
  "references_dir": "References",
  "config_api": "config/APIConfig.json",
  "castor_field_options_file": "castor_meta/field_options.csv",
  "castor_study_variablelist_file": "castor_meta/study_variablelist.csv",
  "mapping_variabelen_file": "mapping/variabelen.csv",
  "mapping_possible_values_dir": "mapping/possibleValues",
  "logger_script": "scripts/Logger.r",
  "config_script": "scripts/config.R",
  "batch_upload_helper_script": "scripts/batch_upload_helper.r",
  "castor_datastructure_file": "castor_meta/Datastructure.json",
  "castor_retrieval_script": "scripts/CastorRetrieval.r",
  "autofill_script": "scripts/autofill.r",
  "medical_terms_dict": "config/medical_terms.json",
  "mapping_radiobuttons_file": "mapping/waarde_radiobuttons.csv",
  "mapping_checkboxes_file": "mapping/waarde_checkboxes.csv"
}
```

### API Configuration

**Castor API credentials** are read from [`config/APIConfig.json`](config/APIConfig.json):

```json
{
  "client_id": "YOUR-CLIENT-ID",
  "client_secret": "YOUR-CLIENT-SECRET",
  "study_id": "YOUR-STUDY-ID",
  "deepl_api_key": "YOUR-DEEPL-KEY-OPTIONAL"
}
```

⚠️ **IMPORTANT**: This file contains sensitive credentials and is excluded from version control via [`.gitignore`](.gitignore). Never commit actual credentials!

### Medical Terms Dictionary

The autofill module uses [`config/medical_terms.json`](config/medical_terms.json) for English→Dutch medical term translations. Customize this file to match your study's terminology.

---

Security tip: keep real credentials out of version control by adding the file to `.gitignore`.

Environment variables:

- `EPIC2CASTOR_LOGDIR`: custom log directory (default: `logs/<timestamp>`).
- `EPIC2CASTOR_STATUS_FILE`: override status JSON path for UI polling.
- `EPIC2CASTOR_AUTOINIT`: set to `0` to disable automatic logger initialization.

---

## Usage

### Run the Shiny app

```r
shiny::runApp(".")
# or open App.r in RStudio and click "Run App"
```

Capabilities:

- Edit mapping CSVs with validation, dropdown options, undo/backups.
- Update Castor API credentials via the UI.
- Trigger ETL runs ([`scripts/baseline/baseline.r`](scripts/baseline/baseline.r)) and upload helpers (baseline/biobank).
- Monitor logs and progress directly in the UI with real-time status updates.
- Autofill EPIC values using intelligent matching against Castor metadata.

### Run the ETL (baseline.r)

```r
source("scripts/baseline/baseline.r")
# or
Rscript scripts/baseline/baseline.r
```

Outputs Castor-ready CSVs under [`output_data/baseline`](output_data/baseline) per mapping definition ([`mapping/variabelen.csv`](mapping/variabelen.csv)).

### Generate Castor upload payloads

After ETL output:

```r
# Baseline JSON + API upload
Rscript scripts/baseline/baselineExport.r "<site_id> - <site_name>"

# Biobank JSON + API upload
Rscript scripts/biobank_data/biobankExport.r "<site_id> - <site_name>"
```

Each export script:

1. Reads staged CSVs from the corresponding `output_data` subfolder.
2. Streams JSON payloads into [`castor_export`](castor_export).
3. Ensures participants, repeating data, and datapoints via the Castor API.
4. Records a detailed [`Datastructure.json`](castor_meta/Datastructure.json) snapshot for auditing.

---

## Data flow

[`scripts/baseline/baseline.r`](scripts/baseline/baseline.r) orchestrates the ETL:

1. Initialize logging/status and load paths via [`scripts/config.R`](scripts/config.R).
2. Rebuild SQLite caches ([`db/*.db`](db/)) when source CSV hashes change.
3. Retrieve or read Castor metadata (option groups, study variable list).
4. For each mapping row in [`mapping/variabelen.csv`](mapping/variabelen.csv):
   - Load Epic source file (Excel or CSV) from [`input_data/epic_export`](input_data/epic_export).
   - Normalize text encodings, dates (`datum`), and durations (weeks conversion).
   - Rename Epic columns to Castor variables via [`mapping/variabelen.csv`](mapping/variabelen.csv).
   - Resolve checkbox and radio options using [`mapping/waarde_checkboxes.csv`](mapping/waarde_checkboxes.csv) and [`mapping/waarde_radiobuttons.csv`](mapping/waarde_radiobuttons.csv).
   - Pivot long-to-wide and consolidate checkbox selections.
   - Apply fixed values, repeating data labels, and creation timestamps.
   - Write Castor-ready CSVs separated by table under [`output_data`](output_data).

Optional upload scripts ([`baselineExport.r`](scripts/baseline/baselineExport.r), [`biobankExport.r`](scripts/biobank_data/biobankExport.r)) reuse these outputs to stream JSON payloads and interact with the Castor API.

---

## Logging & status tracking

- [`scripts/Logger.r`](scripts/Logger.r) creates run directories (`logs/<timestamp>`), captures stdout/stderr into `<Script>_log.txt`, and can be shared across processes (Shiny app + Rscript).
- Status helpers (`epic2castor_status_*`) write a compact JSON file (default: `logs/<run>/status.json`) polled by the UI progress bars.
- All scripts support structured logging with automatic run directory creation and log file management.

---

## Troubleshooting

- **Package installation issues**: ensure the R user library is writable; scripts auto-create `R/win-library/<version>` when needed.
- **Castor API 401/403**: verify credentials in [`config/APIConfig.json`](config/APIConfig.json) and client permissions for the study.
- **Missing input files**: filenames must match `epic_tabel` values in [`mapping/variabelen.csv`](mapping/variabelen.csv); expected locations under [`input_data`](input_data).
- **Unexpected option values**: refresh Castor metadata ([`CastorRetrieval.r`](scripts/CastorRetrieval.r)) and validate mapping CSV entries.
- **Upload failures**: inspect JSON payloads in [`castor_export`](castor_export) and [`Datastructure.json`](castor_meta/Datastructure.json) for server responses.
- **Configuration not found**: ensure [`config/APIConfig.json`](config/APIConfig.json) exists and contains valid credentials. Legacy location `scripts/APIConfig.json` is deprecated.
- **Autofill issues**: check [`config/medical_terms.json`](config/medical_terms.json) for missing translations and review confidence thresholds in [`scripts/autofill.r`](scripts/autofill.r).

---

## Contributing

Contributions are welcome. Omit real credentials or patient data from commits. **Important**: [`config/APIConfig.json`](config/APIConfig.json) is automatically excluded via [`.gitignore`](.gitignore) to protect sensitive credentials.

When contributing:

1. Test changes locally with the Shiny app.
2. Ensure all scripts run without errors via `Rscript`.
3. Update documentation for new features.
4. Never commit files containing real API credentials or patient data.

---

## License

If [`LICENSE.md`](LICENSE.md) exists, its terms apply; otherwise, all rights remain with the repository owners.
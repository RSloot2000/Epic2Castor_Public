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
- Quick Start Guide
- Configuration
- Usage
  - Run the Shiny app
  - Mapping workflow
  - Auto-fill EPIC values
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
  - Tabbed interface for Elements, Checkboxes, Radiobuttons, and Variables.
  - Dropdowns resolve valid values via `option_lists2.R`.
  - Real-time validation with inline feedback.
  - Integrated credential editor and task runner.
  - File manager with upload validation and automatic option refresh.
  - Copy/paste functionality for elements with automatic propagation to related tables.

- **Automated Castor metadata retrieval**
  - OAuth2 flow managed by [`scripts/CastorRetrieval.r`](scripts/CastorRetrieval.r).
  - Cached CSV/SQLite copies in [`castor_meta`](castor_meta) and [`db`](db).
  - In-app refresh via **'Castor' → 'Refresh metadata'** menu.
  - Automatic hash-based change detection to avoid unnecessary rebuilds.

- **Intelligent auto-fill system**
  - 7 matching strategies with confidence scoring (70-100%).
  - Medical terminology dictionary (English→Dutch).
  - DeepL API integration for high-quality translations.
  - Reference dictionary built from approved mappings.
  - Elimination logic for process-of-elimination scenarios.
  - Interactive preview with filtering and bulk approval.
  - Export suggestions to CSV for review.

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
├── EpicToCastor.Rproj                 # RStudio project file
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

## Quick Start Guide

Follow these steps to get Epic2Castor up and running:

### Step 1: Install RStudio

1. **Download and install R** (if not already installed):
   - Visit [https://cran.r-project.org/](https://cran.r-project.org/)
   - Download R ≥ 4.1 for your operating system
   - Run the installer and follow the instructions

2. **Download and install RStudio** (recommended):
   - Visit [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
   - Download the free RStudio Desktop version
   - Run the installer and follow the instructions

### Step 2: Open the Project

1. **Navigate to the project folder** (e.g., `Epic2Castor_Public`)
2. **Double-click** the `EpicToCastor.Rproj` file
3. This will open the project in RStudio with the correct working directory

### Step 3: First Launch

1. **Open the main app file** in RStudio:
   - In the **Files** pane (bottom-right), click on `App.r`
   - Or use the file menu: **File** → **Open File** → select `App.r`

2. **Run the app**:
   - Click the **"Run App"** button at the top-right of the editor pane
   - Or press **Ctrl+Shift+Enter** (Windows/Linux) / **Cmd+Shift+Return** (Mac)
   - Or run in the R console: `shiny::runApp(".")`

3. The app will automatically:
   - Create required directories (`config/`, `db/`, `castor_meta/`, etc.)
   - Generate placeholder files for mappings
   - Create a template `APIConfig.json` file
   
4. You'll see a **warning notification** about missing API credentials - this is expected!

**Note**: Required R packages will be automatically installed on first run. This may take a few minutes.

### Step 4: Configure Credentials

1. Click the **'Castor' menu** at the top of the app
2. Select **'Update credentials'**
3. Fill in the credential form:
   - **Client ID**: From Castor EDC Settings → API
   - **Client Secret**: Generate one in Castor EDC Settings → API
   - **Study ID**: Found in your study's URL (e.g., `ABC123DEF456`)
   - **DeepL API Key** (optional): For improved medical term translations
4. Click **'Save'**
5. The app will confirm credentials are saved successfully

**Where to find Castor credentials:**
- Log in to [Castor EDC](https://data.castoredc.com)
- Navigate to your study
- Go to **Settings** → **API**
- Copy the **Client ID** and generate a **Client Secret**
- The **Study ID** is visible in your browser's URL bar

### Step 5: Refresh Castor Metadata

1. Click **'Castor'** → **'Refresh metadata'**
2. Wait for the progress dialog (this may take 30-60 seconds)
3. The app will download:
   - Field options (all dropdown/radio/checkbox values)
   - Study variable list (all fields in your study)
   - Generate possible values for mappings
4. A success notification will appear when complete

**This step is crucial** - it populates the dropdown options you'll need for mapping!

### Step 6: Load Input File

1. Navigate to the **'Elements' tab** in the mapping section
2. Click the **file browser icon** (📁) at the top to open the file manager
3. In the modal dialog:
   - Select **'Epic Export'** as file type
   - Click **'Browse'** and select your Epic SmartForm export file (`.csv` or `.xlsx`)
   - The app validates the file automatically
   - Click **'Select & Close'** to load the file
4. The dropdown options in your mapping tables will now be populated with values from your Epic export

**You're now ready to create mappings!**

### Step 7: Create Your First Mapping

1. In the **'Elements' tab**, click **'+ Add Row'**
2. Fill in the columns using dropdowns:
   - **Element**: Select an Epic element from your export
   - **Castor Name**: Select the corresponding Castor field
3. The app automatically:
   - Creates matching rows in the **Checkboxes** and **Radiobuttons** tabs (if applicable)
   - Populates the **Castor Name** column in these tables
   - Provides dropdown options for EPIC values

4. Switch to **Checkboxes** or **Radiobuttons** tabs to map individual values:
   - Each Castor option is pre-filled
   - Select the matching **EPIC Value** from the dropdown
   - Use **Auto-fill** (🪄 button) for intelligent suggestions

5. Click **'Save'** to persist your mappings to the database

---

## Configuration

### First Time Setup

When you run the app for the first time, it will automatically:

1. **Create required directories** (`config/`, `db/`, `castor_meta/`, etc.)
2. **Generate a template `APIConfig.json`** with empty credential fields
3. **Create placeholder metadata files** that will be populated once credentials are configured

You'll see console messages like:
```
[Init] Checking required directories and files...
[Init] Created APIConfig.json template at: config/APIConfig.json
[Init] Please configure your Castor API credentials in this file.
[Init] Initialization complete.
[Startup] API credentials not configured yet; using existing Castor metadata files.
```

The app will start with limited functionality until you configure the API credentials (see next section).

### API Configuration

**Castor API credentials** are read from [`config/APIConfig.json`](config/APIConfig.json).

#### Method 1: Using the App UI (Recommended)

The easiest way to configure credentials is through the app interface:

1. **Start the app** - it will show a warning notification about missing credentials
2. **Click the 'Castor' menu** at the top of the app
3. **Select 'Update credentials'**
4. **Fill in your credentials** in the modal dialog:
   - Client ID
   - Client Secret
   - Study ID
   - DeepL API Key (optional, for better translations)
5. **Click 'Save'**
6. **Refresh metadata**: Click 'Castor' → 'Refresh metadata'
7. **Done!** Your app is now fully configured

The app includes helpful tooltips (click the ⓘ icons) explaining where to find each credential.

#### Method 2: Manual Configuration

Alternatively, you can edit the configuration file directly:

1. After the first run, open [`config/APIConfig.json`](config/APIConfig.json)
2. Add your credentials:

```json
{
  "client_id": "YOUR-CLIENT-ID",
  "client_secret": "YOUR-CLIENT-SECRET",
  "study_id": "YOUR-STUDY-ID",
  "deepl_api_key": "YOUR-DEEPL-KEY-OPTIONAL"
}
```

3. Restart the app to apply changes

**Where to find your credentials:**

1. Log in to [Castor EDC](https://data.castoredc.com)
2. Navigate to your study
3. Go to **Settings** → **API**
4. Copy the **Client ID** and generate a **Client Secret**
5. The **Study ID** is in your study's URL

⚠️ **IMPORTANT**: This file contains sensitive credentials and is excluded from version control via [`.gitignore`](.gitignore). Never commit actual credentials!

#### Refreshing Metadata

After configuring credentials:
- Use **'Castor' → 'Refresh metadata'** in the app to update study data without restarting
- The app will show a progress dialog and notify you when the refresh is complete
- Metadata is automatically cached and refreshed only when needed

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

### Medical Terms Dictionary

The autofill module uses [`config/medical_terms.json`](config/medical_terms.json) for English→Dutch medical term translations. Customize this file to match your study's terminology.

---

## Usage

### Run the Shiny app

```r
shiny::runApp(".")
# or open App.r in RStudio and click "Run App"
```

The Shiny app is the primary interface for Epic2Castor, providing:

- **Mapping editor** with tabbed interface (Elements, Checkboxes, Radiobuttons, Variables)
- **Real-time validation** of column values and file structure
- **Dropdown options** automatically populated from Epic exports and Castor metadata
- **Auto-fill functionality** for intelligent EPIC value suggestions
- **Credential management** via integrated UI
- **ETL execution** and upload helpers with progress monitoring
- **File management** with validation and upload capabilities

### Mapping workflow

#### Working with the Elements Table

The **Elements** tab is the foundation of your mappings:

1. **Add elements**: Click **'+ Add Row'** to create new mappings
2. **Select values**: Use dropdowns to choose Epic elements and Castor fields
3. **Automatic propagation**: When you add an element with a checkbox/radiobutton Castor field:
   - Matching rows are automatically created in the Checkboxes or Radiobuttons tab
   - The **Castor Name** column is pre-filled
   - Castor option values are pre-populated

4. **Tab organization**: Elements can be organized across multiple tabs for complex studies
5. **Copy/Paste**: Select rows and use the copy (📋) and paste (📄) buttons to duplicate mappings

#### Working with Checkboxes and Radiobuttons Tables

After creating elements with checkbox/radiobutton fields:

1. **Switch to the appropriate tab** (Checkboxes or Radiobuttons)
2. **Review auto-generated rows**: Each Castor option appears as a separate row
3. **Map EPIC values**: Select the matching Epic value from the **EPIC Value** dropdown
4. **Use Auto-fill**: Click the magic wand (🪄) button for intelligent suggestions (see below)

#### File Management

The file browser (📁 icon) provides:

- **Upload validation**: Real-time column checking before upload
- **Multi-source support**: Epic exports, biobank data, follow-up files
- **File selection**: Choose existing files or upload new ones
- **Automatic option reload**: After uploading an Epic export, dropdown options refresh automatically

### Auto-fill EPIC values

The **Auto-fill** feature (available in Checkboxes and Radiobuttons tabs) uses intelligent matching to suggest EPIC values for Castor options:

#### How to use Auto-fill

1. Ensure you have:
   - Created elements with Castor checkboxes/radiobuttons
   - Loaded an Epic export file
   - At least some empty EPIC values to fill

2. Click the **magic wand button (🪄)** in the Checkboxes or Radiobuttons tab

3. Configure options in the modal:
   - **Tab**: Select which tab to process (or "All")
   - **Review existing**: Check to re-process already filled values
   - **Element filter**: Focus on specific elements

4. Review suggestions:
   - **High confidence** (≥95%): Green highlighting
   - **Medium confidence** (≥85%): Yellow highlighting
   - **Low confidence** (≥70%): Orange highlighting
   - Check/uncheck individual suggestions
   - Use filters to show only specific confidence levels or elements

5. Click **'Apply Selected'** to fill the values

#### Matching strategies

Auto-fill uses 7 intelligent strategies (in order of precedence):

1. **Exact match**: EPIC value exactly matches Castor value
2. **Reference dictionary**: Previously approved mappings
3. **Elimination**: Process of elimination when only one option remains
4. **Medical terms**: Dictionary-based medical terminology matching
5. **DeepL translation**: API-based English→Dutch translation (if configured)
6. **Normalized match**: Case-insensitive, punctuation-normalized matching
7. **Partial match**: Substring and fuzzy string matching

#### Best practices

- Fill high-confidence suggestions first (green items)
- Review medium-confidence suggestions carefully (yellow items)
- Manually check low-confidence suggestions (orange items)
- Use **'Export CSV'** to save suggestions for review
- Approved auto-fills build a reference dictionary for future runs

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
- **Unexpected option values**: refresh Castor metadata via **'Castor' → 'Refresh metadata'** in the app UI and validate mapping CSV entries.
- **Upload failures**: inspect JSON payloads in [`castor_export`](castor_export) and [`Datastructure.json`](castor_meta/Datastructure.json) for server responses.
- **Configuration not found**: ensure [`config/APIConfig.json`](config/APIConfig.json) exists and contains valid credentials. Use the app's **'Castor' → 'Update credentials'** menu for easy configuration.
- **Autofill issues**: check [`config/medical_terms.json`](config/medical_terms.json) for missing translations and review confidence thresholds in [`scripts/autofill.r`](scripts/autofill.r).
- **Empty dropdowns after file upload**: Ensure you've completed the Quick Start Guide steps 1-4 (credentials → refresh metadata → load file).
- **Checkboxes/Radiobuttons not auto-populating**: Make sure the Castor field type is correctly set as checkbox/radiobutton in Castor EDC. Refresh metadata to update field types.
- **Hash file errors**: The app uses MD5 hashing to detect changes. If you see unexpected rebuild messages, check that CSV files aren't being modified by external processes.
- **Locale warnings**: The app uses Dutch locale settings (`,` for decimals, `.` for thousands). If you see locale warnings, ensure your CSV files match this format.

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
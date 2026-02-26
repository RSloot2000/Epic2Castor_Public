# Epic2Castor

Convert Epic SmartForm exports into Castor EDC import files. The toolkit offers an ETL pipeline and a Shiny UI to manage mappings, retrieve Castor metadata, transform data, and generate Castor-ready CSV and JSON payloads.

## Team

**Supervisor:**
- [A. van Laarhoven](https://github.com/ArjanvL)

**Developers:**

*Lead:*
- [R. Sloot](https://github.com/RSloot2000)

*Other developers/advisors:*
- [W. Peeters](https://github.com/wouterpeeters)
- [R. Elbers](https://github.com/relbersradboudumc)
- V. Aukes

**Funding:**
- [ZonMw](https://www.zonmw.nl/en), a clinical fellowship to A. van Laarhoven (09032212110006)

---

## Table of contents

- [Overview](#overview)
- [Features](#features)
- [Project structure](#project-structure)
- [Quick Start Guide](#quick-start-guide)
  - [Prerequisites](#prerequisites)
  - [Step 1: Install R, Rtools and RStudio](#step-1-install-r-rtools-and-rstudio)
  - [Step 2: Open the Project](#step-2-open-the-project)
  - [Step 3: Install Required R Packages](#step-3-install-required-r-packages)
  - [Step 4: First Launch](#step-4-first-launch)
  - [Step 5: Configure Credentials](#step-5-configure-credentials)
  - [Step 6: Refresh Castor Metadata](#step-6-refresh-castor-metadata)
  - [Step 7: Load Input File](#step-7-load-input-file)
  - [Step 8: Create Your First Mapping](#step-8-create-your-first-mapping)
- [Advanced Installation](#advanced-installation)
  - [For Radboudumc Managed Workstations](#for-radboudumc-managed-workstations-app-v--werkplek-20)
- [Configuration](#configuration)
  - [First Time Setup](#first-time-setup)
  - [API Configuration](#api-configuration)
  - [Path Configuration](#path-configuration)
  - [Medical Terms Dictionary](#medical-terms-dictionary)
  - [ML Model Configuration](#ml-model-configuration-optional)
- [User Guide](#user-guide)
  - [1. Starting the App](#1-starting-the-app)
  - [2. Configuring Credentials](#2-configuring-credentials)
  - [3. Refreshing Castor Metadata](#3-refreshing-castor-metadata)
  - [4. Loading Input Files](#4-loading-input-files)
  - [5. Creating Mappings](#5-creating-mappings)
  - [6. Auto-fill EPIC Values](#6-auto-fill-epic-values)
  - [7. Running the ETL Pipeline](#7-running-the-etl-pipeline)
  - [8. Exporting to Castor](#8-exporting-to-castor)
  - [9. Monitoring with the Study Dashboard](#9-monitoring-with-the-study-dashboard)
  - [Keyboard Shortcuts](#keyboard-shortcuts)
- [Troubleshooting](#troubleshooting)
- [Data flow](#data-flow)
- [Logging & status tracking](#logging--status-tracking)
- [Contributing](#contributing)
- [License](#license)

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
  - **Optional ML-based predictions** (8th strategy) using XGBoost model trained on historical mappings.
  - Medical terminology dictionary (English→Dutch).
  - DeepL API integration for high-quality translations.
  - Reference dictionary built from approved mappings.
  - Elimination logic for process-of-elimination scenarios.
  - Interactive preview with filtering and bulk approval.
  - Export suggestions to CSV for review.
  - Automatic model retraining as new mappings are approved.
  - Performance monitoring and prediction logging.

- **ETL pipeline**
  - Reads Epic exports (`.xlsx`/`.csv`), applies mappings, normalizes dates, and resolves radio/checkbox values.
  - Produces Castor-ready CSVs in [`output_data`](output_data).

- **Upload helpers**
  - Baseline: [`scripts/baseline/baselineExport.r`](scripts/baseline/baselineExport.r).
  - Biobank: [`scripts/biobank_data/biobankExport.r`](scripts/biobank_data/biobankExport.r).
  - Follow-up: [`scripts/follow_up/follow_upExport.r`](scripts/follow_up/follow_upExport.r).
  - Support streaming JSON payload creation and Castor API submission.

- **Structured logging and progress reporting**
  - Central logger ([`scripts/Logger.r`](scripts/Logger.r)) creates per-run directories in [`logs`](logs).
  - Status JSON enables real-time UI progress bars.

- **Import Wizard**
  - Step-by-step interface for importing external data files.
  - Multi-format support (CSV, Excel, TSV) with automatic detection.
  - Interactive column mapping with validation and sample preview.
  - Template system for reusable import configurations.
  - Excel sheet selection for multi-sheet workbooks.
  - Direct data transformation and CSV export.

- **Study Dashboard**
  - Real-time study overview powered entirely by the Castor API.
  - **Patient Inclusion**: Total included/archived patients, cumulative inclusion curve over time, monthly inclusion bar chart.
  - **Data Completeness**: Overall completion percentage, per-form completion breakdown (e.g., Medical History, Symptoms, Diagnostic Evaluation), per-record and per-field fill rates.
  - **Biobank Samples**: Total sample count, breakdown by sample type and status, per-patient sample overview.
  - Integrated field ID → variable name mapping via the `/export/structure` API endpoint.
  - 30-minute intelligent caching to minimize API calls.
  - One-click refresh for live data updates.
  - Accessible via the **Dashboard** button in the app's top menu bar.

---

## Project structure

```
.
├── App.r                               # Main Shiny app for mapping management, ETL, uploads
├── README.md
├── .Rprofile                           # R startup configuration
├── .gitignore                          # Git ignore rules (excludes sensitive files!)
├── EpicToCastor.Rproj                 # RStudio project file
├── config/                             # 🔐 Configuration files (add to .gitignore!)
│   ├── paths.json                      # Path configuration & overrides
│   ├── APIConfig.json                  # Castor API credentials & DeepL key (NEVER COMMIT!)
│   ├── medical_terms.json              # Medical terminology dictionary (EN→NL)
│   ├── keyboard_shortcuts.json         # Keyboard shortcut configuration
│   ├── autofill_settings.json          # ML autofill preferences (enable/disable)
│   └── ml_models/                      # ML model storage (optional, auto-generated)
│       ├── autofill_model.xgb          # Trained XGBoost model (167 classes)
│       ├── autofill_metadata.rds       # Model metadata, vectorizer & metrics
│       └── backups/                    # Timestamped model backups
├── scripts/
│   ├── config.R                        # Path helpers (epc_path, epc_paths)
│   ├── Logger.r                        # Run directory + structured logging
│   ├── CastorRetrieval.r               # Fetch Castor metadata via API
│   ├── autofill.r                      # Auto-fill EPIC values from Castor
│   ├── autofill_ml.r                   # ML-based autofill (optional)
│   ├── setup_ml_dependencies.r         # ML package installer & tester
│   ├── database.r                      # CSV ⇄ SQLite loaders
│   ├── option_lists2.R                 # Dropdown option generation
│   ├── export_approved.r               # Export approved autofill suggestions
│   ├── batch_upload_helper.r           # Batch upload utilities
│   ├── import_wizard_combined.r        # Import wizard module (detection, mapping, transformation)
│   ├── dashboard.r                     # Study dashboard module (inclusion, completeness, biobank)
│   ├── baseline/
│   │   ├── baseline.r                  # Baseline ETL orchestrator
│   │   └── baselineExport.r            # Baseline Castor upload helper
│   ├── biobank_data/
│   │   ├── biobank_data.r              # MDN→Participant mapping + CSV writer
│   │   └── biobankExport.r             # Biobank Castor upload helper
│   └── follow_up/
│       ├── follow_up.r                 # Follow-up ETL orchestrator
│       └── follow_upExport.r           # Follow-up Castor upload helper
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
├── castor_export/                      # Generated JSON payloads
│   ├── baseline.json                   # Baseline upload payload
│   ├── biobank.json                    # Biobank upload payload
│   └── follow_up.json                  # Follow-up upload payload
├── input_data/                         # User data files
│   ├── epic_export/                    # EPIC baseline exports
│   │   └── EpicExport.csv
│   ├── biobank_data/                   # Biobank CSV inputs
│   │   ├── biobank_data.csv
│   │   └── MDNS.csv
│   └── follow_up/                      # EPIC follow-up exports
│       └── EpicExport.csv
├── output_data/                        # Processed data
│   ├── baseline/                       # Processed baseline data
│   │   └── baseline.csv
│   ├── biobank_data/                   # Processed biobank data
│   │   └── biobank.csv
│   └── follow_up/                      # Processed follow-up data
│       └── follow_up.csv
├── db/                                 # SQLite databases (mapping & metadata)
│   ├── mapping_data.db
│   ├── castor_meta.db
│   └── dashboard_cache/                # Dashboard API response cache (auto-generated)
│       ├── records.rds
│       ├── data_points.rds
│       ├── field_mapping.rds
│       └── biobank.rds
├── logs/                               # Run logs (gitignored)
│   ├── 2025-11-22_20-08/               # Example run directory
│   │   ├── App_log.txt
│   │   ├── baseline_log.txt
│   │   ├── biobank_data_log.txt
│   │   ├── follow_up_log.txt
│   │   ├── follow_upExport_log.txt
│   │   └── status.json
│   └── ml_predictions/                 # ML prediction logs (optional)
│       └── predictions_YYYYMM.csv      # Monthly prediction tracking
├── Rlibs/                              # Local R package library (gitignored)
│   ├── shiny/
│   ├── data.table/
│   ├── xgboost/
│   └── ...                             # Other installed packages
└── www/                                # Static assets for Shiny UI
    ├── appCSS.css
    ├── appJS.js
    ├── colResizable-1.6.js             # Column resizing library
    ├── select2.min.css                 # Select2 dropdown styling
    ├── select2.min.js                  # Select2 dropdown library
    └── img/
        └── logo.png
 
```

---

## Quick Start Guide

Follow these steps to get Epic2Castor up and running:

### Prerequisites

- R ≥ 4.5.0
- Rtools ≥ 4.5 (Windows only)
- RStudio (recommended)
- Internet access for packages and Castor API calls (unless cached metadata is provided)

### Step 1: Install R, Rtools and RStudio

1. **Download and install R** (if not already installed):
   - Visit [https://cran.r-project.org/](https://cran.r-project.org/)
   - Click on "Download R for (your operating system)"
   - Download R ≥ 4.5.0 for your operating system
   - Run the installer and follow the instructions

2. **Download and install Rtools** (Windows only):
   - Visit [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)
   - Download Rtools ≥ 4.5 matching your R version
   - Run the installer with default settings
   - **Why Rtools?** Required for compiling R packages from source. Windows lacks built-in compilers, while macOS/Linux have them pre-installed.
   - **Note**: If using binary-only installation (see [Advanced Installation](#advanced-installation)), Rtools is optional but still recommended.

3. **Download and install RStudio** (recommended):
   - Visit [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
   - Download the free RStudio Desktop version
   - Run the installer and follow the instructions

### Step 2: Open the Project

1. **Navigate to the project folder** (e.g., `Epic2Castor_Public`)
2. **Double-click** the `EpicToCastor.Rproj` file
3. This will open the project in RStudio with the correct working directory

### Step 3: Install Required R Packages

**Important**: On Radboudumc managed workstations, packages must be installed to the network-based personal R library. See [Advanced Installation](#advanced-installation) below instead for instructions. This step can be skipped.

```r
install.packages(c(
  "shiny", "data.table", "DT", "shinyjs", "shinydashboard",
  "readxl", "readr", "processx", "jsonlite", "httr",
  "DBI", "RSQLite", "digest", "uuid", "stringdist", "later",
  "Matrix", "text2vec", "xgboost", "plotly"
))
```

**Note about ML packages** (`Matrix`, `text2vec`, `xgboost`):
- These enable ML-based autofill predictions (8th matching strategy)
- If installation fails, the app works fine with its standard 7 autofill strategies
- Enable or disable ML predictions via checkbox in the app's Autofill modal
- If not installed, ML autofill is automatically disabled

**Note**: Package installation may take 5-10 minutes depending on your internet connection. Scripts invoked via `Rscript` auto-create writable user libraries and install missing packages when necessary.

### Step 4: First Launch

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

**Note**: If packages weren't installed in Step 3, they will be automatically installed now. This may take a few minutes.

### Step 5: Configure Credentials

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
- The **Study ID** is visible in your browser's URL bar or under the study settings

### Step 6: Refresh Castor Metadata

1. Click **'Castor'** → **'Refresh metadata'**
2. Wait for the progress dialog (this may take 30-60 seconds)
3. The app will download:
   - Field options (all dropdown/radio/checkbox values)
   - Study variable list (all fields in your study)
   - Generate possible values for mappings
4. A success notification will appear when complete

**This step is crucial** - it populates the dropdown options you'll need for mapping!

### Step 7: Load Input File

1. Navigate to the **'Elements' tab** in the file select in the top left
2. Click the **file** button at the top to open a dropdown
3. Click **Manage input files**
4. In the following modal dialog:
   - Select the data type
   - Click **'Browse'** and select your Epic SmartForm export file (`.csv` or `.xlsx`)
   - The relevant file type should be selected for you
      > **Note**: This feature is still a W.I.P. and may select a wrong file type, please ensure the correct file type is selected by overriding when incorrect
   - Map the headings from the file to the corresponding fields in the wizard
      > Having the file open greatly helps with this step, templates can be saved to make the process easier in future
   - Click **Transform data** to continue
   - If everything looks correct click **Export to CSV**
   - You can now close the wizard
5. The dropdown options in your mapping tables will now be populated with values from your Epic export

**You're now ready to create mappings!**

### Step 8: Create Your First Mapping

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

## Advanced Installation

This section covers advanced installation scenarios.

### For Radboudumc Managed Workstations (App-V / Werkplek 2.0)

Radboudumc workstations use **App-V virtualization** for R and RStudio. Packages with compiled C/C++ code (DLLs) — such as `Rcpp`, `shiny`, `later`, and `httpuv` — **will not load correctly** when installed on OneDrive or other non-virtualized paths. They must be installed to the **network-based personal R library**.

##### Automatic Setup (Recommended)

The project's `.Rprofile` **automatically detects** the App-V environment and guides you through the setup on first launch, please follow the 5 steps below:

1. **Open the project** in RStudio by double-clicking `EpicToCastor.Rproj`
2. The `.Rprofile` detects App-V and displays a setup prompt in the R console:
   ```
   ============================================================
     RADBOUDUMC APP-V ENVIRONMENT DETECTED
   ============================================================
     R packages with DLLs (e.g. Shiny) only work from the
     Radboudumc network library.

     Enter your z-number (e.g. z123456):
   ```
3. **Enter your z-number** (e.g. `z123456`) and confirm it a second time
4. The script automatically:
   - Creates your personal network library folder if it doesn't exist
   - Saves the configuration to `config/.net_lib_path` for future sessions
   - Sets the library path so R loads packages from the network share

On subsequent R sessions, the `.Rprofile` silently loads the saved path and displays a blue status banner:
```
  ┌────────────────────────────────────────────────────────┐
  │  App-V environment detected                            │
  │  R packages are loaded from the network library:       │
  │  //umcn.nl/nas/APP/APPDATA/z123456/R/win-library/4.4   │
  └────────────────────────────────────────────────────────┘
```

> **Note**: The network library folder (`\\umcn.nl\nas\APP\APPDATA\...`) is not browsable in Windows Explorer — it is only accessible programmatically. If you encounter permission errors during setup, contact the ServiceDesk via TopDesk.

5. Install packages to the network library

After the automatic setup has configured your library path, install the required packages. The network library path is already active, so you can use `.libPaths()[1]` to reference it:

```r
# First verify your network library is set (should show \\umcn.nl\... as the first path)
.libPaths()
```

```r
# Then disable lock files
options("install.lock" = FALSE)
options(install.packages.compile.from.source = "never")
```

```r
# Last install packages using a CRAN snapshot matching R 4.4.x to ensure binary compatibility
install.packages(c(
  "Rcpp", "shiny", "data.table", "DT", "shinyjs", "shinydashboard",
  "readxl", "readr", "processx", "jsonlite", "httr",
  "DBI", "RSQLite", "digest", "uuid", "stringdist", "later",
  "Matrix", "text2vec", "xgboost"
), lib = .libPaths()[1], type = "binary",
   repos = "https://packagemanager.posit.co/cran/2024-10-01")
```

> **Important**: Use the Posit Package Manager snapshot URL (`https://packagemanager.posit.co/cran/2024-10-01`) instead of the default CRAN mirror. This ensures you get binary packages built for **R 4.4.x**, avoiding version mismatches with the App-V R installation.

##### Manual Setup (Alternative)

If you prefer to configure the network library manually (or the automatic setup fails), add the following to the top of `.Rprofile` (replace `z123456` with your z-number):

```r
net_lib <- "//umcn.nl/nas/APP/APPDATA/z123456/R/win-library/4.4"
if (dir.exists(net_lib)) {
  .libPaths(c(net_lib, .libPaths()))
}
```

Or create the directory manually:
```r
# Replace z123456 with your z-number
system('powershell.exe -command "mkdir \\\\umcn.nl/nas/APP/APPDATA/z123456/R/win-library/4.4"')
```

##### How the automatic detection works

The `.Rprofile` uses a two-phase approach:

- **Phase 1** (during `.Rprofile` load): Detects App-V by checking if R's library paths contain `App-V` or `ProgramData\...\Root\VFS`. If a saved configuration exists in `config/.net_lib_path`, the network library is silently added to `.libPaths()`.
- **Phase 2** (during `.First` function): If App-V was detected but no saved configuration exists, the user is prompted for their z-number with double confirmation (up to 3 attempts). The network library is created if needed and the path is saved for future sessions.

##### Why is this necessary?

App-V virtualizes R's file system. DLLs (compiled C/C++ code) only initialize correctly when loaded from paths that App-V recognizes as trusted: the virtualized system library or the designated network share (`\\umcn.nl\nas\APP\APPDATA\...`). Packages installed on OneDrive, `C:\temp`, or other arbitrary paths will appear to install successfully, but their DLLs will fail to register internal routines at load time, causing errors like:

```
Error: .onLoad failed in loadNamespace() for 'Rcpp', details:
  call: new_dummyObject(.dummyInstancePointer)
  error: object 'class__dummyInstance' not found
```

##### Troubleshooting

- **Z-number prompt not appearing**: Ensure you opened the project via `EpicToCastor.Rproj` (not just a standalone R session). The `.Rprofile` in the project directory must be loaded.
- **Setup prompt reappears each session**: Check that `config/.net_lib_path` was saved successfully. Verify write permissions to the `config/` directory.
- **`Permission denied` errors**: Contact the ServiceDesk via TopDesk to have your personal library folder created or cleared.
- **`install.lock` errors**: Run `options("install.lock" = FALSE)` before installing.
- **Package built under wrong R version**: Use the Posit snapshot URL shown above to get packages matching R 4.4.x.
- **Persistent DLL errors after reinstall**: Restart RStudio completely (close and reopen), then retry `library(shiny)`.
- **Reset configuration**: Delete `config/.net_lib_path` and restart R to re-run the z-number setup.

---

## Configuration

### First Time Setup

On first launch the app automatically creates all required directories (`config/`, `db/`, `castor_meta/`, etc.), generates a template `APIConfig.json`, and populates placeholder metadata files. See [Step 4: First Launch](#step-4-first-launch) and [1. Starting the App](#1-starting-the-app) for a full walkthrough.

### API Configuration

**Castor API credentials** are read from [`config/APIConfig.json`](config/APIConfig.json).

#### Using the App UI (Recommended)

The easiest way to configure credentials is through the app's **'Castor' → 'Update credentials'** menu. See [2. Configuring Credentials](#2-configuring-credentials) in the User Guide for a step-by-step walkthrough.

#### Manual Configuration

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

⚠️ **IMPORTANT**: This file contains sensitive credentials and should never be uploaded!

#### Refreshing Metadata

Use **'Castor' → 'Refresh metadata'** in the app to update study data without restarting. Metadata is cached and only refreshed when needed. See [3. Refreshing Castor Metadata](#3-refreshing-castor-metadata) in the User Guide for details.

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

### Medical Terms Dictionary

The autofill module uses [`config/medical_terms.json`](config/medical_terms.json) for English→Dutch medical term translations. Customize this file to match your study's terminology.

### ML Model Configuration (Optional)

If you've installed the ML packages, trained models are stored in:
- **Model files**: `config/ml_models/autofill_model.xgb`
- **Model metadata**: `config/ml_models/autofill_metadata.rds`
- **Model backups**: `config/ml_models/backups/`
- **Prediction logs**: `logs/ml_predictions/`

The ML model:
- Trains automatically when sufficient approved mappings exist (≥50 records)
- Updates incrementally as you approve more autofill suggestions
- Requires no manual configuration
- Falls back gracefully to standard strategies if unavailable

To manually retrain the model:
```r
source("scripts/autofill_ml.r")
retrain_model()
```

---

## User Guide

This section walks you through the complete Epic2Castor workflow, from launching the app to uploading data to Castor EDC. For initial installation and setup, see the [Quick Start Guide](#quick-start-guide).

### 1. Starting the App

```r
shiny::runApp(".")
# or open App.r in RStudio and click "Run App"
# or press Ctrl+Shift+Enter (Windows/Linux) / Cmd+Shift+Return (Mac)
```

On first launch, the app automatically creates required directories (`config/`, `db/`, `castor_meta/`, etc.), generates placeholder mapping files, and creates a template `APIConfig.json`. You'll see a warning about missing API credentials — configure them in the next step.

The Shiny app is the primary interface for Epic2Castor, providing:

- **Mapping editor** with tabbed interface (Elements, Checkboxes, Radiobuttons, Variables)
- **Real-time validation** of column values and file structure
- **Dropdown options** automatically populated from Epic exports and Castor metadata
- **Auto-fill functionality** for intelligent EPIC value suggestions
- **Credential management** via integrated UI
- **ETL execution** and upload helpers with progress monitoring
- **File management** with validation and upload capabilities
- **Study dashboard** with real-time inclusion, completeness, and biobank statistics

### 2. Configuring Credentials

Before using the app's full functionality, configure your Castor API credentials:

1. Click the **'Castor' menu** at the top of the app
2. Select **'Update credentials'**
3. Fill in:
   - **Client ID**: From Castor EDC → Settings → API
   - **Client Secret**: Generate one in Castor EDC → Settings → API
   - **Study ID**: Found in your study's URL (e.g., `ABC123DEF456`)
   - **DeepL API Key** (optional): For improved medical term translations
4. Click **'Save'**

**Where to find Castor credentials:** Log in to [Castor EDC](https://data.castoredc.com) → navigate to your study → **Settings** → **API**. Copy the Client ID, generate a Client Secret, and note the Study ID from your study's URL.

> Alternatively, edit [`config/APIConfig.json`](config/APIConfig.json) directly. See [API Configuration](#api-configuration) for details.

### 3. Refreshing Castor Metadata

After configuring credentials, fetch your study's field definitions:

1. Click **'Castor'** → **'Refresh metadata'**
2. Wait for the progress dialog (30–60 seconds)
3. The app downloads:
   - Field options (all dropdown, radio, and checkbox values)
   - Study variable list (all fields in your study)
   - Possible values for mapping dropdowns
4. A success notification appears when complete

**This step is crucial** — it populates the dropdown options you need for creating mappings.

> Metadata is cached and only re-fetched when needed. Use **'Castor' → 'Refresh metadata'** at any time to update after changes in Castor EDC.

### 4. Loading Input Files

Use the **Import Wizard** to load your Epic export files:

1. Navigate to the **'Elements' tab** in the table selector (top left)
2. Click the **file button** (📁) → **'Manage input files'**
3. In the Import Wizard modal, follow the steps below:

#### 4a. Select Import Type

Choose the type of data you're importing:

- **EPIC Export (Baseline)**: Patient baseline data in Element-Value format
- **EPIC Export (Follow-up)**: Follow-up data in Element-Value format
- **Biobank Data**: Laboratory/biobank sample data
- **Custom Data**: Generic data with custom column mapping

Each type has predefined column requirements and validation rules.

#### 4b. Upload and Detect File

- Click **'Browse'** to select your file (CSV, Excel, or TSV)
- The wizard automatically detects format, encoding, and column structure
- For Excel files with multiple sheets: select the target sheet from the dropdown
- Optionally load a saved template to auto-populate mappings

#### 4c. Map Columns

- The interactive mapping table shows required columns (marked with red asterisk *), detected source columns, sample data, and validation status
- Map each required column using dropdowns, or enter custom fixed values for constants
- Review sample data previews to verify correct mapping
- **Save as template** for future imports with the same structure
- Click **'Transform Data'** when mapping is complete

#### 4d. Preview and Export

- Review the transformed data table
- Click **'Export to CSV'** to save the processed file
- Close the wizard — dropdown options in your mapping tables are now populated

> **Tip**: Save templates for recurring imports. Templates store import type, column mappings, and fixed values — saving time on future imports from the same data source.

#### Import Types Reference

| Type | Structure | Required Columns |
|------|-----------|-----------------|
| EPIC Export (Baseline/Follow-up) | Element-Value pairs (vertical) | `Element`, `Value`, `MDN`, `Contact Date` |
| Biobank Data | Patient-sample records (horizontal) | `MDN`, `Sample ID`, `Collection Date`, `Sample Type` |
| Custom Data | User-defined (flexible) | Defined by user during import |

#### Template Management

**Save a template:** In Step 4c, click **'Save as Template'** after completing your mapping. Enter a descriptive name (e.g., "EPIC Baseline - Hospital A"). The template stores import type, column mappings, fixed values, and file structure requirements.

**Load a template:** In Step 4b, click **'Load Template'** and select a saved template. Mappings are automatically applied in Step 4c. Verify that mappings match your new file structure.

#### Import Wizard Troubleshooting

- **Column not detected**: Ensure the file has headers and uses standard delimiters
- **Encoding issues**: The wizard auto-detects encoding; if characters display incorrectly, the file may need manual conversion
- **Required column missing**: Check if your source file contains the necessary data; use fixed values for constants
- **Template doesn't match file**: Column names must exactly match between template and new file
- **Transform button disabled**: All required columns must be mapped with valid values
- **Excel sheet not showing**: Ensure the file is a valid Excel workbook (.xlsx/.xls) with named sheets
- **Export fails**: Check that you have write permissions to the output directory

### 5. Creating Mappings

The mapping editor is organized into tabbed tables at the bottom of the screen.

#### Working with the Elements Table

The **Elements** tab is the foundation of your mappings:

1. Click **'+ Add Row'** to create a new mapping
2. Use dropdowns to select:
   - **Element**: An Epic element from your export
   - **Castor Name**: The corresponding Castor field
3. The app automatically:
   - Creates matching rows in the **Checkboxes** or **Radiobuttons** tab (if the Castor field type is checkbox/radiobutton)
   - Pre-fills the **Castor Name** column and option values
   - Provides dropdown options for EPIC values
4. Organize elements across multiple tabs for complex studies
5. Use the copy (📋) and paste (📄) buttons to duplicate mappings

> **💡 Tip**: When copying/cutting elements, related checkboxes and radiobuttons are automatically included!

#### Working with Checkboxes and Radiobuttons

After creating elements with checkbox/radiobutton Castor fields:

1. Switch to the **Checkboxes** or **Radiobuttons** tab
2. Review auto-generated rows — each Castor option appears as a separate row
3. Select the matching **EPIC Value** from the dropdown for each row
4. Use **Auto-fill** (🪄 button) for intelligent suggestions (see next section)

#### File Management

The file browser (📁 icon) provides:

- **Upload validation**: Real-time column checking before upload
- **Multi-source support**: Epic exports, biobank data, follow-up files
- **File selection**: Choose existing files or upload new ones
- **Automatic option reload**: After uploading an Epic export, dropdown options refresh automatically

#### Saving Your Work

Press **Ctrl+S** or click **'Save'** to persist mappings to the database. Changes are saved to SQLite and synced back to CSV files in the `mapping/` directory.

### 6. Auto-fill EPIC Values

The **Auto-fill** feature (available in Checkboxes and Radiobuttons tabs) uses intelligent matching to suggest EPIC values for Castor options.

#### How to Use Auto-fill

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
   - Filter by confidence level or element

5. Click **'Apply Selected'** to fill the values

#### Matching Strategies

Auto-fill uses up to 8 intelligent strategies (in order of precedence):

1. **Exact match**: EPIC value exactly matches Castor value
2. **Normalized match**: Case-insensitive, punctuation-normalized matching
3. **Medical dictionary**: Dictionary-based medical terminology matching (English→Dutch)
4. **ML Prediction** (optional): XGBoost model trained on approved mappings
5. **Google Translate API**: Cloud-based translation service
6. **MyMemory API**: Alternative translation service
7. **Fuzzy string matching**: Similarity-based matching using Levenshtein distance

#### ML-Based Autofill (Optional)

When enabled, the ML strategy provides intelligent predictions based on historical patterns:

- **XGBoost multi-class classifier** with TF-IDF text vectorization (52 features)
- **Confidence scoring** (50–100%) for prediction reliability
- Trains automatically when ≥50 approved mappings exist
- Retrains after 20+ new mappings are approved
- Enable/disable via checkbox in the Autofill modal (persists in `config/autofill_settings.json`)

**Requirements:** R packages `Matrix`, `text2vec`, `xgboost` (auto-installed on first launch). If unavailable, the app continues with standard autofill strategies.

<details>
<summary><strong>ML Model Management</strong></summary>

```r
# Check if retraining is recommended
source("scripts/autofill_ml.r")
check_retrain_status(verbose = TRUE)

# Force retrain with current data
retrain_model(force = TRUE)

# View prediction statistics
stats <- get_ml_stats(months_back = 3)
print(stats)

# Validate model files
status <- validate_model_files()
if (!status$valid) recover_model_from_backup()
```

Trained models are stored in `config/ml_models/`. Backups are created in `config/ml_models/backups/`. Prediction logs are written to `logs/ml_predictions/predictions_YYYYMM.csv`.

</details>

#### Auto-fill Best Practices

- Fill high-confidence suggestions first (green items)
- Review medium-confidence suggestions carefully (yellow items)
- Manually check low-confidence suggestions (orange items)
- Use **'Export CSV'** to save suggestions for offline review
- Approved auto-fills build a reference dictionary for future runs

### 7. Running the ETL Pipeline

Once your mappings are complete, run the ETL to transform Epic data into Castor-ready format.

**From the app:** Use the ETL execution buttons in the app interface with progress monitoring.

**From the command line:**

```r
# Baseline data
Rscript scripts/baseline/baseline.r

# Follow-up data
Rscript scripts/follow_up/follow_up.r

# Biobank data
Rscript scripts/biobank_data/biobank_data.r
```

Each script reads Epic exports from [`input_data/`](input_data), applies your mappings, normalizes dates, resolves option values, and outputs Castor-ready CSVs under the corresponding [`output_data/`](output_data) subfolder.

### 8. Exporting to Castor

After the ETL produces output CSVs, generate JSON payloads and upload to Castor.

**From the app:** Use the export buttons to generate and submit payloads via the Castor API with progress monitoring.

**From the command line:**

```r
# Baseline JSON + API upload
Rscript scripts/baseline/baselineExport.r "<site_id> - <site_name>"

# Biobank JSON + API upload
Rscript scripts/biobank_data/biobankExport.r "<site_id> - <site_name>"

# Follow-up JSON + API upload
Rscript scripts/follow_up/follow_upExport.r "<site_id> - <site_name>"
```

Each export script:

1. Reads staged CSVs from the corresponding `output_data/` subfolder
2. Streams JSON payloads into [`castor_export/`](castor_export)
3. Creates or updates participants, repeating data, and datapoints via the Castor API
4. Records a detailed [`Datastructure.json`](castor_meta/Datastructure.json) snapshot for auditing

### 9. Monitoring with the Study Dashboard

The **Study Dashboard** (📊 button in the top menu bar) provides a real-time overview of your Castor study powered by the Castor API.

#### Patient Inclusion

- Total included and archived patients
- Cumulative inclusion curve over time
- Monthly inclusion bar chart for trend analysis

#### Data Completeness

- Overall completion percentage across all records
- Per-form completion breakdown (e.g., Medical History 42.5%, Symptoms 34.9%)
- Per-record and per-field fill rates
- Calculation fields automatically excluded from metrics

#### Biobank Samples

- Total sample count with unique patient count
- Breakdown by sample type (e.g., Plasma EDTA, DNA stock) and status (e.g., Complete, Pending)
- Per-patient sample overview

The dashboard uses three Castor API endpoints (`/export/data`, `/export/structure`, `/record`). Data is cached for 30 minutes in `db/dashboard_cache/`. Click **Refresh Data** to force a fresh fetch. Requires valid API credentials and internet access.

### Keyboard Shortcuts

Epic2Castor supports Excel-like keyboard shortcuts for faster editing:

| Shortcut | Action |
|----------|--------|
| `Ctrl+F` | Focus search box (with text selection) |
| `Ctrl+Left/Right` | Navigate between tabs (wraps around) |
| `Escape` | Clear all checkbox selections |
| `Ctrl+C` / `Ctrl+Insert` | Copy selected rows (Elements only) |
| `Ctrl+X` | Cut selected rows (Elements only) |
| `Ctrl+V` / `Shift+Insert` | Paste copied/cut rows (Elements only) |
| `Ctrl+S` | Save changes (opens save modal) |
| `F1` | Open help modal (shows all shortcuts) |
| `F5` | Refresh current tab (soft reload) |
| `Ctrl+Shift+P` | Open performance panel |

> **💡 Tip**: When copying/cutting elements, related checkboxes and radiobuttons are automatically included!

**Console commands** (open browser console with F12):
```javascript
shortcutStats()    // View all statistics
shortcutTop(5)     // Show top 5 shortcuts
shortcutReset()    // Reset metrics
```

> See [`KEYBOARD_SHORTCUTS_GUIDE.md`](KEYBOARD_SHORTCUTS_GUIDE.md) for full documentation.

---

## Troubleshooting

- **Package installation issues**: ensure the R user library is writable; scripts auto-create `R/win-library/<version>` when needed.
- **Castor API 401/403**: verify credentials in [`config/APIConfig.json`](config/APIConfig.json) and client permissions for the study.
- **Missing input files**: filenames must match `epic_tabel` values in [`mapping/variabelen.csv`](mapping/variabelen.csv); expected locations under [`input_data`](input_data).
- **Unexpected option values**: refresh Castor metadata via **'Castor' → 'Refresh metadata'** in the app UI and validate mapping CSV entries.
- **Upload failures**: inspect JSON payloads in [`castor_export`](castor_export) and [`Datastructure.json`](castor_meta/Datastructure.json) for server responses.
- **Configuration not found**: ensure [`config/APIConfig.json`](config/APIConfig.json) exists and contains valid credentials. Use the app's **'Castor' → 'Update credentials'** menu for easy configuration.
- **Autofill issues**: check [`config/medical_terms.json`](config/medical_terms.json) for missing translations and review confidence thresholds in [`scripts/autofill.r`](scripts/autofill.r).
- **Empty dropdowns after file upload**: Ensure you've completed the Quick Start Guide steps 1-7 (R/RStudio/Rtools → open project → packages → first launch → credentials → refresh metadata → load file).
- **Checkboxes/Radiobuttons not auto-populating**: Make sure the Castor field type is correctly set as checkbox/radiobutton in Castor EDC. Refresh metadata to update field types.
- **Hash file errors**: The app uses MD5 hashing to detect changes. If you see unexpected rebuild messages, check that CSV files aren't being modified by external processes.
- **Locale warnings**: The app uses Dutch locale settings (`,` for decimals, `.` for thousands). If you see locale warnings, ensure your CSV files match this format.
- **ML package installation fails**: If `text2vec` or `xgboost` fail to install, ensure Rtools is installed (Windows) or development tools are available (macOS/Linux). ML features are optional - autofill works without them. Force binary installation: `install.packages(c("Matrix", "text2vec", "xgboost"), type = "binary")`
- **ML model not training**: The model requires at least 50 approved mappings. Check training data: `prepare_training_data()` should return ≥50 rows. View status: `check_retrain_status(verbose = TRUE)`
- **Low ML confidence scores**: ML predictions improve with more training data. The model starts with ~19% validation accuracy due to 167 classes and limited data. Approve high-quality suggestions to improve future predictions.
- **Model file corrupted**: Run `validate_model_files()` to check integrity. Use `recover_model_from_backup()` to restore from the most recent backup in `config/ml_models/backups/`
- **Retraining recommended**: The app tracks new mappings and model age. When retraining is recommended, run `retrain_model()` or wait for automatic retraining after 20+ new approvals.
- **ML predictions return NULL**: Check `validate_prediction_input()` output. Ensure input is non-empty character string. Review error logs in console for detailed diagnostics.

---

## Data flow

The ETL scripts ([`scripts/baseline/baseline.r`](scripts/baseline/baseline.r), [`scripts/follow_up/follow_up.r`](scripts/follow_up/follow_up.r), [`scripts/biobank_data/biobank_data.r`](scripts/biobank_data/biobank_data.r)) orchestrate the data transformation:

1. Initialize logging/status and load paths via [`scripts/config.R`](scripts/config.R).
2. Rebuild SQLite caches ([`db/*.db`](db/)) when source CSV hashes change.
3. Retrieve or read Castor metadata (option groups, study variable list).
4. For each mapping row in [`mapping/variabelen.csv`](mapping/variabelen.csv):
   - Load Epic source file (Excel or CSV) from the corresponding [`input_data`](input_data) subfolder.
   - Normalize text encodings, dates (`datum`), and durations (weeks conversion).
   - Rename Epic columns to Castor variables via [`mapping/variabelen.csv`](mapping/variabelen.csv).
   - Resolve checkbox and radio options using [`mapping/waarde_checkboxes.csv`](mapping/waarde_checkboxes.csv) and [`mapping/waarde_radiobuttons.csv`](mapping/waarde_radiobuttons.csv).
   - Pivot long-to-wide and consolidate checkbox selections.
   - Apply fixed values, repeating data labels, and creation timestamps.
   - Write Castor-ready CSVs separated by table under [`output_data`](output_data).

Optional upload scripts ([`baselineExport.r`](scripts/baseline/baselineExport.r), [`biobankExport.r`](scripts/biobank_data/biobankExport.r), [`follow_upExport.r`](scripts/follow_up/follow_upExport.r)) reuse these outputs to stream JSON payloads and interact with the Castor API.

---

## Logging & status tracking

- [`scripts/Logger.r`](scripts/Logger.r) creates run directories (`logs/<timestamp>`), captures stdout/stderr into `<Script>_log.txt`, and can be shared across processes (Shiny app + Rscript).
- Status helpers (`epic2castor_status_*`) write a compact JSON file (default: `logs/<run>/status.json`) polled by the UI progress bars.
- All scripts support structured logging with automatic run directory creation and log file management.

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


# Epic2Castor

Convert Epic SmartForm exports into Castor EDC import files. Built for the [MyCoS study](https://www.radboudumc.nl) at Radboudumc, this toolkit provides an ETL pipeline and a Shiny UI to manage mappings, retrieve Castor metadata, transform data, and generate Castor-ready CSV and JSON payloads.

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

## Table of Contents

- [Features](#features)
- [Project Structure](#project-structure)
- [Getting Started (Radboudumc)](#getting-started-radboudumc)
- [Getting Started (Other Systems)](#getting-started-other-systems)
- [Configuration](#configuration)
- [Usage](#usage)
- [Keyboard Shortcuts](#keyboard-shortcuts)
- [Data Flow](#data-flow)
- [Troubleshooting](#troubleshooting)
- [Team](#team)
- [Contributing](#contributing)
- [License](#license)

---

## Features

- **Shiny mapping editor** — tabbed interface (Elements, Checkboxes, Radiobuttons, Variables) with dropdown validation, copy/paste propagation, and inline feedback.
- **Automated Castor metadata retrieval** — OAuth2 via [`scripts/CastorRetrieval.r`](scripts/CastorRetrieval.r), cached in SQLite with hash-based change detection.
- **Intelligent auto-fill** — 7 matching strategies (exact, normalized, medical dictionary, translation APIs, fuzzy) with confidence scoring (70–100%). Optional 8th ML strategy using XGBoost.
- **Import Wizard** — 4-step guided import (Upload → Verification → Mapping → Export) with multi-format support (CSV, Excel, TSV), template system, and automatic column detection.
- **ETL pipeline** — reads Epic exports (`.xlsx`/`.csv`), applies mappings, normalizes dates, resolves radio/checkbox values, produces Castor-ready CSVs.
- **Castor upload helpers** — streaming JSON payload creation and API submission for baseline, biobank, follow-up, and lab data.
- **Study Dashboard** — real-time patient inclusion curves, data completeness metrics, and biobank sample overview via the Castor API.
- **Structured logging** — per-run directories with stdout/stderr capture and JSON status for UI progress bars.

---

## Project Structure

```
.
├── App.r                           # Main Shiny application
├── .Rprofile                       # R startup (App-V detection, library paths)
├── EpicToCastor.Rproj              # RStudio project file
│
├── config/
│   ├── paths.json                  # Path configuration & overrides
│   ├── medical_terms.json          # Medical terminology dictionary (EN→NL)
│   ├── keyboard_shortcuts.json     # Shortcut configuration
│   ├── autofill_settings.json      # ML autofill preferences
│   ├── mapping_templates/          # Saved import wizard templates
│   └── ml_models/                  # Trained XGBoost model & metadata
│
├── scripts/
│   ├── config.R                    # Path helpers (epc_path, epc_paths)
│   ├── Logger.r                    # Run directory + structured logging
│   ├── CastorRetrieval.r           # Fetch Castor metadata via API
│   ├── autofill.r                  # Auto-fill engine (7 strategies)
│   ├── autofill_ml.r               # ML-based autofill (optional, XGBoost)
│   ├── database.r                  # CSV ⇄ SQLite loaders
│   ├── option_lists2.R             # Dropdown option generation
│   ├── export_approved.r           # Export approved autofill suggestions
│   ├── batch_upload_helper.r       # Batch upload utilities
│   ├── import_wizard_combined.r    # Import wizard module
│   ├── dashboard.r                 # Study dashboard module
│   ├── baseline/
│   │   ├── baseline.r              # Baseline ETL orchestrator
│   │   └── baselineExport.r        # Baseline Castor upload
│   ├── biobank_data/
│   │   ├── biobank_data.r          # Biobank ETL orchestrator
│   │   └── biobankExport.r         # Biobank Castor upload
│   ├── follow_up/
│   │   ├── follow_up.r             # Follow-up ETL orchestrator
│   │   └── follow_upExport.r       # Follow-up Castor upload
│   └── lab_data/
│       ├── lab_data.r              # Lab data ETL orchestrator
│       └── lab_dataExport.r        # Lab data Castor upload
│
├── mapping/
│   ├── elements.csv                # EPIC→Castor element mapping
│   ├── variabelen.csv              # Variable name mapping
│   ├── waarde_checkboxes.csv       # Checkbox value mapping
│   ├── waarde_radiobuttons.csv     # Radio button value mapping
│   └── possibleValues/             # Generated possible values per element
│
├── castor_meta/                    # Castor study metadata (from API)
│   ├── field_options.csv
│   └── study_variablelist.csv
│
├── db/                             # SQLite databases & caches
│   └── dashboard_cache/            # Dashboard API response cache
│
├── funtions/
│   └── checks.R                    # Validation helpers
│
└── www/                            # Static assets for Shiny UI
    ├── appCSS.css
    ├── appJS.js
    ├── shortcutHandler.js
    ├── select2.min.css / .js
    └── img/                        # Favicons, logo, manifest
```

> The following directories are generated at runtime and excluded from version control: `input_data/`, `output_data/`, `castor_export/`, `logs/`, `Rlibs/`.

---

## Getting Started (Radboudumc)

Radboudumc workstations use **App-V virtualization** for R and RStudio. Packages with compiled DLLs (e.g. `Rcpp`, `shiny`, `httpuv`) **will not work** when installed on OneDrive or other non-virtualized paths — they must be installed to the **network-based personal R library**.

### Why is this necessary?

App-V virtualizes R's file system. DLLs only initialize correctly from paths App-V recognizes as trusted: the virtualized system library or the network share (`\\umcn.nl\nas\APP\APPDATA\...`). Packages installed elsewhere appear to install successfully but fail at load time with errors like:

```
Error: .onLoad failed in loadNamespace() for 'Rcpp', details:
  call: new_dummyObject(.dummyInstancePointer)
  error: object 'class__dummyInstance' not found
```

### Step 1: Open the Project

1. Double-click `EpicToCastor.Rproj` to open the project in RStudio
2. The `.Rprofile` **automatically detects** the App-V environment and prompts for setup:

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
   - Creates your personal network library folder if needed
   - Saves the configuration to `config/.net_lib_path` for future sessions
   - Sets the library path so R loads packages from the network share

On subsequent sessions, the `.Rprofile` silently loads the saved path and shows a status banner:

```
  ┌────────────────────────────────────────────────────────┐
  │  App-V environment detected                            │
  │  R packages are loaded from the network library:       │
  │  //umcn.nl/nas/APP/APPDATA/z123456/R/win-library/4.4   │
  └────────────────────────────────────────────────────────┘
```

> **Note**: The network library folder (`\\umcn.nl\nas\APP\APPDATA\...`) is not browsable in Windows Explorer — it is only accessible programmatically. Contact the ServiceDesk via TopDesk for permission issues.

#### How the automatic detection works

The `.Rprofile` uses a two-phase approach:

- **Phase 1** (during `.Rprofile` load): Detects App-V by checking if R's library paths contain `App-V` or `ProgramData\...\Root\VFS`. If `config/.net_lib_path` exists, the network library is silently added.
- **Phase 2** (during `.First`): If no saved config exists, the user is prompted for their z-number (double confirmation, up to 3 attempts). The network library is created and saved.

### Step 2: Install Packages

With the network library path active, install packages using a **Posit CRAN snapshot** to guarantee binary compatibility with the App-V R version:

```r
# Verify network library is first (should show \\umcn.nl\... path)
.libPaths()

# Disable lock files (required on App-V)
options("install.lock" = FALSE)
options(install.packages.compile.from.source = "never")

# Install packages from Posit snapshot matching R 4.4.x
install.packages(c(
  "Rcpp", "shiny", "data.table", "DT", "shinyjs", "shinydashboard",
  "readxl", "readr", "processx", "jsonlite", "httr", "httr2",
  "DBI", "RSQLite", "digest", "uuid", "stringdist", "later",
  "Matrix", "text2vec", "xgboost", "plotly", "shinyFiles"
), lib = .libPaths()[1], type = "binary",
   repos = "https://packagemanager.posit.co/cran/2024-10-01")
```

> **Important**: Use the Posit Package Manager snapshot URL instead of the default CRAN mirror. This ensures you get binary packages built for **R 4.4.x**, avoiding version mismatches with the App-V installation.

> **ML packages** (`Matrix`, `text2vec`, `xgboost`) are optional. If installation fails, the app works fine with its standard 7 autofill strategies.

### Step 3: Launch and Configure

Continue with [Step 4: First Launch](#step-4-first-launch) in the general getting started guide below.

#### Manual Setup (Alternative)

If the automatic setup fails, add the following to `.Rprofile` (replace `z123456` with your z-number):

```r
net_lib <- "//umcn.nl/nas/APP/APPDATA/z123456/R/win-library/4.4"
if (dir.exists(net_lib)) {
  .libPaths(c(net_lib, .libPaths()))
}
```

Or create the directory manually:

```r
system('powershell.exe -command "mkdir \\\\umcn.nl/nas/APP/APPDATA/z123456/R/win-library/4.4"')
```

---

## Getting Started (Other Systems)

### Prerequisites

- R ≥ 4.5.0
- Rtools ≥ 4.5 (Windows only, required for compiling packages from source)
- RStudio (recommended)
- Internet access for packages and Castor API

### Step 1: Install R, Rtools and RStudio

1. **R**: Download from [https://cran.r-project.org/](https://cran.r-project.org/) (≥ 4.5.0)
2. **Rtools** (Windows only): Download from [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/) (≥ 4.5)
3. **RStudio**: Download from [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

### Step 2: Open the Project

Double-click `EpicToCastor.Rproj` to open the project in RStudio with the correct working directory.

### Step 3: Install Required R Packages

```r
install.packages(c(
  "shiny", "data.table", "DT", "shinyjs", "shinydashboard",
  "readxl", "readr", "processx", "jsonlite", "httr", "httr2",
  "DBI", "RSQLite", "digest", "uuid", "stringdist", "later",
  "Matrix", "text2vec", "xgboost", "plotly", "shinyFiles"
))
```

> **ML packages** (`Matrix`, `text2vec`, `xgboost`) are optional — the app works without them using its standard 7 autofill strategies. Scripts invoked via `Rscript` auto-install missing packages when necessary.

### Step 4: First Launch

1. Open `App.r` in RStudio and click **"Run App"** (or `Ctrl+Shift+Enter`, or `shiny::runApp(".")`)
2. The app automatically creates required directories, placeholder mappings, and a template `APIConfig.json`
3. A warning about missing API credentials is expected — configure them next

### Step 5: Configure Credentials

1. **'Castor'** menu → **'Update credentials'**
2. Fill in:
   - **Client ID** and **Client Secret**: From [Castor EDC](https://data.castoredc.com) → your study → **Settings** → **API**
   - **Study ID**: Found in your study's URL
   - **DeepL API Key** (optional): For improved medical term translations
3. Click **'Save'**

### Step 6: Refresh Castor Metadata

1. **'Castor'** → **'Refresh metadata'**
2. Wait 30–60 seconds for field options and study variables to download
3. This populates the dropdown options needed for mapping

### Step 7: Load Input File

1. In the **'Elements' tab**, click the **file button** (📁) → **'Manage input files'**
2. The Import Wizard guides you through 4 steps:
   - **Upload**: Select your data type and browse for your Epic export (`.csv` / `.xlsx`)
   - **Verification**: Review detected columns and file structure
   - **Mapping**: Map source columns to required fields using dropdowns
   - **Export**: Preview transformed data and export to CSV
3. Save templates for recurring imports to speed up future sessions

### Step 8: Create Your First Mapping

1. In the **'Elements' tab**, click **'+ Add Row'**
2. Select an **Element** (from Epic) and a **Castor Name** (from Castor) using dropdowns
3. The app auto-creates matching rows in the **Checkboxes** / **Radiobuttons** tabs
4. Switch to those tabs to map individual values — use **Auto-fill** (🪄) for suggestions
5. **Ctrl+S** to save mappings

---

## Configuration

### API Credentials

Credentials are stored in `config/APIConfig.json` (gitignored). Configure via the app's **'Castor' → 'Update credentials'** menu, or edit the file directly:

```json
{
  "client_id": "YOUR-CLIENT-ID",
  "client_secret": "YOUR-CLIENT-SECRET",
  "study_id": "YOUR-STUDY-ID",
  "deepl_api_key": "YOUR-DEEPL-KEY-OPTIONAL"
}
```

### Path Configuration

Paths are managed via [`scripts/config.R`](scripts/config.R) (`epc_path()` / `epc_paths()` helpers). Override defaults in [`config/paths.json`](config/paths.json).

### Medical Terms Dictionary

[`config/medical_terms.json`](config/medical_terms.json) provides English→Dutch medical term translations for the autofill module. Customize to match your study's terminology.

### ML Model (Optional)

Trained models are stored in `config/ml_models/`. The model trains automatically when ≥50 approved mappings exist and retrains after 20+ new approvals. No manual configuration needed.

<details>
<summary><strong>ML Model Management</strong></summary>

```r
source("scripts/autofill_ml.r")
check_retrain_status(verbose = TRUE)  # Check if retraining is recommended
retrain_model(force = TRUE)           # Force retrain
get_ml_stats(months_back = 3)         # View prediction statistics
validate_model_files()                # Check model integrity
recover_model_from_backup()           # Restore from backup
```

</details>

---

## Usage

### Running the ETL Pipeline

Transform Epic exports into Castor-ready CSVs — from the app or command line:

```r
Rscript scripts/baseline/baseline.r           # Baseline
Rscript scripts/follow_up/follow_up.r         # Follow-up
Rscript scripts/biobank_data/biobank_data.r   # Biobank
Rscript scripts/lab_data/lab_data.r           # Lab data
```

### Exporting to Castor

Generate JSON payloads and upload via the Castor API:

```r
Rscript scripts/baseline/baselineExport.r "<site_id> - <site_name>"
Rscript scripts/biobank_data/biobankExport.r "<site_id> - <site_name>"
Rscript scripts/follow_up/follow_upExport.r "<site_id> - <site_name>"
Rscript scripts/lab_data/lab_dataExport.r "<site_id> - <site_name>"
```

### Auto-fill

The auto-fill feature (🪄 button in Checkboxes/Radiobuttons tabs) uses up to 8 strategies:

1. Exact match → 2. Normalized match → 3. Medical dictionary → 4. ML prediction (optional) → 5. DeepL API → 6. Google Translate → 7. MyMemory API → 8. Fuzzy string matching

Review suggestions by confidence level (green ≥95%, yellow ≥85%, orange ≥70%) and click **'Apply Selected'**.

### Study Dashboard

Click **Dashboard** (📊) in the top menu bar for real-time metrics: patient inclusion curves, per-form data completeness, and biobank sample overview. Data is cached for 30 minutes.

---

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+F` | Focus search box |
| `Ctrl+Left/Right` | Navigate between tabs |
| `Escape` | Clear selections |
| `Ctrl+C` / `Ctrl+X` / `Ctrl+V` | Copy / Cut / Paste rows (Elements) |
| `Ctrl+S` | Save changes |
| `F1` | Help modal |
| `F5` | Refresh current tab |
| `Ctrl+Shift+P` | Performance panel |

> When copying/cutting elements, related checkboxes and radiobuttons are automatically included.

---

## Data Flow

```
Epic Export (.xlsx/.csv)
    │
    ▼
Import Wizard ──► input_data/
    │
    ▼
ETL Scripts (baseline.r, follow_up.r, biobank_data.r, lab_data.r)
    │  ├── Load mappings from mapping/*.csv
    │  ├── Normalize dates, encodings, durations
    │  ├── Resolve radio/checkbox values
    │  └── Pivot long→wide, consolidate checkboxes
    ▼
output_data/ (Castor-ready CSVs)
    │
    ▼
Export Scripts (*Export.r) ──► castor_export/*.json ──► Castor API
```

Logging: [`scripts/Logger.r`](scripts/Logger.r) creates per-run directories (`logs/<timestamp>/`) with stdout/stderr capture and `status.json` for UI progress bars.

---

## Troubleshooting

### General

| Problem | Solution |
|---------|----------|
| Package installation fails | Ensure R user library is writable. On Radboudumc: use the network library (see [Getting Started](#getting-started-radboudumc)). |
| Castor API 401/403 | Verify credentials in **'Castor' → 'Update credentials'**. Check client permissions for the study. |
| Missing input files | Filenames must match `epic_tabel` values in `mapping/variabelen.csv`. Place files under `input_data/`. |
| Unexpected option values | Refresh Castor metadata via **'Castor' → 'Refresh metadata'**. |
| Empty dropdowns | Complete all getting started steps (packages → first launch → credentials → refresh metadata → load file). |
| Locale warnings | The app uses Dutch locale (`,` for decimals). Ensure CSV files match this format. |

### Radboudumc-Specific

| Problem | Solution |
|---------|----------|
| Z-number prompt not appearing | Open via `EpicToCastor.Rproj`, not a standalone R session. |
| Setup prompt reappears | Check that `config/.net_lib_path` saved successfully. |
| Permission denied | Contact ServiceDesk via TopDesk. |
| `install.lock` errors | Run `options("install.lock" = FALSE)` before installing. |
| Wrong R version for packages | Use the Posit snapshot URL: `https://packagemanager.posit.co/cran/2024-10-01` |
| Persistent DLL errors | Restart RStudio completely, then retry `library(shiny)`. |
| Reset configuration | Delete `config/.net_lib_path` and restart R. |

### ML Autofill

| Problem | Solution |
|---------|----------|
| ML packages won't install | Optional — app works without them. Try: `install.packages(..., type = "binary")` |
| Model not training | Requires ≥50 approved mappings. Check: `prepare_training_data()` |
| Low confidence scores | Improve by approving more high-quality suggestions. |
| Model corrupted | `validate_model_files()` then `recover_model_from_backup()` |

---

## Contributing

Contributions are welcome. When contributing:

1. Test changes locally with the Shiny app
2. Ensure scripts run without errors via `Rscript`
3. Update documentation for new features
4. Never commit API credentials or patient data (`config/APIConfig.json` is gitignored)

---

## License

If [`LICENSE.md`](LICENSE.md) exists, its terms apply; otherwise, all rights remain with the repository owners.


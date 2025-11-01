rm(list = ls())
gc()
allTables <- list()

# Laad centrale paden uit config/paths.json (met veilige fallback) en source logger/config
paths <- tryCatch(
  jsonlite::fromJSON(file.path("config", "paths.json")),
  error = function(e) list(scripts_dir = "scripts",
                           logger_script = file.path("scripts", "Logger.r"),
                           config_script  = file.path("scripts", "config.R"))
)

# Auto logger (creates logs/<date_time> and <Main>_log.txt; captures console)
logger_path <- if (!is.null(paths$logger_script)) paths$logger_script else file.path(paths$scripts_dir, "Logger.r")
source(logger_path)
# Central path configuration
config_script_path <- if (!is.null(paths$config_script)) paths$config_script else file.path(paths$scripts_dir, "config.R")
source(config_script_path)

# Folders (centralized)
inputPath    <- paste0(epc_path("epic_input_data_dir"), "/")
outputFolder <- paste0(epc_path("baseline_output_data_dir"), "/")

# Log routing: stuur messages/warnings naar stdout en print warnings direct
options(warn = 1)
try(sink(stdout(), type = "message"), silent = TRUE)

# Ensure a writable user library and install/load required packages
neededPackages <- c("readxl", "readr", "data.table", "digest", "DBI", "RSQLite")

# Ensure CRAN repo is set (avoid interactive prompt under Rscript)
repos <- getOption("repos"); if (is.null(repos) || is.na(repos["CRAN"]) || repos["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# Prepend user library path and create it if missing
user_lib <- Sys.getenv("R_LIBS_USER", unset = file.path(Sys.getenv("R_USER", unset = path.expand("~")),
                                                     "R", "win-library", paste(R.version$major, strsplit(R.version$minor, "\\.")[[1]][1], sep = ".")))

# Initialize structured status logging for the UI (progress bar + single-line status)
total_tables <- NA_integer_
try({
  # Estimate total work as number of scheme rows; fallback to NA
  scheme_tmp <- tryCatch({
    # Read mapping to estimate: we'll compute after DB is available; set NA for now
    NULL
  }, error = function(e) NULL)
  epic2castor_status_init(total = total_tables, title = "Baseline", step = "init", detail = "Starting…")
}, silent = TRUE)
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(user_lib, .libPaths())))

for (pkg in neededPackages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, lib = user_lib)
    if (!require(pkg, character.only = TRUE)) stop(sprintf("Package '%s' kon niet geïnstalleerd worden.", pkg))
  }
}

# Load meta information from Castor via centrale scriptlocatie
source(epc_path("castor_retrieval_script"))

# Load the database functions
source(file.path(epc_path("scripts_dir"), "database.r"))

# Computes MD5 of all CSVs in a folder (including subfolders)
compute_csv_hash <- function(dataFolder) {
  csvs <- list.files(dataFolder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  sums <- tools::md5sum(csvs)
  digest(paste0(names(sums), sums, collapse = ""), algo = "md5")
}

# Check and (re)build only if hash changes or DB/hash-file is missing
check_and_build <- function(dataFolder, dbPath, build_fun) {
  hashFile <- paste0(dbPath, ".hash")
  newHash  <- compute_csv_hash(dataFolder)
  if (!file.exists(dbPath) ||
      !file.exists(hashFile) ||
      readLines(hashFile) != newHash) {
    build_fun()
    writeLines(newHash, hashFile)
  }
}

# Mapping-data
check_and_build(
  dataFolder = epc_path("mapping_dir"),
  dbPath     = epc_path("mapping_db"),
  build_fun  = function() csv_to_database(
    dataFolder = epc_path("mapping_dir"),
    dbPath     = epc_path("mapping_db")
  )
)

# Castor-meta data
check_and_build(
  dataFolder = epc_path("castor_meta_dir"),
  dbPath     = epc_path("castor_meta_db"),
  build_fun  = function() csv_to_database_meta(
    dataFolder = epc_path("castor_meta_dir"),
    dbPath     = epc_path("castor_meta_db")
  )
)

# =====================
# Speciale velden: helpers
# =====================
# Zet vrije-tekst duur (bv. "9 maanden", "4 weken", "1 jaar", "12 dagen") om naar weken (1 decimaal)
# Regels:
# - Jaar = 365,2425 dagen; maand = 365,2425/12 dagen; week = 7 dagen; dag = 1 dag
# - Resultaat = (dagen / 7), afgerond op 1 decimaal
parse_duration_to_weeks <- function(x) {
  to_weeks_one <- function(s) {
    if (is.na(s)) return(NA_real_)
    # Als al numeriek is, neem aan dat dit al weken zijn
    if (is.numeric(s)) return(round(s, 1))
    s <- tolower(trimws(as.character(s)))
    if (s == "" || s == "na") return(NA_real_)
  # geen cijfers aanwezig -> leeg laten
  if (!grepl("[0-9]", s)) return(NA_real_)
    # vervang decimale komma door punt
    s <- gsub(",", ".", s, fixed = TRUE)
    # pak eerste numerieke waarde in de string
    num_txt <- sub("^.*?(-?\\d*\\.?\\d+).*$", "\\1", s)
    num <- suppressWarnings(as.numeric(num_txt))
    if (is.na(num)) return(NA_real_)
    # bepaal eenheid
    unit <- if (grepl("dag|dagen", s)) {
      "day"
    } else if (grepl("week|weken", s)) {
      "week"
    } else if (grepl("maand|maanden", s)) {
      "month"
    } else if (grepl("jaar|jaren", s)) {
      "year"
    } else {
      NA_character_
    }
    if (is.na(unit)) return(NA_real_)
    days_per_year <- 365.2425
    days <- switch(unit,
      day   = num * 1,
      week  = num * 7,
      month = num * (days_per_year / 12),
      year  = num * days_per_year,
      NA_real_
    )
    if (is.na(days)) return(NA_real_)
    round(days / 7, 1)
  }
  vapply(x, to_weeks_one, numeric(1))
}

# Centrale plek om speciale veldtransformaties toe te passen (uitbreidbaar)
apply_special_field_transformations <- function(df) {
  if (!all(c("column_name", "value") %in% names(df))) return(df)
  # PBAIG#200 -> elements.csv: symp_onset_of_complaints
  mask <- !is.na(df$column_name) & df$column_name == "symp_onset_of_complaints"
  if (any(mask)) {
    df$value[mask] <- parse_duration_to_weeks(df$value[mask])
  }
  df
}

# Helper function to read a table from the database and adjust column names
readDBTable <- function(conn, tableName) {
  df <- dbReadTable(conn, tableName)
  colnames(df) <- gsub("\\.", " ", colnames(df))
  return(df)
}

# Open database connections
conn_mapping <- dbConnect(SQLite(), dbname = epc_path("mapping_db"))
conn_meta    <- dbConnect(SQLite(), dbname = epc_path("castor_meta_db"))

# Load mapping information from the mapping database
variabelenAll      <- readDBTable(conn_mapping, "variabelen")
elements           <- readDBTable(conn_mapping, "elements")
waarde_checkboxes  <- readDBTable(conn_mapping, "waarde_checkboxes")
waarde_radiobuttons<- readDBTable(conn_mapping, "waarde_radiobuttons")

# Load meta information from the castor_meta database
metaRadioButtons <- readDBTable(conn_meta, "field_options")
# Select the required columns
metaRadioButtons <- metaRadioButtons[, c("Option Group Name", "Option Name", "Option Value", "Option Group Id")]
metaRadioButtons$`Option Value` <- as.integer(metaRadioButtons$`Option Value`)

metaVariables <- readDBTable(conn_meta, "study_variablelist")
metaVariables <- metaVariables[, c("Form Name", "Form Order", "Field Option Group", "Field Variable Name", "Field Type")]

# Replace column names in metaVariables with the correct information; use the lookup from metaRadioButtons
lookup <- metaRadioButtons[, c("Option Group Id", "Option Group Name")]
lookup <- lookup[order(lookup$`Option Group Id`), ]
metaVariables$`Field Option Group` <- lookup$`Option Group Name`[match(metaVariables$`Field Option Group`, lookup$`Option Group Id`)]

# NOTE: Field Variable Names in meta are used zonder form-prefix.

# Sort metaVariables by Form Order
metaVariables <- metaVariables[order(metaVariables$`Form Order`), ]

# Remove unnecessary columns from metaRadioButtons and metaVariables
metaRadioButtons <- metaRadioButtons[, -ncol(metaRadioButtons)]
metaVariables <- metaVariables[, !(names(metaVariables) %in% c("Form Name", "Form Order"))]

# Create a translation table for radio button values
radioButtonOptionValues <- metaVariables[metaVariables$`Field Type` == "radio", ]
radioButtonOptionValues <- merge(x = radioButtonOptionValues,
                                 y = metaRadioButtons,
                                 by.x = "Field Option Group",
                                 by.y = "Option Group Name")
radioButtonOptionValues <- radioButtonOptionValues[, c("Field Variable Name", "Option Name", "Option Value")]

allTables <- list()

scheme <- unique(variabelenAll[, c("epic_tabel", "castor_tabel")])
# Now we know how many tables to process; update total for better ETA (force write for immediate UI update)
try(epic2castor_status_update(total = nrow(scheme), step = "prepare", detail = sprintf("Found %d table(s)", nrow(scheme)), force = TRUE), silent = TRUE)

# Hoofdlogica in een tryCatch-blok om fouten af te vangen en te rapporteren
tryCatch({
  for (table in seq_len(nrow(scheme))){
    # Announce the file we're about to load (force write so UI reflects step immediately)
    try(epic2castor_status_update(step = "read", detail = sprintf("Loading %s (%d/%d)", scheme[table,]$epic_tabel, table, nrow(scheme)), current = table - 1, force = TRUE), silent = TRUE)
    bestandXLSX <- paste0(inputPath, scheme[table,]$epic_tabel, ".xlsx")
    bestandCSV  <- paste0(inputPath, scheme[table,]$epic_tabel, ".csv")
    
    if (file.exists(bestandXLSX)) {
      inputData <- readxl::read_excel(bestandXLSX)
    } else if (file.exists(bestandCSV)) {
      inputData <- readr::read_csv2(
        bestandCSV, 
        col_types = cols(), 
        locale = locale(decimal_mark = ",", grouping_mark = ".")
      )
    } else {
      # Gebruik gestandaardiseerde error string
      # We geven het CSV-pad mee als de "meest waarschijnlijke" kandidaat voor de gebruiker
      stop(paste0("ERROR:FILE_NOT_FOUND:", bestandCSV))
    }

    # --- Mojibake / encoding reparatie (bronbestanden kunnen al fout zijn) ---
    repair_mojibake <- function(x) {
      if (is.list(x)) x <- unlist(x, use.names = FALSE)
      if (!is.character(x)) return(x)
      if (!any(grepl("Ã|Â", x))) return(x)
      # Veelvoorkomende dubbele interpretaties herstellen
      patterns <- c(
        "Ã¶"="ö","ÃÖ"="Ö","Ã¤"="ä","Ã„"="Ä","Ã¼"="ü","Ãœ"="Ü","ÃŸ"="ß",
        "Ãé"="é","Ãè"="è","Ãê"="ê","Ãë"="ë","Ãá"="á","Ãà"="à","ÃÀ"="À",
        "Ãî"="î","Ãô"="ô","ÃÓ"="Ó","Ãñ"="ñ","Ã§"="ç","Ã¹"="ù","Ãú"="ú",
        "Â·"="·","Â"=""  # resterende losse Â weghalen
      )
      for (p in names(patterns)) {
        x <- gsub(p, patterns[[p]], x, fixed = TRUE)
      }
      # Non-breaking space naar gewone spatie
      x <- gsub("\u00A0", " ", x, useBytes = TRUE)
      trimws(x)
    }

    normalize_columns_text <- function(df){
      df[] <- lapply(df, repair_mojibake)
      df
    }
    inputData <- normalize_columns_text(inputData)
       
    #How to deal with dates?
    ###
    if ("datum" %in% colnames(inputData)){
      if (typeof(inputData$datum) == "character"){
        inputData$datum <- as.Date(inputData$datum, format = "%d-%m-%Y")
      } else {
        inputData$datum <- as.Date(inputData$datum, origin = "1970-01-01")
      }
    } else {
      warning("No datum column")
    }
    
    variabelen <- variabelenAll[variabelenAll$epic_tabel == scheme[table,]$epic_tabel & variabelenAll$castor_tabel == scheme[table,]$castor_tabel,]
    
    #Rename the columns that are not being transposed. This are most of the times also the by variables
    variabelenRename <- variabelen[!is.na(variabelen$epic_kolom) & variabelen$epic_kolom != "", c("epic_kolom", "castor_kolom", "gerelateerde_mapping")]
    variabelenRename$castor_kolom[variabelenRename$gerelateerde_mapping == "elements" & !is.na(variabelenRename$gerelateerde_mapping)] <- "Element"
    variabelenRename$castor_kolom[variabelenRename$gerelateerde_mapping == "waarde_checkboxes|waarde_radiobuttons" & !is.na(variabelenRename$gerelateerde_mapping)] <- "waarde"
     
    old <- variabelenRename$epic_kolom
    new <- variabelenRename$castor_kolom
    rm(variabelenRename)
          
    inputData <- inputData[, old]
    colnames(inputData) <- new
    
    # Verwijder regels zonder Participant Id (lege of NA studiealias)
    if ("Participant Id" %in% names(inputData)) {
      before_n <- nrow(inputData)
      # zorg dat we karaktervergelijken en whitespace trimmen
      pid_chr <- as.character(inputData$`Participant Id`)
      keep <- !is.na(pid_chr) & nzchar(trimws(pid_chr))
      inputData <- inputData[keep, , drop = FALSE]
      removed_n <- before_n - nrow(inputData)
      if (removed_n > 0) warning(sprintf("Regels zonder Participant Id verwijderd: %d", removed_n))
    }
        
    if (
      sum(variabelen$gerelateerde_mapping == "elements", na.rm = TRUE) == 1 
      && sum(variabelen$gerelateerde_mapping == "waarde_checkboxes|waarde_radiobuttons", na.rm = TRUE) == 1
    ){
          
      #Join mapping data to epic data. By joining the information is added needed for the transponation.
      inputData <- merge(x = inputData, 
        y = elements, 
        by = "Element", 
        all = FALSE
      )
          
      inputData <- merge(x = inputData, 
        y = waarde_checkboxes, 
        by = c("Element", "waarde"), 
        all.x = TRUE
      )
    
      inputData <- merge(x = inputData, 
        y = waarde_radiobuttons, 
        by = c("Element", "waarde"), 
        all.x = TRUE
      )
       
      # Match radiobuttons op on-geprefixte veldnaam (castor_kolom zonder deel vóór eerste underscore)
      inputData$field_var_name <- sub("^[^_]*_", "", inputData$castor_kolom)
      inputData <- merge(x = inputData,
        y = radioButtonOptionValues,
        by.x = c("field_var_name", "castor_waarde"),
        by.y = c("Field Variable Name", "Option Name"), all.x = TRUE
      )

      #For long to wide transformation you need try things, by vars, a column with the new columns names, and a column with the values
          
      #By variables are already in the long file
          
      #Create column with the new column names
      inputData$column_name <- ifelse(
        !is.na(inputData$kolom_toevoeging), 
        paste0(inputData$castor_kolom, "#", inputData$kolom_toevoeging), 
        inputData$castor_kolom 
      )
          
      #Create value column
      inputData$value <- ifelse(
        !is.na(inputData$kolom_toevoeging), 
        1, 
        inputData$waarde 
      )
          
      #Create value column
      inputData$value <- ifelse(
        !is.na(inputData$`Option Value`), 
        inputData$`Option Value`, 
        inputData$value 
      )

    # Speciale velden toepassen (bv. PBAIG#200: 'Aanvang klachten' -> weken)
    inputData <- apply_special_field_transformations(inputData)
             
      byVars <- variabelen$castor_kolom[variabelen$key == "TRUE" & !is.na(variabelen$key)]
      if (any(duplicated(inputData[, c(byVars, "column_name")]))){
        #inputData <- inputData[!duplicated(inputData[,c(byVars, "Element")]),]
        duplicatedData <- inputData[duplicated(inputData[,c(byVars, "column_name")]),]
        warning(paste0("Duplicated data found for elements: ", paste0(unique(duplicatedData$Element), collapse = " ")))
        inputData <- as.data.table(inputData)[,
          .(value = {
            # werk altijd met character zodat type consistent is
            v_chr <- as.character(value)
            # vervang NA door lege string en trim
            v_chr[is.na(v_chr)] <- ""
            v_chr <- trimws(v_chr)
            # filter lege waarden
            v_chr <- v_chr[nzchar(v_chr)]
            # combineer met ';' zodat multi-waarden niet aan elkaar plakken ("3;1" i.p.v. "31")
            if (length(v_chr) == 0) NA_character_ else paste(v_chr, collapse = ";")
          }),
          by = c(byVars, "column_name")
        ]
        rm(duplicatedData)
      }else{
        inputData <- inputData[,c(byVars, "column_name", "value")]
      }
          
      expression <- formula(paste0(paste0(paste0("`",byVars, "`"), collapse = " + ")," ~ column_name"))
      outputData <- as.data.frame(dcast(as.data.table(inputData), formula = expression, value.var = "value"))

      # Zorg dat symp_onset_of_complaints lege cel blijft (geen letterlijke "NA")
      if ("symp_onset_of_complaints" %in% names(outputData)) {
        soc <- outputData[["symp_onset_of_complaints"]]
        # werk met character-vergelijking; zet lege strings en "NA" varianten naar NA
        soc_chr <- as.character(soc)
        soc_chr[trimws(tolower(soc_chr)) %in% c("na", "")] <- NA_character_
        outputData[["symp_onset_of_complaints"]] <- soc_chr
      }
          
      #Check box columns do only have 1 and NA where it should be 0. Set NA to 0 for these columns
      checkboxColumns <- colnames(outputData)[grepl("#", colnames(outputData))]
      outputData[, checkboxColumns] <- lapply(checkboxColumns, function(x) {
        ifelse(is.na(outputData[[x]]), 0, outputData[[x]])
      })

    checkboxLookups <- list()
    checkboxMeta <- metaVariables[metaVariables$`Field Type` == "checkbox", ]
      for (i in seq_len(nrow(checkboxMeta))) {
    fieldName <- checkboxMeta$`Field Variable Name`[i]  # on-geprefixte naam uit meta
        # The field has a Field Option Group, which after lookup becomes an Option Group Name
        optionGroup <- checkboxMeta$`Field Option Group`[i]
        # Search in metaRadioButtons for all options for this field (match on Option Group Name)
        mapping <- metaRadioButtons[metaRadioButtons$`Option Group Name` == optionGroup, ]
        # Create a named vector: names = Option Name, value = Option Value
        if (nrow(mapping) > 0) {
          lookupMap <- setNames(mapping$`Option Value`, mapping$`Option Name`)
          checkboxLookups[[fieldName]] <- lookupMap
        }
      }

      # Combine by checkbox group (fill column before the '#' in the column name)
      checkboxGroups <- unique(gsub("#.*", "", checkboxColumns))
      for (group in checkboxGroups) {
        # Find the columns that belong to this checkbox field
        groupCols <- grep(paste0("^", group, "#"), colnames(outputData), value = TRUE)
        # On-geprefixte veldnaam voor lookup in meta
        groupShort <- sub("^[^_]*_", "", group)
        outputData[[group]] <- apply(outputData[, groupCols, drop = FALSE], 1, function(x) {
          # For each column in the group: if the value is 1, look up the corresponding Option Value.
          selected <- sapply(seq_along(x), function(i) {
            if (x[i] == 1) {
              # Retrieve the checkbox option (the part after "#")
              optionName <- sub(".*#", "", groupCols[i])
              if (!is.null(checkboxLookups[[groupShort]]) && optionName %in% names(checkboxLookups[[groupShort]])) {
                return(checkboxLookups[[groupShort]][[optionName]])
              } else {
                # Fallback: if there is no mapping, use the number indicating the position
                return(i)
              }
            } else {
              return(NULL)
            }
          })
          selected <- unlist(selected)
          if (length(selected) == 0) return(NA_character_)  # Returns NA so fwrite writes it as an empty cell
          if (length(selected) == 1) return(as.character(selected))
          # If there are multiple values, combine them with a semicolon and surround with double quotes.
          combined <- paste(selected, collapse = ";")
          return(combined)
        })
      }

      # If original checkbox columns no longer needed, remove them:
      outputData[, checkboxColumns] <- NULL

      # If some checkbox answers are not in data, add with value 0
      waarde_checkboxes_add <- merge(x = waarde_checkboxes,
        y = elements,
        by = "Element"
      )  
            
      fixedValues <- variabelen[!is.na(variabelen$vaste_waarde),]
      
      for (i in seq_len(nrow(fixedValues))){
        kolomNaam <- unlist(fixedValues[i, "castor_kolom"])
        vasteWaarde <- unlist(fixedValues[i, "vaste_waarde"])
        if (!is.null(vasteWaarde) && vasteWaarde != "") {
          outputData[[kolomNaam]] <- vasteWaarde
        }
      } 
         
      if (any(variabelen$repeating == "TRUE")){
        
        if (sum(variabelen$repeating == "TRUE", na.rm = TRUE) > 1){
          stop("Only allowed to have 1 column per output table for option repeating. Please check your variables in the mapping file")
        }
        
        colRep <- variabelen$castor_kolom[variabelen$repeating == "TRUE" & !is.na(variabelen$repeating)]
         
        #Note data_baseline maybe to be a variable in future (possible custom value in app) since it is user defined
        outputData <- outputData[do.call(order, outputData[, byVars]), ]
        outputData$date_baseline <- format(outputData$date_baseline, format = "%d-%m-%Y")
        outputData[[colRep]] <- paste0(outputData[[colRep]], " - ", 
                                       ave(seq_len(nrow(outputData)), 
                                           outputData$`Participant Id`, 
                                           FUN = seq_along))
        outputData[["Repeating Data Creation Date"]] <- as.character(format(Sys.time(), "%d-%m-%Y %H %M"))
        rm(colRep, fixedValues, byVars, expression, waarde_checkboxes_add)
        gc()
      }
    }else{
      outputData <- inputData
    }

    # --- Nieuwe aggregatie: per deelnemer slechts één rij (baseline) ---
    if (identical(scheme[table, ]$castor_tabel, "baseline") && "Participant Id" %in% names(outputData)) {
      dt_tmp <- as.data.table(outputData)

      # --- Bepaal radiobutton-kolommen en kies waarde van laatste datum ---
      radio_unprefixed <- metaVariables$`Field Variable Name`[metaVariables$`Field Type` == "radio"]
      # Kolomnamen in output hebben vaak een prefix voor het eerste '_'; match op on-geprefixte naam
      radio_cols <- Filter(function(cn){
        short <- sub("^[^_]*_", "", cn)
        short %in% radio_unprefixed
      }, setdiff(colnames(dt_tmp), c("Participant Id", "date_baseline")))

      radio_last_values <- NULL
      if (length(radio_cols)) {
        # Maak parsebare datumkolom
        dt_tmp[, date_baseline_chr := as.character(date_baseline)]
        dt_tmp[, parsed_date := suppressWarnings(as.Date(date_baseline_chr, format = "%d-%m-%Y"))]
        # Sorteer zodat laatste visit per participant onderaan komt (NA-datums worden als laatste geplaatst)
        # data.table::setorder accepteert geen expressies; gebruik hulpkolom voor is.na(parsed_date)
        dt_tmp[, parsed_na := is.na(parsed_date)]
        setorder(dt_tmp, `Participant Id`, parsed_na, parsed_date, date_baseline_chr)
        dt_tmp[, parsed_na := NULL]
        # Pak laatste rij per participant
        last_rows <- dt_tmp[, .SD[.N], by = `Participant Id`]
        # Houd alleen radio kolommen vast en maak single-token (eerste token bij samengestelde waarde)
        keep_cols <- c("Participant Id", radio_cols)
        radio_last_values <- last_rows[, ..keep_cols]
        for (rc in radio_cols) {
          radio_last_values[[rc]] <- as.character(radio_last_values[[rc]])
          radio_last_values[[rc]][is.na(radio_last_values[[rc]])] <- ""
          # Neem eerste niet-lege token per veld
          radio_last_values[[rc]] <- vapply(strsplit(radio_last_values[[rc]], ";", fixed = TRUE), function(x){
            x <- trimws(x)
            x <- x[nzchar(x)]
            if (length(x)) x[1] else NA_character_
          }, character(1))
        }
      }

      # Hulpfunctie voor niet-datum kolommen: unieke niet-lege waarden behouden
      collapse_unique <- function(v) {
        if (is.list(v)) v <- unlist(v, use.names = FALSE)
        v <- v[!is.na(v)]
        v_chr <- trimws(as.character(v))
        v_chr <- v_chr[nzchar(v_chr)]
        if (!length(v_chr)) return(NA_character_)
        # Split samengestelde waarden op ';'
        splitted <- strsplit(v_chr, ";", fixed = TRUE)
        flat <- trimws(unlist(splitted, use.names = FALSE))
        flat <- flat[nzchar(flat)]
        if (!length(flat)) return(NA_character_)
        # Sorteer numeriek indien alles numeriek te parsen is, anders lexicografisch
        nums <- suppressWarnings(as.numeric(flat))
        if (!any(is.na(nums))) {
          ord <- order(nums, flat)
          uniq <- unique(flat[ord])
        } else {
          uniq <- unique(flat)
          uniq <- uniq[order(tolower(uniq), uniq)]
        }
        paste(uniq, collapse = ";")
      }

      # Bouw aggregatie expressie
      other_cols <- setdiff(colnames(dt_tmp), c("Participant Id", "date_baseline"))

      agg_list <- dt_tmp[, {
        # Combineer datums: uniek, sorteer chronologisch (probeer te parsen), daarna samenvoegen met ';'
        db_raw <- unique(na.omit(as.character(date_baseline)))
        # Converteer naar Date indien mogelijk (dd-mm-YYYY formaat), behoud onparseerbare strings op einde
        parsed <- suppressWarnings(as.Date(db_raw, format = "%d-%m-%Y"))
        order_idx <- order(ifelse(is.na(parsed), as.Date("2100-01-01"), parsed), db_raw)
        db_sorted <- db_raw[order_idx]
db_baseline <- paste(db_sorted, collapse = ",")
        # Andere kolommen
        vals <- lapply(.SD, collapse_unique)
        c(list(date_baseline = db_baseline), vals)
      }, by = .(`Participant Id`), .SDcols = other_cols]

      # Kolommen die specifiek waren voor repeating constructie kunnen worden verwijderd
      drop_cols <- intersect(c("Repeating Data Creation Date", "Repeating data Name Custom"), names(agg_list))
      if (length(drop_cols)) agg_list[, (drop_cols) := NULL]

      outputData <- as.data.frame(agg_list)
      # Overschrijf radio-kolommen met 'laatste datum' keuze
      if (!is.null(radio_last_values) && nrow(radio_last_values)) {
        # Zorg dat de beoogde radio kolommen ook in outputData bestaan (na aggregatie)
        apply_cols <- intersect(radio_cols, colnames(outputData))
        if (length(apply_cols)) {
          idx <- match(outputData$`Participant Id`, radio_last_values$`Participant Id`)
          for (rc in apply_cols) {
            outputData[[rc]] <- radio_last_values[[rc]][idx]
          }
        }
      }
      rm(dt_tmp, agg_list, other_cols, collapse_unique)
    }
    
    # Enforce column order: keys first, then castor fields in the order of mapping/elements.csv, then any remaining columns
    {
      keys <- variabelen$castor_kolom[variabelen$key == "TRUE" & !is.na(variabelen$key)]
      # Preserve the order as it appears in elements.csv
      desired_from_elements <- unique(elements$castor_kolom)
      desired <- c(keys, desired_from_elements[desired_from_elements %in% colnames(outputData)])
      rest <- setdiff(colnames(outputData), desired)
      outputData <- outputData[, c(desired, rest), drop = FALSE]
    }
                 
    allTables[[scheme[table,]$castor_tabel]] <- outputData
    fwrite(as.data.table(outputData), 
      paste0(outputFolder, scheme[table,]$castor_tabel, ".csv"), 
      sep = ";", 
      na = "", 
      bom = TRUE)
    cat("Successfully saved: ", outputFolder, scheme[table,]$castor_tabel, ".csv\n", sep = "")
    # Update progress to this completed table (force write for UI)
    try(epic2castor_status_update(step = "write", detail = paste0("Saved ", scheme[table,]$castor_tabel, ".csv"), current = table, force = TRUE), silent = TRUE)
    rm(outputData, inputData, old, new, variabelen)
    gc()          
    
  }
}, error = function(e) {
  # Bij een fout, schrijf de foutmelding naar de status en stop het script met een error-code
  err_msg <- conditionMessage(e)
  
  # Schrijf de onbewerkte foutmelding naar BEIDE stdout en stderr voor de app
  # Gebruik writeLines en flush expliciet zodat Shiny de output ontvangt
  writeLines(err_msg, con = stderr())
  flush(stderr())
  writeLines(err_msg, con = stdout())
  flush(stdout())
  
  # Update de status voor de UI met een meer leesbare melding
  if (startsWith(err_msg, "ERROR:FILE_NOT_FOUND:")) {
    display_msg <- paste("Baseline script mislukt: een benodigd inputbestand kon niet worden gevonden.")
  } else {
    display_msg <- paste("Baseline script mislukt:", err_msg)
  }
  
  try(epic2castor_status_update(step = "error", detail = display_msg, force = TRUE), silent = TRUE)
  
  # Stop het script met een non-zero exit code
  quit(status = 1, save = "no")
})

# Disconnect from the databases
dbDisconnect(conn_mapping)
dbDisconnect(conn_meta)
cat("EPIC2CASTOR::DONE\n")

# Bepaal loglocatie en meld dit zowel in status (voor modal) als in stdout/log
try({
  logfile <- getOption("epic2castor.logger.file", NULL)
  if (is.null(logfile) || !nzchar(logfile)) {
    run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
    script_base <- "baseline"
    if (nzchar(run_dir)) logfile <- file.path(run_dir, paste0(script_base, "_log.txt"))
    else logfile <- paste0(script_base, "_log.txt")
  }
  # Update de UI-status met het logpad en print ook naar stdout
  try(epic2castor_status_done(paste0("Baseline finished — Log saved to ", logfile)), silent = TRUE)
}, silent = TRUE)

# Expliciete cleanup om Windows crash bij exit te voorkomen
try({
  rm(list = setdiff(ls(), c()))
  gc(verbose = FALSE, full = TRUE)
}, silent = TRUE)

# Flush alle output streams voor we exit
flush(stdout())
flush(stderr())

# Expliciete normale exit
quit(status = 0, save = "no")
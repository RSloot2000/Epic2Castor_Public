library(data.table)
library(DBI)
library(RSQLite)

# Paden centraliseren via scripts/paths.json
paths <- jsonlite::fromJSON(file.path("config", "paths.json"))
mapping_dir <- paths$mapping_dir
mapping_db <- paths$mapping_db
castor_meta_dir <- paths$castor_meta_dir
castor_meta_db <- paths$castor_meta_db
mapping_possible_values_dir <- if (!is.null(paths$mapping_possible_values_dir)) paths$mapping_possible_values_dir else file.path(mapping_dir, "possibleValues")

# Genereer mapping/possibleValues/pv_elements.csv uit castor_meta/study_variablelist.csv
# - Groepeer eerst op Form Collection Name in de volgorde zoals ze in de CSV voorkomen
# - Sorteer binnen elke groep op Form Order, daarna Field Order
# - Waarde = tolower(substr(Form Name,1,4)) _ Field Variable Name
generate_pv_elements <- function(inFile = file.path(castor_meta_dir, "study_variablelist.csv"),
                                 outDir = mapping_possible_values_dir) {
  # Lees study variable list
  svl <- tryCatch(
    fread(inFile, sep = ";", colClasses = "character", strip.white = TRUE),
    error = function(e) NULL
  )
  if (is.null(svl)) return(invisible(FALSE))

  # Vereiste kolommen
  required_cols <- c("Form Collection Name", "Form Name", "Field Variable Name")
  if (!all(required_cols %in% names(svl))) return(invisible(FALSE))

  # Filter lege veldnamen
  keep <- !is.na(svl[["Field Variable Name"]]) & nzchar(svl[["Field Variable Name"]])
  svl <- svl[keep, ]
  if (!nrow(svl)) return(invisible(FALSE))

  # Index voor Form Collection Name in volgorde van voorkomen
  fc_levels <- unique(svl[["Form Collection Name"]])
  fc_idx <- match(svl[["Form Collection Name"]], fc_levels)

  # Orders numeriek maken indien aanwezig
  form_order  <- suppressWarnings(as.integer(if ("Form Order"  %in% names(svl)) svl[["Form Order"]]  else NA_integer_))
  field_order <- suppressWarnings(as.integer(if ("Field Order" %in% names(svl)) svl[["Field Order"]] else NA_integer_))

  # Prefix samenstellen uit Form Name
  prefix <- tolower(substr(svl[["Form Name"]], 1, 4))
  castor_names <- paste0(prefix, "_", svl[["Field Variable Name"]])

  # Sorteervolgorde: Form Collection Name volgorde, daarna Form Order, daarna Field Order
  ord <- order(fc_idx, form_order, field_order, na.last = TRUE)
  castor_names <- castor_names[ord]

  # Uniek met behoud van eerste
  castor_names <- castor_names[!duplicated(castor_names)]

  # Schrijf naar CSV
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(outDir, "pv_elements.csv")
  fwrite(data.table(castor_kolom = castor_names), out_path, sep = ";")
  invisible(TRUE)
}

csv_to_database <- function(dataFolder = mapping_dir, dbPath = mapping_db) {
  csvFiles <- list.files(dataFolder, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
  con <- dbConnect(SQLite(), dbPath)
  
  for(file in csvFiles) {
    if (grepl("possibleValues", dirname(file))) next
    tableName <- sub("\\.csv$", "", basename(file))
    dt <- fread(file, sep = ";", colClasses = "character", strip.white = TRUE)
    
    # FASE 6: Geen filtering nodig - tab_name_meta en tab_order_meta zijn normale kolommen
    
    dbWriteTable(con, tableName, dt, overwrite = TRUE)
  }
  
  possibleFolder <- mapping_possible_values_dir
  if (dir.exists(possibleFolder)) {
    csvFiles2 <- list.files(possibleFolder, pattern = "\\.csv$", full.names = TRUE)
    for(file in csvFiles2) {
      tableName <- if (basename(file) == "pv_elements.csv") {
                     "possibleValues_Elements"
                   } else {
                     paste0("possibleValues_", sub("\\.csv$", "", basename(file)))
                   }
      dt <- fread(file, sep = ";", colClasses = "character", strip.white = TRUE)
      
      # FASE 6: Geen filtering nodig - tab_name_meta en tab_order_meta zijn normale kolommen
      
      dbWriteTable(con, tableName, dt, overwrite = TRUE)
    }
  }
  
  dbDisconnect(con)
}

database_to_csv <- function(dataFolder = mapping_dir, dbPath = mapping_db) {
  con <- dbConnect(SQLite(), dbPath)
  table_names <- dbListTables(con)
  
  # Schrijf possibleValues_Elements (bron: pv_elements.csv uit Castor meta) niet terug om overschrijven te voorkomen
  table_names <- setdiff(table_names, c("possibleValues_pv_elements", "possibleValues_Elements"))
  
  possibleFolder <- mapping_possible_values_dir
  dir.create(possibleFolder, showWarnings = FALSE, recursive = TRUE)
  
  for (tbl in table_names) {
    dt <- as.data.table(dbReadTable(con, tbl))
    
    if (tbl == "possibleValues_Elements") {
      filePath <- file.path(possibleFolder, "pv_elements.csv")
    } else if (startsWith(tbl, "possibleValues_")) {
      fileName <- paste0(substr(tbl, 15, nchar(tbl)), ".csv")
      filePath <- file.path(possibleFolder, fileName)
    } else {
      filePath <- file.path(dataFolder, paste0(tbl, ".csv"))
    }
    
    fwrite(dt, filePath, sep = ";")
  }
  
  dbDisconnect(con)
}

csv_to_database_meta <- function(dataFolder = castor_meta_dir, dbPath = castor_meta_db) {
  csvFiles <- list.files(dataFolder, pattern = "\\.csv$", full.names = TRUE)
  con <- dbConnect(SQLite(), dbPath)
  
  for(file in csvFiles) {
    tableName <- sub("\\.csv$", "", basename(file))
    dt <- fread(file, sep = ";", colClasses = "character", strip.white = TRUE)
    dbWriteTable(con, tableName, dt, overwrite = TRUE)
  }
  
  dbDisconnect(con)
}
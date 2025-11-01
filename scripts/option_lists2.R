## Paden centraliseren via scripts/paths.json
paths <- jsonlite::fromJSON("config/paths.json")

# Kleine helper om paden te bouwen (OS-onafhankelijk)
path_join <- function(...) file.path(...)

# Afgeleide pad-variabelen uit paths.json
dir_scripts <- paths$scripts_dir
dir_input_epic <- paths$epic_input_data_dir
dir_output_example <- paths$output_data_example_dir
dir_mapping <- paths$mapping_dir
dir_castor_meta <- paths$castor_meta_dir
dir_castor_export <- paths$castor_export_dir

# Optionele/nieuw toegevoegde sleutels (met veilige fallback indien ontbreken)
file_castor_field_options <- if (!is.null(paths$castor_field_options_file)) paths$castor_field_options_file else path_join(dir_castor_meta, "field_options.csv")
file_castor_study_variablelist <- if (!is.null(paths$castor_study_variablelist_file)) paths$castor_study_variablelist_file else path_join(dir_castor_meta, "study_variablelist.csv")
file_mapping_variabelen <- if (!is.null(paths$mapping_variabelen_file)) paths$mapping_variabelen_file else path_join(dir_mapping, "variabelen.csv")
dir_mapping_possible_values <- if (!is.null(paths$mapping_possible_values_dir)) paths$mapping_possible_values_dir else path_join(dir_mapping, "possibleValues")

force_option_reload <- isTRUE(getOption("epic2castor.force_option_reload", FALSE))
cache <- getOption("epic2castor.option_cache", NULL)
cached_loaded <- !force_option_reload && !is.null(cache)
if (cached_loaded) {
  option_data <- cache$option_data
  checkBoxesValues <- cache$checkBoxesValues
  radioButtonOptionValues <- cache$radioButtonOptionValues
  checkboxes <- cache$checkboxes
  radiobuttons <- cache$radiobuttons
  metaRadioButtons <- cache$metaRadioButtons
  metaVariables <- cache$metaVariables
}

capitalize_first <- function(x) {
  if (is.na(x) || length(x) == 0 || nchar(x) == 0) return(x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# Helper functies voor het lezen van data
read_data <- function(file_path, ...) {
  result <- NULL
  if (grepl("\\.csv$", file_path)) {
    result <- fread(file_path, colClasses = "character", strip.white = TRUE, ...)
  } else if (grepl("\\.xlsx$", file_path)) {
    result <- as.data.table(readxl::read_excel(file_path, ...))
  }
  
  if (!is.null(result)) {
    # Normaliseer waarden in specifieke kolommen waar hoofdletters belangrijk zijn
    # Focus op de relevante kolommen voor dropdown-opties
    value_columns <- c("waarde", "Waarde", "Element")
    for (col in names(result)) {
      if (col %in% value_columns && is.character(result[[col]])) {
        result[[col]] <- sapply(result[[col]], capitalize_first)
      }
    }
  }
  
  return(result)
}

# Configuratie opbouwen
if (!cached_loaded) {
  config <- do.call(rbind, lapply(
    list.files(dir_mapping, full.names = TRUE, pattern = "\\.csv$"), 
    function(file_path) {
      cols <- colnames(read_data(file_path, nrows = 0))
      data.frame(
        table = rep(file_path, length(cols)),
        col = cols,
        option_file = NA_character_,
        option_col = NA_character_,
        option_col_match_list = NA_character_,
        option_col_match_table = NA_character_,
        type = NA_character_,
        custom = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  ))

  # Specifieke configuraties instellen
  config[config$col == "castor_kolom", "option_file"] <- file_castor_study_variablelist
  config[config$col == "castor_kolom", "option_col"] <- "Field Variable Name"

  config[config$col == "epic_tabel", "option_file"] <- dir_input_epic
  config[config$col == "epic_tabel", "type"] <- "elementen"

  config[config$col == "epic_kolom", "option_file"] <- dir_input_epic
  config[config$col == "epic_kolom", "type"] <- "kolommen"

  config[config$col == "castor_tabel", "option_file"] <- dir_castor_export
  config[config$col == "castor_tabel", "type"] <- "tabellen"

  # Geen fallback meer op output_data_example voor castor_kolom; enkel Castor meta gebruiken

  config[config$col == "key", "custom"] <- "TRUE"
  config[config$col == "repeating", "custom"] <- "TRUE"
  config[config$col == "gerelateerde_mapping", "custom"] <- "waarde_checkboxes|waarde_radiobuttons/elements"

  # Element en waarde configuraties
  element_rows <- which(config$col == "Element")
  waarde_rows <- which(config$col == "waarde")
  castor_waarde_rows <- which(config$col == "castor_waarde")
  kolom_toevoeging_rows <- which(config$col == "kolom_toevoeging")

  if (length(element_rows) >= 1) {
    config[element_rows[1], "option_file"] <- dir_input_epic
    config[element_rows[1], "type"] <- "elementen"
  }

  if (length(waarde_rows) >= 1) {
    config[waarde_rows[1], "option_file"] <- dir_input_epic
    config[waarde_rows[1], "type"] <- "waarden"
    config[waarde_rows[1], "option_col_match_table"] <- "Element"
    config[waarde_rows[1], "option_col_match_list"] <- "Element"
  }

  if (length(castor_waarde_rows) >= 1) {
    config[castor_waarde_rows[1], "option_file"] <- file_castor_field_options
    config[castor_waarde_rows[1], "option_col"] <- "Option Name"
  }

  if (length(element_rows) >= 2) {
    config[element_rows[2], "option_file"] <- dir_input_epic
    config[element_rows[2], "type"] <- "elementen"
  }

  if (length(waarde_rows) >= 2) {
    config[waarde_rows[2], "option_file"] <- dir_input_epic
    config[waarde_rows[2], "type"] <- "waarden"
    config[waarde_rows[2], "option_col_match_table"] <- "Element"
    config[waarde_rows[2], "option_col_match_list"] <- "Element"
  }

  if (length(kolom_toevoeging_rows) >= 1) {
    config[kolom_toevoeging_rows[1], "option_file"] <- file_castor_field_options
    config[kolom_toevoeging_rows[1], "option_col"] <- "Option Name"
  }

  # Functie om opties te genereren
  generate_options <- function(config_row) {
    # Custom opties
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
    
    # Type-specifieke opties
    if (!is.na(config_row$type) && config_row$type == "tabellen") {
      files <- list.files(config_row$option_file, full.names = TRUE)
      return(list(
        table = NA,
        match = NA,
        options = tools::file_path_sans_ext(basename(files))
      ))
    } 
    
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
    
    # Voor waarden, gebruik een andere aanpak om element-waarde paren te maken
    if (!is.na(config_row$type) && config_row$type == "waarden") {
      # Lees eerst de variabelen data in
      vars <- read_data(file_mapping_variabelen, 
                      select = c("epic_tabel", "gerelateerde_mapping", "epic_kolom"))
      vars <- unique(vars[vars$gerelateerde_mapping == "waarde_checkboxes|waarde_radiobuttons", ])
      
      element_value_pairs <- data.frame(element = character(0), value = character(0))

      for (file in list.files(dir_input_epic, full.names = TRUE)) {
        file_name <- tools::file_path_sans_ext(basename(file))
        column <- vars$epic_kolom[vars$epic_tabel == file_name]
        
        if (length(column) != 1) next
        
        data <- read_data(file)
        
        # Controleer of benodigde kolommen bestaan
        if (!all(c(column, "Element") %in% colnames(data))) next
        
        # Haal alle element-waarde paren op
        pairs <- data[, c("Element", column), with = FALSE]
        colnames(pairs) <- c("element", "value")
        
        # Normaliseer de waarden expliciet (voor het geval ze nog niet genormaliseerd zijn)
        pairs$value <- sapply(pairs$value, capitalize_first)
        
        # Voeg toe aan de verzameling
        element_value_pairs <- rbind(element_value_pairs, pairs)
      }
      
      # Verwijder duplicaten
      element_value_pairs <- unique(element_value_pairs)
      
      return(list(
        options = element_value_pairs$value,
        match = element_value_pairs$element,
        match_col = "Element",
        table = NA
      ))
    }
    
    # input_data opties voor andere types
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
    
    # Specifiek bestand opties
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
    
    # Default return als geen conditie waar is
    return(list(table = NA, match = NA, options = character(0)))
  }

  # Opties genereren met foutafhandeling
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


  # Elements|castor_kolom toevoegen
  tryCatch({
    option_data[["elements|castor_kolom"]] <- list(
      table = "elements",
      match = NA,
      options = read_data(path_join(dir_mapping_possible_values, "pv_elements.csv"), sep = ";")$castor_kolom
    )
  }, error = function(e) {
    warning(paste("Error adding elements|castor_kolom:", e$message))
  })

  # Castor metadata inlezen
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
      `Option Group Id` = character(0)
    )
  })

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
      `Field Type` = character(0)
    )
  })

  # Checkbox en radiobutton identificatie
  dt <- tryCatch({
    read_data(file_castor_study_variablelist, sep = ";")
  }, error = function(e) {
    warning(paste("Error reading study_variablelist.csv for checkboxes:", e$message))
    data.table()
  })

  checkboxes <- character(0)
  radiobuttons <- character(0)

  if (nrow(dt) > 0 && "Field Type" %in% names(dt)) {
    checkbox_dt <- dt[dt$`Field Type` == "checkbox", ]
    if (nrow(checkbox_dt) > 0 && "Form Name" %in% names(checkbox_dt) && "Field Variable Name" %in% names(checkbox_dt)) {
      checkboxes <- paste0(
        tolower(substr(checkbox_dt$`Form Name`, 1, 4)), 
        "_", 
        checkbox_dt$`Field Variable Name`
      )
    }
    
    radio_dt <- dt[dt$`Field Type` == "radio", ]
    if (nrow(radio_dt) > 0 && "Form Name" %in% names(radio_dt) && "Field Variable Name" %in% names(radio_dt)) {
      radiobuttons <- paste0(
        tolower(substr(radio_dt$`Form Name`, 1, 4)), 
        "_", 
        radio_dt$`Field Variable Name`
      )
    }
  }

  # Option Group ID naar Name omzetten
  if (nrow(metaRadioButtons) > 0 && nrow(metaVariables) > 0 && 
      "Option Group Id" %in% names(metaRadioButtons) && 
      "Field Option Group" %in% names(metaVariables)) {
    
    lookup <- metaRadioButtons[, c("Option Group Id", "Option Group Name")]
    lookup <- lookup[order(lookup$`Option Group Id`), ]
    metaVariables$`Field Option Group` <- lookup$`Option Group Name`[
      match(metaVariables$`Field Option Group`, lookup$`Option Group Id`)
    ]
    
    # Prefix toevoegen
    if ("Form Name" %in% names(metaVariables) && "Field Variable Name" %in% names(metaVariables)) {
      metaVariables$`Field Variable Name` <- paste0(
        tolower(substr(metaVariables$`Form Name`, 1, 4)), 
        "_", 
        metaVariables$`Field Variable Name`
      )
    }
    
    # Sorteren en kolommen verwijderen
    if ("Form Order" %in% names(metaVariables)) {
      metaVariables <- metaVariables[order(metaVariables$`Form Order`), ]
    }
    
    if ("Option Group Id" %in% names(metaRadioButtons)) {
      metaRadioButtons <- metaRadioButtons[, -which(names(metaRadioButtons) == "Option Group Id")]
    }
    
    if (all(c("Form Name", "Form Order") %in% names(metaVariables))) {
      metaVariables <- metaVariables[, -which(names(metaVariables) %in% c("Form Name", "Form Order"))]
    }
    
    # Radiobutton opties
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
    
    # Checkbox opties
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

}

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
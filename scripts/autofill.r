# ============================================================================
# AUTO-FILL EPIC VALUES - INTELLIGENT TRANSLATION ENGINE
# ============================================================================
# Version: 2025-11-01 (Production Ready)
# 
# Purpose: 
#   Automatically suggest Dutch EPIC values based on English Castor values
#   Uses 7 intelligent matching strategies with confidence scoring
#
# Flow:
#   Input:  Castor Values (English) - pre-filled by user in app
#   Output: EPIC Values (Dutch) - automatically suggested with confidence scores
#   Direction: English → Dutch translation/matching
#
# Usage:
#   source("scripts/autofill.r")
#   results <- process_autofill(
#     table_name = "waarde_radiobuttons", 
#     tab_name = "Main",
#     review_existing = FALSE
#   )
#
# Returns:
#   data.table with columns:
#     - row_id: Database row identifier
#     - Castor_value: English input value
#     - EPIC_value: Suggested Dutch value
#     - strategy: Matching strategy used
#     - confidence: Match confidence (0-100)
#     - Element: Castor element name
#     - table_name: Source table name
# ============================================================================

cat("✓ Loading autofill.r - Production Ready v2025-11-01\n")

library(data.table)
library(stringdist)
library(jsonlite)
library(RSQLite)
library(DBI)

# Load ML autofill module (if available)
tryCatch({
  source("scripts/autofill_ml.r")
  cat("✓ ML autofill module integrated\n")
}, error = function(e) {
  cat("ℹ ML autofill not available:", conditionMessage(e), "\n")
  ML_AVAILABLE <<- FALSE
})

# ===== LOAD CONFIGURATION =====
# Load central path configuration if not already loaded
if (!exists("epc_path")) {
  source("scripts/config.R", local = FALSE)
}

# ============================================================================
# AUTOFILL CONFIGURATION
# ============================================================================

AUTOFILL_CONFIG <- list(
  # ===== CONFIDENCE THRESHOLDS =====
  # Determines how suggestions are handled based on confidence score
  confidence_thresholds = list(
    auto_apply = 85,    # Automatically apply if confidence >= 85%
    suggest = 70,       # Show as suggestion if confidence >= 70%
    log_only = 0        # Log but don't suggest if confidence < 70%
  ),
  
  # ===== PRESERVE PATTERNS =====
  # Values matching these patterns are preserved as-is (no translation)
  preserve_patterns = c(
    "^GOLD",                  # Medical staging: GOLD I, GOLD II, etc.
    "^[0-9]+\\.?[0-9]*$",    # Pure numbers: 1, 2.5, 100
    "^[0-9]+%$",             # Percentages: 50%, 100%
    "^[0-9]+\\+$",           # Numbers with plus: 18+, 65+
    "^>",                     # Greater than: >50, >100
    "^<",                     # Less than: <10, <50
    "^≥",                     # Greater or equal: ≥60
    "^≤"                      # Less or equal: ≤50, ≤100
  ),
  
  # ===== MEDICAL DICTIONARY =====
  # Path to medical terms dictionary for specialized translations
  medical_dict_path = epc_path("medical_terms_dict"),
  
  # ===== TRANSLATION API CONFIGURATION =====
  # External translation services for automatic translation fallback
  translation = list(
    provider = "deepl",                               # Primary provider: "deepl", "mymemory", "libretranslate", "none"
    fallback_chain = c("deepl", "mymemory"),         # Try providers in order if primary fails
    deepl_api_key = NULL,                             # Set via Sys.getenv("DEEPL_API_KEY")
    libretranslate_api_key = NULL,                    # Set via Sys.getenv("LIBRETRANSLATE_API_KEY") if needed
    cache_translations = TRUE,                        # Cache API results to avoid repeated calls
    show_alternatives = TRUE,                         # Show fuzzy matches as alternatives
    min_similarity = 0.70                             # Minimum similarity score for fuzzy matches (0-1)
  ),
  
  # ===== STRATEGY CONFIDENCE WEIGHTS =====
  # Base confidence score for each matching strategy (0-100)
  # Higher weights = more trustworthy strategies
  strategy_weights = list(
    existing_map = 100,           # Already mapped values (highest confidence)
    numeric = 100,                # Numeric values (no translation needed)
    medical_dict = 100,           # Medical dictionary lookup (curated translations)
    compound_dict = 100,          # Compound medical terms (multiple dictionary words)
    cross_element = 95,           # Cross-element matching (learned from other fields)
    api_translation_deepl = 90,   # DeepL API translation (high quality)
    api_translation_libre = 75,   # LibreTranslate API (open source, lower quality)
    fuzzy_match = 75,             # Fuzzy string matching (moderate confidence)
    elimination = 60              # Process of elimination (lowest confidence)
  )
)

# ===== GLOBAL CACHES =====
# Performance optimization: cache frequently accessed data

# Translation cache - stores API translation results
TRANSLATION_CACHE <- new.env()

# DeepL warning flag - prevent repeated warnings about missing API key
DEEPL_WARNING_SHOWN <- FALSE

# Internet connectivity cache - avoid repeated connection checks
INTERNET_STATUS <- list(
  available = NULL,         # NULL = not checked, TRUE = online, FALSE = offline
  last_check = NULL,        # Timestamp of last connectivity check
  check_interval = 300      # Re-check every 5 minutes (seconds)
)

# ===== EPIC EXPORT DATA CACHE =====
# FASE 10.1: Cache EpicExport.csv to avoid repeated file reads
# Improves performance when processing multiple rows
EPIC_EXPORT_CACHE <- list(
  data = NULL,              # Cached data.table with EPIC export data
  file_path = NULL,         # Path to cached file
  file_mtime = NULL,        # Last modified timestamp of file
  loaded_at = NULL          # When cache was loaded (system time)
)

# ============================================================================
# EPIC EXPORT CACHE MANAGER
# ============================================================================

# Load and cache EpicExport.csv data with intelligent refresh
# 
# Performance optimization: Only re-reads file if:
# - Cache is empty
# - File path has changed
# - File has been modified since last load
# 
# @return data.table with EpicExport.csv contents or NULL if error
# 
# Validates file structure:
# - Must contain "Element" and "waarde" columns
# - Must have at least 1 row
# - Must be valid CSV with UTF-8 encoding
# 
# Cache is stored in global EPIC_EXPORT_CACHE with metadata:
# - data: The actual data.table
# - file_path: Path to cached file
# - file_mtime: Last modification time
# - loaded_at: Cache timestamp
load_epic_export_cached <- function() {
  epic_export_path <- epc_path("epic_export_file")
  
  # ===== FILE EXISTENCE CHECK =====
  if (!file.exists(epic_export_path)) {
    autofill_log("EpicExport.csv not found - cache empty", "WARNING")
    return(NULL)
  }
  
  # ===== CACHE VALIDATION =====
  # Check if file has been modified since last load
  current_mtime <- file.info(epic_export_path)$mtime
  
  # Use cache if all conditions are met:
  # 1. Cache exists (data is not NULL)
  # 2. Same file path (not switched to different file)
  # 3. File hasn't been modified (mtime unchanged)
  if (!is.null(EPIC_EXPORT_CACHE$data) && 
      identical(EPIC_EXPORT_CACHE$file_path, epic_export_path) &&
      !is.null(EPIC_EXPORT_CACHE$file_mtime) &&
      identical(EPIC_EXPORT_CACHE$file_mtime, current_mtime)) {
    
    autofill_log("Using cached EpicExport.csv data", "INFO")
    return(EPIC_EXPORT_CACHE$data)
  }
  
  # ===== LOAD FRESH DATA =====
  autofill_log(sprintf("Loading EpicExport.csv from: %s", epic_export_path), "INFO")
  
  epic_data <- tryCatch({
    # Read CSV with semicolon delimiter and UTF-8 encoding
    data <- fread(epic_export_path, sep = ";", encoding = "UTF-8")
    
    # Validate structure
    if (!is.data.table(data) || nrow(data) == 0) {
      stop("Invalid or empty CSV")
    }
    
    # Check required columns
    required_cols <- c("Element", "waarde")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
    }
    
    autofill_log(sprintf("Successfully loaded %d rows from EpicExport.csv", nrow(data)), "INFO")
    data
  }, error = function(e) {
    autofill_log(sprintf("ERROR loading EpicExport.csv: %s", e$message), "ERROR")
    NULL
  })
  
  # ===== UPDATE CACHE =====
  if (!is.null(epic_data)) {
    EPIC_EXPORT_CACHE$data <<- epic_data
    EPIC_EXPORT_CACHE$file_path <<- epic_export_path
    EPIC_EXPORT_CACHE$file_mtime <<- current_mtime
    EPIC_EXPORT_CACHE$loaded_at <<- Sys.time()
    
    autofill_log(sprintf("EpicExport.csv cached (%d rows, %d elements)", 
                        nrow(epic_data), 
                        length(unique(epic_data$Element))), "INFO")
  }
  
  return(epic_data)
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Check internet connectivity
#' Cached result for performance (rechecks every 5 minutes)
#' @return Logical indicating if internet is available
check_internet_connection <- function() {
  # Check if we have a recent cached result
  if (!is.null(INTERNET_STATUS$available) && !is.null(INTERNET_STATUS$last_check)) {
    time_since_check <- as.numeric(difftime(Sys.time(), INTERNET_STATUS$last_check, units = "secs"))
    
    if (time_since_check < INTERNET_STATUS$check_interval) {
      # Use cached result
      return(INTERNET_STATUS$available)
    }
  }
  
  # Perform actual check
  result <- tryCatch({
    # Try to reach Google DNS (fast and reliable)
    response <- httr::GET("https://www.google.com", httr::timeout(3))
    httr::status_code(response) == 200
  }, error = function(e) {
    FALSE
  })
  
  # Update cache
  INTERNET_STATUS$available <<- result
  INTERNET_STATUS$last_check <<- Sys.time()
  
  if (!result) {
    autofill_log("No internet connection detected - API translation disabled", "WARNING")
  }
  
  return(result)
}

# ============================================================================
# LOGGING FUNCTIONS
# ============================================================================

#' Log autofill messages
#' Uses central Logger.r infrastructure if available, otherwise creates own log
#' @param message Message to log
#' @param level Log level (INFO, WARNING, ERROR)
#' @param console If FALSE, suppress console output (default: TRUE for ERROR, FALSE for WARNING)
autofill_log <- function(message, level = "INFO", console = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s\n", timestamp, level, message)
  
  # Determine console output behavior
  if (is.null(console)) {
    # Default: show ERROR, hide WARNING and INFO (reduce console clutter)
    console <- (level == "ERROR")
  }
  
  # ===== TRY TO USE CENTRAL LOGGER =====
  # Check if Logger.r is loaded and has a log directory set
  log_dir <- NULL
  
  # Priority 1: Use environment variable from Logger.r
  log_dir_env <- Sys.getenv("EPIC2CASTOR_LOGDIR", unset = "")
  if (nzchar(log_dir_env) && dir.exists(log_dir_env)) {
    log_dir <- log_dir_env
  }
  
  # Priority 2: Use option from Logger.r
  if (is.null(log_dir)) {
    log_dir_opt <- getOption("epic2castor.logdir", NULL)
    if (!is.null(log_dir_opt) && dir.exists(log_dir_opt)) {
      log_dir <- log_dir_opt
    }
  }
  
  # Priority 3: Create own directory (fallback for standalone use)
  if (is.null(log_dir)) {
    log_dir <- file.path("logs", format(Sys.time(), "%Y-%m-%d_%H-%M"))
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # ===== WRITE TO LOG FILE =====
  log_written <- tryCatch({
    log_file <- file.path(log_dir, "autofill_log.txt")
    cat(log_entry, file = log_file, append = TRUE)
    TRUE
  }, error = function(e) {
    # Fallback: print to console only if enabled
    if (console) {
      cat(sprintf("[AUTOFILL-LOG-ERROR] Could not write to log file: %s\n", e$message))
    }
    FALSE
  })
  
  # Print to console only if enabled (reduce clutter)
  if (console) {
    cat(log_entry)
  }
}

#' Check if a value should be preserved as-is
#' @param value Value to check
#' @return TRUE if should be preserved
should_preserve <- function(value) {
  if (is.na(value) || value == "") return(FALSE)
  
  for (pattern in AUTOFILL_CONFIG$preserve_patterns) {
    if (grepl(pattern, value, perl = TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Normalize numeric values for comparison
#' @param value Value to normalize
#' @return Normalized numeric value or original if not numeric
normalize_numeric <- function(value) {
  if (is.na(value) || value == "") return(value)
  
  # Remove common non-numeric suffixes/prefixes for comparison
  cleaned <- gsub("%|\\s+|graden|degrees|°", "", value)
  
  # Try to extract number
  num <- suppressWarnings(as.numeric(cleaned))
  if (!is.na(num)) return(num)
  
  return(value)
}

# ============================================================================
# PHASE 2: MATCHING STRATEGIES
# ============================================================================

#' Add a new translation to the medical dictionary
#' @param english English term
#' @param dutch Dutch translation
#' @param category Category to add to: "medical_terms" or "compound_terms" (default: "medical_terms")
#' @return TRUE if successfully added, FALSE otherwise
add_to_medical_dictionary <- function(english, dutch, category = "medical_terms") {
  dict_path <- epc_path("medical_terms_dict")
  
  tryCatch({
    # Load current dictionary
    med_dict <- jsonlite::fromJSON(dict_path, simplifyVector = FALSE)
    
    # Check if term already exists
    all_terms <- c(
      names(med_dict$common_terms),
      names(med_dict$medical_terms),
      names(med_dict$compound_terms)
    )
    
    if (english %in% all_terms) {
      autofill_log(sprintf("Term '%s' already exists in dictionary", english), "INFO")
      return(FALSE)
    }
    
    # Add to appropriate category
    if (!category %in% c("medical_terms", "compound_terms")) {
      category <- "medical_terms"
    }
    
    # Add the new term
    med_dict[[category]][[english]] <- dutch
    
    # Write back to file with pretty formatting
    jsonlite::write_json(
      med_dict,
      path = dict_path,
      pretty = TRUE,
      auto_unbox = TRUE
    )
    
    autofill_log(sprintf("Added to dictionary: '%s' → '%s' (category: %s)", 
                         english, dutch, category), "INFO")
    return(TRUE)
    
  }, error = function(e) {
    autofill_log(sprintf("Failed to add to dictionary: %s", conditionMessage(e)), "WARNING")
    return(FALSE)
  })
}

#' Add multiple approved translations to medical dictionary
#' Called when user confirms autofill suggestions
#' @param results data.table with autofill results (must have strategy == "API Translation")
#' @param selected_indices Vector of 0-based indices of approved suggestions
#' @return Number of terms added
add_approved_translations_to_dictionary <- function(results, selected_indices = NULL) {
  if (is.null(results) || nrow(results) == 0) {
    return(0)
  }
  
  # Filter to only API Translation results
  api_translations <- results[strategy == "API Translation" | grepl("translation", strategy, ignore.case = TRUE)]
  
  if (nrow(api_translations) == 0) {
    autofill_log("No API translations to add to dictionary", "INFO")
    return(0)
  }
  
  # If specific indices provided, filter to those
  if (!is.null(selected_indices)) {
    # Convert 0-based indices to 1-based
    selected_rows <- selected_indices + 1
    # Filter api_translations to only selected ones
    api_translations <- api_translations[selected_rows[selected_rows <= nrow(api_translations)]]
  }
  
  if (nrow(api_translations) == 0) {
    return(0)
  }
  
  added_count <- 0
  
  for (i in seq_len(nrow(api_translations))) {
    row <- api_translations[i]
    
    # Determine category based on castor_value content
    category <- if (grepl(",", row$castor_value)) {
      "compound_terms"  # Has comma, likely compound term
    } else {
      "medical_terms"   # Single term
    }
    
    # Add to dictionary
    success <- add_to_medical_dictionary(
      english = row$castor_value,
      dutch = row$epic_value_new,
      category = category
    )
    
    if (success) {
      added_count <- added_count + 1
    }
  }
  
  if (added_count > 0) {
    autofill_log(sprintf("Added %d approved translation(s) to medical dictionary", added_count), "INFO")
  }
  
  return(added_count)
}

#' Strategy 1: Use existing mappings from reference dictionary
#' @param castor_value The Castor value (English) to match
#' @param ref_dict Reference dictionary from existing mappings
#' @return List with epic_value and confidence, or NULL if no match
strategy_existing_map <- function(castor_value, ref_dict) {
  if (castor_value %in% names(ref_dict)) {
    return(list(
      epic_value = ref_dict[[castor_value]],
      confidence = AUTOFILL_CONFIG$strategy_weights$existing_map,
      strategy = "Existing Map",
      source = "Reference dictionary"
    ))
  }
  return(NULL)
}

#' Strategy 2: Handle numeric values (copy as-is)
#' @param castor_value The Castor value to check
#' @return List with epic_value and confidence, or NULL if not numeric
strategy_numeric <- function(castor_value) {
  if (should_preserve(castor_value)) {
    return(list(
      epic_value = castor_value,
      confidence = AUTOFILL_CONFIG$strategy_weights$numeric,
      strategy = "Numeric/Preserve",
      source = "Direct copy"
    ))
  }
  return(NULL)
}

#' Strategy 3: Medical dictionary lookup (common + medical + compound terms)
#' @param castor_value The Castor value (English) to lookup
#' @param med_dict Medical dictionary
#' @return List with epic_value and confidence, or NULL if no match
strategy_medical_dict <- function(castor_value, med_dict) {
  # Check all dictionary sections
  all_terms <- c(
    med_dict$common_terms,
    med_dict$medical_terms,
    med_dict$compound_terms
  )
  
  if (castor_value %in% names(all_terms)) {
    dict_type <- if (castor_value %in% names(med_dict$compound_terms)) {
      "compound_dict"
    } else {
      "medical_dict"
    }
    
    return(list(
      epic_value = all_terms[[castor_value]],
      confidence = AUTOFILL_CONFIG$strategy_weights[[dict_type]],
      strategy = "Medical Dictionary",
      source = sprintf("Dict: %s", dict_type)
    ))
  }
  return(NULL)
}

#' Strategy 4: Cross-element matching (find same Castor value in other elements)
#' @param castor_value The Castor value to find
#' @param all_data Complete dataset to search
#' @param current_element Current element ID to exclude
#' @return List with epic_value and confidence, or NULL if no match
strategy_cross_element <- function(castor_value, all_data, current_element) {
  # Determine castor column
  castor_col <- if ("castor_waarde" %in% names(all_data)) "castor_waarde" else "kolom_toevoeging"
  
  # Find other elements with same Castor value and filled EPIC value
  matches <- all_data[
    get(castor_col) == castor_value & 
    Element != current_element & 
    !is.na(waarde) & waarde != ""
  ]
  
  if (nrow(matches) > 0) {
    # Use most common EPIC value
    epic_counts <- table(matches$waarde)
    most_common <- names(epic_counts)[which.max(epic_counts)]
    
    return(list(
      epic_value = most_common,
      confidence = AUTOFILL_CONFIG$strategy_weights$cross_element,
      strategy = "Cross-element",
      source = sprintf("Found in %d other element(s)", nrow(matches))
    ))
  }
  return(NULL)
}

#' Translate text using DeepL API
#' @param text Text to translate (English)
#' @param target_lang Target language code (default: "NL")
#' @return Translated text or NULL if failed
translate_deepl <- function(text, target_lang = "NL") {
  # Check cache first
  cache_key <- paste0("deepl_", text, "_", target_lang)
  if (exists(cache_key, envir = TRANSLATION_CACHE)) {
    autofill_log(sprintf("Using cached DeepL translation: '%s'", text), "INFO")
    return(get(cache_key, envir = TRANSLATION_CACHE))
  }
  
  # Get API key - first try environment variable, then APIConfig.json
  api_key <- Sys.getenv("DEEPL_API_KEY")
  
  if (api_key == "" || is.null(api_key)) {
    # Try to load from APIConfig.json
    tryCatch({
      api_config_path <- epc_path("config_api")
      if (file.exists(api_config_path)) {
        config <- jsonlite::fromJSON(api_config_path)
        if (!is.null(config$deepl_api_key) && config$deepl_api_key != "") {
          api_key <- config$deepl_api_key
        }
      }
    }, error = function(e) {
      # Silently fail if APIConfig can't be read
    })
  }
  
  if (api_key == "" || is.null(api_key)) {
    # Only log warning once per session
    if (!exists("DEEPL_WARNING_SHOWN") || !DEEPL_WARNING_SHOWN) {
      autofill_log("DeepL API key not found - using MyMemory as fallback", "INFO")
      DEEPL_WARNING_SHOWN <<- TRUE
    }
    return(NULL)
  }
  
  # Make API call
  tryCatch({
    response <- httr::POST(
      "https://api-free.deepl.com/v2/translate",
      body = list(
        text = text,
        target_lang = target_lang,
        source_lang = "EN",
        auth_key = api_key
      ),
      encode = "form"
    )
    
    status <- httr::status_code(response)
    
    if (status == 200) {
      result <- httr::content(response, as = "parsed", encoding = "UTF-8")
      translated <- result$translations[[1]]$text
      
      # EDGE CASE: Validate translation is not empty or same as input
      if (is.null(translated) || translated == "" || nchar(trimws(translated)) == 0) {
        autofill_log(sprintf("DeepL returned empty translation for: '%s'", text), "WARNING")
        return(NULL)
      }
      
      # EDGE CASE: Check if translation is identical to input (failed to translate)
      if (tolower(trimws(translated)) == tolower(trimws(text))) {
        autofill_log(sprintf("DeepL returned untranslated text for: '%s'", text), "WARNING")
        # Don't cache this result
        return(translated)  # Return anyway but with warning
      }
      
      # Cache result
      if (AUTOFILL_CONFIG$translation$cache_translations) {
        assign(cache_key, translated, envir = TRANSLATION_CACHE)
      }
      
      autofill_log(sprintf("DeepL: '%s' → '%s'", text, translated), "INFO")
      return(translated)
    } else if (status == 403 || status == 456) {
      # 403: Authorization failed (invalid key)
      # 456: Quota exceeded
      autofill_log(sprintf("DeepL quota exceeded or authorization failed (status %d), falling back to MyMemory", status), "WARNING")
      return(NULL)
    } else {
      autofill_log(sprintf("DeepL API error: %d", status), "WARNING")
      return(NULL)
    }
  }, error = function(e) {
    autofill_log(sprintf("DeepL error: %s", conditionMessage(e)), "WARNING")
    return(NULL)
  })
}

#' Translate text using MyMemory API (Free, no key required)
#' @param text Text to translate (English)
#' @param target_lang Target language code (default: "nl")
#' @param retry_count Number of retries on timeout (default: 1)
#' @return Translated text or NULL if failed
translate_mymemory <- function(text, target_lang = "nl", retry_count = 1) {
  # Check cache first
  cache_key <- paste0("mymemory_", text, "_", target_lang)
  if (exists(cache_key, envir = TRANSLATION_CACHE)) {
    autofill_log(sprintf("Using cached MyMemory translation: '%s'", text), "INFO")
    return(get(cache_key, envir = TRANSLATION_CACHE))
  }
  
  # Try API call with retry
  for (attempt in 1:(retry_count + 1)) {
    result <- tryCatch({
      # URL encode the text
      encoded_text <- URLencode(text, reserved = TRUE)
      url <- sprintf("https://api.mymemory.translated.net/get?q=%s&langpair=en|%s", 
                     encoded_text, target_lang)
      
      # Add small delay between attempts to avoid rate limiting
      if (attempt > 1) {
        Sys.sleep(0.5)
      }
      
      response <- httr::GET(url, httr::timeout(5))  # Reduced timeout to 5 seconds
      
      if (httr::status_code(response) == 200) {
        parsed <- httr::content(response, as = "parsed", encoding = "UTF-8")
        
        if (!is.null(parsed$responseData$translatedText)) {
          translated <- parsed$responseData$translatedText
          
          # EDGE CASE: Validate translation is not empty or same as input
          if (is.null(translated) || nchar(trimws(translated)) == 0) {
            autofill_log(sprintf("MyMemory returned empty translation for: '%s'", text), "WARNING")
            return(NULL)
          }
          
          # EDGE CASE: Check if translation is identical to input (failed to translate)
          if (tolower(trimws(translated)) == tolower(trimws(text))) {
            autofill_log(sprintf("MyMemory returned untranslated text for: '%s'", text), "WARNING")
            # Return anyway but with warning - might be correct (e.g., medical terms)
            return(translated)
          }
          
          # Cache result
          if (AUTOFILL_CONFIG$translation$cache_translations) {
            assign(cache_key, translated, envir = TRANSLATION_CACHE)
          }
          
          autofill_log(sprintf("MyMemory: '%s' → '%s'", text, translated), "INFO")
          return(translated)
        } else {
          autofill_log("MyMemory: No translation in response", "WARNING")
          return(NULL)
        }
      } else {
        if (attempt < (retry_count + 1)) {
          autofill_log(sprintf("MyMemory API error %d, retrying...", httr::status_code(response)), "INFO")
          next
        } else {
          autofill_log(sprintf("MyMemory API error: %d", httr::status_code(response)), "WARNING")
          return(NULL)
        }
      }
    }, error = function(e) {
      if (attempt < (retry_count + 1)) {
        autofill_log(sprintf("MyMemory attempt %d failed, retrying...", attempt), "INFO")
        NULL
      } else {
        autofill_log(sprintf("MyMemory error: %s", conditionMessage(e)), "WARNING")
        NULL
      }
    })
    
    if (!is.null(result)) {
      return(result)
    }
  }
  
  return(NULL)
}

#' Translate text using LibreTranslate API
#' @param text Text to translate (English)
#' @param target_lang Target language code (default: "nl")
#' @return Translated text or NULL if failed
translate_libretranslate <- function(text, target_lang = "nl") {
  # Check cache first
  cache_key <- paste0("libre_", text, "_", target_lang)
  if (exists(cache_key, envir = TRANSLATION_CACHE)) {
    autofill_log(sprintf("Using cached LibreTranslate translation: '%s'", text), "INFO")
    return(get(cache_key, envir = TRANSLATION_CACHE))
  }
  
  # Check for API key
  api_key <- Sys.getenv("LIBRETRANSLATE_API_KEY")
  
  # Make API call
  tryCatch({
    body_data <- list(
      q = text,
      source = "en",
      target = target_lang,
      format = "text"
    )
    
    # Add API key if available
    if (api_key != "") {
      body_data$api_key <- api_key
    }
    
    response <- httr::POST(
      "https://libretranslate.com/translate",
      body = jsonlite::toJSON(body_data, auto_unbox = TRUE),
      httr::content_type_json(),
      httr::timeout(10)
    )
    
    if (httr::status_code(response) == 200) {
      result <- httr::content(response, as = "parsed", encoding = "UTF-8")
      translated <- result$translatedText
      
      # Cache result
      if (AUTOFILL_CONFIG$translation$cache_translations) {
        assign(cache_key, translated, envir = TRANSLATION_CACHE)
      }
      
      autofill_log(sprintf("LibreTranslate: '%s' → '%s'", text, translated), "INFO")
      return(translated)
    } else {
      if (api_key == "") {
        autofill_log("LibreTranslate requires API key (set LIBRETRANSLATE_API_KEY)", "WARNING")
      } else {
        autofill_log(sprintf("LibreTranslate API error: %d", httr::status_code(response)), "WARNING")
      }
      return(NULL)
    }
  }, error = function(e) {
    autofill_log(sprintf("LibreTranslate error: %s", conditionMessage(e)), "WARNING")
    return(NULL)
  })
}

#' Strategy 5: API Translation with fallback chain
#' @param castor_value The Castor value (English) to translate
#' @return List with epic_value and confidence, or NULL if translation failed
#' 
#' FASE 10.2 TODO: Batch API calls for performance
#' Currently: One API call per value (simple but slower)
#' Future: Group multiple values, single API call with batch endpoint
#' Considerations: DeepL batch pricing, rate limits, error handling per item
strategy_api_translation <- function(castor_value) {
  # EDGE CASE: Check internet connectivity first
  if (!check_internet_connection()) {
    autofill_log("Skipping API translation - no internet connection", "INFO")
    return(NULL)
  }
  
  fallback_chain <- AUTOFILL_CONFIG$translation$fallback_chain
  
  for (provider in fallback_chain) {
    translated <- NULL
    confidence <- 75  # Default confidence
    
    if (provider == "deepl") {
      translated <- translate_deepl(castor_value)
      confidence <- AUTOFILL_CONFIG$strategy_weights$api_translation_deepl
      source_name <- "DeepL"
    } else if (provider == "mymemory") {
      translated <- translate_mymemory(castor_value)
      confidence <- 80  # MyMemory gets 80% confidence
      source_name <- "MyMemory"
    } else if (provider == "libretranslate") {
      translated <- translate_libretranslate(castor_value)
      confidence <- AUTOFILL_CONFIG$strategy_weights$api_translation_libre
      source_name <- "LibreTranslate"
    }
    
    if (!is.null(translated)) {
      # Return translation result
      # Note: Will be added to medical dictionary only when user approves it
      return(list(
        epic_value = translated,
        confidence = confidence,
        strategy = "API Translation",
        source = source_name,
        castor_value = castor_value  # Store for later dictionary addition
      ))
    }
  }
  
  autofill_log(sprintf("All translation APIs failed for: '%s'", castor_value), "WARNING")
  return(NULL)
}

#' Strategy 6: Fuzzy matching with existing EPIC values
#' @param translated_value Translated value (Dutch) from API
#' @param all_epic_values All existing EPIC values to match against
#' @return List with epic_value and confidence, or NULL if no good match
strategy_fuzzy_match <- function(translated_value, all_epic_values) {
  if (is.null(translated_value) || length(all_epic_values) == 0) {
    return(NULL)
  }
  
  # Remove empty values
  all_epic_values <- all_epic_values[!is.na(all_epic_values) & all_epic_values != ""]
  
  if (length(all_epic_values) == 0) {
    return(NULL)
  }
  
  # Calculate string similarity
  similarities <- stringdist::stringsim(
    tolower(translated_value),
    tolower(all_epic_values),
    method = "jw"  # Jaro-Winkler distance
  )
  
  # Find best match
  best_idx <- which.max(similarities)
  best_similarity <- similarities[best_idx]
  best_match <- all_epic_values[best_idx]
  
  # Only return if similarity is above threshold
  min_sim <- AUTOFILL_CONFIG$translation$min_similarity
  if (best_similarity >= min_sim) {
    # Adjust confidence based on similarity
    base_confidence <- AUTOFILL_CONFIG$strategy_weights$fuzzy_match
    adjusted_confidence <- base_confidence + (best_similarity - min_sim) * 25
    
    return(list(
      epic_value = best_match,
      confidence = min(adjusted_confidence, 95),  # Cap at 95%
      strategy = "Fuzzy Match",
      source = sprintf("%.0f%% similar to '%s'", best_similarity * 100, best_match),
      similarity = best_similarity
    ))
  }
  
  return(NULL)
}

#' Strategy 7: Process of elimination (last resort)
#' @param element_data All rows for this element
#' @param castor_col Name of castor column
#' @return List with epic_value and confidence, or NULL if not applicable
strategy_elimination <- function(element_data, castor_col) {
  # Count filled vs empty EPIC values
  filled <- element_data[!is.na(waarde) & waarde != ""]
  empty <- element_data[is.na(waarde) | waarde == ""]
  
  # Only works if exactly 1 empty and we have other filled values
  if (nrow(empty) == 1 && nrow(filled) > 0) {
    # Get all unique EPIC values used in filled rows
    used_epic_values <- unique(filled$waarde)
    
    # Get all unique Castor values for this element
    all_castor_values <- unique(element_data[[castor_col]])
    
    # If counts match (N castor values → N-1 used epic values)
    # We could suggest a pattern match, but this is risky
    # Only do this if there's a clear 1-to-1 remaining
    
    # For now, return NULL - elimination is too risky without more context
    # We can enable this later with user confirmation
    autofill_log(sprintf("Elimination possible for element %s but skipping (too risky)", 
                         element_data$Element[1]), "INFO")
  }
  
  return(NULL)
}

#' Apply all strategies to find best EPIC value
#' @param castor_value The Castor value (English) to match
#' @param element_id Element ID
#' @param all_data Complete dataset
#' @param ref_dict Reference dictionary
#' @param med_dict Medical dictionary
#' @param use_ml Enable ML predictions
#' @param ml_model Pre-loaded ML model (NULL if not available)
#' @return List with best match and alternatives
apply_all_strategies <- function(castor_value, element_id, all_data, ref_dict, med_dict, use_elimination = TRUE, use_ml = TRUE, ml_model = NULL) {
  autofill_log(sprintf("Applying strategies for: '%s' (Element: %s) [elimination=%s, ML=%s]", 
                      castor_value, element_id, use_elimination, use_ml), "INFO")
  
  # Get available EPIC values for this element (from pv_elements)
  available_values <- get_available_epic_values(element_id, all_data)
  
  # STRATEGY 1: Elimination - if only one option remains unused
  # ONLY run if use_elimination = TRUE (Pass 2 of two-pass system)
  # FASE 11.4: Performance optimalisatie - cache element data
  if (use_elimination && !is.null(available_values) && length(available_values) > 0) {
    # FASE 11.5: Vroege exit checks - voorkom onnodige berekeningen
    # Als er maar 1 available value is en meer dan 1 empty value, skip elimination
    if (length(available_values) == 1) {
      element_data <- all_data[Element == element_id]
      empty_mask <- is.na(element_data$waarde) | element_data$waarde == ""
      empty_count <- sum(empty_mask)
      
      if (empty_count > 1) {
        autofill_log(sprintf("  ⚠ Only one option available but %d empty values - cannot use elimination", empty_count), "WARNING")
        # Skip naar volgende strategie
      } else if (empty_count == 1) {
        # CASE A: Snelle path voor single option/single empty
        autofill_log(sprintf("  ✓ Only one option available and one empty value: '%s'", available_values[1]), "INFO")
        return(list(
          best = list(
            epic_value = available_values[1],
            confidence = 100,
            strategy = "elimination (only option)",
            source = "Single option for element"
          ),
          alternatives = list()
        ))
      }
    } else {
      # Multiple options - volledige elimination logica
      # Get all data for this element (CACHE voor hergebruik)
      element_data <- all_data[Element == element_id]
      
      # FASE 11.6: Geoptimaliseerde berekeningen met vroege exits
      # Count unique Castor values (total possible options from Castor dataset)
      castor_col <- names(element_data)[3]  # Third column is Castor value
      unique_castor_values <- unique(element_data[[castor_col]][!is.na(element_data[[castor_col]]) & element_data[[castor_col]] != ""])
      total_castor_options <- length(unique_castor_values)
      
      # Early exit: als totaal aantal Castor != aantal EPIC opties, skip elimination
      if (total_castor_options != length(available_values)) {
        autofill_log(sprintf("  ⚠ ELIMINATION UNSAFE: Data mismatch - %d total Castor values vs %d EPIC options (old data in export)", 
                            total_castor_options, length(available_values)), "WARNING")
        # Skip naar volgende strategie
      } else {
        # Get all filled EPIC values for this element
        filled_values <- element_data$waarde[!is.na(element_data$waarde) & element_data$waarde != ""]
        
        # BELANGRIJKE AANPASSING: Get Castor values die horen bij LEGE EPIC values
        # Dit zijn de "actieve" Castor values die nog gematcht moeten worden
        empty_mask <- is.na(element_data$waarde) | element_data$waarde == ""
        active_castor_values <- unique(element_data[[castor_col]][empty_mask & !is.na(element_data[[castor_col]]) & element_data[[castor_col]] != ""])
        
        # Find which available EPIC values are NOT yet used
        unused_values <- setdiff(available_values, filled_values)
        
        # Count how many empty values still exist for this element
        empty_count <- sum(empty_mask)
        
        # CASE B: Multiple options available, but only one unused and exactly one empty value
        # CRITICAL FIX: Check dat aantal Castor values GELIJK is aan aantal EPIC opties
        # Anders kunnen we niet veilig elimineren (oude data mismatch)
        if (length(unused_values) == 1 && empty_count == 1) {
          unused_epic <- unused_values[1]
        
        # BELANGRIJKE CHECK 1: Is het huidige Castor value dat we proberen te matchen
        # daadwerkelijk een van de ACTIEVE Castor values (die bij lege EPIC values horen)?
        current_castor_is_active <- castor_value %in% active_castor_values
        
        # BELANGRIJKE CHECK 2: Aantal TOTALE Castor values moet EXACT GELIJK zijn aan aantal available EPIC values
        # Als EpicExport.csv oude data heeft met 7 EPIC opties maar er zijn nu 6 Castor checkboxes,
        # dan is de 7e EPIC optie ("Nee") verouderd en mag niet worden gematcht!
        castor_epic_count_match <- total_castor_options == length(available_values)
        
        # BELANGRIJKE CHECK 3: Valideer data integriteit
        # Tel unieke Castor values die een EPIC match hebben
        filled_castor_values <- unique(element_data[[castor_col]][!is.na(element_data$waarde) & element_data$waarde != "" & !is.na(element_data[[castor_col]]) & element_data[[castor_col]] != ""])
        filled_castor_count <- length(filled_castor_values)
        filled_epic_count <- length(unique(filled_values))
        
        # KRITIEKE CHECK: aantal filled + aantal active moet gelijk zijn aan totaal
        # Dit garandeert dat er geen "orphaned" EPIC values zijn (zoals oude "Nee" optie)
        # En dat elke Castor value precies 1 keer voorkomt (geen duplicaten)
        data_integrity_check <- (filled_castor_count + length(active_castor_values)) == total_castor_options
        
        # NIEUWE CHECK 4: Valideer of de unused EPIC value logisch matched met het current Castor value
        # Dit voorkomt false positives zoals "Postural drainage" → "Nee"
        # Check: Zou de unused EPIC value gevonden worden via translation/existing map?
        unused_epic_validated <- FALSE
        
        # Check 4a: Is er een existing mapping voor current Castor → unused EPIC?
        if (castor_value %in% names(ref_dict) && ref_dict[[castor_value]] == unused_epic) {
          unused_epic_validated <- TRUE
        }
        
        # Check 4b: Is de translation van current Castor gelijk aan unused EPIC?
        if (!unused_epic_validated) {
          # Probeer translation (gebruik existing translation function maar zonder API call)
          translated <- tolower(trimws(castor_value))
          if (tolower(trimws(unused_epic)) == translated) {
            unused_epic_validated <- TRUE
          }
          
          # Check ook fuzzy match van Castor value met unused EPIC
          if (!unused_epic_validated && !is.null(available_values)) {
            library(stringdist)
            similarity <- 1 - stringdist(tolower(castor_value), tolower(unused_epic), method = "jw")
            if (similarity >= 0.80) {  # 80% similarity threshold
              unused_epic_validated <- TRUE
            }
          }
        }
        
        # Als deze check faalt, betekent het:
        # - Sommige Castor values hebben meerdere EPIC matches, OF
        # - Sommige EPIC values hebben geen corresponderende Castor value (oude data)
        
        if (!current_castor_is_active) {
          autofill_log(sprintf("  ⚠ ELIMINATION SKIPPED: Current Castor value '%s' not in active Castor values (may be old data)", 
                              castor_value), "WARNING")
        } else if (!castor_epic_count_match) {
          # Data mismatch: aantal Castor checkboxes != aantal EPIC opties in export
          # Dit betekent dat EpicExport.csv verouderde data bevat
          autofill_log(sprintf("  ⚠ ELIMINATION UNSAFE: Data mismatch - %d total Castor values vs %d EPIC options (old data in export)", 
                              total_castor_options, length(available_values)), "WARNING")
        } else if (!data_integrity_check) {
          # Data integriteit gefaald: aantal filled + active Castor != totaal
          # Dit betekent orphaned EPIC values of duplicate mappings
          autofill_log(sprintf("  ⚠ ELIMINATION UNSAFE: Data integrity fail - %d filled + %d active != %d total Castor (orphaned EPIC values or duplicates)", 
                              filled_castor_count, length(active_castor_values), total_castor_options), "WARNING")
        } else if (!unused_epic_validated) {
          # Logische validatie gefaald: unused EPIC value matched niet met current Castor value
          # Voorbeeld: "Postural drainage" (Castor) vs "Nee" (unused EPIC) - geen logische match!
          autofill_log(sprintf("  ⚠ ELIMINATION UNSAFE: Unused EPIC '%s' does not logically match Castor '%s' (likely old data)", 
                              unused_epic, castor_value), "WARNING")
        } else if (length(active_castor_values) != length(unused_values)) {
          # Als er 2+ actieve Castor values zijn maar slechts 1 unused EPIC value,
          # dan kunnen we niet veilig elimineren (ambiguity)
          autofill_log(sprintf("  ⚠ ELIMINATION UNSAFE: %d active Castor values but only %d unused EPIC value - ambiguous mapping", 
                              length(active_castor_values), length(unused_values)), "WARNING")
        } else {
          # All checks passed - safe to eliminate
          autofill_log(sprintf("  ✓ Elimination: only unused option is '%s' (all others already used, %d/%d)", 
                              unused_epic, length(available_values) - 1, length(available_values)), "INFO")
          return(list(
            best = list(
              epic_value = unused_epic,
              confidence = 100,
              strategy = "elimination (last remaining)",
              source = sprintf("%d/%d options already used", length(available_values) - 1, length(available_values))
            ),
            alternatives = list()
          ))
        }
      }  # End of if (length(unused_values) == 1 && empty_count == 1)
    }  # End of else block (after if total_castor_options != length check)
  }  # End of else block for multiple options (after if length == 1)
}  # End of if (use_elimination &&!is.null(available_values))
  
  # STRATEGY 2: Check existing mappings (same Castor → EPIC mapping exists)
  result <- strategy_existing_map(castor_value, ref_dict)
  if (!is.null(result) && is_valid_epic_value(result$epic_value, available_values)) {
    autofill_log(sprintf("  ✓ Existing mapping found: '%s'", result$epic_value), "INFO")
    return(list(
      best = result,
      alternatives = list(result)
    ))
  } else if (!is.null(result)) {
    autofill_log(sprintf("  ⚠ Existing map '%s' not in available options", result$epic_value), "WARNING")
  }
  
  # STRATEGY 3: Check medical dictionary
  # Medical dictionary matches are ALWAYS accepted (high confidence, curated translations)
  # Don't check against available_values - the dictionary is the source of truth
  result <- strategy_medical_dict(castor_value, med_dict)
  if (!is.null(result)) {
    autofill_log(sprintf("  ✓ Medical dictionary match: '%s' (always accepted)", result$epic_value), "INFO")
    return(list(
      best = result,
      alternatives = list(result)
    ))
  }
  
  # STRATEGY 4: ML Prediction (if enabled and model available)
  if (use_ml && !is.null(ml_model) && exists("predict_epic_value_ml")) {
    tryCatch({
      # Use pre-loaded model to avoid repeated disk I/O
      ml_result <- predict_epic_value_ml(castor_value, element = element_id, ref_dict = ref_dict, model_info = ml_model)
      
      if (!is.null(ml_result)) {
        # Validate ML prediction against available values
        if (is_valid_epic_value(ml_result$epic_value, available_values)) {
          autofill_log(sprintf("  ✓ ML prediction: '%s' (%.1f%% confidence)", 
                              ml_result$epic_value, ml_result$confidence), "INFO")
          
          # Convert ML result to standard format
          result <- list(
            epic_value = ml_result$epic_value,
            confidence = ml_result$confidence,
            strategy = ml_result$strategy,
            source = ml_result$source
          )
          
          return(list(
            best = result,
            alternatives = list(result)
          ))
        } else {
          autofill_log(sprintf("  ⚠ ML prediction '%s' not in available options", 
                              ml_result$epic_value), "WARNING")
        }
      }
    }, error = function(e) {
      autofill_log(sprintf("  ⚠ ML prediction error: %s", conditionMessage(e)), "WARNING")
    })
  }
  
  # STRATEGY 5: Translate and check against available options
  translated_result <- strategy_api_translation(castor_value)
  if (!is.null(translated_result)) {
    # 4a. Check if translation is directly available
    if (is_valid_epic_value(translated_result$epic_value, available_values)) {
      autofill_log(sprintf("  ✓ Translation match: '%s'", translated_result$epic_value), "INFO")
      return(list(
        best = translated_result,
        alternatives = list(translated_result)
      ))
    }
    
    # 4b. Try fuzzy match of translation against available options
    if (!is.null(available_values)) {
      fuzzy_result <- strategy_fuzzy_match(translated_result$epic_value, available_values)
      if (!is.null(fuzzy_result) && fuzzy_result$confidence >= 70) {
        fuzzy_result$strategy <- "translation + fuzzy match"
        autofill_log(sprintf("  ✓ Translation fuzzy match: '%s' (%.0f%% confidence)", 
                             fuzzy_result$epic_value, fuzzy_result$confidence), "INFO")
        return(list(
          best = fuzzy_result,
          alternatives = list(fuzzy_result)
        ))
      }
    }
    
    autofill_log(sprintf("  ⚠ Translation '%s' not found in available options", 
                         translated_result$epic_value), "WARNING")
  }
  
  # FALLBACK STRATEGIES: If above strategies fail, try additional methods
  
  # Fallback 1: Numeric/Preserve patterns
  result <- strategy_numeric(castor_value)
  if (!is.null(result) && is_valid_epic_value(result$epic_value, available_values)) {
    autofill_log(sprintf("  ✓ Numeric pattern match: '%s'", result$epic_value), "INFO")
    return(list(
      best = result,
      alternatives = list(result)
    ))
  }
  
  # Fallback 2: Cross-element matching
  result <- strategy_cross_element(castor_value, all_data, element_id)
  if (!is.null(result) && is_valid_epic_value(result$epic_value, available_values)) {
    autofill_log(sprintf("  ✓ Cross-element match: '%s'", result$epic_value), "INFO")
    return(list(
      best = result,
      alternatives = list(result)
    ))
  }
  
  # Fallback 3: Direct fuzzy match on available values (already in Dutch from EPIC)
  if (!is.null(available_values)) {
    fuzzy_result <- strategy_fuzzy_match(castor_value, available_values)
    if (!is.null(fuzzy_result) && fuzzy_result$confidence >= 70) {
      fuzzy_result$strategy <- "fuzzy match (EPIC values)"
      autofill_log(sprintf("  ✓ Fuzzy match from EPIC: '%s' (%.0f%% confidence)", 
                           fuzzy_result$epic_value, fuzzy_result$confidence), "INFO")
      return(list(
        best = fuzzy_result,
        alternatives = list(fuzzy_result)
      ))
    }
  }
  
  # No match found
  autofill_log(sprintf("  ✗ No match found for: '%s'", castor_value), "WARNING")
  return(NULL)
}

# ============================================================================
# PHASE 1: DATA LOADING & PREPARATION
# ============================================================================

#' Load mapping data from memory or database
#' @param table_name Name of the table (waarde_radiobuttons or waarde_checkboxes)
#' @param current_data Optional: provide current data directly (overrides loading)
#' @return data.table with mapping data
load_mappings_data <- function(table_name, current_data = NULL) {
  # If data is provided directly, use it (real-time UI state)
  if (!is.null(current_data)) {
    autofill_log(sprintf("Using provided data for table: %s (%d rows)", table_name, nrow(current_data)))
    return(as.data.table(current_data))
  }
  
  autofill_log(sprintf("Loading data for table: %s", table_name))
  
  # Check if data exists in global environment (from App.r)
  if (exists("mappingData", envir = .GlobalEnv)) {
    data <- get("mappingData", envir = .GlobalEnv)[[table_name]]
    
    if (is.null(data)) {
      stop(sprintf("Table '%s' not found in mappingData", table_name))
    }
    
    autofill_log(sprintf("Loaded %d rows from memory", nrow(data)))
    return(as.data.table(data))
  }
  
  # Fallback: load from CSV
  csv_path <- if (table_name == "waarde_radiobuttons") {
    epc_path("mapping_radiobuttons_file")
  } else if (table_name == "waarde_checkboxes") {
    epc_path("mapping_checkboxes_file")
  } else {
    file.path(epc_path("mapping_dir"), paste0(table_name, ".csv"))
  }
  
  if (!file.exists(csv_path)) {
    stop(sprintf("CSV file not found: %s", csv_path))
  }
  
  data <- fread(csv_path, encoding = "UTF-8")
  autofill_log(sprintf("Loaded %d rows from CSV", nrow(data)))
  
  return(data)
}

#' Get available EPIC values for a specific element
#' Queries castor_meta.db to get field_options for radiobuttons/checkboxes
#' @param element_id Element ID to get options for
#' @param all_data Full data table with all elements
#' @return Character vector of available EPIC values, or NULL if all values are allowed
get_available_epic_values <- function(element_id, all_data) {
  # For radiobuttons/checkboxes, get all unique Dutch EPIC values from EpicExport.csv
  # These are the actual values that can be selected in the UI
  
  tryCatch({
    # FASE 10.1: Use cached EpicExport.csv data (performance optimization)
    epic_data <- load_epic_export_cached()
    
    # Validate data loaded successfully
    if (is.null(epic_data)) {
      autofill_log("Failed to load EpicExport.csv - skipping value filtering", "ERROR")
      return(NULL)
    }
    
    # Filter for this specific element (fast - in-memory operation)
    element_data <- epic_data[Element == element_id]
    
    if (nrow(element_data) == 0) {
      autofill_log(sprintf("No data found for element %s in EpicExport.csv", element_id), "INFO")
      return(NULL)
    }
    
    # Get all unique non-empty values from the "waarde" column
    available_values <- unique(element_data$waarde)
    available_values <- available_values[!is.na(available_values) & available_values != ""]
    
    if (length(available_values) == 0) {
      autofill_log(sprintf("No valid values found for element %s", element_id), "INFO")
      return(NULL)
    }
    
    # Remove case-insensitive duplicates (keep the one with most occurrences)
    if (length(available_values) > 1) {
      # Convert to lowercase for comparison
      lower_values <- tolower(available_values)
      unique_lower <- unique(lower_values)
      
      # For each unique lowercase value, keep the most frequent casing
      deduplicated <- sapply(unique_lower, function(lv) {
        matches <- available_values[lower_values == lv]
        # Count occurrences of each casing variant
        counts <- table(element_data$waarde[tolower(element_data$waarde) == lv])
        # Return the most frequent variant
        names(counts)[which.max(counts)]
      })
      
      available_values <- as.character(deduplicated)
    }
    
    autofill_log(sprintf("Found %d unique EPIC values for element %s from EpicExport.csv", 
                         length(available_values), element_id), "INFO")
    
    return(available_values)
    
  }, error = function(e) {
    autofill_log(sprintf("Error reading EpicExport.csv for element %s: %s", 
                         element_id, conditionMessage(e)), "WARNING")
    return(NULL)
  })
}

#' Validate if a suggested EPIC value is in the available options
#' @param epic_value Suggested EPIC value
#' @param available_values Available EPIC values (NULL = all allowed)
#' @return TRUE if valid, FALSE otherwise
is_valid_epic_value <- function(epic_value, available_values) {
  # If no filtering, all values are valid
  if (is.null(available_values) || length(available_values) == 0) {
    return(TRUE)
  }
  
  # Check if value is in available list
  return(epic_value %in% available_values)
}

#' Build reference dictionary from existing complete mappings
#' @param data data.table with all mapping data
#' @return Named list: Castor Value (EN) → EPIC Value (NL)
build_reference_dictionary <- function(data) {
  autofill_log("Building reference dictionary from existing mappings")
  
  # Determine which column contains the Castor values (English)
  castor_col <- if ("castor_waarde" %in% names(data)) {
    "castor_waarde"
  } else if ("kolom_toevoeging" %in% names(data)) {
    "kolom_toevoeging"
  } else {
    stop("Could not find Castor value column (castor_waarde or kolom_toevoeging)")
  }
  
  autofill_log(sprintf("Using column '%s' for Castor values", castor_col))
  
  # Filter for complete mappings (both waarde and castor column filled)
  complete <- data[!is.na(waarde) & waarde != "" & 
                   !is.na(get(castor_col)) & get(castor_col) != ""]
  
  # Create dictionary: castor_col → waarde
  dict <- setNames(complete$waarde, complete[[castor_col]])
  
  # Remove duplicates (keep first occurrence)
  dict <- dict[!duplicated(names(dict))]
  
  autofill_log(sprintf("Built dictionary with %d unique mappings", length(dict)))
  
  # Log some examples
  if (length(dict) > 0) {
    examples <- head(dict, 5)
    for (i in seq_along(examples)) {
      autofill_log(sprintf("  Example: '%s' → '%s'", names(examples)[i], examples[i]), "INFO")
    }
  }
  
  return(as.list(dict))
}

#' Load medical terms dictionary from JSON
#' @return List with medical term mappings
load_medical_dictionary <- function() {
  dict_path <- AUTOFILL_CONFIG$medical_dict_path
  
  if (file.exists(dict_path)) {
    autofill_log(sprintf("Loading medical dictionary from: %s", dict_path))
    
    # EDGE CASE: Handle corrupt JSON file
    dict <- tryCatch({
      parsed <- fromJSON(dict_path)
      
      # Validate structure
      if (!is.list(parsed)) {
        stop("Dictionary must be a list structure")
      }
      
      autofill_log(sprintf("Successfully loaded medical dictionary with %d entries", length(unlist(parsed))))
      parsed
    }, error = function(e) {
      autofill_log(sprintf("ERROR loading medical dictionary: %s", e$message), "ERROR")
      autofill_log("Using default fallback dictionary", "WARNING")
      NULL
    })
    
    # If parsing failed, use default
    if (is.null(dict)) {
      return(get_default_dictionary())
    }
    
    return(dict)
  } else {
    autofill_log(sprintf("Medical dictionary not found at: %s", dict_path), "WARNING")
    return(get_default_dictionary())
  }
}

#' Get default fallback dictionary
#' @return List with common medical terms
get_default_dictionary <- function() {
  return(list(
    common_terms = list(
      "Yes" = "Ja",
      "No" = "Nee",
      "Unknown" = "Onbekend",
      "Normal" = "Normaal",
      "Abnormal" = "Afwijkend"
    ),
    medical_terms = list(
      "Inpatient" = "Klinisch",
      "Outpatient" = "Poliklinisch"
    ),
    preserve_as_is = c("COPD", "HIV", "BCG", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")
  ))
}

#' Identify rows with empty EPIC values that need filling
#' @param data data.table with mapping data
#' @param tab_name Optional: filter for specific tab
#' @param review_existing If TRUE, also include already filled values for review
#' @return data.table with rows to process
identify_empty_epic_values <- function(data, tab_name = NULL, review_existing = FALSE) {
  # Determine which column contains the Castor values (English)
  castor_col <- if ("castor_waarde" %in% names(data)) {
    "castor_waarde"
  } else if ("kolom_toevoeging" %in% names(data)) {
    "kolom_toevoeging"
  } else {
    stop("Could not find Castor value column (castor_waarde or kolom_toevoeging)")
  }
  
  # Filter by tab if specified
  if (!is.null(tab_name)) {
    data <- data[tab_name_meta == tab_name]
    autofill_log(sprintf("Filtering for tab: %s", tab_name))
  }
  
  # Find empty EPIC values
  if (!review_existing) {
    empty <- data[is.na(waarde) | waarde == ""]
    autofill_log(sprintf("Found %d empty EPIC values", nrow(empty)))
  } else {
    empty <- data
    autofill_log(sprintf("Review mode: processing all %d rows", nrow(empty)))
  }
  
  # Validate that castor column is filled
  if (any(is.na(empty[[castor_col]]) | empty[[castor_col]] == "")) {
    invalid_count <- sum(is.na(empty[[castor_col]]) | empty[[castor_col]] == "")
    autofill_log(sprintf("WARNING: %d rows have empty Castor values (skipping these)", invalid_count), "WARNING")
    empty <- empty[!is.na(get(castor_col)) & get(castor_col) != ""]
  }
  
  return(empty)
}

#' Validate data integrity before processing
#' @param data data.table to validate
#' @param element Element ID to validate
#' @return TRUE if valid, FALSE otherwise
validate_before_autofill <- function(data, element = NULL) {
  # Determine which column contains the Castor values
  castor_col <- if ("castor_waarde" %in% names(data)) {
    "castor_waarde"
  } else if ("kolom_toevoeging" %in% names(data)) {
    "kolom_toevoeging"
  } else {
    "unknown"
  }
  
  checks <- list(
    has_data = nrow(data) > 0,
    castor_values_exist = castor_col != "unknown" && all(!is.na(data[[castor_col]]) & data[[castor_col]] != ""),
    # REMOVED: no_duplicate_castor check - het is normaal dat meerdere elementen dezelfde Castor waarde hebben
    has_required_columns = all(c("Element", "waarde", "tab_name_meta") %in% names(data)) && castor_col != "unknown"
  )
  
  failed <- names(checks)[!unlist(checks)]
  
  if (length(failed) > 0) {
    msg <- sprintf("Validation failed for %s: %s", 
                   ifelse(is.null(element), "data", element),
                   paste(failed, collapse = ", "))
    autofill_log(msg, "ERROR")
    return(FALSE)
  }
  
  return(TRUE)
}

# ============================================================================
# MAIN PROCESSING FUNCTION (PHASE 2)
# ============================================================================

#' Main function to process autofill for empty EPIC values
#' @param table_name Name of the table (waarde_radiobuttons or waarde_checkboxes)
#' @param tab_name Optional: specific tab to process
#' @param review_existing If TRUE, also review already filled values
#' @param min_confidence Minimum confidence to include in results
#' @param use_ml Enable ML predictions (default: TRUE if ML available)
#' @return data.table with suggestions
process_autofill <- function(table_name, tab_name = NULL, review_existing = FALSE, min_confidence = 70, current_data = NULL, use_ml = TRUE) {
  autofill_log(sprintf("\n========== Starting autofill for %s (ML: %s) ==========", table_name, use_ml))
  
  # 1. Load data (use provided data if available for real-time UI state)
  data <- load_mappings_data(table_name, current_data = current_data)
  
  # 2. Build dictionaries
  ref_dict <- build_reference_dictionary(data)
  med_dict <- load_medical_dictionary()
  
  # 2b. Load ML model once (if enabled)
  ml_model <- NULL
  if (use_ml && exists("ML_AVAILABLE") && ML_AVAILABLE && exists("load_model")) {
    tryCatch({
      autofill_log("Loading ML model for predictions...", "INFO")
      # Load silently (verbose=FALSE) to avoid console clutter
      ml_model <- load_model(verbose = FALSE)
      if (!is.null(ml_model)) {
        autofill_log(sprintf("✓ ML model loaded: %d classes, %.1f%% validation accuracy", 
                            length(ml_model$epic_levels), 
                            ml_model$metrics$validation_accuracy * 100), "INFO")
      }
    }, error = function(e) {
      autofill_log(sprintf("⚠ Could not load ML model: %s", conditionMessage(e)), "WARNING")
    })
  }
  
  # 3. Identify empty values
  empty <- identify_empty_epic_values(data, tab_name, review_existing)
  
  if (nrow(empty) == 0) {
    autofill_log("No empty EPIC values to fill")
    return(data.table())
  }
  
  # 4. Validate
  if (!validate_before_autofill(empty)) {
    stop("Data validation failed")
  }
  
  # Determine castor column
  castor_col <- if ("castor_waarde" %in% names(empty)) "castor_waarde" else "kolom_toevoeging"
  
  # ============================================================================
  # TWO-PASS SYSTEM WITH BATCH PROCESSING:
  # Pass 1: All strategies INCLUDING elimination (with original data)
  # Pass 2: Retry failed rows with elimination (with Pass 1 results applied)
  # FASE 11: Batch processing voor grote datasets (>100 rows)
  # ============================================================================
  
  autofill_log("\n========== PASS 1: All strategies with original data ==========")
  
  # FASE 11.1: Bepaal batch grootte op basis van dataset omvang
  total_rows <- nrow(empty)
  batch_size <- if (total_rows > 500) {
    100  # Grote datasets: 100 per batch
  } else if (total_rows > 100) {
    50   # Middelgrote datasets: 50 per batch
  } else {
    total_rows  # Kleine datasets: alles in 1 batch
  }
  
  num_batches <- ceiling(total_rows / batch_size)
  
  if (num_batches > 1) {
    autofill_log(sprintf("BATCH PROCESSING: %d rows in %d batches van ~%d rows", 
                        total_rows, num_batches, batch_size), "INFO")
  }
  
  # 5. PASS 1: Process each row WITH all strategies (including elimination)
  results_list <- list()
  pass1_successful <- list()  # Track successful matches for Pass 2
  
  for (batch_num in 1:num_batches) {
    # Bepaal batch range
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, total_rows)
    batch_rows <- empty[start_idx:end_idx]
    
    if (num_batches > 1) {
      autofill_log(sprintf("  Processing batch %d/%d (rows %d-%d)", 
                          batch_num, num_batches, start_idx, end_idx), "INFO")
    }
    
    # Process batch
    for (i in seq_len(nrow(batch_rows))) {
      row <- batch_rows[i]
      global_idx <- start_idx + i - 1  # Global row index
      
      castor_value <- row[[castor_col]]
      element_id <- row$Element
    
      # FASE 11.2: Progress logging voor grote batches
      if (num_batches > 1 && i %% 25 == 0) {
        autofill_log(sprintf("    Batch %d: %d/%d rows processed", 
                            batch_num, i, nrow(batch_rows)), "INFO")
      }
    
      # EDGE CASE: Skip if Castor value is NA/NULL/empty
      if (is.na(castor_value) || is.null(castor_value) || castor_value == "") {
        autofill_log(sprintf("Skipping row %d: Castor value is empty (Element: %s)", global_idx, element_id), "WARNING")
        
        results_list[[length(results_list) + 1]] <- data.table(
          Element = element_id,
          castor_value = "[EMPTY]",
          epic_value_old = row$waarde,
          epic_value_new = NA_character_,
          strategy = "Skipped",
          source = "Empty Castor value",
          confidence = 0,
          tab_name_meta = row$tab_name_meta,
          tab_order_meta = row$tab_order_meta,
          alternatives_count = 0
        )
        next
      }
    
      # Apply all strategies INCLUDING elimination (Pass 1 with original data)
      match_result <- apply_all_strategies(
        castor_value = castor_value,
        element_id = element_id,
        all_data = data,
        ref_dict = ref_dict,
        med_dict = med_dict,
        use_elimination = TRUE,  # ENABLED: Pass 1 can use elimination with original data
        use_ml = use_ml,  # Pass ML setting from process_autofill
        ml_model = ml_model  # Pass pre-loaded model (avoid repeated loading)
      )
    
      if (!is.null(match_result) && !is.null(match_result$best)) {
        best <- match_result$best
      
        # Only include if meets minimum confidence
        if (best$confidence >= min_confidence) {
          result_entry <- data.table(
            Element = element_id,
            castor_value = castor_value,
            epic_value_old = row$waarde,
            epic_value_new = best$epic_value,
            strategy = best$strategy,
            source = best$source,
            confidence = best$confidence,
            tab_name_meta = row$tab_name_meta,
            tab_order_meta = row$tab_order_meta,
            alternatives_count = length(match_result$alternatives) - 1
          )
          results_list[[length(results_list) + 1]] <- result_entry
        
          # Track successful match for Pass 2 elimination
          pass1_successful[[as.character(global_idx)]] <- list(
            element_id = element_id,
            epic_value = best$epic_value
          )
          next
        }
      }
    
      # No match in Pass 1 - will retry in Pass 2 with elimination
      results_list[[length(results_list) + 1]] <- data.table(
        Element = element_id,
        castor_value = castor_value,
        epic_value_old = row$waarde,
        epic_value_new = NA_character_,
        strategy = "No Match (Pass 1)",
        source = "Pending Pass 2",
        confidence = 0,
        tab_name_meta = row$tab_name_meta,
        tab_order_meta = row$tab_order_meta,
        alternatives_count = 0,
        pass1_failed = TRUE  # Mark for Pass 2
      )
    }
    
    # FASE 11.3: Memory cleanup na elke batch
    if (num_batches > 1) {
      gc(verbose = FALSE)
    }
  }  # End of batch loop
  
  autofill_log(sprintf("Pass 1 complete: %d successful matches", length(pass1_successful)))
  
  # ============================================================================
  # PASS 2: Retry with elimination using UPDATED data (Pass 1 results applied)
  # This enables INCREMENTAL elimination: more filled values = better elimination
  # ============================================================================
  
  if (length(pass1_successful) > 0) {
    autofill_log("\n========== PASS 2: Incremental elimination with updated data ==========")
    
    # Create temporary updated dataset with Pass 1 results applied
    # This allows elimination to work with MORE filled values than Pass 1
    temp_data <- copy(data)
    for (success in pass1_successful) {
      mask <- temp_data$Element == success$element_id & 
              (is.na(temp_data$waarde) | temp_data$waarde == "")
      if (sum(mask) > 0) {
        # Update first matching empty row (simulating Pass 1 application)
        temp_data[mask][1, waarde := success$epic_value]
      }
    }
    
    # Retry failed Pass 1 rows with elimination strategy enabled
    pass2_results <- list()
    
    for (i in seq_along(results_list)) {
      result <- results_list[[i]]
      
      # Only retry rows that failed in Pass 1
      if (isTRUE(result$pass1_failed)) {
        element_id <- result$Element
        castor_value <- result$castor_value
        
        autofill_log(sprintf("  Pass 2: Retrying Element %s with elimination", element_id), "INFO")
        
        # Apply strategies WITH elimination, using updated temp_data
        match_result <- apply_all_strategies(
          castor_value = castor_value,
          element_id = element_id,
          all_data = temp_data,  # Use temp data with Pass 1 results
          ref_dict = ref_dict,
          med_dict = med_dict,
          use_elimination = TRUE,  # PASS 2: Enable elimination
          use_ml = use_ml,  # Pass ML setting through
          ml_model = ml_model  # Pass pre-loaded model
        )
        
        if (!is.null(match_result) && !is.null(match_result$best)) {
          best <- match_result$best
          
          if (best$confidence >= min_confidence) {
            # Update result with Pass 2 match
            results_list[[i]] <- data.table(
              Element = element_id,
              castor_value = castor_value,
              epic_value_old = result$epic_value_old,
              epic_value_new = best$epic_value,
              strategy = paste0(best$strategy, " (Pass 2)"),
              source = best$source,
              confidence = best$confidence,
              tab_name_meta = result$tab_name_meta,
              tab_order_meta = result$tab_order_meta,
              alternatives_count = length(match_result$alternatives) - 1
            )
            
            autofill_log(sprintf("  ✓ Pass 2 SUCCESS: %s → %s (strategy: %s)", 
                                castor_value, best$epic_value, best$strategy), "INFO")
            next
          }
        }
        
        # Still no match even with elimination
        results_list[[i]]$strategy <- "No Match"
        results_list[[i]]$source <- "All strategies failed (2 passes)"
        results_list[[i]]$pass1_failed <- NULL
      } else {
        # Remove pass1_failed flag from successful Pass 1 results
        if ("pass1_failed" %in% names(results_list[[i]])) {
          results_list[[i]]$pass1_failed <- NULL
        }
      }
    }
    
    autofill_log(sprintf("Pass 2 complete"))
  }  # End of if (length(pass1_successful) > 0)
  
  # Combine results
  results <- rbindlist(results_list, fill = TRUE)
  
  # FASE 11.4: Enhanced summary met strategy statistics
  autofill_log("\n========== Autofill Summary ==========")
  autofill_log(sprintf("Total processed: %d", nrow(results)))
  
  successful <- sum(results$confidence > 0)
  success_rate <- if (nrow(results) > 0) successful / nrow(results) * 100 else 0
  
  autofill_log(sprintf("Successful matches: %d (%.1f%%)", successful, success_rate))
  autofill_log(sprintf("High confidence (>=95%%): %d", sum(results$confidence >= 95)))
  autofill_log(sprintf("Medium confidence (85-94%%): %d", sum(results$confidence >= 85 & results$confidence < 95)))
  autofill_log(sprintf("Low confidence (70-84%%): %d", sum(results$confidence >= 70 & results$confidence < 85)))
  autofill_log(sprintf("Failed: %d", sum(results$confidence == 0)))
  
  # Strategy breakdown met percentages
  autofill_log("\nStrategy breakdown:")
  if (successful > 0) {
    strategy_counts <- table(results$strategy[results$confidence > 0])
    strategy_sorted <- sort(strategy_counts, decreasing = TRUE)
    
    for (strat in names(strategy_sorted)) {
      count <- strategy_sorted[strat]
      pct <- round((count / successful) * 100, 1)
      autofill_log(sprintf("  %-35s: %3d (%5.1f%%)", strat, count, pct))
    }
    
    # Average confidence per strategy
    autofill_log("\nAverage confidence per strategy:")
    for (strat in names(strategy_sorted)) {
      avg_conf <- mean(results$confidence[results$strategy == strat & results$confidence > 0])
      autofill_log(sprintf("  %-35s: %.1f%%", strat, avg_conf))
    }
  } else {
    autofill_log("  No successful matches to analyze")
  }
  
  autofill_log("========================================\n")
  
  # FASE 10.3: Memory cleanup for large datasets
  if (nrow(results) > 50) {
    gc(verbose = FALSE)
    autofill_log(sprintf("Memory cleanup performed (processed %d rows)", nrow(results)), "INFO")
  }
  
  return(results)
}

# ============================================================================
# MAIN ENTRY POINT (Fase 1 Preview)
# ============================================================================

#' Main function to test Phase 1 functionality
#' @param table_name Name of table to process
#' @param tab_name Optional: specific tab to process
test_phase1 <- function(table_name = "waarde_radiobuttons", tab_name = NULL) {
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("Auto-Fill EPIC Values - Phase 1 Test\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("\n")
  
  # 1. Load data
  cat("Step 1: Loading data...\n")
  data <- load_mappings_data(table_name)
  cat(sprintf("✓ Loaded %d rows\n\n", nrow(data)))
  
  # 2. Build reference dictionary
  cat("Step 2: Building reference dictionary...\n")
  ref_dict <- build_reference_dictionary(data)
  cat(sprintf("✓ Built dictionary with %d mappings\n\n", length(ref_dict)))
  
  # 3. Load medical dictionary
  cat("Step 3: Loading medical dictionary...\n")
  med_dict <- load_medical_dictionary()
  total_med_terms <- length(med_dict$common_terms) + length(med_dict$medical_terms)
  cat(sprintf("✓ Loaded %d medical terms\n\n", total_med_terms))
  
  # 4. Identify empty values
  cat("Step 4: Identifying empty EPIC values...\n")
  empty <- identify_empty_epic_values(data, tab_name = tab_name, review_existing = FALSE)
  cat(sprintf("✓ Found %d empty EPIC values to fill\n\n", nrow(empty)))
  
  # 5. Validate data
  cat("Step 5: Validating data integrity...\n")
  is_valid <- validate_before_autofill(empty)
  if (is_valid) {
    cat("✓ Data validation passed\n\n")
  } else {
    cat("✗ Data validation failed\n\n")
    return(invisible(NULL))
  }
  
  # Determine castor column name
  castor_col <- if ("castor_waarde" %in% names(empty)) "castor_waarde" else "kolom_toevoeging"
  
  # 6. Show summary
  cat("Summary:\n")
  cat(sprintf("  Table: %s\n", table_name))
  if (!is.null(tab_name)) {
    cat(sprintf("  Tab: %s\n", tab_name))
  }
  cat(sprintf("  Total rows: %d\n", nrow(data)))
  cat(sprintf("  Complete mappings: %d\n", length(ref_dict)))
  cat(sprintf("  Empty EPIC values: %d\n", nrow(empty)))
  cat(sprintf("  Unique elements: %d\n", length(unique(empty$Element))))
  cat("\n")
  
  # Show some examples of empty values
  if (nrow(empty) > 0) {
    cat("Examples of empty EPIC values:\n")
    examples <- head(empty[, c("Element", castor_col, "tab_name_meta"), with = FALSE], 10)
    setnames(examples, castor_col, "castor_value")
    print(examples)
  }
  
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("Phase 1 Complete!\n")
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("\n")
  
  # Return results for further processing
  return(invisible(list(
    data = data,
    ref_dict = ref_dict,
    med_dict = med_dict,
    empty = empty,
    is_valid = is_valid
  )))
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

if (FALSE) {
  # Test Phase 1
  results <- test_phase1("waarde_radiobuttons")
  
  # Test with specific tab
  results <- test_phase1("waarde_radiobuttons", tab_name = "Main")
  
  # Test checkboxes
  results <- test_phase1("waarde_checkboxes")
  
  # Test Phase 2 - Full autofill processing
  suggestions <- process_autofill("waarde_radiobuttons")
  View(suggestions)
  
  # Test with specific tab
  suggestions <- process_autofill("waarde_radiobuttons", tab_name = "Main")
  
  # Test with higher confidence threshold
  suggestions <- process_autofill("waarde_radiobuttons", min_confidence = 85)
}

#' Test Phase 2 functionality
#' @param table_name Name of table to process
#' @param tab_name Optional: specific tab
test_phase2 <- function(table_name = "waarde_radiobuttons", tab_name = NULL) {
  cat("\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("Auto-Fill EPIC Values - Phase 2 Test\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("\n")
  
  # Run full autofill process
  cat("Running autofill process...\n\n")
  results <- process_autofill(table_name, tab_name = tab_name, min_confidence = 70)
  
  if (nrow(results) == 0) {
    cat("No results to display.\n")
    return(invisible(NULL))
  }
  
  # Display summary statistics
  cat("\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("RESULTS SUMMARY\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("\n")
  
  cat(sprintf("Total suggestions: %d\n", nrow(results)))
  cat(sprintf("Successful: %d (%.1f%%)\n", 
              sum(results$confidence > 0),
              sum(results$confidence > 0) / nrow(results) * 100))
  cat("\n")
  
  cat("Confidence Distribution:\n")
  cat(sprintf("  High (≥95%%):   %d\n", sum(results$confidence >= 95)))
  cat(sprintf("  Medium (85-94%%): %d\n", sum(results$confidence >= 85 & results$confidence < 95)))
  cat(sprintf("  Low (70-84%%):   %d\n", sum(results$confidence >= 70 & results$confidence < 85)))
  cat(sprintf("  Failed (0%%):    %d\n", sum(results$confidence == 0)))
  cat("\n")
  
  cat("Strategy Breakdown:\n")
  strategy_counts <- table(results$strategy[results$confidence > 0])
  for (strat in names(strategy_counts)) {
    cat(sprintf("  %-20s: %d\n", strat, strategy_counts[strat]))
  }
  cat("\n")
  
  # Show top 10 high-confidence suggestions
  cat("=", rep("=", 70), "\n", sep = "")
  cat("TOP 10 HIGH-CONFIDENCE SUGGESTIONS\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("\n")
  
  high_conf <- results[confidence >= 85][order(-confidence)][1:min(10, .N)]
  if (nrow(high_conf) > 0) {
    print(high_conf[, .(Element, castor_value, epic_value_new, strategy, confidence)])
  } else {
    cat("No high-confidence suggestions.\n")
  }
  
  # Show examples that need review
  cat("\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("EXAMPLES NEEDING REVIEW (Confidence 70-84%)\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("\n")
  
  review <- results[confidence >= 70 & confidence < 85][1:min(5, .N)]
  if (nrow(review) > 0) {
    print(review[, .(Element, castor_value, epic_value_new, strategy, confidence)])
  } else {
    cat("No items need review.\n")
  }
  
  # Show failed matches
  cat("\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("FAILED MATCHES (Manual Review Required)\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("\n")
  
  failed <- results[confidence == 0]
  if (nrow(failed) > 0) {
    cat(sprintf("Total failed: %d\n\n", nrow(failed)))
    print(head(failed[, .(Element, castor_value, source)], 10))
  } else {
    cat("No failed matches - excellent!\n")
  }
  
  cat("\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("Phase 2 Complete!\n")
  cat("=", rep("=", 70), "\n", sep = "")
  cat("\n")
  
  return(invisible(results))
}

# ============================================================================
# HTML PREVIEW GENERATION
# ============================================================================

#' Build HTML preview table for autofill suggestions
#' @param successful data.table with successful matches (confidence > 0)
#' @return HTML string with complete table and summary stats
build_autofill_preview_html <- function(successful) {
  # Calculate summary stats
  total <- nrow(successful)
  high_conf <- sum(successful$confidence >= 95)
  med_conf <- sum(successful$confidence >= 85 & successful$confidence < 95)
  low_conf <- sum(successful$confidence >= 70 & successful$confidence < 85)
  
  # Build HTML table
  table_html <- '<table class="table table-striped table-bordered" style="width:100%; font-size: 12px;">'
  table_html <- paste0(table_html, '<thead><tr>
    <th style="width: 50px; text-align: center;">
      <input type="checkbox" id="select_all_autofill" checked style="cursor: pointer;">
    </th>
    <th style="width: 120px;">Element</th>
    <th style="width: 180px;">Castor Value</th>
    <th style="width: 180px;">Suggested EPIC Value</th>
    <th style="width: 120px;">Strategy</th>
    <th style="width: 80px; text-align: center;">Confidence</th>
  </tr></thead><tbody>')
  
  # Add rows
  if (total > 0) {
    for (i in 1:total) {
      row <- successful[i, ]
      conf_num <- as.numeric(row$confidence)
      
      # Determine background color based on confidence
      bg_color <- if (conf_num >= 95) {
        "#d4edda"  # green
      } else if (conf_num >= 85) {
        "#fff9e6"  # light yellow
      } else {
        "#fff3cd"  # orange
      }
      
      table_html <- paste0(table_html, sprintf(
        '<tr style="background-color: %s;">
          <td style="text-align: center;">
            <input type="checkbox" class="autofill_row_checkbox" data-row-index="%d" checked style="cursor: pointer;">
          </td>
          <td>%s</td>
          <td>%s</td>
          <td><strong>%s</strong></td>
          <td>%s</td>
          <td style="text-align: center;">%s%%</td>
        </tr>',
        bg_color,
        i - 1,  # 0-indexed for JavaScript
        htmltools::htmlEscape(row$Element),
        htmltools::htmlEscape(row$castor_value),
        htmltools::htmlEscape(row$epic_value_new),
        htmltools::htmlEscape(row$strategy),
        conf_num
      ))
    }
  }
  
  table_html <- paste0(table_html, '</tbody></table>')
  
  # Build summary list
  summary_html <- sprintf('
    <h4>Summary</h4>
    <ul>
      <li>Total processed: %d</li>
      <li style="color: green;">High confidence (≥95%%): %d</li>
      <li style="color: orange;">Medium confidence (85-94%%): %d</li>
      <li style="color: #ff9800;">Low confidence (70-84%%): %d</li>
    </ul>
  ', total, high_conf, med_conf, low_conf)
  
  return(list(
    summary = summary_html,
    table = table_html,
    stats = list(
      total = total,
      high = high_conf,
      medium = med_conf,
      low = low_conf
    )
  ))
}

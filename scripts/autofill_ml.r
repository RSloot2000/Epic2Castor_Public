# ============================================================================
# ML-BASED AUTOFILL - MACHINE LEARNING PREDICTION ENGINE
# ============================================================================
# Version: 2025-11-22 (Initial Implementation)
# 
# Purpose: 
#   Enhance autofill with machine learning predictions based on historical
#   approved mappings. Uses XGBoost and TF-IDF text features to learn patterns.
#
# Flow:
#   1. Extract training data from approved mappings
#   2. Build TF-IDF features from Castor values
#   3. Train XGBoost multi-class classifier
#   4. Predict EPIC values with confidence scores
#   5. Log predictions for monitoring and retraining
#
# Usage:
#   source("scripts/autofill_ml.r")
#   
#   # Check if ML is available
#   if (ML_AVAILABLE) {
#     result <- predict_epic_value_ml("Patient has fever", "ELEMENT#001")
#   }
#
# Requirements:
#   - Matrix, text2vec, xgboost packages
#   - Minimum 50 approved mappings for training
# ============================================================================

cat("✓ Loading autofill_ml.r - ML Prediction Engine v2025-11-22\n")

# ===== CHECK ML DEPENDENCIES =====
ML_AVAILABLE <- FALSE

tryCatch({
  suppressPackageStartupMessages({
    library(Matrix, quietly = TRUE)
    library(text2vec, quietly = TRUE)
    library(xgboost, quietly = TRUE)
    library(data.table, quietly = TRUE)
  })
  ML_AVAILABLE <- TRUE
  cat("✓ ML packages loaded successfully\n")
}, error = function(e) {
  cat("⚠ ML packages not available:", conditionMessage(e), "\n")
  cat("  ML autofill will be disabled. To enable, install: Matrix, text2vec, xgboost\n")
})

if (!ML_AVAILABLE) {
  cat("ℹ Continuing without ML autofill functionality\n\n")
  # Define placeholder functions so code doesn't break
  prepare_training_data <- function() NULL
  train_autofill_model <- function() NULL
  predict_epic_value_ml <- function(...) NULL
  
  # Stop processing this file
  return(invisible(NULL))
}

# ============================================================================
# CONFIGURATION
# ============================================================================

# Minimum number of training samples required
ML_MIN_TRAINING_SIZE <- 50

# Confidence threshold for predictions (0-100)
ML_CONFIDENCE_THRESHOLD <- 50

# Model paths
ML_MODEL_DIR <- file.path("config", "ml_models")
ML_MODEL_FILE <- file.path(ML_MODEL_DIR, "autofill_model.xgb")
ML_METADATA_FILE <- file.path(ML_MODEL_DIR, "autofill_metadata.rds")
ML_BACKUP_DIR <- file.path(ML_MODEL_DIR, "backups")
ML_LOG_DIR <- file.path("logs", "ml_predictions")

# Ensure directories exist
if (!dir.exists(ML_MODEL_DIR)) dir.create(ML_MODEL_DIR, recursive = TRUE)
if (!dir.exists(ML_BACKUP_DIR)) dir.create(ML_BACKUP_DIR, recursive = TRUE)
if (!dir.exists(ML_LOG_DIR)) dir.create(ML_LOG_DIR, recursive = TRUE)

# ============================================================================
# STEP 11: ERROR HANDLING & VALIDATION
# ============================================================================

#' Validate model files exist and are readable
#' 
#' Checks that required model files exist, are readable, and not corrupted.
#' 
#' @return List with status:
#'   - valid: Logical, TRUE if all checks pass
#'   - model_exists: Logical
#'   - metadata_exists: Logical
#'   - model_readable: Logical
#'   - metadata_readable: Logical
#'   - message: Character, description of status
#' 
#' @export
validate_model_files <- function() {
  result <- list(
    valid = FALSE,
    model_exists = FALSE,
    metadata_exists = FALSE,
    model_readable = FALSE,
    metadata_readable = FALSE,
    message = ""
  )
  
  # Check model file
  if (!file.exists(ML_MODEL_FILE)) {
    result$message <- "Model file not found"
    return(result)
  }
  result$model_exists <- TRUE
  
  # Check metadata file
  if (!file.exists(ML_METADATA_FILE)) {
    result$message <- "Metadata file not found"
    return(result)
  }
  result$metadata_exists <- TRUE
  
  # Try to load model
  tryCatch({
    xgb.load(ML_MODEL_FILE)
    result$model_readable <- TRUE
  }, error = function(e) {
    result$message <- sprintf("Model file corrupted: %s", conditionMessage(e))
    return(result)
  })
  
  # Try to load metadata
  tryCatch({
    metadata <- readRDS(ML_METADATA_FILE)
    
    # Validate metadata structure (essential fields only)
    required_fields <- c("vectorizer", "tfidf_model", "epic_levels")
    missing_fields <- setdiff(required_fields, names(metadata))
    
    if (length(missing_fields) > 0) {
      result$message <- sprintf("Metadata missing critical fields: %s", 
                               paste(missing_fields, collapse = ", "))
      return(result)
    }
    
    # Check optional but important fields
    if (!"n_samples" %in% names(metadata)) {
      # Backwards compatibility: calculate from epic_levels
      metadata$n_samples <- 0  # Unknown, but don't fail
    }
    
    result$metadata_readable <- TRUE
  }, error = function(e) {
    result$message <- sprintf("Metadata file corrupted: %s", conditionMessage(e))
    return(result)
  })
  
  # All checks passed
  if (result$model_readable && result$metadata_readable) {
    result$valid <- TRUE
    result$message <- "Model files valid"
  }
  
  return(result)
}

#' Validate input for ML prediction
#' 
#' Checks if input is valid for making predictions.
#' 
#' @param castor_value Character. Input value to validate
#' @param element Character or NULL. Element ID (optional)
#' 
#' @return List with:
#'   - valid: Logical, TRUE if input is valid
#'   - reason: Character, reason if invalid
#' 
#' @export
validate_prediction_input <- function(castor_value, element = NULL) {
  # Check NULL
  if (is.null(castor_value)) {
    return(list(valid = FALSE, reason = "Input is NULL"))
  }
  
  # Check type
  if (!is.character(castor_value)) {
    return(list(valid = FALSE, reason = "Input is not character"))
  }
  
  # Check length
  if (length(castor_value) == 0) {
    return(list(valid = FALSE, reason = "Input vector is empty"))
  }
  
  # Check content
  if (nchar(trimws(castor_value[1])) == 0) {
    return(list(valid = FALSE, reason = "Input string is empty or whitespace"))
  }
  
  # Check element if provided
  if (!is.null(element) && !is.character(element)) {
    return(list(valid = FALSE, reason = "Element ID is not character"))
  }
  
  return(list(valid = TRUE, reason = "Input valid"))
}

#' Safe file operation wrapper
#' 
#' Wraps file operations with error handling and retry logic.
#' 
#' @param operation Function to execute
#' @param max_retries Integer. Maximum number of retry attempts
#' @param error_message Character. Custom error message prefix
#' 
#' @return Result of operation or NULL on failure
#' 
#' @export
safe_file_operation <- function(operation, max_retries = 3, error_message = "File operation failed") {
  attempt <- 1
  last_error <- NULL
  
  while (attempt <= max_retries) {
    result <- tryCatch({
      operation()
    }, error = function(e) {
      last_error <<- e
      NULL
    })
    
    if (!is.null(result)) {
      return(result)
    }
    
    # Wait before retry (exponential backoff)
    if (attempt < max_retries) {
      Sys.sleep(0.1 * attempt)
    }
    attempt <- attempt + 1
  }
  
  # All retries failed
  warning(sprintf("%s after %d attempts: %s", 
                  error_message, max_retries, 
                  if (!is.null(last_error)) conditionMessage(last_error) else "Unknown error"))
  return(NULL)
}

#' Recover from corrupted model files
#' 
#' Attempts to restore model from backup if current files are corrupted.
#' 
#' @return Logical. TRUE if recovery successful, FALSE otherwise
#' 
#' @export
recover_model_from_backup <- function() {
  cat("\n=== Attempting Model Recovery ===\n\n")
  
  # Find most recent backup
  backup_files <- list.files(ML_BACKUP_DIR, pattern = "autofill_model_.*\\.xgb", full.names = TRUE)
  
  if (length(backup_files) == 0) {
    cat("✗ No backup files found\n")
    return(FALSE)
  }
  
  # Sort by modification time (most recent first)
  backup_files <- backup_files[order(file.mtime(backup_files), decreasing = TRUE)]
  
  cat(sprintf("Found %d backup(s)\n", length(backup_files)))
  
  # Try each backup until one works
  for (i in seq_along(backup_files)) {
    backup_model <- backup_files[i]
    backup_date <- sub(".*autofill_model_(\\d{8}_\\d{6})\\.xgb", "\\1", basename(backup_model))
    backup_metadata <- file.path(ML_BACKUP_DIR, sprintf("autofill_metadata_%s.rds", backup_date))
    
    cat(sprintf("Trying backup from %s...\n", backup_date))
    
    # Check if metadata exists
    if (!file.exists(backup_metadata)) {
      cat("  ✗ Metadata backup not found\n")
      next
    }
    
    # Try to validate backup
    tryCatch({
      xgb.load(backup_model)
      metadata <- readRDS(backup_metadata)
      
      # Backup is valid, restore it
      file.copy(backup_model, ML_MODEL_FILE, overwrite = TRUE)
      file.copy(backup_metadata, ML_METADATA_FILE, overwrite = TRUE)
      
      cat(sprintf("  ✓ Restored backup from %s\n", backup_date))
      cat("  Model files recovered successfully\n\n")
      return(TRUE)
      
    }, error = function(e) {
      cat(sprintf("  ✗ Backup corrupted: %s\n", conditionMessage(e)))
    })
  }
  
  cat("✗ All backups failed. Manual intervention required.\n\n")
  return(FALSE)
}

# ============================================================================
# STEP 2: TRAINING DATA PREPARATION
# ============================================================================

#' Prepare training data from approved mappings
#' 
#' Extracts approved Castor→EPIC mappings from waarde_radiobuttons and 
#' waarde_checkboxes CSV files. Filters out empty or invalid mappings.
#' 
#' @return data.table with columns: Element, castor_value, epic_value, source
#' @export
prepare_training_data <- function() {
  cat("\n=== Preparing ML Training Data ===\n\n")
  
  # Load path configuration if not already loaded
  if (!exists("epc_path")) {
    source("scripts/config.R", local = TRUE)
  }
  
  training_data <- data.table()
  
  # ----- Extract from waarde_radiobuttons -----
  cat("Loading radiobutton mappings...\n")
  radio_file <- epc_path("mapping_radiobuttons_file")
  
  if (file.exists(radio_file)) {
    radio_data <- tryCatch({
      fread(radio_file, encoding = "UTF-8")
    }, error = function(e) {
      cat("  ⚠ Error reading radiobuttons file:", conditionMessage(e), "\n")
      data.table()
    })
    
    if (nrow(radio_data) > 0) {
      # Filter valid mappings
      radio_valid <- radio_data[
        !is.na(waarde) & waarde != "" & 
        !is.na(castor_waarde) & castor_waarde != "" &
        !is.na(Element) & Element != ""
      ]
      
      if (nrow(radio_valid) > 0) {
        radio_training <- radio_valid[, .(
          Element = Element,
          castor_value = castor_waarde,
          epic_value = waarde,
          source = "radiobuttons"
        )]
        
        training_data <- rbind(training_data, radio_training)
        cat(sprintf("  ✓ Extracted %d radiobutton mappings\n", nrow(radio_training)))
      }
    }
  } else {
    cat("  ⚠ Radiobuttons file not found:", radio_file, "\n")
  }
  
  # ----- Extract from waarde_checkboxes -----
  cat("Loading checkbox mappings...\n")
  checkbox_file <- epc_path("mapping_checkboxes_file")
  
  if (file.exists(checkbox_file)) {
    checkbox_data <- tryCatch({
      fread(checkbox_file, encoding = "UTF-8")
    }, error = function(e) {
      cat("  ⚠ Error reading checkboxes file:", conditionMessage(e), "\n")
      data.table()
    })
    
    if (nrow(checkbox_data) > 0) {
      # Filter valid mappings
      checkbox_valid <- checkbox_data[
        !is.na(waarde) & waarde != "" & 
        !is.na(kolom_toevoeging) & kolom_toevoeging != "" &
        !is.na(Element) & Element != ""
      ]
      
      if (nrow(checkbox_valid) > 0) {
        checkbox_training <- checkbox_valid[, .(
          Element = Element,
          castor_value = kolom_toevoeging,
          epic_value = waarde,
          source = "checkboxes"
        )]
        
        training_data <- rbind(training_data, checkbox_training)
        cat(sprintf("  ✓ Extracted %d checkbox mappings\n", nrow(checkbox_training)))
      }
    }
  } else {
    cat("  ⚠ Checkboxes file not found:", checkbox_file, "\n")
  }
  
  # ----- Summary -----
  cat("\n--- Training Data Summary ---\n")
  cat(sprintf("Total records: %d\n", nrow(training_data)))
  
  if (nrow(training_data) > 0) {
    cat(sprintf("Unique Elements: %d\n", uniqueN(training_data$Element)))
    cat(sprintf("Unique Castor values: %d\n", uniqueN(training_data$castor_value)))
    cat(sprintf("Unique EPIC values: %d\n", uniqueN(training_data$epic_value)))
    
    # Show source distribution
    source_counts <- training_data[, .N, by = source]
    cat("\nBy source:\n")
    for (i in seq_len(nrow(source_counts))) {
      cat(sprintf("  %s: %d\n", source_counts$source[i], source_counts$N[i]))
    }
    
    # Check for minimum size
    if (nrow(training_data) < ML_MIN_TRAINING_SIZE) {
      cat(sprintf("\n⚠ Warning: Only %d records available. Minimum %d required for training.\n",
                  nrow(training_data), ML_MIN_TRAINING_SIZE))
    } else {
      cat(sprintf("\n✓ Sufficient data for training (%d >= %d)\n",
                  nrow(training_data), ML_MIN_TRAINING_SIZE))
    }
  } else {
    cat("\n⚠ No training data found!\n")
    cat("  Make sure you have approved mappings in:\n")
    cat("  - mapping/waarde_radiobuttons.csv\n")
    cat("  - mapping/waarde_checkboxes.csv\n")
  }
  
  cat("\n")
  return(training_data)
}

# ============================================================================
# STEP 3: TEXT PREPROCESSING
# ============================================================================

#' Preprocess text for ML model
#' 
#' Normalizes text by:
#' - Converting to lowercase
#' - Removing special characters
#' - Normalizing whitespace
#' - Preserving numbers
#' 
#' @param text Character vector of text to preprocess
#' @return Character vector of cleaned text
#' @export
prep_text <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(character(0))
  }
  
  # Convert to character and handle NA
  text <- as.character(text)
  text[is.na(text)] <- ""
  
  # Lowercase
  text <- tolower(text)
  
  # Remove special characters but keep spaces and numbers
  text <- gsub("[^a-z0-9 ]", " ", text)
  
  # Normalize whitespace
  text <- gsub("\\s+", " ", text)
  
  # Trim
  text <- trimws(text)
  
  return(text)
}

# ============================================================================
# STEP 4: FEATURE ENGINEERING
# ============================================================================

#' Create TF-IDF features from training data
#' 
#' Builds vocabulary from Castor values and creates document-term matrix
#' using TF-IDF (Term Frequency - Inverse Document Frequency) weighting.
#' 
#' @param training_data data.table with castor_value and epic_value columns
#' @param min_term_count Minimum times a term must appear to be included (default: 2)
#' @return List with vocabulary, vectorizer, and document-term matrix
#' @export
create_features <- function(training_data, min_term_count = 2) {
  cat("\n=== Creating TF-IDF Features ===\n\n")
  
  if (nrow(training_data) == 0) {
    cat("⚠ No training data provided\n")
    return(NULL)
  }
  
  # Preprocess text
  cat("Preprocessing text...\n")
  training_data[, castor_clean := prep_text(castor_value)]
  
  # Remove empty texts
  valid_data <- training_data[castor_clean != ""]
  
  if (nrow(valid_data) == 0) {
    cat("⚠ No valid text after preprocessing\n")
    return(NULL)
  }
  
  cat(sprintf("  ✓ Preprocessed %d documents\n", nrow(valid_data)))
  
  # Create iterator
  cat("Building vocabulary...\n")
  it <- itoken(valid_data$castor_clean, progressbar = FALSE)
  
  # Create vocabulary
  vocab <- create_vocabulary(it)
  
  cat(sprintf("  Initial vocabulary size: %d terms\n", nrow(vocab)))
  
  # Prune vocabulary (remove rare terms)
  if (min_term_count > 1) {
    vocab <- prune_vocabulary(vocab, term_count_min = min_term_count)
    cat(sprintf("  Pruned vocabulary size: %d terms (min count: %d)\n", 
                nrow(vocab), min_term_count))
  }
  
  # Check if vocabulary is too small
  if (nrow(vocab) < 5) {
    cat("⚠ Warning: Very small vocabulary. Consider lowering min_term_count.\n")
  }
  
  # Create vectorizer
  cat("Creating vectorizer...\n")
  vectorizer <- vocab_vectorizer(vocab)
  
  # Create document-term matrix
  cat("Generating document-term matrix...\n")
  it <- itoken(valid_data$castor_clean, progressbar = FALSE)
  dtm <- create_dtm(it, vectorizer)
  
  # Calculate sparsity
  sparsity <- 1 - (length(dtm@x) / (dtm@Dim[1] * dtm@Dim[2]))
  
  cat(sprintf("  ✓ DTM dimensions: %d documents x %d features\n", 
              dtm@Dim[1], dtm@Dim[2]))
  cat(sprintf("  Matrix sparsity: %.1f%%\n", sparsity * 100))
  
  # Calculate TF-IDF weights
  cat("Applying TF-IDF transformation...\n")
  tfidf_model <- TfIdf$new()
  dtm_tfidf <- fit_transform(dtm, tfidf_model)
  
  cat("  ✓ TF-IDF transformation complete\n")
  
  result <- list(
    vocab = vocab,
    vectorizer = vectorizer,
    tfidf_model = tfidf_model,
    dtm = dtm_tfidf,
    training_data = valid_data
  )
  
  cat("\n✓ Feature engineering complete\n\n")
  
  return(result)
}

#' Transform new text data using existing vectorizer
#' 
#' Applies the same preprocessing and vectorization to new data
#' 
#' @param text Character vector of new text to transform
#' @param vectorizer Trained vectorizer from create_features()
#' @param tfidf_model Trained TF-IDF model from create_features()
#' @return Document-term matrix for the new text
#' @export
transform_new_data <- function(text, vectorizer, tfidf_model) {
  # Preprocess
  text_clean <- prep_text(text)
  
  # Vectorize
  it <- itoken(text_clean, progressbar = FALSE)
  dtm <- create_dtm(it, vectorizer)
  
  # Apply TF-IDF
  dtm_tfidf <- transform(dtm, tfidf_model)
  
  return(dtm_tfidf)
}

# ============================================================================
# STEP 5: MODEL TRAINING
# ============================================================================

#' Train XGBoost model for EPIC value prediction
#' 
#' Trains a multi-class classifier using TF-IDF features from Castor values
#' to predict EPIC values. Includes train/validation split and performance
#' evaluation.
#' 
#' @param training_data data.table from prepare_training_data() (optional)
#' @param validation_split Fraction of data to use for validation (default: 0.2)
#' @param nrounds Number of boosting rounds (default: 50)
#' @param max_depth Maximum tree depth (default: 6)
#' @param eta Learning rate (default: 0.3)
#' @param verbose Verbosity level (default: 0 = silent)
#' @return List with model, vectorizer, tfidf_model, epic_levels, and metrics
#' @export
train_autofill_model <- function(training_data = NULL, 
                                   validation_split = 0.2,
                                   nrounds = 50,
                                   max_depth = 6,
                                   eta = 0.3,
                                   verbose = 0) {
  
  cat("\n=== Training ML Autofill Model ===\n\n")
  
  # Step 11: Wrap entire training in error handler
  tryCatch({
  
  # Load training data if not provided
  if (is.null(training_data)) {
    cat("Loading training data...\n")
    training_data <- prepare_training_data()
  }
  
  # Check minimum size
  if (nrow(training_data) < ML_MIN_TRAINING_SIZE) {
    cat(sprintf("⚠ Insufficient training data (%d < %d). Cannot train model.\n",
                nrow(training_data), ML_MIN_TRAINING_SIZE))
    return(NULL)
  }
  
  cat(sprintf("Training data: %d records\n", nrow(training_data)))
  
  # Create features
  features_result <- create_features(training_data)
  
  if (is.null(features_result)) {
    cat("⚠ Feature creation failed\n")
    return(NULL)
  }
  
  dtm <- features_result$dtm
  valid_data <- features_result$training_data
  
  # Encode labels (EPIC values -> integer indices)
  cat("Encoding labels...\n")
  epic_levels <- sort(unique(valid_data$epic_value))
  n_classes <- length(epic_levels)
  
  cat(sprintf("  Number of classes: %d\n", n_classes))
  
  if (n_classes < 2) {
    cat("⚠ Need at least 2 different EPIC values for training\n")
    return(NULL)
  }
  
  labels <- match(valid_data$epic_value, epic_levels) - 1  # 0-indexed for xgboost
  
  # Split train/validation
  cat(sprintf("\nSplitting data (%.0f%% train, %.0f%% validation)...\n",
              (1 - validation_split) * 100, validation_split * 100))
  
  set.seed(42)  # For reproducibility
  n_total <- nrow(dtm)
  n_val <- ceiling(n_total * validation_split)
  val_idx <- sample(seq_len(n_total), n_val)
  train_idx <- setdiff(seq_len(n_total), val_idx)
  
  cat(sprintf("  Training samples: %d\n", length(train_idx)))
  cat(sprintf("  Validation samples: %d\n", length(val_idx)))
  
  dtm_train <- dtm[train_idx, ]
  labels_train <- labels[train_idx]
  
  dtm_val <- dtm[val_idx, ]
  labels_val <- labels[val_idx]
  
  # Train XGBoost model
  cat("\nTraining XGBoost model...\n")
  cat(sprintf("  Parameters: nrounds=%d, max_depth=%d, eta=%.2f\n", 
              nrounds, max_depth, eta))
  
  start_time <- Sys.time()
  
  model <- xgboost(
    data = dtm_train,
    label = labels_train,
    nrounds = nrounds,
    objective = "multi:softprob",
    num_class = n_classes,
    max_depth = max_depth,
    eta = eta,
    verbose = verbose
  )
  
  train_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("  ✓ Training completed in %.1f seconds\n", train_time))
  
  # Evaluate on validation set
  cat("\nEvaluating model...\n")
  
  # Predictions on validation set
  pred_val <- predict(model, dtm_val)
  pred_matrix <- matrix(pred_val, ncol = n_classes, byrow = TRUE)
  pred_classes <- apply(pred_matrix, 1, which.max) - 1  # Back to 0-indexed
  
  # Calculate accuracy
  accuracy <- mean(pred_classes == labels_val)
  cat(sprintf("  Validation accuracy: %.1f%%\n", accuracy * 100))
  
  # Confusion matrix (simplified)
  if (n_classes <= 10) {  # Only show for small number of classes
    cat("\n  Confusion Matrix (top 5 classes):\n")
    conf_data <- data.table(
      actual = epic_levels[labels_val + 1],
      predicted = epic_levels[pred_classes + 1]
    )
    conf_table <- conf_data[, .N, by = .(actual, predicted)]
    print(head(conf_table[order(-N)], 5))
  }
  
  # Calculate confidence distribution
  max_probs <- apply(pred_matrix, 1, max)
  cat("\n  Confidence distribution:\n")
  cat(sprintf("    Mean: %.1f%%\n", mean(max_probs) * 100))
  cat(sprintf("    Median: %.1f%%\n", median(max_probs) * 100))
  cat(sprintf("    Min: %.1f%%\n", min(max_probs) * 100))
  cat(sprintf("    Max: %.1f%%\n", max(max_probs) * 100))
  
  # High confidence predictions (>80%)
  high_conf <- sum(max_probs > 0.8)
  cat(sprintf("    High confidence (>80%%): %d/%d (%.1f%%)\n",
              high_conf, length(max_probs), high_conf / length(max_probs) * 100))
  
  # Package result
  model_info <- list(
    model = model,
    vectorizer = features_result$vectorizer,
    tfidf_model = features_result$tfidf_model,
    epic_levels = epic_levels,
    n_classes = n_classes,
    training_date = Sys.time(),
    training_size = nrow(training_data),
    n_features = ncol(dtm),
    hyperparameters = list(
      nrounds = nrounds,
      max_depth = max_depth,
      eta = eta
    ),
    metrics = list(
      validation_accuracy = accuracy,
      mean_confidence = mean(max_probs),
      median_confidence = median(max_probs),
      high_confidence_pct = high_conf / length(max_probs),
      train_time_secs = train_time
    )
  )
  
  cat("\n✓ Model training complete!\n\n")
  
  return(model_info)
  
  }, error = function(e) {
    # Step 11: Handle training errors gracefully
    cat("\n✗ Error during model training:\n")
    cat(sprintf("  %s\n", conditionMessage(e)))
    cat("\nTraining failed. Please check:\n")
    cat("  1. Training data quality and format\n")
    cat("  2. Available memory (XGBoost requires sufficient RAM)\n")
    cat("  3. Package versions (xgboost, text2vec, Matrix)\n\n")
    return(NULL)
  })
}

# ============================================================================
# PLACEHOLDER FUNCTIONS FOR NEXT STEPS
# ============================================================================
# These will be implemented in subsequent steps

# ============================================================================
# STEP 7: PREDICTION FUNCTION
# ============================================================================

#' Predict EPIC value using ML model (Step 7)
#' 
#' Uses trained XGBoost model to predict EPIC value from Castor value.
#' Returns prediction only if confidence exceeds ML_CONFIDENCE_THRESHOLD.
#' 
#' @param castor_value Character string with Castor field value to predict from
#' @param element Character string with Element ID (optional, for logging)
#' @param row_data Named list with additional row context (optional, currently unused)
#' @param ref_dict Reference dictionary (optional, currently unused)
#' @param model_info Optional pre-loaded model. If NULL, loads from disk.
#' 
#' @return List with prediction details if confident, NULL otherwise:
#'   - epic_value: Predicted EPIC value
#'   - confidence: Confidence score (0-100)
#'   - strategy: "ML Prediction"
#'   - source: "Machine Learning (XGBoost)"
#'   - alternatives: Top 3 alternative predictions with confidences
#' 
#' @export
predict_epic_value_ml <- function(castor_value, element = NULL, row_data = NULL, 
                                   ref_dict = NULL, model_info = NULL) {
  
  # Step 11: Validate input with improved error handling
  validation <- validate_prediction_input(castor_value, element)
  if (!validation$valid) {
    # Silently return NULL for invalid input (don't disrupt autofill)
    return(NULL)
  }
  
  # Load model if not provided
  if (is.null(model_info)) {
    # Step 11: Validate model files before loading
    file_status <- validate_model_files()
    
    if (!file_status$valid) {
      # Try to recover from backup
      if (file_status$model_exists && !file_status$model_readable) {
        recovery_success <- recover_model_from_backup()
        if (!recovery_success) {
          return(NULL)
        }
      } else {
        # No model available
        return(NULL)
      }
    }
    
    # Load model with error handling
    model_info <- safe_file_operation(
      operation = function() load_model(verbose = FALSE),
      max_retries = 2,
      error_message = "Failed to load ML model"
    )
    
    if (is.null(model_info)) {
      return(NULL)
    }
  }
  
  tryCatch({
    # Preprocess input text
    processed_text <- prep_text(castor_value)
    
    # Transform to feature matrix (suppress "dtm has 0 rows" warnings)
    features <- suppressWarnings(
      transform_new_data(
        processed_text, 
        model_info$vectorizer, 
        model_info$tfidf_model
      )
    )
    
    # Get predictions (probabilities for all classes)
    probs <- predict(model_info$model, features)
    
    # XGBoost returns flat vector for multiclass, reshape if needed
    if (length(probs) != length(model_info$epic_levels)) {
      # Reshape to matrix (n_samples x n_classes)
      n_classes <- length(model_info$epic_levels)
      probs <- matrix(probs, ncol = n_classes, byrow = TRUE)
      probs <- probs[1, ]  # Take first row (single prediction)
    }
    
    # Get top 3 predictions
    top3_idx <- order(probs, decreasing = TRUE)[1:3]
    top3_probs <- probs[top3_idx]
    top3_values <- model_info$epic_levels[top3_idx]
    
    # Convert to percentage
    top1_confidence <- top3_probs[1] * 100
    
    # Check if confidence meets threshold
    if (top1_confidence < ML_CONFIDENCE_THRESHOLD) {
      return(NULL)
    }
    
    # Build alternatives list
    alternatives <- list()
    if (length(top3_probs) >= 2) {
      for (i in 2:min(3, length(top3_probs))) {
        alternatives[[i-1]] <- list(
          value = top3_values[i],
          confidence = round(top3_probs[i] * 100, 1)
        )
      }
    }
    
    # Return prediction
    result <- list(
      epic_value = top3_values[1],
      confidence = round(top1_confidence, 1),
      strategy = "ML Prediction",
      source = "Machine Learning (XGBoost)",
      alternatives = alternatives,
      element = element,
      castor_value = castor_value
    )
    
    # Log prediction for performance monitoring (Step 10)
    log_ml_prediction(
      element_id = element,
      castor_value = castor_value,
      prediction = result$epic_value,
      confidence = result$confidence,
      accepted = NULL,  # Will be updated when user accepts/rejects
      strategy = "ML"
    )
    
    return(result)
    
  }, error = function(e) {
    # Fail silently to not disrupt autofill flow
    cat("⚠ ML prediction error:", conditionMessage(e), "\n")
    cat("   Full error:\n")
    print(e)
    return(NULL)
  })
}

#' Save trained model (Step 6)
#' @export
#' Save trained model to disk (Step 6)
#' 
#' Saves XGBoost model and associated metadata to ML_MODEL_DIR.
#' Creates backup of previous model if it exists.
#' 
#' @param model_info List returned by train_autofill_model() containing:
#'   - model: XGBoost model object
#'   - vectorizer: text2vec itoken/vocabulary object
#'   - tfidf_model: TF-IDF model
#'   - epic_levels: Character vector of class labels
#'   - metrics: Named list with validation_accuracy, mean_confidence, etc.
#' @param overwrite If FALSE, creates backup before overwriting
#' 
#' @return TRUE if save successful, FALSE otherwise
#' @export
save_model <- function(model_info, overwrite = FALSE) {
  cat("\n=== Saving Model to Disk ===\n\n")
  
  # Validate input
  if (is.null(model_info) || !is.list(model_info)) {
    cat("✗ Error: model_info must be a list\n")
    return(FALSE)
  }
  
  required_fields <- c("model", "vectorizer", "tfidf_model", "epic_levels")
  missing_fields <- setdiff(required_fields, names(model_info))
  if (length(missing_fields) > 0) {
    cat("✗ Error: model_info missing required fields:", paste(missing_fields, collapse = ", "), "\n")
    return(FALSE)
  }
  
  tryCatch({
    # Backup existing model if it exists
    if (file.exists(ML_MODEL_FILE) && !overwrite) {
      backup_name <- paste0("autofill_model_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xgb")
      backup_path <- file.path(ML_BACKUP_DIR, backup_name)
      
      cat("Creating backup of existing model...\n")
      file.copy(ML_MODEL_FILE, backup_path)
      cat("  ✓ Backup saved to:", backup_path, "\n\n")
      
      # Also backup metadata
      if (file.exists(ML_METADATA_FILE)) {
        backup_meta_name <- paste0("autofill_metadata_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
        backup_meta_path <- file.path(ML_BACKUP_DIR, backup_meta_name)
        file.copy(ML_METADATA_FILE, backup_meta_path)
      }
    }
    
    # Save XGBoost model
    cat("Saving XGBoost model...\n")
    xgb.save(model_info$model, ML_MODEL_FILE)
    cat("  ✓ Model saved to:", ML_MODEL_FILE, "\n")
    
    # Prepare metadata
    metadata <- list(
      vectorizer = model_info$vectorizer,
      tfidf_model = model_info$tfidf_model,
      epic_levels = model_info$epic_levels,
      metrics = model_info$metrics,
      training_date = Sys.time(),
      r_version = R.version.string,
      xgboost_version = as.character(packageVersion("xgboost")),
      text2vec_version = as.character(packageVersion("text2vec"))
    )
    
    # Save metadata
    cat("Saving metadata...\n")
    saveRDS(metadata, ML_METADATA_FILE)
    cat("  ✓ Metadata saved to:", ML_METADATA_FILE, "\n")
    
    # Summary
    cat("\n--- Save Summary ---\n")
    cat(sprintf("Model file size: %.1f KB\n", file.size(ML_MODEL_FILE) / 1024))
    cat(sprintf("Metadata file size: %.1f KB\n", file.size(ML_METADATA_FILE) / 1024))
    if (!is.null(model_info$metrics)) {
      cat(sprintf("Validation accuracy: %.1f%%\n", model_info$metrics$validation_accuracy * 100))
      cat(sprintf("Number of classes: %d\n", length(model_info$epic_levels)))
    }
    cat(sprintf("Training date: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    
    cat("\n✓ Model saved successfully!\n\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("✗ Error saving model:", conditionMessage(e), "\n")
    return(FALSE)
  })
}

#' Load trained model from disk (Step 6)
#' 
#' Loads XGBoost model and metadata from ML_MODEL_DIR.
#' Validates that all required components are present.
#' 
#' @param verbose If TRUE, print loading messages to console (default: FALSE)
#' @return List with same structure as train_autofill_model() output, or NULL if load fails
#' @export
load_model <- function(verbose = FALSE) {
  if (verbose) cat("\n=== Loading Model from Disk ===\n\n")
  
  # Check if model files exist
  if (!file.exists(ML_MODEL_FILE)) {
    if (verbose) {
      cat("✗ Error: Model file not found:", ML_MODEL_FILE, "\n")
      cat("  Run train_autofill_model() first to create a model.\n\n")
    }
    return(NULL)
  }
  
  if (!file.exists(ML_METADATA_FILE)) {
    if (verbose) {
      cat("✗ Error: Metadata file not found:", ML_METADATA_FILE, "\n")
      cat("  Model file exists but metadata is missing.\n\n")
    }
    return(NULL)
  }
  
  tryCatch({
    # Load XGBoost model (suppress XGBoost console output)
    if (verbose) cat("Loading XGBoost model...\n")
    model <- xgb.load(ML_MODEL_FILE)
    if (verbose) cat("  ✓ Model loaded from:", ML_MODEL_FILE, "\n")
    
    # Load metadata
    if (verbose) cat("Loading metadata...\n")
    metadata <- readRDS(ML_METADATA_FILE)
    if (verbose) cat("  ✓ Metadata loaded from:", ML_METADATA_FILE, "\n")
    
    # Validate metadata structure
    required_fields <- c("vectorizer", "tfidf_model", "epic_levels")
    missing_fields <- setdiff(required_fields, names(metadata))
    if (length(missing_fields) > 0) {
      cat("✗ Error: Metadata missing required fields:", paste(missing_fields, collapse = ", "), "\n")
      return(NULL)
    }
    
    # Reconstruct model_info object
    model_info <- list(
      model = model,
      vectorizer = metadata$vectorizer,
      tfidf_model = metadata$tfidf_model,
      epic_levels = metadata$epic_levels,
      metrics = metadata$metrics,
      training_date = metadata$training_date
    )
    
    # Summary (only if verbose)
    if (verbose) {
      cat("\n--- Model Summary ---\n")
      if (!is.null(metadata$training_date)) {
        cat(sprintf("Training date: %s\n", format(metadata$training_date, "%Y-%m-%d %H:%M:%S")))
        days_old <- as.numeric(difftime(Sys.time(), metadata$training_date, units = "days"))
        cat(sprintf("Model age: %.1f days\n", days_old))
      }
      if (!is.null(metadata$metrics)) {
        cat(sprintf("Validation accuracy: %.1f%%\n", metadata$metrics$validation_accuracy * 100))
        cat(sprintf("Mean confidence: %.1f%%\n", metadata$metrics$mean_confidence * 100))
      }
      cat(sprintf("Number of classes: %d\n", length(metadata$epic_levels)))
      cat(sprintf("R version: %s\n", metadata$r_version))
      cat(sprintf("XGBoost version: %s\n", metadata$xgboost_version))
      cat("\n✓ Model loaded successfully!\n\n")
    }
    
    return(model_info)
    
  }, error = function(e) {
    if (verbose) cat("✗ Error loading model:", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Retrain ML model with latest approved mappings (Step 9)
#' 
#' Incrementally retrains the autofill model by incorporating new approved mappings
#' from the radiobuttons and checkboxes CSV files. Can be triggered manually or
#' automatically when sufficient new data is available.
#' 
#' @param force Logical. If TRUE, retrain even if not enough new data. Default FALSE.
#' @param min_new_samples Minimum number of new samples required before retraining (default 20)
#' @param verbose Logical. If TRUE, print detailed progress. Default TRUE.
#' 
#' @return List with retraining results:
#'   - success: Logical, TRUE if retraining completed
#'   - new_samples: Number of new samples added
#'   - total_samples: Total samples after retraining
#'   - metrics: New model performance metrics
#'   - message: Status message
#' 
#' @export
retrain_model <- function(force = FALSE, min_new_samples = 20, verbose = TRUE) {
  
  if (!ML_AVAILABLE) {
    return(list(
      success = FALSE,
      message = "ML packages not available. Cannot retrain model."
    ))
  }
  
  if (verbose) cat("\n=== Model Retraining Pipeline ===\n\n")
  
  # Step 1: Check if model exists
  model_path <- "config/ml_models/autofill_model.xgb"
  metadata_path <- "config/ml_models/autofill_metadata.rds"
  
  if (!file.exists(model_path) || !file.exists(metadata_path)) {
    if (verbose) cat("ℹ No existing model found. Training initial model...\n")
    result <- train_autofill_model(verbose = verbose)
    return(list(
      success = !is.null(result),
      new_samples = if (!is.null(result)) result$n_samples else 0,
      total_samples = if (!is.null(result)) result$n_samples else 0,
      metrics = if (!is.null(result)) result$metrics else NULL,
      message = if (!is.null(result)) "Initial model trained successfully" else "Model training failed"
    ))
  }
  
  # Step 2: Load existing metadata
  if (verbose) cat("📂 Loading existing model metadata...\n")
  metadata <- readRDS(metadata_path)
  old_sample_count <- metadata$n_samples
  old_training_date <- metadata$training_date
  
  if (verbose) {
    cat(sprintf("  Current model: %d samples, trained %s\n", 
                old_sample_count, 
                format(old_training_date, "%Y-%m-%d %H:%M")))
  }
  
  # Step 3: Prepare new training data
  if (verbose) cat("\n📊 Preparing current training data...\n")
  training_data <- prepare_training_data()
  
  if (is.null(training_data) || nrow(training_data) == 0) {
    return(list(
      success = FALSE,
      message = "Failed to prepare training data"
    ))
  }
  
  new_sample_count <- nrow(training_data)
  new_samples <- new_sample_count - old_sample_count
  
  if (verbose) {
    cat(sprintf("  Total samples available: %d\n", new_sample_count))
    cat(sprintf("  New samples since last training: %d\n", new_samples))
  }
  
  # Step 4: Check if retraining is needed
  if (!force && new_samples < min_new_samples) {
    if (verbose) {
      cat(sprintf("\nℹ Insufficient new data for retraining\n"))
      cat(sprintf("  Need at least %d new samples, but only %d available\n", 
                  min_new_samples, new_samples))
      cat("  Use force=TRUE to retrain anyway, or wait for more data\n")
    }
    return(list(
      success = FALSE,
      new_samples = new_samples,
      total_samples = new_sample_count,
      message = sprintf("Not enough new data (%d/%d samples)", new_samples, min_new_samples)
    ))
  }
  
  # Step 5: Backup old model
  if (verbose) cat("\n💾 Backing up current model...\n")
  backup_dir <- "config/ml_models/backups"
  dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
  
  backup_suffix <- format(old_training_date, "%Y%m%d_%H%M%S")
  backup_model_path <- file.path(backup_dir, sprintf("autofill_model_%s.xgb", backup_suffix))
  backup_metadata_path <- file.path(backup_dir, sprintf("autofill_metadata_%s.rds", backup_suffix))
  
  tryCatch({
    file.copy(model_path, backup_model_path, overwrite = TRUE)
    file.copy(metadata_path, backup_metadata_path, overwrite = TRUE)
    if (verbose) cat(sprintf("  ✓ Backup saved: %s\n", backup_suffix))
  }, error = function(e) {
    if (verbose) cat(sprintf("  ⚠ Backup failed: %s\n", conditionMessage(e)))
  })
  
  # Step 6: Retrain model with all data
  if (verbose) {
    cat("\n🔄 Retraining model with updated data...\n")
    if (force && new_samples < min_new_samples) {
      cat("  (Force mode: retraining with fewer new samples)\n")
    }
  }
  
  result <- train_autofill_model(verbose = verbose)
  
  if (is.null(result)) {
    # Restore backup on failure
    if (verbose) cat("\n✗ Retraining failed! Restoring backup...\n")
    tryCatch({
      file.copy(backup_model_path, model_path, overwrite = TRUE)
      file.copy(backup_metadata_path, metadata_path, overwrite = TRUE)
      if (verbose) cat("  ✓ Backup restored successfully\n")
    }, error = function(e) {
      if (verbose) cat(sprintf("  ✗ Backup restoration failed: %s\n", conditionMessage(e)))
    })
    
    return(list(
      success = FALSE,
      new_samples = new_samples,
      message = "Retraining failed, backup restored"
    ))
  }
  
  # Step 7: Compare metrics
  if (verbose) {
    cat("\n📈 Retraining Results:\n")
    cat(sprintf("  Samples: %d → %d (+%d)\n", old_sample_count, new_sample_count, new_samples))
    if (!is.null(metadata$metrics) && !is.null(result$metrics)) {
      old_acc <- metadata$metrics$validation_accuracy * 100
      new_acc <- result$metrics$validation_accuracy * 100
      diff_acc <- new_acc - old_acc
      cat(sprintf("  Validation accuracy: %.1f%% → %.1f%% (%+.1f%%)\n", 
                  old_acc, new_acc, diff_acc))
    }
    cat("\n✓ Model retrained successfully!\n")
  }
  
  return(list(
    success = TRUE,
    new_samples = new_samples,
    total_samples = new_sample_count,
    old_metrics = metadata$metrics,
    new_metrics = result$metrics,
    message = sprintf("Model retrained with %d new samples", new_samples)
  ))
}

#' Check if model retraining is recommended
#' 
#' Analyzes current model status and determines if retraining would be beneficial.
#' Considers: new samples available, model age, and performance metrics.
#' 
#' @param min_new_samples Minimum new samples to recommend retraining (default 20)
#' @param max_age_days Maximum model age in days before recommending retrain (default 30)
#' @param verbose Logical. Print status info. Default FALSE.
#' 
#' @return List with recommendation:
#'   - recommended: Logical, TRUE if retraining is recommended
#'   - reason: Character, reason for recommendation
#'   - new_samples: Number of new samples available
#'   - model_age_days: Age of current model in days
#'   - urgency: Character, "high", "medium", "low", or "none"
#' 
#' @export
check_retrain_status <- function(min_new_samples = 20, max_age_days = 30, verbose = FALSE) {
  
  if (!ML_AVAILABLE) {
    return(list(
      recommended = FALSE,
      reason = "ML packages not available",
      urgency = "none"
    ))
  }
  
  metadata_path <- "config/ml_models/autofill_metadata.rds"
  
  # No model exists - recommend initial training
  if (!file.exists(metadata_path)) {
    return(list(
      recommended = TRUE,
      reason = "No model exists - initial training needed",
      new_samples = NA,
      model_age_days = NA,
      urgency = "high"
    ))
  }
  
  # Load metadata
  metadata <- tryCatch(readRDS(metadata_path), error = function(e) NULL)
  if (is.null(metadata)) {
    return(list(
      recommended = TRUE,
      reason = "Model metadata corrupted - retraining needed",
      urgency = "high"
    ))
  }
  
  # Prepare current training data
  training_data <- prepare_training_data()
  if (is.null(training_data)) {
    return(list(
      recommended = FALSE,
      reason = "Cannot prepare training data",
      urgency = "none"
    ))
  }
  
  # Calculate metrics
  old_sample_count <- if (!is.null(metadata$n_samples)) metadata$n_samples else 0
  new_sample_count <- nrow(training_data)
  new_samples <- new_sample_count - old_sample_count
  
  model_age <- as.numeric(difftime(Sys.time(), metadata$training_date, units = "days"))
  if (is.na(model_age)) model_age <- 999  # Treat missing date as very old
  
  # Determine recommendation
  reasons <- c()
  urgency_score <- 0
  
  if (!is.na(new_samples) && new_samples >= min_new_samples * 2) {
    reasons <- c(reasons, sprintf("%d new samples available (threshold: %d)", new_samples, min_new_samples))
    urgency_score <- urgency_score + 3
  } else if (!is.na(new_samples) && new_samples >= min_new_samples) {
    reasons <- c(reasons, sprintf("%d new samples available", new_samples))
    urgency_score <- urgency_score + 2
  }
  
  if (!is.na(model_age) && model_age > max_age_days * 2) {
    reasons <- c(reasons, sprintf("Model is %.0f days old (max: %d)", model_age, max_age_days))
    urgency_score <- urgency_score + 3
  } else if (!is.na(model_age) && model_age > max_age_days) {
    reasons <- c(reasons, sprintf("Model is %.0f days old", model_age))
    urgency_score <- urgency_score + 1
  }
  
  # Determine urgency
  urgency <- if (urgency_score >= 4) "high" 
             else if (urgency_score >= 2) "medium"
             else if (urgency_score >= 1) "low"
             else "none"
  
  recommended <- length(reasons) > 0
  reason_text <- if (recommended) paste(reasons, collapse = " AND ") else "Model is up to date"
  
  if (verbose) {
    cat("\n=== Model Retrain Status ===\n")
    cat(sprintf("Recommendation: %s\n", if (recommended) "YES" else "NO"))
    cat(sprintf("Reason: %s\n", reason_text))
    cat(sprintf("Urgency: %s\n", urgency))
    cat(sprintf("New samples: %d\n", new_samples))
    cat(sprintf("Model age: %.1f days\n", model_age))
    cat("\n")
  }
  
  return(list(
    recommended = recommended,
    reason = reason_text,
    new_samples = new_samples,
    model_age_days = model_age,
    urgency = urgency
  ))
}

#' Log ML prediction for performance monitoring (Step 10)
#' 
#' Records ML predictions to CSV files for analysis and model performance tracking.
#' Creates monthly log files in logs/ml_predictions/ directory. Tracks what was
#' predicted, confidence scores, and whether predictions were accepted by users.
#' 
#' @param element_id Character. Element ID from Castor
#' @param castor_value Character. Input Castor value that was used for prediction
#' @param prediction Character. Predicted EPIC value
#' @param confidence Numeric. Confidence score (0-100)
#' @param accepted Logical or NULL. TRUE if user accepted, FALSE if rejected, NULL if unknown
#' @param strategy Character. Strategy used (default "ML")
#' @param log_dir Character. Optional custom log directory
#' 
#' @return Invisible TRUE if logged successfully, FALSE otherwise
#' 
#' @export
log_ml_prediction <- function(element_id = NA, castor_value, prediction, confidence, 
                              accepted = NULL, strategy = "ML", log_dir = NULL) {
  
  if (!ML_AVAILABLE) {
    return(invisible(FALSE))
  }
  
  tryCatch({
    # Determine log directory
    if (is.null(log_dir)) {
      log_dir <- "logs/ml_predictions"
    }
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Create monthly log file
    month_suffix <- format(Sys.time(), "%Y%m")
    log_file <- file.path(log_dir, sprintf("predictions_%s.csv", month_suffix))
    
    # Create log entry
    log_entry <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      element_id = as.character(element_id),
      castor_value = as.character(castor_value),
      prediction = as.character(prediction),
      confidence = as.numeric(confidence),
      accepted = if (is.null(accepted)) NA else as.logical(accepted),
      strategy = as.character(strategy),
      stringsAsFactors = FALSE
    )
    
    # Write to CSV (append if exists, create with header if new)
    if (file.exists(log_file)) {
      write.table(log_entry, log_file, append = TRUE, sep = ",", 
                  row.names = FALSE, col.names = FALSE, quote = TRUE)
    } else {
      write.table(log_entry, log_file, append = FALSE, sep = ",", 
                  row.names = FALSE, col.names = TRUE, quote = TRUE)
    }
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    # Silently fail - don't disrupt autofill process
    return(invisible(FALSE))
  })
}

#' Get ML prediction statistics from logs
#' 
#' Analyzes logged ML predictions to provide performance metrics and insights.
#' 
#' @param months_back Integer. How many months of logs to analyze (default 3)
#' @param log_dir Character. Log directory path
#' 
#' @return List with statistics:
#'   - total_predictions: Total number of predictions logged
#'   - acceptance_rate: Percentage of predictions accepted by users
#'   - avg_confidence: Average confidence score
#'   - by_confidence_bucket: Performance by confidence ranges
#'   - monthly_trend: Predictions per month
#' 
#' @export
get_ml_stats <- function(months_back = 3, log_dir = "logs/ml_predictions") {
  
  if (!ML_AVAILABLE) {
    return(list(
      total_predictions = 0,
      message = "ML not available"
    ))
  }
  
  tryCatch({
    # Find all log files in date range
    current_date <- Sys.Date()
    target_months <- format(seq(current_date, by = "-1 month", length.out = months_back), "%Y%m")
    
    log_files <- list.files(log_dir, pattern = "predictions_.*\\.csv", full.names = TRUE)
    log_files <- log_files[grepl(paste(target_months, collapse = "|"), log_files)]
    
    if (length(log_files) == 0) {
      return(list(
        total_predictions = 0,
        message = "No prediction logs found"
      ))
    }
    
    # Read and combine all logs
    all_logs <- lapply(log_files, function(f) {
      tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    })
    all_logs <- do.call(rbind, all_logs[!sapply(all_logs, is.null)])
    
    if (is.null(all_logs) || nrow(all_logs) == 0) {
      return(list(
        total_predictions = 0,
        message = "No predictions in logs"
      ))
    }
    
    # Calculate statistics
    total <- nrow(all_logs)
    accepted_data <- all_logs[!is.na(all_logs$accepted), ]
    acceptance_rate <- if (nrow(accepted_data) > 0) {
      mean(accepted_data$accepted) * 100
    } else NA
    
    avg_confidence <- mean(all_logs$confidence, na.rm = TRUE)
    
    # Performance by confidence bucket
    all_logs$conf_bucket <- cut(all_logs$confidence, 
                                 breaks = c(0, 50, 70, 90, 100),
                                 labels = c("0-50%", "50-70%", "70-90%", "90-100%"),
                                 include.lowest = TRUE)
    
    bucket_stats <- if (nrow(accepted_data) > 0) {
      accepted_data$conf_bucket <- cut(accepted_data$confidence,
                                       breaks = c(0, 50, 70, 90, 100),
                                       labels = c("0-50%", "50-70%", "70-90%", "90-100%"),
                                       include.lowest = TRUE)
      aggregate(accepted ~ conf_bucket, data = accepted_data, 
                FUN = function(x) c(count = length(x), rate = mean(x) * 100))
    } else NULL
    
    # Monthly trend
    all_logs$month <- substr(all_logs$timestamp, 1, 7)  # YYYY-MM
    monthly <- table(all_logs$month)
    
    return(list(
      total_predictions = total,
      predictions_with_feedback = nrow(accepted_data),
      acceptance_rate = acceptance_rate,
      avg_confidence = avg_confidence,
      confidence_buckets = as.data.frame(table(all_logs$conf_bucket)),
      monthly_trend = as.data.frame(monthly),
      date_range = range(all_logs$timestamp)
    ))
    
  }, error = function(e) {
    return(list(
      total_predictions = 0,
      error = conditionMessage(e)
    ))
  })
}

cat("✓ ML autofill module loaded (Steps 2-11 COMPLETE)\n")
cat("  Available functions:\n")
cat("    - prepare_training_data()    ✓ Ready\n")
cat("    - prep_text()                ✓ Ready\n")
cat("    - create_features()          ✓ Ready\n")
cat("    - transform_new_data()       ✓ Ready\n")
cat("    - train_autofill_model()     ✓ Ready\n")
cat("    - save_model()               ✓ Ready\n")
cat("    - load_model()               ✓ Ready\n")
cat("    - predict_epic_value_ml()    ✓ Ready\n")
cat("    - retrain_model()            ✓ Ready\n")
cat("    - check_retrain_status()     ✓ Ready\n")
cat("    - log_ml_prediction()        ✓ Ready\n")
cat("    - get_ml_stats()             ✓ Ready\n")
cat("    - validate_model_files()     ✓ Ready [NEW]\n")
cat("    - validate_prediction_input()✓ Ready [NEW]\n")
cat("    - safe_file_operation()      ✓ Ready [NEW]\n")
cat("    - recover_model_from_backup()✓ Ready [NEW]\n")
cat("\n")

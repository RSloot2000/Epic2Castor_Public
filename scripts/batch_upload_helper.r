## Batch upload helper functie voor Castor API
## Gebruik: source("scripts/batch_upload_helper.r")

# NULL-coalescing operator: retourneer linkerzijde tenzij NULL, dan rechterzijde
`%||%` <- function(x, y) if (is.null(x)) y else x

cat("Batch upload helper functies geladen\n")

# Batch upload datapoints naar Castor repeating data instance
# 
# @param access_token OAuth access token
# @param api_base_url Base URL voor Castor API (https://data.castoredc.com/api)
# @param study_id Study UUID
# @param participant_id Participant ID
# @param instance_id Repeating data instance UUID
# @param fields_list Named list met field_id -> field_value mappings
# @param rep_name Instance naam (voor logging)
# @param existing_map Optioneel: bestaande waarden om duplicaten te filteren
# @param skip_unchanged Boolean: sla ongewijzigde velden over (default TRUE)
# 
# @return List met:
#   - success: Boolean
#   - uploaded: Aantal succesvol geüploade velden
#   - failed: Aantal gefaalde velden
#   - errors: List met error details
#   - skipped: Aantal overgeslagen (ongewijzigde) velden
batch_upload_datapoints <- function(access_token, api_base_url, study_id, 
                                   participant_id, instance_id, 
                                   fields_list, rep_name = "",
                                   existing_map = NULL,
                                   skip_unchanged = TRUE) {
  
  # Validatie
  if (length(fields_list) == 0) {
    return(list(
      success = TRUE, 
      uploaded = 0, 
      failed = 0,
      skipped = 0,
      errors = list()
    ))
  }
  
  # Filter ongewijzigde velden indien gewenst
  skipped_count <- 0
  if (skip_unchanged && !is.null(existing_map) && length(existing_map) > 0) {
    original_count <- length(fields_list)
    
    # Helper: normaliseer waarden voor vergelijking (hergebruik uit baselineExport.r)
    normalize_for_compare <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      if (is.list(x)) x <- unlist(x, use.names = FALSE)
      x <- as.character(x)
      x <- x[!is.na(x)]
      if (length(x) == 0) return(NA_character_)
      x <- gsub(";", ",", x, fixed = TRUE)
      trimws(paste(x, collapse = ","))
    }
    
    fields_to_upload <- list()
    for (fid in names(fields_list)) {
      existing_val_norm <- existing_map[[fid]]
      new_val_norm <- normalize_for_compare(fields_list[[fid]])
      
      # Skip als waarden gelijk zijn
      if (!is.null(existing_val_norm) && !is.na(existing_val_norm) &&
          !is.na(new_val_norm) && 
          tolower(trimws(existing_val_norm)) == tolower(trimws(new_val_norm))) {
        skipped_count <- skipped_count + 1
        # Log overgeslagen veld (toon eerste 50 chars van waarde)
        field_val_preview <- substr(as.character(fields_list[[fid]]), 1, 50)
        if (nchar(as.character(fields_list[[fid]])) > 50) field_val_preview <- paste0(field_val_preview, "...")
        cat("  ⊘ Skipping unchanged field", fid, "- value:", field_val_preview, "\n")
        next
      }
      
      # Dit veld WORDT geüpload - log het
      field_val_preview <- substr(as.character(fields_list[[fid]]), 1, 50)
      if (nchar(as.character(fields_list[[fid]])) > 50) field_val_preview <- paste0(field_val_preview, "...")
      cat("  ↑ Uploading field", fid, "- value:", field_val_preview, "\n")
      
      fields_to_upload[[fid]] <- fields_list[[fid]]
    }
    
    fields_list <- fields_to_upload
    
    if (skipped_count > 0) {
      cat("Skipped", skipped_count, "unchanged field(s) for", participant_id, rep_name, "\n")
    }
  }
  
  # Als na filtering niets over is
  if (length(fields_list) == 0) {
    return(list(
      success = TRUE,
      uploaded = 0,
      failed = 0,
      skipped = skipped_count,
      errors = list()
    ))
  }
  
  # Bouw Castor API payload met "data" wrapper
  # Elke item MOET instance_id bevatten (zelfs als het in URL staat)
  datapoints_array <- lapply(names(fields_list), function(fid) {
    list(
      field_id = fid,
      instance_id = instance_id,
      field_value = fields_list[[fid]]
    )
  })
  
  batch_payload <- list(data = datapoints_array)
  
  # POST naar batch endpoint
  batch_url <- paste0(
    api_base_url, "/study/", study_id,
    "/participant/", participant_id,
    "/data-points/repeating-data-instance/", instance_id
  )
  
  cat("Batch uploading", length(datapoints_array), "field(s) for", 
      participant_id, rep_name, "\n")
  
  # HTTP request met retry logica
  response <- tryCatch({
    if (exists("REQ_POST")) {
      # Gebruik bestaande helper met retry
      REQ_POST(
        batch_url,
        add_headers(
          Authorization = paste("Bearer", access_token),
          `Content-Type` = "application/json",
          Accept = "application/hal+json"
        ),
        body = toJSON(batch_payload, auto_unbox = TRUE),
        encode = "json",
        times = 2,
        timeout_s = 30
      )
    } else {
      # Fallback zonder retry
      POST(
        batch_url,
        add_headers(
          Authorization = paste("Bearer", access_token),
          `Content-Type` = "application/json",
          Accept = "application/hal+json"
        ),
        body = toJSON(batch_payload, auto_unbox = TRUE),
        encode = "json"
      )
    }
  }, error = function(e) {
    return(list(error = TRUE, message = conditionMessage(e)))
  })
  
  # Error tijdens HTTP request
  if (is.list(response) && !is.null(response$error)) {
    cat("✗ HTTP error:", response$message, "\n")
    return(list(
      success = FALSE, 
      uploaded = 0,
      failed = length(fields_list),
      skipped = skipped_count,
      errors = list(list(
        message = response$message, 
        fields = names(fields_list)
      ))
    ))
  }
  
  # Parse response
  status <- status_code(response)
  
  if (status %in% c(200, 201, 207)) {
    result_content <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
    
    # Castor API geeft altijd deze structuur terug
    total_success <- result_content$total_success %||% 0
    total_failed <- result_content$total_failed %||% 0
    failed_items <- result_content$failed %||% list()
    
    if (total_failed == 0) {
      cat("✓ Batch upload successful:", total_success, "field(s) uploaded for", 
          participant_id, rep_name, "\n")
    } else {
      cat("⚠ Partial success:", total_success, "uploaded,", total_failed, 
          "failed for", participant_id, rep_name, "\n")
    }
    
    # Parse error details
    errors_list <- list()
    if (length(failed_items) > 0) {
      for (failed_item in failed_items) {
        error_msg <- failed_item$message %||% failed_item$error %||% "Unknown error"
        field_id <- failed_item$field_id %||% "unknown"
        cat("  ✗ Field", field_id, "failed:", error_msg, "\n")
        errors_list <- c(errors_list, list(list(
          field_id = field_id,
          message = error_msg
        )))
      }
    }
    
    return(list(
      success = TRUE,
      uploaded = total_success,
      failed = total_failed,
      skipped = skipped_count,
      errors = errors_list,
      partial = (total_failed > 0)
    ))
    
  } else {
    # HTTP error status
    error_text <- content(response, as = "text", encoding = "UTF-8")
    cat("✗ Batch upload failed with status", status, "for", participant_id, rep_name, "\n")
    cat("Response:", error_text, "\n")
    
    return(list(
      success = FALSE,
      uploaded = 0,
      failed = length(fields_list),
      skipped = skipped_count,
      errors = list(list(
        message = error_text,
        status = status,
        fields = names(fields_list)
      ))
    ))
  }
}


# Fallback: individuele upload functie
# Gebruikt de oude methode voor velden die falen in batch upload
individual_upload_datapoint <- function(access_token, base_url, api_base_url,
                                       study_id, participant_id, instance_id,
                                       field_id, field_value, clean_field_var = "",
                                       rep_name = "") {
  
  update_url <- paste0(
    base_url, "/api/study/", study_id,
    "/participant/", participant_id,
    "/data-point/repeating-data/", instance_id, "/", field_id
  )
  
  update_response <- POST(
    update_url,
    add_headers(
      Accept = "application/hal+json",
      Authorization = paste("Bearer", access_token),
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(field_value = field_value), auto_unbox = TRUE),
    encode = "json"
  )
  
  if (status_code(update_response) %in% c(200, 201)) {
    cat("✓ Individual upload successful:", clean_field_var, "for", participant_id, rep_name, "\n")
    return(TRUE)
  } else {
    cat("✗ Individual upload failed:", clean_field_var, "for", participant_id, rep_name, "\n")
    cat("Response:", content(update_response, as = "text", encoding = "UTF-8"), "\n")
    return(FALSE)
  }
}


# Batch upload met automatische fallback
# Probeert eerst batch upload, bij failures retried individueel
batch_upload_with_fallback <- function(access_token, base_url, api_base_url,
                                      study_id, participant_id, instance_id,
                                      fields_list, fields_metadata,
                                      rep_name = "", existing_map = NULL) {
  
  # Probeer batch upload
  result <- batch_upload_datapoints(
    access_token = access_token,
    api_base_url = api_base_url,
    study_id = study_id,
    participant_id = participant_id,
    instance_id = instance_id,
    fields_list = fields_list,
    rep_name = rep_name,
    existing_map = existing_map,
    skip_unchanged = TRUE
  )
  
  total_uploaded <- result$uploaded
  total_failed <- result$failed
  
  # Bij partiële of volledige failure: probeer individuele uploads
  if (result$failed > 0 && !is.null(result$errors)) {
    cat("Retrying failed fields individually...\n")
    
    for (error_item in result$errors) {
      failed_field_id <- error_item$field_id
      
      if (!is.null(failed_field_id) && failed_field_id %in% names(fields_list)) {
        clean_var <- fields_metadata[[failed_field_id]] %||% failed_field_id
        
        success <- individual_upload_datapoint(
          access_token = access_token,
          base_url = base_url,
          api_base_url = api_base_url,
          study_id = study_id,
          participant_id = participant_id,
          instance_id = instance_id,
          field_id = failed_field_id,
          field_value = fields_list[[failed_field_id]],
          clean_field_var = clean_var,
          rep_name = rep_name
        )
        
        if (success) {
          total_uploaded <- total_uploaded + 1
          total_failed <- total_failed - 1
        }
      }
    }
  }
  
  return(list(
    uploaded = total_uploaded,
    failed = total_failed,
    skipped = result$skipped
  ))
}

cat("Batch upload helper functies geladen\n")

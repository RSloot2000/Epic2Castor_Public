# ============================================================================
# FASE 11.7: Export Functionaliteit voor Approved Matches
# ============================================================================

#' Export approved autofill matches to CSV
#' @param approved_data Data.table with approved matches
#' @param table_name Original table name (waarde_checkboxes or waarde_radiobuttons)
#' @param export_path Optional custom export path
#' @return Path to exported file
export_approved_matches <- function(approved_data, table_name, export_path = NULL) {
  if (is.null(export_path)) {
    # Default export path met timestamp
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    export_dir <- file.path("output_data", "autofill_approved")
    
    if (!dir.exists(export_dir)) {
      dir.create(export_dir, recursive = TRUE)
    }
    
    export_path <- file.path(export_dir, sprintf("%s_approved_%s.csv", table_name, timestamp))
  }
  
  # Bereid export data voor
  export_data <- approved_data[, .(
    Element,
    Castor_Value = castor_value,
    EPIC_Value_Old = epic_value_old,
    EPIC_Value_New = epic_value_new,
    Strategy = strategy,
    Source = source,
    Confidence = paste0(confidence, "%"),
    Tab = tab_name_meta,
    Tab_Order = tab_order_meta,
    Export_Timestamp = Sys.time()
  )]
  
  # Write to CSV
  fwrite(export_data, export_path, sep = ";", na = "")
  
  return(export_path)
}

#' Apply approved matches to original mapping table
#' @param original_data Original mapping data
#' @param approved_matches Approved autofill suggestions
#' @param castor_col Name of Castor value column
#' @return Updated data.table
apply_approved_matches <- function(original_data, approved_matches, castor_col = "kolom_toevoeging") {
  # Maak kopie om origineel te behouden
  updated_data <- copy(original_data)
  
  # Match counter
  matches_applied <- 0
  
  for (i in 1:nrow(approved_matches)) {
    match_row <- approved_matches[i]
    
    # Vind matching rijen in original data
    # Match op: Element + Castor value + empty EPIC value
    target_rows <- which(
      updated_data$Element == match_row$Element &
      updated_data[[castor_col]] == match_row$castor_value &
      (is.na(updated_data$waarde) | updated_data$waarde == "")
    )
    
    if (length(target_rows) > 0) {
      # Update eerste matching row (of alle als gewenst)
      updated_data[target_rows[1], waarde := match_row$epic_value_new]
      matches_applied <- matches_applied + 1
    }
  }
  
  message(sprintf("Applied %d approved matches to original data", matches_applied))
  
  return(updated_data)
}

#' Generate autofill summary report
#' @param results Autofill results data.table
#' @return List with summary statistics
generate_autofill_summary <- function(results) {
  total_suggestions <- nrow(results)
  
  # Strategy breakdown
  strategy_counts <- results[, .N, by = strategy][order(-N)]
  
  # Confidence breakdown
  confidence_ranges <- list(
    `100%` = nrow(results[confidence == 100]),
    `90-99%` = nrow(results[confidence >= 90 & confidence < 100]),
    `80-89%` = nrow(results[confidence >= 80 & confidence < 90]),
    `70-79%` = nrow(results[confidence >= 70 & confidence < 80]),
    `<70%` = nrow(results[confidence < 70])
  )
  
  # Element breakdown (top 10)
  element_counts <- results[, .N, by = Element][order(-N)][1:min(10, .N)]
  
  summary <- list(
    total_suggestions = total_suggestions,
    strategy_breakdown = strategy_counts,
    confidence_ranges = confidence_ranges,
    top_elements = element_counts,
    avg_confidence = round(mean(results$confidence, na.rm = TRUE), 1),
    high_confidence_count = nrow(results[confidence >= 90]),
    high_confidence_pct = round(nrow(results[confidence >= 90]) / total_suggestions * 100, 1)
  )
  
  return(summary)
}

#' Export autofill summary to text file
#' @param summary Summary list from generate_autofill_summary
#' @param output_path Path to output file
export_summary_report <- function(summary, output_path = NULL) {
  if (is.null(output_path)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    export_dir <- file.path("output_data", "autofill_reports")
    
    if (!dir.exists(export_dir)) {
      dir.create(export_dir, recursive = TRUE)
    }
    
    output_path <- file.path(export_dir, sprintf("autofill_summary_%s.txt", timestamp))
  }
  
  # Generate report text
  report <- c(
    "========================================================================",
    "AUTOFILL SUMMARY REPORT",
    sprintf("Generated: %s", Sys.time()),
    "========================================================================",
    "",
    sprintf("Total Suggestions: %d", summary$total_suggestions),
    sprintf("Average Confidence: %.1f%%", summary$avg_confidence),
    sprintf("High Confidence (≥90%%): %d (%.1f%%)", 
            summary$high_confidence_count, summary$high_confidence_pct),
    "",
    "CONFIDENCE DISTRIBUTION:",
    sprintf("  100%%:    %d", summary$confidence_ranges$`100%`),
    sprintf("  90-99%%:  %d", summary$confidence_ranges$`90-99%`),
    sprintf("  80-89%%:  %d", summary$confidence_ranges$`80-89%`),
    sprintf("  70-79%%:  %d", summary$confidence_ranges$`70-79%`),
    sprintf("  <70%%:    %d", summary$confidence_ranges$`<70%`),
    "",
    "STRATEGY BREAKDOWN:"
  )
  
  for (i in 1:nrow(summary$strategy_breakdown)) {
    row <- summary$strategy_breakdown[i]
    pct <- round(row$N / summary$total_suggestions * 100, 1)
    report <- c(report, sprintf("  %s: %d (%.1f%%)", row$strategy, row$N, pct))
  }
  
  report <- c(
    report,
    "",
    "TOP 10 ELEMENTS:",
    sapply(1:nrow(summary$top_elements), function(i) {
      row <- summary$top_elements[i]
      sprintf("  %d. %s: %d suggestions", i, row$Element, row$N)
    })
  )
  
  # Write to file
  writeLines(report, output_path)
  
  return(output_path)
}

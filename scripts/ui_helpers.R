# ============================================================================
# UI HELPER FUNCTIONS
# ============================================================================
# Contains helper functions for generating dynamic UI components
# such as keyboard shortcuts sections loaded from JSON configuration.

#' Generate Keyboard Shortcuts HTML Section
#'
#' Reads the keyboard shortcuts configuration and builds an HTML section
#' with categorized shortcut tables. Used in the User Guide modal.
#'
#' @param config List parsed from config/keyboard_shortcuts.json
#' @return Shiny tags object with keyboard shortcuts HTML
generate_keyboard_shortcuts_section <- function(config) {
  if (is.null(config) || is.null(config$shortcuts)) {
    return(tags$p("No shortcuts configured"))
  }
  
  # Category metadata
  category_info <- list(
    clipboard = list(icon = "clipboard", label = "Clipboard Operations", color = "#e67e22"),
    data = list(icon = "save", label = "Data Operations", color = "#27ae60"),
    navigation = list(icon = "compass", label = "Navigation", color = "#3498db"),
    ui = list(icon = "window-maximize", label = "Interface", color = "#9b59b6")
  )
  
  sections <- list()
  
  for (cat_name in names(config$shortcuts)) {
    cat_config <- config$shortcuts[[cat_name]]
    cat_info <- category_info[[cat_name]]
    
    if (is.null(cat_info)) next
    
    # Build table rows for this category
    rows <- list()
    for (action_name in names(cat_config)) {
      action <- cat_config[[action_name]]
      
      # Skip if disabled
      if (!is.null(action$enabled) && !action$enabled) next
      
      # Build shortcut keys display
      keys <- action$keys
      alt_keys <- if (!is.null(action$alt_keys)) {
        paste0(" or ", tags$kbd(action$alt_keys))
      } else {
        ""
      }
      
      # Scope warning
      scope_note <- if (!is.null(action$scope) && action$scope == "elements_table_only") {
        tags$div(
          style = "margin-top: 5px;",
          tags$small(
            style = "color: #e67e22;",
            icon("exclamation-triangle"), " Elements table only"
          )
        )
      } else {
        NULL
      }
      
      rows[[action_name]] <- tags$tr(
        tags$td(
          tags$kbd(keys),
          if (nchar(alt_keys) > 0) tags$span(style = "color: #7f8c8d;", alt_keys) else NULL,
          style = "width: 250px; font-weight: 500;"
        ),
        tags$td(
          action$description,
          scope_note
        )
      )
    }
    
    # Create section if we have rows
    if (length(rows) > 0) {
      sections[[cat_name]] <- tags$div(
        style = "margin-bottom: 25px;",
        tags$h4(
          style = sprintf("color: #2c3e50; border-bottom: 2px solid %s; padding-bottom: 8px; margin-bottom: 15px;", cat_info$color),
          icon(cat_info$icon), " ", cat_info$label
        ),
        tags$table(
          class = "table table-hover",
          style = "margin-bottom: 0;",
          tags$tbody(rows)
        )
      )
    }
  }
  
  # Return wrapped sections
  tags$div(
    tags$div(
      style = "background: #e8f5e9; padding: 12px; border-radius: 4px; margin-bottom: 20px; border-left: 4px solid #27ae60;",
      icon("keyboard"), " ", 
      tags$strong("Keyboard Shortcuts:"), 
      " Use these shortcuts for faster workflow. Press ",
      tags$kbd("F1"), " anytime to view this help."
    ),
    sections
  )
}

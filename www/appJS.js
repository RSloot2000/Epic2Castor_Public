// ===== CONSTANTS =====
const INIT_DELAY = 500;         // Initial delay for table initialization (ms)
const BACKUP_INIT_DELAY = 1500; // Backup delay for table initialization (ms)
const COL_RESIZE_DELAY = 100;   // Delay for column resize operations (ms)
const STICKY_COLUMNS_WIDTH = "55px"; // Width for sticky columns
const LOADING_DOTS_INTERVAL = 500;   // Interval for loading dots animation (ms)

// ===== LOADING SCREEN =====
let loadingDotsInterval = null;

/**
 * Animates the loading text by cycling through dots (0-4 dots)
 * NOTE: Animation is now handled by CSS for better performance
 * CSS animations run on GPU compositor thread and won't freeze when main thread is blocked
 * This function is kept for potential fallback but does nothing by default
 */
function animateLoadingDots() {
    console.log('[Loading] Loading animation handled by CSS');
    // CSS animation handles the dots now - no JavaScript needed
    // This prevents animation from freezing when DataTable blocks the main thread
}

/**
 * Stops the loading dots animation and removes the loading screen
 * Called when Shiny app is fully loaded and ready to display
 * Cleans up animation timers and fades out the loading overlay
 */
function hideLoadingScreen() {
    console.log('[Loading] Hiding loading screen...');
    
    // No need to clear interval anymore - CSS handles animation
    // Keeping this for backward compatibility
    if (loadingDotsInterval) {
        console.log('[Loading] Clearing interval ID:', loadingDotsInterval);
        clearInterval(loadingDotsInterval);
        loadingDotsInterval = null;
    }
    
    // Fade out and remove the loading screen from DOM
    const loadingScreen = document.getElementById('loading-screen');
    if (loadingScreen) {
        loadingScreen.classList.add('fade-out'); // CSS handles smooth transition
        setTimeout(function() {
            loadingScreen.remove(); // Remove from DOM after fade completes
            console.log('[Loading] Loading screen removed from DOM');
        }, 500); // Match the CSS transition duration
    } else {
        console.log('[Loading] Loading screen element not found');
    }
}

// ===== LOADING SCREEN =====

// Start loading animation when DOM is ready
$(document).ready(function() {
    animateLoadingDots();
    
    // Register Shiny custom message handler for hiding the loading screen
    // Server calls this when app is fully initialized
    if (typeof Shiny !== 'undefined') {
        Shiny.addCustomMessageHandler('hideLoadingScreen', function(message) {
            hideLoadingScreen();
        });
    }
});

// ===== CORE FUNCTIONS =====

/**
 * Initializes column resize functionality for a table
 * Configures colResizable plugin and sets up sticky columns
 * @param {jQuery} $table - The jQuery table object
 */
function initColResizable($table) {
    if (!$.fn.colResizable || !$table || $table.length === 0) return;
    
    try {
        // Clean up existing colResizable if present
        if ($table.hasClass('JColResizer')) {
            $table.colResizable({ disable: true });
        }
        
        // Delete existing grips
        $("div.JCLRgrips").remove();
        
        const colCount = $table.find("th").length;
        
        // Make sure table head is correctly positioned
        $table.find("thead").css({
            "position": "relative", // Important for correct positioning of grips
            "z-index": "20" // Higher than the sticky columns
        });
        
        // Make sure the table has a fixed layout for correct calculations
        $table.css("table-layout", "fixed");
        
        // Initialize colResizable with the correct options
        $table.colResizable({
            liveDrag: true,
            gripInnerHtml: "",
            disabledColumns: [0, colCount - 1], // First and last column not resizable
            headerOnly: true, // IMPORTANT: only place grips in header
            fixed: false,
            partialRefresh: true
        });
        
        // Make sure the JCLRgrips container is correctly positioned
        $(".JCLRgrips").css({
            "position": "absolute",
            "top": "0",
            "left": "0", 
            "z-index": "15" // Between normal cells and sticky columns
        });
        
        // Force correct positioning of sticky columns after resizing
        setTimeout(() => {
            // Make sure sticky columns continue to work correctly
            $table.find("th:first-child, td:first-child, th:last-child, td:last-child").css({
                "position": "sticky",
                "z-index": "25" // Higher than the grip elements
            });
            
            // First column left sticky
            $table.find("th:first-child, td:first-child").css({
                "left": "0"
            });
            
            // Last column right sticky
            $table.find("th:last-child, td:last-child").css({
                "right": "0"
            });
        }, 0);
    } catch(e) {
        console.error("Error initializing colResizable:", e);
    }
}

/**
 * Initializes Select2 for all dropdown elements in the table
 * Configures styling, positioning and event handlers for dropdowns
 * Handles dropdown positioning to prevent overflow outside viewport
 * Re-applies CSS classes based on data attributes (e.g., red-cell, blue-cell)
 */
function initializeSelect2Elements() {
    const selects = $('#table table select.lazy-load');
    
    selects.each(function() {
        const $sel = $(this);
        
        // Remove existing select2 if present (cleanup before re-init)
        if ($sel.hasClass("select2-hidden-accessible")) {
            $sel.select2("destroy");
        }
        
        // Initialize select2 with full width and body as dropdown parent
        $sel.select2({
            width: '100%',
            dropdownParent: $('body') // Render dropdown in body to avoid overflow issues
        });
        
        // Position dropdown menu correctly using fixed positioning
        $sel.off("select2:open").on("select2:open", function() {
            window.requestAnimationFrame(function() {
                const $container = $sel.next('.select2-container');
                if ($container.length === 0) return;
                
                // Get position of the select2 container
                const rect = $container[0].getBoundingClientRect();
                const $dd = $('.select2-container--open .select2-dropdown');

                if ($dd.length === 0) return;

                // Apply fixed positioning based on container location
                $dd.css({
                    position: 'fixed',
                    left:   rect.left   + 'px',
                    minWidth: rect.width + 'px'
                });

                // Check if dropdown fits below the select box
                const dropdownHeight = $dd.outerHeight();
                const viewportHeight = window.innerHeight || document.documentElement.clientHeight;
                const fitsBelow = rect.bottom + dropdownHeight <= viewportHeight;

                if (fitsBelow) {
                    // Position dropdown below the select box
                    $dd.removeClass('select2-dropdown--above').addClass('select2-dropdown--below');
                    $sel.next('.select2-container').removeClass('select2-container--above').addClass('select2-container--below');
                    $dd.css('top', rect.bottom + 'px');
                } else {
                    // Position dropdown above the select box if not enough space below
                    const topPos = Math.max(rect.top - dropdownHeight, 0);
                    $dd.removeClass('select2-dropdown--below').addClass('select2-dropdown--above');
                    $sel.next('.select2-container').removeClass('select2-container--below').addClass('select2-container--above');
                    $dd.css('top', topPos + 'px');
                }
            });
        });
        
        // Apply CSS class if needed (for color coding cells)
        const cssClass = $sel.data('cssclass');
        if (cssClass && cssClass.trim() !== "") {
            $sel.next('.select2-container')
                .find('.select2-selection')
                .removeClass("red-cell blue-cell") // Remove old classes first
                .addClass(cssClass); // Apply new class
        }
    });
    
    // Initialize colResizable after Select2 to ensure correct column widths
    const $table = $("#table table");
    if ($table.length) {
        setTimeout(() => {
            initColResizable($table);
            // Note: notifyTableReady is now called via DataTable draw event listener
        }, COL_RESIZE_DELAY);
    }
}

/**
 * Notifies the app that the table is fully initialized and ready for interaction
 * This triggers hiding the loading screen on the server side
 * Sets a Shiny input value that the server can observe
 */
function notifyTableReady() {
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
        // Verify that the table is actually rendered and visible
        const $table = $("#table table");
        const $tableContainer = $("#table");
        
        // Only notify if table exists, is visible, and has content
        if ($table.length > 0 && $tableContainer.is(":visible")) {
            // Check if table has been initialized by DataTables
            const hasDataTable = $.fn.DataTable && $.fn.DataTable.isDataTable($table);
            const hasRows = $table.find('tbody tr').length > 0;
            
            if (hasDataTable && hasRows) {
                console.log('[Loading Screen] Table is ready and has content, notifying server...');
                Shiny.setInputValue('table_ready', true, {priority: 'event'});
            } else if (hasDataTable) {
                console.log('[Loading Screen] Table initialized but no rows yet, waiting...');
                // Table exists but empty - wait a bit longer
                setTimeout(notifyTableReady, 200);
            } else {
                console.log('[Loading Screen] Table not initialized as DataTable yet, waiting...');
                // Not a DataTable yet - wait for initialization
                setTimeout(notifyTableReady, 200);
            }
        } else {
            // Table not ready yet, try again after a short delay
            console.log('[Loading Screen] Table not visible yet, retrying...');
            setTimeout(notifyTableReady, 200);
        }
    }
}

/**
 * Updates body padding based on header and footer height
 * Ensures content is properly positioned and not hidden behind fixed elements
 * Called on page load and window resize to maintain correct spacing
 */
function updateBodyPadding() {
    const $fixedFooter = $(".fixed-footer");
    const $fixedHeader = $(".fixed-header");
    
    // Calculate footer height with extra margin
    const footerHeight = $fixedFooter.length ? $fixedFooter.outerHeight() + 10 : 0;
    // Calculate header height with offset adjustment
    const headerHeight = $fixedHeader.length ? $fixedHeader.outerHeight() - 25 : 0;
    
    // Apply padding to body to prevent content overlap with fixed elements
    $("body").css({
        "padding-bottom": footerHeight + "px",
        "padding-top": headerHeight + "px"
    });
}

/**
 * Manages popover display/hiding
 * Ensures only one popover is active at a time to prevent UI clutter
 * Toggles popover visibility and tracks the currently active popover
 * @param {Element} el - The DOM element on which to show/hide the popover
 */
window.customTogglePopover = function(el) {
    // Close currently active popover if exists
    if (window.activePopover) {
        $(window.activePopover).popover("hide");
        // If clicking the same element, just close it and exit
        if (window.activePopover === el) {
            window.activePopover = null;
            return;
        }
    }
    // Show popover on the clicked element
    $(el).popover("show");
    // Track this as the active popover
    window.activePopover = el;
};

// ===== EVENT HANDLERS =====

$(document).ready(function() {
    // Initialize UI elements on page load
    $("[data-toggle='popover']").popover({ trigger: "manual" }); // Manual trigger for custom control
    updateBodyPadding(); // Set initial body padding for fixed header/footer
    
    // Custom message handler: clear all checkboxes in the table
    // Used when resetting selection state
    Shiny.addCustomMessageHandler("clearCheckboxes", function(message) {
        $(".delete-rows:checked").prop("checked", false); // Uncheck all delete checkboxes
        Shiny.setInputValue("table_rows_selected", []); // Clear server-side selection array
    });
    
    // Custom message handler: reset tab click state
    // Used to reset double-click detection when switching contexts
    Shiny.addCustomMessageHandler("resetTabClickState", function(message) {
        if (window.tabClickState) {
            window.tabClickState.lastClickedTabId = null;
            window.tabClickState.tabClickCount = 0;
            if (window.tabClickState.tabClickTimer) {
                clearTimeout(window.tabClickState.tabClickTimer);
                window.tabClickState.tabClickTimer = null;
            }
        }
    });
    
    // Resize handler with throttling for better performance
    // Only updates on animation frame to prevent excessive recalculations
    let resizePending = false;
    $(window).on("resize", function() {
        if (!resizePending) {
            resizePending = true;
            window.requestAnimationFrame(function() {
                updateBodyPadding(); // Recalculate body padding for new window size
                resizePending = false;
            });
        }
    });
    
    // Hide processing indicators after AJAX calls complete
    // DataTables shows processing indicators during data loading
    $(document).ajaxComplete(function() {
        $(".dataTables_processing").hide();
    });
    
    $(document).ajaxStop(function() {
        $(".dataTables_processing").hide();
    });
    
    // Initialize table with delay to ensure DOM is ready
    // First attempt at table initialization
    setTimeout(function() {
        const $table = $("#table table");
        if ($table.length) {
            initColResizable($table);
            // Don't call notifyTableReady here - wait for DataTable draw event
        }
    }, INIT_DELAY);
    
    // Backup initialization if first attempt failed
    // Checks if colResizable was applied, if not, tries again
    setTimeout(function() {
        const $table = $("#table table");
        if ($table.length && !$table.hasClass('JColResizer')) {
            initColResizable($table);
        }
    }, BACKUP_INIT_DELAY);
    
    // Listen for DataTable draw events - most reliable indicator that table is ready
    // This should fire when the table is rendered for the first time
    $(document).on('draw.dt', '#table table', function() {
        console.log('[Loading Screen] DataTable draw event fired');
        // Use a small delay to ensure all rendering is complete (select2, etc.)
        setTimeout(function() {
            notifyTableReady();
        }, 300);
    });
    
    // Fallback: If draw event never fires (edge case), try to notify anyway
    // This should only trigger if something went wrong with DataTable initialization
    setTimeout(function() {
        // Only notify if we haven't already
        if (!$('#loading-screen').hasClass('fade-out')) {
            console.log('[Loading Screen] Fallback timeout reached, attempting to notify...');
            notifyTableReady();
        }
    }, 5000);  // 5 seconds - give plenty of time for normal initialization
    
    // Trigger window resize event to update UI measurements
    $(window).trigger("resize");
});

// Initialize Select2 on both document ready and window load
// Second attempt ensures Select2 is initialized even if first attempt fails
$(window).on("load", function() {
    setTimeout(function() {
        $(document).trigger("initializeSelect2");
    }, INIT_DELAY);
});

// Select2 initialization handler - responds to custom trigger event
$(document).on("initializeSelect2", initializeSelect2Elements);

/**
 * Helper function to update table_rows_selected based on checked checkboxes
 * Collects all checked checkbox IDs and sends them to Shiny server
 * Extracts row numbers from checkbox IDs (format: deleterows_N)
 */
function updateTableRowsSelected() {
    const selected = [];
    $(".delete-rows:checked").each(function() {
        // Extract row number from checkbox ID (remove "deleterows_" prefix)
        selected.push($(this).attr("id").replace("deleterows_", ""));
    });
    // Update Shiny input with selected row numbers
    Shiny.setInputValue("table_rows_selected", selected);
}

// Checkbox change handler - updates selection when checkboxes are toggled
// Triggered on any checkbox state change in the table
$(document).on("change", ".delete-rows", function() {
    updateTableRowsSelected();
});

// Delete rows handler - collects checked rows and sends to Shiny
// Triggered when delete button is clicked
$(document).on("click", "#delete_rows", function() {
    updateTableRowsSelected();
});

// Double-click handler for select2 elements
// Triggers custom event when user double-clicks a dropdown selection
// Sends row, column, and selected value information to Shiny
$(document).on("dblclick", ".select2-selection", function() {
    const $select = $(this).closest(".select2-container").prev("select.lazy-load");
    if ($select.length) {
        const row = $select.data("row");    // Row number
        const col = $select.data("col");    // Column number
        const value = $select.val();        // Currently selected value
        const id = $select.attr("id");      // Dropdown ID
        // Send all info to Shiny for processing (e.g., open edit modal)
        Shiny.setInputValue("dropdown_dblclick", {id: id, row: row, col: col, selected: value});
    }
});

// Modal keyboard handling - Enter for primary action, Esc for cancel
// Provides keyboard shortcuts for modal dialog interactions
$(document).on("keydown", ".modal", function(e) {
    const key = e.which || e.keyCode;
    if (key === 13) { // Enter key
        const that = this;
        setTimeout(function() {
            // Click the primary (confirm) button
            $(that).find(".btn-primary").first().click();
        }, 100); // Small delay to allow any pending operations to complete
        e.preventDefault();
    } else if (key === 27) { // Escape key
        // Click the cancel button
        $(this).find(".modal-cancel").first().click();
        e.preventDefault();
    }
});

// ===== POPOVER MANAGEMENT =====

// Global variable to track currently active popover
window.activePopover = null;

// Close popover when clicking outside of it
// Improves UX by dismissing popovers on outside clicks
$(document).on("click", function(e) {
    // Check if click target is not a popover trigger or inside a popover
    if (!$(e.target).closest("[data-toggle='popover'], .popover").length) {
        if (window.activePopover) {
            $(window.activePopover).popover("hide");
            window.activePopover = null;
        }
    }
});

// Close popover when modal is hidden
// Prevents popovers from staying visible after modal closes
$(document).on("hide.bs.modal hidden.bs.modal", ".modal", function() {
    if (window.activePopover) {
        $(window.activePopover).popover("hide");
        window.activePopover = null;
    }
});

// Save scroll position to localStorage on scroll
// Allows restoring scroll position after page reload
window.addEventListener("scroll", function() {
    localStorage.setItem("scrollPos", window.scrollY);
});

// ===== SHINY MESSAGE HANDLERS =====

// Register handlers after a short delay to ensure Shiny is fully loaded
// This prevents "Shiny is undefined" errors on initial page load
$(document).ready(function() {
    setTimeout(function() {
        registerShinyHandlers();
    }, 100);
});

/**
 * Registers all custom Shiny message handlers
 * These handlers allow the R server to send messages to the JavaScript client
 * Each handler responds to a specific message type sent from the server
 */
function registerShinyHandlers() {
    // Check if Shiny is available before attempting to register handlers
    if (typeof Shiny === 'undefined' || typeof Shiny.addCustomMessageHandler !== 'function') {
        console.error("Shiny.addCustomMessageHandler not available");
        return;
    }
    
// Resize div and adjust table column widths
// Called when user changes table width via slider or input
try {
    Shiny.addCustomMessageHandler("resizeDiv", function(message) {
        // Adjust div size
        const div = document.querySelector(".col-sm-8");
        if (!div) {
            console.warn("[resizeDiv] .col-sm-8 element not found");
            return;
        }
        div.style.width = message.width;
        div.style.height = message.height;
        
        const widthInput = document.getElementById("width");
        if (widthInput) {
            widthInput.value = parseInt(message.width, 10);
        }

        const $table = $("#table table");
        if ($table.length === 0) return;
        
        // Reset existing colResizable
        if ($.fn.colResizable && $table.hasClass('JColResizer')) {
            try {
                $table.colResizable({ disable: true });
            } catch(e) {
                // Error handling?
            }
        }
        
        $(".JCLRgrips").remove();
        
        // Adjust table width
        $("#scrollDiv").width(message.width);
        $table.width(message.width);
        $table.find("th, td").removeAttr("style");
        
        // Set fixed width for first and last columns
        const stickyWidth = (typeof STICKY_COLUMNS_WIDTH !== 'undefined') ? STICKY_COLUMNS_WIDTH : "55px";
        $table.find("th:first-child, td:first-child, th:last-child, td:last-child").css({
            "width": stickyWidth,
            "min-width": stickyWidth,
            "max-width": stickyWidth
        });
        
        // Distribute available space across middle columns
        const colCount = $table.find("th").length;
        if (colCount > 2) {
            const availableWidth = parseInt(message.width) - 110;
            const colWidth = Math.floor(availableWidth / (colCount - 2));
            
            $table.find("th:not(:first-child):not(:last-child)").css("width", colWidth + "px");
        }
        
        $("#scrollDiv").css("overflow-x", "visible");
        
        // Reinitialize colResizable
        if (typeof initColResizable === 'function') {
            initColResizable($table);
        }
    });
} catch(e) {
    console.error("[ERROR] Failed to register resizeDiv handler:", e);
}

// DataTable search functionality
// Filters table rows based on search key sent from server
Shiny.addCustomMessageHandler("DT_search", function(message) {
    const tableEl = $("#table table");
    // Check if table exists and is a DataTable instance
    if (tableEl.length > 0 && $.fn.DataTable.isDataTable(tableEl[0])) {
        tableEl.DataTable().search(message.key).draw(); // Apply search and redraw
    }
});

// Update selected option in dropdown
// Changes the text and value of currently selected option in a dropdown
Shiny.addCustomMessageHandler("updateSelectedOption", function(message) {
    const selectElem = document.getElementById(message.id);
    if (selectElem) {
        // Update both text and value of selected option
        $(selectElem).find("option:selected").text(message.new_value);
        $(selectElem).find("option:selected").val(message.new_value);
    }
});

// Update console output by appending text
// Appends new text to the console output element (used in modals for progress tracking)
Shiny.addCustomMessageHandler("updateOutput", function(message) {
    const pre = document.getElementById("console-output");
    if (pre) { 
        pre.innerText += message.output + "\n"; // Append new line to console
        // Autoscroll to bottom so new output is visible
        pre.scrollTop = pre.scrollHeight;
    }
    // Force Close button to show if flag is active
    if (window._forceCloseBtnVisible) { tryShowCloseBtn(window._closeBtnTargetId); }
    
    // Extra fallback: detect success patterns in output and show upload Close button
    try {
        const txt = (message && message.output) ? String(message.output) : "";
        // Check for success messages in output
        if (/Datastructure saved to/i.test(txt) || /successfully uploaded/i.test(txt) || /successfully created/i.test(txt)) {
            window._forceCloseBtnVisible = true;
            window._closeBtnTargetId = 'close_modal_upload';
            // Try multiple times with delays to ensure button appears
            tryShowCloseBtn('close_modal_upload');
            setTimeout(function(){ tryShowCloseBtn('close_modal_upload'); }, 50);
            setTimeout(function(){ tryShowCloseBtn('close_modal_upload'); }, 200);
            setTimeout(function(){ tryShowCloseBtn('close_modal_upload'); }, 1000);
        }
    } catch (e) { /* Silently ignore errors in pattern matching */ }
});

// Reload the page
// Triggers a full page reload (used after certain operations complete)
try {
    Shiny.addCustomMessageHandler("reloadPage", function(message) {
        window.location.reload();
    });
} catch(e) {
    console.error("[ERROR] Failed to register reloadPage handler:", e);
}

// Restore scroll position from localStorage
// Restores user's scroll position after page reload for better UX
try {
    Shiny.addCustomMessageHandler("restoreScroll", function(message) {
        const pos = localStorage.getItem("scrollPos");
        if (pos !== null) {
            setTimeout(function() {
                window.scrollTo(0, pos); // Scroll to saved position
            }, 100); // Small delay to ensure page is fully rendered
        }
    });
} catch(e) {
    console.error("[ERROR] Failed to register restoreScroll handler:", e);
}

// Update tooltip for dynamic button state feedback
// Updates button tooltips based on current application state
// Used for bulk move button to show contextual help messages
Shiny.addCustomMessageHandler("updateTooltip", function(message) {
    if (message && message.id && message.title) {
        const $element = $('#' + message.id);
        
        if ($element.length > 0) {
            // Update the title attribute directly (native browser tooltip)
            $element.attr('title', message.title);
            
            // If Bootstrap tooltip is initialized, update it as well
            if ($element.data('bs.tooltip')) {
                $element.tooltip('dispose').tooltip({title: message.title});
            }
            
            // Force update by removing and re-adding the title
            // This ensures browser tooltip is refreshed immediately
            const currentTitle = $element.attr('title');
            $element.removeAttr('title');
            setTimeout(function() {
                $element.attr('title', currentTitle);
            }, 10);
        }
    }
});

} // End of registerShinyHandlers function

// ===== MODAL CLOSE BUTTON MANAGEMENT =====

/**
 * Force the Close button to be visible in the active modal
 * Tries to show an existing button or create one if it doesn't exist
 * Removes shinyjs hidden classes and forces visibility
 * @param {string} id - The ID of the close button (defaults to "close_modal")
 * @returns {boolean} - True if button was successfully shown, false otherwise
 */
function tryShowCloseBtn(id) {
    const targetId = id || "close_modal";
    let btn = document.getElementById(targetId);
    
    if (!btn) {
        // Try to dynamically add the button if it doesn't exist
        const $footer = $('.modal:visible .modal-footer').last();
        if ($footer && $footer.length) {
            btn = document.createElement('button');
            btn.id = targetId;
            btn.className = 'btn btn-primary';
            btn.textContent = 'Close';
            btn.style.display = 'inline-block';
            // Trigger Shiny input event when button is clicked
            btn.addEventListener('click', function() {
                Shiny.setInputValue(targetId, Date.now(), { priority: 'event' });
            });
            $footer.append(btn);
        }
    }
    
    if (btn) {
        // Remove shinyjs hidden classes and inline styles that block visibility
        try { btn.classList.remove('shinyjs-hide', 'shinyjs-hidden'); } catch(e) {}
        try { btn.style.removeProperty('display'); } catch(e) {}
        try { btn.style.removeProperty('visibility'); } catch(e) {}
        // Force visibility
        btn.style.display = 'inline-block';
        btn.style.visibility = 'visible';
        return true;
    }
    return false;
}

// Handler to show the Close button in modals
// Called from server when modal operations complete
Shiny.addCustomMessageHandler("showCloseBtn", function(message) {
    try {
        // Set flag to retry showing button if needed
        window._forceCloseBtnVisible = true;
        window._closeBtnTargetId = (message && message.id) ? message.id : (window._closeBtnTargetId || 'close_modal');
        
        // Try immediately
        if (tryShowCloseBtn(window._closeBtnTargetId)) return;
        
        // Plan several retries for timing issues (modal may not be fully rendered)
        setTimeout(function(){ tryShowCloseBtn(window._closeBtnTargetId); }, 50);
        setTimeout(function(){ tryShowCloseBtn(window._closeBtnTargetId); }, 200);
        setTimeout(function(){ tryShowCloseBtn(window._closeBtnTargetId); }, 1000);
    } catch (e) {
        console.error("showCloseBtn handler error:", e);
    }
});

/**
 * Attach a MutationObserver to the console output element
 * Monitors changes to console output and triggers Close button display
 * Used to show Close button when console output indicates completion
 * @param {Element} modalEl - The modal element containing the console
 */
function attachConsoleObserver(modalEl) {
    try {
        if (typeof MutationObserver === 'undefined') return;
        
        // Clean up previous observer if exists
        if (window._consoleObserver) {
            try { window._consoleObserver.disconnect(); } catch(e) {}
            window._consoleObserver = null;
        }
        
        const pre = modalEl.querySelector('#console-output');
        if (!pre) return;
        
        // Create new observer to watch for console changes
        const obs = new MutationObserver(function() {
            if (window._forceCloseBtnVisible) { 
                tryShowCloseBtn(window._closeBtnTargetId); 
            }
        });
        
        // Observe changes to console content
        obs.observe(pre, { 
            childList: true,      // Watch for added/removed child nodes
            characterData: true,  // Watch for text changes
            subtree: true         // Watch entire subtree
        });
        
        window._consoleObserver = obs;
    } catch (e) {
        // Silently ignore errors in observer setup
    }
}

// When a modal becomes visible, try to show the Close button if needed
$(document).on('shown.bs.modal', '.modal', function() {
    if (window._forceCloseBtnVisible) {
        tryShowCloseBtn();
        // Extra retry shortly after modal is shown
        setTimeout(tryShowCloseBtn, 50);
    }
    // Attach observer to monitor console output in this modal
    attachConsoleObserver(this);
});

// ===== ADDITIONAL MESSAGE HANDLERS =====

// Initialize popovers with manual trigger
// Called from server to reinitialize popovers after DOM changes
Shiny.addCustomMessageHandler("initPopovers", function(message) {
    $("[data-toggle='popover']").popover({ trigger: "manual" });
});

// Reload Select2 elements
// Triggers reinitialization of all Select2 dropdowns in the table
Shiny.addCustomMessageHandler("reloadSelect2", function(message) {
    $(document).trigger("initializeSelect2");
});

// Listen for table updates to reinitialize Select2
// When Shiny updates the table, dropdowns need to be reinitialized
$(document).on('shiny:value', function(event) {
    if (event.name === 'table') {
        setTimeout(function() {
            $(document).trigger("initializeSelect2");
        }, INIT_DELAY);
    }
});

// ===== TAB MANAGEMENT =====

// Initialize when document is ready
$(document).ready(function() {
    
    // Track clicks for double-click detection (global state)
    // Used to detect when user double-clicks a tab to rename it
    window.tabClickState = {
        tabClickTimer: null,      // Timer for double-click window
        tabClickCount: 0,         // Number of clicks on current tab
        lastClickedTabId: null    // ID of last clicked tab
    };
    
    // Handle clicks on tab buttons (for switching AND renaming)
    $(document).on('click', '.tab-button', function(e) {
        // Don't handle if clicking on close button
        if ($(e.target).hasClass('tab-close-btn')) {
            return;
        }
        
        const $tabButton = $(this);
        const tabId = $tabButton.attr('data-tab-id');
        
        // Increment click count
        window.tabClickState.tabClickCount++;
        
        // If this is a different tab, reset counter and switch
        if (window.tabClickState.lastClickedTabId !== tabId) {
            window.tabClickState.tabClickCount = 1;
            window.tabClickState.lastClickedTabId = tabId;
            
            // Switch to this tab via Shiny
            Shiny.setInputValue('switch_tab', tabId, {priority: 'event'});
        }
        
        // Clear existing timer
        if (window.tabClickState.tabClickTimer) {
            clearTimeout(window.tabClickState.tabClickTimer);
            window.tabClickState.tabClickTimer = null;
        }
        
        // If double-click detected on same tab
        if (window.tabClickState.tabClickCount === 2 && window.tabClickState.lastClickedTabId === tabId) {
            window.tabClickState.tabClickCount = 0;
            window.tabClickState.lastClickedTabId = null;
            
            // Get the current tab name and start rename mode
            const currentName = $tabButton.find('.tab-name').text().trim();
            startTabRename(tabId, currentName);
            return;
        }
        
        // Set timer to reset click count after double-click window expires
        window.tabClickState.tabClickTimer = setTimeout(() => {
            window.tabClickState.tabClickCount = 0;
            window.tabClickState.tabClickTimer = null;
        }, 300); // 300ms window for double-click detection
    });
    
});

/**
 * Start inline rename mode for a tab
 * Replaces the tab name span with an editable input field
 * Sets up event handlers for Enter (confirm), Escape (cancel), and blur (confirm)
 * @param {string} tabId - The tab ID to rename
 * @param {string} currentName - The current tab name
 */
function startTabRename(tabId, currentName) {
    // Find the tab name span element
    const $tabButton = $(`.tab-button[data-tab-id="${tabId}"]`);
    const $nameSpan = $tabButton.find('.tab-name');
    
    if ($nameSpan.length === 0) {
        return; // Tab name element not found
    }
    
    // Create input field with current name as value
    const $input = $('<input>', {
        type: 'text',
        class: 'tab-rename-input',
        value: currentName,
        'data-tab-id': tabId,
        'data-original-name': currentName
    });
    
    // Replace span with input field
    $nameSpan.replaceWith($input);
    
    // Focus and select all text for easy editing
    $input.focus().select();
    
    // Handle Enter key - confirm rename
    $input.on('keydown', function(e) {
        if (e.key === 'Enter') {
            e.preventDefault();
            e.stopPropagation();
            finishTabRename(tabId, $input.val());
        } else if (e.key === 'Escape') {
            e.preventDefault();
            e.stopPropagation();
            cancelTabRename(tabId, currentName);
        }
    });
    
    // Handle blur - confirm rename when focus is lost
    $input.on('blur', function() {
        // Small delay to allow click events to fire first
        setTimeout(() => {
            if ($input.is(':visible')) {
                finishTabRename(tabId, $input.val());
            }
        }, 100);
    });
    
    // Prevent tab switch when clicking input field
    $input.on('click', function(e) {
        e.stopPropagation();
    });
}

/**
 * Finish tab rename and send to server
 * Validates the new name and sends rename request to Shiny
 * Replaces input field with span containing new name
 * @param {string} tabId - The tab ID
 * @param {string} newName - The new tab name entered by user
 */
function finishTabRename(tabId, newName) {
    const $input = $(`.tab-rename-input[data-tab-id="${tabId}"]`);
    const originalName = $input.attr('data-original-name');
    
    // Trim whitespace from new name
    newName = newName.trim();
    
    // If name is empty or unchanged, cancel the rename
    if (newName === "" || newName === originalName) {
        cancelTabRename(tabId, originalName);
        return;
    }
    
    // Send rename request to Shiny server
    Shiny.setInputValue('rename_tab', {
        tab_id: tabId,
        new_name: newName
    }, {priority: 'event'});
    
    // Replace input with span (will be updated by server re-render)
    const $nameSpan = $('<span>', {
        class: 'tab-name',
        text: newName
    });
    $input.replaceWith($nameSpan);
}

/**
 * Cancel tab rename and restore original name
 * Discards changes and replaces input field with original span
 * @param {string} tabId - The tab ID
 * @param {string} originalName - The original tab name to restore
 */
function cancelTabRename(tabId, originalName) {
    const $input = $(`.tab-rename-input[data-tab-id="${tabId}"]`);
    
    // Replace input with original span showing original name
    const $nameSpan = $('<span>', {
        class: 'tab-name',
        text: originalName
    });
    
    $input.replaceWith($nameSpan);
}

// ===== AUTO-FILL FUNCTIONALITY =====

/**
 * Initialize auto-fill preview table interactions
 * Sets up "select all" checkbox and individual row checkboxes
 * Handles checkbox state synchronization and selection tracking
 */
function initAutofillPreview() {
    // Select all checkbox handler
    $("#select_all_autofill").on("change", function() {
        $(".autofill_row_checkbox").prop("checked", this.checked);
    });
    
    // Individual checkbox handler
    // FASE 11.3: Enable/disable Apply button based on selection
    function updateApplyButton() {
        var anyChecked = $(".autofill_row_checkbox:checked").length > 0;
        $("#apply_autofill").prop("disabled", !anyChecked);
        
        // Visual feedback
        if (anyChecked) {
            $("#apply_autofill").removeClass("btn-default").addClass("btn-primary");
        } else {
            $("#apply_autofill").removeClass("btn-primary").addClass("btn-default");
        }
    }
    
    $(".autofill_row_checkbox").on("change", function() {
        // If any checkbox is unchecked, uncheck "select all"
        if (!this.checked) {
            $("#select_all_autofill").prop("checked", false);
        } else {
            // If all checkboxes are checked, check "select all"
            if ($(".autofill_row_checkbox:not(:checked)").length === 0) {
                $("#select_all_autofill").prop("checked", true);
            }
        }
        
        // Update Apply button state
        updateApplyButton();
    });
    
    // Initialize button state on modal open
    updateApplyButton();
    
    // Apply button handler - send selected row indices to Shiny
    $("#apply_autofill").on("click", function() {
        var selectedIndices = [];
        $(".autofill_row_checkbox:checked").each(function() {
            selectedIndices.push(parseInt($(this).data("row-index")));
        });
        Shiny.setInputValue("autofill_selected_rows", selectedIndices, {priority: "event"});
    });
}

// Listen for modal shown event to initialize auto-fill preview
$(document).on('shown.bs.modal', '.modal', function() {
    // Check if this is the autofill preview modal
    if ($(this).find('#select_all_autofill').length > 0) {
        initAutofillPreview();
    }
});

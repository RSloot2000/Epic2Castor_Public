// ===== CONSTANTS =====
const INIT_DELAY = 500;         // Initial delay for table initialization (ms)
const BACKUP_INIT_DELAY = 1500; // Backup delay for table initialization (ms)
const COL_RESIZE_DELAY = 100;   // Delay for column resize operations (ms)
const STICKY_COLUMNS_WIDTH = "55px"; // Width for sticky columns
const LOADING_DOTS_INTERVAL = 500;   // Interval for loading dots animation (ms)

// ===== WIZARD FUNCTIONALITY =====
/**
 * Import Wizard - Enhanced multi-step data import and mapping
 * Handles file upload, structure detection, column mapping, and data transformation
 * Integrates with import_wizard_combined.r backend functions
 */
const importWizard = {
  currentStep: 1,
  maxSteps: 4,
  
  /**
   * Initialize wizard UI interactions
   */
  init: function() {
    $(document).ready(() => {
      // Enable form controls for wizard
      $(document).on('click', '#wizard_process_file', () => {
        importWizard.analyzeFile();
      });
      
      $(document).on('click', '#wizard_start_transform', () => {
        importWizard.startTransformation();
      });
    });
  },
  
  /**
   * Analyze uploaded file structure
   */
  analyzeFile: function() {
    Shiny.setInputValue('wizard_process_file_click', Math.random());
  },
  
  /**
   * Start data transformation
   */
  startTransformation: function() {
    Shiny.setInputValue('wizard_start_transform_click', Math.random());
  },
  
  /**
   * Navigate to next wizard step
   */
  nextStep: function() {
    if (importWizard.currentStep < importWizard.maxSteps) {
      importWizard.currentStep++;
      importWizard.updateStepDisplay();
    }
  },
  
  /**
   * Update step indicator display
   */
  updateStepDisplay: function() {
    $('.wizard-step').each(function(index) {
      $(this).removeClass('step-active step-complete step-pending');
      
      if (index + 1 < importWizard.currentStep) {
        $(this).addClass('step-complete');
      } else if (index + 1 === importWizard.currentStep) {
        $(this).addClass('step-active');
      } else {
        $(this).addClass('step-pending');
      }
    });
  }
};

// Initialize wizard on document ready
$(document).ready(() => {
  importWizard.init();
});

// ===== PERFORMANCE MONITORING =====
/**
 * Performance Monitor - Track and log timing metrics for key operations
 * 
 * Features:
 * - Development mode toggle (enable/disable logging)
 * - High-resolution timing via Performance API
 * - Operation tracking with start/end pairs
 * - Automatic statistics calculation (min/max/avg)
 * - Memory usage monitoring (when available)
 * - Optional performance panel UI
 * 
 * Usage:
 *   performanceMonitor.start('operation_name');
 *   // ... do work ...
 *   performanceMonitor.end('operation_name');
 */
const performanceMonitor = {
    // Configuration
    enabled: true,  // Set to false in production to disable all logging
    showPanel: false, // Toggle performance panel visibility
    
    // Storage for active timers and completed metrics
    timers: {},
    metrics: {},
    
    // App-specific performance tracking
    appMetrics: {
        tableRenders: [],
        memorySnapshots: [],
        domUpdates: [],
        memoryBaseline: null,
        startTime: Date.now()
    },
    
    /**
     * Start timing an operation
     * @param {string} name - Unique operation identifier
     * @param {object} metadata - Optional metadata to log with timing
     */
    start: function(name, metadata = {}) {
        if (!this.enabled) return;
        
        const startTime = performance.now();
        this.timers[name] = {
            startTime: startTime,
            metadata: metadata
        };
        
        // Log start in verbose mode
        if (this.verbose) {
            console.log(`[Perf] ⏱️  START: ${name}`, metadata);
        }
    },
    
    /**
     * End timing an operation and log results
     * @param {string} name - Operation identifier (must match start() call)
     * @param {object} additionalData - Extra data to log with results
     * @returns {number} Duration in milliseconds
     */
    end: function(name, additionalData = {}) {
        if (!this.enabled) return 0;
        
        const endTime = performance.now();
        const timer = this.timers[name];
        
        if (!timer) {
            console.warn(`[Perf] ⚠️  No timer found for: ${name}`);
            return 0;
        }
        
        const duration = endTime - timer.startTime;
        
        // Store metric
        if (!this.metrics[name]) {
            this.metrics[name] = {
                count: 0,
                total: 0,
                min: Infinity,
                max: 0,
                durations: []
            };
        }
        
        const metric = this.metrics[name];
        metric.count++;
        metric.total += duration;
        metric.min = Math.min(metric.min, duration);
        metric.max = Math.max(metric.max, duration);
        metric.durations.push(duration);
        
        // Keep only last 50 measurements to prevent memory bloat
        if (metric.durations.length > 50) {
            metric.durations.shift();
        }
        
        // Log result with color coding
        const color = duration < 100 ? '#28a745' : duration < 500 ? '#ffc107' : '#dc3545';
        console.log(
            `%c[Perf] ✓ ${name}: ${duration.toFixed(2)}ms`,
            `color: ${color}; font-weight: bold`,
            {
                duration: `${duration.toFixed(2)}ms`,
                avg: `${(metric.total / metric.count).toFixed(2)}ms`,
                min: `${metric.min.toFixed(2)}ms`,
                max: `${metric.max.toFixed(2)}ms`,
                count: metric.count,
                ...timer.metadata,
                ...additionalData
            }
        );
        
        // Clean up timer
        delete this.timers[name];
        
        // Update performance panel if visible
        if (this.showPanel) {
            this.updatePanel();
        }
        
        return duration;
    },
    
    /**
     * Quick measure: wrap a function with automatic timing
     * @param {string} name - Operation name
     * @param {function} fn - Function to measure
     * @returns {*} Result of fn()
     */
    measure: function(name, fn) {
        if (!this.enabled) return fn();
        
        this.start(name);
        try {
            const result = fn();
            this.end(name);
            return result;
        } catch (error) {
            this.end(name, { error: error.message });
            throw error;
        }
    },
    
    /**
     * Get memory usage (if available)
     * @returns {object} Memory info or null
     */
    getMemoryUsage: function() {
        if (performance.memory) {
            const usedMB = performance.memory.usedJSHeapSize / 1048576;
            const totalMB = performance.memory.totalJSHeapSize / 1048576;
            const limitMB = performance.memory.jsHeapSizeLimit / 1048576;
            
            // Set baseline on first call
            if (!this.appMetrics.memoryBaseline) {
                this.appMetrics.memoryBaseline = usedMB;
            }
            
            return {
                used: usedMB.toFixed(2) + ' MB',
                usedRaw: usedMB,
                total: totalMB.toFixed(2) + ' MB',
                limit: limitMB.toFixed(2) + ' MB',
                baseline: this.appMetrics.memoryBaseline.toFixed(2) + ' MB',
                growth: (usedMB - this.appMetrics.memoryBaseline).toFixed(2) + ' MB'
            };
        }
        return null;
    },
    
    /**
     * Track table render performance
     * @param {string} tableName - Name of the table (e.g., 'elements', 'checkboxes')
     * @param {number} rowCount - Number of rows rendered
     * @param {number} duration - Render time in ms
     */
    trackTableRender: function(tableName, rowCount, duration) {
        if (!this.enabled) return;
        
        const render = {
            tableName,
            rowCount,
            duration,
            timestamp: Date.now(),
            rowsPerMs: rowCount / duration
        };
        
        this.appMetrics.tableRenders.push(render);
        
        // Keep only last 20 renders
        if (this.appMetrics.tableRenders.length > 20) {
            this.appMetrics.tableRenders.shift();
        }
        
        console.log(
            `%c[Perf] 📊 Table Render: ${tableName}`,
            'color: #9c27b0; font-weight: bold',
            {
                rows: rowCount,
                duration: `${duration.toFixed(2)}ms`,
                speed: `${render.rowsPerMs.toFixed(2)} rows/ms`
            }
        );
        
        if (this.showPanel) this.updatePanel();
    },
    
    /**
     * Take memory snapshot and track growth
     */
    snapshotMemory: function() {
        if (!this.enabled) return;
        
        const memory = this.getMemoryUsage();
        if (!memory) return;
        
        const snapshot = {
            timestamp: Date.now(),
            used: memory.usedRaw,
            elapsed: (Date.now() - this.appMetrics.startTime) / 1000 // seconds
        };
        
        this.appMetrics.memorySnapshots.push(snapshot);
        
        // Keep only last 50 snapshots
        if (this.appMetrics.memorySnapshots.length > 50) {
            this.appMetrics.memorySnapshots.shift();
        }
        
        // Calculate growth rate if we have enough data
        if (this.appMetrics.memorySnapshots.length >= 2) {
            const recent = this.appMetrics.memorySnapshots.slice(-10);
            const first = recent[0];
            const last = recent[recent.length - 1];
            const timeDiff = (last.timestamp - first.timestamp) / 1000; // seconds
            const memDiff = last.used - first.used;
            const growthRate = timeDiff > 0 ? (memDiff / timeDiff) : 0;
            
            snapshot.growthRate = growthRate; // MB per second
        }
        
        if (this.showPanel) this.updatePanel();
    },
    
    /**
     * Track DOM update performance
     * @param {string} operation - Description of DOM operation
     * @param {number} nodeCount - Number of nodes affected
     * @param {number} duration - Duration in ms
     */
    trackDOMUpdate: function(operation, nodeCount, duration) {
        if (!this.enabled) return;
        
        const update = {
            operation,
            nodeCount,
            duration,
            timestamp: Date.now(),
            nodesPerMs: nodeCount / duration
        };
        
        this.appMetrics.domUpdates.push(update);
        
        // Keep only last 30 updates
        if (this.appMetrics.domUpdates.length > 30) {
            this.appMetrics.domUpdates.shift();
        }
        
        console.log(
            `%c[Perf] 🔄 DOM Update: ${operation}`,
            'color: #ff9800; font-weight: bold',
            {
                nodes: nodeCount,
                duration: `${duration.toFixed(2)}ms`,
                speed: `${update.nodesPerMs.toFixed(2)} nodes/ms`
            }
        );
        
        if (this.showPanel) this.updatePanel();
    },
    
    /**
     * Log current metrics summary
     */
    logSummary: function() {
        if (!this.enabled) return;
        
        console.group('📊 Performance Summary');
        
        for (const [name, metric] of Object.entries(this.metrics)) {
            const avg = metric.total / metric.count;
            console.log(
                `${name}:`,
                `avg: ${avg.toFixed(2)}ms`,
                `min: ${metric.min.toFixed(2)}ms`,
                `max: ${metric.max.toFixed(2)}ms`,
                `count: ${metric.count}`
            );
        }
        
        const memory = this.getMemoryUsage();
        if (memory) {
            console.log('💾 Memory:', memory);
        }
        
        console.groupEnd();
    },
    
    /**
     * Clear all metrics
     */
    reset: function() {
        this.metrics = {};
        this.timers = {};
        this.appMetrics.tableRenders = [];
        this.appMetrics.memorySnapshots = [];
        this.appMetrics.domUpdates = [];
        // Keep baseline and startTime
        console.log('[Perf] 🔄 Metrics reset');
        
        if (this.showPanel) this.updatePanel();
    },
    
    /**
     * Toggle performance panel visibility
     */
    togglePanel: function() {
        this.showPanel = !this.showPanel;
        
        if (this.showPanel) {
            this.createPanel();
        } else {
            this.removePanel();
        }
    },
    
    /**
     * Create performance panel UI
     */
    createPanel: function() {
        // Remove existing panel
        $('#perf-panel').remove();
        
        const $panel = $(`
            <div id="perf-panel" style="
                position: fixed;
                bottom: 50px;
                right: 10px;
                width: 350px;
                max-height: 400px;
                background: rgba(0, 0, 0, 0.9);
                color: #fff;
                border: 1px solid #444;
                border-radius: 4px;
                padding: 10px;
                font-family: monospace;
                font-size: 11px;
                overflow-y: auto;
                z-index: 9999;
                box-shadow: 0 4px 6px rgba(0,0,0,0.3);
            ">
                <div style="display: flex; justify-content: space-between; margin-bottom: 10px; border-bottom: 1px solid #444; padding-bottom: 5px;">
                    <strong>⚡ Performance Monitor</strong>
                    <button id="perf-panel-close" style="background: none; border: none; color: #fff; cursor: pointer; font-size: 16px;">×</button>
                </div>
                <div id="perf-panel-content"></div>
            </div>
        `);
        
        $('body').append($panel);
        
        // Close button
        $('#perf-panel-close').on('click', () => this.togglePanel());
        
        // Update content
        this.updatePanel();
    },
    
    /**
     * Update performance panel content
     */
    updatePanel: function() {
        const $content = $('#perf-panel-content');
        if ($content.length === 0) return;
        
        let html = '';
        
        // Memory usage with growth rate
        const memory = this.getMemoryUsage();
        if (memory) {
            const growthColor = parseFloat(memory.growth) > 5 ? '#dc3545' : 
                               parseFloat(memory.growth) > 2 ? '#ffc107' : '#28a745';
            
            html += `<div style="margin-bottom: 10px; padding: 5px; background: rgba(255,255,255,0.1); border-radius: 3px;">
                <strong>💾 Memory:</strong> ${memory.used} / ${memory.total}
                <div style="font-size: 10px; color: #aaa; margin-top: 3px;">
                    Baseline: ${memory.baseline} | 
                    <span style="color: ${growthColor}">Growth: ${memory.growth}</span>
                </div>
            </div>`;
            
            // Memory growth rate (if available)
            const snapshots = this.appMetrics.memorySnapshots;
            if (snapshots.length >= 2) {
                const recent = snapshots[snapshots.length - 1];
                if (recent.growthRate !== undefined) {
                    const rateColor = recent.growthRate > 0.1 ? '#dc3545' : 
                                     recent.growthRate > 0.05 ? '#ffc107' : '#28a745';
                    html += `<div style="margin-bottom: 10px; padding: 5px; background: rgba(156, 39, 176, 0.2); border-radius: 3px;">
                        <strong>📈 Memory Growth Rate:</strong>
                        <span style="color: ${rateColor}; font-weight: bold;">
                            ${recent.growthRate >= 0 ? '+' : ''}${recent.growthRate.toFixed(3)} MB/s
                        </span>
                        <div style="font-size: 10px; color: #aaa; margin-top: 3px;">
                            Samples: ${snapshots.length}
                        </div>
                    </div>`;
                }
            }
        }
        
        // Table render metrics
        const tableRenders = this.appMetrics.tableRenders;
        if (tableRenders.length > 0) {
            const avgRenderTime = tableRenders.reduce((sum, r) => sum + r.duration, 0) / tableRenders.length;
            const avgRowsPerMs = tableRenders.reduce((sum, r) => sum + r.rowsPerMs, 0) / tableRenders.length;
            const lastRender = tableRenders[tableRenders.length - 1];
            
            html += `<div style="margin-bottom: 10px; padding: 5px; background: rgba(156, 39, 176, 0.2); border-radius: 3px;">
                <strong>📊 Table Renders:</strong> ${tableRenders.length}
                <div style="font-size: 10px; color: #aaa; margin-top: 3px;">
                    Avg: ${avgRenderTime.toFixed(2)}ms | 
                    Speed: ${avgRowsPerMs.toFixed(1)} rows/ms
                </div>
                <div style="font-size: 10px; color: #bbb; margin-top: 3px;">
                    Last: ${lastRender.tableName} (${lastRender.rowCount} rows, ${lastRender.duration.toFixed(2)}ms)
                </div>
            </div>`;
        }
        
        // DOM update metrics
        const domUpdates = this.appMetrics.domUpdates;
        if (domUpdates.length > 0) {
            const avgDuration = domUpdates.reduce((sum, u) => sum + u.duration, 0) / domUpdates.length;
            const avgNodesPerMs = domUpdates.reduce((sum, u) => sum + u.nodesPerMs, 0) / domUpdates.length;
            const lastUpdate = domUpdates[domUpdates.length - 1];
            
            html += `<div style="margin-bottom: 10px; padding: 5px; background: rgba(255, 152, 0, 0.2); border-radius: 3px;">
                <strong>🔄 DOM Updates:</strong> ${domUpdates.length}
                <div style="font-size: 10px; color: #aaa; margin-top: 3px;">
                    Avg: ${avgDuration.toFixed(2)}ms | 
                    Speed: ${avgNodesPerMs.toFixed(1)} nodes/ms
                </div>
                <div style="font-size: 10px; color: #bbb; margin-top: 3px;">
                    Last: ${lastUpdate.operation}
                </div>
            </div>`;
        }
        
        // Keyboard Shortcuts Metrics
        if (window.shortcutHandler && window.shortcutHandler.performanceMonitor) {
            const scStats = window.shortcutHandler.performanceMonitor.getStats();
            const topShortcuts = window.shortcutHandler.performanceMonitor.getTopShortcuts(3);
            
            html += `<div style="margin-bottom: 10px; padding: 5px; background: rgba(33, 150, 243, 0.2); border-radius: 3px;">
                <strong>⌨️ Keyboard Shortcuts:</strong>
                <div style="font-size: 10px; color: #aaa; margin-top: 5px;">
                    Total: ${scStats.totalShortcuts} | 
                    Avg: ${scStats.avgCallbackTime} | 
                    Max: ${scStats.maxCallbackTime}
                </div>`;
            
            if (topShortcuts.length > 0) {
                html += '<div style="font-size: 10px; color: #bbb; margin-top: 5px;">Top: ';
                html += topShortcuts.map(s => `${s.category}.${s.action} (${s.count}x)`).join(', ');
                html += '</div>';
            }
            
            html += '</div>';
        }
        
        // Metrics
        html += '<div style="margin-top: 10px;">';
        for (const [name, metric] of Object.entries(this.metrics)) {
            const avg = metric.total / metric.count;
            const color = avg < 100 ? '#28a745' : avg < 500 ? '#ffc107' : '#dc3545';
            
            html += `
                <div style="margin-bottom: 8px; padding: 5px; background: rgba(255,255,255,0.05); border-left: 3px solid ${color};">
                    <div style="font-weight: bold;">${name}</div>
                    <div style="font-size: 10px; color: #aaa;">
                        Avg: ${avg.toFixed(2)}ms | 
                        Min: ${metric.min.toFixed(2)}ms | 
                        Max: ${metric.max.toFixed(2)}ms | 
                        Count: ${metric.count}
                    </div>
                </div>
            `;
        }
        html += '</div>';
        
        $content.html(html);
    },
    
    /**
     * Remove performance panel
     */
    removePanel: function() {
        $('#perf-panel').remove();
    }
};

// Keyboard shortcut to toggle performance panel: Ctrl+Shift+P
$(document).on('keydown', function(e) {
    if (e.ctrlKey && e.shiftKey && e.key === 'P') {
        e.preventDefault();
        performanceMonitor.togglePanel();
    }
});

// Log summary on Ctrl+Shift+L
$(document).on('keydown', function(e) {
    if (e.ctrlKey && e.shiftKey && e.key === 'L') {
        e.preventDefault();
        performanceMonitor.logSummary();
    }
});

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
    console.log('[Loading] ===== hideLoadingScreen called at:', new Date().toISOString(), '=====');
    console.log('[Loading] isFirstResize flag:', window.isFirstResize);
    
    // No need to clear interval anymore - CSS handles animation
    // Keeping this for backward compatibility
    if (loadingDotsInterval) {
        console.log('[Loading] Clearing interval ID:', loadingDotsInterval);
        clearInterval(loadingDotsInterval);
        loadingDotsInterval = null;
    }
    
    // Add 1 second delay before starting fade out
    console.log('[Loading] Starting 1 second delay before fade out');
    setTimeout(function() {
        console.log('[Loading] 1 second delay complete, starting fade out');
        // Fade out and remove the loading screen from DOM
        const loadingScreen = document.getElementById('loading-screen');
        if (loadingScreen) {
            // Apply table resize BEFORE hiding loading screen
            // This ensures all resizes happen while loading screen is visible
            console.log('[Loading] Forcing tableAutoWidth to true for startup resize');
            window.tableAutoWidth = true;
            
            if (typeof window.resizeTableToWindow === 'function') {
                console.log('[Loading] Starting 300ms delay for cold start before resize');
                // Use setTimeout with longer delay for cold starts (full browser restart)
                // This gives the footer more time to fully render and get correct dimensions
                setTimeout(function() {
                    console.log('[Loading] 300ms delay complete, starting requestAnimationFrame');
                    // Double requestAnimationFrame to ensure all layout is complete
                    requestAnimationFrame(function() {
                        console.log('[Loading] First requestAnimationFrame');
                        requestAnimationFrame(function() {
                            console.log('[Loading] Second requestAnimationFrame - about to measure footer');
                            // Log footer height for debugging
                            const $footer = $('.fixed-footer');
                            const footerHeight = $footer.length > 0 ? $footer.outerHeight(true) : 0;
                            const footerExists = $footer.length > 0;
                            const footerVisible = $footer.length > 0 ? $footer.is(':visible') : false;
                            console.log('[Loading] Footer - exists:', footerExists, ', visible:', footerVisible, ', height:', footerHeight + 'px');
                            console.log('[Loading] About to call resizeTableToWindow, isFirstResize:', window.isFirstResize);
                            
                            window.resizeTableToWindow();
                            
                            console.log('[Loading] resizeTableToWindow completed');
                            
                            // Wait a bit for any final adjustments, then hide loading screen
                            setTimeout(function() {
                                console.log('[Loading] Adding fade-out class');
                                loadingScreen.classList.add('fade-out'); // CSS handles smooth transition
                                setTimeout(function() {
                                    loadingScreen.remove(); // Remove from DOM after fade completes
                                    console.log('[Loading] Loading screen removed from DOM at:', new Date().toISOString());
                                    // Enable window resize events now that loading screen is completely gone
                                    window.loadingScreenActive = false;
                                    console.log('[Loading] loadingScreenActive set to false - window resizes now enabled');
                                }, 500); // Match the CSS transition duration
                            }, 100); // Small delay to let final resize settle
                        });
                    });
                }, 300); // Extra delay for cold start scenarios
            } else {
                console.log('[Loading] tableAutoWidth is false or resizeTableToWindow not available');
                // If resize not available, just hide loading screen normally
                console.log('[Loading] Adding fade-out class');
                loadingScreen.classList.add('fade-out');
                setTimeout(function() {
                    loadingScreen.remove();
                    console.log('[Loading] Loading screen removed from DOM at:', new Date().toISOString());
                }, 500);
            }
        } else {
            console.log('[Loading] Loading screen element not found');
        }
    }, 1000); // Keep loading screen visible for 1 extra second
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
    
    // Start periodic memory monitoring (every 10 seconds)
    if (performanceMonitor.enabled) {
        setInterval(function() {
            performanceMonitor.snapshotMemory();
        }, 10000);
        
        // Take initial snapshot
        performanceMonitor.snapshotMemory();
    }
});

// ===== LOADING INDICATORS (STEP 3) =====

/**
 * Shows a loading overlay on the table during updates
 */
function showTableLoading() {
    const domStart = performance.now();
    const $tableContainer = $('#table');
    
    // Remove existing overlay if present
    $tableContainer.find('.table-loading-overlay').remove();
    
    // Create and append overlay
    const $overlay = $('<div class="table-loading-overlay">' +
                       '<div class="table-loading-spinner"></div>' +
                       '</div>');
    
    $tableContainer.css('position', 'relative');
    $tableContainer.append($overlay);
    
    const domDuration = performance.now() - domStart;
    if (performanceMonitor.enabled) {
        performanceMonitor.trackDOMUpdate('show_table_loading', 2, domDuration);
    }
}

/**
 * Hides the table loading overlay
 */
function hideTableLoading() {
    $('#table .table-loading-overlay').fadeOut(200, function() {
        $(this).remove();
    });
}

/**
 * Shows a spinner inside a button
 * @param {string} buttonId - The ID of the button
 */
function showButtonSpinner(buttonId) {
    const $button = $('#' + buttonId);
    if ($button.find('.btn-spinner').length === 0) {
        $button.prepend('<span class="btn-spinner"></span>');
        $button.prop('disabled', true);
    }
}

/**
 * Hides spinner from a button
 * @param {string} buttonId - The ID of the button
 */
function hideButtonSpinner(buttonId) {
    const $button = $('#' + buttonId);
    $button.find('.btn-spinner').remove();
    $button.prop('disabled', false);
}

// ===== CORE FUNCTIONS =====

/**
 * Initialize column resize synchronization between header and body tables
 * Uses ResizeObserver to detect width changes in header columns and applies them to body columns
 * This creates Excel-like behavior where resizing header columns resizes the entire column
 */
function initColumnResizeSync() {
    const $scrollWrapper = $('.dataTables_scroll');
    const $headerTable = $scrollWrapper.find('.dataTables_scrollHead table');
    const $bodyTable = $scrollWrapper.find('.dataTables_scrollBody table');
    
    if ($headerTable.length === 0 || $bodyTable.length === 0) {
        console.warn('[ColumnResize] Header or body table not found');
        return;
    }
    
    // Clean up existing observers
    if (window.columnResizeObservers) {
        window.columnResizeObservers.forEach(observer => observer.disconnect());
    }
    window.columnResizeObservers = [];
    
    // Use ResizeObserver to detect column width changes in header
    const $headerCols = $headerTable.find('th');
    
    $headerCols.each(function(index) {
        const $headerCol = $(this);
        
        // Skip sticky columns: first (Index) and last (Select)
        const isIndexColumn = index === 0;
        const isSelectColumn = index === $headerCols.length - 1;
        
        if (isIndexColumn || isSelectColumn) {
            console.log('[ColumnResize] Skipping sticky column at index', index, '(total:', $headerCols.length + ')');
            // Force sticky column width and prevent resizing
            $headerCol.css({
                'width': '55px',
                'min-width': '55px',
                'max-width': '55px',
                'resize': 'none'
            });
            return;
        }
        
        // Create ResizeObserver for this header column
        const observer = new ResizeObserver(entries => {
            for (let entry of entries) {
                const newWidth = entry.contentRect.width;
                
                // Double-check this isn't a sticky column before applying resize
                const $currentHeader = $(entry.target);
                const currentIndex = $currentHeader.index();
                const totalCols = $headerCols.length;
                const isSticky = currentIndex === 0 || currentIndex === totalCols - 1;
                
                if (isSticky) {
                    console.warn('[ColumnResize] Blocked resize attempt on sticky column at index', currentIndex);
                    return;
                }
                
                // Apply same width to corresponding body column cells
                $bodyTable.find('tr').each(function() {
                    const $cell = $(this).find('td').eq(index);
                    if ($cell.length > 0) {
                        $cell.css({
                            'width': newWidth + 'px',
                            'min-width': newWidth + 'px',
                            'max-width': newWidth + 'px'
                        });
                    }
                });
            }
        });
        
        observer.observe($headerCol[0]);
        window.columnResizeObservers.push(observer);
    });
    
    console.log('[ColumnResize] Initialized resize sync for', $headerCols.length, 'columns');
}

/**
 * Initialize custom column resize grips
 * Creates draggable grips on column borders for precise resizing
 * Only affects adjacent columns (left shrinks, right expands or vice versa)
 */
function initColumnResizeGrips() {
    const $scrollWrapper = $('.dataTables_scroll');
    const $headerTable = $scrollWrapper.find('.dataTables_scrollHead table');
    const $bodyTable = $scrollWrapper.find('.dataTables_scrollBody table');
    
    if ($headerTable.length === 0 || $bodyTable.length === 0) {
        console.warn('[ResizeGrip] Header or body table not found');
        return;
    }
    
    // Remove existing grips
    $headerTable.find('.resize-grip').remove();
    
    const $headerCols = $headerTable.find('th');
    const totalCols = $headerCols.length;
    
    $headerCols.each(function(index) {
        const $th = $(this);
        
        // Skip last two columns (no grip after second-to-last and last column)
        if (index >= totalCols - 2) {
            return;
        }
        
        // Skip first column (Index) - no grip before it
        if (index === 0) {
            return;
        }
        
        // Create resize grip element
        const $grip = $('<div class="resize-grip"></div>');
        $th.append($grip);
        
        // Mouse down: start resize
        $grip.on('mousedown', function(e) {
            e.preventDefault();
            
            const $currentCol = $(this).parent();
            const $nextCol = $currentCol.next();
            const currentIndex = $currentCol.index();
            const nextIndex = $nextCol.index();
            
            // Verify both columns are resizable (not sticky)
            const isCurrentSticky = currentIndex === 0 || currentIndex === totalCols - 1;
            const isNextSticky = nextIndex === 0 || nextIndex === totalCols - 1;
            
            if (isCurrentSticky || isNextSticky) {
                console.warn('[ResizeGrip] Cannot resize sticky columns');
                return;
            }
            
            $grip.addClass('resizing');
            
            const startX = e.pageX;
            const startWidthCurrent = $currentCol.outerWidth();
            const startWidthNext = $nextCol.outerWidth();
            const minWidth = 50; // Minimum column width
            
            console.log('[ResizeGrip] Start resize - Current:', startWidthCurrent, 'Next:', startWidthNext);
            
            // Mouse move: perform resize
            const onMouseMove = function(e) {
                const deltaX = e.pageX - startX;
                
                // Calculate new widths
                let newWidthCurrent = startWidthCurrent + deltaX;
                let newWidthNext = startWidthNext - deltaX;
                
                // Enforce minimum widths
                if (newWidthCurrent < minWidth) {
                    newWidthCurrent = minWidth;
                    newWidthNext = startWidthNext + startWidthCurrent - minWidth;
                } else if (newWidthNext < minWidth) {
                    newWidthNext = minWidth;
                    newWidthCurrent = startWidthCurrent + startWidthNext - minWidth;
                }
                
                // Apply to header columns
                $currentCol.css({
                    'width': newWidthCurrent + 'px',
                    'min-width': newWidthCurrent + 'px',
                    'max-width': newWidthCurrent + 'px'
                });
                
                $nextCol.css({
                    'width': newWidthNext + 'px',
                    'min-width': newWidthNext + 'px',
                    'max-width': newWidthNext + 'px'
                });
                
                // Apply to body columns
                $bodyTable.find('tr').each(function() {
                    const $currentCell = $(this).find('td').eq(currentIndex);
                    const $nextCell = $(this).find('td').eq(nextIndex);
                    
                    if ($currentCell.length > 0) {
                        $currentCell.css({
                            'width': newWidthCurrent + 'px',
                            'min-width': newWidthCurrent + 'px',
                            'max-width': newWidthCurrent + 'px'
                        });
                    }
                    
                    if ($nextCell.length > 0) {
                        $nextCell.css({
                            'width': newWidthNext + 'px',
                            'min-width': newWidthNext + 'px',
                            'max-width': newWidthNext + 'px'
                        });
                    }
                });
            };
            
            // Mouse up: end resize
            const onMouseUp = function(e) {
                $grip.removeClass('resizing');
                $(document).off('mousemove', onMouseMove);
                $(document).off('mouseup', onMouseUp);
                
                const finalWidthCurrent = $currentCol.outerWidth();
                const finalWidthNext = $nextCol.outerWidth();
                console.log('[ResizeGrip] End resize - Current:', finalWidthCurrent, 'Next:', finalWidthNext);
            };
            
            $(document).on('mousemove', onMouseMove);
            $(document).on('mouseup', onMouseUp);
        });
    });
    
    console.log('[ResizeGrip] Initialized resize grips for', $headerCols.length, 'columns');
}

/**
 * Initialize table resize grip in bottom-right corner
 * Allows users to drag-resize the entire table width like in Excel
 * Replaces the old slider-based width control
 */
function initTableResizeGrip() {
    console.log('[TableResize] Initializing table resize grip');
    
    // Remove any existing grip first
    $('.table-resize-grip').remove();
    
    // Create grip element (no text content - icon will be in CSS)
    const $grip = $('<div class="table-resize-grip" title="Sleep om tabel te vergroten/verkleinen"></div>');
    
    // Append to body with fixed positioning (always visible)
    $('body').append($grip);
    console.log('[TableResize] Created and appended resize grip to body');
    
    // Calculate position based on table scroll body location
    const updateGripPosition = function() {
        const $scrollBody = $('.dataTables_scrollBody');
        const $footer = $('.fixed-footer');
        
        if ($scrollBody.length > 0 && $scrollBody.is(':visible')) {
            // Position relative to the bottom-right corner of scroll body
            const scrollBodyOffset = $scrollBody.offset();
            const scrollBodyHeight = $scrollBody.outerHeight();
            const scrollBodyWidth = $scrollBody.outerWidth();
            
            // Position grip relatively: always 50px from right edge and in info bar area below table
            const gripOffsetFromRight = 50; // px from right edge of scrollBody
            const gripOffsetFromBottom = 15; // px below scrollBody (in info bar area)
            const gripLeft = scrollBodyOffset.left + scrollBodyWidth - gripOffsetFromRight;
            const gripTop = scrollBodyOffset.top + scrollBodyHeight + gripOffsetFromBottom;
            
            $grip.css({
                'position': 'fixed',
                'top': gripTop + 'px',
                'left': gripLeft + 'px',
                'bottom': 'auto',
                'right': 'auto',
                'display': 'flex',
                'visibility': 'visible'
            });
            
            console.log('[TableResize] Grip positioned at top:', gripTop, 'px, left:', gripLeft, 'px (scroll body bottom-right + info bar)');
        } else if ($footer.length > 0 && $footer.is(':visible')) {
            // Fallback: position relative to footer if scroll body not found yet
            const footerOffset = $footer.offset();
            const footerTop = footerOffset.top;
            const windowHeight = $(window).height();
            const windowWidth = $(window).width();
            const gripBottom = windowHeight - footerTop + 10;
            const gripLeft = windowWidth - 80; // 80px from right edge
            
            $grip.css({
                'position': 'fixed',
                'bottom': gripBottom + 'px',
                'left': gripLeft + 'px',
                'top': 'auto',
                'right': 'auto',
                'display': 'flex',
                'visibility': 'visible'
            });
            
            console.log('[TableResize] Grip positioned at bottom:', gripBottom, 'px, left:', gripLeft, 'px (fallback to footer)');
        } else {
            console.warn('[TableResize] Could not find scroll body or footer');
        }
    };
    
    // Update position initially with delays to ensure table is fully rendered
    setTimeout(function() {
        updateGripPosition();
    }, 500);
    
    setTimeout(function() {
        updateGripPosition();
    }, 1000);
    
    // Wait even longer for initial table load to complete
    setTimeout(function() {
        updateGripPosition();
    }, 2000);
    
    // Extra long delay to catch late table initialization
    setTimeout(function() {
        updateGripPosition();
        console.log('[TableResize] Final grip position update after full table load');
    }, 3000);
    
    // Update after window resizes (table is auto-resized)
    $(window).on('resize.gripPosition', function() {
        setTimeout(updateGripPosition, 100); // Small delay after resize
    });
    
    // Also update when DataTables redraws (after data loads)
    $(document).on('draw.dt', function() {
        setTimeout(updateGripPosition, 100);
    });
    
    // Verify grip is visible
    setTimeout(function() {
        const isVisible = $('.table-resize-grip').is(':visible');
        const gripOffset = $grip.offset();
        console.log('[TableResize] Grip check - Visible:', isVisible, 'Position:', gripOffset);
        if (isVisible) {
            console.log('[TableResize] ✓ Grip is visible and positioned correctly');
        } else {
            console.warn('[TableResize] ✗ Grip is not visible - attempting fix');
            updateGripPosition();
        }
    }, 600);
    
    // Remove any existing event handlers
    $grip.off('mousedown');
    
    // Mouse down: start resize
    $grip.on('mousedown', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        console.log('[TableResize] Starting resize drag');
        $grip.addClass('resizing');
        
        // Disable auto-width mode when user manually resizes
        window.tableAutoWidth = false;
        
        // Get initial dimensions
        const $container = $('.col-sm-8');
        const $scrollDiv = $('#scrollDiv');
        const startX = e.pageX;
        const startY = e.pageY;
        const startWidth = $container.width();
        const startHeight = $scrollDiv.height();
        
        console.log('[TableResize] Start - width:', startWidth, 'px, height:', startHeight, 'px');
        
        // Mouse move: update width and height
        const onMouseMove = function(moveEvent) {
            const deltaX = moveEvent.pageX - startX;
            const deltaY = moveEvent.pageY - startY;
            
            // Calculate max width/height based on window size to prevent table going off-screen
            const windowWidth = $(window).width();
            const windowHeight = $(window).height();
            const maxWidth = windowWidth - 100; // Leave 100px margin on right
            const maxHeight = windowHeight - 200; // Leave 200px for header/footer
            
            const newWidth = Math.max(400, Math.min(maxWidth, startWidth + deltaX)); // Min 400px, max = window width - margin
            const newHeight = Math.max(300, Math.min(maxHeight, startHeight + deltaY)); // Min 300px, max = window height - margins
            
            // Update container width and height
            $container.css('width', newWidth + 'px');
            
            // Update scrollDiv width and height so the container actually grows
            $scrollDiv.css({
                'width': newWidth + 'px',
                'height': newHeight + 'px'
            });
            
            // Update DataTables scroll wrapper + body height (use height, not only max-height)
            const scrollBodyHeight = newHeight - 100;
            $('.dataTables_scroll').css({
                'height': newHeight + 'px',
                'max-height': newHeight + 'px'
            });
            $('.dataTables_scrollBody').css({
                'height': scrollBodyHeight + 'px',
                'max-height': scrollBodyHeight + 'px'
            });
            
            // Update grip position based on new dimensions - RELATIVE to table size
            const $scrollBodyForGrip = $('.dataTables_scrollBody');
            if ($scrollBodyForGrip.length > 0) {
                const scrollBodyOffset = $scrollBodyForGrip.offset();
                
                // Position grip relatively: always 50px from right edge and in info bar area
                const gripOffsetFromRight = 50; // px from right edge
                const gripOffsetFromBottom = 15; // px below scrollBody
                const gripLeft = scrollBodyOffset.left + newWidth - gripOffsetFromRight;
                const gripTop = scrollBodyOffset.top + scrollBodyHeight + gripOffsetFromBottom;
                
                $grip.css({
                    'top': gripTop + 'px',
                    'left': gripLeft + 'px'
                });
                
                console.log('[TableResize] Grip positioned at top:', gripTop, 'px, left:', gripLeft, 'px (scroll body bottom-right + info bar)');
            }
            
            // Update DataTables widths
            const $scrollWrapper = $('.dataTables_scroll');
            const $headerTable = $scrollWrapper.find('.dataTables_scrollHead table');
            const $bodyTable = $scrollWrapper.find('.dataTables_scrollBody table');
            const $scrollHead = $('.dataTables_scrollHead');
            const $scrollBody = $('.dataTables_scrollBody');
            
            // Set exact same width on all table elements
            const widthStyle = newWidth + 'px';
            
            if ($scrollHead.length > 0) {
                $scrollHead.css({
                    'width': widthStyle,
                    'overflow': 'hidden'
                });
            }
            if ($scrollBody.length > 0) {
                $scrollBody.css('width', widthStyle);
            }
            
            // Set table widths
            $headerTable.css({
                'width': widthStyle,
                'table-layout': 'auto'
            });
            $bodyTable.css({
                'width': widthStyle,
                'table-layout': 'auto'
            });
            
            // Calculate column widths
            const stickyWidth = (typeof STICKY_COLUMNS_WIDTH !== 'undefined') ? STICKY_COLUMNS_WIDTH : "55px";
            const colCount = $headerTable.find("th").length || $bodyTable.find("th").length;
            
            if (colCount > 2) {
                const availableWidth = newWidth - (parseInt(stickyWidth) * 2);
                const middleColWidth = Math.floor(availableWidth / (colCount - 2));
                
                // Apply widths to header
                $headerTable.find("th").each(function(index) {
                    const $th = $(this);
                    if (index === 0 || index === colCount - 1) {
                        $th.css({
                            'width': stickyWidth,
                            'min-width': stickyWidth,
                            'max-width': stickyWidth
                        });
                    } else {
                        $th.css({
                            'width': middleColWidth + 'px',
                            'min-width': middleColWidth + 'px',
                            'max-width': middleColWidth + 'px'
                        });
                    }
                });
                
                // Apply widths to body
                $bodyTable.find("tr:first td").each(function(index) {
                    const $td = $(this);
                    if (index === 0 || index === colCount - 1) {
                        $td.css({
                            'width': stickyWidth,
                            'min-width': stickyWidth,
                            'max-width': stickyWidth
                        });
                    } else {
                        $td.css({
                            'width': middleColWidth + 'px',
                            'min-width': middleColWidth + 'px',
                            'max-width': middleColWidth + 'px'
                        });
                    }
                });
            }
            
            // DO NOT call dt.columns.adjust() during drag - causes misalignment warnings
            // Only adjust columns when drag is complete (in onMouseUp)
        };
        
        // Mouse up: end resize
        const onMouseUp = function(upEvent) {
            console.log('[TableResize] Ending resize drag');
            $grip.removeClass('resizing');
            $(document).off('mousemove', onMouseMove);
            $(document).off('mouseup', onMouseUp);
            
            const finalWidth = $('.col-sm-8').width();
            const finalHeight = $('#scrollDiv').height();
            console.log('[TableResize] Final dimensions - width:', finalWidth, 'px, height:', finalHeight, 'px');
            
            // Update grip position one final time
            if (typeof updateGripPosition === 'function') {
                updateGripPosition();
            }
            
            // Adjust DataTables columns ONCE after resize is complete
            // Use setTimeout to ensure all CSS changes have been applied
            setTimeout(function() {
                const $bodyTable = $('.dataTables_scrollBody table');
                const dt = $bodyTable.DataTable();
                if (dt) {
                    try {
                        dt.columns.adjust().draw(false); // draw(false) = don't reset paging
                        console.log('[TableResize] Columns adjusted after resize');
                    } catch(e) {
                        console.warn('[TableResize] Error adjusting columns:', e);
                    }
                }
            }, 50);
        };
        
        $(document).on('mousemove', onMouseMove);
        $(document).on('mouseup', onMouseUp);
    });
    
    console.log('[TableResize] Resize grip initialized successfully');
}

/**
 * Converts lazy dropdown placeholder div into actual Select2 dropdown
 * Called when user focuses on a cell for the first time
 * Generates full HTML dropdown on-demand to improve initial render performance
 * @param {jQuery} $div - The lazy-dropdown div element to convert
 */
function convertLazyDropdownToSelect($div) {
    performanceMonitor.start('lazy_dropdown_convert');
    
    // Extract data from placeholder
    const row = $div.data('row');
    const col = $div.data('col');
    const originalCol = $div.data('original-col') || col;
    const currentValue = $div.data('value') || '';
    const cssClass = $div.data('cssclass') || '';
    const optionsAttr = $div.attr('data-options');
    
    // Parse options from JSON
    let options = [];
    try {
        // Decode HTML entities back to JSON
        const optionsJson = optionsAttr
            .replace(/&quot;/g, '"')
            .replace(/&#39;/g, "'");
        options = JSON.parse(optionsJson);
    } catch (e) {
        console.error('Failed to parse dropdown options:', e);
        options = [currentValue]; // Fallback to current value
    }
    
    // Build select element HTML
    let optionsHtml = '';
    options.forEach(function(opt) {
        const isSelected = opt === currentValue;
        const escapedOpt = $('<div>').text(opt).html(); // HTML escape
        optionsHtml += '<option value="' + escapedOpt + '"' + 
                       (isSelected ? ' selected' : '') + '>' + 
                       escapedOpt + '</option>';
    });
    
    // Add "Add value..." option
    optionsHtml += '<option value="__ADD__">Add value...</option>';
    
    // Create select element
    const selectId = 'dropdown_' + originalCol + '_' + row;
    const selectHtml = '<select id="' + selectId + '" ' +
                       'data-row="' + row + '" ' +
                       'data-col="' + originalCol + '" ' +
                       'data-options=\'' + optionsAttr + '\' ' +
                       'data-cssclass="' + cssClass + '" ' +
                       'class="lazy-load">' +
                       optionsHtml +
                       '</select>';
    
    // Replace div with select
    const $select = $(selectHtml);
    $div.replaceWith($select);
    
    performanceMonitor.end('lazy_dropdown_convert', { row, col: originalCol });
    
    // Initialize Select2 on the new select element
    initializeSelect2OnElement($select);
    
    // Open dropdown immediately (user just clicked/focused)
    setTimeout(function() {
        $select.select2('open');
    }, 10);
    
    // Note: Change handler is set up globally via delegated event
    // See $(document).on("change", "select.lazy-load", ...) below
}

/**
 * Initializes Select2 on a specific select element
 * Extracted to separate function for reuse
 * @param {jQuery} $sel - The select element to initialize
 */
function initializeSelect2OnElement($sel) {
    // Remove existing select2 if present
    if ($sel.hasClass("select2-hidden-accessible")) {
        $sel.select2("destroy");
    }
    
    // Initialize select2 with full width and body parent; we manage scroll manually to avoid jumps
    $sel.select2({
        width: '100%',
        dropdownParent: $('body')
    });

    // Remember scroll position of the table body right before opening the dropdown
    $sel.off('select2:opening').on('select2:opening', function() {
        const $scrollBody = $('.dataTables_scrollBody');
        if ($scrollBody.length) {
            $sel.data('scrollTopBeforeOpen', $scrollBody.scrollTop());
        }
        $sel.data('windowScrollBeforeOpen', $(window).scrollTop());
    });
    
    // Position dropdown menu correctly using fixed positioning
    $sel.off("select2:open").on("select2:open", function() {
            window.requestAnimationFrame(function() {
                const $container = $sel.next('.select2-container');
                if ($container.length === 0) return;
                
                // Get position of the select2 container
                const rect = $container[0].getBoundingClientRect();
                const $dd = $('.select2-container--open .select2-dropdown');
                const $scrollBody = $('.dataTables_scrollBody');
                const storedScrollTop = $sel.data('scrollTopBeforeOpen');
                const storedWinScroll = $sel.data('windowScrollBeforeOpen');

                if ($dd.length === 0) return;

                // Apply fixed positioning based on container location
                $dd.css({
                    position: 'fixed',
                    left: rect.left + 'px',
                    minWidth: rect.width + 'px'
                });

                // Dynamic placement with clamped height to avoid jumping to the top
                const rawHeight = $dd.outerHeight();
                const viewportHeight = window.innerHeight || document.documentElement.clientHeight;
                const padding = 8; // small viewport margin
                const spaceBelow = viewportHeight - rect.bottom - padding;
                const spaceAbove = rect.top - padding;

                let placeBelow = true;
                let maxHeight = null;
                if (spaceBelow >= rawHeight) {
                    placeBelow = true;
                } else if (spaceAbove >= rawHeight) {
                    placeBelow = false;
                } else {
                    // Not enough space on either side: choose the larger side and clamp height
                    placeBelow = spaceBelow >= spaceAbove;
                    maxHeight = Math.max(120, placeBelow ? spaceBelow : spaceAbove);
                    $dd.css({ maxHeight: maxHeight + 'px', overflowY: 'auto' });
                }

                const usedHeight = maxHeight ? Math.min(rawHeight, maxHeight) : rawHeight;

                if (placeBelow) {
                    $dd.removeClass('select2-dropdown--above').addClass('select2-dropdown--below');
                    $sel.next('.select2-container').removeClass('select2-container--above').addClass('select2-container--below');
                    $dd.css('top', rect.bottom + 'px');
                } else {
                    const topPos = Math.max(rect.top - usedHeight, padding);
                    $dd.removeClass('select2-dropdown--below').addClass('select2-dropdown--above');
                    $sel.next('.select2-container').removeClass('select2-container--below').addClass('select2-container--above');
                    $dd.css('top', topPos + 'px');
                }

                // Restore table scroll position (Select2 may auto-scroll the container)
                if ($scrollBody.length && storedScrollTop !== undefined) {
                    $scrollBody.scrollTop(storedScrollTop);
                }
                if (storedWinScroll !== undefined) {
                    // Restore window scroll in case focus auto-scrolled the page
                    const currentWinScroll = $(window).scrollTop();
                    if (Math.abs(currentWinScroll - storedWinScroll) > 1) {
                        $(window).scrollTop(storedWinScroll);
                    }
                }
            });
        });

    // Cleanup stored scroll data on close to avoid stale values
    $sel.off('select2:close').on('select2:close', function() {
        $sel.removeData('scrollTopBeforeOpen');
        $sel.removeData('windowScrollBeforeOpen');
    });
    
    // Apply CSS class after select2 initialization (for color coding cells)
    const cssClass = $sel.data('cssclass');
    if (cssClass && cssClass.trim() !== "") {
        // Use setTimeout to ensure select2 container is fully rendered
        setTimeout(function() {
            $sel.next('.select2-container')
                .find('.select2-selection')
                .removeClass("red-cell blue-cell orange-cell") // Remove old classes first
                .addClass(cssClass); // Apply new class
            // Also mirror class on original select for consistency
            $sel.removeClass("red-cell blue-cell orange-cell").addClass(cssClass);
        }, 10);
    }
    
    // Re-apply CSS class after select2 opens (in case it gets reset)
    $sel.on('select2:open', function() {
        const cssClass = $(this).data('cssclass');
        if (cssClass && cssClass.trim() !== "") {
            setTimeout(function() {
                $sel.next('.select2-container')
                    .find('.select2-selection')
                    .removeClass("red-cell blue-cell orange-cell")
                    .addClass(cssClass);
                $sel.removeClass("red-cell blue-cell orange-cell").addClass(cssClass);
            }, 10);
        }
    });
}

/**
 * Initializes Select2 for all existing dropdown elements in the table
 * Called on table initialization and after table updates
 * Now also handles lazy dropdown conversion on focus
 */
function initializeSelect2Elements() {
    const domStart = performance.now();
    
    // Initialize existing select elements (if any remain from old code)
    const selects = $('#table table select.lazy-load');
    const nodeCount = selects.length;
    
    selects.each(function() {
        const $sel = $(this);
        initializeSelect2OnElement($sel);
    });
    
    const domDuration = performance.now() - domStart;
    if (performanceMonitor.enabled && nodeCount > 0) {
        performanceMonitor.trackDOMUpdate('initialize_select2', nodeCount, domDuration);
    }
    
    // Note: colResizable initialization is now handled by draw.dt event
    // to avoid duplicate initialization conflicts
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
    
    // Calculate footer height with exact measurement
    const footerHeight = $fixedFooter.length ? $fixedFooter.outerHeight(true) : 0;
    // Calculate header height with exact measurement
    const headerHeight = $fixedHeader.length ? $fixedHeader.outerHeight(true) : 0;
    
    // Apply padding to body to prevent content overlap with fixed elements
    $("body").css({
        "padding-bottom": footerHeight + "px",
        "padding-top": headerHeight + "px",
        "overflow": "hidden", // Ensure no body scrollbar
        "height": "100vh" // Full viewport height
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
    
    // Initialize table resize grip (replaces old slider)
    // Delay initialization to ensure DOM is fully ready
    setTimeout(function() {
        initTableResizeGrip();
    }, 500);
    
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
    // Also handles automatic table width adjustment when in auto-width mode
    let resizePending = false;
    $(window).on("resize", function() {
        if (!resizePending) {
            resizePending = true;
            window.requestAnimationFrame(function() {
                updateBodyPadding(); // Recalculate body padding for new window size
                
                // Auto-adjust table width if enabled AND loading screen is not active
                if (window.tableAutoWidth === true && !window.loadingScreenActive && typeof window.resizeTableToWindow === 'function') {
                    console.log('[Resize] Window resize event - calling resizeTableToWindow');
                    window.resizeTableToWindow();
                } else if (window.loadingScreenActive) {
                    console.log('[Resize] Window resize event blocked - loading screen still active');
                }
                
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
    
    $(document).on('draw.dt', '#table table', function() {
        performanceMonitor.start('table_render');
        
        // Notify that table is ready after a short delay
        setTimeout(function() {
            notifyTableReady();
            
            // End timing after render completes
            const $table = $('#table table');
            const rowCount = $table.DataTable() ? $table.DataTable().rows().count() : 0;
            performanceMonitor.end('table_render', { rows: rowCount });
            
            // Initialize column resize sync after table is ready
            initColumnResizeSync();
            
            // Initialize custom resize grips for precise column resizing
            initColumnResizeGrips();
            
            // Force info bar visibility immediately after draw
            const $infoBar = $('.dataTables_info');
            if ($infoBar.length > 0) {
                $infoBar.css({
                    'display': 'block',
                    'visibility': 'visible',
                    'opacity': '1',
                    'position': 'relative',
                    'z-index': '100'
                });
                console.log('[Init] Info bar forced visible after table draw');
            } else {
                console.warn('[Init] Info bar not found in DOM after table draw');
            }
            
            // Apply initial auto-width after table is fully rendered
            // This ensures info bar is visible from the start
            // DON'T call resizeTableToWindow here - wait for hideLoadingScreen
            // to ensure footer is fully rendered
            if (window.tableAutoWidth === true && typeof window.resizeTableToWindow === 'function') {
                console.log('[Init] Skipping auto-width after table draw - waiting for loading screen removal');
            }
        }, 100); // Reduced delay for faster initial render
    });
    
    // Fallback: If draw event never fires (edge case), try to notify anyway
    // This should only trigger if something went wrong with DataTable initialization
    // DISABLED: This fallback causes unnecessary re-renders after successful initialization
    // setTimeout(function() {
    //     // Only notify if we haven't already
    //     if (!$('#loading-screen').hasClass('fade-out')) {
    //         console.log('[Loading Screen] Fallback timeout reached, attempting to notify...');
    //         notifyTableReady();
    //     }
    // }, 5000);  // 5 seconds - give plenty of time for normal initialization
    
    // Initial setup - just update body padding without triggering full resize
    // Actual table resize happens in hideLoadingScreen before screen is removed
    setTimeout(function() {
        console.log('[Init] Updating body padding on startup');
        window.tableAutoWidth = true; // Ensure auto-width is enabled on startup
        updateBodyPadding(); // Update body padding directly without triggering resize event
    }, 300);
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

// ===== LAZY DROPDOWN CONVERSION =====
// Convert lazy-dropdown placeholders to actual Select2 dropdowns on focus/click
// This dramatically improves initial render performance by deferring dropdown creation

// Handle focus event (keyboard navigation)
$(document).on("focus", ".lazy-dropdown", function() {
    const $div = $(this);
    // Only convert if not already converted (check if still a div)
    if ($div.hasClass('lazy-dropdown')) {
        convertLazyDropdownToSelect($div);
    }
});

// Handle click event (mouse interaction)
$(document).on("click", ".lazy-dropdown", function(e) {
    const $div = $(this);
    // Only convert if not already converted
    if ($div.hasClass('lazy-dropdown')) {
        e.preventDefault(); // Prevent default click behavior
        convertLazyDropdownToSelect($div);
    }
});

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

// Checkbox click handler - immediately update selection on click (don't wait for blur)
// This ensures keyboard shortcuts work immediately after checking a box
$(document).on("click", ".delete-rows", function(e) {
    // Use setTimeout to ensure checkbox state has updated after the click
    setTimeout(function() {
        updateTableRowsSelected();
        console.log('[Checkbox] Selection updated after click');
    }, 10);
});

// Delete rows handler - collects checked rows and sends to Shiny
// Triggered when delete button is clicked
$(document).on("click", "#delete_rows", function() {
    updateTableRowsSelected();
});

// Dropdown change handler - sends dropdown value changes to Shiny
// Uses event delegation to work with both pre-existing and dynamically created dropdowns
$(document).on("change", "select.lazy-load", function() {
    const $select = $(this);
    const row = $select.data("row");
    const col = $select.data("col");
    const value = $select.val();
    
    // Send change event to Shiny server
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
        Shiny.setInputValue('dropdown_change', {
            row: row,
            col: col,
            id: this.id,
            value: value
        });
    }
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

// Block double-click on checkbox cells to prevent edit modal from opening
// The Select column contains checkboxes and should never be editable
// Use capture phase to intercept before DataTables' handler
document.addEventListener("dblclick", function(e) {
    const target = e.target;
    // Check if clicked on or inside a cell with checkbox
    const $cell = $(target).closest('td');
    if ($cell.length > 0 && $cell.find('.delete-rows').length > 0) {
        e.preventDefault();
        e.stopPropagation();
        e.stopImmediatePropagation();
        console.log('[Table] Double-click blocked on checkbox cell');
        return false;
    }
    // Also check if target is the checkbox itself
    if ($(target).hasClass('delete-rows') || $(target).closest('.delete-rows').length > 0) {
        e.preventDefault();
        e.stopPropagation();
        e.stopImmediatePropagation();
        console.log('[Table] Double-click blocked on checkbox');
        return false;
    }
}, true); // Use capture phase

// Prevent Bootstrap modal from adding padding-right to body
// This prevents the table layout from shifting when modals open/close
$(document).on('show.bs.modal', '.modal', function() {
    // Remove any padding-right that Bootstrap tries to add
    setTimeout(function() {
        $('body').css('padding-right', '0');
    }, 0);
});

$(document).on('hidden.bs.modal', '.modal', function() {
    // Ensure padding-right is removed after modal closes
    $('body').css('padding-right', '0');
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
    
// ===== TABLE AUTO-WIDTH FUNCTIONALITY =====
// Global flag to track if user has manually adjusted table width
// When false (default), table will automatically resize to fit window
// When true, table width is controlled by user's slider input
window.tableAutoWidth = true;

// Global flag to track if this is the first resize (at startup)
window.isFirstResize = true;

// Global flag to prevent resizes while loading screen is active
window.loadingScreenActive = true;

// Global function to resize table to fit window
// Can be called from window resize handler or reset button
// Debounced to prevent rapid successive calls
window.resizeTableToWindow = function() {
    if (window.tableAutoWidth !== true) {
        console.log('[Auto-Width] Disabled - user has manual width set');
        return;
    }
    
    // Debounce: prevent calls within 100ms of each other
    const now = Date.now();
    if (window.lastResizeTime && (now - window.lastResizeTime) < 100) {
        console.log('[Auto-Width] Debounced - too soon after last resize');
        return;
    }
    window.lastResizeTime = now;
    
    const mainPanel = $(".col-sm-8");
    if (mainPanel.length === 0) {
        console.warn('[Auto-Width] Main panel not found');
        return;
    }
    
    // Calculate available width (window width minus margins)
    const availableWidth = Math.max($(window).width() - 100, 800);
    const newWidth = Math.min(availableWidth, 2500); // Cap at max slider value
    
    // Calculate available height (viewport height minus header and footer)
    console.log('[Auto-Width] ===== Starting height calculation =====');
    console.log('[Auto-Width] isFirstResize:', window.isFirstResize);
    
    const $header = $('.fixed-header');
    const $footer = $('.fixed-footer');
    console.log('[Auto-Width] Header element found:', $header.length > 0);
    console.log('[Auto-Width] Footer element found:', $footer.length > 0);
    
    const fixedHeaderHeight = $header.length > 0 ? $header.outerHeight(true) : 0;
    const fixedFooterHeight = $footer.length > 0 ? $footer.outerHeight(true) : 0;
    console.log('[Auto-Width] Measured header height:', fixedHeaderHeight + 'px');
    console.log('[Auto-Width] Measured footer height:', fixedFooterHeight + 'px');
    
    // On first resize, if footer height is 0 or suspiciously small, use a safe default
    // This happens on cold starts when footer hasn't fully rendered yet
    let actualFooterHeight = fixedFooterHeight;
    if (window.isFirstResize && (fixedFooterHeight === 0 || fixedFooterHeight < 50)) {
        actualFooterHeight = 150; // Conservative default footer height for cold start
        console.warn('[Auto-Width] *** Footer not yet rendered (height:', fixedFooterHeight + 'px), using default:', actualFooterHeight + 'px ***');
    } else {
        console.log('[Auto-Width] Using measured footer height:', actualFooterHeight + 'px');
    }
    
    // Use consistent margin for all resizes
    const extraMargin = 30; // Standard margin
    
    // Don't adjust footer height on first resize - let the table be as large as possible
    // The second resize will fix any small positioning issues after footer renders
    console.log('[Auto-Width] Using measured footer height without adjustment');
    console.log('[Auto-Width] Extra margin:', extraMargin + 'px (first resize:', window.isFirstResize + ')');
    
    const windowHeight = $(window).height();
    const totalReserved = fixedHeaderHeight + actualFooterHeight + extraMargin;
    const calculatedHeight = windowHeight - totalReserved;
    const availableHeight = Math.max(calculatedHeight, 300);
    
    console.log('[Auto-Width] Window height:', windowHeight + 'px');
    console.log('[Auto-Width] Total reserved (header+footer+margin):', totalReserved + 'px');
    console.log('[Auto-Width] Calculated available height:', calculatedHeight + 'px');
    console.log('[Auto-Width] Final available height (min 300):', availableHeight + 'px');
    
    console.log('[Auto-Width] Heights - Window:', $(window).height() + 'px, Header:', fixedHeaderHeight + 'px, Footer:', fixedFooterHeight + 'px (actual:', actualFooterHeight + 'px), Extra:', extraMargin + 'px, Available:', availableHeight + 'px', 'First resize:', window.isFirstResize);
    
    // Apply the resize
    const resizeMessage = {
        width: newWidth + "px",
        height: availableHeight,
        manual: false,
        updateSlider: false
    };
    
    // Update scrollDiv dimensions directly (don't touch col-sm-8 to avoid conflicts)
    const $scrollDiv = $("#scrollDiv");
    // Don't set explicit width, let it be 100% of parent
    $scrollDiv.css({
        'width': '100%',
        'overflow-x': 'visible',
        'overflow-y': 'visible'
    });
    
    // Get both header and body tables
    const $scrollWrapper = $(".dataTables_scroll");
    const $headerTable = $scrollWrapper.find(".dataTables_scrollHead table");
    const $bodyTable = $scrollWrapper.find(".dataTables_scrollBody table");
    
    if ($headerTable.length === 0 && $bodyTable.length === 0) {
        console.warn('[Auto-Width] No DataTables found - skipping resize (isFirstResize remains:', window.isFirstResize + ')');
        return;
    }
    
    // Mark that we've done the first SUCCESSFUL resize (only if DataTables exists)
    console.log('[Auto-Width] DataTables found, proceeding with resize and marking first resize as complete');
    window.isFirstResize = false;
    
    // Set table widths - ensure header and body are perfectly aligned
    const tableWidth = parseInt(resizeMessage.width);
    const tableHeight = resizeMessage.height;
    
    // Get scroll wrappers first
    let $scrollHead = $('.dataTables_scrollHead');
    let $scrollBody = $('.dataTables_scrollBody');
    
    // Set exact same width on ALL elements to ensure perfect alignment
    const widthStyle = tableWidth + 'px';
    
    // Set wrapper widths (containers)
    if ($scrollHead.length > 0) {
        $scrollHead.css({
            'width': widthStyle,
            'overflow': 'hidden'
        });
    }
    if ($scrollBody.length > 0) {
        $scrollBody.css({
            'width': widthStyle
        });
    }
    
    // Set table widths (content) - use fixed layout for consistent column distribution
    $headerTable.css({
        'width': widthStyle,
        'table-layout': 'fixed' // Fixed layout for equal distribution
    });
    $bodyTable.css({
        'width': widthStyle,
        'table-layout': 'fixed'
    });
    
    // Calculate and set column widths for even distribution
    const stickyWidth = STICKY_COLUMNS_WIDTH;
    const colCount = $headerTable.find("th").length || $bodyTable.find("th").length;
    
    if (colCount > 2) {
        // We have 2 sticky columns: Index (first) and Select (last)
        const availableWidth = tableWidth - (parseInt(stickyWidth) * 2);
        const middleColWidth = Math.floor(availableWidth / (colCount - 2));
        
        console.log('[Auto-Width] Distributing columns: sticky=' + stickyWidth + ' (x2), middle=' + middleColWidth + 'px, count=' + colCount);
        
        // Apply widths to header
        $headerTable.find("th").each(function(index) {
            const $th = $(this);
            // Sticky columns: first (Index) and last (Select)
            if (index === 0 || index === colCount - 1) {
                // Sticky columns - fixed width, cannot be resized
                $th.css({
                    "width": stickyWidth,
                    "min-width": stickyWidth,
                    "max-width": stickyWidth
                });
            } else {
                // Middle columns - equal width, can be resized
                $th.css({
                    "width": middleColWidth + "px",
                    "min-width": "50px" // Allow resizing down to 50px
                });
            }
        });
        
        // Apply same widths to body
        $bodyTable.find("tr:first td").each(function(index) {
            const $td = $(this);
            // Sticky columns: first (Index) and last (Select)
            if (index === 0 || index === colCount - 1) {
                // Sticky columns - fixed width
                $td.css({
                    "width": stickyWidth,
                    "min-width": stickyWidth,
                    "max-width": stickyWidth
                });
            } else {
                // Middle columns - equal width, can be resized
                $td.css({
                    "width": middleColWidth + "px",
                    "min-width": "50px" // Allow resizing down to 50px
                });
            }
        });
    }
    
    // Update vertical scroll height for DataTables Scroller
    // Reuse existing selectors
    const $datatablesWrapper = $('.dataTables_wrapper');
    const $mainPanel = $('.col-sm-8');
    
    if ($scrollBody.length > 0) {
        // Calculate height for scroll body
        // Reserve space for: header (at top) + info bar (at bottom) + margins
        const tableHeaderHeight = $scrollHead.length > 0 ? $scrollHead.outerHeight(true) : 0;
        const infoBarSpace = 40; // Space for info bar
        const reservedSpace = tableHeaderHeight + infoBarSpace;
        const scrollBodyHeight = Math.max(resizeMessage.height - reservedSpace, 250);
        
        // Set wrapper height to contain everything
        if ($datatablesWrapper.length > 0) {
            $datatablesWrapper.css({
                'height': 'auto',
                'max-height': 'none'
            });
        }
        
        // Ensure main panel can show the info bar
        if ($mainPanel.length > 0) {
            $mainPanel.css({
                'position': 'relative',
                'padding-bottom': '60px', // Extra space for info bar
                'min-height': (resizeMessage.height + 60) + 'px' // Ensure enough height
            });
        }
        
        $scrollBody.css({
            'height': scrollBodyHeight + 'px',
            'max-height': scrollBodyHeight + 'px',
            'overflow-y': 'auto',
            'overflow-x': 'hidden'
        });
        
        // IMPORTANT: Calculate scrollbar width AFTER setting height
        // The scrollbar only appears after the height constraint is applied
        // Use requestAnimationFrame twice to ensure browser has fully rendered
        requestAnimationFrame(function() {
            requestAnimationFrame(function() {
                let scrollbarWidth = 0;
                if ($scrollBody.length > 0) {
                    const bodyElement = $scrollBody[0];
                    scrollbarWidth = bodyElement.offsetWidth - bodyElement.clientWidth;
                    console.log('[Auto-Width] Detected scrollbar width after height set:', scrollbarWidth + 'px');
                }
                
                // Add padding to header to compensate for scrollbar in body
                if (scrollbarWidth > 0 && $scrollHead.length > 0) {
                    $scrollHead.css({
                        'padding-right': scrollbarWidth + 'px',
                        'box-sizing': 'content-box' // Padding adds to width, not included in it
                    });
                    console.log('[Auto-Width] Added header padding for scrollbar:', scrollbarWidth + 'px');
                } else if ($scrollHead.length > 0) {
                    // No scrollbar - remove any previous padding
                    $scrollHead.css({
                        'padding-right': '0px',
                        'box-sizing': 'border-box'
                    });
                }
                
                // Column widths are already set above, no need to reapply here
            });
        });
        
        // Ensure info bar is visible and positioned correctly
        const $infoBar = $('.dataTables_info');
        if ($infoBar.length > 0) {
            $infoBar.css({
                'display': 'block',
                'visibility': 'visible',
                'position': 'relative',
                'z-index': '100'
            });
            console.log('[Auto-Width] Info bar found and made visible');
        } else {
            console.warn('[Auto-Width] Info bar not found in DOM');
        }
        
        console.log('[Auto-Width] Scroll height:', scrollBodyHeight + 'px', '| Total:', resizeMessage.height + 'px', '| Header:', tableHeaderHeight + 'px', '| InfoBar space:', infoBarSpace + 'px');
        console.log('[Auto-Width] Header width:', $headerTable.width() + 'px', '| Body width:', $bodyTable.width() + 'px', '| ScrollHead:', $scrollHead.width() + 'px');
    } else {
        console.warn('[Auto-Width] No scrollBody found');
    }
};

// Resize div and adjust table column widths
// Called automatically on window resize (no longer uses slider)
try {
    Shiny.addCustomMessageHandler("resizeDiv", function(message) {
        // If this is a manual adjustment, disable auto-width mode
        if (message.manual === true) {
            window.tableAutoWidth = false;
        }
        
        // Adjust div size
        const div = document.querySelector(".col-sm-8");
        if (!div) {
            console.warn("[resizeDiv] .col-sm-8 element not found");
            return;
        }
        div.style.width = message.width;
        div.style.height = message.height;
        
        // Note: No longer update slider input as it has been removed
        // Users now use the resize grip to manually adjust table width

        // Adjust container width
        $("#scrollDiv").width(message.width);
        
        // Get both header and body tables (DataTables Scroller creates separate tables)
        const $scrollWrapper = $(".dataTables_scroll");
        const $headerTable = $scrollWrapper.find(".dataTables_scrollHead table");
        const $bodyTable = $scrollWrapper.find(".dataTables_scrollBody table");
        
        if ($headerTable.length === 0 && $bodyTable.length === 0) {
            console.warn("[resizeDiv] No DataTables found");
            return;
        }
        
        // Set table widths - ensure perfect alignment
        const tableWidth = parseInt(message.width);
        const widthStyle = tableWidth + 'px';
        
        // Get scroll wrappers
        const $scrollHead = $('.dataTables_scrollHead');
        const $scrollBody = $('.dataTables_scrollBody');
        
        // Set exact same width on ALL elements
        if ($scrollHead.length > 0) {
            $scrollHead.css({
                'width': widthStyle,
                'overflow': 'hidden'
            });
        }
        if ($scrollBody.length > 0) {
            $scrollBody.css('width', widthStyle);
        }
        
        // Set table widths - let DataTables handle column distribution
        $headerTable.css({
            'width': widthStyle,
            'table-layout': 'auto'
        });
        $bodyTable.css({
            'width': widthStyle,
            'table-layout': 'auto'
        });
        
        // Calculate column widths
        const stickyWidth = (typeof STICKY_COLUMNS_WIDTH !== 'undefined') ? STICKY_COLUMNS_WIDTH : "55px";
        const colCount = $headerTable.find("th").length || $bodyTable.find("th").length;
        
        if (colCount > 2) {
            const availableWidth = tableWidth - (parseInt(stickyWidth) * 2); // subtract sticky columns
            const middleColWidth = Math.floor(availableWidth / (colCount - 2));
            
            // Apply widths to header
            $headerTable.find("th").each(function(index) {
                const $th = $(this);
                if (index === 0 || index === colCount - 1) {
                    // Sticky columns
                    $th.css({
                        "width": stickyWidth,
                        "min-width": stickyWidth,
                        "max-width": stickyWidth
                    });
                } else {
                    // Middle columns
                    $th.css("width", middleColWidth + "px");
                }
            });
            
            // Apply same widths to body
            $bodyTable.find("tr:first td").each(function(index) {
                const $td = $(this);
                if (index === 0 || index === colCount - 1) {
                    // Sticky columns
                    $td.css({
                        "width": stickyWidth,
                        "min-width": stickyWidth,
                        "max-width": stickyWidth
                    });
                } else {
                    // Middle columns
                    $td.css("width", middleColWidth + "px");
                }
            });
        }
        
        // Update scrollDiv - let it be full width
        const totalHeight = parseInt(message.height);
        $("#scrollDiv").css({
            'width': '100%',
            'overflow-x': 'visible',
            'overflow-y': 'visible'
        });
        
        // Update vertical scroll height for DataTables
        const $datatablesWrapper = $('.dataTables_wrapper');
        const $mainPanel = $('.col-sm-8');
        
        if ($scrollBody.length > 0) {
            const tableHeaderHeight = $scrollHead.length > 0 ? $scrollHead.outerHeight(true) : 0;
            const infoBarSpace = 40;
            const reservedSpace = tableHeaderHeight + infoBarSpace;
            const scrollBodyHeight = Math.max(totalHeight - reservedSpace, 250);
            
            // Set wrapper height
            if ($datatablesWrapper.length > 0) {
                $datatablesWrapper.css({
                    'height': 'auto',
                    'max-height': 'none'
                });
            }
            
            // Ensure main panel can show info bar
            if ($mainPanel.length > 0) {
                $mainPanel.css({
                    'position': 'relative',
                    'padding-bottom': '60px',
                    'min-height': (totalHeight + 60) + 'px'
                });
            }
            
            $scrollBody.css({
                'height': scrollBodyHeight + 'px',
                'max-height': scrollBodyHeight + 'px',
                'overflow-y': 'auto',
                'overflow-x': 'hidden'
            });
            
            // Ensure info bar visibility
            const $infoBar = $('.dataTables_info');
            if ($infoBar.length > 0) {
                $infoBar.css({
                    'display': 'block',
                    'visibility': 'visible',
                    'position': 'relative',
                    'z-index': '100'
                });
            }
        }
        
        $("#scrollDiv").css("overflow-x", "visible");
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

// Adjust DataTables Scroller (for tab switches and visibility changes)
Shiny.addCustomMessageHandler("adjustScroller", function(message) {
    const $table = $("#table table");
    if ($table.length && $.fn.DataTable && $.fn.DataTable.isDataTable($table)) {
        const dt = $table.DataTable();
        if (dt.scroller) {
            // Scroll to top first, then recalculate viewport
            dt.scroller.toPosition(0);
            dt.scroller.measure();
            dt.draw(false); // Redraw without resetting paging
            
            // Also reset the scroll container to the top
            const $scrollBody = $table.closest('.dataTables_scrollBody');
            if ($scrollBody.length) {
                $scrollBody.scrollTop(0);
            }

            // End tab switch timing if it was started
            // adjustScroller is called at the end of tab switches
            if (performanceMonitor.timers['tab_switch']) {
                const rowCount = dt.rows().count();
                performanceMonitor.end('tab_switch', { rows: rowCount });
            }
        }
    }
});

// Loading indicator handlers (Step 3)
Shiny.addCustomMessageHandler("showTableLoading", function(message) {
    showTableLoading();
});

Shiny.addCustomMessageHandler("hideTableLoading", function(message) {
    hideTableLoading();
});

Shiny.addCustomMessageHandler("showButtonSpinner", function(message) {
    if (message.buttonId) {
        showButtonSpinner(message.buttonId);
    }
});

Shiny.addCustomMessageHandler("hideButtonSpinner", function(message) {
    if (message.buttonId) {
        hideButtonSpinner(message.buttonId);
    }
});

// Update specific table rows (Step 4 - row-level updates for performance)
Shiny.addCustomMessageHandler("updateTableRows", function(message) {
    performanceMonitor.start('update_table_rows');
    
    if (!message.rows || !message.data) {
        console.error('[updateTableRows] Invalid message format');
        performanceMonitor.end('update_table_rows', { error: 'Invalid format' });
        return;
    }
    
    const $table = $("#table table");
    if ($table.length && $.fn.DataTable && $.fn.DataTable.isDataTable($table)) {
        const dt = $table.DataTable();
        const rowIndices = Array.isArray(message.rows) ? message.rows : [message.rows];
        
        // Update each specified row
        rowIndices.forEach((rowIndex, i) => {
            const rowData = message.data[i];
            if (rowData && rowIndex >= 0 && rowIndex < dt.data().count()) {
                // Update the row data in DataTable
                dt.row(rowIndex).data(rowData).invalidate();
            }
        });
        
        // Redraw only the affected rows (no full table redraw)
        dt.draw('page');
        
        // Reinitialize Select2 for updated rows
        setTimeout(function() {
            $(document).trigger("initializeSelect2");
            performanceMonitor.end('update_table_rows', { rowCount: rowIndices.length });
        }, 50);
    } else {
        performanceMonitor.end('update_table_rows', { error: 'Table not found' });
    }
});

// Update cell value directly (for manual value addition via modal)
Shiny.addCustomMessageHandler("updateCellValue", function(message) {
    const row = message.row;
    const col = message.col;
    const value = message.value;
    const cssClass = message.cssClass || '';
    
    // Try to find the select element
    const selectId = 'dropdown_' + col + '_' + row;
    let $element = $('#' + selectId);
    
    if ($element.length > 0 && $element.is('select')) {
        // It's already a select element
        // Add the new value as an option if it doesn't exist
        if ($element.find('option[value="' + value + '"]').length === 0) {
            // Insert new option before "__ADD__"
            const $addOption = $element.find('option[value="__ADD__"]');
            const newOption = '<option value="' + value + '">' + value + '</option>';
            if ($addOption.length > 0) {
                $addOption.before(newOption);
            } else {
                $element.append(newOption);
            }
        }
        
        // Set the value
        $element.val(value);
        
        // Update Select2 if initialized
        if ($element.hasClass('select2-hidden-accessible')) {
            $element.trigger('change.select2');
            
            // Update CSS class on Select2 container
            const $container = $element.next('.select2-container');
            if ($container.length > 0) {
                const $selection = $container.find('.select2-selection');
                $selection.removeClass('red-cell blue-cell orange-cell');
                if (cssClass) {
                    $selection.addClass(cssClass);
                }
            }
        }
    } else if ($element.length > 0 && $element.hasClass('lazy-dropdown')) {
        // It's still a lazy-dropdown div - just update the text and data attributes
        $element.text(value);
        $element.attr('data-value', value);
        
        // Update CSS class
        $element.removeClass('red-cell blue-cell orange-cell');
        if (cssClass) {
            $element.addClass(cssClass);
        }
        
        // Update options in data-options attribute
        let options = [];
        try {
            const optionsAttr = $element.attr('data-options');
            const optionsJson = optionsAttr.replace(/&quot;/g, '"').replace(/&#39;/g, "'");
            options = JSON.parse(optionsJson);
            
            // Add new value if not already in list
            if (!options.includes(value)) {
                options.push(value);
                const newOptionsJson = JSON.stringify(options);
                const newOptionsEscaped = newOptionsJson.replace(/'/g, "&#39;").replace(/"/g, "&quot;");
                $element.attr('data-options', newOptionsEscaped);
            }
        } catch (e) {
            console.error('Failed to update options:', e);
        }
    }
});

// Update duplicate row highlighting (Step 3 - Duplicate Detection Feature)
// Applies .duplicate-row class to rows with duplicate Element or castor_kolom values
Shiny.addCustomMessageHandler("updateDuplicateRows", function(message) {
    performanceMonitor.start('update_duplicate_rows');
    
    const $table = $("#table table");
    if ($table.length && $.fn.DataTable && $.fn.DataTable.isDataTable($table)) {
        const dt = $table.DataTable();
        const duplicateIndices = message.duplicateIndices || [];
        
        // Remove duplicate-row class from all rows first
        $table.find('tbody tr').removeClass('duplicate-row');
        
        // Add duplicate-row class to specified rows
        if (duplicateIndices.length > 0) {
            duplicateIndices.forEach(function(rowIndex) {
                // Get the row element (rowIndex is 0-based, matches DataTables)
                const $row = $table.find('tbody tr').eq(rowIndex);
                if ($row.length > 0) {
                    $row.addClass('duplicate-row');
                }
            });
            
            console.log('[updateDuplicateRows] Applied duplicate-row class to', duplicateIndices.length, 'rows');
        } else {
            console.log('[updateDuplicateRows] No duplicate rows to highlight');
        }
        
        performanceMonitor.end('update_duplicate_rows', { 
            duplicateCount: duplicateIndices.length 
        });
    } else {
        console.warn('[updateDuplicateRows] Table not found or not initialized');
        performanceMonitor.end('update_duplicate_rows', { error: 'Table not found' });
    }
});

// Listen for table updates to reinitialize Select2
// When Shiny updates the table, dropdowns need to be reinitialized
$(document).on('shiny:value', function(event) {
    if (event.name === 'table') {
        setTimeout(function() {
            $(document).trigger("initializeSelect2");
            
            // Adjust DataTables Scroller after table update (important for tab switches)
            const $table = $("#table table");
            if ($table.length && $.fn.DataTable && $.fn.DataTable.isDataTable($table)) {
                const dt = $table.DataTable();
                if (dt.scroller) {
                    // Force Scroller to recalculate viewport and row positions
                    dt.scroller.measure();
                    dt.draw(false); // Redraw without resetting paging
                }
            }
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
            
            // Start timing tab switch
            performanceMonitor.start('tab_switch');
            
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

// ===== KEYBOARD SHORTCUTS INITIALIZATION =====
/**
 * Initialize keyboard shortcuts system
 * - Loads configuration from keyboard_shortcuts.json
 * - Registers all shortcut callbacks
 * - Connects shortcuts to existing UI buttons
 */
$(document).ready(function() {
    console.log('[Shortcuts] Document ready, checking for ShortcutHandler...');
    
    // Wait for ShortcutHandler to be available
    if (typeof ShortcutHandler === 'undefined') {
        console.error('[Shortcuts] ShortcutHandler not loaded! Check if shortcutHandler.js is loaded.');
        return;
    }
    
    console.log('[Shortcuts] ShortcutHandler found, loading configuration...');
    
    // Load keyboard shortcuts configuration
    fetch('keyboard_shortcuts.json')
        .then(response => response.json())
        .then(config => {
            console.log('[Shortcuts] Configuration loaded:', config);
            
            // Initialize the shortcut handler
            const handler = new ShortcutHandler(config);
            handler.debugMode = false; // Set to true for debugging
            
            // Make handler globally accessible for debugging
            window.shortcutHandler = handler;
            
            // ===== CLIPBOARD SHORTCUTS =====
            // Trigger copy/cut/paste by simulating button clicks via Shiny
            
            handler.register('clipboard', 'copy', () => {
                console.log('[Shortcuts] Copy triggered');
                
                // Check if we have selected rows
                const selectedCount = $('.delete-rows:checked').length;
                console.log('[Shortcuts] Checked boxes count:', selectedCount);
                
                if (selectedCount === 0) {
                    showShortcutToast('⚠️ Select rows first', 2000);
                    return;
                }
                
                // Collect selected row indices SYNCHRONOUSLY
                const selected = [];
                $('.delete-rows:checked').each(function() {
                    const id = $(this).attr('id');
                    const rowNum = id.replace('deleterows_', '');
                    selected.push(rowNum);
                    console.log('[Shortcuts] Found checked box:', id, '-> row', rowNum);
                });
                
                console.log('[Shortcuts] Selected rows array:', selected);
                console.log('[Shortcuts] Current table_rows_selected:', Shiny.shinyapp.$inputValues.table_rows_selected);
                
                if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
                    // Set selection first with high priority to ensure it's processed
                    console.log('[Shortcuts] Setting table_rows_selected to:', selected);
                    Shiny.setInputValue('table_rows_selected', selected, {priority: 'event'});
                    
                    // Trigger copy with a delay to ensure selection was processed
                    setTimeout(() => {
                        console.log('[Shortcuts] Triggering copy_rows');
                        console.log('[Shortcuts] table_rows_selected at copy time:', Shiny.shinyapp.$inputValues.table_rows_selected);
                        Shiny.setInputValue('copy_rows', Math.random(), {priority: 'event'});
                        showShortcutToast(`Copied ${selectedCount} row(s)`, 2000);
                    }, 150);
                } else {
                    console.error('[Shortcuts] Shiny not available');
                }
            });
            
            handler.register('clipboard', 'cut', () => {
                console.log('[Shortcuts] Cut triggered');
                
                // Check if we have selected rows
                const selectedCount = $('.delete-rows:checked').length;
                
                if (selectedCount === 0) {
                    showShortcutToast('⚠️ Select rows first', 2000);
                    return;
                }
                
                // Collect selected row indices SYNCHRONOUSLY
                const selected = [];
                $('.delete-rows:checked').each(function() {
                    selected.push($(this).attr('id').replace('deleterows_', ''));
                });
                
                console.log('[Shortcuts] Selected rows:', selected);
                
                if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
                    // Set selection first with high priority to ensure it's processed
                    Shiny.setInputValue('table_rows_selected', selected, {priority: 'event'});
                    
                    // Trigger cut with a delay to ensure selection was processed
                    setTimeout(() => {
                        Shiny.setInputValue('cut_rows', Math.random(), {priority: 'event'});
                        showShortcutToast(`Cut ${selectedCount} row(s)`, 2000);
                    }, 150);
                } else {
                    console.error('[Shortcuts] Shiny not available');
                }
            });
            
            handler.register('clipboard', 'paste', () => {
                console.log('[Shortcuts] Paste triggered');
                
                // Trigger the paste_rows button via Shiny
                if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
                    Shiny.setInputValue('paste_rows', Math.random(), {priority: 'event'});
                } else {
                    console.error('[Shortcuts] Shiny not available');
                }
            });
            
            // ===== DATA SHORTCUTS =====
            handler.register('data', 'save', () => {
                console.log('[Shortcuts] Save triggered');
                
                // Trigger the save action via Shiny (File menu > Save changes)
                if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
                    Shiny.setInputValue('save', Math.random(), {priority: 'event'});
                    showShortcutToast('💾 Saving changes...', 2000);
                } else {
                    console.error('[Shortcuts] Shiny not available');
                }
            });
            
            // ===== NAVIGATION SHORTCUTS =====
            handler.register('navigation', 'search', () => {
                console.log('[Shortcuts] Search triggered');
                
                setTimeout(() => {
                    // Find inputs with 'search' in class or id
                    const searchInputs = [];
                    $('input').each(function(i, el) {
                        const $el = $(el);
                        const id = ($el.attr('id') || '').toLowerCase();
                        const cls = ($el.attr('class') || '').toLowerCase();
                        if (id.includes('search') || cls.includes('search')) {
                            searchInputs.push($el);
                        }
                    });
                    
                    // Use first found search input
                    let searchBox = searchInputs.length > 0 ? searchInputs[0] : null;
                    
                    // Fallback: Try standard DataTables selectors
                    if (!searchBox || searchBox.length === 0) {
                        searchBox = $('.dataTables_filter input');
                    }
                    if (!searchBox || searchBox.length === 0) {
                        searchBox = $('input[aria-controls="table"]');
                    }
                    
                    if (searchBox && searchBox.length > 0) {
                        console.log('[Shortcuts] Search box focused');
                        searchBox.first().focus().select();
                    } else {
                        console.log('[Shortcuts] Search box not found');
                    }
                }, 150);
            });
            
            handler.register('navigation', 'next_tab', () => {
                // Navigate to next tab
                const tabs = $('.tab-button');
                const activeTab = $('.tab-button.active');
                if (tabs.length > 0 && activeTab.length > 0) {
                    const currentIndex = tabs.index(activeTab);
                    const nextIndex = (currentIndex + 1) % tabs.length;
                    console.log('[Shortcuts] Next tab:', nextIndex);
                    tabs.eq(nextIndex).click();
                } else {
                    console.log('[Shortcuts] No tabs found or no active tab');
                }
            });
            
            handler.register('navigation', 'prev_tab', () => {
                // Navigate to previous tab
                const tabs = $('.tab-button');
                const activeTab = $('.tab-button.active');
                if (tabs.length > 0 && activeTab.length > 0) {
                    const currentIndex = tabs.index(activeTab);
                    const prevIndex = (currentIndex - 1 + tabs.length) % tabs.length;
                    console.log('[Shortcuts] Previous tab:', prevIndex);
                    tabs.eq(prevIndex).click();
                } else {
                    console.log('[Shortcuts] No tabs found or no active tab');
                }
            });
            
            handler.register('navigation', 'clear_selection', () => {
                console.log('[Shortcuts] Clear selection triggered');
                
                setTimeout(() => {
                    // Clear checkbox selections (app uses checkboxes, not DataTable selection)
                    const checkedBoxes = $('.delete-rows:checked');
                    const count = checkedBoxes.length;
                    
                    if (count > 0) {
                        checkedBoxes.prop('checked', false);
                        // Update Shiny with empty selection
                        if (typeof Shiny !== 'undefined') {
                            Shiny.setInputValue('table_rows_selected', []);
                        }
                        console.log('[Shortcuts] Cleared', count, 'checkboxes');
                        showShortcutToast(`Cleared ${count} selection(s)`, 2000);
                    } else {
                        console.log('[Shortcuts] No checkboxes selected');
                    }
                }, 50);
            });
            
            // ===== UI SHORTCUTS =====
            handler.register('ui', 'help', () => {
                // Open keyboard shortcuts help modal
                const helpButton = $('#show_shortcuts');
                if (helpButton.length > 0) {
                    console.log('[Shortcuts] Opening help modal');
                    helpButton.click();
                } else {
                    console.log('[Shortcuts] Help button not found');
                }
            });
            
            handler.register('ui', 'refresh', () => {
                console.log('[Shortcuts] Refresh triggered');
                // F5 refresh - trigger Shiny to re-render current tab without full page reload
                showShortcutToast('Refreshing current tab...', 1000);
                
                // Send refresh event to Shiny server
                if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
                    Shiny.setInputValue('keyboard_refresh', Math.random(), {priority: 'event'});
                }
            });
            
            console.log('[Shortcuts] ✓ All shortcuts registered successfully');
            
            // IMPORTANT: Enable the handler to start listening for keyboard events
            handler.enable();
            console.log('[Shortcuts] ✓ Keyboard shortcuts enabled');
            console.log('[Shortcuts] Available:', 
                'Ctrl+Left/Right (tabs)', 
                'Ctrl+F (search)', 
                'Ctrl+C/X/V (copy/cut/paste)',
                'Ctrl+S (save)',
                'Escape (clear)',
                'F1 (help)',
                'F5 (refresh)');
            
            // Add convenience functions to window for debugging
            window.shortcutStats = () => handler.performanceMonitor.logStats();
            window.shortcutReset = () => {
                handler.performanceMonitor.reset();
                console.log('[Shortcuts] Performance metrics reset');
            };
            window.shortcutTop = (n = 5) => {
                console.log('[Shortcuts] Top shortcuts:');
                handler.performanceMonitor.getTopShortcuts(n).forEach(s => {
                    console.log(`  ${s.category}.${s.action}: ${s.count}x`);
                });
            };
            
            // Add performance monitoring debug commands
            window.perfStats = () => performanceMonitor.logSummary();
            window.perfReset = () => performanceMonitor.reset();
            window.perfMemory = () => {
                const mem = performanceMonitor.getMemoryUsage();
                if (mem) {
                    console.log('💾 Memory Usage:', mem);
                } else {
                    console.log('⚠️ Memory API not available (try Chrome with --enable-precise-memory-info)');
                }
            };
            window.perfTables = () => {
                const renders = performanceMonitor.appMetrics.tableRenders;
                console.log(`📊 Table Renders (${renders.length}):`);
                renders.forEach(r => {
                    console.log(`  ${r.tableName}: ${r.rowCount} rows in ${r.duration.toFixed(2)}ms (${r.rowsPerMs.toFixed(2)} rows/ms)`);
                });
            };
            window.perfDOM = () => {
                const updates = performanceMonitor.appMetrics.domUpdates;
                console.log(`🔄 DOM Updates (${updates.length}):`);
                updates.forEach(u => {
                    console.log(`  ${u.operation}: ${u.nodeCount} nodes in ${u.duration.toFixed(2)}ms (${u.nodesPerMs.toFixed(2)} nodes/ms)`);
                });
            };
            
            console.log('[Shortcuts] Debug commands: shortcutStats(), shortcutTop(), shortcutReset()');
            console.log('[Performance] Debug commands: perfStats(), perfReset(), perfMemory(), perfTables(), perfDOM()');
        })
        .catch(error => {
            console.error('[Shortcuts] Failed to load configuration:', error);
        });
});

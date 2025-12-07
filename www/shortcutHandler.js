// ============================================================================
// KEYBOARD SHORTCUTS HANDLER
// ============================================================================
// Centralized keyboard shortcut management system for Epic2Castor
// Provides context-aware shortcut detection and execution
//
// Features:
// - Platform detection (Windows/Mac)
// - Context-aware execution (modal, table, scope checking)
// - Configurable via JSON
// - Debug mode for troubleshooting
// - Internationalization support
// - Alternative key bindings
//
// Usage:
//   const handler = new ShortcutHandler(config);
//   handler.enable();
// ============================================================================

/**
 * Shortcut Handler Class
 * Manages keyboard shortcut registration, detection, and execution
 */
class ShortcutHandler {
    /**
     * Initialize shortcut handler
     * @param {Object} config - Configuration object from keyboard_shortcuts.json
     */
    constructor(config) {
        console.log('[ShortcutHandler] Constructor called with config:', config);
        
        this.config = config;
        this.enabled = false; // Start disabled, will be enabled explicitly
        this.locale = config.locale || this.detectLocale();
        this.translations = config.translations?.[this.locale] || config.translations?.['en'] || {};
        this.listeners = new Map(); // category.action -> callback
        this.debugMode = false;
        this.boundHandleKeyDown = this.handleKeyDown.bind(this);
        this.performanceMonitor = new ShortcutPerformanceMonitor();
        
        console.log('[ShortcutHandler] Initialized successfully with performance monitoring');
    }
    
    /**
     * Detect browser locale
     * @returns {string} Locale code (e.g., 'nl', 'en')
     */
    detectLocale() {
        const browserLang = navigator.language.split('-')[0];
        const supported = ['en', 'nl'];
        return supported.includes(browserLang) ? browserLang : 'en';
    }
    
    /**
     * Parse shortcut string into object
     * Converts "Ctrl+C" into {ctrl: true, shift: false, alt: false, key: 'c'}
     * 
     * @param {string} shortcutString - String like "Ctrl+C", "Ctrl+Shift+S"
     * @returns {Object} Parsed shortcut object
     */
    parseShortcut(shortcutString) {
        if (!shortcutString) return null;
        
        const parts = shortcutString.split('+');
        const parsed = {
            ctrl: false,
            shift: false,
            alt: false,
            meta: false,
            key: ''
        };
        
        parts.forEach(part => {
            const lower = part.toLowerCase();
            if (lower === 'ctrl' || lower === 'control') {
                parsed.ctrl = true;
            } else if (lower === 'shift') {
                parsed.shift = true;
            } else if (lower === 'alt') {
                parsed.alt = true;
            } else if (lower === 'cmd' || lower === 'command' || lower === 'meta') {
                parsed.meta = true;
            } else {
                // This is the actual key
                parsed.key = part.toLowerCase();
            }
        });
        
        return parsed;
    }
    
    /**
     * Check if keyboard event matches shortcut definition
     * 
     * @param {KeyboardEvent} event - Browser keyboard event
     * @param {Object} shortcut - Parsed shortcut object
     * @returns {boolean} True if event matches shortcut
     */
    matchesShortcut(event, shortcut) {
        if (!shortcut) return false;
        
        // Normalize the event key
        const eventKey = event.key.toLowerCase();
        const shortcutKey = shortcut.key.toLowerCase();
        
        // Check modifiers
        const ctrlMatch = shortcut.ctrl === (event.ctrlKey || event.metaKey);
        const shiftMatch = shortcut.shift === event.shiftKey;
        const altMatch = shortcut.alt === event.altKey;
        
        // Check key - handle special cases
        let keyMatch = false;
        if (shortcutKey === 'escape' || shortcutKey === 'esc') {
            keyMatch = eventKey === 'escape';
        } else if (shortcutKey === 'arrowleft' || shortcutKey === 'left') {
            keyMatch = eventKey === 'arrowleft';
        } else if (shortcutKey === 'arrowright' || shortcutKey === 'right') {
            keyMatch = eventKey === 'arrowright';
        } else if (shortcutKey === 'arrowup' || shortcutKey === 'up') {
            keyMatch = eventKey === 'arrowup';
        } else if (shortcutKey === 'arrowdown' || shortcutKey === 'down') {
            keyMatch = eventKey === 'arrowdown';
        } else if (shortcutKey === 'insert') {
            keyMatch = eventKey === 'insert';
        } else {
            keyMatch = eventKey === shortcutKey;
        }
        
        const matches = ctrlMatch && shiftMatch && altMatch && keyMatch;
        
        if (this.debugMode && matches) {
            console.log('[ShortcutHandler] Match found:', {
                event: {
                    key: event.key,
                    ctrl: event.ctrlKey,
                    shift: event.shiftKey,
                    alt: event.altKey
                },
                shortcut: shortcut
            });
        }
        
        return matches;
    }
    
    /**
     * Register a callback for a specific shortcut
     * 
     * @param {string} category - Category name (e.g., 'clipboard', 'data')
     * @param {string} action - Action name (e.g., 'copy', 'save')
     * @param {Function} callback - Function to call when shortcut triggered
     */
    register(category, action, callback) {
        const key = `${category}.${action}`;
        this.listeners.set(key, callback);
        
        if (this.debugMode) {
            const shortcut = this.config.shortcuts[category]?.[action];
            console.log(`[ShortcutHandler] ✓ Registered: ${key} -> ${shortcut?.keys}`);
        }
    }
    
    /**
     * Get shortcut configuration for display
     * 
     * @param {string} category - Category name
     * @param {string} action - Action name
     * @returns {string} Formatted shortcut string (e.g., "Ctrl+C" or "⌘+C" on Mac)
     */
    getShortcutForAction(category, action) {
        const shortcut = this.config.shortcuts[category]?.[action];
        if (!shortcut) return '';
        
        // Use Mac keys if on Mac platform
        const keys = PlatformHelper.isMac() && shortcut.keys_mac 
            ? shortcut.keys_mac 
            : shortcut.keys;
        
        return PlatformHelper.displayShortcut(keys);
    }
    
    /**
     * Main keyboard event handler
     * Checks context, finds matching shortcuts, and executes callbacks
     * 
     * @param {KeyboardEvent} event - Browser keyboard event
     */
    handleKeyDown(event) {
        const handlerStart = performance.now();
        
        if (this.debugMode) {
            console.log('[ShortcutHandler] Key pressed:', event.key, 'Ctrl:', event.ctrlKey, 'Alt:', event.altKey, 'Shift:', event.shiftKey);
        }
        
        if (!this.enabled) {
            if (this.debugMode) {
                console.log('[ShortcutHandler] Handler is disabled');
            }
            return;
        }
        
        // Allow Escape to work in modals
        if (event.key === 'Escape' && ContextManager.isModalOpen()) {
            // Let modal handle escape
            if (this.debugMode) {
                console.log('[ShortcutHandler] Escape in modal - allowing default');
            }
            return;
        }
        
        // Block all other shortcuts when modal is open
        if (ContextManager.isModalOpen()) {
            if (this.debugMode) {
                console.log('[ShortcutHandler] Blocked: Modal is open');
            }
            return;
        }
        
        // Block shortcuts when typing in input fields (except specific shortcuts)
        if (this.isTypingInInput(event.target)) {
            // Allow Escape, F1, F5, and Ctrl+S even in input fields
            const allowedKeys = ['Escape', 'F1', 'F5'];
            const isCtrlS = event.ctrlKey && event.key.toLowerCase() === 's';
            const isAllowedKey = allowedKeys.includes(event.key);
            
            if (isAllowedKey || isCtrlS) {
                // Continue to shortcut execution
            } else {
                return;
            }
        }
        
        // Check all registered shortcuts
        for (let [key, callback] of this.listeners) {
            const [category, action] = key.split('.');
            const shortcutConfig = this.config.shortcuts[category]?.[action];
            
            if (!shortcutConfig || !shortcutConfig.enabled) continue;
            
            // Check primary shortcut
            const primaryShortcut = this.parseShortcut(
                PlatformHelper.isMac() && shortcutConfig.keys_mac 
                    ? shortcutConfig.keys_mac 
                    : shortcutConfig.keys
            );
            
            let matched = this.matchesShortcut(event, primaryShortcut);
            
            // Check alternative shortcut if no match
            if (!matched && shortcutConfig.alt_keys) {
                const altShortcut = this.parseShortcut(shortcutConfig.alt_keys);
                matched = this.matchesShortcut(event, altShortcut);
            }
            
            if (matched) {
                // Check context/scope
                const reason = ContextManager.getDisabledReason(category, action, this.config);
                
                if (reason) {
                    if (this.debugMode) {
                        console.log(`[ShortcutHandler] Blocked: ${reason}`);
                    }
                    const message = this.translate('messages.' + this.getMessageKey(reason));
                    showShortcutToast(`⚠️ ${message}`, 3000);
                    return;
                }
                
                // Execute callback with performance monitoring
                if (shortcutConfig.prevent_default) {
                    event.preventDefault();
                    event.stopPropagation();
                }
                
                if (this.debugMode) {
                    console.log(`[ShortcutHandler] Executed: ${category}.${action}`);
                }
                
                // Measure callback execution time
                const measurement = this.performanceMonitor.measure(() => callback(event));
                this.performanceMonitor.recordExecution(category, action, measurement.duration);
                
                // Announce to screen readers
                if (shortcutConfig.aria_label) {
                    announceToScreenReader(shortcutConfig.aria_label);
                }
                
                // Record total handler time
                const handlerEnd = performance.now();
                this.performanceMonitor.recordHandlerTime(handlerEnd - handlerStart);
                
                return; // Only execute first matching shortcut
            }
        }
        
        // Record handler time even if no shortcut matched
        const handlerEnd = performance.now();
        this.performanceMonitor.recordHandlerTime(handlerEnd - handlerStart);
    }
    
    /**
     * Check if user is typing in an input field
     * @param {HTMLElement} target - Event target element
     * @returns {boolean} True if typing in input/textarea/contenteditable
     */
    isTypingInInput(target) {
        if (!target) return false;
        
        const tagName = target.tagName.toLowerCase();
        
        // Checkboxes and radio buttons should not block shortcuts
        if (tagName === 'input') {
            const inputType = (target.type || '').toLowerCase();
            if (inputType === 'checkbox' || inputType === 'radio') {
                return false; // Allow shortcuts when checkbox/radio has focus
            }
        }
        
        const isInput = tagName === 'input' || tagName === 'textarea';
        const isContentEditable = target.isContentEditable;
        
        return isInput || isContentEditable;
    }
    
    /**
     * Get message key from reason string
     * @param {string} reason - Disabled reason
     * @returns {string} Message key for translation
     */
    getMessageKey(reason) {
        if (reason.includes('modal')) return 'modal_blocked';
        if (reason.includes('Elements table')) return 'wrong_table';
        if (reason.includes('Select rows')) return 'no_selection';
        return 'modal_blocked'; // Default
    }
    
    /**
     * Translate a key using current locale
     * @param {string} key - Translation key (e.g., 'messages.saved')
     * @returns {string} Translated text
     */
    translate(key) {
        const keys = key.split('.');
        let value = this.translations;
        
        for (let k of keys) {
            value = value?.[k];
            if (value === undefined) break;
        }
        
        return value || key;
    }
    
    /**
     * Enable shortcut handler
     */
    enable() {
        if (this.enabled) {
            console.log('[ShortcutHandler] Already enabled');
            return;
        }
        this.enabled = true;
        document.addEventListener('keydown', this.boundHandleKeyDown, true);
        
        console.log('[ShortcutHandler] ✓ ENABLED - Now listening for keyboard events');
        console.log('[ShortcutHandler] Registered shortcuts:', this.listeners.size);
    }
    
    /**
     * Disable shortcut handler
     */
    disable() {
        if (!this.enabled) return;
        this.enabled = false;
        document.removeEventListener('keydown', this.boundHandleKeyDown, true);
        
        if (this.debugMode) {
            console.log('[ShortcutHandler] Disabled');
        }
    }
    
    /**
     * Clean up resources
     */
    destroy() {
        this.disable();
        this.listeners.clear();
        this.config = null;
        this.translations = null;
    }
}

// ============================================================================
// CONTEXT MANAGER
// ============================================================================
// Determines whether shortcuts should be enabled based on current UI context

class ContextManager {
    /**
     * Check if a modal dialog is currently open
     * @returns {boolean} True if modal is open
     */
    static isModalOpen() {
        return $('.modal.show').length > 0 || $('.modal.in').length > 0;
    }
    
    /**
     * Check if DataTable has focus
     * @returns {boolean} True if table or its children are focused
     */
    static isTableFocused() {
        const activeElement = document.activeElement;
        if (!activeElement) return false;
        
        // Check if active element is inside table
        return $(activeElement).closest('#table').length > 0 ||
               $('#table').is(':focus');
    }
    
    /**
     * Get currently selected table name
     * @returns {string} Table name (e.g., 'elements', 'waarde_radiobuttons')
     */
    static getCurrentTable() {
        return $('#file').val();
    }
    
    /**
     * Check if any rows are selected in the table
     * Uses checkbox-based selection, not DataTable API
     * @returns {boolean} True if rows are selected
     */
    static hasSelectedRows() {
        // Use checkbox selection instead of DataTable selection API
        return $('.delete-rows:checked').length > 0;
    }
    
    /**
     * Check if copy/paste operations are allowed in current context
     * @returns {boolean} True if copy/paste is allowed
     */
    static canUseCopyPaste() {
        return this.getCurrentTable() === 'elements' && 
               this.hasSelectedRows() && 
               !this.isModalOpen();
    }
    
    /**
     * Check if save operation is allowed
     * @returns {boolean} True if save is allowed
     */
    static canSave() {
        return !this.isModalOpen();
    }
    
    /**
     * Get reason why shortcut is disabled (if any)
     * 
     * @param {string} category - Shortcut category
     * @param {string} action - Shortcut action
     * @param {Object} config - Configuration object
     * @returns {string|null} Reason string or null if shortcut is enabled
     */
    static getDisabledReason(category, action, config) {
        const shortcut = config.shortcuts[category]?.[action];
        if (!shortcut) return 'Shortcut not found';
        
        // Check if shortcut is disabled in config
        if (!shortcut.enabled) {
            return 'This shortcut is disabled in settings';
        }
        
        // Modal is open (Escape is handled separately)
        if (this.isModalOpen()) {
            return 'Shortcuts disabled while modal is open';
        }
        
        // Check scope restrictions
        if (shortcut.scope === 'elements_table_only') {
            if (this.getCurrentTable() !== 'elements') {
                return 'This shortcut only works in the Elements table';
            }
        }
        
        // Check if selection is required (for clipboard operations)
        if (category === 'clipboard' && (action === 'copy' || action === 'cut')) {
            if (!this.hasSelectedRows()) {
                return 'Select rows first to use copy/cut';
            }
        }
        
        return null; // Shortcut is enabled
    }
}

// ============================================================================
// PLATFORM HELPER
// ============================================================================
// Handles platform-specific keyboard shortcuts (Windows/Mac)

class PlatformHelper {
    /**
     * Check if running on macOS
     * @returns {boolean} True if Mac
     */
    static isMac() {
        return navigator.platform.toUpperCase().indexOf('MAC') >= 0;
    }
    
    /**
     * Display shortcut with platform-specific symbols
     * Converts "Ctrl+C" to "⌘+C" on Mac
     * 
     * @param {string} keys - Shortcut string (e.g., "Ctrl+C")
     * @returns {string} Formatted shortcut for display
     */
    static displayShortcut(keys) {
        if (!keys) return '';
        
        if (this.isMac()) {
            return keys
                .replace(/Ctrl/g, '⌘')
                .replace(/Cmd/g, '⌘')
                .replace(/Alt/g, '⌥')
                .replace(/Shift/g, '⇧')
                .replace(/Enter/g, '↵')
                .replace(/Backspace/g, '⌫')
                .replace(/Delete/g, '⌦');
        }
        
        return keys;
    }
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Show toast notification for shortcut feedback
 * 
 * @param {string} message - Message to display
 * @param {number} duration - Duration in milliseconds (default: 2000)
 */
function showShortcutToast(message, duration = 2000) {
    // Remove any existing toast
    $('.shortcut-toast').remove();
    
    const toast = $('<div class="shortcut-toast"></div>').text(message);
    $('body').append(toast);
    
    // Trigger reflow for animation
    toast[0].offsetHeight;
    toast.addClass('show');
    
    setTimeout(() => {
        toast.removeClass('show');
        setTimeout(() => toast.remove(), 300);
    }, duration);
}

/**
 * Announce message to screen readers
 * Uses ARIA live region for accessibility
 * 
 * @param {string} message - Message to announce
 */
function announceToScreenReader(message) {
    let announcer = $('#aria-announcer');
    
    if (announcer.length === 0) {
        announcer = $('<div>', {
            id: 'aria-announcer',
            'class': 'sr-only',
            'aria-live': 'polite',
            'aria-atomic': 'true'
        }).appendTo('body');
    }
    
    announcer.text(message);
    
    // Clear after 5 seconds
    setTimeout(() => announcer.text(''), 5000);
}

// ============================================================================
// PERFORMANCE MONITOR
// ============================================================================

/**
 * Performance monitoring for keyboard shortcuts
 * Tracks execution time, frequency, and provides statistics
 */
class ShortcutPerformanceMonitor {
    constructor() {
        this.metrics = {
            shortcutExecutions: [],      // Array of {timestamp, category, action, duration}
            totalShortcuts: 0,
            handlerTimes: [],            // Time spent in handleKeyDown
            callbackTimes: []            // Time spent in callback execution
        };
        this.enabled = true;
    }
    
    /**
     * Measure execution time of a function
     * @param {Function} fn - Function to measure
     * @returns {Object} {result, duration}
     */
    measure(fn) {
        if (!this.enabled) {
            return {result: fn(), duration: 0};
        }
        
        const start = performance.now();
        const result = fn();
        const end = performance.now();
        const duration = end - start;
        
        return {result, duration};
    }
    
    /**
     * Record a shortcut execution
     * @param {string} category - Shortcut category
     * @param {string} action - Shortcut action
     * @param {number} duration - Execution time in ms
     */
    recordExecution(category, action, duration) {
        if (!this.enabled) return;
        
        this.metrics.shortcutExecutions.push({
            timestamp: Date.now(),
            category,
            action,
            duration
        });
        
        this.metrics.totalShortcuts++;
        this.metrics.callbackTimes.push(duration);
        
        // Warn if slow execution
        if (duration > 50) {
            console.warn(`[Performance] Slow shortcut: ${category}.${action} took ${duration.toFixed(2)}ms`);
        }
        
        // Keep only last 100 executions to avoid memory bloat
        if (this.metrics.shortcutExecutions.length > 100) {
            this.metrics.shortcutExecutions.shift();
        }
        if (this.metrics.callbackTimes.length > 100) {
            this.metrics.callbackTimes.shift();
        }
        
        // Update performance panel if it's open
        if (window.performanceMonitor && window.performanceMonitor.showPanel) {
            window.performanceMonitor.updatePanel();
        }
    }
    
    /**
     * Record handler processing time
     * @param {number} duration - Time in ms
     */
    recordHandlerTime(duration) {
        if (!this.enabled) return;
        
        this.metrics.handlerTimes.push(duration);
        
        // Keep only last 100
        if (this.metrics.handlerTimes.length > 100) {
            this.metrics.handlerTimes.shift();
        }
    }
    
    /**
     * Calculate average of an array
     * @param {Array<number>} arr - Numbers to average
     * @returns {number} Average value
     */
    avg(arr) {
        if (arr.length === 0) return 0;
        return arr.reduce((a, b) => a + b, 0) / arr.length;
    }
    
    /**
     * Get performance statistics
     * @returns {Object} Statistics object
     */
    getStats() {
        return {
            totalShortcuts: this.metrics.totalShortcuts,
            avgHandlerTime: this.avg(this.metrics.handlerTimes).toFixed(2) + 'ms',
            maxHandlerTime: Math.max(...this.metrics.handlerTimes, 0).toFixed(2) + 'ms',
            avgCallbackTime: this.avg(this.metrics.callbackTimes).toFixed(2) + 'ms',
            maxCallbackTime: Math.max(...this.metrics.callbackTimes, 0).toFixed(2) + 'ms',
            recentExecutions: this.metrics.shortcutExecutions.slice(-10) // Last 10
        };
    }
    
    /**
     * Get most used shortcuts
     * @param {number} limit - Number of top shortcuts to return
     * @returns {Array} Array of {category, action, count}
     */
    getTopShortcuts(limit = 5) {
        const counts = {};
        
        for (const exec of this.metrics.shortcutExecutions) {
            const key = `${exec.category}.${exec.action}`;
            counts[key] = (counts[key] || 0) + 1;
        }
        
        return Object.entries(counts)
            .map(([key, count]) => {
                const [category, action] = key.split('.');
                return {category, action, count};
            })
            .sort((a, b) => b.count - a.count)
            .slice(0, limit);
    }
    
    /**
     * Print statistics to console
     */
    logStats() {
        const stats = this.getStats();
        console.log('[Shortcuts Performance]');
        console.log('  Total executions:', stats.totalShortcuts);
        console.log('  Avg handler time:', stats.avgHandlerTime);
        console.log('  Max handler time:', stats.maxHandlerTime);
        console.log('  Avg callback time:', stats.avgCallbackTime);
        console.log('  Max callback time:', stats.maxCallbackTime);
        
        const top = this.getTopShortcuts();
        if (top.length > 0) {
            console.log('  Top shortcuts:');
            top.forEach(s => {
                console.log(`    - ${s.category}.${s.action}: ${s.count}x`);
            });
        }
    }
    
    /**
     * Reset all metrics
     */
    reset() {
        this.metrics = {
            shortcutExecutions: [],
            totalShortcuts: 0,
            handlerTimes: [],
            callbackTimes: []
        };
    }
    
    /**
     * Enable/disable monitoring
     * @param {boolean} enabled - Enable state
     */
    setEnabled(enabled) {
        this.enabled = enabled;
    }
}

// ============================================================================
// EXPORT
// ============================================================================
// Make classes available globally
window.ShortcutHandler = ShortcutHandler;
window.ContextManager = ContextManager;
window.PlatformHelper = PlatformHelper;
window.ShortcutPerformanceMonitor = ShortcutPerformanceMonitor;

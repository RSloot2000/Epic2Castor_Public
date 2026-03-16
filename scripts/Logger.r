## Auto-initializing logger for Epic2Castor
## - Creates logs/<YYYY-MM-DD_HH-MM> (or ..._X) once per run
## - Creates per-entry script log file: <ScriptBase>_log.txt
## - Captures all console output (stdout) and messages/warnings/errors (stderr)
## - Stays active without explicit activation in scripts (just source this file)

# ---- internal helpers -----------------------------------------------------
.ep2c_get_script_base <- function() {
	# 1) Rscript invocation: --file=...
	args <- commandArgs(trailingOnly = FALSE)
	fileArg <- grep("^--file=", args, value = TRUE)
	if (length(fileArg) > 0) {
		p <- sub("^--file=", "", fileArg[[1]])
		return(tools::file_path_sans_ext(basename(p)))
	}
	# 2) RStudio active document (best-effort)
	if (interactive()) {
		if (requireNamespace("rstudioapi", quietly = TRUE)) {
			if (isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
				p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
				if (!is.null(p) && nzchar(p)) {
					return(tools::file_path_sans_ext(basename(p)))
				}
			}
		}
	}
	# 3) Shiny default guess
	if (requireNamespace("shiny", quietly = TRUE)) {
		# If a reactive domain exists, we can assume App
		dom <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
		if (!is.null(dom)) return("App")
	}
	# 4) Fallback
	"Session"
}

.ep2c_next_run_dir <- function(root = "logs") {
	if (!dir.exists(root)) dir.create(root, recursive = TRUE, showWarnings = FALSE)
	stamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
	base <- file.path(root, stamp)
	if (!dir.exists(base)) return(base)

	# Find next suffix _X
	entries <- list.dirs(root, recursive = FALSE, full.names = FALSE)
	pattern <- paste0("^", gsub("\\\\", "\\\\\\\\", stamp), "(_\\\\d+)?$")
	matches <- grep(pattern, entries, value = TRUE)
	if (length(matches) == 0) return(paste0(base, "_1"))
	# Extract numeric suffixes
	suff <- sub(paste0("^", stamp, "_"), "", matches)
	suff[!grepl("_\\\\d+", matches)] <- "0"  # plain base has implicit 0
	nums <- suppressWarnings(as.integer(suff))
	next_n <- max(nums, na.rm = TRUE) + 1L
	file.path(root, paste0(stamp, "_", next_n))
}

# Try to get logs root from central config if available
.ep2c_logs_root <- function() {
	root <- "logs"
	# Als config al geladen is, gebruik het centrale pad
	if (exists("epc_path", mode = "function")) {
		val <- tryCatch(epc_path("logs_dir"), error = function(e) NULL)
		if (!is.null(val) && is.character(val) && nzchar(val)) root <- val
		return(root)
	}
	# Probeer centrale config te laden en opnieuw te proberen
	cfg <- file.path("scripts", "config.R")
	if (file.exists(cfg)) {
		try(source(cfg), silent = TRUE)
		if (exists("epc_path", mode = "function")) {
			val <- tryCatch(epc_path("logs_dir"), error = function(e) NULL)
			if (!is.null(val) && is.character(val) && nzchar(val)) root <- val
		}
	}
	root
}

.ep2c_open_sinks <- function(logfile) {
	# Protect against double-sinking in same session
	if (isTRUE(getOption("epic2castor.logger.active", FALSE))) return(invisible(TRUE))
	# Stdout with split=TRUE keeps console output visible
	try(sink(logfile, append = TRUE, split = TRUE), silent = TRUE)
	# Stderr/messages (split isn't supported for message; prioritize capturing to file)
	try(sink(logfile, append = TRUE, type = "message"), silent = TRUE)
	options(epic2castor.logger.active = TRUE,
					epic2castor.logger.file   = logfile)
	invisible(TRUE)
}

.ep2c_write_header <- function(logfile, script_base, run_dir) {
	hdr <- paste0(
		"\n===== Epic2Castor Log Start =====\n",
		"Timestamp   : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n",
		"Script      : ", script_base, "\n",
		"Working dir : ", tryCatch(normalizePath(getwd()), error = function(e) getwd()), "\n",
		"Run dir     : ", run_dir, "\n",
		"R version   : ", R.version.string, "\n",
		"=================================\n\n"
	)
	# Write header directly to file, independent of sinks
	cat(hdr, file = logfile, append = TRUE)
}

# ---- public helpers -------------------------------------------------------
epic2castor_logdir <- function() {
	getOption("epic2castor.logdir", default = NULL)
}

epic2castor_stop_logger <- function() {
	# Close message sinks
	while (sink.number(type = "message") > 0) {
		try(sink(type = "message"), silent = TRUE)
	}
	# Close output sinks
	while (sink.number() > 0) {
		try(sink(), silent = TRUE)
	}
	options(epic2castor.logger.active = FALSE)
	invisible(TRUE)
}

epic2castor_init_logger <- function(force = FALSE) {
	# ===== CHECK IF ALREADY INITIALIZED =====
	# If already initialized in this session and not forcing, reuse existing setup
	if (isTRUE(getOption("epic2castor.logger.initialized", FALSE)) && !force) {
		return(invisible(TRUE))
	}

	# ===== FORCE RE-INITIALIZATION =====
	# Stop current sinks and clear state for truly fresh run
	if (isTRUE(force)) {
		epic2castor_stop_logger()
		options(
			epic2castor.logger.initialized = FALSE,
			epic2castor.logger.active = FALSE,
			epic2castor.logdir = NULL
		)
		Sys.unsetenv("EPIC2CASTOR_LOGDIR")
	}

	# ===== DETERMINE RUN DIRECTORY =====
	# Priority:
	# 1. Existing environment variable (shared across scripts in same session)
	# 2. Existing option (shared within R session)
	# 3. Create new directory (only if neither exists)
	run_dir_env <- Sys.getenv("EPIC2CASTOR_LOGDIR", unset = "")
	run_dir_opt <- getOption("epic2castor.logdir", NULL)
	run_dir <- NULL
	
	if (!isTRUE(force)) {
		# Try to reuse existing directory from environment variable
		if (nzchar(run_dir_env) && dir.exists(run_dir_env)) {
			run_dir <- run_dir_env
		} 
		# Fallback to option if env var not set
		else if (!is.null(run_dir_opt) && dir.exists(run_dir_opt)) {
			run_dir <- run_dir_opt
		}
	}
	
	# Only create NEW directory if no existing one found
	if (is.null(run_dir) || !nzchar(run_dir)) {
		run_dir <- .ep2c_next_run_dir(root = .ep2c_logs_root())
		dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
	}

	# ===== STORE RUN DIRECTORY =====
	# Store in both option and environment for downstream/child processes
	# This ensures all scripts in same session use same log directory
	options(epic2castor.logdir = run_dir)
	Sys.setenv(EPIC2CASTOR_LOGDIR = run_dir)

	# ===== DETERMINE SCRIPT-SPECIFIC LOG FILE =====
	# Each script gets its own log file within the shared run directory
	# e.g., App_log.txt, baseline_log.txt, biobank_data_log.txt
	script_base <- .ep2c_get_script_base()
	logfile <- file.path(run_dir, paste0(script_base, "_log.txt"))

	# ===== INITIALIZE LOGGING FOR THIS SCRIPT =====
	# Write header for this specific script
	.ep2c_write_header(logfile, script_base, run_dir)
	# Open sinks only if not already active (prevents double-sinking)
	.ep2c_open_sinks(logfile)

	# Mark initialized
	options(epic2castor.logger.initialized = TRUE)

	# Ensure sinks are closed at end of session/process
	reg.finalizer(.GlobalEnv, function(e) {
		epic2castor_stop_logger()
	}, onexit = TRUE)

	invisible(TRUE)
}

# Convenience: start a brand new run directory and re-open sinks
epic2castor_new_run <- function() {
	epic2castor_init_logger(force = TRUE)
}

# ---- auto-run on source ---------------------------------------------------
# Gate auto-init behind an env var so callers can opt-out and initialize manually
# Usage to disable auto-init before sourcing this file: Sys.setenv(EPIC2CASTOR_AUTOINIT = "0")
if (!identical(tolower(Sys.getenv("EPIC2CASTOR_AUTOINIT", unset = "1")), "0")) {
	try(epic2castor_init_logger(), silent = TRUE)
}

# ---- structured status logging (for UI progress) ---------------------------
# Purpose: lightweight status channel that overwrites a tiny JSON file
# so UIs (e.g., Shiny) can poll without tailing large logs.
# Usage (in scripts after sourcing this file):
#   epic2castor_status_init(total = N, title = "Baseline", step = "start")
#   epic2castor_status_update(step = "table", detail = "X (i/N)", current = i)
#   epic2castor_status_done(detail = "Done")

.ep2c_status_env <- new.env(parent = emptyenv())

.ep2c_status_path <- function() {
	# Prefer explicit env var; otherwise use current run-dir
	pth <- Sys.getenv("EPIC2CASTOR_STATUS_FILE", unset = "")
	if (nzchar(pth)) return(pth)
	run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", unset = ""))
	if (!nzchar(run_dir)) return("status.json")
	file.path(run_dir, "status.json")
}

.ep2c_now_epoch <- function() as.numeric(Sys.time())

.ep2c_to_json <- function(obj) {
	# Try jsonlite; fallback to a very small manual JSON writer
	if (requireNamespace("jsonlite", quietly = TRUE)) {
		return(jsonlite::toJSON(obj, auto_unbox = TRUE, digits = 8))
	}
	# Minimal manual encoding for strings and numbers
	esc <- function(x) gsub("\\\\", "\\\\\\\\", gsub("\"", "\\\"", x, fixed = TRUE))
	parts <- mapply(function(k, v) {
		if (is.null(v) || (is.atomic(v) && length(v) == 1 && is.na(v))) {
			sprintf('"%s":null', esc(k))
		} else if (is.numeric(v)) {
			sprintf('"%s":%s', esc(k), as.character(v))
		} else if (is.logical(v)) {
			sprintf('"%s":%s', esc(k), ifelse(v, "true", "false"))
		} else {
			sprintf('"%s":"%s"', esc(k), esc(as.character(v)))
		}
	}, names(obj), obj, USE.NAMES = FALSE)
	paste0("{", paste(parts, collapse = ","), "}")
}

.ep2c_status_write <- function(force = FALSE) {
	se <- .ep2c_status_env
	if (isTRUE(se$disabled)) return(invisible(FALSE))

	# Throttle by interval unless forced
	interval_ms <- suppressWarnings(as.integer(Sys.getenv("EPIC2CASTOR_STATUS_THROTTLE_MS", unset = "300")))
	if (is.na(interval_ms) || interval_ms < 0) interval_ms <- 300L
	last <- se$last_write_epoch %||% 0
	now <- .ep2c_now_epoch()
	if (!isTRUE(force) && (now - last) * 1000 < interval_ms) return(invisible(FALSE))

	# Build payload
	cur <- se$current %||% NA_real_
	tot <- se$total %||% NA_real_
	start_ep <- se$start_epoch %||% now
	elapsed <- now - start_ep
	percent <- if (!is.na(cur) && !is.na(tot) && tot > 0) max(0, min(100, round(100 * cur / tot, 1))) else NA_real_
	eta_s <- if (!is.na(cur) && !is.na(tot) && cur > 0) {
		rate <- elapsed / cur
		max(0, as.numeric((tot - cur) * rate))
	} else NA_real_

	obj <- list(
		ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
		step = se$step %||% NA_character_,
		detail = se$detail %||% NA_character_,
		severity = se$severity %||% "INFO",
		current = if (is.na(cur)) NA_real_ else as.numeric(cur),
		total = if (is.na(tot)) NA_real_ else as.numeric(tot),
		percent = percent,
		elapsed_s = round(elapsed, 1),
		eta_s = if (is.na(eta_s)) NA_real_ else round(eta_s, 1),
		pid = Sys.getpid(),
		script = tryCatch(.ep2c_get_script_base(), error = function(e) "Session"),
		run_dir = getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
	)

	pth <- se$path %||% .ep2c_status_path()
	dir.create(dirname(pth), recursive = TRUE, showWarnings = FALSE)
	json <- .ep2c_to_json(obj)
	# Always overwrite small file
	writeLines(as.character(json), pth, useBytes = TRUE)
	se$last_write_epoch <- now
	invisible(TRUE)
}

# Public API
epic2castor_status_file <- function() {
	# Returns current status file path
	se <- .ep2c_status_env
	pth <- se$path %||% .ep2c_status_path()
	return(pth)
}

epic2castor_status_init <- function(total = NA_integer_, title = NULL, step = "init", detail = NULL, enabled = NULL) {
	# Enable unless explicitly disabled via env
	env_enabled <- !identical(tolower(Sys.getenv("EPIC2CASTOR_STATUS", unset = "1")), "0")
	if (!is.null(enabled)) env_enabled <- isTRUE(enabled)
	se <- .ep2c_status_env
	se$disabled <- !env_enabled
	se$start_epoch <- .ep2c_now_epoch()
	se$last_write_epoch <- 0
	se$total <- suppressWarnings(as.numeric(total))
	if (!is.finite(se$total)) se$total <- NA_real_
	se$current <- 0
	se$title <- if (is.null(title)) NULL else as.character(title)
	se$step <- as.character(step)
	se$detail <- if (is.null(detail)) NULL else as.character(detail)
	se$severity <- "INFO"
	se$path <- .ep2c_status_path()
	if (!isTRUE(se$disabled)) .ep2c_status_write(force = TRUE)
	invisible(TRUE)
}

epic2castor_status_update <- function(step = NULL, detail = NULL, current = NULL, total = NULL, severity = NULL, force = FALSE) {
	se <- .ep2c_status_env
	if (!missing(total) && !is.null(total)) {
		val <- suppressWarnings(as.numeric(total)); if (is.finite(val)) se$total <- val
	}
	if (!missing(current) && !is.null(current)) {
		val <- suppressWarnings(as.numeric(current)); if (is.finite(val)) se$current <- val
	}
	if (!missing(step) && !is.null(step)) se$step <- as.character(step)
	if (!missing(detail) && !is.null(detail)) se$detail <- as.character(detail)
	if (!missing(severity) && !is.null(severity)) se$severity <- as.character(severity)
	.ep2c_status_write(force = isTRUE(force))
	invisible(TRUE)
}

epic2castor_status_tick <- function(n = 1, detail = NULL, step = NULL, force = FALSE) {
	se <- .ep2c_status_env
	cur <- se$current %||% 0
	se$current <- cur + n
	if (!is.null(detail)) se$detail <- as.character(detail)
	if (!is.null(step)) se$step <- as.character(step)
	.ep2c_status_write(force = isTRUE(force))
	invisible(TRUE)
}

epic2castor_status_done <- function(detail = "Done", severity = "INFO") {
	se <- .ep2c_status_env
	if (!is.null(se$total) && is.finite(se$total)) se$current <- se$total
	se$step <- "done"
	se$detail <- as.character(detail)
	se$severity <- as.character(severity)
	.ep2c_status_write(force = TRUE)
	invisible(TRUE)
}

# Helper: infix for NULL-coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a


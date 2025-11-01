## Biobank data mapping script (dependency-light, base R)
## - Leest biobank_data.csv en mdns.csv (semicolon-gescheiden)
## - Vervangt waarden in kolom "Identificatie" op basis van mdns: MDN -> Participant ID
## - Schrijft resultaat naar output_data/biobank_data/biobank.csv
##   (alleen waarden in kolom "Afname.datumtijd" worden gequote om interne ';' te behouden)

# Hoofdlogica in een tryCatch-blok om fouten af te vangen en te rapporteren
tryCatch({
	## Paden laden
	source(file.path("scripts", "config.R"), local = TRUE)
	paths <- epc_paths()

	# Start centrale logger zodat een runmap en hoofdlog worden aangemaakt
	log_script <- tryCatch(epc_path("logger_script"), error = function(e) NULL)
	if (!is.null(log_script) && file.exists(log_script)) {
		try(source(log_script), silent = TRUE)
	}

	# Init status voor UI (4 stappen: read bb, read mdns, map, write)
	try(epic2castor_status_init(total = 4, title = "Biobank", step = "init", detail = "Starting…"), silent = TRUE)

	in_dir  <- paths$biobank_input_data_dir
	out_dir <- paths$biobank_output_data_dir

	# Define full paths to input and output files
	biobank_file <- file.path(in_dir, "biobank_data.csv")
	mdns_file    <- file.path(in_dir, "MDNS.csv")
	out_file     <- file.path(out_dir, "biobank.csv")

	# Log routing: stuur messages/warnings naar stdout en print warnings direct
	options(warn = 1)
	try(sink(stdout(), type = "message"), silent = TRUE)

	# Helpers: semicolon CSV in base R; houd strings als character en trim
	read_semicolon_csv <- function(path) {
		if (!file.exists(path)) {
			# Gebruik gestandaardiseerde error string
			stop(paste0("ERROR:FILE_NOT_FOUND:", path))
		}
		utils::read.csv2(
			file = path,
			sep = ";",
			dec = ",",
			stringsAsFactors = FALSE,
			na.strings = c("", "NA"),
			fileEncoding = "UTF-8-BOM",
			check.names = TRUE
		)
	}

	# Find column name helper (supports variants like "Participant ID" vs "Participant.ID")
	pick_col <- function(df, candidates, label) {
		for (nm in candidates) if (nm %in% names(df)) return(nm)
		stop(sprintf("Column not found (%s). Tried: %s", label, paste(candidates, collapse = ", ")))
	}

	message("Reading: ", biobank_file)
	bb <- read_semicolon_csv(biobank_file)
	try(epic2castor_status_update(step = "read", detail = sprintf("Read biobank_data.csv (%d rows)", NROW(bb)), current = 1, force = TRUE), silent = TRUE)
	message("Reading: ", mdns_file)
	md <- read_semicolon_csv(mdns_file)
	try(epic2castor_status_update(step = "read", detail = sprintf("Read mdns.csv (%d rows)", NROW(md)), current = 2, force = TRUE), silent = TRUE)

	# Validation + determine column names
	ident_col <- pick_col(bb, c("Identificatie", "identificatie"), "Identificatie (biobank)")
	mdn_col   <- pick_col(md, c("MDN", "mdn"), "MDN (mdns)")
	pid_col   <- pick_col(md, c("Participant ID", "Participant.ID", "participant_id", "ParticipantId", "participantID"), "Participant ID (mdns)")

	# Types en trimming
	bb[[ident_col]] <- trimws(as.character(bb[[ident_col]]))
	md[[mdn_col]]   <- trimws(as.character(md[[mdn_col]]))
	md[[pid_col]]   <- trimws(as.character(md[[pid_col]]))

	# Unieke mapping bouwen
	md_key <- unique(md[, c(mdn_col, pid_col)])

	# Named vector voor snelle substitutie
	map_vec <- md_key[[pid_col]]
	names(map_vec) <- md_key[[mdn_col]]

	# Vervanging toepassen
	old_ids <- bb[[ident_col]]
	rep_idx <- match(old_ids, names(map_vec))
	rep_val <- map_vec[rep_idx]

	# Bepaal matches en nieuwe IDs
	matched <- !is.na(rep_val) & nzchar(rep_val)
	new_ids <- ifelse(matched, rep_val, old_ids)

	# Maak output met alleen gematchte regels en vervang Identificatie
	bb_out <- bb[matched, , drop = FALSE]
	if (NROW(bb_out) > 0) {
		bb_out[[ident_col]] <- rep_val[matched]
	}

	# Sorteren op Identificatie: haal numerieke component (bijv. 0013 -> 13) en sorteer oplopend
	if (NROW(bb_out) > 0) {
		ident_vals <- bb_out[[ident_col]]
		# Extraheer digits; niet-numerieke of lege waarden komen als NA en worden onderaan geplaatst
		num_part <- suppressWarnings(as.integer(gsub("\\D", "", ident_vals)))
		ord <- order(num_part, ident_vals, na.last = TRUE)
		bb_out <- bb_out[ord, , drop = FALSE]
	}

	# Normalisatie datum/tijd kolommen naar Castor-formaat
	normalize_date <- function(x) {
		if (is.null(x)) return(x)
		s <- trimws(as.character(x))
		s[s == "" | is.na(s)] <- NA_character_
		conv <- function(v) {
			if (is.na(v)) return(NA_character_)
			vv <- gsub("[./]", "-", v)
			if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", vv)) {
				d <- strsplit(vv, "-")[[1]]; return(sprintf("%02d-%02d-%04d", as.integer(d[3]), as.integer(d[2]), as.integer(d[1])))
			}
			if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}", vv)) {
				d <- strsplit(vv, "-")[[1]]; return(sprintf("%02d-%02d-%04d", as.integer(d[1]), as.integer(d[2]), as.integer(d[3])))
			}
			vv
		}
		vapply(s, conv, character(1))
	}

	normalize_datetime <- function(x) {
		if (is.null(x)) return(x)
		s <- trimws(as.character(x))
		s[s == "" | is.na(s)] <- NA_character_
		round_quarter <- function(m) { allowed <- c(0,15,30,45); allowed[which.min(abs(allowed - as.integer(m)))] }
		pad2 <- function(n) sprintf("%02d", as.integer(n))
		conv <- function(v) {
			if (is.na(v)) return(NA_character_)
			vv <- gsub("[./]", "-", v)
			# Accepteer formats met spatie tussen datum en tijd
			if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2} \\d{1,2}:\\d{2}$", vv)) {
				parts <- strsplit(vv, "[\n\r\t ]+")[[1]]; d <- strsplit(parts[1], "-")[[1]]; tm <- strsplit(parts[2], ":")[[1]]
				mm <- round_quarter(tm[2])
				return(paste0(pad2(d[3]), "-", pad2(d[2]), "-", sprintf("%04d", as.integer(d[1])), ";", pad2(tm[1]), ":", pad2(mm)))
			}
			if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4} \\d{1,2}:\\d{2}$", vv)) {
				parts <- strsplit(vv, "[\n\r\t ]+")[[1]]; d <- strsplit(parts[1], "-")[[1]]; tm <- strsplit(parts[2], ":")[[1]]
				mm <- round_quarter(tm[2])
				return(paste0(pad2(d[1]), "-", pad2(d[2]), "-", sprintf("%04d", as.integer(d[3])), ";", pad2(tm[1]), ":", pad2(mm)))
			}
			vv
		}
		vapply(s, conv, character(1))
	}

	# Pas normalisatie toe als kolommen bestaan (tolerant zoeken)
	find_col_regex <- function(df, pattern) {
		hits <- names(df)[grepl(pattern, names(df), ignore.case = TRUE)]
		if (length(hits) > 0) hits[1] else NULL
	}

	adt_col <- NULL
	if ("Afname.datumtijd" %in% names(bb_out)) adt_col <- "Afname.datumtijd" else adt_col <- find_col_regex(bb_out, "afname.*(datum|date).*(tijd|time)")
	fin_col <- NULL
	if ("Afgerond" %in% names(bb_out)) fin_col <- "Afgerond" else fin_col <- find_col_regex(bb_out, "afgerond|finished|completion|completed")

	if (NROW(bb_out) > 0 && !is.null(adt_col)) bb_out[[adt_col]] <- normalize_datetime(bb_out[[adt_col]])
	if (NROW(bb_out) > 0 && !is.null(fin_col)) bb_out[[fin_col]] <- normalize_date(bb_out[[fin_col]])

	# Report
	n_total   <- NROW(bb)
	n_mapped  <- sum(matched)
	n_unmapped <- n_total - n_mapped
	message(sprintf("Records: %d | Mapped: %d | Not found: %d", n_total, n_mapped, n_unmapped))
	try(epic2castor_status_update(step = "map", detail = sprintf("Mapped %d/%d (unmapped %d)", n_mapped, n_total, n_unmapped), current = 3, force = TRUE), silent = TRUE)

	# Log each unmatched identification with file line (header is line 1)
	if (n_unmapped > 0) {
		unmatched_idx <- which(is.na(rep_idx) | is.na(rep_val) | !nzchar(rep_val))
		# Log een compact blok met alle niet-gematchte entries naar console + centrale log
		cat("\n----- Niet-gematchte Identificaties (line_no;Identificatie) -----\n")
		block_lines <- paste0(unmatched_idx + 1L, ";", old_ids[unmatched_idx])
		cat(paste(block_lines, collapse = "\n"), "\n")
		for (i in unmatched_idx) {
			line_no <- i + 1L  # +1 voor headerregel
			warning(sprintf("Geen match gevonden voor Identificatie '%s' op regel %d van biobank_data.csv", old_ids[[i]], line_no), call. = FALSE)
		}
	}

	# Schrijf output (alleen gematchte regels); semicolon, geen rownames, NA leeg
	# Quote alleen de kolom met datum+tijd, omdat de waarde een interne ';' bevat ("dd-mm-jjjj;hh:mm").
	# Gebruik kolomindex voor robuustheid: als de naam niet (meer) bestaat, geen quoting.
	quote_cols <- FALSE
	if (!is.null(adt_col) && length(bb_out) > 0) {
		idx <- match(adt_col, names(bb_out))
		if (!is.na(idx) && idx >= 1L) quote_cols <- idx
	}
	write.table(
		bb_out,
		out_file,
		sep = ";",
		dec = ",",
		row.names = FALSE,
		col.names = TRUE,
		na = "",
		quote = quote_cols,   # alleen Afname.datumtijd quoten
		fileEncoding = "UTF-8"
	)
	message("Geschreven: ", out_file, " (", NROW(bb_out), " rijen)")
	try(epic2castor_status_update(step = "write", detail = sprintf("Wrote %s (%d rows)", out_file, NROW(bb_out)), current = 4, force = TRUE), silent = TRUE)
	cat("EPIC2CASTOR::DONE\n")

	# Status afronden met loglocatie uitsluitend voor modal (niet naar stdout printen)
	try({
		logfile <- getOption("epic2castor.logger.file", NULL)
		if (is.null(logfile) || !is.null(logfile)) {
			run_dir <- getOption("epic2castor.logdir", Sys.getenv("EPIC2CASTOR_LOGDIR", ""))
			script_base <- "biobank_data"
			if (nzchar(run_dir)) logfile <- file.path(run_dir, paste0(script_base, "_log.txt"))
			else logfile <- paste0(script_base, "_log.txt")
		}
		epic2castor_status_done(paste0("Biobank finished — Log saved to ", logfile))
	}, silent = TRUE)

}, error = function(e) {
	# Bij een fout, schrijf de foutmelding naar de status en stop het script met een error-code
	err_msg <- paste("Biobank script failed:", conditionMessage(e))
	
	# Schrijf direct naar stderr (zonder sink) en flush expliciet
	stderr_con <- stderr()
	writeLines(err_msg, con = stderr_con)
	flush(stderr_con)
	
	# Ook naar stdout voor de logs
	writeLines(err_msg, con = stdout())
	flush(stdout())
	
	# Update de status voor de UI
	try(epic2castor_status_update(step = "error", detail = err_msg, force = TRUE), silent = TRUE)
	# Stop het script met een non-zero exit code
	quit(status = 1, save = "no")
},
finally = {
	# Zorg ervoor dat de sink wordt gesloten, zelfs na een fout
	try(sink(type = "message"), silent = TRUE)
})
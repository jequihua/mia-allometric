#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# 06_build_sqlite_unified.R
# ------------------------------------------------------------------------------
# Build the unified flat SQLite database from the unified clean CSV.
#
# Inputs:
#   data_clean/unified/equation_application_unified.csv
#   db/schema_unified.sql
#
# Outputs:
#   db/allometry_unified.sqlite
#   logs/06_build_sqlite_unified.log
#
# Run from the repo root:
#   Rscript R/06_build_sqlite_unified.R
#
# Loads with `dbAppendTable` into the schema-created table so PK/types are
# preserved end to end. Reads the CSV with explicit `colClasses` to keep
# identifier-style fields (e.g. `clave_umafor` `0101`) as TEXT. Same pattern
# as R/02_build_sqlite_volume.R, see decision_log.md.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

DEFAULT_CSV    <- file.path("data_clean", "unified", "equation_application_unified.csv")
DEFAULT_DB     <- file.path("db", "allometry_unified.sqlite")
DEFAULT_SCHEMA <- file.path("db", "schema_unified.sql")
DEFAULT_LOG    <- file.path("logs", "06_build_sqlite_unified.log")

dir_create_if_missing <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

log_line <- function(msg, log_file) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

stop_with_log <- function(msg, log_file) {
  log_line(paste0("ERROR: ", msg), log_file)
  stop(msg, call. = FALSE)
}

execute_sql_script <- function(conn, sql_file) {
  sql_lines <- readLines(sql_file, warn = FALSE)
  sql_lines <- sql_lines[!grepl("^\\s*--", sql_lines)]
  sql_all   <- paste(sql_lines, collapse = "\n")
  stmts <- strsplit(sql_all, ";", fixed = TRUE)[[1]]
  stmts <- trimws(stmts)
  stmts <- stmts[nzchar(stmts)]
  for (s in stmts) DBI::dbExecute(conn, s)
  invisible(length(stmts))
}

# ---- argument parsing --------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
pick_arg <- function(opt_val, args, idx, default) {
  if (!is.na(opt_val) && nzchar(opt_val)) return(opt_val)
  if (length(args) >= idx && nzchar(args[[idx]])) return(args[[idx]])
  default
}

csv_path    <- pick_arg(getOption("UNI_DB_CSV",    NA_character_), args, 1, DEFAULT_CSV)
db_path     <- pick_arg(getOption("UNI_DB_PATH",   NA_character_), args, 2, DEFAULT_DB)
schema_path <- pick_arg(getOption("UNI_DB_SCHEMA", NA_character_), args, 3, DEFAULT_SCHEMA)
log_file    <- pick_arg(getOption("UNI_DB_LOG",    NA_character_), args, 4, DEFAULT_LOG)

dir_create_if_missing(dirname(db_path))
dir_create_if_missing(dirname(log_file))
if (file.exists(log_file)) invisible(file.remove(log_file))

log_line("Starting unified SQLite build", log_file)
log_line(paste0("CSV:    ", normalizePath(csv_path,    winslash = "/", mustWork = FALSE)), log_file)
log_line(paste0("DB:     ", normalizePath(db_path,     winslash = "/", mustWork = FALSE)), log_file)
log_line(paste0("Schema: ", normalizePath(schema_path, winslash = "/", mustWork = FALSE)), log_file)

if (!file.exists(csv_path))    stop_with_log(paste0("CSV not found: ", csv_path), log_file)
if (!file.exists(schema_path)) stop_with_log(paste0("Schema not found: ", schema_path), log_file)

# Column-class contract for the unified flat CSV. text_cols are those declared
# TEXT in db/schema_unified.sql; integer_cols and numeric_cols match
# INTEGER/REAL declarations.
text_cols <- c(
  "source_dataset", "source_record_id",
  "state", "umafor_code", "mx_inegi_cveecon4",
  "scientific_name_apg_raw", "scientific_name_standardized",
  "assignment_level_desc", "equation_code",
  "source_code", "source_reference",
  "equation_raw", "equation_normalized",
  "equation_numpy", "equation_numpy_wd_fixed",
  "dbh_range_cm_raw", "height_range_m_raw",
  "wood_density_units", "wood_density_source",
  "response_variable", "response_units",
  "parse_status", "parse_notes"
)
integer_cols <- c("assignment_level")
numeric_cols <- c("dbh_min_cm", "dbh_max_cm", "height_min_m", "height_max_m",
                  "wood_density_value")

col_classes <- c(
  setNames(rep("character", length(text_cols)),    text_cols),
  setNames(rep("integer",   length(integer_cols)), integer_cols),
  setNames(rep("numeric",   length(numeric_cols)), numeric_cols)
)

eq <- read.csv(
  csv_path,
  stringsAsFactors = FALSE,
  check.names      = FALSE,
  colClasses       = col_classes,
  na.strings       = c("", "NA")
)

expected <- c(text_cols, integer_cols, numeric_cols)
missing  <- setdiff(expected, names(eq))
if (length(missing) > 0) {
  stop_with_log(paste0("CSV missing columns: ", paste(missing, collapse = ", ")), log_file)
}
for (tc in text_cols) {
  if (!is.character(eq[[tc]])) {
    stop_with_log(paste0("Column '", tc, "' is not character after load (",
                         class(eq[[tc]])[1], ")"), log_file)
  }
}

eq <- eq[, expected, drop = FALSE]

n_total <- nrow(eq)
n_infys <- sum(eq$source_dataset == "infys")
n_dina  <- sum(eq$source_dataset == "dina")
log_line(paste0("Rows to load: ", n_total,
                " (infys=", n_infys, ", dina=", n_dina, ")"), log_file)

if (file.exists(db_path)) invisible(file.remove(db_path))

conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
if (!DBI::dbIsValid(conn)) {
  stop_with_log(paste0("Failed to create SQLite connection: ", db_path), log_file)
}
on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

invisible(DBI::dbExecute(conn, "PRAGMA foreign_keys = ON;"))
n_stmts <- execute_sql_script(conn, schema_path)
log_line(paste0("Executed schema statements: ", n_stmts), log_file)

invisible(DBI::dbAppendTable(conn, "equation_application", eq))

invisible(DBI::dbExecute(conn, "DELETE FROM db_meta;"))
meta_pairs <- list(
  built_at               = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  csv_path               = csv_path,
  schema_path            = schema_path,
  pipeline_version       = "unified_v1",
  n_equation_application = as.character(n_total),
  n_infys                = as.character(n_infys),
  n_dina                 = as.character(n_dina)
)
for (k in names(meta_pairs)) {
  invisible(DBI::dbExecute(
    conn,
    "INSERT OR REPLACE INTO db_meta(key, value) VALUES (?, ?);",
    params = list(k, meta_pairs[[k]])
  ))
}

# ---- post-load verification --------------------------------------------------

verify <- function(label, ok, detail = "") {
  status <- if (ok) "OK" else "FAIL"
  log_line(paste0("verify[", status, "] ", label,
                  if (nzchar(detail)) paste0(" -- ", detail) else ""),
           log_file)
  if (!ok) stop_with_log(paste0("verification failed: ", label), log_file)
}

n_db <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM equation_application")$n
verify("row count matches", n_db == n_total,
       paste0("expected=", n_total, " got=", n_db))

n_db_infys <- DBI::dbGetQuery(conn,
  "SELECT COUNT(*) AS n FROM equation_application WHERE source_dataset='infys'")$n
n_db_dina  <- DBI::dbGetQuery(conn,
  "SELECT COUNT(*) AS n FROM equation_application WHERE source_dataset='dina'")$n
verify("infys row count", n_db_infys == n_infys,
       paste0("expected=", n_infys, " got=", n_db_infys))
verify("dina row count",  n_db_dina  == n_dina,
       paste0("expected=", n_dina,  " got=", n_db_dina))

col_info <- DBI::dbGetQuery(conn, "PRAGMA table_info('equation_application')")
get_decl_type <- function(name) {
  hit <- col_info[col_info$name == name, "type"]
  if (length(hit) == 0) "" else hit[1]
}
verify("umafor_code declared TEXT",
       toupper(get_decl_type("umafor_code")) == "TEXT",
       paste0("declared=", get_decl_type("umafor_code")))
verify("equation_numpy_wd_fixed declared TEXT",
       toupper(get_decl_type("equation_numpy_wd_fixed")) == "TEXT",
       paste0("declared=", get_decl_type("equation_numpy_wd_fixed")))
verify("wood_density_value declared REAL",
       toupper(get_decl_type("wood_density_value")) == "REAL",
       paste0("declared=", get_decl_type("wood_density_value")))

# Identifier round-trip: at least one infys umafor_code still starts with '0'.
n_leading_zero <- DBI::dbGetQuery(
  conn,
  "SELECT COUNT(*) AS n FROM equation_application
    WHERE umafor_code IS NOT NULL AND substr(umafor_code,1,1)='0'"
)$n
verify("umafor_code leading zeros preserved", n_leading_zero > 0,
       paste0("rows starting with '0' = ", n_leading_zero))

# Every dina row must carry both a numeric WD value and a populated
# equation_numpy_wd_fixed. This is the headline biomass invariant.
n_dina_complete <- DBI::dbGetQuery(
  conn,
  "SELECT COUNT(*) AS n FROM equation_application
    WHERE source_dataset = 'dina'
      AND wood_density_value IS NOT NULL
      AND equation_numpy_wd_fixed IS NOT NULL"
)$n
verify("every dina row has WD value + WD-fixed equation",
       n_dina_complete == n_dina,
       paste0("complete=", n_dina_complete, " expected=", n_dina))

# No infys row should have a wood_density_value or a wd_fixed equation.
n_infys_wd <- DBI::dbGetQuery(
  conn,
  "SELECT COUNT(*) AS n FROM equation_application
    WHERE source_dataset = 'infys'
      AND (wood_density_value IS NOT NULL OR equation_numpy_wd_fixed IS NOT NULL)"
)$n
verify("no infys row has biomass-only WD columns", n_infys_wd == 0,
       paste0("violating_rows=", n_infys_wd))

# autoincrement PK works
pk_eq <- DBI::dbGetQuery(conn, "PRAGMA table_info('equation_application')")
verify("PK on equation_application_id",
       any(pk_eq$pk > 0 & pk_eq$name == "equation_application_id"))

# named indexes
expected_idx <- c("idx_uni_source", "idx_uni_taxon", "idx_uni_geo",
                  "idx_uni_source_code", "idx_uni_level")
idx_rows <- DBI::dbGetQuery(conn, "PRAGMA index_list('equation_application')")
verify("expected indexes present",
       length(setdiff(expected_idx, idx_rows$name)) == 0)

invisible(DBI::dbExecute(conn, "VACUUM;"))

log_line(paste0("Done. Unified SQLite created at: ",
                normalizePath(db_path, winslash = "/", mustWork = FALSE)),
         log_file)

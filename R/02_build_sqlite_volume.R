#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# 02_build_sqlite_volume.R
# ------------------------------------------------------------------------------
# Build the volume-only SQLite database from the maintained clean CSV.
#
# Inputs:
#   data_clean/volume/equation_application_clean.csv
#   db/schema_volume.sql
#
# Outputs:
#   db/allometry_volume.sqlite
#   logs/02_build_sqlite_volume.log
#
# Run from the repo root:
#   Rscript R/02_build_sqlite_volume.R
#   Rscript R/02_build_sqlite_volume.R <csv> [<db>] [<schema>] [<log>]
#
# Maintained port of
#   90_legacy_review/mia-allometric-main/R/02_build_sqlite_A5.R
# with these deliberate differences vs legacy (see decision_log.md):
#   * persists `ecuacion_numpy`
#   * persists `response_units` and `source_dataset`
#   * loads rows into the schema-created tables instead of replacing them
#     with `dbWriteTable(overwrite = TRUE)`, so the declared PK / FK /
#     `equation_application_id` AUTOINCREMENT and column types from
#     `db/schema_volume.sql` are preserved at the live-database level
#   * reads the clean CSV with explicit `colClasses` so identifier-like
#     fields (e.g. `clave_umafor`, `cveecon4`, `clave_ecuacion`) keep
#     their leading zeros instead of being silently coerced to integers
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

DEFAULT_CSV    <- file.path("data_clean", "volume", "equation_application_clean.csv")
DEFAULT_DB     <- file.path("db", "allometry_volume.sqlite")
DEFAULT_SCHEMA <- file.path("db", "schema_volume.sql")
DEFAULT_LOG    <- file.path("logs", "02_build_sqlite_volume.log")

# ---- helpers -----------------------------------------------------------------

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

opt_csv    <- getOption("VOL_DB_CSV_PATH",    default = NA_character_)
opt_db     <- getOption("VOL_DB_PATH",        default = NA_character_)
opt_schema <- getOption("VOL_DB_SCHEMA_PATH", default = NA_character_)
opt_log    <- getOption("VOL_DB_LOG_PATH",    default = NA_character_)

pick_arg <- function(opt_val, args, idx, default) {
  if (!is.na(opt_val) && nzchar(opt_val)) return(opt_val)
  if (length(args) >= idx && nzchar(args[[idx]])) return(args[[idx]])
  default
}

csv_path    <- pick_arg(opt_csv,    args, 1, DEFAULT_CSV)
db_path     <- pick_arg(opt_db,     args, 2, DEFAULT_DB)
schema_path <- pick_arg(opt_schema, args, 3, DEFAULT_SCHEMA)
log_file    <- pick_arg(opt_log,    args, 4, DEFAULT_LOG)

# ---- main --------------------------------------------------------------------

dir_create_if_missing(dirname(db_path))
dir_create_if_missing(dirname(log_file))
if (file.exists(log_file)) invisible(file.remove(log_file))

log_line("Starting volume SQLite build", log_file)
log_line(paste0("CSV:    ", normalizePath(csv_path,    winslash = "/", mustWork = FALSE)), log_file)
log_line(paste0("DB:     ", normalizePath(db_path,     winslash = "/", mustWork = FALSE)), log_file)
log_line(paste0("Schema: ", normalizePath(schema_path, winslash = "/", mustWork = FALSE)), log_file)

if (!file.exists(csv_path))    stop_with_log(paste0("Clean CSV not found: ", csv_path), log_file)
if (!file.exists(schema_path)) stop_with_log(paste0("Schema SQL not found: ", schema_path), log_file)

# Load the CSV with explicit column classes. read.csv would otherwise
# infer types per column and coerce identifier-like text fields such as
# `clave_umafor` (which contains values like "0101") into integers,
# stripping leading zeros. Explicit classes also pin `parse_notes` as
# text even though it is currently empty.
text_cols    <- c(
  "state", "umafor_code", "mx_inegi_cveecon4", "scientific_name_apg_raw",
  "assignment_level_desc", "equation_code",
  "source_code", "source_reference",
  "equation_raw", "equation_normalized", "equation_numpy",
  "dbh_range_cm_raw", "height_range_m_raw",
  "response_variable", "response_units",
  "parse_status", "parse_notes",
  "source_dataset"
)
integer_cols <- c("assignment_level")
numeric_cols <- c("dbh_min_cm", "dbh_max_cm", "height_min_m", "height_max_m")

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

missing <- setdiff(expected, names(eq))
if (length(missing) > 0) {
  stop_with_log(paste0("CSV missing columns: ", paste(missing, collapse = ", ")), log_file)
}

# Sanity check: identifier-style text fields must not have been coerced
# back to numeric anywhere downstream. This catches future regressions if
# someone changes the load path.
for (tc in text_cols) {
  if (!is.character(eq[[tc]])) {
    stop_with_log(paste0("Column '", tc, "' is not character after load (",
                         class(eq[[tc]])[1], ")"), log_file)
  }
}

# Lookup tables: dedupe on the natural key. When the same key appears with
# both a populated description and NA (because some equation rows did not
# join), prefer the populated value so the lookup keeps a single row per key.
is_blank <- function(x) is.na(x) | as.character(x) == ""

collapse_lookup <- function(df, key_col, val_col) {
  # Treat NA and "" identically for both key and value columns: the CSV
  # round-trip writes NA as "" and reads it back as "".
  df <- df[!is_blank(df[[key_col]]), ]
  df[[val_col]][is_blank(df[[val_col]])] <- NA
  df <- df[order(df[[key_col]], is.na(df[[val_col]])), ]  # non-NA first
  df <- df[!duplicated(df[[key_col]]), ]
  rownames(df) <- NULL
  df
}

assignment_level <- collapse_lookup(
  unique(eq[, c("assignment_level", "assignment_level_desc")]),
  "assignment_level", "assignment_level_desc"
)

source_reference <- collapse_lookup(
  unique(eq[, c("source_code", "source_reference")]),
  "source_code", "source_reference"
)

eq_app <- eq[, expected]

log_line(paste0("Lookup rows: assignment_level=", nrow(assignment_level),
                " source_reference=",            nrow(source_reference)), log_file)
log_line(paste0("Equation rows to load: ", nrow(eq_app)), log_file)

# Surface assignment levels that appear in equations but have no description
# joined. In the current workbook these are 9, 10, 11 (1750 rows total). The
# legacy pipeline silently dropped these into NA-description rows; we log
# them explicitly instead.
unknown_levels <- assignment_level[is.na(assignment_level$assignment_level_desc), "assignment_level"]
if (length(unknown_levels) > 0) {
  log_line(paste0("Assignment levels without lookup description: ",
                  paste(unknown_levels, collapse = ", ")),
           log_file)
}

if (file.exists(db_path)) invisible(file.remove(db_path))

conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
if (!DBI::dbIsValid(conn)) {
  stop_with_log(paste0("Failed to create SQLite connection: ", db_path), log_file)
}
on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

invisible(DBI::dbExecute(conn, "PRAGMA foreign_keys = ON;"))

n_stmts <- execute_sql_script(conn, schema_path)
log_line(paste0("Executed schema statements: ", n_stmts), log_file)

# Append into the schema-created tables (no `overwrite = TRUE`). This
# preserves PRIMARY KEY / FOREIGN KEY constraints, declared TEXT/INTEGER
# types, and the `equation_application_id INTEGER PRIMARY KEY
# AUTOINCREMENT` column from db/schema_volume.sql. The schema statements
# above also create the four indexes; no separate CREATE INDEX is needed.
#
# Insert order matters because of the FK constraints from
# equation_application -> assignment_level / source_reference.
invisible(DBI::dbAppendTable(conn, "assignment_level",     assignment_level))
invisible(DBI::dbAppendTable(conn, "source_reference",     source_reference))
invisible(DBI::dbAppendTable(conn, "equation_application", eq_app))

invisible(DBI::dbExecute(conn, "DELETE FROM db_meta;"))
meta_pairs <- list(
  built_at               = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  csv_path               = csv_path,
  schema_path            = schema_path,
  source_dataset         = "volume_vrtacc_2015_2020",
  n_equation_application = as.character(nrow(eq_app)),
  n_assignment_level     = as.character(nrow(assignment_level)),
  n_source_reference     = as.character(nrow(source_reference)),
  pipeline_version       = "volume_v1"
)
for (k in names(meta_pairs)) {
  invisible(DBI::dbExecute(
    conn,
    "INSERT OR REPLACE INTO db_meta(key, value) VALUES (?, ?);",
    params = list(k, meta_pairs[[k]])
  ))
}

# ---- post-load verification --------------------------------------------------
# Lightweight checks that the live database matches the declared schema and
# that source identifiers were preserved. Each failure aborts the build.

verify <- function(label, ok, detail = "") {
  status <- if (ok) "OK" else "FAIL"
  log_line(paste0("verify[", status, "] ", label,
                  if (nzchar(detail)) paste0(" -- ", detail) else ""),
           log_file)
  if (!ok) stop_with_log(paste0("verification failed: ", label), log_file)
}

# 1. row counts match the in-memory frames
n_eq    <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM equation_application")$n
n_lvl   <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM assignment_level")$n
n_src   <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM source_reference")$n
verify("equation_application row count", n_eq  == nrow(eq_app),
       paste0("expected=", nrow(eq_app), " got=", n_eq))
verify("assignment_level row count",     n_lvl == nrow(assignment_level),
       paste0("expected=", nrow(assignment_level), " got=", n_lvl))
verify("source_reference row count",     n_src == nrow(source_reference),
       paste0("expected=", nrow(source_reference), " got=", n_src))

# 2. expected columns exist in equation_application
live_cols <- DBI::dbListFields(conn, "equation_application")
verify("equation_application has equation_application_id",
       "equation_application_id" %in% live_cols)
for (req in c("equation_numpy", "response_units", "source_dataset")) {
  verify(paste0("equation_application has ", req), req %in% live_cols)
}

# 3. column type contract: umafor_code and parse_notes must remain TEXT
col_info <- DBI::dbGetQuery(conn, "PRAGMA table_info('equation_application')")
get_decl_type <- function(name) {
  hit <- col_info[col_info$name == name, "type"]
  if (length(hit) == 0) "" else hit[1]
}
verify("umafor_code declared TEXT", toupper(get_decl_type("umafor_code")) == "TEXT",
       paste0("declared=", get_decl_type("umafor_code")))
verify("parse_notes declared TEXT", toupper(get_decl_type("parse_notes"))  == "TEXT",
       paste0("declared=", get_decl_type("parse_notes")))

# 4. leading-zero preservation: the volume workbook contains UMAFOR codes that
#    legitimately start with "0" (e.g. "0101"). Confirm at least one such
#    value survived the round-trip.
n_leading_zero <- DBI::dbGetQuery(
  conn,
  "SELECT COUNT(*) AS n
     FROM equation_application
    WHERE umafor_code IS NOT NULL
      AND substr(umafor_code, 1, 1) = '0'"
)$n
verify("umafor_code leading-zero values preserved", n_leading_zero > 0,
       paste0("rows starting with '0' = ", n_leading_zero))

# 5. PK / FK / index presence reported by SQLite itself
pk_eq <- DBI::dbGetQuery(
  conn, "PRAGMA table_info('equation_application')"
)
pk_eq_present <- any(pk_eq$pk > 0 & pk_eq$name == "equation_application_id")
verify("equation_application primary key on equation_application_id",
       pk_eq_present)

fks <- DBI::dbGetQuery(conn, "PRAGMA foreign_key_list('equation_application')")
# db/schema_volume.sql declares exactly two FKs from equation_application
# (-> source_reference, -> assignment_level). Assert the exact count so an
# accidentally added or dropped FK fails this build.
verify("equation_application foreign keys declared", nrow(fks) == 2,
       paste0("fk_count=", nrow(fks)))

idx_rows <- DBI::dbGetQuery(conn, "PRAGMA index_list('equation_application')")
expected_idx <- c("idx_eqapp_taxon", "idx_eqapp_geo",
                  "idx_eqapp_source", "idx_eqapp_level")
missing_idx <- setdiff(expected_idx, idx_rows$name)
verify("equation_application indexes present",
       length(missing_idx) == 0,
       if (length(missing_idx) > 0) paste0("missing=", paste(missing_idx, collapse = ",")) else "")

# 6. spot-check FK integrity: every nivel/fuente in equations must resolve in
#    the lookup tables (or be NULL).
fk_violations <- DBI::dbGetQuery(conn, "PRAGMA foreign_key_check;")
verify("foreign_key_check passes", nrow(fk_violations) == 0,
       paste0("violations=", nrow(fk_violations)))

invisible(DBI::dbExecute(conn, "VACUUM;"))

log_line(paste0("Done. SQLite created at: ",
                normalizePath(db_path, winslash = "/", mustWork = FALSE)),
         log_file)

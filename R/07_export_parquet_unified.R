#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# 07_export_parquet_unified.R
# ------------------------------------------------------------------------------
# Export the unified flat clean table to parquet (snappy and zstd).
#
# Inputs:
#   data_clean/unified/equation_application_unified.csv
#
# Outputs:
#   data_clean/unified/equation_application_unified.snappy.parquet
#   data_clean/unified/equation_application_unified.zstd.parquet
#   logs/07_export_parquet_unified.log
#
# This export is the actual handoff artifact for downstream BigQuery loads.
# It reads the CSV with the same explicit `colClasses` contract used by
# R/06_build_sqlite_unified.R, sanity-checks the in-memory frame, writes
# parquet, then reads the parquet back and asserts that identifier-style
# fields (especially `umafor_code`) survive the round trip as strings with
# leading zeros intact. A bare `read.csv(...)` here would silently coerce
# `umafor_code` ("0101") to integer (101) and reintroduce the legacy bug at
# the parquet layer even though the SQLite layer is correct.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow)
})

DEFAULT_CSV    <- file.path("data_clean", "unified", "equation_application_unified.csv")
DEFAULT_OUTDIR <- file.path("data_clean", "unified")
DEFAULT_LOG    <- file.path("logs", "07_export_parquet_unified.log")

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

args <- commandArgs(trailingOnly = TRUE)
pick_arg <- function(opt_val, args, idx, default) {
  if (!is.na(opt_val) && nzchar(opt_val)) return(opt_val)
  if (length(args) >= idx && nzchar(args[[idx]])) return(args[[idx]])
  default
}

csv_path <- pick_arg(getOption("UNI_PQ_CSV",    NA_character_), args, 1, DEFAULT_CSV)
out_dir  <- pick_arg(getOption("UNI_PQ_OUTDIR", NA_character_), args, 2, DEFAULT_OUTDIR)
log_file <- pick_arg(getOption("UNI_PQ_LOG",    NA_character_), args, 3, DEFAULT_LOG)

dir_create_if_missing(out_dir)
dir_create_if_missing(dirname(log_file))
if (file.exists(log_file)) invisible(file.remove(log_file))

log_line("Starting unified parquet export", log_file)
log_line(paste0("CSV:    ", normalizePath(csv_path, winslash = "/", mustWork = FALSE)), log_file)
log_line(paste0("OutDir: ", normalizePath(out_dir,  winslash = "/", mustWork = FALSE)), log_file)

if (!file.exists(csv_path)) stop_with_log(paste0("CSV not found: ", csv_path), log_file)

# Same column-class contract as R/06_build_sqlite_unified.R.
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

df <- read.csv(
  csv_path,
  stringsAsFactors = FALSE,
  check.names      = FALSE,
  colClasses       = col_classes,
  na.strings       = c("", "NA")
)
log_line(paste0("Rows read: ", nrow(df)), log_file)

for (tc in text_cols) {
  if (!is.character(df[[tc]])) {
    stop_with_log(paste0("Column '", tc, "' is not character before parquet write (",
                         class(df[[tc]])[1], ")"), log_file)
  }
}

snappy_path <- file.path(out_dir, "equation_application_unified.snappy.parquet")
zstd_path   <- file.path(out_dir, "equation_application_unified.zstd.parquet")

write_parquet(df, snappy_path, compression = "snappy")
write_parquet(df, zstd_path,   compression = "zstd")

sizes <- file.info(c(snappy_path, zstd_path))[, "size"]
log_line(paste0("Wrote ", snappy_path, " (", sizes[1], " bytes)"), log_file)
log_line(paste0("Wrote ", zstd_path,   " (", sizes[2], " bytes)"), log_file)

# ---- post-write readback verification ----------------------------------------

verify <- function(label, ok, detail = "") {
  status <- if (ok) "OK" else "FAIL"
  log_line(paste0("verify[", status, "] ", label,
                  if (nzchar(detail)) paste0(" -- ", detail) else ""),
           log_file)
  if (!ok) stop_with_log(paste0("verification failed: ", label), log_file)
}

pq <- as.data.frame(read_parquet(zstd_path))

verify("parquet row count matches CSV", nrow(pq) == nrow(df),
       paste0("csv=", nrow(df), " pq=", nrow(pq)))
verify("parquet umafor_code is character",
       is.character(pq$umafor_code),
       paste0("class=", class(pq$umafor_code)[1]))
n_pq_zero <- sum(!is.na(pq$umafor_code) & substr(pq$umafor_code, 1, 1) == "0")
verify("parquet umafor_code preserves leading zeros", n_pq_zero > 0,
       paste0("rows starting with '0' = ", n_pq_zero))
for (tc in text_cols) {
  verify(paste0("parquet ", tc, " is character"),
         is.character(pq[[tc]]),
         paste0("class=", class(pq[[tc]])[1]))
}
verify("parquet assignment_level is integer-like",
       is.integer(pq$assignment_level) || is.numeric(pq$assignment_level),
       paste0("class=", class(pq$assignment_level)[1]))
for (nc in numeric_cols) {
  verify(paste0("parquet ", nc, " is numeric"),
         is.numeric(pq[[nc]]),
         paste0("class=", class(pq[[nc]])[1]))
}

log_line("Done.", log_file)

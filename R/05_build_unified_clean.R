#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# 05_build_unified_clean.R
# ------------------------------------------------------------------------------
# Combine the volume (infys) and biomass (dina) clean tables into one flat
# clean CSV. Volume rows get null wood-density / WD-fixed equation columns.
# Biomass rows get null geography / range / lookup columns. Both sources keep
# their original equation text and their normalized + numpy-ready forms.
#
# Inputs:
#   data_clean/volume/equation_application_clean.csv
#   data_clean/biomass/biomass_equation_application_clean.csv
#
# Outputs:
#   data_clean/unified/equation_application_unified.csv
#   logs/05_build_unified_clean.log
#
# Run from the repo root:
#   Rscript R/05_build_unified_clean.R
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

DEFAULT_VOL    <- file.path("data_clean", "volume",  "equation_application_clean.csv")
DEFAULT_BIO    <- file.path("data_clean", "biomass", "biomass_equation_application_clean.csv")
DEFAULT_OUT    <- file.path("data_clean", "unified", "equation_application_unified.csv")
DEFAULT_LOG    <- file.path("logs", "05_build_unified_clean.log")

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

# Canonical flat schema. The order here is also the column order of the output
# CSV and of the unified SQLite table, so a downstream reviewer can scan rows
# left-to-right by section: provenance, geography, taxonomy, equation,
# applicability, wood density, response, parse status.
UNIFIED_COLS <- c(
  # provenance
  "source_dataset", "source_record_id",
  # volume-only geography
  "state", "umafor_code", "mx_inegi_cveecon4",
  # taxonomy
  "scientific_name_apg_raw", "scientific_name_standardized",
  # volume-only assignment + reference metadata
  "assignment_level", "assignment_level_desc",
  "equation_code", "source_code", "source_reference",
  # equation text
  "equation_raw", "equation_normalized",
  "equation_numpy", "equation_numpy_wd_fixed",
  # volume-only applicability ranges
  "dbh_range_cm_raw", "dbh_min_cm", "dbh_max_cm",
  "height_range_m_raw", "height_min_m", "height_max_m",
  # biomass-only wood-density columns
  "wood_density_value", "wood_density_units", "wood_density_source",
  # response + parse
  "response_variable", "response_units",
  "parse_status", "parse_notes"
)

args <- commandArgs(trailingOnly = TRUE)
pick_arg <- function(opt_val, args, idx, default) {
  if (!is.na(opt_val) && nzchar(opt_val)) return(opt_val)
  if (length(args) >= idx && nzchar(args[[idx]])) return(args[[idx]])
  default
}

vol_csv <- pick_arg(getOption("UNI_VOL_CSV", NA_character_), args, 1, DEFAULT_VOL)
bio_csv <- pick_arg(getOption("UNI_BIO_CSV", NA_character_), args, 2, DEFAULT_BIO)
out_csv <- pick_arg(getOption("UNI_OUT_CSV", NA_character_), args, 3, DEFAULT_OUT)
log_file <- pick_arg(getOption("UNI_LOG",    NA_character_), args, 4, DEFAULT_LOG)

dir_create_if_missing(dirname(out_csv))
dir_create_if_missing(dirname(log_file))
if (file.exists(log_file)) invisible(file.remove(log_file))

log_line("Starting unified clean build", log_file)
log_line(paste0("Volume CSV:  ", normalizePath(vol_csv, winslash = "/", mustWork = FALSE)), log_file)
log_line(paste0("Biomass CSV: ", normalizePath(bio_csv, winslash = "/", mustWork = FALSE)), log_file)
log_line(paste0("Output CSV:  ", normalizePath(out_csv, winslash = "/", mustWork = FALSE)), log_file)

if (!file.exists(vol_csv)) stop_with_log(paste0("Volume CSV not found: ", vol_csv), log_file)
if (!file.exists(bio_csv)) stop_with_log(paste0("Biomass CSV not found: ", bio_csv), log_file)

# Read both CSVs as character (no inferred types). The unified CSV is itself
# a text artifact; per-column type contracts are enforced when the unified
# SQLite is built in 06_build_sqlite_unified.R.
vol <- read.csv(vol_csv, stringsAsFactors = FALSE, check.names = FALSE,
                colClasses = "character", na.strings = c("", "NA"))
bio <- read.csv(bio_csv, stringsAsFactors = FALSE, check.names = FALSE,
                colClasses = "character", na.strings = c("", "NA"))

log_line(paste0("Volume rows in:  ", nrow(vol)), log_file)
log_line(paste0("Biomass rows in: ", nrow(bio)), log_file)

add_missing_cols <- function(df, cols) {
  for (col in cols) if (!col %in% names(df)) df[[col]] <- NA_character_
  df
}

# Map biomass-source column names onto the canonical taxonomy column names
# used by the unified schema. The volume source already uses
# `scientific_name_apg_raw`; the biomass source uses `species_raw` /
# `species_standardized`. Without this rename the species would be silently
# dropped from the unified rows.
if ("species_raw" %in% names(bio) &&
    !"scientific_name_apg_raw" %in% names(bio)) {
  bio$scientific_name_apg_raw <- bio$species_raw
}
if ("species_standardized" %in% names(bio) &&
    !"scientific_name_standardized" %in% names(bio)) {
  bio$scientific_name_standardized <- bio$species_standardized
}

vol <- add_missing_cols(vol, UNIFIED_COLS)
bio <- add_missing_cols(bio, UNIFIED_COLS)

# Override `source_dataset` to the canonical short names used in the unified
# table. The per-source clean CSVs keep their snapshot-specific identifiers.
vol$source_dataset <- "infys"
bio$source_dataset <- "dina"

# Volume CSV does not carry a row id; the per-row id in the volume SQLite is
# the autoincrement PK. Mint a stable infys_<n> id here so unified rows can be
# traced back to a specific volume CSV row.
if (any(is.na(vol$source_record_id))) {
  vol$source_record_id <- paste0("infys_", sprintf("%05d", seq_len(nrow(vol))))
}

unified <- bind_rows(
  vol[, UNIFIED_COLS, drop = FALSE],
  bio[, UNIFIED_COLS, drop = FALSE]
)

# Diagnostics
n_total      <- nrow(unified)
n_infys      <- sum(unified$source_dataset == "infys")
n_dina       <- sum(unified$source_dataset == "dina")
n_wd_fixed   <- sum(!is.na(unified$equation_numpy_wd_fixed))
n_dina_wd_ok <- sum(unified$source_dataset == "dina" &
                      !is.na(unified$wood_density_value))

log_line(paste0("Total unified rows: ", n_total), log_file)
log_line(paste0("  infys rows:       ", n_infys), log_file)
log_line(paste0("  dina rows:        ", n_dina), log_file)
log_line(paste0("Rows with equation_numpy_wd_fixed populated: ", n_wd_fixed), log_file)
log_line(paste0("Dina rows with wood_density_value present:   ", n_dina_wd_ok), log_file)

# Hard checks: every dina row must have a WD value and a fixed equation.
if (n_dina_wd_ok != n_dina) {
  stop_with_log("Some dina rows are missing wood_density_value", log_file)
}
if (n_wd_fixed != n_dina) {
  stop_with_log("equation_numpy_wd_fixed is not populated for every dina row",
                log_file)
}
# Hard check: every dina row must carry a species name. This catches the
# kind of column-name drift between the biomass clean CSV and the unified
# schema that would otherwise silently drop taxonomy.
n_dina_species_ok <- sum(unified$source_dataset == "dina" &
                           !is.na(unified$scientific_name_apg_raw) &
                           unified$scientific_name_apg_raw != "")
if (n_dina_species_ok != n_dina) {
  stop_with_log("Some dina rows are missing scientific_name_apg_raw", log_file)
}

write_csv(unified, out_csv, na = "")
log_line(paste0("Wrote unified CSV: ", out_csv, " (rows=", n_total, ")"),
         log_file)
log_line("Done.", log_file)

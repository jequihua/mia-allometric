#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# 01_ingest_excel_A2.R
# ------------------------------------------------------------------------------
# Purpose:
#   Ingest the Excel workbook containing applied equations (Mexico),
#   reconstruct the merged header in sheet "Ecuaciones_asignadas",
#   join lookups from "NivelesdeAsignación" and "Referencias",
#   standardize equation predictor tokens, parse applicability ranges,
#   and write a clean staging CSV.
#
# Usage (from repo root):
#   Rscript R/01_ingest_excel_A2.R data_raw/EcuacionesAsignadas_volumen_vrtacc_2015-2020.xlsx
#
# Outputs:
#   data_clean/equation_application_clean.csv
#   logs/01_ingest_excel_A2.log
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

# ---------------------------- helpers -----------------------------------------

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

# Reconstruct headers where:
# - A:H header are in row 1
# - I:J are under merged group header in row 1, with subheaders in row 2
reconstruct_headers_ecuaciones <- function(df_raw) {
  # df_raw is a tibble with col_names = FALSE
  # row 1 and row 2 contain header info
  if (nrow(df_raw) < 3) stop("Not enough rows to reconstruct headers.")
  
  header_r1 <- as.character(df_raw[1, ])
  header_r2 <- as.character(df_raw[2, ])
  
  # Columns A:H = 1:8 use row 1
  cols_main <- 1:8
  # Columns I:J = 9:10 use row 2
  cols_sub  <- 9:10
  
  names_vec <- rep(NA_character_, ncol(df_raw))
  names_vec[cols_main] <- header_r1[cols_main]
  names_vec[cols_sub]  <- header_r2[cols_sub]
  
  # Clean names: trim whitespace and replace multiple spaces
  names_vec <- str_squish(names_vec)
  
  # Safety checks
  if (any(is.na(names_vec))) {
    idx <- which(is.na(names_vec))
    stop(sprintf("Header reconstruction produced NA names at columns: %s",
                 paste(idx, collapse = ", ")))
  }
  
  # Drop first two rows (headers), then set names
  df <- df_raw[-c(1, 2), , drop = FALSE]
  names(df) <- names_vec
  
  df
}

# Parse a range like "7.5-132.5" into numeric min/max; keep NA if not parseable
parse_range_minmax <- function(x) {
  x0 <- str_squish(as.character(x))
  x0[x0 %in% c("", "NA", "N/A", "na", "n/a")] <- NA_character_
  
  # Accept formats: "a-b" (with optional spaces)
  parts <- str_split_fixed(x0, "\\s*-\\s*", 2)
  minv <- suppressWarnings(as.numeric(parts[, 1]))
  maxv <- suppressWarnings(as.numeric(parts[, 2]))
  
  # If no hyphen, min/max become NA
  no_hyphen <- is.na(x0) | !str_detect(x0, "-")
  minv[no_hyphen] <- NA_real_
  maxv[no_hyphen] <- NA_real_
  
  tibble(min = minv, max = maxv)
}

# Standardize predictor tokens in equation text (keep raw too)
normalize_equation_tokens <- function(eq) {
  if (length(eq) == 0) return(eq)
  out <- as.character(eq)
  
  # Normalize spacing
  out <- str_replace_all(out, "\\s+", " ")
  out <- str_squish(out)
  
  # Standardize common function names to lower
  out <- str_replace_all(out, "\\bEXP\\b", "exp")
  out <- str_replace_all(out, "\\bLN\\b", "ln")
  out <- str_replace_all(out, "\\bLOG\\b", "log")
  
  # Standardize DBH token variants to "diam"
  # Note: keep this conservative for this workbook; extend later as you add sources.
  out <- str_replace_all(out, regex("\\bDiametro\\b", ignore_case = TRUE), "diam")
  out <- str_replace_all(out, regex("\\bDi[áa]metro\\b", ignore_case = TRUE), "diam")
  out <- str_replace_all(out, regex("\\bDiam\\b", ignore_case = TRUE), "diam")
  
  # Standardize height token variants to "alt"
  out <- str_replace_all(out, regex("\\bAltura\\b", ignore_case = TRUE), "alt")
  out <- str_replace_all(out, regex("\\bAlt\\b", ignore_case = TRUE), "alt")
  
  out
}

# ---------------------------- main --------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

# Default path: if user didn't pass one, try the common filename in data_raw/
default_input <- file.path("data_raw", "EcuacionesAsignadas_volumen_vrtacc_2015-2020.xlsx")
input_path <- if (length(args) >= 1) args[[1]] else default_input

# Output locations
dir_create_if_missing("data_clean")
dir_create_if_missing("logs")
log_file <- file.path("logs", "01_ingest_excel_A2.log")
if (file.exists(log_file)) file.remove(log_file)

log_line("Starting ingestion (A2) ...", log_file)
log_line(paste0("Input workbook: ", normalizePath(input_path, winslash = "/", mustWork = FALSE)), log_file)

if (!file.exists(input_path)) {
  stop_with_log(paste0("Input file not found: ", input_path), log_file)
}

# Read lookups
log_line("Reading sheet: NivelesdeAsignación", log_file)
levels <- readxl::read_excel(input_path, sheet = "NivelesdeAsignación") %>%
  janitor::clean_names() %>% # optional; needs janitor
  # If janitor isn't installed, comment out clean_names() above and use below mapping.
  rename_with(~.x)

# In case janitor is not installed, do a light fallback:
if (!("nivel_de_asignacion" %in% names(levels))) {
  # Try common names from the sheet (Spanish)
  # Expecting columns similar to: "Nivel de asignación" and "Descripción"
  nm <- names(levels)
  # Find the likely columns
  idx_level <- which(str_detect(tolower(nm), "nivel"))
  idx_desc  <- which(str_detect(tolower(nm), "descr"))
  if (length(idx_level) == 1) levels <- levels %>% rename(nivel_de_asignacion = all_of(nm[idx_level]))
  if (length(idx_desc) == 1)  levels <- levels %>% rename(descripcion = all_of(nm[idx_desc]))
}

levels <- levels %>%
  transmute(
    nivel_asignacion = suppressWarnings(as.integer(nivel_de_asignacion)),
    nivel_asignacion_desc = as.character(descripcion)
  ) %>%
  filter(!is.na(nivel_asignacion))

log_line(paste0("Loaded niveles: ", nrow(levels)), log_file)

log_line("Reading sheet: Referencias", log_file)
refs <- readxl::read_excel(input_path, sheet = "Referencias")

# Normalize reference columns (expecting columns like "Fuente" and "Referencia")
nm_r <- names(refs)
idx_fuente <- which(str_detect(tolower(nm_r), "fuente"))
idx_ref <- which(str_detect(tolower(nm_r), "refer"))

if (length(idx_fuente) != 1 || length(idx_ref) != 1) {
  stop_with_log("Could not unambiguously detect 'Fuente' and 'Referencia' columns in Referencias sheet.", log_file)
}

refs <- refs %>%
  transmute(
    fuente_clave = as.character(.data[[nm_r[idx_fuente]]]),
    fuente_referencia = as.character(.data[[nm_r[idx_ref]]])
  ) %>%
  mutate(
    fuente_clave = str_squish(fuente_clave),
    fuente_referencia = str_squish(fuente_referencia)
  ) %>%
  filter(!is.na(fuente_clave), fuente_clave != "")

log_line(paste0("Loaded referencias: ", nrow(refs)), log_file)

# Read main sheet with merged headers
log_line("Reading sheet: Ecuaciones_asignadas (raw, no headers)", log_file)
eq_raw <- readxl::read_excel(input_path, sheet = "Ecuaciones_asignadas", col_names = FALSE)

# Reconstruct headers and drop first two rows
log_line("Reconstructing headers for Ecuaciones_asignadas", log_file)
eq <- tryCatch(
  reconstruct_headers_ecuaciones(eq_raw),
  error = function(e) stop_with_log(paste0("Header reconstruction failed: ", e$message), log_file)
)

# Rename columns to canonical internal names (Spanish -> snake_case-ish)
# Expected headers in A:H (row 1): Estado, Clave_UMAFOR, CVEECON4, NombreCientifico_APG, Nivel de asignación, Clave_ecuacion, Ecuación, Fuente
# Expected I:J (row 2): Diámetro normal (diam), Altura total (alt)
nm <- names(eq)

# Helper to find a column by pattern
find_col <- function(pattern) {
  hits <- which(str_detect(str_to_lower(nm), pattern))
  if (length(hits) == 1) return(nm[hits])
  NA_character_
}

col_estado   <- find_col("estado")
col_umafor   <- find_col("umafor")
col_cveecon4 <- find_col("cveecon4|cveecon")
col_nombre   <- find_col("nombrecientifico|nombre\\s*cientifico|apg")
col_nivel    <- find_col("nivel")
col_claveeq  <- find_col("clave_?ecuacion|clave\\s*ecuacion")
col_ecuacion <- find_col("ecuaci")
col_fuente   <- find_col("fuente")
col_diamrng  <- find_col("di[aá]metro normal|diam\\)")
col_altrng   <- find_col("altura total|alt\\)")

needed <- c(col_estado, col_umafor, col_cveecon4, col_nombre, col_nivel,
            col_claveeq, col_ecuacion, col_fuente, col_diamrng, col_altrng)

if (any(is.na(needed))) {
  missing <- c("estado","umafor","cveecon4","nombre","nivel","clave_ecuacion","ecuacion","fuente","diam_range","alt_range")[is.na(needed)]
  stop_with_log(paste0("Could not detect required columns: ", paste(missing, collapse = ", ")), log_file)
}

# Build clean table
eq_clean <- eq %>%
  transmute(
    estado = as.character(.data[[col_estado]]),
    clave_umafor = as.character(.data[[col_umafor]]),
    cveecon4 = as.character(.data[[col_cveecon4]]),
    nombrecientifico_apg_raw = as.character(.data[[col_nombre]]),
    nivel_asignacion = suppressWarnings(as.integer(.data[[col_nivel]])),
    clave_ecuacion = as.character(.data[[col_claveeq]]),
    ecuacion_raw = as.character(.data[[col_ecuacion]]),
    fuente_clave = as.character(.data[[col_fuente]]),
    dbh_range_cm_raw = as.character(.data[[col_diamrng]]),
    height_range_m_raw = as.character(.data[[col_altrng]])
  ) %>%
  mutate(
    across(where(is.character), ~ str_squish(.x)),
    response_variable = "VRTAcc_m3",
    ecuacion_normalizada = normalize_equation_tokens(ecuacion_raw)
  )

# Parse ranges into numeric min/max
log_line("Parsing applicability ranges (diam cm, alt m)", log_file)
dbh_mm <- parse_range_minmax(eq_clean$dbh_range_cm_raw)
alt_mm <- parse_range_minmax(eq_clean$height_range_m_raw)

eq_clean <- eq_clean %>%
  bind_cols(
    dbh_min_cm = dbh_mm$min,
    dbh_max_cm = dbh_mm$max,
    alt_min_m = alt_mm$min,
    alt_max_m = alt_mm$max
  ) %>%
  mutate(
    parse_status = case_when(
      is.na(ecuacion_raw) | ecuacion_raw == "" ~ "missing_equation",
      TRUE ~ "ok"
    ),
    parse_notes = NA_character_
  )

# Join lookups
log_line("Joining lookups: NivelesdeAsignación and Referencias", log_file)
eq_clean <- eq_clean %>%
  left_join(levels, by = "nivel_asignacion") %>%
  left_join(refs, by = "fuente_clave")

# Basic diagnostics
n_total <- nrow(eq_clean)
n_missing_ref <- sum(is.na(eq_clean$fuente_referencia))
n_missing_level_desc <- sum(is.na(eq_clean$nivel_asignacion_desc))

log_line(paste0("Rows in clean table: ", n_total), log_file)
log_line(paste0("Rows with missing fuente_referencia: ", n_missing_ref), log_file)
log_line(paste0("Rows with missing nivel_asignacion_desc: ", n_missing_level_desc), log_file)

# Drop fully empty rows (sometimes trailing blanks exist)
eq_clean <- eq_clean %>%
  filter(!(is.na(estado) & is.na(clave_umafor) & is.na(cveecon4) & (is.na(ecuacion_raw) | ecuacion_raw == "")))

# Write output
out_csv <- file.path("data_clean", "equation_application_clean.csv")
log_line(paste0("Writing: ", out_csv), log_file)
readr::write_csv(eq_clean, out_csv, na = "")

log_line("Done.", log_file)
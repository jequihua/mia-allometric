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

LOGS = file.path("logs", "01_ingest_excel_A2.log")
DATARAW = file.path("data_raw", "EcuacionesAsignadas_volumen_vrtacc_2015-2020.xlsx")
DATACLEAN = file.path("data_clean", "equation_application_clean.csv")

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
default_input <- DATARAW

# Allow RStudio runner to pass the path via options()
opt_input <- getOption("A2_INPUT_PATH", default = NA_character_)

input_path <- if (!is.na(opt_input) && nzchar(opt_input)) {
  opt_input
} else if (length(args) >= 1 && nzchar(args[[1]])) {
  args[[1]]
} else {
  default_input
}

# Output locations
dir_create_if_missing("data_clean")
dir_create_if_missing("logs")
log_file <- LOGS
if (file.exists(log_file)) file.remove(log_file)

log_line("Starting ingestion (A2) ...", log_file)
log_line(paste0("Input workbook: ", normalizePath(input_path, winslash = "/", mustWork = FALSE)), log_file)

if (!file.exists(input_path)) {
  stop_with_log(paste0("Input file not found: ", input_path), log_file)
}

# Read lookups
log_line("Reading sheet: NivelesdeAsignación", log_file)

levels_raw <- suppressMessages(
  readxl::read_excel(input_path, sheet = "NivelesdeAsignación", col_names = TRUE, skip = 1)
)

# Drop fully empty columns (Excel often creates blank named columns like ...2)
levels_raw <- levels_raw %>%
  dplyr::select(where(~ !all(is.na(.x))))

# Normalize names for matching
nms <- names(levels_raw)
nms_norm <- nms %>%
  as.character() %>%
  stringr::str_to_lower() %>%
  stringr::str_squish()

# Find level column
idx_level <- which(stringr::str_detect(nms_norm, "nivel") & stringr::str_detect(nms_norm, "asign"))
if (length(idx_level) != 1) idx_level <- which(stringr::str_detect(nms_norm, "nivel"))

if (length(idx_level) != 1) {
  stop_with_log(
    paste0("Could not uniquely detect the 'nivel' column in NivelesdeAsignación. Candidates: ",
           paste(nms[idx_level], collapse = " | ")),
    log_file
  )
}

# Find description column candidates
idx_desc <- which(stringr::str_detect(nms_norm, "descrip"))

if (length(idx_desc) < 1) {
  stop_with_log(
    paste0("Could not detect any 'descripcion' column in NivelesdeAsignación. Columns: ",
           paste(nms, collapse = " | ")),
    log_file
  )
}

# Pick the most text-like description column
pick_desc_col <- function(df, idxs, idx_level) {
  idxs <- setdiff(idxs, idx_level)  # never allow desc == level column
  if (length(idxs) == 1) return(idxs[1])
  
  scores <- sapply(idxs, function(i) {
    v <- as.character(df[[i]])
    v <- v[!is.na(v)]
    if (length(v) == 0) return(-Inf)
    
    v_trim <- stringr::str_squish(v)
    avg_len <- mean(nchar(v_trim))
    
    # fraction of values that look purely numeric (e.g., "1", "2.0")
    frac_numeric <- mean(stringr::str_detect(v_trim, "^[0-9]+(\\.[0-9]+)?$"))
    
    # Score: longer text is better, numeric-heavy is worse
    avg_len - 100 * frac_numeric
  })
  
  idxs[which.max(scores)]
}

idx_desc_best <- pick_desc_col(levels_raw, idx_desc, idx_level)

col_level <- nms[idx_level]
col_desc  <- nms[idx_desc_best]

levels <- levels_raw %>%
  dplyr::transmute(
    nivel_asignacion = suppressWarnings(as.integer(.data[[col_level]])),
    nivel_asignacion_desc = stringr::str_squish(as.character(.data[[col_desc]]))
  ) %>%
  dplyr::filter(!is.na(nivel_asignacion))

# Sanity check: descriptions should not be purely numeric
if (all(stringr::str_detect(levels$nivel_asignacion_desc, "^[0-9]+(\\.[0-9]+)?$"))) {
  stop_with_log(
    paste0("nivel_asignacion_desc looks numeric after parsing. Selected desc column: ", col_desc,
           ". Please check the NivelesdeAsignación sheet layout."),
    log_file
  )
}

log_line(paste0("Loaded niveles: ", nrow(levels), " (desc column = ", col_desc, ")"), log_file)

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
eq_raw <- suppressMessages(
  readxl::read_excel(input_path, sheet = "Ecuaciones_asignadas", col_names = FALSE)
)

log_line("Reconstructing headers for Ecuaciones_asignadas", log_file)
eq <- tryCatch(
  reconstruct_headers_ecuaciones(eq_raw),
  error = function(e) stop_with_log(paste0("Header reconstruction failed: ", e$message), log_file)
)

# ---- Canonical column mapping by position (A–J) ------------------------------
# Map by column position and rename first (so dplyr can refer by string names).

if (ncol(eq) < 10) {
  stop_with_log(
    paste0("Expected at least 10 columns (A–J) after header reconstruction, got: ", ncol(eq)),
    log_file
  )
}

eq10 <- eq[, 1:10]

# Assign fixed names to the first 10 columns (A–J)
names(eq10) <- c(
  "Estado",
  "Clave_UMAFOR",
  "CVEECON4",
  "NombreCientifico_APG",
  "Nivel_asignacion",
  "Clave_ecuacion",
  "Ecuacion",
  "Fuente",
  "Diametro_normal_diam",
  "Altura_total_alt"
)

eq_clean <- eq10 %>%
  transmute(
    estado = as.character(.data[["Estado"]]),
    clave_umafor = as.character(.data[["Clave_UMAFOR"]]),
    cveecon4 = as.character(.data[["CVEECON4"]]),
    nombrecientifico_apg_raw = as.character(.data[["NombreCientifico_APG"]]),
    nivel_asignacion = suppressWarnings(as.integer(.data[["Nivel_asignacion"]])),
    clave_ecuacion = as.character(.data[["Clave_ecuacion"]]),
    ecuacion_raw = as.character(.data[["Ecuacion"]]),
    fuente_clave = as.character(.data[["Fuente"]]),
    dbh_range_cm_raw = as.character(.data[["Diametro_normal_diam"]]),
    height_range_m_raw = as.character(.data[["Altura_total_alt"]])
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
out_csv <- DATACLEAN
log_line(paste0("Writing: ", out_csv), log_file)
readr::write_csv(eq_clean, out_csv, na = "")

log_line("Done.", log_file)
#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# 01_ingest_volume.R
# ------------------------------------------------------------------------------
# Volume-only ingest of the applied-equations workbook (Mexico, VRTAcc).
#
# Inputs:
#   01_data/data/eq_sources/EcuacionesAsignadas_volumen_vrtacc_2015-2020.xlsx
#
# Outputs:
#   data_clean/volume/equation_application_clean.csv
#   logs/01_ingest_volume.log
#
# Run from the repo root:
#   Rscript R/01_ingest_volume.R
#   Rscript R/01_ingest_volume.R <path-to-xlsx> [out-csv] [log-file]
#
# This is a maintained port of
#   90_legacy_review/mia-allometric-main/R/01_ingest_excel_A2.R
# with paths adapted to this repo and conservative behavior preserved.
# Do not modify legacy files; treat them as a frozen reference.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(readr)
})

# ---- defaults (relative to repo root) ----------------------------------------

DEFAULT_INPUT  <- file.path("01_data", "data", "eq_sources",
                            "EcuacionesAsignadas_volumen_vrtacc_2015-2020.xlsx")
DEFAULT_OUTDIR <- file.path("data_clean", "volume")
DEFAULT_OUTCSV <- file.path(DEFAULT_OUTDIR, "equation_application_clean.csv")
DEFAULT_LOG    <- file.path("logs", "01_ingest_volume.log")

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

# Reconstruct the manually merged two-row header in `Ecuaciones_asignadas`:
#   - row 1 holds the header for columns A..H
#   - row 2 holds the subheaders under the merged "Rangos de aplicabilidad"
#     group for columns I..J
reconstruct_headers_ecuaciones <- function(df_raw) {
  if (nrow(df_raw) < 3) stop("Not enough rows to reconstruct headers.")

  header_r1 <- as.character(df_raw[1, ])
  header_r2 <- as.character(df_raw[2, ])

  cols_main <- 1:8
  cols_sub  <- 9:10

  names_vec <- rep(NA_character_, ncol(df_raw))
  names_vec[cols_main] <- header_r1[cols_main]
  names_vec[cols_sub]  <- header_r2[cols_sub]
  names_vec <- str_squish(names_vec)

  if (any(is.na(names_vec))) {
    idx <- which(is.na(names_vec))
    stop(sprintf("Header reconstruction produced NA names at columns: %s",
                 paste(idx, collapse = ", ")))
  }

  df <- df_raw[-c(1, 2), , drop = FALSE]
  names(df) <- names_vec
  df
}

# Parse a range string like "7.5-132.5" into numeric (min, max).
# Returns NA, NA when the value is missing, blank, or has no hyphen.
parse_range_minmax <- function(x) {
  x0 <- str_squish(as.character(x))
  x0[x0 %in% c("", "NA", "N/A", "na", "n/a", "-")] <- NA_character_

  parts <- str_split_fixed(x0, "\\s*-\\s*", 2)
  minv <- suppressWarnings(as.numeric(parts[, 1]))
  maxv <- suppressWarnings(as.numeric(parts[, 2]))

  no_hyphen <- is.na(x0) | !str_detect(x0, "-")
  minv[no_hyphen] <- NA_real_
  maxv[no_hyphen] <- NA_real_

  tibble(min = minv, max = maxv)
}

# Conservative token normalization. Lowercase, squish, and standardize the
# small set of predictor and function names known to vary in this workbook.
normalize_equation_tokens <- function(eq) {
  if (length(eq) == 0) return(eq)
  out <- as.character(eq)

  out <- tolower(out)
  out <- str_replace_all(out, "\\s+", " ")
  out <- str_squish(out)

  # function names (idempotent under tolower; kept for clarity)
  out <- str_replace_all(out, "\\bexp\\b", "exp")
  out <- str_replace_all(out, "\\bln\\b",  "ln")
  out <- str_replace_all(out, "\\blog\\b", "log")

  # diameter token variants -> "diam"
  out <- str_replace_all(out, regex("\\bdiametro\\b",  ignore_case = TRUE), "diam")
  out <- str_replace_all(out, regex("\\bdi[áa]metro\\b", ignore_case = TRUE), "diam")
  out <- str_replace_all(out, regex("\\bdiam\\b",      ignore_case = TRUE), "diam")

  # height token variants -> "alt"
  out <- str_replace_all(out, regex("\\baltura\\b", ignore_case = TRUE), "alt")
  out <- str_replace_all(out, regex("\\balt\\b",    ignore_case = TRUE), "alt")

  out
}

# Convert a normalized equation into a NumPy-evaluable form.
normalize_to_numpy <- function(eq_norm) {
  out <- tolower(as.character(eq_norm))
  out <- str_squish(str_replace_all(out, "\\s+", " "))

  out <- str_replace_all(out, "\\bexp\\s*\\(",      "np.exp(")
  out <- str_replace_all(out, "\\bln\\s*\\(",       "np.log(")
  out <- str_replace_all(out, "\\bpotencia\\s*\\(", "np.power(")
  out <- str_replace_all(out, "\\bpow\\s*\\(",      "np.power(")
  out <- str_replace_all(out, "\\braiz\\s*\\(",     "np.sqrt(")
  out <- str_replace_all(out, "\\bsqrt\\s*\\(",     "np.sqrt(")
  out <- str_replace_all(out, "\\babs\\s*\\(",      "np.abs(")

  out
}

# Score description-column candidates by how text-like they are. Used to
# pick the right "descripcion" column out of `NivelesdeAsignacion` when more
# than one column matches the regex.
pick_desc_col <- function(df, idxs, idx_level) {
  idxs <- setdiff(idxs, idx_level)
  if (length(idxs) == 1) return(idxs[1])

  scores <- sapply(idxs, function(i) {
    v <- as.character(df[[i]])
    v <- v[!is.na(v)]
    if (length(v) == 0) return(-Inf)

    v_trim <- str_squish(v)
    avg_len <- mean(nchar(v_trim))
    frac_numeric <- mean(str_detect(v_trim, "^[0-9]+(\\.[0-9]+)?$"))

    avg_len - 100 * frac_numeric
  })

  idxs[which.max(scores)]
}

# ---- argument parsing --------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

opt_input <- getOption("VOL_INPUT_PATH",  default = NA_character_)
opt_out   <- getOption("VOL_OUTPUT_CSV",  default = NA_character_)
opt_log   <- getOption("VOL_LOG_PATH",    default = NA_character_)

pick_arg <- function(opt_val, args, idx, default) {
  if (!is.na(opt_val) && nzchar(opt_val)) return(opt_val)
  if (length(args) >= idx && nzchar(args[[idx]])) return(args[[idx]])
  default
}

input_path <- pick_arg(opt_input, args, 1, DEFAULT_INPUT)
out_csv    <- pick_arg(opt_out,   args, 2, DEFAULT_OUTCSV)
log_file   <- pick_arg(opt_log,   args, 3, DEFAULT_LOG)

# ---- main --------------------------------------------------------------------

dir_create_if_missing(dirname(out_csv))
dir_create_if_missing(dirname(log_file))
if (file.exists(log_file)) invisible(file.remove(log_file))

log_line("Starting volume ingest", log_file)
log_line(paste0("Input workbook: ",
                normalizePath(input_path, winslash = "/", mustWork = FALSE)),
         log_file)
log_line(paste0("Output CSV:     ",
                normalizePath(out_csv,    winslash = "/", mustWork = FALSE)),
         log_file)

if (!file.exists(input_path)) {
  stop_with_log(paste0("Input file not found: ", input_path), log_file)
}

# --- assignment-level lookup --------------------------------------------------

# The legacy file uses the accented sheet name "NivelesdeAsignación".
# Some environments have rewritten it to "NivelesdeAsignacion". Try both.
log_line("Reading sheet: NivelesdeAsignacion (with accented fallback)", log_file)

read_levels_sheet <- function(path) {
  sheets <- readxl::excel_sheets(path)
  candidates <- c("NivelesdeAsignación", "NivelesdeAsignacion",
                  "NivelesdeAsignación")
  match <- intersect(candidates, sheets)
  if (length(match) == 0) {
    stop("Could not find an assignment-level sheet. Sheets present: ",
         paste(sheets, collapse = " | "))
  }
  suppressMessages(readxl::read_excel(path, sheet = match[[1]],
                                      col_names = TRUE, skip = 1))
}

levels_raw <- tryCatch(
  read_levels_sheet(input_path),
  error = function(e) stop_with_log(conditionMessage(e), log_file)
)

levels_raw <- levels_raw %>% select(where(~ !all(is.na(.x))))

nms <- names(levels_raw)
nms_norm <- str_squish(str_to_lower(as.character(nms)))

idx_level <- which(str_detect(nms_norm, "nivel") & str_detect(nms_norm, "asign"))
if (length(idx_level) != 1) idx_level <- which(str_detect(nms_norm, "nivel"))
if (length(idx_level) != 1) {
  stop_with_log(
    paste0("Could not uniquely detect the 'nivel' column. Candidates: ",
           paste(nms[idx_level], collapse = " | ")),
    log_file)
}

idx_desc <- which(str_detect(nms_norm, "descrip"))
if (length(idx_desc) < 1) {
  stop_with_log(
    paste0("Could not detect any 'descripcion' column. Columns: ",
           paste(nms, collapse = " | ")),
    log_file)
}

idx_desc_best <- pick_desc_col(levels_raw, idx_desc, idx_level)
col_level <- nms[idx_level]
col_desc  <- nms[idx_desc_best]

levels <- levels_raw %>%
  transmute(
    assignment_level      = suppressWarnings(as.integer(.data[[col_level]])),
    assignment_level_desc = str_squish(as.character(.data[[col_desc]]))
  ) %>%
  filter(!is.na(assignment_level))

if (all(str_detect(levels$assignment_level_desc, "^[0-9]+(\\.[0-9]+)?$"))) {
  stop_with_log(
    paste0("assignment_level_desc looks numeric. Selected desc column: ",
           col_desc),
    log_file)
}

log_line(paste0("Loaded niveles: ", nrow(levels),
                " (desc column = ", col_desc, ")"),
         log_file)

# --- references lookup --------------------------------------------------------

log_line("Reading sheet: Referencias", log_file)
refs <- suppressMessages(readxl::read_excel(input_path, sheet = "Referencias"))

nm_r <- names(refs)
idx_fuente <- which(str_detect(tolower(nm_r), "fuente"))
idx_ref    <- which(str_detect(tolower(nm_r), "refer"))

if (length(idx_fuente) != 1 || length(idx_ref) != 1) {
  stop_with_log(
    "Could not unambiguously detect 'Fuente' and 'Referencia' columns.",
    log_file)
}

refs <- refs %>%
  transmute(
    source_code      = as.character(.data[[nm_r[idx_fuente]]]),
    source_reference = as.character(.data[[nm_r[idx_ref]]])
  ) %>%
  mutate(
    source_code      = str_squish(source_code),
    source_reference = str_squish(source_reference)
  ) %>%
  filter(!is.na(source_code), source_code != "")

log_line(paste0("Loaded referencias: ", nrow(refs)), log_file)

# --- main equations sheet -----------------------------------------------------

log_line("Reading sheet: Ecuaciones_asignadas (raw, no headers)", log_file)
eq_raw <- suppressMessages(
  readxl::read_excel(input_path, sheet = "Ecuaciones_asignadas",
                     col_names = FALSE)
)

log_line("Reconstructing headers", log_file)
eq <- tryCatch(
  reconstruct_headers_ecuaciones(eq_raw),
  error = function(e)
    stop_with_log(paste0("Header reconstruction failed: ",
                         conditionMessage(e)), log_file)
)

if (ncol(eq) < 10) {
  stop_with_log(
    paste0("Expected at least 10 columns (A-J) after header reconstruction, ",
           "got: ", ncol(eq)),
    log_file)
}

eq10 <- eq[, 1:10]
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
    state                    = as.character(.data[["Estado"]]),
    umafor_code              = as.character(.data[["Clave_UMAFOR"]]),
    mx_inegi_cveecon4        = as.character(.data[["CVEECON4"]]),
    scientific_name_apg_raw  = as.character(.data[["NombreCientifico_APG"]]),
    assignment_level         = suppressWarnings(as.integer(.data[["Nivel_asignacion"]])),
    equation_code            = as.character(.data[["Clave_ecuacion"]]),
    equation_raw             = as.character(.data[["Ecuacion"]]),
    source_code              = as.character(.data[["Fuente"]]),
    dbh_range_cm_raw         = as.character(.data[["Diametro_normal_diam"]]),
    height_range_m_raw       = as.character(.data[["Altura_total_alt"]])
  ) %>%
  mutate(
    across(where(is.character), ~ str_squish(.x)),
    response_variable    = "VRTAcc",
    response_units       = "m3",
    source_dataset       = "volume_vrtacc_2015_2020",
    equation_normalized  = normalize_equation_tokens(equation_raw),
    equation_numpy       = normalize_to_numpy(equation_normalized)
  )

# --- range parsing ------------------------------------------------------------

log_line("Parsing applicability ranges (diam cm, alt m)", log_file)
dbh_mm <- parse_range_minmax(eq_clean$dbh_range_cm_raw)
alt_mm <- parse_range_minmax(eq_clean$height_range_m_raw)

eq_clean <- eq_clean %>%
  bind_cols(
    dbh_min_cm    = dbh_mm$min,
    dbh_max_cm    = dbh_mm$max,
    height_min_m  = alt_mm$min,
    height_max_m  = alt_mm$max
  ) %>%
  mutate(
    parse_status = case_when(
      is.na(equation_raw) | equation_raw == "" ~ "missing_equation",
      TRUE                                     ~ "ok"
    ),
    parse_notes = NA_character_
  )

# --- joins --------------------------------------------------------------------

log_line("Joining lookups: assignment_level and source_reference", log_file)
eq_clean <- eq_clean %>%
  left_join(levels, by = "assignment_level") %>%
  left_join(refs,   by = "source_code")

# --- diagnostics --------------------------------------------------------------

n_total              <- nrow(eq_clean)
n_missing_ref        <- sum(is.na(eq_clean$source_reference))
n_missing_level_desc <- sum(is.na(eq_clean$assignment_level_desc))
n_missing_eq         <- sum(eq_clean$parse_status == "missing_equation")
n_dbh_parsed         <- sum(!is.na(eq_clean$dbh_min_cm) & !is.na(eq_clean$dbh_max_cm))
n_height_parsed      <- sum(!is.na(eq_clean$height_min_m) & !is.na(eq_clean$height_max_m))

log_line(paste0("Rows in clean table: ", n_total), log_file)
log_line(paste0("Rows with missing source_reference: ",     n_missing_ref), log_file)
log_line(paste0("Rows with missing assignment_level_desc: ", n_missing_level_desc), log_file)
log_line(paste0("Rows with missing equation text: ",         n_missing_eq), log_file)
log_line(paste0("Rows with parsed DBH range: ",              n_dbh_parsed), log_file)
log_line(paste0("Rows with parsed height range: ",           n_height_parsed), log_file)

# Drop fully empty trailing rows
eq_clean <- eq_clean %>%
  filter(!(is.na(state) & is.na(umafor_code) & is.na(mx_inegi_cveecon4) &
           (is.na(equation_raw) | equation_raw == "")))

# --- write --------------------------------------------------------------------

log_line(paste0("Writing: ", out_csv), log_file)
readr::write_csv(eq_clean, out_csv, na = "")

log_line(paste0("Done. Final row count: ", nrow(eq_clean)), log_file)

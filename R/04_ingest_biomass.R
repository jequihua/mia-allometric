#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# 04_ingest_biomass.R
# ------------------------------------------------------------------------------
# Biomass-source ingest: four mangrove equations + exact-match wood density.
#
# Inputs:
#   01_data/data/eq_sources/mangrove_allometric_equations_dina.csv
#   01_data/data/wd_sources/wood_density_values_table29.csv
#
# Outputs:
#   data_clean/biomass/biomass_equation_application_clean.csv
#   logs/04_ingest_biomass.log
#
# Run from the repo root:
#   Rscript R/04_ingest_biomass.R
#   Rscript R/04_ingest_biomass.R <bio-csv> <wd-csv> [<out-csv>] [<log>]
#
# The clean output preserves the original biomass formula in `ecuacion_raw`
# and adds two derived equation strings:
#   * `ecuacion_numpy`           — RHS expression with WD as a free variable
#   * `ecuacion_numpy_wd_fixed`  — RHS expression with WD substituted to the
#                                  numeric value from the wood-density lookup
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
})

DEFAULT_BIO <- file.path("01_data", "data", "eq_sources",
                         "mangrove_allometric_equations_dina.csv")
DEFAULT_WD  <- file.path("01_data", "data", "wd_sources",
                         "wood_density_values_table29.csv")
DEFAULT_OUT <- file.path("data_clean", "biomass",
                         "biomass_equation_application_clean.csv")
DEFAULT_LOG <- file.path("logs", "04_ingest_biomass.log")

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

# Conservative biomass-equation token normalization. The dina source uses the
# tokens `B`, `WD`, `DAP`. We map `DAP` to the volume convention `diam`,
# lowercase the rest, and squish whitespace.
normalize_biomass_tokens <- function(eq) {
  out <- as.character(eq)
  out <- tolower(out)
  out <- str_replace_all(out, "\\s+", " ")
  out <- str_squish(out)
  # predictor token: DAP / DBH variants -> diam (volume convention)
  out <- str_replace_all(out, "\\bdap\\b", "diam")
  out <- str_replace_all(out, "\\bdbh\\b", "diam")
  # response variable kept as `b` to preserve the LHS for traceability
  # wood density kept as `wd`
  out
}

# Convert a normalized biomass equation to a NumPy-evaluable RHS expression.
# Drops the `b = ` LHS, replaces the `^` infix exponent with Python `**`, and
# applies the same function rewrites the volume pipeline uses.
biomass_to_numpy <- function(eq_norm) {
  out <- as.character(eq_norm)
  out <- str_replace(out, "^\\s*b\\s*=\\s*", "")
  out <- str_replace_all(out, "\\^", "**")
  out <- str_replace_all(out, "\\bexp\\s*\\(",      "np.exp(")
  out <- str_replace_all(out, "\\bln\\s*\\(",       "np.log(")
  out <- str_replace_all(out, "\\bpotencia\\s*\\(", "np.power(")
  out <- str_replace_all(out, "\\bpow\\s*\\(",      "np.power(")
  out <- str_replace_all(out, "\\braiz\\s*\\(",     "np.sqrt(")
  out <- str_replace_all(out, "\\bsqrt\\s*\\(",     "np.sqrt(")
  out <- str_replace_all(out, "\\babs\\s*\\(",      "np.abs(")
  str_squish(out)
}

# Substitute the bare token `wd` with the numeric value (formatted plainly).
substitute_wd <- function(numpy_expr, wd_value) {
  if (is.na(wd_value)) return(NA_character_)
  literal <- format(wd_value, scientific = FALSE, trim = TRUE)
  str_replace_all(numpy_expr, "\\bwd\\b", literal)
}

# ---- argument parsing --------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

opt_bio <- getOption("BIO_INPUT_PATH",  default = NA_character_)
opt_wd  <- getOption("BIO_WD_PATH",     default = NA_character_)
opt_out <- getOption("BIO_OUTPUT_CSV",  default = NA_character_)
opt_log <- getOption("BIO_LOG_PATH",    default = NA_character_)

pick_arg <- function(opt_val, args, idx, default) {
  if (!is.na(opt_val) && nzchar(opt_val)) return(opt_val)
  if (length(args) >= idx && nzchar(args[[idx]])) return(args[[idx]])
  default
}

bio_path <- pick_arg(opt_bio, args, 1, DEFAULT_BIO)
wd_path  <- pick_arg(opt_wd,  args, 2, DEFAULT_WD)
out_csv  <- pick_arg(opt_out, args, 3, DEFAULT_OUT)
log_file <- pick_arg(opt_log, args, 4, DEFAULT_LOG)

# ---- main --------------------------------------------------------------------

dir_create_if_missing(dirname(out_csv))
dir_create_if_missing(dirname(log_file))
if (file.exists(log_file)) invisible(file.remove(log_file))

log_line("Starting biomass ingest", log_file)
log_line(paste0("Biomass CSV:    ", normalizePath(bio_path, winslash = "/", mustWork = FALSE)), log_file)
log_line(paste0("Wood density:   ", normalizePath(wd_path,  winslash = "/", mustWork = FALSE)), log_file)
log_line(paste0("Output CSV:     ", normalizePath(out_csv,  winslash = "/", mustWork = FALSE)), log_file)

if (!file.exists(bio_path)) stop_with_log(paste0("Biomass CSV not found: ", bio_path), log_file)
if (!file.exists(wd_path))  stop_with_log(paste0("Wood density CSV not found: ", wd_path), log_file)

bio <- read.csv(bio_path, stringsAsFactors = FALSE, check.names = FALSE)
wd  <- read.csv(wd_path,  stringsAsFactors = FALSE, check.names = FALSE)

if (!all(c("Species", "EquationBiomass") %in% names(bio))) {
  stop_with_log(paste0("Biomass CSV missing required columns. Got: ",
                       paste(names(bio), collapse = ", ")), log_file)
}
if (!all(c("Scientific Name", "Wood Density (tonne/m3)") %in% names(wd))) {
  stop_with_log(paste0("Wood density CSV missing required columns. Got: ",
                       paste(names(wd), collapse = ", ")), log_file)
}

# Trim whitespace on join keys so an accidental trailing space does not silently
# break the join.
bio$Species         <- str_squish(bio$Species)
wd$`Scientific Name` <- str_squish(wd$`Scientific Name`)

log_line(paste0("Biomass rows: ", nrow(bio)), log_file)
log_line(paste0("Wood density rows: ", nrow(wd)), log_file)

joined <- bio %>%
  rename(species_raw = Species, equation_biomass = EquationBiomass) %>%
  left_join(
    wd %>% select(species_raw = `Scientific Name`,
                  wood_density_value = `Wood Density (tonne/m3)`),
    by = "species_raw"
  )

# Hard-fail on any unmatched species. The dina file is small and curated, so
# any miss here is a real data issue worth surfacing rather than logging away.
unmatched <- joined %>% filter(is.na(wood_density_value)) %>% pull(species_raw)
if (length(unmatched) > 0) {
  stop_with_log(
    paste0("Wood density not found for: ", paste(unmatched, collapse = "; ")),
    log_file
  )
}
log_line(paste0("All ", nrow(joined), " biomass species matched wood density exactly."),
         log_file)

biomass_clean <- joined %>%
  mutate(
    source_dataset           = "dina",
    source_record_id         = paste0("dina_", sprintf("%03d", seq_len(n()))),
    species_standardized     = species_raw,
    equation_raw             = equation_biomass,
    equation_normalized      = normalize_biomass_tokens(equation_biomass),
    equation_numpy           = biomass_to_numpy(equation_normalized),
    equation_numpy_wd_fixed  = mapply(substitute_wd, equation_numpy, wood_density_value,
                                      USE.NAMES = FALSE),
    wood_density_units       = "tonne/m3",
    wood_density_source      = basename(wd_path),
    response_variable        = "B",
    response_units           = "kg",  # Chave et al. style mangrove eqs return kg dry biomass
    parse_status             = "ok",
    parse_notes              = NA_character_
  ) %>%
  select(
    source_dataset, source_record_id,
    species_raw, species_standardized,
    equation_raw, equation_normalized,
    equation_numpy, equation_numpy_wd_fixed,
    wood_density_value, wood_density_units, wood_density_source,
    response_variable, response_units,
    parse_status, parse_notes
  )

# ---- diagnostics -------------------------------------------------------------

for (i in seq_len(nrow(biomass_clean))) {
  log_line(paste0(
    "  ", biomass_clean$species_raw[i],
    "  WD=",     biomass_clean$wood_density_value[i],
    "  raw=`",   biomass_clean$equation_raw[i],          "`",
    "  numpy=`", biomass_clean$equation_numpy[i],        "`",
    "  fixed=`", biomass_clean$equation_numpy_wd_fixed[i], "`"
  ), log_file)
}

log_line(paste0("Writing: ", out_csv), log_file)
write_csv(biomass_clean, out_csv, na = "")

log_line(paste0("Done. Final row count: ", nrow(biomass_clean)), log_file)

# R/run_ingest_A2.R
# Run A2 ingestion from RStudio (no command line needed)

# --- set working directory to repo root (based on this script's path) ----------
get_script_dir <- function() {
  # Works in RStudio. If it fails, falls back to current working directory.
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- rstudioapi::getActiveDocumentContext()
    if (!is.null(ctx$path) && nzchar(ctx$path)) return(dirname(ctx$path))
  }
  # If running via source(), try sys.frames
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(sys.frames()[[1]]$ofile))
  getwd()
}

script_dir <- get_script_dir()

# This runner is in R/, so repo root is its parent
repo_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
setwd(repo_root)

message("Working directory set to: ", getwd())

# --- configure input workbook path -------------------------------------------
input_path <- file.path("data_raw", "EcuacionesAsignadas_volumen_vrtacc_2015-2020.xlsx")

if (!file.exists(input_path)) {
  stop("Input file not found at: ", input_path, "\n",
       "Place the workbook under data_raw/ or update input_path.")
}

# --- run the original script as if from command line --------------------------
# We temporarily set commandArgs() via a small trick: source a copy that reads from `args`.
# To avoid modifying your original script, we set an option the script can read if present.
options(A2_INPUT_PATH = input_path)

source(file.path("R", "01_ingest_excel_A2.R"), local = new.env(parent = globalenv()))

message("A2 ingestion finished. Check:")
message(" - data_clean/equation_application_clean.csv")
message(" - logs/01_ingest_excel_A2.log")
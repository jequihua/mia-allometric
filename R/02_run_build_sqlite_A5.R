# R/run_build_sqlite_A5.R
# ------------------------------------------------------------
# Run A5 SQLite build script from RStudio (no command line)
# ------------------------------------------------------------

# ---- Detect script directory (works in RStudio) ------------------------------

get_script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    ctx <- rstudioapi::getActiveDocumentContext()
    if (!is.null(ctx$path) && nzchar(ctx$path)) {
      return(dirname(ctx$path))
    }
  }
  # fallback
  getwd()
}

CSVPATH = file.path("data_clean", "equation_application_clean.csv")
DBPATH = file.path("db", "allometry.sqlite")

script_dir <- get_script_dir()

# This runner lives in R/, so repo root is its parent
repo_root <- normalizePath(file.path(script_dir, ".."),
                           winslash = "/", mustWork = FALSE)

setwd(repo_root)

cat("Working directory set to:\n", getwd(), "\n\n")

# ---- Configure paths ----------------------------------------------------------

csv_path <- CSVPATH
db_path  <- DBPATH

if (!file.exists(csv_path)) {
  stop("Clean CSV not found at: ", csv_path,
       "\nRun the ingestion script first.")
}

if (!file.exists(file.path("db", "schema_A5.sql"))) {
  stop("Schema file not found at db/schema_A5.sql")
}

# ---- Pass arguments to build script ------------------------------------------

options(A5_CSV_PATH = csv_path)
options(A5_DB_PATH  = db_path)

# ---- Run the main build script ------------------------------------------------

source(file.path("R", "02_build_sqlite_A5.R"),
       local = new.env(parent = globalenv()))

cat("\nSQLite build finished.\n")
cat("Database created at:\n", normalizePath(db_path, winslash = "/", mustWork = FALSE), "\n")
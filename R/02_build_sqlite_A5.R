#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

execute_sql_script <- function(conn, sql_file) {
  sql_lines <- readLines(sql_file, warn = FALSE)
  
  # Remove full-line comments starting with --
  sql_lines <- sql_lines[!grepl("^\\s*--", sql_lines)]
  
  sql_all <- paste(sql_lines, collapse = "\n")
  
  # Split on semicolons; execute each statement
  stmts <- strsplit(sql_all, ";", fixed = TRUE)[[1]]
  stmts <- trimws(stmts)
  stmts <- stmts[nzchar(stmts)]
  
  for (s in stmts) {
    DBI::dbExecute(conn, s)
  }
  
  invisible(length(stmts))
}

build_sqlite_a5 <- function(csv_path, db_path, schema_path) {
  
  message("CSV: ", normalizePath(csv_path, winslash = "/", mustWork = FALSE))
  message("DB:  ", normalizePath(db_path, winslash = "/", mustWork = FALSE))
  
  if (!file.exists(csv_path)) stop("Clean CSV not found: ", csv_path)
  if (!file.exists(schema_path)) stop("Schema SQL not found: ", schema_path)
  
  if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  
  eq <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  expected <- c(
    "estado","clave_umafor","cveecon4","nombrecientifico_apg_raw",
    "nivel_asignacion","nivel_asignacion_desc",
    "clave_ecuacion",
    "fuente_clave","fuente_referencia",
    "ecuacion_raw","ecuacion_normalizada",
    "dbh_range_cm_raw","dbh_min_cm","dbh_max_cm",
    "height_range_m_raw","alt_min_m","alt_max_m",
    "response_variable","parse_status","parse_notes"
  )
  
  missing <- setdiff(expected, names(eq))
  if (length(missing) > 0) stop("CSV missing columns: ", paste(missing, collapse = ", "))
  
  eq$nivel_asignacion <- suppressWarnings(as.integer(eq$nivel_asignacion))
  for (cc in c("dbh_min_cm","dbh_max_cm","alt_min_m","alt_max_m")) {
    eq[[cc]] <- suppressWarnings(as.numeric(eq[[cc]]))
  }
  
  assignment_level <- unique(eq[, c("nivel_asignacion","nivel_asignacion_desc")])
  assignment_level <- assignment_level[order(assignment_level$nivel_asignacion), ]
  
  source_reference <- unique(eq[, c("fuente_clave","fuente_referencia")])
  source_reference <- source_reference[order(source_reference$fuente_clave), ]
  
  eq_app <- eq[, expected]
  
  if (file.exists(db_path)) file.remove(db_path)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  
  message("Connection class: ", paste(class(conn), collapse = ", "))
  message("dbIsValid(conn): ", DBI::dbIsValid(conn))
  
  if (!DBI::dbIsValid(conn)) {
    stop("Failed to create a valid SQLite connection to: ", db_path)
  }
  
  # Always disconnect
  on.exit({
    try(DBI::dbDisconnect(conn), silent = TRUE)
  }, add = TRUE)
  
  # Now do the build
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON;")
  
  n_stmts <- execute_sql_script(conn, schema_path)
  message("Executed schema statements: ", n_stmts)
  
  DBI::dbWriteTable(conn, "assignment_level", assignment_level, overwrite = TRUE)
  DBI::dbWriteTable(conn, "source_reference", source_reference, overwrite = TRUE)
  DBI::dbWriteTable(conn, "equation_application", eq_app, overwrite = TRUE)
  
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_eqapp_taxon ON equation_application(nombrecientifico_apg_raw);")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_eqapp_geo ON equation_application(estado, clave_umafor, cveecon4);")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_eqapp_fuente ON equation_application(fuente_clave);")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_eqapp_nivel ON equation_application(nivel_asignacion);")
  
  DBI::dbExecute(conn, "DELETE FROM db_meta;")
  DBI::dbExecute(conn, "INSERT OR REPLACE INTO db_meta(key,value) VALUES ('built_at', datetime('now'));")
  DBI::dbExecute(conn, sprintf(
    "INSERT OR REPLACE INTO db_meta(key,value) VALUES ('csv_path', '%s');",
    gsub("'", "''", csv_path)
  ))
  DBI::dbExecute(conn, sprintf(
    "INSERT OR REPLACE INTO db_meta(key,value) VALUES ('n_equation_application', '%s');",
    nrow(eq_app)
  ))
  
  DBI::dbExecute(conn, "VACUUM;")
  
  message("Done. SQLite created at: ", normalizePath(db_path, winslash = "/", mustWork = FALSE))
  invisible(TRUE)
}

# --- main (supports options + args) -------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

opt_csv <- getOption("A5_CSV_PATH", default = NA_character_)
opt_db  <- getOption("A5_DB_PATH",  default = NA_character_)

csv_path <- if (!is.na(opt_csv) && nzchar(opt_csv)) {
  opt_csv
} else if (length(args) >= 1 && nzchar(args[[1]])) {
  args[[1]]
} else {
  file.path("data_clean", "equation_application_clean.csv")
}

db_path <- if (!is.na(opt_db) && nzchar(opt_db)) {
  opt_db
} else if (length(args) >= 2 && nzchar(args[[2]])) {
  args[[2]]
} else {
  file.path("db", "allometry.sqlite")
}

schema_path <- file.path("db", "schema_A5.sql")

tryCatch(
  build_sqlite_a5(csv_path, db_path, schema_path),
  error = function(e) stop("SQLite build failed: ", conditionMessage(e))
)


-- db/schema_volume.sql
-- Maintained volume-only schema. Adapted from the legacy
-- 90_legacy_review/mia-allometric-main/db/schema_A5.sql with these
-- intentional differences (see 05_governance/decision_log.md):
--
--   * `equation_application.equation_numpy` is persisted (the legacy
--     schema and builder dropped it even though the legacy README and
--     ingest both produce it).
--   * `response_units` and `source_dataset` are persisted so the volume
--     row carries enough provenance to coexist with the future biomass
--     source without ambiguity.
--   * Column names are English; the original Spanish raw-source column
--     names (`estado`, `clave_umafor`, `cveecon4`, `nombrecientifico_*`,
--     `nivel_asignacion*`, `clave_ecuacion`, `fuente_*`, `ecuacion_*`,
--     `alt_*`) have been translated. See README.md "Source-language note".
--
-- This schema describes only the volume source. The biomass + unified
-- pipeline lives in db/schema_unified.sql.

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS source_reference (
  source_code      TEXT PRIMARY KEY,
  source_reference TEXT
);

CREATE TABLE IF NOT EXISTS assignment_level (
  assignment_level      INTEGER PRIMARY KEY,
  assignment_level_desc TEXT
);

CREATE TABLE IF NOT EXISTS equation_application (
  equation_application_id INTEGER PRIMARY KEY AUTOINCREMENT,

  state                    TEXT,
  umafor_code              TEXT,
  mx_inegi_cveecon4        TEXT,
  scientific_name_apg_raw  TEXT,

  assignment_level         INTEGER,
  equation_code            TEXT,

  source_code              TEXT,

  equation_raw             TEXT,
  equation_normalized      TEXT,
  equation_numpy           TEXT,

  dbh_range_cm_raw         TEXT,
  dbh_min_cm               REAL,
  dbh_max_cm               REAL,

  height_range_m_raw       TEXT,
  height_min_m             REAL,
  height_max_m             REAL,

  response_variable        TEXT,
  response_units           TEXT,

  parse_status             TEXT,
  parse_notes              TEXT,

  source_dataset           TEXT,

  -- denormalized lookup copies (kept as in legacy for query convenience)
  assignment_level_desc    TEXT,
  source_reference         TEXT,

  FOREIGN KEY (assignment_level) REFERENCES assignment_level(assignment_level),
  FOREIGN KEY (source_code)      REFERENCES source_reference(source_code)
);

CREATE INDEX IF NOT EXISTS idx_eqapp_taxon
  ON equation_application(scientific_name_apg_raw);

CREATE INDEX IF NOT EXISTS idx_eqapp_geo
  ON equation_application(state, umafor_code, mx_inegi_cveecon4);

CREATE INDEX IF NOT EXISTS idx_eqapp_source
  ON equation_application(source_code);

CREATE INDEX IF NOT EXISTS idx_eqapp_level
  ON equation_application(assignment_level);

CREATE TABLE IF NOT EXISTS db_meta (
  key   TEXT PRIMARY KEY,
  value TEXT
);

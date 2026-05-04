-- db/schema_unified.sql
-- Maintained unified flat schema for the two-source pipeline.
-- One table covers both `infys` (volume) and `dina` (biomass) rows.
--
-- Volume-only fields (geography, ranges, lookups) are NULL for `dina` rows.
-- Biomass-only fields (`wood_density_value`, `wood_density_units`,
-- `wood_density_source`, `equation_numpy_wd_fixed`) are NULL for `infys`
-- rows.
--
-- Column names are English. The original Spanish raw-source columns
-- (`estado`, `clave_umafor`, `cveecon4`, `nombrecientifico_*`,
-- `nivel_asignacion*`, `clave_ecuacion`, `fuente_*`, `ecuacion_*`,
-- `alt_*`) have been translated. See README.md "Source-language note"
-- for the predictor token convention used inside equation strings
-- (`alt`/`altura`/`height`/`h`, `diam`/`diámetro`/`DAP`/`DBH`).
--
-- The `equation_numpy_wd_fixed` column carries a derived NumPy-evaluable
-- expression with `WD` already substituted to the species' wood-density
-- value from db/../wd_sources/wood_density_values_table29.csv. The original
-- biomass formula is preserved unchanged in `equation_raw`. See
-- 05_governance/decision_log.md for the rationale.
--
-- Decision: the unified database is intentionally flat. No
-- `assignment_level` or `source_reference` lookup tables. Volume rows
-- carry the denormalized `assignment_level_desc` and `source_reference`
-- directly. The volume-only SQLite (`db/allometry_volume.sqlite`) keeps
-- the relational form for the volume milestone deliverable; this file
-- describes the canonical final maintained product.

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS equation_application (
  equation_application_id  INTEGER PRIMARY KEY AUTOINCREMENT,

  -- provenance
  source_dataset           TEXT NOT NULL,        -- "infys" or "dina"
  source_record_id         TEXT,                 -- per-source stable id

  -- volume-only geography
  state                    TEXT,
  umafor_code              TEXT,
  mx_inegi_cveecon4        TEXT,

  -- taxonomy
  scientific_name_apg_raw       TEXT,
  scientific_name_standardized  TEXT,

  -- volume-only assignment / reference metadata
  assignment_level         INTEGER,
  assignment_level_desc    TEXT,
  equation_code            TEXT,
  source_code              TEXT,
  source_reference         TEXT,

  -- equation text
  equation_raw             TEXT,
  equation_normalized      TEXT,
  equation_numpy           TEXT,
  equation_numpy_wd_fixed  TEXT,                 -- biomass-only

  -- volume-only applicability ranges
  dbh_range_cm_raw         TEXT,
  dbh_min_cm               REAL,
  dbh_max_cm               REAL,
  height_range_m_raw       TEXT,
  height_min_m             REAL,
  height_max_m             REAL,

  -- biomass-only wood density
  wood_density_value       REAL,
  wood_density_units       TEXT,
  wood_density_source      TEXT,

  -- response + parse
  response_variable        TEXT,
  response_units           TEXT,
  parse_status             TEXT,
  parse_notes              TEXT
);

CREATE INDEX IF NOT EXISTS idx_uni_source
  ON equation_application(source_dataset);

CREATE INDEX IF NOT EXISTS idx_uni_taxon
  ON equation_application(scientific_name_apg_raw);

CREATE INDEX IF NOT EXISTS idx_uni_geo
  ON equation_application(state, umafor_code, mx_inegi_cveecon4);

CREATE INDEX IF NOT EXISTS idx_uni_source_code
  ON equation_application(source_code);

CREATE INDEX IF NOT EXISTS idx_uni_level
  ON equation_application(assignment_level);

CREATE TABLE IF NOT EXISTS db_meta (
  key   TEXT PRIMARY KEY,
  value TEXT
);

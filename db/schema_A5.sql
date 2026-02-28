-- db/schema_A5.sql
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS source_reference (
  fuente_clave TEXT PRIMARY KEY,
  fuente_referencia TEXT
);

CREATE TABLE IF NOT EXISTS assignment_level (
  nivel_asignacion INTEGER PRIMARY KEY,
  nivel_asignacion_desc TEXT
);

CREATE TABLE IF NOT EXISTS equation_application (
  equation_application_id INTEGER PRIMARY KEY AUTOINCREMENT,

  estado TEXT,
  clave_umafor TEXT,
  cveecon4 TEXT,
  nombrecientifico_apg_raw TEXT,

  nivel_asignacion INTEGER,
  clave_ecuacion TEXT,

  fuente_clave TEXT,

  ecuacion_raw TEXT,
  ecuacion_normalizada TEXT,

  dbh_range_cm_raw TEXT,
  dbh_min_cm REAL,
  dbh_max_cm REAL,

  height_range_m_raw TEXT,
  alt_min_m REAL,
  alt_max_m REAL,

  response_variable TEXT,

  parse_status TEXT,
  parse_notes TEXT,

  -- convenience denormalized copies (optional but useful)
  nivel_asignacion_desc TEXT,
  fuente_referencia TEXT,

  FOREIGN KEY (nivel_asignacion) REFERENCES assignment_level(nivel_asignacion),
  FOREIGN KEY (fuente_clave) REFERENCES source_reference(fuente_clave)
);

CREATE INDEX IF NOT EXISTS idx_eqapp_taxon
  ON equation_application(nombrecientifico_apg_raw);

CREATE INDEX IF NOT EXISTS idx_eqapp_geo
  ON equation_application(estado, clave_umafor, cveecon4);

CREATE INDEX IF NOT EXISTS idx_eqapp_fuente
  ON equation_application(fuente_clave);

CREATE INDEX IF NOT EXISTS idx_eqapp_nivel
  ON equation_application(nivel_asignacion);

CREATE TABLE IF NOT EXISTS db_meta (
  key TEXT PRIMARY KEY,
  value TEXT
);
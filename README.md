# Allometric Equations SQLite (Mexico)

This repository builds a reproducible R pipeline to ingest, clean, standardize, and store forestry allometric equations applied in Mexico.

The current phase focuses on a structured Excel workbook containing volume equations (VRTAcc). The pipeline:

1. Ingests the workbook robustly  
2. Cleans and standardizes fields  
3. Normalizes equation predictor syntax  
4. Parses applicability ranges  
5. Builds a relational SQLite database  

The first batch of data was downloaded from:

https://snif.cnf.gob.mx/datos-abiertos/

---

# Project Goal

Create a consistent, auditable, and extensible database of allometric equations, starting with one curated source and later expanding to:

- Multi-source ingestion (Excel, CSV, TXT, scientific publications)
- Additional response variables (AGB, BGB, carbon)
- Cross-source equation harmonization
- Structured model parsing

---

# Current Scope (Phase A – Single Workbook)

This phase processes one Excel file containing:

- `Ecuaciones_asignadas`  
  Applied equations by geography and taxon.

- `NivelesdeAsignación`  
  Assignment level definitions (1–8).

- `Referencias`  
  Maps `Fuente` keys to bibliographic labels.

- `Notas_técnicas`  
  Context: equations estimate VRTAcc; predictors are diameter (`diam`, cm) and height (`alt`, m).

---

# Important Technical Details

## Robust Header Reconstruction

The sheet `Ecuaciones_asignadas` contains merged headers created manually in Excel.

The ingestion script:

1. Reads the sheet with `col_names = FALSE`
2. Reconstructs headers using:
   - Row 1 for columns A–H
   - Row 2 for subheaders under “Rangos de aplicabilidad”
3. Removes the first two rows before cleaning

This avoids fragile behavior from Excel header merging and ensures reproducibility.

---

# What Was Achieved (Phase A2–A5)

## A2 – Ingestion and Cleaning

Script:

R/01_ingest_excel_A2.R


Outputs:

data_clean/equation_application_clean.csv
logs/01_ingest_excel_A2.log


Features:
- Robust header reconstruction
- Automatic detection of lookup columns
- Joins:
  - `nivel_asignacion_desc`
  - `fuente_referencia`
- Preservation of raw values

---

## A3 – Equation Normalization

Each equation is stored twice:

- `ecuacion_raw` (verbatim)
- `ecuacion_normalizada`

Normalization rules include:

- Diam, Diametro, Diámetro, DN, dap → `diam`
- Alt, Altura, altura, Ht → `alt`
- `LN(` → `ln(`
- `EXP(` → `exp(`

This creates a reusable normalization layer for future multi-source ingestion.

---

## A4 – Range Parsing

Applicability ranges like:


7.5-132.5


are parsed into numeric fields:

- `dbh_min_cm`
- `dbh_max_cm`
- `alt_min_m`
- `alt_max_m`

Raw range strings are preserved.

---

## A5 – SQLite Database Build

Script:

R/02_build_sqlite_A5.R


Creates:

db/allometry.sqlite


### Database Structure

#### assignment_level
- nivel_asignacion (PK)
- nivel_asignacion_desc

#### source_reference
- fuente_clave (PK)
- fuente_referencia

#### equation_application
- equation_application_id (PK)
- estado
- clave_umafor
- cveecon4
- nombrecientifico_apg_raw
- nivel_asignacion (FK)
- clave_ecuacion
- fuente_clave (FK)
- ecuacion_raw
- ecuacion_normalizada
- dbh_range_cm_raw
- dbh_min_cm
- dbh_max_cm
- height_range_m_raw
- alt_min_m
- alt_max_m
- response_variable
- parse_status
- parse_notes

Indexes are created for:
- Taxon
- Geography
- Source
- Assignment level

The build process:
- Executes schema statement-by-statement
- Enforces foreign keys
- Compacts the database (`VACUUM`)
- Records metadata in `db_meta`

---

# File Size Note

For ~20,000 rows, the SQLite database is approximately 10 MB.

This is normal because:

- SQLite does not compress text
- Equation strings are stored per row
- Indexes add storage overhead
- Text fields are not dictionary-encoded

For analytical workflows, exporting to Parquet can significantly reduce file size due to columnar compression.

---

# Workflow

## 1) Place Raw File

Put the Excel workbook into:


data_raw/


Do not edit the raw file.

---

## 2) Run Ingestion

Command line:


Rscript R/01_ingest_excel_A2.R data_raw/EcuacionesAsignadas_volumen_vrtacc_2015-2020.xlsx


Or from RStudio:


source("R/01_ingest_excel_A2.R")


---

## 3) Build SQLite Database

Command line:


Rscript R/02_build_sqlite_A5.R


Or from RStudio:


source("R/02_build_sqlite_A5.R")


Output:


db/allometry.sqlite


---

# Suggested Repository Structure


mia-allometric/
├── README.md
├── data_raw/
├── data_clean/
├── db/
├── logs/
├── R/
│ ├── 01_ingest_excel_A2.R
│ └── 02_build_sqlite_A5.R


---

# Provenance and Reproducibility

The project preserves:

- Raw equation strings
- Assignment level definitions
- Source identifiers
- Reference labels

Future extensions will include:

- File hashing
- Multi-source ingestion metadata
- Bibliographic metadata extraction
- License tracking

---

# Future Phases

Phase B (planned):

- Biomass equations (AGB, BGB)
- Separation of:
  - equation definition
  - geographic application scope
- Structured model parsing
- Multi-source harmonization

---

# Keywords

Manglaria · forestry · allometry · volume equations · sqlite · Mexico · SNIF · CONAFOR · VRTAcc

---

# License

MIT License



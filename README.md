# Allometric Equations SQLite (Mexico) – Mini Project

This repository builds a simple, reproducible pipeline (in R) to ingest a curated Excel workbook
containing forestry allometric equations applied in Mexico (initially volume equations for VRTAcc),
clean and standardize key fields, and export a “clean staging” dataset ready to be loaded into an
SQLite database.

The first batch of data was downloaded from:

https://snif.cnf.gob.mx/datos-abiertos/

---

## Project Goal

Create a consistent and auditable dataset of allometric equations (starting with one ordered source),
keeping strong provenance and preparing for later extension to:

- Multi-source ingestion (Excel, CSV, TXT, scientific publications)
- Additional response variables (e.g., biomass AGB/BGB, carbon)

---

## Current Scope (Phase A: Single Workbook)

This first phase focuses on one Excel file containing:

- `Ecuaciones_asignadas`  
  Main data table: applied equations by geography and taxon.
  
- `NivelesdeAsignación`  
  Lookup table: definitions for assignment levels 1–8.
  
- `Referencias`  
  Lookup table: maps `Fuente` keys to reference labels.
  
- `Notas_técnicas`  
  Context: equations for VRTAcc; predictors are `diam` (cm) and `alt` (m).

### Important Technical Note

The sheet `Ecuaciones_asignadas` contains merged headers created manually in Excel.

The ingestion script **does not rely on automatic column names**.  
Instead, it:

1. Reads the sheet without headers.
2. Reconstructs column names using:
   - Row 1 for columns A–H
   - Row 2 for subheaders under “Rangos de aplicabilidad”
3. Removes the first two rows before cleaning.

This ensures reproducibility and avoids fragile Excel header behavior.

---

## Key Design Decisions (A1)

In Phase A, we standardize and export a clean staging dataset (CSV) that can later be loaded into SQLite.

### Canonical Fields (Current Workbook)

**Application geography**
- `estado`
- `clave_umafor`
- `cveecon4`

**Taxon**
- `nombrecientifico_apg_raw` (raw preserved)

**Assignment / provenance**
- `nivel_asignacion` (1–8)
- `nivel_asignacion_desc` (joined from `NivelesdeAsignación`)
- `fuente_clave`
- `fuente_referencia` (joined from `Referencias`)
- `clave_ecuacion` (kept as-is)

**Equation**
- `ecuacion_raw`
- `ecuacion_normalizada` (standardized predictor tokens)
- `response_variable` (currently `"VRTAcc_m3"`)

**Applicability ranges**
- `dbh_range_cm_raw`
- `height_range_m_raw`
- `dbh_min_cm`, `dbh_max_cm`
- `alt_min_m`, `alt_max_m`

### Structural Decision

For Phase A, the dataset remains **wide** (one row per applied equation record),
because the source is already structured that way.

In later phases (multi-source biomass equations), we may normalize further into:

- `equation`
- `scope`
- `source`
- `parameter`

tables inside SQLite.

---

## Workflow

### 1️⃣ Place Raw File

Put the Excel workbook into:

data_raw/

Do **not** edit the raw file.

---

### 2️ Run Ingestion Script

From the repository root:

Rscript R/01_ingest_excel_A2.R data_raw/EcuacionesAsignadas_volumen_vrtacc_2015-2020.xlsx

Outputs:

data_clean/equation_application_clean.csv
logs/01_ingest_excel_A2.log


---

### 3️ (Later) Build SQLite

A future script will create:

db/allometry.sqlite


from the cleaned staging dataset.

---

## Suggested Repository Structure

allometria-sqlite-mx/
README.md
data_raw/
data_staged/
data_clean/
db/
logs/
R/
inst/metadata/


---

## Inputs and Outputs (Current Phase)

**Input**
- Excel workbook (SNIF / CONAFOR open data portal)

**Output**
- Clean staging CSV with joined lookup information

---

## Provenance and Citation

This project preserves:

- Raw equation strings
- Source identifiers (`Fuente`)
- Assignment level definitions
- Reference labels

When adding additional sources, we will extend provenance tracking to include:

- File hashes
- Extraction method
- Bibliographic metadata
- License information (when available)

---

## Keywords

Manglaria · forestry · allometry · volume equations · sqlite · mexico · SNIF · CONAFOR

## License

MIT License



# MIA Allometric Rebuild

This repository contains a maintained local R pipeline for rebuilding and
extending a Mexican allometric-equation dataset.

The pipeline starts from two source families:

- a large INFYS / SNIF volume-equation workbook
- a small `dina` biomass-equation CSV for mangroves

It produces clean CSV tables, SQLite databases, parquet files, and a committed
BigQuery handoff schema.

## What The Pipeline Does Today

The maintained scripts can currently:

1. Ingest the volume workbook
   `01_data/data/eq_sources/EcuacionesAsignadas_volumen_vrtacc_2015-2020.xlsx`
2. Reconstruct the two-row workbook header and join its lookup sheets
3. Normalize equation text and produce machine-ready `equation_numpy`
4. Parse DBH and height applicability ranges for the volume rows
5. Build a volume-only SQLite database with PK/FK constraints preserved
6. Ingest the 4 mangrove biomass equations from
   `01_data/data/eq_sources/mangrove_allometric_equations_dina.csv`
7. Join all 4 biomass species exactly to
   `01_data/data/wd_sources/wood_density_values_table29.csv`
8. Keep both:
   - the original biomass equation
   - a derived `equation_numpy_wd_fixed` version with wood density baked in
9. Build one flat unified table covering both sources:
   - `infys` for volume rows
   - `dina` for biomass rows
10. Export the unified data to SQLite, parquet, and a BigQuery-ready JSON schema

## Current Pipeline Scripts

Run from the repo root:

```bash
Rscript R/01_ingest_volume.R
Rscript R/02_build_sqlite_volume.R
Rscript R/03_export_parquet_volume.R
Rscript R/04_ingest_biomass.R
Rscript R/05_build_unified_clean.R
Rscript R/06_build_sqlite_unified.R
Rscript R/07_export_parquet_unified.R
```

Script roles:

- `R/01_ingest_volume.R`: build the clean volume CSV
- `R/02_build_sqlite_volume.R`: build the relational volume SQLite
- `R/03_export_parquet_volume.R`: export the volume parquet files
- `R/04_ingest_biomass.R`: build the clean biomass CSV and exact WD join
- `R/05_build_unified_clean.R`: build the flat unified CSV
- `R/06_build_sqlite_unified.R`: build the flat unified SQLite
- `R/07_export_parquet_unified.R`: export the unified parquet files

Environment setup and expected outputs are documented in
[06_infra/local_setup.md](C:/Users/dev/work/manglaria/repos_dev/mia-allometric-dev/06_infra/local_setup.md).

## Main Outputs

Volume-only outputs:

- `data_clean/volume/equation_application_clean.csv`
- `data_clean/volume/equation_application_clean.snappy.parquet`
- `data_clean/volume/equation_application_clean.zstd.parquet`
- `db/allometry_volume.sqlite`

Biomass staging output:

- `data_clean/biomass/biomass_equation_application_clean.csv`

Unified outputs:

- `data_clean/unified/equation_application_unified.csv`
- `data_clean/unified/equation_application_unified.snappy.parquet`
- `data_clean/unified/equation_application_unified.zstd.parquet`
- `db/allometry_unified.sqlite`
- `db/bigquery_unified_schema.json`

Run logs:

- `logs/01_ingest_volume.log`
- `logs/02_build_sqlite_volume.log`
- `logs/03_export_parquet_volume.log`
- `logs/04_ingest_biomass.log`
- `logs/05_build_unified_clean.log`
- `logs/06_build_sqlite_unified.log`
- `logs/07_export_parquet_unified.log`

## Current Data Shape

As of the current maintained state:

- volume rows: `20,123`
- biomass rows: `4`
- unified rows: `20,127`
- volume `umafor_code` values with leading zeros preserved: `4,486`

The unified table is intentionally flat. The volume-only SQLite keeps a
relational shape with lookup tables; the unified SQLite is a single-table
analytical product.

Detailed schema notes live in
[01_data/schema.md](C:/Users/dev/work/manglaria/repos_dev/mia-allometric-dev/01_data/schema.md).

## Wood Density Handling

Each `dina` biomass row carries:

- `equation_raw`: original source equation
- `equation_normalized`: normalized symbolic form
- `equation_numpy`: machine-ready equation with `wd` still as a free variable
- `equation_numpy_wd_fixed`: machine-ready equation with the species'
  literal wood density already substituted

Example:

- raw: `B = 0.403*WD*(DAP)^1.934`
- numpy: `0.403*wd*(diam)**1.934`
- wd-fixed: `0.403*0.78*(diam)**1.934`

The original biomass formula is not overwritten. The WD-baked-in version exists
as a separate field so downstream users can choose either provenance or
convenience.

## BigQuery Handoff

The committed BigQuery schema for the unified zstd parquet is:

- [db/bigquery_unified_schema.json](C:/Users/dev/work/manglaria/repos_dev/mia-allometric-dev/db/bigquery_unified_schema.json)

Canonical load form:

```bash
bq load --source_format=PARQUET \
  --schema=db/bigquery_unified_schema.json \
  <dataset>.<table> \
  data_clean/unified/equation_application_unified.zstd.parquet
```

Important handoff guarantees:

- `umafor_code` is preserved as `STRING`, not integer
- `parse_notes` is preserved as `STRING`, even when all values are null
- `assignment_level` is integer-like
- quantitative fields load as numeric

Suggested clustering for downstream use:

- `source_dataset`
- `scientific_name_apg_raw`

## Source-Language Note

The main volume source comes from a Mexican forestry workflow and arrives with
Spanish column names. The maintained outputs use English column names.

Examples:

- `estado` -> `state`
- `clave_umafor` -> `umafor_code`
- `cveecon4` -> `mx_inegi_cveecon4`
- `nivel_asignacion` -> `assignment_level`
- `clave_ecuacion` -> `equation_code`
- `fuente` -> `source_code`
- `referencia` -> `source_reference`

Two identifiers intentionally keep Mexican-domain proper-noun form:

- `umafor_code`
- `mx_inegi_cveecon4`

### Predictor Tokens Inside Equations

Column names were translated, but symbolic tokens inside equations were not.

The maintained convention inside `equation_raw`, `equation_normalized`,
`equation_numpy`, and `equation_numpy_wd_fixed` is:

- `diam`: diameter / DBH / DAP in cm
- `alt`: total tree height in m
- `wd`: wood density in tonne/m3

This is intentional. Renaming columns is a safe structural change; rewriting
the mathematical content of 20,000+ equations is a different kind of change
and remains out of scope.

## What The Pipeline Cannot Do Yet

The current maintained pipeline does **not** yet:

- support biomass sources beyond the current 4-row `dina` file
- support fuzzy or synonym-based wood-density joins
- translate predictor tokens inside formulas to fully English symbolic names
- resolve the meaning of volume `assignment_level` values `9`, `10`, and `11`
- add richer geographic or bibliographic metadata to biomass rows
- perform formal scientific validation of every equation beyond the current
  normalization, range parsing, and structural checks
- provide a package, API, dashboard, or production scheduler

## What Still Needs Development

The project is usable now, but the most valuable next improvements would be:

1. Clarify the source meaning of volume `assignment_level` values `9`, `10`,
   and `11`.
2. Add more biomass sources and define a repeatable mapping pattern for sparse
   metadata.
3. Improve taxonomic standardization beyond the current raw-name preservation.
4. Add more explicit automated validation around equation semantics and units.
5. Add a lightweight release/checkpoint pattern for regenerated outputs.
6. Decide whether downstream consumers need a stable published data contract
   beyond the current SQLite/parquet artifacts.

## Project Boundaries

This repository is intentionally a simple R-script workflow.

It is not trying to be:

- a Python package
- an R package
- a web app
- an API service
- a production orchestration repo

The legacy reference implementation is kept under:

- `90_legacy_review/mia-allometric-main/`

That folder is reference material, not the maintained working pipeline.

## Related Artifacts

- [01_data/schema.md](C:/Users/dev/work/manglaria/repos_dev/mia-allometric-dev/01_data/schema.md)
- [01_data/storage_layout.md](C:/Users/dev/work/manglaria/repos_dev/mia-allometric-dev/01_data/storage_layout.md)
- [03_experiments/run_summary.md](C:/Users/dev/work/manglaria/repos_dev/mia-allometric-dev/03_experiments/run_summary.md)
- [05_governance/decision_log.md](C:/Users/dev/work/manglaria/repos_dev/mia-allometric-dev/05_governance/decision_log.md)
- [06_infra/local_setup.md](C:/Users/dev/work/manglaria/repos_dev/mia-allometric-dev/06_infra/local_setup.md)

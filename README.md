\name{mia-allometric}
\title{Allometric equations SQLite Database (Mexico)}
\description{
This repository builds a simple, reproducible pipeline (in R) to ingest a curated Excel workbook
containing forestry allometric equations applied in Mexico (initially volume equations for VRTAcc),
clean and standardize key fields, and export a “clean staging” dataset ready to be loaded into an
SQLite database.

The first batch of data was downloaded from:
https://snif.cnf.gob.mx/datos-abiertos/
}

\details{
\section{Project goal}{
Create a consistent and auditable dataset of allometric equations (starting with one ordered source),
keeping strong provenance and preparing for later extension to multi-source ingestion (Excel/CSV/TXT
and published scientific sources) and additional response variables (e.g., biomass AGB/BGB).
}

\section{Current scope (Phase A: single workbook)}{
This first phase focuses on one Excel file:
\itemize{
  \item Sheet \code{Ecuaciones_asignadas} (main data table; applied equations by geography and taxon)
  \item Sheet \code{NivelesdeAsignación} (lookup: definitions for assignment levels 1--8)
  \item Sheet \code{Referencias} (lookup: maps \code{Fuente} keys to reference labels)
  \item Sheet \code{Notas_técnicas} (context: equations for VRTAcc; predictors diam (cm) and alt (m))
}

Because the workbook has “human-style” merged headers in the first sheet, the ingestion script does
NOT rely on automatic column names. Instead it reconstructs the header from row 1 (columns A--H)
and row 2 (subheaders for the range columns I--J), then removes the first two rows before cleaning.
}

\section{Key design decisions (A1)}{
In Phase A, we standardize and export a clean staging dataset (CSV) that can later be loaded into SQLite.

Canonical fields for this workbook:
\itemize{
  \item Application geography: \code{estado}, \code{clave_umafor}, \code{cveecon4}
  \item Taxon: \code{nombrecientifico_apg_raw} (raw preserved)
  \item Assignment/provenance: \code{nivel_asignacion} (1--8), \code{nivel_asignacion_desc}
        (joined from \code{NivelesdeAsignación}), \code{fuente_clave} and \code{fuente_referencia}
        (joined from \code{Referencias}), \code{clave_ecuacion} (kept as-is)
  \item Equation: \code{ecuacion_raw} (raw string), \code{ecuacion_normalizada} (predictor tokens standardized)
  \item Ranges: \code{dbh_range_cm_raw}, \code{height_range_m_raw}, plus parsed numeric min/max fields
  \item Context: \code{response_variable} defaults to \code{"VRTAcc_m3"} (from \code{Notas_técnicas})
}

We intentionally keep this Phase A dataset “wide” (one row per applied equation record) because the
source is already organized that way. Later phases may normalize further into separate \code{equation},
\code{scope}, and \code{source} tables as we add heterogeneous sources.
}

\section{Workflow}{
\enumerate{
  \item Put the raw Excel file into \code{data_raw/} (do not edit it).
  \item Run \code{R/01_ingest_excel_A2.R}. This produces:
    \itemize{
      \item \code{data_clean/equation_application_clean.csv}
      \item \code{logs/01_ingest_excel_A2.log}
    }
  \item (Later) Run a DB build script to create/update \code{db/allometry.sqlite}.
}

\section{Suggested repo structure}{
\preformatted{
allometria-sqlite-mx/
  README.RD
  data_raw/
  data_staged/
  data_clean/
  db/
  logs/
  R/
  inst/metadata/
}
}

\section{Inputs and outputs (current)}{
\itemize{
  \item Input: Excel workbook (downloaded from SNIF/CONAFOR open data portal)
  \item Output: Clean staging CSV (one row per record in \code{Ecuaciones_asignadas}, with lookups joined)
}

\section{How to cite / provenance}{
This project keeps raw equation strings and references as provided by the source workbook, and
records joined reference labels from the \code{Referencias} sheet. When adding additional sources,
we will extend provenance fields to include file hashes, extraction method, and bibliographic metadata.
}
}

\author{Julián Equihua}
\keyword{forestry}
\keyword{allometry}
\keyword{sqlite}
\keyword{mexico}

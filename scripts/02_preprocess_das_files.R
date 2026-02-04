# 02_preprocess_das_files.R
# =========================
# Processes DAS (DeQS Auswertungsstelle) XML files to extract:
# - Documentation rates (Dokumentationsraten)
# - QS results (QS-Ergebnisse)

# Setup -----------------------------------------------------------
library(tidyverse)
library(xml2)
library(glue)
library(furrr)
library(tictoc)

# Set working directory to repository root
if (rstudioapi::isAvailable()) {
  setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
}

# Load configuration and utilities
source("config.R")
source("scripts/utils/utils_das.R")

# Ensure output directory exists
if (!dir.exists(PATH_QB_OUTPUT)) {
  dir.create(PATH_QB_OUTPUT, recursive = TRUE)
}

# Build paths for this year
xml_source_path <- file.path(PATH_QB_RAW, glue("xml_{JAHR}"))

xml_files <- get_qb_xml_files(
  file_path = xml_source_path,
  pattern = glue("{JAHR}-das\\.xml$")
)

# Set up parallel plan
plan(multisession, workers = N_CORES)

# Lese Dokumentationsraten ein ------------------------------------
tic("Processing documentation rates")

qualitaetsberichte_das_dokumentationsraten_raw <- future_map_dfr(
  xml_files,
  read_qualitaetsberichte_das_dokumentationsraten,
  .progress = TRUE
)

toc()

qualitaetsberichte_das_dokumentationsraten <-
  unnest_and_convert_qualitaetsberichte_das_dokumentationsraten(
    qualitaetsberichte_das_dokumentationsraten_raw
  )

# Save results
save(
  qualitaetsberichte_das_dokumentationsraten,
  file = file.path(
    PATH_QB_OUTPUT,
    glue("Qualitaetsdaten_Dokumentationsraten_das_files_{JAHR}.RData")
  )
)

# Lese QS-Ergebnisse ein ------------------------------------------
tic("Processing QS results")

qualitaetsberichte_das_ergebnis_raw <- future_map_dfr(
  xml_files,
  read_qualitaetsberichte_das_ergebnis,
  .progress = TRUE
)

toc()

qualitaetsberichte_das_ergebnis <-
  unnest_and_convert_qualitaetsberichte_das_ergebnis(
    qualitaetsberichte_das_ergebnis_raw
  )

# Save results
save(
  qualitaetsberichte_das_ergebnis,
  file = file.path(
    PATH_QB_OUTPUT,
    glue("Qualitaetsdaten_Ergebnis_das_files_{JAHR}.RData")
  )
)

# Cleanup: revert to sequential processing
plan(sequential)

message(glue("Finished processing DAS files for {JAHR}"))

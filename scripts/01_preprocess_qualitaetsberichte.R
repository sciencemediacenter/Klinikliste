# 01_preprocess_qualitaetsberichte.R
# ==================================
# Processes XML quality reports from G-BA to extract:
# - Basic hospital data (beds, emergency levels, etc.)
# - Procedures (Prozeduren)
# - Diagnoses (Diagnosen)
# - Medical services (Medizinisches Leistungsangebot)
# - Department codes (Fachabteilungsschluessel)

# Setup -----------------------------------------------------------
library(tidyverse)
library(stringr)
library(xml2)
library(glue)
library(furrr)
library(future)
library(tictoc)

# Set working directory to repository root
if (rstudioapi::isAvailable()) {
  setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
}

# Load configuration and utilities
source("config.R")
source("scripts/utils/utils_xml.R")

# Ensure output directory exists
if (!dir.exists(PATH_QB_OUTPUT)) {
  dir.create(PATH_QB_OUTPUT, recursive = TRUE)
}

# Build paths for this year
xml_source_path <- file.path(PATH_QB_RAW, glue("xml_{JAHR}"))

xml_files <- get_qb_xml_files(
  file_path = xml_source_path,
  pattern = glue("{JAHR}-xml\\.xml$")
)

# Set up parallel plan
plan(multisession, workers = N_CORES)

# Lese Qualitätsberichte ein --------------------------------------
tic("Qualitätsberichte einlesen")
qualitaetsdaten <- future_map_dfr(
  xml_files,
  read_qualitaetsberichte_xml,
  .progress = TRUE
)
toc()

# Save data
save(
  qualitaetsdaten,
  file = file.path(PATH_QB_OUTPUT, glue("Qualitaetsdaten_{JAHR}.Rdata"))
)

# Lese Prozeduren ein ----------------------------------------------
tic("Prozeduren einlesen")
Fallzahlen_Prozeduren <- future_map_dfr(
  xml_files,
  read_qualitaetsberichte_xml_prozeduren,
  .progress = TRUE
)
toc()

# Save data
save(
  Fallzahlen_Prozeduren,
  file = file.path(PATH_QB_OUTPUT, glue("Fallzahlen_Prozeduren_{JAHR}.Rdata"))
)

# Lese Diagnosen ein -----------------------------------------------
tic("Diagnosen einlesen")
Fallzahlen_Diagnosen <- future_map_dfr(
  xml_files,
  read_qualitaetsberichte_xml_diagnosen,
  .progress = TRUE
)
toc()

# Save data
save(
  Fallzahlen_Diagnosen,
  file = file.path(PATH_QB_OUTPUT, glue("Fallzahlen_Diagnosen_{JAHR}.Rdata"))
)

# Lese Medizinisches Leistungsangebot ein -------------------------
tic("Medizinisches Leistungsangebot einlesen")
Medizinisches_Leistungsangebot <- future_map_dfr(
  xml_files,
  read_qualitaetsberichte_xml_medizinisches_leistungsangebot,
  .progress = TRUE
)
toc()

# Save data
save(
  Medizinisches_Leistungsangebot,
  file = file.path(PATH_QB_OUTPUT, glue("Medizinisches_Leistungsangebot_{JAHR}.Rdata"))
)

# Lese Fachabteilungsschluessel ein --------------------------------
tic("Fachabteilungsschluessel einlesen")
Fachabteilungsschluessel <- future_map_dfr(
  xml_files,
  read_qualitaetsberichte_xml_fachabteilungsschluessel,
  .progress = TRUE
)
toc()

# Save data
save(
  Fachabteilungsschluessel,
  file = file.path(PATH_QB_OUTPUT, glue("Fachabteilungsschluessel_{JAHR}.Rdata"))
)

# Cleanup: revert to sequential processing
plan(sequential)

message(glue("Finished processing quality reports for {JAHR}"))

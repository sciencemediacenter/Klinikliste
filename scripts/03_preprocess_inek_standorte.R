# 03_preprocess_inek_standorte.R
# ==============================
# Processes InEK hospital location directory XML to extract:
# - Hospital list (Krankenhaeuser)
# - Location list (Standorte)

# Setup -----------------------------------------------------------
library(xml2)
library(tidyverse)

# Set working directory to repository root
if (rstudioapi::isAvailable()) {
  setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
}

# Load configuration and utilities
source("config.R")
source("scripts/utils/utils_inek.R")

# Ensure output directory exists
if (!dir.exists(PATH_INEK_OUTPUT)) {
  dir.create(PATH_INEK_OUTPUT, recursive = TRUE)
}

########################
## InEK Standortliste ##
########################

# Set the date of the InEK directory file
# Format: YYYY-MM-DD (will be converted to YYYYMMDD for filename)
INEK_VERZEICHNIS_DATUM <- "2024-08-09"

Krankenhausverzeichnis <- read_Krankenhausverzeichnis(
  Verzeichnisdatum = INEK_VERZEICHNIS_DATUM,
  Dateipfad = PATH_INEK_RAW
)

# Save results
save(
  Krankenhausverzeichnis$Krankenhaeuser,
  Krankenhausverzeichnis$Standorte,
  file = file.path(PATH_INEK_OUTPUT, "InEK_Krankenhausliste.RData")
)

message("Finished processing InEK hospital directory")

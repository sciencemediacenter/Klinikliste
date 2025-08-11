# Setup -----------------------------------------------------------
library(tidyverse)
library(stringr)
library(xml2)
library(glue)
library(furrr)
library(future)
library(tictoc)

# Set working directory to script location (if in RStudio)
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

source("utils.R")
source("utils_xml_file.R")

jahr <- "2023"

# Basisverzeichnis für alle Ein- und Ausgaben
destination_path <- file.path("Qualitaetsberichte/nobackup/results")

# Falls das Zielverzeichnis nicht existiert, erzeugen
if (!dir.exists(destination_path)) {
  dir.create(destination_path, recursive = TRUE)
}

xml_source_path <- file.path(
  "Qualitaetsberichte",
  "nobackup",
  glue("xml_{jahr}")
)

xml_files <-
  get_qb_xml_files(
    file_path = xml_source_path,
    pattern = glue("{jahr}-xml\\.xml$")
  )

# Set up parallel plan using all but two cores --------------------
plan(multisession, workers = parallel::detectCores() - 2)

# Lese Qualitätsberichte ein --------------------------------------
tic("Qualitätsberichte einlesen")
qualitaetsdaten <-
  future_map_dfr(
    xml_files,
    read_qualitaetsberichte_xml,
    .progress = TRUE # Show progress bar
  )
toc()
# Qualitätsberichte einlesen: 7.233 sec elapsed
# Qualitätsberichte einlesen: 8.912 sec elapsed

# Save data --------------------------------------------------------
save(
  qualitaetsdaten,
  file = file.path(destination_path, glue("Qualitaetsdaten_{jahr}.Rdata"))
)

# Lese Prozeduren ein ----------------------------------------------
tic("Prozeduren einlesen")
Fallzahlen_Prozeduren <-
  future_map_dfr(
    xml_files,
    read_qualitaetsberichte_xml_prozeduren,
    .progress = TRUE # Show progress bar
  )
# %>%
#   select(-Anzahl_Datenschutz) %>%
#   mutate(Anzahl = as.numeric(Anzahl))
toc()
# Prozeduren einlesen: 2167.53 sec elapsed

# Save data --------------------------------------------------------
save(
  Fallzahlen_Prozeduren,
  file = file.path(destination_path, glue("Fallzahlen_Prozeduren_{jahr}.Rdata"))
)

# Lese Diagnosen ein -----------------------------------------------
tic("Diagnosen einlesen")
Fallzahlen_Diagnosen <-
  future_map_dfr(
    xml_files,
    read_qualitaetsberichte_xml_diagnosen,
    .progress = TRUE # Show progress bar
  )
# %>%
#   select(-Fallzahl_Datenschutz) %>%
#   mutate(Fallzahl = as.numeric(Fallzahl))
toc()

# Save data --------------------------------------------------------
save(
  Fallzahlen_Diagnosen,
  file = file.path(destination_path, glue("Fallzahlen_Diagnosen_{jahr}.Rdata"))
)

# Lese Medizinisches Leistungsangebot ein -------------------------
tic("Medizinisches Leistungsangebot einlesen")
Medizinisches_Leistungsangebot <-
  future_map_dfr(
    xml_files,
    read_qualitaetsberichte_xml_medizinisches_leistungsangebot,
    .progress = TRUE # Show progress bar
  )
toc()

# Save data --------------------------------------------------------
save(
  Medizinisches_Leistungsangebot,
  file = file.path(
    destination_path,
    glue("Medizinisches_Leistungsangebot_{jahr}.Rdata")
  )
)

# Lese Fachabteilungsschluessel ein --------------------------------
tic("Fachabteilungsschluessel einlesen")
Fachabteilungsschluessel <-
  future_map_dfr(
    xml_files,
    read_qualitaetsberichte_xml_fachabteilungsschluessel,
    .progress = TRUE # Show progress bar
  )
toc()

# Save data --------------------------------------------------------
save(
  Fachabteilungsschluessel,
  file = file.path(
    destination_path,
    glue("Fachabteilungsschluessel_{jahr}.Rdata")
  )
)

# Optionally, revert to sequential processing after this script runs
plan(sequential)

#  Progress: ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 100%
# Qualitätsberichte einlesen: 10.891 sec elapsed
#  Progress: ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 100%
# Prozeduren einlesen: 2167.786 sec elapsed
#  Progress: ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────  100%
# Diagnosen einlesen: 1273.953 sec elapsed
#  Progress: ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 100%
# Medizinisches Leistungsangebot einlesen: 250.917 sec elapsed
#  Progress: ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────  100%
# Fachabteilungsschluessel einlesen: 13.144 sec elapsed
# There were 50 or more warnings (use warnings() to see the first 50)

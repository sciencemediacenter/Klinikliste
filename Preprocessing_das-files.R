# Dokumentationsraten

library(tidyverse)
library(xml2)
library(glue)
library(rvest)
library(furrr) # For parallel processing
library(tictoc) # For timing


## workspace directory und filename
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

source("utils.R")
source("utils_das_file.R")

jahr <- 2023

# Basisverzeichnis für alle Ein- und Ausgaben
destination_path <- file.path("Qualitaetsberichte/nobackup/results")

path <- file.path("Qualitaetsberichte", "nobckup", glue("xml_{jahr}"))

file_list <- list.files(path)
file_list <- file_list[str_ends(file_list, pattern = "das.xml")] # nur ...das.xml files !!!!
file_list <- unique(str_extract(file_list, glue("[0-9\\-]*(?=\\-{jahr}\\-)")))

xml_source_path <- file.path(
  "Qualitaetsberichte",
  "nobackup",
  glue("xml_{jahr}")
)

xml_files <-
  get_qb_xml_files(
    file_path = xml_source_path,
    pattern = glue("{jahr}-das\\.xml$")
  )

# Set up parallel plan using all but two cores --------------------
plan(multisession, workers = parallel::detectCores() - 2)


# Lese Qualitätsberichte ein --------------------------------------

# Start timing
tic("Processing XML files in parallel")

# Process all files in parallel
qualitaetsberichte_das_dokumentationsraten_raw <- future_map_dfr(
  xml_files,
  read_qualitaetsberichte_das_dokumentationsraten,
  .progress = TRUE # Show progress bar
)

# End timing
toc()

qualitaetsberichte_das_dokumentationsraten <-
  unnest_and_convert_qualitaetsberichte_das_dokumentationsraten(
    qualitaetsberichte_das_dokumentationsraten_raw
  )

# Save results
save(
  qualitaetsberichte_das_dokumentationsraten,
  file = file.path(
    destination_path,
    glue("Qualitaetsdaten_Dokumentationsraten_das_files_{jahr}.RData")
  )
)


# Start timing
tic("Processing XML files in parallel")

# Process all files in parallel
qualitaetsberichte_das_ergebnis_raw <- future_map_dfr(
  xml_files,
  read_qualitaetsberichte_das_ergebnis,
  .progress = TRUE # Show progress bar
)

# End timing
toc()

qualitaetsberichte_das_ergebnis <-
  unnest_and_convert_qualitaetsberichte_das_ergebnis(
    qualitaetsberichte_das_ergebnis_raw
  )


# Save results
save(
  qualitaetsberichte_das_ergebnis,
  file = file.path(
    destination_path,
    glue("Qualitaetsdaten_Ergebnis_das_files_{jahr}.RData")
  )
)

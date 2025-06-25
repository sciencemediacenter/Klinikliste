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

Jahr <- 2023

# Basisverzeichnis für alle Ein- und Ausgaben
destination_path <- file.path("Qualitaetsberichte/nobackup/results")

path <- file.path("Qualitaetsberichte", "nobackup", glue("xml_{jahr}"))

file_list <- list.files(path)
file_list <- file_list[str_ends(file_list, pattern = "das.xml")] # nur ...das.xml files !!!!
file_list <- unique(str_extract(file_list, glue("[0-9\\-]*(?=\\-{Jahr}\\-)")))

# Function to extract HTML elements
extract_html_element <- function(x, Element) {
  x <- lapply(x, function(x) {
    html_elements(x, Element) |> html_text()
  })
  x[lengths(x) == 0] <- NA
  return(x)
}

# Function to process a single file
process_file <- function(i) {
  # Read XML file
  neue_Dokdaten_xml <- read_xml(file.path(
    path,
    paste(i, Jahr, "das.xml", sep = "-")
  ))

  # Extract IK and Standortnummer
  IK_temp <- html_element(neue_Dokdaten_xml, "IK") |> html_text()
  Standortnummer_temp <- html_element(neue_Dokdaten_xml, "Standortnummer") |>
    html_text()

  # Extract Leistungsbereich elements
  Leistungsbereich <- html_elements(
    neue_Dokdaten_xml,
    xpath = ".//Leistungsbereich_DeQS"
  )

  # Convert to tibble
  Leistungsbereich <- bind_rows(lapply(
    Leistungsbereich,
    function(x) tibble(Leistungsbereich = list(x))
  ))

  # If no Leistungsbereiche found, return empty tibble
  if (nrow(Leistungsbereich) == 0) {
    return(tibble())
  }

  # Create data frame with all elements
  table_Dokdaten <- Leistungsbereich |>
    mutate(
      IK = IK_temp,
      Standortnummer = Standortnummer_temp,
      Kuerzel = extract_html_element(Leistungsbereich, "Kuerzel"),
      Bezeichnung = extract_html_element(Leistungsbereich, "Bezeichnung"),
      Fallzahl = extract_html_element(Leistungsbereich, "Fallzahl"),
      Dokumentationsrate = extract_html_element(
        Leistungsbereich,
        "Dokumentationsrate"
      ),
      Anzahl_Datensaetze_Standort = extract_html_element(
        Leistungsbereich,
        "Anzahl_Datensaetze_Standort"
      )
    )

  return(table_Dokdaten)
}

# Set up parallel processing
# Use one less than available cores to keep system responsive
plan(multisession, workers = parallel::detectCores() - 1)

# Start timing
tic("Processing XML files in parallel")

# Process all files in parallel
Qualitaetsdaten_das_files_raw <- future_map_dfr(
  file_list,
  process_file,
  .progress = TRUE # Show progress bar
)

# End timing
toc()

# Post-processing: Unnest and convert data types
Qualitaetsdaten_das_files <-
  Qualitaetsdaten_das_files_raw |>
  select(-Leistungsbereich) |>
  unnest(
    cols = c(
      Kuerzel,
      Bezeichnung,
      Fallzahl,
      Dokumentationsrate,
      Anzahl_Datensaetze_Standort
    )
  ) |>
  # Convert values to proper types
  mutate(
    Fallzahl = as.integer(Fallzahl),
    Dokumentationsrate = parse_number(
      Dokumentationsrate,
      locale = locale(decimal_mark = ","),
      trim_ws = TRUE
    ),
    Anzahl_Datensaetze_Standort = as.integer(Anzahl_Datensaetze_Standort),
  )

# Save results
save(
  Qualitaetsdaten_das_files,
  file = file.path(
    destination_path,
    glue("Qualitaetsdaten_das_files_{Jahr}.RData")
  )
)

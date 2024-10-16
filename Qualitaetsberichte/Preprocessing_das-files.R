# Dokumentationsraten

library(tidyverse)
# library(stringr)
library(xml2)
# library(pbapply)
# library(parallel)
library(glue)
library(rvest)

## workspace directory und filename
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}


source("../utils.R")

Jahr <- 2021

path <- file.path("nobackup", glue("xml_{Jahr}"))

file_list <- list.files(path)
file_list <- file_list[str_ends(file_list, pattern = "das.xml")] # nur ...das.xml files !!!!
file_list <- unique(str_extract(file_list, glue("[0-9\\-]*(?=\\-{Jahr}\\-)")))


Qualitaetsdaten_das_files <- NULL

extract_html_element <-
  function(x, Element) {
    x <- lapply(x, function(x) {
      html_elements(x, Element) |> html_text()
    })
    x[lengths(x) == 0] <- NA
    return(x)
  }

for (i in file_list) {
  neue_Dokdaten_xml <-
    read_xml(file.path(path, paste(i, Jahr, "das.xml", sep = "-")))

  IK_temp <-
    html_element(neue_Dokdaten_xml, "IK") |>
    html_text()

  Standortnummer_temp <-
    html_element(neue_Dokdaten_xml, "Standortnummer") |>
    html_text()

  Leistungsbereich <- html_elements(neue_Dokdaten_xml, xpath = ".//Leistungsbereich_DeQS")

  Leistungsbereich <-  bind_rows(lapply(Leistungsbereich, function(x)
    tibble(Leistungsbereich = list(x))))

  table_Dokdaten <-
    Leistungsbereich |>
    mutate(
      IK = IK_temp,
      Standortnummer = Standortnummer_temp,
      Kuerzel = extract_html_element(Leistungsbereich, "Kuerzel"),
      Bezeichnung = extract_html_element(Leistungsbereich, "Bezeichnung"),
      Fallzahl = extract_html_element(Leistungsbereich, "Fallzahl"),
      Dokumentationsrate = extract_html_element(Leistungsbereich, "Dokumentationsrate"),
      Anzahl_Datensaetze_Standort = extract_html_element(Leistungsbereich, "Anzahl_Datensaetze_Standort")
    )

  Qualitaetsdaten_das_files <- bind_rows(Qualitaetsdaten_das_files, table_Dokdaten)
}

Qualitaetsdaten_das_files <-
  Qualitaetsdaten_das_files |>
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
  # Werte in Zahlen umwandeln
  mutate(
    Fallzahl = as.integer(Fallzahl),
    Dokumentationsrate = parse_number(
      Dokumentationsrate,
      locale = locale(decimal_mark = ","),
      trim_ws = TRUE
    ),
    Anzahl_Datensaetze_Standort = as.integer(Anzahl_Datensaetze_Standort),
  )

save(
  Qualitaetsdaten_das_files,
  file = file.path(".", glue("Qualitaetsdaten_das_files_{Jahr}.RData"))
)

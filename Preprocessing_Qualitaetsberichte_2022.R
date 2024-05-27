
# Setup -----------------------------------------------------------
library(tidyverse)
library(stringr)
library(xml2)
library(pbapply)

## workspace directory und filename
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

source("utils.R")

xml_files <-
  get_qb_xml_files(
    file_path = file.path("Qualitaetsberichte", "nobackup", "xml_2022"),
    pattern = "2022-xml\\.xml$"
  )

# Lese Qualitätsberichte ein --------------------------------------
qualitaetsdaten <- pblapply(xml_files, read_qualitaetsberichte_xml)
qualitaetsdaten <- bind_rows(qualitaetsdaten)

# Save data -------------------------------------------------------
save(qualitaetsdaten,
     file = file.path("Qualitaetsberichte", "Qualitaetsdaten.Rdata"))



# Lese Prozeduren ein ---------------------------------------------

Fallzahlen_Prozeduren <- pblapply(xml_files, read_qualitaetsberichte_xml_prozeduren)
Fallzahlen_Prozeduren <- 
  bind_rows(Fallzahlen_Prozeduren) |> 
  select(-Anzahl_Datenschutz) |> 
  mutate(Anzahl = as.numeric(Anzahl))

# Save data -------------------------------------------------------
save(Fallzahlen_Prozeduren,
     file = file.path("Qualitaetsberichte", "Fallzahlen_Prozeduren.Rdata"))

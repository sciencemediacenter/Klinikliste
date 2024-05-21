
# Setup -----------------------------------------------------------
library(stringr)
library(dplyr)
library(xml2)
library(pbapply)
source("utils.R")

xml_files <- get_qb_xml_fils()
# file_path <- xml_files[grepl("260102343-773448000", xml_files)]

# Lese Qualitätsberichte ein --------------------------------------
qualitaetsdaten <- pblapply(xml_files, read_qualitaetsberichte_xml)

qualitaetsdaten <- bind_rows(qualitaetsdaten)

# Save data -------------------------------------------------------
save(qualitaetsdaten,
     file = file.path("Qualitaetsberichte", "Qualitaetsdaten.Rdata"))


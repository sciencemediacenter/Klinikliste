
# Setup -----------------------------------------------------------
library(tidyverse)
library(stringr)
library(xml2)
library(pbapply)
library(parallel)

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

cl <- makeCluster(detectCores()-2)

Fallzahlen_Prozeduren <- parLapply(cl, xml_files, read_qualitaetsberichte_xml_prozeduren)

stopCluster(cl)


Fallzahlen_Prozeduren <- 
  bind_rows(Fallzahlen_Prozeduren) |> 
  select(-Anzahl_Datenschutz) |> 
  mutate(Anzahl = as.numeric(Anzahl))

# Save data -------------------------------------------------------
save(Fallzahlen_Prozeduren,
     file = file.path("Qualitaetsberichte", "Fallzahlen_Prozeduren.Rdata"))



# Lese Diagnosen ein ---------------------------------------------

cl <- makeCluster(detectCores()-2)

Fallzahlen_Diagnosen <- parLapply(cl, xml_files, read_qualitaetsberichte_xml_diagnosen)

stopCluster(cl)


Fallzahlen_Diagnosen <- 
  bind_rows(Fallzahlen_Diagnosen) |> 
  select(-Fallzahl_Datenschutz) |>
  mutate(Fallzahl = as.numeric(Fallzahl))

# Save data -------------------------------------------------------
save(Fallzahlen_Diagnosen,
     file = file.path("Qualitaetsberichte", "Fallzahlen_Diagnosen.Rdata"))




# Lese Medizinisches Leistungsanbeot ein ---------------------------------------------

cl <- makeCluster(detectCores()-2)

Medizinisches_Leistungsangebot <- parLapply(cl, xml_files, read_qualitaetsberichte_xml_medizinisches_leistungsangebot)

stopCluster(cl)


Medizinisches_Leistungsangebot <- 
  bind_rows(Medizinisches_Leistungsangebot) 

# Save data -------------------------------------------------------
save(Medizinisches_Leistungsangebot,
     file = file.path("Qualitaetsberichte", "Medizinisches_Leistungsangebot.Rdata"))

library(xml2)
library(tidyverse)

## workspace directory und filename
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

########################
## InEK Standortliste ##
########################

source("InEK_Krankenhausstandorte_function.R")

Krankenhausverzeichnis <-
  read_Krankenhausverzeichnis(
    # Datum bei Bedarf anpassen
    Verzeichnisdatum = "2024-04-05",
    Dateipfad = file.path("Standortliste_InEK", "nobackup")
  )

save(
  Krankenhausverzeichnis$Krankenhaeuser,
  Krankenhausverzeichnis$Standorte,
  file = file.path("Standortliste_InEK", "InEK_Krankenhausliste.RData")
)

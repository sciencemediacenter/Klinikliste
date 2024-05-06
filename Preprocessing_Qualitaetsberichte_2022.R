## Qualitaetsberichte

library(xml2)
library(tidyverse)

## workspace directory und filename
if(rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}


path <- file.path("Qualitaetsberichte", "nobackup", "xml_2022")
file_list <- list.files(path)
file_list <- file_list[str_ends(file_list, pattern = "xml.xml")]
file_list <- unique(str_extract(file_list, "[0-9\\-]*(?=\\-2022\\-)"))
Jahr <- 2022

listnames_recursive <- function(x) {
  return <- character(5)
  
  try(return[1] <- names(x), silent = TRUE)
  try(return[2] <- names(x[[1]]), silent = TRUE)
  try(return[3] <- names(x[[1]][[1]]), silent = TRUE)
  try(return[4] <- names(x[[1]][[1]][[1]]), silent = TRUE)
  try(return[5] <- names(x[[1]][[1]][[1]][[1]]), silent = TRUE)
  return
}

Qualitaetsdaten <- NULL

for (i in file_list) {
  neue_Qualidaten <- NULL
  neue_Qualidaten_tibble <- NULL
  
  if (file.exists(file.path(path, paste(i, Jahr, "xml.xml", sep = "-")))) {
    neue_Qualidaten <-
      read_xml(file.path(path, paste(i, Jahr, "xml.xml", sep = "-"))) |>
      as_list()
  }
  
  if (!is.null(neue_Qualidaten)) {
    neue_Qualidaten <-
      neue_Qualidaten[[1]]
 
    
    # Standortdaten Krankenhaus mit mehreren Standorten   
    neue_Qualidaten_tibble <-
      tibble(
        IK = unlist(neue_Qualidaten$Krankenhaus$Mehrere_Standorte$Standortkontaktdaten$IK),
        Name = unlist(neue_Qualidaten$Krankenhaus$Mehrere_Standorte$Standortkontaktdaten$Name),
        Strasse = unlist(neue_Qualidaten$Krankenhaus$Mehrere_Standorte$Standortkontaktdaten$Kontakt_Zugang$Strasse),
        Hausnummer = unlist(neue_Qualidaten$Krankenhaus$Mehrere_Standorte$Standortkontaktdaten$Kontakt_Zugang$Hausnummer),
        Postleitzahl = unlist(neue_Qualidaten$Krankenhaus$Mehrere_Standorte$Standortkontaktdaten$Kontakt_Zugang$Postleitzahl),
        Ort = unlist(neue_Qualidaten$Krankenhaus$Mehrere_Standorte$Standortkontaktdaten$Kontakt_Zugang$Ort),
        Standortnummer = unlist(neue_Qualidaten$Krankenhaus$Mehrere_Standorte$Standortkontaktdaten$Standortnummer),
      )
    
    # Standortdaten Krankenhaus mit einem Standort
    if (nrow(neue_Qualidaten_tibble) == 0) {
      neue_Qualidaten_tibble <-
        tibble(
          IK = unlist(neue_Qualidaten$Krankenhaus$Ein_Standort$Krankenhauskontaktdaten$IK),
          Name = unlist(neue_Qualidaten$Krankenhaus$Ein_Standort$Krankenhauskontaktdaten$Name),
          Strasse = unlist(neue_Qualidaten$Krankenhaus$Ein_Standort$Krankenhauskontaktdaten$Kontakt_Zugang$Strasse),
          Hausnummer = unlist(neue_Qualidaten$Krankenhaus$Ein_Standort$Krankenhauskontaktdaten$Kontakt_Zugang$Hausnummer),
          Postleitzahl = unlist(neue_Qualidaten$Krankenhaus$Ein_Standort$Krankenhauskontaktdaten$Kontakt_Zugang$Postleitzahl),
          Ort = unlist(neue_Qualidaten$Krankenhaus$Ein_Standort$Krankenhauskontaktdaten$Kontakt_Zugang$Ort),
          Standortnummer = unlist(neue_Qualidaten$Krankenhaus$Ein_Standort$Krankenhauskontaktdaten$Standortnummer),
        )
    }
    
    neue_Qualidaten_tibble <- 
      neue_Qualidaten_tibble |> 
      mutate(
        Notfallstufe = list(listnames_recursive(neue_Qualidaten$Teilnahme_Notfallversorgung$Teilnahme_Notfallstufe)),
        Betten = unlist(neue_Qualidaten$Anzahl_Betten),
        Vollstationaere_Fallzahl = unlist(neue_Qualidaten$Fallzahlen$Vollstationaere_Fallzahl), 
        Teilstationaere_Fallzahl = unlist(neue_Qualidaten$Fallzahlen$Teilstationaere_Fallzahl), 
        Ambulante_Fallzahl = unlist(neue_Qualidaten$Fallzahlen$Ambulante_Fallzahl), 
        StaeB_Fallzahl = unlist(neue_Qualidaten$Fallzahlen$StaeB_Fallzahl)
        )
  }
  
  Qualitaetsdaten <- bind_rows(Qualitaetsdaten, neue_Qualidaten_tibble)
  
}


Qualitaetsdaten <- 
  Qualitaetsdaten |> 
  mutate(across(Betten:StaeB_Fallzahl, as.integer))

tmp <- 
  Qualitaetsdaten |> 
  unnest_longer(Notfallstufe) |> 
  mutate(
    Notfallstufe = 
      case_match(Notfallstufe, 
                 "Keine_Teilnahme_Notfallversorgung" ~ 0L,
                 "Basisnotfallversorgung_Stufe_1" ~ 1L, 
                 "Erweiterte_Notfallversorgung_Stufe_2" ~ 2L, 
                 "Umfassende_Notfallversorgung_Stufe_3" ~ 3L,
                 .default = NA)
    ) |> 
  filter(!is.na(Notfallstufe)) |> 
    select(Standortnummer, Notfallstufe)

Qualitaetsdaten <- 
  Qualitaetsdaten |> 
  select(-Notfallstufe) |>
  left_join(tmp, by = "Standortnummer") |> 
  select(-Standortnummer)



save(Qualitaetsdaten, file = file.path("Qualitaetsberichte", "Qualitaetsdaten.RData"))

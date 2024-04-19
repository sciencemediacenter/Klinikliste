library(xml2)
library(tidyverse)

## workspace directory und filename
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

### Für dieses Skript müssen die Quaitätsberichte bereits vorverarbeitet sein.

################################
## Anreicherung Notfallstufen ##
################################

load(file.path("Standortliste_InEK", "InEK_Krankenhausliste.RData"))
load(file.path("Qualitaetsberichte", "Qualitaetsdaten.RData"))

Qualitaetsdaten_Notfallstufen <-
  Qualitaetsdaten |>
  select(Standortnummer, Notfallstufe, Name_Qualitaetsbericht = Name) |>
  ##########
mutate(
  Standortnummer = 
    case_when( # fix falsche Standortnummern in Qualitaetsberichten 2022
      Standortnummer == "770001000" ~ "773274000",
      Standortnummer == "770002000" ~ "773562000",
      Standortnummer == "770004000" ~ "773563000",
      Standortnummer == "770000000" ~ "773783000",
      .default =  Standortnummer)) 
##########

Standorte_Notfallstufe <-
  Standorte |>
  full_join(Qualitaetsdaten_Notfallstufen,
            by = c(Einrichtung_Standortnummer = "Standortnummer"))

## Validierung merge Notfallstufe
Standorte_Notfallstufe |>
  filter(Notfallstufe >= 0,
         is.na(Version)) |>
  select(ReferenzKrankenhaus_IK, Einrichtung_Standortnummer) |>
  left_join(Qualitaetsdaten,
            by = c(Einrichtung_Standortnummer = "Standortnummer"))

## Entferne Notfallstufen ohne InEK match

Standorte_Notfallstufe <-
  Standorte_Notfallstufe |>
  filter(!is.na(Version))

## Validierung Kliniken ohne Notfallstufe
Standorte_Notfallstufe |> 
  filter(Einrichtung_Einrichtungstyp == "00", 
         is.na(Notfallstufe),
         is.na(GültigBis) | GültigBis >= today())

save(
  Standorte_Notfallstufe,
  Krankenhaeuser,
  file = file.path("Standortliste_InEK", "Standorte_mit_Notfallstufe.RData")
)
write_csv(
  Standorte_Notfallstufe,
  file = file.path("Standortliste_InEK", "Standorte_mit_Notfallstufe.csv")
)
write_csv(Krankenhaeuser,
          file = file.path("Standortliste_InEK", "Krankenhaeuser.csv"))



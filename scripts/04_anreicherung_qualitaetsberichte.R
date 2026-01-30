# 04_anreicherung_qualitaetsberichte.R
# ====================================
# Enriches InEK hospital data with emergency level information from quality reports.
# Prerequisites: Run scripts 01 and 03 first!

# Setup -----------------------------------------------------------
library(xml2)
library(tidyverse)
library(glue)

# Set working directory to repository root
if (rstudioapi::isAvailable()) {
  setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
}

# Load configuration
source("config.R")

################################
## Anreicherung Notfallstufen ##
################################

# Load preprocessed data
load(file.path(PATH_INEK_OUTPUT, "InEK_Krankenhausliste.RData"))
load(file.path(PATH_QB_OUTPUT, glue("Qualitaetsdaten_{JAHR}.Rdata")))

# Standardize column names
colnames(qualitaetsdaten) <- tools::toTitleCase(colnames(qualitaetsdaten))

# Fix known incorrect Standortnummern in quality reports
Qualitaetsdaten_Notfallstufen <- qualitaetsdaten |>
  mutate(
    Standortnummer = case_when(
      # Fix incorrect Standortnummern in Qualitaetsberichten 2022
      Standortnummer == "770001000" ~ "773274000",
      Standortnummer == "770002000" ~ "773562000",
      Standortnummer == "770004000" ~ "773563000",
      Standortnummer == "770000000" ~ "773783000",
      .default = Standortnummer
    )
  )

# Merge InEK locations with quality report data
Standorte_Notfallstufe <- Standorte |>
  full_join(
    Qualitaetsdaten_Notfallstufen,
    by = c(Einrichtung_Standortnummer = "Standortnummer")
  )

# Validation: Check for unmatched emergency levels
unmatched <- Standorte_Notfallstufe |>
  filter(Notfallstufe >= 0, is.na(Version)) |>
  select(ReferenzKrankenhaus_IK, Einrichtung_Standortnummer) |>
  left_join(
    qualitaetsdaten,
    by = c(Einrichtung_Standortnummer = "Standortnummer")
  )

if (nrow(unmatched) > 0) {
  warning(glue("{nrow(unmatched)} locations with emergency levels could not be matched to InEK data"))
}

# Remove entries without InEK match
Standorte_Notfallstufe <- Standorte_Notfallstufe |>
  filter(!is.na(Version))

# Validation: Check hospitals without emergency level
missing_notfallstufe <- Standorte_Notfallstufe |>
  filter(
    Einrichtung_Einrichtungstyp == "00",
    is.na(Notfallstufe),
    is.na(GültigBis) | GültigBis >= today()
  )

if (nrow(missing_notfallstufe) > 0) {
  message(glue("{nrow(missing_notfallstufe)} active hospital locations have no emergency level assigned"))
}

# Save results
save(
  Standorte_Notfallstufe,
  Krankenhaeuser,
  file = file.path(PATH_INEK_OUTPUT, "Standorte_mit_Notfallstufe.RData")
)

write_csv(
  Standorte_Notfallstufe,
  file = file.path(PATH_INEK_OUTPUT, "Standorte_mit_Notfallstufe.csv")
)

write_csv(
  Krankenhaeuser,
  file = file.path(PATH_INEK_OUTPUT, "Krankenhaeuser.csv")
)

message(glue("Finished enrichment for year {JAHR}"))

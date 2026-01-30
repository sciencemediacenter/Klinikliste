# 99_extract_single_hospital.R
# ============================
# Einzelnes Krankenhaus abfragen - ohne alle Daten verarbeiten zu müssen.
#
# Dieses Skript lädt die Funktionen zum Abfragen einzelner Krankenhäuser
# und enthält Beispiele zur Nutzung.
#
# Funktionen:
#   - list_available_years()        Verfügbare Jahre anzeigen
#   - list_available_hospitals()    Verfügbare Krankenhäuser eines Jahres
#   - extract_hospital()            Daten eines Krankenhauses (ein Jahr)
#   - extract_hospital_history()    Daten eines Krankenhauses (mehrere Jahre)
#   - hospital_info()               Kurzform für Basisdaten

# Setup -----------------------------------------------------------
library(tidyverse)
library(xml2)
library(glue)

# Set working directory to repository root (if in RStudio)
if (rstudioapi::isAvailable()) {
  setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
}

# Load configuration and utilities
source("config.R")
source("scripts/utils/utils_extract_single_hospital.R")


# =============================================================================
# Beispiele: Einzelnes Jahr
# =============================================================================

# Verfügbare Jahre anzeigen
list_available_years()

# Alle verfügbaren Krankenhäuser auflisten (für aktuelles Jahr aus config.R)
hospitals <- list_available_hospitals()

# Krankenhaus per IK-Nummer finden
data <- extract_hospital(ik = "260100023")

# Krankenhaus per Standortnummer finden
data <- extract_hospital(standortnummer = "773287000")

# Krankenhaus per Name suchen (Teilstring, case-insensitive)
data <- extract_hospital(name = "Charité")

# Mehrere Datentypen extrahieren
data <- extract_hospital(
  ik = "260100023",
  extract = c("basic", "prozeduren", "diagnosen")
)

# Leistungsangebot (enthält zwei tibbles: fachabteilung und ambulanz)
data <- extract_hospital(
  ik = "260100023",
  extract = c("basic", "leistungsangebot")
)

# Alle verfügbaren Daten extrahieren
data <- extract_hospital(ik = "260100023", extract = "all")

# Nur Basisinformationen (Kurzform)
info <- hospital_info(ik = "260100023")


# =============================================================================
# Beispiele: Mehrere Jahre (History)
# =============================================================================

# Basisdaten über alle verfügbaren Jahre extrahieren
history <- extract_hospital_history(ik = "260100023")

# Bestimmte Jahre auswählen
history <- extract_hospital_history(
  ik = "260100023",
  jahre = c(2022, 2023)
)

# Mehrere Datentypen über mehrere Jahre
history <- extract_hospital_history(
  ik = "260100023",
  jahre = c(2022, 2023),
  extract = c("basic", "prozeduren")
)

# Ergebnis: Tibbles mit Jahr-Spalte
# history$basic       -> tibble mit Jahr, name, betten, ...
# history$prozeduren  -> tibble mit Jahr, IK, OPS_301, ...

# Beispiel: Bettenentwicklung analysieren
history$basic |> select(Jahr, name, betten)

# =============================================================================
# Verfügbare Datentypen (extract Parameter)
# =============================================================================
#
# - "basic"              Name, Adresse, Betten, Notfallstufe
# - "prozeduren"         Prozeduren mit Fallzahlen
# - "diagnosen"          Diagnosen mit Fallzahlen
# - "leistungsangebot"   Medizinisches Leistungsangebot (zwei tibbles:
#                        leistungsangebot_fachabteilung, leistungsangebot_ambulanz)
# - "fachabteilungen"    Fachabteilungsschlüssel
# - "dokumentationsraten" Dokumentationsraten (aus DAS-Datei)
# - "qs_ergebnisse"      QS-Ergebnisse (aus DAS-Datei)
# - "all"                Alle obigen Datentypen

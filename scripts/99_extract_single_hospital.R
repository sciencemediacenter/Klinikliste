# 99_extract_single_hospital.R
# ============================
# Einzelnes Krankenhaus abfragen - ohne alle Daten verarbeiten zu müssen.
#
# Dieses Skript lädt die Funktionen zum Abfragen einzelner Krankenhäuser
# und enthält Beispiele zur Nutzung.

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
# Beispiele
# =============================================================================

# Alle verfügbaren Krankenhäuser auflisten
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

data <- extract_hospital(
  ik = "260100023",
  extract = c("basic", "leistungsangebot")
)


# Alle verfügbaren Daten extrahieren
data <- extract_hospital(ik = "260100023", extract = "all")

# Nur Basisinformationen (Kurzform)
info <- hospital_info(ik = "260100023")

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

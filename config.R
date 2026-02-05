# Klinikliste Configuration
# =========================
# Edit this file to set processing parameters for all scripts.
# All scripts source this file to get consistent settings.

# Year to process
# Change this to process different years (e.g., 2021, 2022, 2023)
JAHR <- 2023

# Paths (relative to repository root)
PATH_DATA_RAW <- "data-raw"
PATH_DATA_OUTPUT <- "data"

# Parallel processing settings
# Uses all available cores minus 4 to keep system responsive
N_CORES <- parallel::detectCores() - 4

# Derived paths (don't edit these)
PATH_QB_RAW <- file.path(PATH_DATA_RAW, "Qualitaetsberichte")
PATH_QB_OUTPUT <- file.path(PATH_DATA_OUTPUT, "Qualitaetsberichte", JAHR)
PATH_INEK_RAW <- file.path(PATH_DATA_RAW, "Standortliste_InEK")
PATH_INEK_OUTPUT <- file.path(PATH_DATA_OUTPUT, "Standortliste_InEK")

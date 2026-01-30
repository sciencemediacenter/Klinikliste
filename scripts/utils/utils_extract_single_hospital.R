# utils_extract_single_hospital.R
# ================================
# Funktionen zum Abfragen einzelner Krankenhäuser.
# Wird von scripts/99_extract_single_hospital.R verwendet.
#
# Benötigt: config.R, utils_xml.R, utils_das.R (werden vom aufrufenden Skript geladen)

# Lade Abhängigkeiten falls noch nicht geladen
if (!exists("PATH_QB_RAW")) source("config.R")
if (!exists("get_kh_path")) source("scripts/utils/utils_xml.R")
if (!exists("read_qualitaetsberichte_das_dokumentationsraten")) source("scripts/utils/utils_das.R")


# =============================================================================
# Lookup Functions
# =============================================================================

#' list_available_hospitals
#'
#' Returns a tibble of all available hospitals for a given year,
#' parsed from the XML filenames.
#'
#' @param jahr Integer, year to search (default from config.R)
#' @return tibble with columns: ik, standortnummer, file_xml, file_das
list_available_hospitals <- function(jahr = JAHR) {
  xml_path <- file.path(PATH_QB_RAW, glue("xml_{jahr}"))

  if (!dir.exists(xml_path)) {
    stop(glue("Directory not found: {xml_path}\nCheck that data-raw contains XML files for year {jahr}"))
  }

  # Get all XML files (main quality reports, not das/iqtig)
  all_files <- list.files(xml_path, pattern = glue("{jahr}-xml\\.xml$"), full.names = TRUE)

  if (length(all_files) == 0) {
    stop(glue("No XML files found in {xml_path}"))
  }

  # Parse filenames to extract IK and Standortnummer
  # Pattern: {IK}-{Standortnummer}-{Jahr}-xml.xml
  hospitals <- tibble(file_xml = all_files) |>
    mutate(
      basename = basename(file_xml),
      ik = str_extract(basename, "^[0-9]+"),
      standortnummer = str_extract(basename, "(?<=-)[0-9]{9}(?=-)"),
      # Also find corresponding DAS file
      file_das = str_replace(file_xml, "-xml\\.xml$", "-das.xml"),
      file_das = if_else(file.exists(file_das), file_das, NA_character_)
    ) |>
    select(ik, standortnummer, file_xml, file_das)

  return(hospitals)
}


#' find_hospital_xml
#'
#' Finds XML file(s) for a hospital by IK, Standortnummer, or name.
#' At least one search criterion must be provided.
#'
#' @param ik Character, IK number (e.g., "260100023")
#' @param standortnummer Character, 9-digit location number (e.g., "773287000")
#' @param name Character, partial name to search for (case-insensitive)
#' @param jahr Integer, year (default from config.R)
#' @return tibble with matching hospitals and their file paths
find_hospital_xml <- function(ik = NULL, standortnummer = NULL, name = NULL, jahr = JAHR) {

  if (is.null(ik) && is.null(standortnummer) && is.null(name)) {
    stop("At least one search criterion must be provided: ik, standortnummer, or name")
  }

  # Get all available hospitals
  hospitals <- list_available_hospitals(jahr)

  # Filter by IK
  if (!is.null(ik)) {
    hospitals <- hospitals |> filter(ik == !!ik)
  }

  # Filter by Standortnummer
  if (!is.null(standortnummer)) {
    hospitals <- hospitals |> filter(standortnummer == !!standortnummer)
  }

  # Filter by name (requires reading XML files - slower)
  if (!is.null(name)) {
    # Read basic info from each remaining file to get names
    hospitals <- hospitals |>
      mutate(
        hospital_name = map_chr(file_xml, function(f) {
          tryCatch({
            xml_data <- read_xml(f)
            kh_path <- get_kh_path(xml_data)
            xml_text(xml_find_first(xml_data, glue("//{kh_path}/Name")))
          }, error = function(e) NA_character_)
        })
      ) |>
      filter(str_detect(str_to_lower(hospital_name), str_to_lower(!!name)))
  }

  if (nrow(hospitals) == 0) {
    message("No hospitals found matching the search criteria.")
  }

  return(hospitals)
}


# =============================================================================
# Main Extraction Function
# =============================================================================

#' extract_hospital
#'
#' Extracts data for a single hospital.
#'
#' @param ik Character, IK number
#' @param standortnummer Character, 9-digit location number
#' @param name Character, partial name to search for
#' @param jahr Integer, year (default from config.R)
#' @param extract Character vector specifying which data to extract:
#'   - "basic": Hospital info (name, address, beds, emergency level)
#'   - "prozeduren": Procedures with case counts
#'   - "diagnosen": Diagnoses with case counts
#'   - "leistungsangebot": Medical services offered (returns two tibbles:
#'       leistungsangebot_fachabteilung and leistungsangebot_ambulanz)
#'   - "fachabteilungen": Department codes
#'   - "dokumentationsraten": Documentation rates (from DAS file)
#'   - "qs_ergebnisse": Quality results (from DAS file)
#'   - "all": Extract everything
#'   Default: c("basic")
#' @param save_to Optional file path to save results as RData
#' @return Named list with requested data frames
#'
#' @examples
#' # Basic hospital info only (fast)
#' data <- extract_hospital(ik = "260100023")
#'
#' # Multiple data types
#' data <- extract_hospital(ik = "260100023",
#'                          extract = c("basic", "prozeduren", "diagnosen"))
#'
#' # Everything
#' data <- extract_hospital(ik = "260100023", extract = "all")
extract_hospital <- function(ik = NULL, standortnummer = NULL, name = NULL,
                             jahr = JAHR,
                             extract = c("basic"),
                             save_to = NULL) {

  # Handle "all" shortcut
  if ("all" %in% extract) {
    extract <- c("basic", "prozeduren", "diagnosen", "leistungsangebot",
                 "fachabteilungen", "dokumentationsraten", "qs_ergebnisse")
  }

  # Find the hospital
  found <- find_hospital_xml(ik = ik, standortnummer = standortnummer, name = name, jahr = jahr)

  if (nrow(found) == 0) {
    return(NULL)
  }

  if (nrow(found) > 1) {
    message(glue("Found {nrow(found)} matching hospitals:"))
    if ("hospital_name" %in% names(found)) {
      print(found |> select(ik, standortnummer, hospital_name))
    } else {
      print(found |> select(ik, standortnummer))
    }
    message("\nPlease refine your search to select a single hospital.")
    return(found)
  }

  # Single hospital found
  xml_file <- found$file_xml[1]
  das_file <- found$file_das[1]

  message(glue("Extracting data from: {basename(xml_file)}"))

  # Initialize result list
  result <- list()

  # Extract requested data types
  if ("basic" %in% extract) {
    message("  - Basic hospital info...")
    result$basic <- read_qualitaetsberichte_xml(xml_file)
    # Print summary
    message(glue("    Hospital: {result$basic$name}"))
    message(glue("    IK: {result$basic$ik}, Standort: {result$basic$standortnummer}"))
    message(glue("    Beds: {result$basic$betten}, Emergency level: {result$basic$notfallstufe}"))
  }

  if ("prozeduren" %in% extract) {
    message("  - Procedures (Prozeduren)...")
    result$prozeduren <- read_qualitaetsberichte_xml_prozeduren(xml_file)
    message(glue("    {nrow(result$prozeduren)} procedures found"))
  }

  if ("diagnosen" %in% extract) {
    message("  - Diagnoses (Diagnosen)...")
    result$diagnosen <- read_qualitaetsberichte_xml_diagnosen(xml_file)
    message(glue("    {nrow(result$diagnosen)} diagnoses found"))
  }

  if ("leistungsangebot" %in% extract) {
    message("  - Medical services (Leistungsangebot)...")
    leistungsangebot_raw <- read_qualitaetsberichte_xml_medizinisches_leistungsangebot(xml_file)
    result$leistungsangebot_fachabteilung <- leistungsangebot_raw$fachabteilung
    result$leistungsangebot_ambulanz <- leistungsangebot_raw$ambulanz
    message(glue("    {nrow(result$leistungsangebot_fachabteilung)} services (Fachabteilung), {nrow(result$leistungsangebot_ambulanz)} services (Ambulanz)"))
  }

  if ("fachabteilungen" %in% extract) {
    message("  - Department codes (Fachabteilungen)...")
    result$fachabteilungen <- read_qualitaetsberichte_xml_fachabteilungsschluessel(xml_file)
    message(glue("    {nrow(result$fachabteilungen)} departments found"))
  }

  # DAS file extractions
  if ("dokumentationsraten" %in% extract) {
    if (!is.na(das_file)) {
      message("  - Documentation rates (from DAS file)...")
      raw <- read_qualitaetsberichte_das_dokumentationsraten(das_file)
      if (nrow(raw) > 0) {
        result$dokumentationsraten <- unnest_and_convert_qualitaetsberichte_das_dokumentationsraten(raw)
        message(glue("    {nrow(result$dokumentationsraten)} entries found"))
      } else {
        result$dokumentationsraten <- tibble()
        message("    No documentation rates found")
      }
    } else {
      message("  - Documentation rates: DAS file not available")
      result$dokumentationsraten <- NULL
    }
  }

  if ("qs_ergebnisse" %in% extract) {
    if (!is.na(das_file)) {
      message("  - QS results (from DAS file)...")
      raw <- read_qualitaetsberichte_das_ergebnis(das_file)
      if (nrow(raw) > 0) {
        result$qs_ergebnisse <- unnest_and_convert_qualitaetsberichte_das_ergebnis(raw)
        message(glue("    {nrow(result$qs_ergebnisse)} results found"))
      } else {
        result$qs_ergebnisse <- tibble()
        message("    No QS results found")
      }
    } else {
      message("  - QS results: DAS file not available")
      result$qs_ergebnisse <- NULL
    }
  }

  # Optionally save to file
  if (!is.null(save_to)) {
    save(result, file = save_to)
    message(glue("\nResults saved to: {save_to}"))
  }

  message("\nDone!")
  return(result)
}


# =============================================================================
# Convenience wrapper for quick lookups
# =============================================================================

#' hospital_info
#'
#' Quick lookup of basic hospital information.
#' Wrapper around extract_hospital() that only extracts basic info.
#'
#' @param ... Arguments passed to extract_hospital()
#' @return tibble with basic hospital info (single row)
hospital_info <- function(...) {
  result <- extract_hospital(..., extract = "basic")
  if (is.list(result) && "basic" %in% names(result)) {
    return(result$basic)
  }
  return(result)
}

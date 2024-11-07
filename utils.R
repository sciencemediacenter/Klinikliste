
#' read_qualitaetsberichte_xml
#' Funktion zum Einlesen und Verarbeiten der XML-Qualitätsberichte
#' @param file_path string, Pfad zu einer XML-Datei
#' @param debugmode boolean, Im Debugmode wird der Dateiname zusätzlich
#' ausgeben, um ggf. manuell die Datenextraktion zu überprüfen.
#' Default ist FALSE
#' @details
#' Einige Angaben für die Teilnahme an der Notfallversorgung erscheinen
#' nicht plausibel bzw. sind anscheinend nicht vollständig in den
#' Qualitätsberichten dokumentiert. Mehrfach haben große
#' Universitätskliniken bspw. keine Chest Pain Unit oder Stroke Unit,
#' was Angaben verschiedener Zertifizierungsstellen widerspricht.
#'
#' @return dataframe mit den Adressdaten, IK- bzw. Standortnummer und
#' Notfallstufe
#' @example \dontrun{
#'  xml_files <- list.files(directory, pattern = "\\.xml$", full.names = TRUE)
#'  xml_files <- xml_files[grepl("2022-xml", xml_files)]
#'  qualitaetsdaten <- lapply(xml_files, read_process_xml)
#'  qualitaetsdaten <- bind_rows(qualitaetsdaten_swr)
#' }

read_qualitaetsberichte_xml <-
  function(
    file_path,
    debugmode = FALSE) {
    xml_data <- read_xml(file_path)
    
  # Gibt es mehrere Standorte?
    mehrere_standorte <- length(xml_children(xml_find_all(xml_data, "//Krankenhaus/Mehrere_Standorte"))) == 2
    kh_path <- ifelse(mehrere_standorte, "Standortkontaktdaten", "Krankenhauskontaktdaten")

    ik <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/IK")))
    name <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/Name")))
    strasse <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/Kontakt_Zugang/Strasse")))
    hausnummer <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/Kontakt_Zugang/Hausnummer")))
    plz <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/Kontakt_Zugang/Postleitzahl")))
    ort <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/Kontakt_Zugang/Ort")))
    standortnummer <- xml_text(xml_find_all(xml_data, glue::glue("//Standortnummer")))
    betten <- xml_text(xml_find_all(xml_data, "//Anzahl_Betten"))
    kommentar_notfallstufe <- xml_text(xml_find_all(xml_data, "//Teilnahme_Notfallstufe/Erlaeuterungen"))

    file <- basename((file_path))
    
    if (length(kommentar_notfallstufe) == 0) {
      kommentar_notfallstufe <- NA
    }
    
    # Default Notfallstufe 0
    notfallstufe <- 0
    
    if (length(xml_children(xml_find_all(xml_data, "//Basisnotfallversorgung_Stufe_1"))) > 0){
      notfallstufe <- 1
    }
    
    if (length(xml_children(xml_find_all(xml_data, "//Erweiterte_Notfallversorgung_Stufe_2"))) > 0){
      notfallstufe <- 2
    }
    
    if (length(xml_children(xml_find_all(xml_data, "//Umfassende_Notfallversorgung_Stufe_3"))) > 0){
      notfallstufe <- 3
    }
    
    spezialversorgung <- xml_text(xml_find_all(xml_data, "//Module_Spezielle_Notfallversorgung"))
    
    spezialversorgung <- get_spezialversorgung_code(spezialversorgung)
    
    ### Daten zusammenfuehren
    
    data <-
      data.frame(
        ik,
        name,
        strasse,
        hausnummer,
        plz,
        ort,
        standortnummer,
        betten,
        notfallstufe,
        kommentar_notfallstufe
      ) |>
      bind_cols(spezialversorgung)
    
    if (debugmode)
      data <- dplyr::bind_cols(data, file = file)
    
    return(data)
  }


#' get_qb_xml_fils
#'
#' Erzeugt einen Vektor mit den Dateipfaden der Qualitaetsberichte
#'
#' @return Vektor

get_qb_xml_files <-
  function(file_path, pattern = "\\.xml$") {
    xml_files <- list.files(file_path, pattern = pattern, full.names = TRUE)
    return(xml_files)
  }

#' get_spezialversorgung_code
#'
#' ermittelt eventuell vorhandene Module der Speziellen
#' Notfallversorgung. Siehe S. 13f "Regelungen des gemeinsamen
#' Bundesausschusses"
#' @note
#' In Fällen in denen mehrere Stufen der Kindernotfallversorgung
#' angegeben wurden, werden alle Einträge komma-separiert übernommen.
#' @param codes string
#' SN01 - Modul Notfallversorgung Kinder (Basis)
#' SN02 - Modul Notfallversorgung Kinder (erweitert)
#' SN03 - Modul Notfallversorgung Kinder (umfassend)
#' SN04 - Modul Schwerverletztenversorgung
#' SN05 - Modul Schlaganfallversorgung (Stroke Unit)
#' SN06 - Modul Durchblutungsstörungen am Herzen (Chest Pain Unit)
#' @return dataframe mit den Spalten der Spezialversorgung kodiert nach
#'
#' @references
#' https://www.g-ba.de/downloads/62-492-3380/Qb-R_2023-12-21_iK-2024-02-17.pdf
#'
#'

get_spezialversorgung_code <-
  function(codes) {
    codes <- tolower(codes)
    alle_spalten <- paste0("sn", sprintf("%02d", 1:6))
    
    # DataFrame erstellen
    df <- data.frame(matrix(FALSE, nrow = 1, ncol = 6))
    colnames(df) <- alle_spalten
    
    # TRUE setzen für die Spalten, die im Vektor sind
    df[, codes] <- TRUE
    
    # definiere Kinderversorgung
    kinder_versorgung <-
      c("sn01" = "basis",
        "sn02" = "erweitert",
        "sn03" = "umfassend")
    df$notfallversorgung_kinder <- NA
    if (length(kinder_versorgung[df[, names(kinder_versorgung)] |> unlist()] |> unname()) > 0) {
      df$notfallversorgung_kinder <- 
        paste(kinder_versorgung[df[, names(kinder_versorgung)] |> unlist()] |> unname(), collapse = ", ")
    }
    
    df <- 
      df |>
      select(
        schwerverletztenversorgung = sn04,
        stroke_unit = sn05,
        chest_pain_unit = sn06,
        notfallversorgung_kinder
      )
    
    return(df)
    
  }


#' read_qualitaetsberichte_xml_prozeduren
#' Funktion zum Einlesen und Verarbeiten der XML-Qualitätsberichte zwecks Extraktion der Fallzahlen der Prozeduren
#' @param file_path string, Pfad zu einer XML-Datei
#' @return dataframe mit den Fallzahlen für verschiedene Prozeduren nach Standort
#' @example \dontrun{
#' xml_files <- list.files(directory, pattern = "\\.xml$", full.names = TRUE)
#' xml_files <- xml_files[grepl("2022-xml", xml_files)]
#' Fallzahlen_Prozeduren <- pblapply(xml_files, read_qualitaetsberichte_xml_prozeduren)
#' }

read_qualitaetsberichte_xml_prozeduren <- 
  function(file_path){
    require(xml2)
    require(tidyverse)
    
    extract_orgaeinheit <-
      function(einzelne_prozedur) {
        orgaeinheit_node <- xml_parents(einzelne_prozedur)[4] # go back to <Organisationseinheit-Fachabteilung>
        
        column_names <- c(xml_name(einzelne_prozedur), "OrgaEinheit_Nummer", "OrgaEinheit_Name")
        column_values <- c(xml_text(einzelne_prozedur), 
                           xml_text(xml_find_all(orgaeinheit_node, "Gliederungsnummer")),
                           xml_text(xml_find_all(orgaeinheit_node, "Name")))
        
        named_values <- setNames(column_values, column_names)
        tib <- tibble(!!!named_values)
        
        return(tib)
      }
    
    xml_data <- read_xml(file_path)
    mehrere_standorte <- length(xml_children(xml_find_all(xml_data, "//Krankenhaus/Mehrere_Standorte"))) == 2
    kh_path <- ifelse(mehrere_standorte, "Standortkontaktdaten", "Krankenhauskontaktdaten")
    
    IK <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/IK")))
    Standortnummer <- xml_text(xml_find_all(xml_data, glue::glue("//Standortnummer")))
    
    prozedur <- xml_find_all(xml_data, "//Prozedur")
    tmp <- lapply(prozedur, xml_children)
    tmp <- lapply(tmp, extract_orgaeinheit)
    # tmp <- lapply(tmp, function(x) tibble(!!!setNames(xml_text(x), xml_name(x))))
    
    prozedurliste <- bind_cols(
      IK = IK,
      Standortnummer = Standortnummer,
      bind_rows(tmp)
    )
    
    return(prozedurliste)
  }


read_qualitaetsberichte_xml_diagnosen <- 
  function(file_path){
    require(xml2)
    require(tidyverse)
    
    extract_orgaeinheit <-
      function(einzelne_diagnose) {
        orgaeinheit_node <- xml_parents(einzelne_diagnose)[3] # go back to <Organisationseinheit-Fachabteilung>
        
        column_names <- c(xml_name(einzelne_diagnose), "OrgaEinheit_Nummer", "OrgaEinheit_Name")
        column_values <- c(xml_text(einzelne_diagnose), 
                           xml_text(xml_find_all(orgaeinheit_node, "Gliederungsnummer")),
                           xml_text(xml_find_all(orgaeinheit_node, "Name")))
        
        named_values <- setNames(column_values, column_names)
        tib <- tibble(!!!named_values)
        
        return(tib)
      }
    
    xml_data <- read_xml(file_path)
    mehrere_standorte <- length(xml_children(xml_find_all(xml_data, "//Krankenhaus/Mehrere_Standorte"))) == 2
    kh_path <- ifelse(mehrere_standorte, "Standortkontaktdaten", "Krankenhauskontaktdaten")
    
    IK <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/IK")))
    Standortnummer <- xml_text(xml_find_all(xml_data, glue::glue("//Standortnummer")))
    
    hauptdiagnose <- xml_find_all(xml_data, "//Hauptdiagnose")
    tmp <- lapply(hauptdiagnose, xml_children)
    tmp <- lapply(tmp, extract_orgaeinheit)
    # tmp <- lapply(tmp, function(x) tibble(!!!setNames(xml_text(x), xml_name(x))))
    
    diagnoseliste <- bind_cols(
      IK = IK,
      Standortnummer = Standortnummer,
      bind_rows(tmp)
    )
    
    return(diagnoseliste)
  }


read_qualitaetsberichte_xml_medizinisches_leistungsangebot <- 
  function(file_path){
    require(xml2)
    require(tidyverse)
  
    xml_data <- read_xml(file_path)
    mehrere_standorte <- length(xml_children(xml_find_all(xml_data, "//Krankenhaus/Mehrere_Standorte"))) == 2
    kh_path <- ifelse(mehrere_standorte, "Standortkontaktdaten", "Krankenhauskontaktdaten")
    
    IK <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/IK")))
    Standortnummer <- xml_text(xml_find_all(xml_data, glue::glue("//Standortnummer")))
    
    medizinisches_leistungsangebot <- xml_find_all(xml_data, "//Medizinisches_Leistungsangebot")
    tmp <- lapply(medizinisches_leistungsangebot, xml_children)
    tmp <- lapply(tmp, function(x) tibble(!!!setNames(xml_text(x), xml_name(x))))
    
    prozedurliste <- bind_cols(
      IK = IK,
      Standortnummer = Standortnummer,
      bind_rows(tmp)
    )
    

read_qualitaetsberichte_xml_fachabteilungsschluessel <- 
  function(file_path){
    require(xml2)
    require(tidyverse)
    
    extract_orgaeinheit <-
      function(einzelner_fachabteilungsschluessel) {
        orgaeinheit_node <- xml_parents(einzelner_fachabteilungsschluessel)[2] # go back to <Organisationseinheit-Fachabteilung>
        
        column_names <- c(xml_name(einzelner_fachabteilungsschluessel), "OrgaEinheit_Nummer", "OrgaEinheit_Name")
        column_values <- c(xml_text(einzelner_fachabteilungsschluessel), 
                           xml_text(xml_find_all(orgaeinheit_node, "Gliederungsnummer")),
                           xml_text(xml_find_all(orgaeinheit_node, "Name")))
        
        named_values <- setNames(column_values, column_names)
        tib <- tibble(!!!named_values)
        
        return(tib)
      }
    
    xml_data <- read_xml(file_path)
    mehrere_standorte <- length(xml_children(xml_find_all(xml_data, "//Krankenhaus/Mehrere_Standorte"))) == 2
    kh_path <- ifelse(mehrere_standorte, "Standortkontaktdaten", "Krankenhauskontaktdaten")
    
    IK <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/IK")))
    Standortnummer <- xml_text(xml_find_all(xml_data, glue::glue("//Standortnummer")))
    
    fachabteilungsschluessel <- xml_find_all(xml_data, "//Fachabteilungsschluessel")
    tmp <- lapply(fachabteilungsschluessel, xml_children)
    tmp <- lapply(tmp, extract_orgaeinheit)
    # tmp <- lapply(tmp, function(x) tibble(!!!setNames(xml_text(x), xml_name(x))))
    
    fachabteilungsschluesselliste <- bind_cols(
      IK = IK,
      Standortnummer = Standortnummer,
      bind_rows(tmp)
    )
    
    return(fachabteilungsschluesselliste)
  }

#' @noRd
#' Intern: Entscheidet, ob "<Krankenhaus/Mehrere_Standorte>" existiert
#' und gibt entweder "Standortkontaktdaten" oder "Krankenhauskontaktdaten" zurück.
get_kh_path <- function(xml_data) {
  has_multiple <- length(xml_find_all(
    xml_data,
    "//Krankenhaus/Mehrere_Standorte"
  )) ==
    2
  if (has_multiple) "Standortkontaktdaten" else "Krankenhauskontaktdaten"
}

#' get_qb_xml_files
#' Erzeugt einen Vektor mit den Dateipfaden der Qualitätsberichte (XML-Dateien).
#' @param file_path character, Verzeichnis, in dem gesucht wird
#' @param pattern    character, Regex-Pattern (z.B. "\\.xml$")
#' @return character-Vektor
get_qb_xml_files <- function(file_path, pattern = "\\.xml$") {
  list.files(file_path, pattern = pattern, full.names = TRUE)
}

#' get_spezialversorgung_code
#'
#' ermittelt eventuell vorhandene Module der Speziellen
#' Notfallversorgung. Siehe S. 13f "Regelungen des gemeinsamen
#' Bundesausschusses"
#' @note
#' In Fällen in denen mehrere Stufen der Kindernotfallversorgung
#' angegeben wurden, werden alle Einträge komma-separiert übernommen.
#'
#' Kodiert vorhandene "Module_Spezielle_Notfallversorgung".
#' @param codes character, Vektor von Codes (z.B. c("SN01","SN04")),
#'              kann auch Länge 0 haben.
#' SN01 - Modul Notfallversorgung Kinder (Basis)
#' SN02 - Modul Notfallversorgung Kinder (erweitert)
#' SN03 - Modul Notfallversorgung Kinder (umfassend)
#' SN04 - Modul Schwerverletztenversorgung
#' SN05 - Modul Schlaganfallversorgung (Stroke Unit)
#' SN06 - Modul Durchblutungsstörungen am Herzen (Chest Pain Unit)
#'
#' @references
#' https://www.g-ba.de/downloads/62-492-3380/Qb-R_2023-12-21_iK-2024-02-17.pdf
#' @return tibble mit Spalten:
#'   - schwerverletztenversorgung (logical),
#'   - stroke_unit (logical),
#'   - chest_pain_unit (logical),
#'   - notfallversorgung_kinder (character, z.B. "basis, umfassend" oder NA)
get_spezialversorgung_code <- function(codes) {
  # in Kleinschreibung
  codes <- tolower(codes %||% character(0)) # safely ensure that codes is not NULL

  # Wir definieren alle möglichen Spalten c("sn01","sn02",...,"sn06")
  alle_spalten <- paste0("sn", sprintf("%02d", 1:6))

  # Erzeuge eine Länge-1-tibble mit allen Spalten FALSE
  df <- tibble(
    !!!setNames(as.list(rep(FALSE, length(alle_spalten))), alle_spalten)
  )

  # Setze TRUE, wo der Code im Vektor vorkommt
  present <- intersect(alle_spalten, codes)
  if (length(present) > 0) {
    df <- df %>% mutate(across(all_of(present), ~TRUE))
  }

  # Kinderversorgung: SN01–SN03 => "basis", "erweitert", "umfassend"
  kinder_map <- c("sn01" = "basis", "sn02" = "erweitert", "sn03" = "umfassend")
  kind_codes <- names(kinder_map)[as.logical(df[1, names(kinder_map)])]
  if (length(kind_codes) > 0) {
    kind_vals <- kinder_map[kind_codes]
    df <- df %>%
      mutate(notfallversorgung_kinder = paste(kind_vals, collapse = ", "))
  } else {
    df <- df %>% mutate(notfallversorgung_kinder = NA_character_)
  }

  # Nur die vier gewünschten Spalten zurückgeben
  df %>%
    select(
      schwerverletztenversorgung = sn04,
      stroke_unit = sn05,
      chest_pain_unit = sn06,
      notfallversorgung_kinder
    )
}

#' read_qualitaetsberichte_xml
#' Funktion zum Einlesen und Verarbeiten der XML-Qualitätsberichte.
#' Extrahiert Adressdaten, IK, Standortnummer und Notfallstufe.
#' @param file_path character, Pfad zur XML-Datei
#' @param debugmode logical, (optional) wenn TRUE, füge Spalte "file" hinzu
#' @return tibble mit den Feldern:
#'   ik, name, strasse, hausnummer, plz, ort, standortnummer, betten,
#'   notfallstufe (0–3), kommentar_notfallstufe, plus die Spezialversorgungs-Spalten.
read_qualitaetsberichte_xml <- function(file_path, debugmode = FALSE) {
  # 1) XML laden
  xml_data <- read_xml(file_path)

  # 2) KH-Pfad bestimmen
  kh_path <- get_kh_path(xml_data)

  # 3) Funktion, um Text an einem XPath //{kh_path}/... abzurufen
  extract_text <- function(unter_path) {
    xml_data %>%
      xml_find_all(glue::glue("//{kh_path}/{unter_path}")) %>%
      xml_text()
  }

  # 4) Adress-/Kontakt-Felder
  ik <- extract_text("IK")
  name <- extract_text("Name")
  strasse <- extract_text("Kontakt_Zugang/Strasse")
  hausnummer <- extract_text("Kontakt_Zugang/Hausnummer")
  plz <- extract_text("Kontakt_Zugang/Postleitzahl")
  ort <- extract_text("Kontakt_Zugang/Ort")
  standortnummer <- xml_data %>% xml_find_all("//Standortnummer") %>% xml_text()
  betten <- xml_data %>% xml_find_all("//Anzahl_Betten") %>% xml_text()

  kommentar_notfallstufe <- xml_data %>%
    xml_find_all("//Teilnahme_Notfallstufe/Erlaeuterungen") %>%
    xml_text()
  if (length(kommentar_notfallstufe) == 0) {
    kommentar_notfallstufe <- NA_character_
  }

  # 5) Notfallstufe 0–3 (in absteigender Priorität)
  notfallstufe <- dplyr::case_when(
    xml_length(xml_find_all(
      xml_data,
      "//Umfassende_Notfallversorgung_Stufe_3"
    )) >
      0 ~
      3L,
    xml_length(xml_find_all(
      xml_data,
      "//Erweiterte_Notfallversorgung_Stufe_2"
    )) >
      0 ~
      2L,
    xml_length(xml_find_all(xml_data, "//Basisnotfallversorgung_Stufe_1")) > 0 ~
      1L,
    TRUE ~ 0L
  )

  # 6) Spezialversorgung auslesen und codieren
  spezial_codes <- xml_data %>%
    xml_find_all("//Module_Spezielle_Notfallversorgung") %>%
    xml_text()
  spezialversorgung <- get_spezialversorgung_code(spezial_codes)

  # 7) Alles zusammenführen
  result <- tibble(
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
  ) %>%
    bind_cols(spezialversorgung)

  if (debugmode) {
    result <- result %>% mutate(file = basename(file_path))
  }

  return(result)
}

#' read_qualitaetsberichte_xml_prozeduren
#' Liest für jede Organisationseinheit die Fallzahlen der Prozeduren.
#' 
#' Hinweis zur Datenstruktur: Jedes XML-Kind-Element einer <Prozedur> wird als 
#' eigene Zeile extrahiert. Dies bedeutet:
#' - OPS_301 (Prozedurcode) und Anzahl/Anzahl_Datenschutz sind separate Zeilen
#' - Ein leerer String ("") in der Wertspalte bedeutet, dass die Fallzahl aus 
#'   Datenschutzgründen nicht angegeben wird (stammt aus <Anzahl_Datenschutz/>)
#' - Bei Bedarf können die Zeilen über OrgaEinheit_Nummer gruppiert/zusammengeführt werden
#'
#' @param file_path character, Pfad zur XML-Datei
#' @return tibble mit Spalten: IK, Standortnummer, <Elementname>, OrgaEinheit_Nummer, OrgaEinheit_Name
#'   Mögliche Elementnamen: OPS_301, Anzahl, Anzahl_Datenschutz
read_qualitaetsberichte_xml_prozeduren <-
  function(file_path) {
    extract_orgaeinheit <-
      function(einzelne_prozedur) {
        orgaeinheit_node <- xml_parents(einzelne_prozedur)[4] # go back to <Organisationseinheit-Fachabteilung>

        column_names <- c(
          xml_name(einzelne_prozedur),
          "OrgaEinheit_Nummer",
          "OrgaEinheit_Name"
        )
        column_values <- c(
          xml_text(einzelne_prozedur),
          xml_text(xml_find_all(orgaeinheit_node, "Gliederungsnummer")),
          xml_text(xml_find_all(orgaeinheit_node, "Name"))
        )

        named_values <- setNames(column_values, column_names)
        tib <- tibble(!!!named_values)

        return(tib)
      }

    xml_data <- read_xml(file_path)
    mehrere_standorte <- length(xml_children(xml_find_all(
      xml_data,
      "//Krankenhaus/Mehrere_Standorte"
    ))) ==
      2
    kh_path <- ifelse(
      mehrere_standorte,
      "Standortkontaktdaten",
      "Krankenhauskontaktdaten"
    )

    IK <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/IK")))
    Standortnummer <- xml_text(xml_find_all(
      xml_data,
      glue::glue("//Standortnummer")
    ))

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


#' read_qualitaetsberichte_xml_diagnosen
#' Liest für jede Organisationseinheit die Fallzahlen der Hauptdiagnosen.
#' 
#' Hinweis zur Datenstruktur: Jedes XML-Kind-Element einer <Hauptdiagnose> wird als 
#' eigene Zeile extrahiert. Dies bedeutet:
#' - ICD_10 (Diagnosecode) und Anzahl/Anzahl_Datenschutz sind separate Zeilen
#' - Ein leerer String ("") in der Wertspalte bedeutet, dass die Fallzahl aus 
#'   Datenschutzgründen nicht angegeben wird (stammt aus <Anzahl_Datenschutz/>)
#' - Bei Bedarf können die Zeilen über OrgaEinheit_Nummer gruppiert/zusammengeführt werden
#'
#' @param file_path character, Pfad zur XML-Datei
#' @return tibble mit Spalten: IK, Standortnummer, <Elementname>, OrgaEinheit_Nummer, OrgaEinheit_Name
#'   Mögliche Elementnamen: ICD_10, Anzahl, Anzahl_Datenschutz

read_qualitaetsberichte_xml_diagnosen <-
  function(file_path) {
    extract_orgaeinheit <-
      function(einzelne_diagnose) {
        orgaeinheit_node <- xml_parents(einzelne_diagnose)[3] # go back to <Organisationseinheit-Fachabteilung>

        column_names <- c(
          xml_name(einzelne_diagnose),
          "OrgaEinheit_Nummer",
          "OrgaEinheit_Name"
        )
        column_values <- c(
          xml_text(einzelne_diagnose),
          xml_text(xml_find_all(orgaeinheit_node, "Gliederungsnummer")),
          xml_text(xml_find_all(orgaeinheit_node, "Name"))
        )

        named_values <- setNames(column_values, column_names)
        tib <- tibble(!!!named_values)

        return(tib)
      }

    xml_data <- read_xml(file_path)
    mehrere_standorte <- length(xml_children(xml_find_all(
      xml_data,
      "//Krankenhaus/Mehrere_Standorte"
    ))) ==
      2
    kh_path <- ifelse(
      mehrere_standorte,
      "Standortkontaktdaten",
      "Krankenhauskontaktdaten"
    )

    IK <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/IK")))
    Standortnummer <- xml_text(xml_find_all(
      xml_data,
      glue::glue("//Standortnummer")
    ))

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

#' read_qualitaetsberichte_xml_medizinisches_leistungsangebot
#' Liest das medizinische Leistungsangebot aus zwei Quellen:
#' 1. Organisationseinheiten/Fachabteilungen - unter Organisationseinheit_Fachabteilung
#' 2. Ambulanzen - unter Ambulante_Behandlungsmoeglichkeiten/Ambulanz
#'
#' Die XML-Strukturen unterscheiden sich:
#' - Fachabteilung: Medizinisches_Leistungsangebot enthält VA_VU_Schluessel,
#'   Parent ist Organisationseinheit_Fachabteilung mit Gliederungsnummer und Name
#' - Ambulanz: Medizinisches_Leistungsangebot enthält VA_VU_Schluessel_Ambulanz,
#'   Parent ist Ambulanz mit AM_Schluessel und Bezeichnung
#'
#' @param file_path character, Pfad zur XML-Datei
#' @return list mit zwei tibbles:
#'   - fachabteilung: IK, Standortnummer, <VA_VU_Schluessel>, OrgaEinheit_Nummer, OrgaEinheit_Name
#'   - ambulanz: IK, Standortnummer, <VA_VU_Schluessel_Ambulanz>, Ambulanz_Schluessel, Ambulanz_Bezeichnung

read_qualitaetsberichte_xml_medizinisches_leistungsangebot <-
  function(file_path) {
    
    # Helper für Fachabteilung-Einträge
    extract_fachabteilung <- function(node) {
      # Parent[3] = Organisationseinheit_Fachabteilung
      orgaeinheit_node <- xml_parents(node)[3]
      
      column_names <- c(
        xml_name(node),
        "OrgaEinheit_Nummer",
        "OrgaEinheit_Name"
      )
      column_values <- c(
        xml_text(node),
        xml_text(xml_find_first(orgaeinheit_node, "Gliederungsnummer")),
        xml_text(xml_find_first(orgaeinheit_node, "Name"))
      )
      
      named_values <- setNames(column_values, column_names)
      tibble(!!!named_values)
    }
    
    # Helper für Ambulanz-Einträge
    extract_ambulanz <- function(node) {
      # Parent[3] = Ambulanz
      ambulanz_node <- xml_parents(node)[3]
      
      column_names <- c(
        xml_name(node),
        "Ambulanz_Schluessel",
        "Ambulanz_Bezeichnung"
      )
      column_values <- c(
        xml_text(node),
        xml_text(xml_find_first(ambulanz_node, "AM_Schluessel")),
        xml_text(xml_find_first(ambulanz_node, "Bezeichnung"))
      )
      
      named_values <- setNames(column_values, column_names)
      tibble(!!!named_values)
    }

    xml_data <- read_xml(file_path)
    mehrere_standorte <- length(xml_children(xml_find_all(
      xml_data,
      "//Krankenhaus/Mehrere_Standorte"
    ))) == 2
    kh_path <- ifelse(
      mehrere_standorte,
      "Standortkontaktdaten",
      "Krankenhauskontaktdaten"
    )

    IK <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/IK")))
    Standortnummer <- xml_text(xml_find_all(
      xml_data,
      glue::glue("//Standortnummer")
    ))

    # Fachabteilung: VA_VU_Schluessel (nicht Ambulanz)
    fachabteilung_nodes <- xml_find_all(
      xml_data,
      "//Organisationseinheit_Fachabteilung//Medizinisches_Leistungsangebot/*[not(self::Erlaeuterungen)]"
    )
    
    if (length(fachabteilung_nodes) > 0) {
      tmp_fa <- lapply(fachabteilung_nodes, extract_fachabteilung)
      leistungsangebot_fachabteilung <- bind_cols(
        IK = IK,
        Standortnummer = Standortnummer,
        bind_rows(tmp_fa)
      )
    } else {
      leistungsangebot_fachabteilung <- tibble(
        IK = character(),
        Standortnummer = character(),
        OrgaEinheit_Nummer = character(),
        OrgaEinheit_Name = character()
      )
    }

    # Ambulanz: VA_VU_Schluessel_Ambulanz
    ambulanz_nodes <- xml_find_all(
      xml_data,
      "//Ambulanz//Medizinisches_Leistungsangebot/VA_VU_Schluessel_Ambulanz"
    )
    
    if (length(ambulanz_nodes) > 0) {
      tmp_amb <- lapply(ambulanz_nodes, extract_ambulanz)
      leistungsangebot_ambulanz <- bind_cols(
        IK = IK,
        Standortnummer = Standortnummer,
        bind_rows(tmp_amb)
      )
    } else {
      leistungsangebot_ambulanz <- tibble(
        IK = character(),
        Standortnummer = character(),
        Ambulanz_Schluessel = character(),
        Ambulanz_Bezeichnung = character()
      )
    }

    return(list(
      fachabteilung = leistungsangebot_fachabteilung,
      ambulanz = leistungsangebot_ambulanz
    ))
  }


#' read_qualitaetsberichte_xml_fachabteilungsschluessel
#' Liest für jede Organisationseinheit den Fachabteilungsschlüssel.
#' @param file_path character, Pfad zur XML-Datei
#' @return tibble mit Spalten: IK, Standortnummer, <Schlüsselname>, OrgaEinheit_Nummer, OrgaEinheit_Name

read_qualitaetsberichte_xml_fachabteilungsschluessel <-
  function(file_path) {
    extract_orgaeinheit <-
      function(einzelner_fachabteilungsschluessel) {
        orgaeinheit_node <- xml_parents(einzelner_fachabteilungsschluessel)[2] # go back to <Organisationseinheit-Fachabteilung>

        column_names <- c(
          xml_name(einzelner_fachabteilungsschluessel),
          "OrgaEinheit_Nummer",
          "OrgaEinheit_Name"
        )
        column_values <- c(
          xml_text(einzelner_fachabteilungsschluessel),
          xml_text(xml_find_all(orgaeinheit_node, "Gliederungsnummer")),
          xml_text(xml_find_all(orgaeinheit_node, "Name"))
        )

        named_values <- setNames(column_values, column_names)
        tib <- tibble(!!!named_values)

        return(tib)
      }

    xml_data <- read_xml(file_path)
    mehrere_standorte <- length(xml_children(xml_find_all(
      xml_data,
      "//Krankenhaus/Mehrere_Standorte"
    ))) ==
      2
    kh_path <- ifelse(
      mehrere_standorte,
      "Standortkontaktdaten",
      "Krankenhauskontaktdaten"
    )

    IK <- xml_text(xml_find_all(xml_data, glue::glue("//{kh_path}/IK")))
    Standortnummer <- xml_text(xml_find_all(
      xml_data,
      glue::glue("//Standortnummer")
    ))

    fachabteilungsschluessel <- xml_find_all(
      xml_data,
      "//Fachabteilungsschluessel"
    )
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

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
#' @param file_path character, Pfad zur XML-Datei
#' @return tibble mit Spalten: IK, Standortnummer, <Prozedurname>, OrgaEinheit_Nummer, OrgaEinheit_Name
read_qualitaetsberichte_xml_prozeduren <- function(file_path) {
  xml_data <- read_xml(file_path)
  kh_path <- get_kh_path(xml_data)

  IK <- xml_data %>% xml_find_all(glue::glue("//{kh_path}/IK")) %>% xml_text()
  Standortnummer <- xml_data %>% xml_find_all("//Standortnummer") %>% xml_text()

  # Alle <Prozedur>-Nodes finden
  prozeduren <- xml_find_all(xml_data, "//Prozedur")
  if (length(prozeduren) == 0) {
    # Falls keine Prozeduren: leeres tibble zurückgeben
    return(tibble(
      IK = IK,
      Standortnummer = Standortnummer
      # keine weiteren Spalten
    ))
  }

  # Für jede Prozedur ein tibble erzeugen
  extract_orgaeinheit <- function(node_proz) {
    # Wir steigen 4 Ebenen auf zur <Organisationseinheit-Fachabteilung>
    orga_node <- xml_parents(node_proz)[[4]]
    spalten_name <- xml_name(node_proz)
    spalten_wert <- xml_text(node_proz)
    orga_nummer <- xml_find_first(orga_node, "Gliederungsnummer") %>% xml_text()
    orga_name <- xml_find_first(orga_node, "Name") %>% xml_text()

    tibble(
      !!spalten_name := spalten_wert,
      OrgaEinheit_Nummer = orga_nummer,
      OrgaEinheit_Name = orga_name
    )
  }

  # Liste von tibbles: für jede Prozedur
  tmp_list <- map(prozeduren, extract_orgaeinheit)

  # Bind rows (Prozeduren) und dann bind_cols für IK/Standortnummer
  bind_rows(tmp_list) %>%
    mutate(
      IK = IK,
      Standortnummer = Standortnummer
    ) %>%
    select(IK, Standortnummer, everything())
}

#' read_qualitaetsberichte_xml_diagnosen
#' Liest für jede Organisationseinheit die Fallzahlen der Hauptdiagnosen.
#' @param file_path character, Pfad zur XML-Datei
#' @return tibble mit Spalten: IK, Standortnummer, <Diagnosecode>, OrgaEinheit_Nummer, OrgaEinheit_Name
read_qualitaetsberichte_xml_diagnosen <- function(file_path) {
  xml_data <- read_xml(file_path)
  kh_path <- get_kh_path(xml_data)

  IK <- xml_data %>% xml_find_all(glue::glue("//{kh_path}/IK")) %>% xml_text()
  Standortnummer <- xml_data %>% xml_find_all("//Standortnummer") %>% xml_text()

  # Alle <Hauptdiagnose>-Nodes finden
  diagnosen <- xml_find_all(xml_data, "//Hauptdiagnose")
  if (length(diagnosen) == 0) {
    return(tibble(
      IK = IK,
      Standortnummer = Standortnummer
    ))
  }

  extract_orgaeinheit <- function(node_diag) {
    orga_node <- xml_parents(node_diag)[[3]]
    spalten_name <- xml_name(node_diag)
    spalten_wert <- xml_text(node_diag)
    orga_nummer <- xml_find_first(orga_node, "Gliederungsnummer") %>% xml_text()
    orga_name <- xml_find_first(orga_node, "Name") %>% xml_text()

    tibble(
      !!spalten_name := spalten_wert,
      OrgaEinheit_Nummer = orga_nummer,
      OrgaEinheit_Name = orga_name
    )
  }

  tmp_list <- map(diagnosen, extract_orgaeinheit)

  bind_rows(tmp_list) %>%
    mutate(
      IK = IK,
      Standortnummer = Standortnummer
    ) %>%
    select(IK, Standortnummer, everything())
}

#' read_qualitaetsberichte_xml_medizinisches_leistungsangebot
#' Liest für jede Organisationseinheit das medizinische Leistungsangebot.
#' @param file_path character, Pfad zur XML-Datei
#' @return tibble mit Spalten: IK, Standortnummer, <Angebotsname>, OrgaEinheit_Nummer, OrgaEinheit_Name
read_qualitaetsberichte_xml_medizinisches_leistungsangebot <- function(
  file_path
) {
  xml_data <- read_xml(file_path)
  kh_path <- get_kh_path(xml_data)

  IK <- xml_data %>% xml_find_all(glue::glue("//{kh_path}/IK")) %>% xml_text()
  Standortnummer <- xml_data %>% xml_find_all("//Standortnummer") %>% xml_text()

  angebote <- xml_find_all(xml_data, "//Medizinisches_Leistungsangebot")
  if (length(angebote) == 0) {
    return(tibble(
      IK = IK,
      Standortnummer = Standortnummer
    ))
  }

  extract_orgaeinheit <- function(node_angebot) {
    orga_node <- xml_parents(node_angebot)[[3]]
    spalten_name <- xml_name(node_angebot)
    spalten_wert <- xml_text(node_angebot)
    orga_nummer <- xml_find_first(orga_node, "Gliederungsnummer") %>% xml_text()
    orga_name <- xml_find_first(orga_node, "Name") %>% xml_text()

    tibble(
      !!spalten_name := spalten_wert,
      OrgaEinheit_Nummer = orga_nummer,
      OrgaEinheit_Name = orga_name
    )
  }

  tmp_list <- map(angebote, extract_orgaeinheit)

  bind_rows(tmp_list) %>%
    mutate(
      IK = IK,
      Standortnummer = Standortnummer
    ) %>%
    select(IK, Standortnummer, everything())
}

#' read_qualitaetsberichte_xml_fachabteilungsschluessel
#' Liest für jede Organisationseinheit den Fachabteilungsschlüssel.
#' @param file_path character, Pfad zur XML-Datei
#' @return tibble mit Spalten: IK, Standortnummer, <Schlüsselname>, OrgaEinheit_Nummer, OrgaEinheit_Name
read_qualitaetsberichte_xml_fachabteilungsschluessel <- function(file_path) {
  xml_data <- read_xml(file_path)
  kh_path <- get_kh_path(xml_data)

  IK <- xml_data %>% xml_find_all(glue::glue("//{kh_path}/IK")) %>% xml_text()
  Standortnummer <- xml_data %>% xml_find_all("//Standortnummer") %>% xml_text()

  faecher <- xml_find_all(xml_data, "//Fachabteilungsschluessel")
  if (length(faecher) == 0) {
    return(tibble(
      IK = IK,
      Standortnummer = Standortnummer
    ))
  }

  extract_orgaeinheit <- function(node_fach) {
    orga_node <- xml_parents(node_fach)[[2]]
    spalten_name <- xml_name(node_fach)
    spalten_wert <- xml_text(node_fach)
    orga_nummer <- xml_find_first(orga_node, "Gliederungsnummer") %>% xml_text()
    orga_name <- xml_find_first(orga_node, "Name") %>% xml_text()

    tibble(
      !!spalten_name := spalten_wert,
      OrgaEinheit_Nummer = orga_nummer,
      OrgaEinheit_Name = orga_name
    )
  }

  tmp_list <- map(faecher, extract_orgaeinheit)

  bind_rows(tmp_list) %>%
    mutate(
      IK = IK,
      Standortnummer = Standortnummer
    ) %>%
    select(IK, Standortnummer, everything())
}

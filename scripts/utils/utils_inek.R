# Liest das InEK-Standortverzeichnis, XML-Schema 1.2.0 und 2.1.0.
# Beide Schemata ergeben dieselben Spalten (Namen wie bisher). Felder, die
# nur ein Schema hat, sind beim anderen leer (NA): z. B. PSY, DRG,
# Fachabteilungen nur in 2.1.0, Gültigkeit von AbrechnungsIK und
# Betriebsstättennummer nur in 1.2.0.
read_Krankenhausverzeichnis <-
  function(Verzeichnisdatum,
           Dateipfad) {
    # Hilfsfunktionen (neu, für das Einlesen per XPath) --------------------

    # Text des ersten Treffers je Knoten, leer -> NA
    xml_wert <- function(knoten, pfad) {
      na_if(xml_text(xml_find_first(knoten, pfad)), "")
    }

    # Alle Treffer je Knoten, mit ", " verbunden (z. B. Fachabteilungen)
    xml_werte <- function(knoten, pfad) {
      x <- xml_find_all(knoten, pfad, flatten = FALSE) |>
        map_chr(\(treffer) paste(xml_text(treffer), collapse = ", "))
      na_if(x, "")
    }

    # HauptIK, AbrechnungsIK, Betriebsstättennummer: in 1.2.0 Block mit
    # Gültigkeit (bei mehreren der noch gültige, sonst der letzte), in 2.1.0
    # einfacher Wert
    xml_block_oder_wert <- function(knoten, element, feld) {
      coalesce(
        xml_wert(knoten, sprintf("%s[not(GültigBis)]/%s", element, feld)),
        xml_wert(knoten, sprintf("%s[last()]/%s", element, feld)),
        xml_wert(knoten, sprintf("%s[not(*)]", element))
      )
    }

    # GeoAdresse gibt es beim Standort und bei der Einrichtung
    xml_adresse <- function(knoten, element) {
      felder <- c(
        "GeoreferenzZone", "GeoreferenzOst", "GeoreferenzNord", "Längengrad",
        "Breitengrad", "PLZ", "Ort", "Straße", "Hausnummer", "AdressZusatz"
      )
      map(felder, \(f) xml_wert(knoten, paste0(element, "/", f))) |>
        set_names(paste0(element, "_", felder)) |>
        as_tibble()
    }

    # Datei ---------------------------------------------------------------

    # neu: Dateiname darf ein Suffix haben (z. B. "..._aktuell_2.xml")
    Datei <- list.files(
      Dateipfad,
      pattern = paste0("^", str_remove_all(Verzeichnisdatum, "-"), "_Verzeichnisabruf_aktuell.*\\.xml$"),
      full.names = TRUE
    )
    if (length(Datei) != 1) {
      stop("Keine oder mehrere Verzeichnisdateien zum Datum ", Verzeichnisdatum, " in ", Dateipfad)
    }

    # neu: XPath statt as_list()/unnest(); das klappt für beide Schemata.
    # Namespace vor dem Parsen entfernen, damit Pfade ohne Präfix gehen
    # (xml_ns_strip() braucht für die ganze Datei Minuten)
    xml_roh <- readChar(Datei, file.size(Datei), useBytes = TRUE)
    doc <- read_xml(charToRaw(sub(' xmlns="[^"]*"', "", xml_roh, useBytes = TRUE)))
    message(
      "InEK-Standortverzeichnis, XML-Schema ",
      xml_wert(doc, "/Standortverzeichnis/Dateikopf/XmlSchemaVersion")
    )

    # Krankenhäuser -------------------------------------------------------

    kh <- xml_find_all(doc, "/Standortverzeichnis/Krankenhaus")
    Krankenhaeuser <- tibble(
      Version = xml_wert(kh, "Version"),
      LetzteÄnderung = xml_wert(kh, "LetzteÄnderung"),
      HauptIK = xml_block_oder_wert(kh, "HauptIK", "IK"),
      NachfolgeIK = xml_wert(kh, "NachfolgeIK"), # neu, nur 2.1.0
      Bezeichnung = xml_wert(kh, "Bezeichnung"),
      Ermächtigungsgrundlage = xml_wert(kh, "Ermächtigungsgrundlage"),
      Träger = xml_wert(kh, "Träger"),
      Trägerart = xml_wert(kh, "Trägerart"),
      Rechtsform = xml_wert(kh, "Rechtsform"),
      SitzGesellschaft = xml_wert(kh, "SitzGesellschaft"),
      Registriergericht = xml_wert(kh, "Registriergericht | Registergericht"), # 2.1.0: Registergericht
      Registriernummer = xml_wert(kh, "Registriernummer | Registernummer"), # 2.1.0: Registernummer
      GültigVon = xml_wert(kh, "GültigVon"),
      GültigBis = xml_wert(kh, "GültigBis")
    ) |>
      mutate(across(c(LetzteÄnderung, GültigVon, GültigBis), as.Date))

    # Standorte -----------------------------------------------------------

    # eine Zeile je Einrichtung, dazu die Felder ihres Standorts
    ei <- xml_find_all(doc, "/Standortverzeichnis/Standort/Einrichtung")
    st <- xml_find_first(ei, "..")
    Standorte <- bind_cols(
      tibble(
        Version = xml_wert(st, "Version"),
        GültigVon = xml_wert(st, "GültigVon"),
        LetzteÄnderung = xml_wert(st, "LetzteÄnderung"),
        Bezeichnung = xml_wert(st, "Bezeichnung"),
        GültigBis = xml_wert(st, "GültigBis"),
        ReferenzKrankenhaus_IK = xml_wert(st, "ReferenzKrankenhaus/IK"),
        ReferenzKrankenhaus_Version = xml_wert(st, "ReferenzKrankenhaus/Version"),
        StandortId = xml_wert(st, "StandortId")
      ),
      xml_adresse(st, "GeoAdresse"),
      tibble(
        Gemeindeschlüssel = xml_wert(st, "Gemeindeschlüssel"),
        KVBezirk = xml_wert(st, "KVBezirk"),
        Bundesland = xml_wert(st, "Bundesland"),
        Adresstyp = xml_wert(st, "Adresstyp"),
        PostAdresse_PLZ = xml_wert(st, "PostAdresse/PLZ"),
        PostAdresse_Ort = xml_wert(st, "PostAdresse/Ort"),
        PostAdresse_StraßePostfach = xml_wert(st, "PostAdresse/StraßePostfach"),
        PostAdresse_Hausnummer = xml_wert(st, "PostAdresse/Hausnummer"),
        Aktiv = xml_wert(st, "Aktiv"), # neu, nur 2.1.0
        Zentren = xml_werte(st, "Zentren/ZentrenArt"), # neu, nur 2.1.0
        Einrichtung_Einrichtungstyp = xml_wert(ei, "Einrichtungstyp"),
        Einrichtung_Standortnummer = xml_wert(ei, "Standortnummer"),
        Einrichtung_GültigVon = xml_wert(ei, "GültigVon"),
        Einrichtung_Name = xml_wert(ei, "Name")
      ),
      xml_adresse(ei, "GeoAdresse") |> rename_with(\(n) paste0("Einrichtung_", n)),
      tibble(
        Einrichtung_AbrechnungsIK_IK = xml_block_oder_wert(ei, "AbrechnungsIK", "IK"),
        Einrichtung_AbrechnungsIK_GültigVon = xml_wert(ei, "AbrechnungsIK[last()]/GültigVon"), # nur 1.2.0
        Einrichtung_AbrechnungsIK_GültigBis = xml_wert(ei, "AbrechnungsIK[last()]/GültigBis"), # nur 1.2.0
        Einrichtung_Betriebsstättennummer_BSNR = xml_block_oder_wert(ei, "Betriebsstättennummer", "BSNR"),
        Einrichtung_Betriebsstättennummer_GültigVon = xml_wert(ei, "Betriebsstättennummer[last()]/GültigVon"), # nur 1.2.0
        Einrichtung_Betriebsstättennummer_GültigBis = xml_wert(ei, "Betriebsstättennummer[last()]/GültigBis"), # nur 1.2.0
        Einrichtung_GültigBis = xml_wert(ei, "GültigBis"),
        Einrichtung_Aktiv = xml_wert(ei, "Aktiv"), # neu, nur 2.1.0
        Einrichtung_PSY = xml_wert(ei, "PSY"), # neu, nur 2.1.0
        Einrichtung_DRG = xml_wert(ei, "DRG"), # neu, nur 2.1.0
        Einrichtung_Fachabteilungen = xml_werte(ei, "Fachabteilungen/Fachabteilungsschlüssel") # neu, nur 2.1.0
      )
    ) |>
      mutate(
        across(c(contains("GültigVon"), contains("GültigBis"), LetzteÄnderung), as.Date),
        across(c(ends_with("Längengrad"), ends_with("Breitengrad")), as.numeric)
      ) |>
      filter(Einrichtung_Einrichtungstyp == "00") |> # Standorte, keine Ambulanzen etc.
      mutate(
        gueltiger_eintrag = any(
          is.na(Einrichtung_GültigBis) | Einrichtung_GültigBis > today()
        ),
        .by = "Einrichtung_Standortnummer"
      ) |> # gueltiger Eintrag vorhanden?
      filter(
        !gueltiger_eintrag |
          is.na(Einrichtung_GültigBis) |
          Einrichtung_GültigBis > today()
      ) |> # wenn gueltige Eintraege vorhanden sind, dann nur diese filtern
      filter(Version == max(Version), .by = "Einrichtung_Standortnummer") |> # filter alte Versionen raus
      distinct(Einrichtung_Standortnummer, .keep_all = TRUE) |>  # zum Teil Versionsnummern mehrfach vergeben
      select(-gueltiger_eintrag)
    
    return(list(Krankenhaeuser = Krankenhaeuser, Standorte = Standorte))
  }

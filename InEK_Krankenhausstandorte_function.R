read_Krankenhausverzeichnis <-
  function(Verzeichnisdatum,
           Dateipfad) {
    Krankenhausverzeichnis_aktuell <-
      read_xml(file.path(
        Dateipfad,
        paste0(
          str_remove_all(Verzeichnisdatum, "-"),
          "_Verzeichnisabruf_aktuell.xml"
        )
      )) |>
      as_list() |>
      as_tibble() |>
      filter(row_number() != 1) |>
      mutate(N = row_number()) |>
      unnest_longer(Standortverzeichnis) |>
      pivot_wider(names_from = Standortverzeichnis_id,
                  values_from = Standortverzeichnis,
                  values_fn = list)
    
    
    Krankenhaeuser <-
      Krankenhausverzeichnis_aktuell |>
      select(-Einrichtung) |>
      unnest(c(HauptIK, ReferenzKrankenhaus, GeoAdresse, PostAdresse)) |>
      unnest_wider(c(HauptIK, ReferenzKrankenhaus, GeoAdresse, PostAdresse),
                   names_sep = "_") |>
      unnest(everything()) |>
      unnest(everything()) |>
      unnest(everything()) |>
      filter(is.na(ReferenzKrankenhaus_IK)) |>
      select(
        Version,
        LetzteÄnderung,
        HauptIK = HauptIK_IK,
        Bezeichnung,
        Ermächtigungsgrundlage,
        Träger,
        Trägerart,
        Rechtsform,
        SitzGesellschaft,
        Registriergericht,
        Registriernummer,
        GültigVon,
        GültigBis
      ) |>
      mutate(
        GültigVon = as.Date(GültigVon),
        LetzteÄnderung = as.Date(LetzteÄnderung),
        GültigBis = as.Date(GültigBis)
      )
    
    Standorte <-
      Krankenhausverzeichnis_aktuell |>
      unnest_longer(Einrichtung) |>
      unnest(c(HauptIK, ReferenzKrankenhaus, GeoAdresse, PostAdresse)) |>
      unnest_wider(c(
        HauptIK,
        ReferenzKrankenhaus,
        GeoAdresse,
        PostAdresse,
        Einrichtung
      ),
      names_sep = "_") |>
      unnest_wider(
        c(
          Einrichtung_GeoAdresse,
          Einrichtung_AbrechnungsIK,
          Einrichtung_Betriebsstättennummer,
          Einrichtung_GeoAdresse,
          Einrichtung_AbrechnungsIK,
          Einrichtung_Betriebsstättennummer
        ),
        names_sep = "_"
      ) |>
      unnest(everything()) |>
      unnest(everything()) |>
      unnest(everything()) |>
      select(
        -c(
          HauptIK_1,
          Ermächtigungsgrundlage,
          Träger,
          Trägerart,
          Rechtsform,
          SitzGesellschaft,
          Registriergericht,
          Registriernummer
        )
      ) |>
      mutate_at(
        .vars = c(
          "GültigVon",
          "LetzteÄnderung",
          "GültigBis",
          "Einrichtung_GültigVon",
          "Einrichtung_GültigBis",
          "Einrichtung_AbrechnungsIK_GültigVon",
          "Einrichtung_AbrechnungsIK_GültigBis",
          "Einrichtung_Betriebsstättennummer_GültigVon",
          "Einrichtung_Betriebsstättennummer_GültigBis"
        ),
        .funs = as.Date
      ) |>
      mutate_at(
        .vars = c(
          "GeoAdresse_Längengrad",
          "GeoAdresse_Breitengrad",
          "Einrichtung_GeoAdresse_Längengrad",
          "Einrichtung_GeoAdresse_Breitengrad"
        ),
        .funs = as.numeric
      ) |>
      select(-N, -Einrichtung_id) |>
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

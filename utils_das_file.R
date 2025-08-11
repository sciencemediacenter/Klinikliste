extract_html_element <-
  function(x, Element) {
    x <- lapply(x, function(x) {
      html_elements(x, Element) |> html_text()
    })
    x[lengths(x) == 0] <- NA
    return(x)
  }

extract_nested_html_element <-
  function(x, Element) {
    x <- lapply(x, function(x) {
      html_elements(x, Element) |> html_text()
    })
    x[lengths(x) == 0] <- NA
    return(x)
  }

extract_nested_html_element <- function(x, Element) {
  x <- lapply(x, function(x) {
    # Get the main element
    element_node <- html_elements(x, Element)

    if (length(element_node) == 0) {
      return(NA)
    }

    # Check if this is a simple node or has children
    child_nodes <- xml_children(element_node)

    if (length(child_nodes) == 0) {
      # If no children, check if there's text directly in the element
      element_text <- html_text(element_node)
      if (length(element_text) > 0 && element_text != "") {
        # Return the text as a named value using the element name
        result <- list()
        result[[Element]] <- element_text
        return(result)
      } else {
        # Empty element with no children
        return(NA)
      }
    } else {
      # Complex element with children - extract all children
      result <- list()

      # Get all child node names
      child_names <- xml_name(child_nodes)

      # Extract text from each child node
      for (i in seq_along(child_nodes)) {
        child_text <- html_text(child_nodes[i])
        # Handle empty elements
        if (length(child_text) == 0 || child_text == "") {
          child_text <- NA
        }

        # Use just the child name without the parent prefix
        result[[child_names[i]]] <- child_text
      }

      return(result)
    }
  })

  # Handle cases where the element wasn't found
  x[lengths(x) == 0] <- NA

  # Make sure all entries have the same structure
  # Find all unique keys across all list elements
  all_keys <- unique(unlist(lapply(x, function(item) {
    if (is.list(item)) names(item) else NULL
  })))

  # Ensure each list element has all keys
  x <- lapply(x, function(item) {
    if (is.list(item)) {
      # Add missing keys with NA values
      missing_keys <- setdiff(all_keys, names(item))
      for (key in missing_keys) {
        item[[key]] <- NA
      }
      return(item)
    } else {
      # For non-list items (like NA), create a list with all keys set to NA
      result <- as.list(rep(NA, length(all_keys)))
      names(result) <- all_keys
      return(result)
    }
  })

  return(x)
}

read_qualitaetsberichte_das_dokumentationsraten <-
  function(file_path) {
    # Read XML file
    neue_Dokdaten_xml <- read_xml(file_path)

    # Extract IK and Standortnummer
    IK_temp <- html_element(neue_Dokdaten_xml, "IK") |> html_text()
    Standortnummer_temp <- html_element(neue_Dokdaten_xml, "Standortnummer") |>
      html_text()

    # Extract Leistungsbereich elements
    Leistungsbereich <- html_elements(
      neue_Dokdaten_xml,
      xpath = ".//Leistungsbereich_DeQS"
    )

    # Convert to tibble
    Leistungsbereich <- bind_rows(lapply(
      Leistungsbereich,
      function(x) tibble(Leistungsbereich = list(x))
    ))

    # If no Leistungsbereiche found, return empty tibble
    if (nrow(Leistungsbereich) == 0) {
      return(tibble())
    }

    # Create data frame with all elements
    table_Dokdaten <- Leistungsbereich |>
      mutate(
        IK = IK_temp,
        Standortnummer = Standortnummer_temp,
        Kuerzel = extract_html_element(Leistungsbereich, "Kuerzel"),
        Bezeichnung = extract_html_element(Leistungsbereich, "Bezeichnung"),
        Fallzahl = extract_html_element(Leistungsbereich, "Fallzahl"),
        Dokumentationsrate = extract_html_element(
          Leistungsbereich,
          "Dokumentationsrate"
        ),
        Anzahl_Datensaetze_Standort = extract_html_element(
          Leistungsbereich,
          "Anzahl_Datensaetze_Standort"
        )
      )

    return(table_Dokdaten)
  }

unnest_and_convert_qualitaetsberichte_das_dokumentationsraten <-
  function(qualitaetsberichte_das_dokumentationsraten_raw) {
    # Post-processing: Unnest and convert data types
    qualitaetsberichte_das_dokumentationsraten <- qualitaetsberichte_das_dokumentationsraten_raw |>
      select(-Leistungsbereich) |>
      unnest(
        cols = c(
          Kuerzel,
          Bezeichnung,
          Fallzahl,
          Dokumentationsrate,
          Anzahl_Datensaetze_Standort
        )
      ) |>
      # Convert values to proper types
      mutate(
        Fallzahl = as.integer(Fallzahl),
        Dokumentationsrate = parse_number(
          Dokumentationsrate,
          locale = locale(decimal_mark = ","),
          trim_ws = TRUE
        ),
        Anzahl_Datensaetze_Standort = as.integer(Anzahl_Datensaetze_Standort)
      )

    return(qualitaetsberichte_das_dokumentationsraten)
  }


read_qualitaetsberichte_das_ergebnis <-
  function(file_path) {
    # Read XML file
    neue_Dokdaten_xml <- read_xml(file_path)

    # Extract IK and Standortnummer
    IK_temp <- html_element(neue_Dokdaten_xml, "IK") |> html_text()
    Standortnummer_temp <- html_element(neue_Dokdaten_xml, "Standortnummer") |>
      html_text()

    # Extract Leistungsbereich elements
    Ergebnis <- html_elements(
      neue_Dokdaten_xml,
      xpath = ".//QS-Ergebnis"
    )

    # Convert to tibble
    Ergebnis <- bind_rows(lapply(
      Ergebnis,
      function(x) tibble(Ergebnis = list(x))
    ))

    # If no Leistungsbereiche found, return empty tibble
    if (nrow(Ergebnis) == 0) {
      return(tibble())
    }

    # Create data frame with all elements
    table_Ergebnisse <- Ergebnis |>
      mutate(
        IK = IK_temp,
        Standortnummer = Standortnummer_temp,
        Kuerzel_Leistungsbereich = extract_html_element(
          Ergebnis,
          "Kuerzel_Leistungsbereich"
        ),
        Bezeichnung_Leistungsbereich = extract_html_element(
          Ergebnis,
          "Bezeichnung_Leistungsbereich"
        ),
        Ergebnis_ID = extract_html_element(Ergebnis, "Ergebnis_ID"),
        Bezeichnung_Ergebnis = extract_html_element(
          Ergebnis,
          "Bezeichnung_Ergebnis"
        ),
        Art_des_Wertes = extract_html_element(Ergebnis, "Art_des_Wertes"),
        Bezug_zum_Verfahren = extract_html_element(
          Ergebnis,
          "Bezug_zum_Verfahren"
        ),
        Fachlicher_Hinweis_IQTIG = extract_html_element(
          Ergebnis,
          "Fachlicher_Hinweis_IQTIG"
        ),
        Einheit = extract_html_element(Ergebnis, "Einheit"),
        Bundesergebnis = extract_html_element(Ergebnis, "Bundesergebnis"),
        Vertrauensbereich_Bundesweit = extract_nested_html_element(
          Ergebnis,
          "Vertrauensbereich_Bundesweit"
        ),
        Rechnerisches_Ergebnis = extract_html_element(
          Ergebnis,
          "Rechnerisches_Ergebnis"
        ),
        Vertrauensbereich_Krankenhaus = extract_nested_html_element(
          Ergebnis,
          "Vertrauensbereich_Krankenhaus"
        ),
        Fallzahl = extract_nested_html_element(
          Ergebnis,
          "Fallzahl"
        ),
        Ergebnis_Bewertung = extract_nested_html_element(
          Ergebnis,
          "Ergebnis_Bewertung"
        )
      )
    return(table_Ergebnisse)
  }


unnest_and_convert_qualitaetsberichte_das_ergebnis <-
  function(qualitaetsberichte_das_ergebnis_raw) {
    # Post-processing: Unnest and convert data types
    qualitaetsberichte_das_ergebnis <- qualitaetsberichte_das_ergebnis_raw |>
      select(-Ergebnis) |>
      unnest(
        cols = c(
          Kuerzel_Leistungsbereich,
          Bezeichnung_Leistungsbereich,
          Ergebnis_ID,
          Bezeichnung_Ergebnis,
          Art_des_Wertes,
          Bezug_zum_Verfahren,
          Fachlicher_Hinweis_IQTIG,
          Einheit,
          Bundesergebnis,
          Rechnerisches_Ergebnis
        )
      ) |>
      # Unnest the nested data columns
      unnest_wider(Vertrauensbereich_Bundesweit, names_sep = "_") |>
      unnest_wider(Vertrauensbereich_Krankenhaus, names_sep = "_") |>
      unnest_wider(Fallzahl, names_sep = "_") |>
      unnest_wider(Ergebnis_Bewertung, names_sep = "_") |>
      # Convert values to proper types
      mutate(
        across(
          c(
            Fallzahl_Grundgesamtheit,
            Fallzahl_Beobachtete_Ereignisse
          ),
          as.integer
        )
      ) |>
      mutate(
        across(
          c(
            Fallzahl_Erwartete_Ereignisse,
            Bundesergebnis,
            Vertrauensbereich_Bundesweit_Vertrauensbereich_Untere_Grenze,
            Vertrauensbereich_Bundesweit_Vertrauensbereich_Obere_Grenze,
            Rechnerisches_Ergebnis,
            Vertrauensbereich_Krankenhaus_Vertrauensbereich_Untere_Grenze,
            Vertrauensbereich_Krankenhaus_Vertrauensbereich_Obere_Grenze
          ),
          ~ parse_number(
            .,
            locale = locale(decimal_mark = ","),
            trim_ws = TRUE
          )
        )
      )

    return(qualitaetsberichte_das_ergebnis)
  }

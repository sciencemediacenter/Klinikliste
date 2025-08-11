#' get_qb_xml_files
#' Erzeugt einen Vektor mit den Dateipfaden der Qualitätsberichte (XML-Dateien).
#' @param file_path character, Verzeichnis, in dem gesucht wird
#' @param pattern    character, Regex-Pattern (z.B. "\\.xml$")
#' @return character-Vektor
get_qb_xml_files <- function(file_path, pattern = "\\.xml$") {
  list.files(file_path, pattern = pattern, full.names = TRUE)
}

#' Qualitaetsdaten
#'
#' @format Enthält ausgewählte Daten der deutschen Krankenhausstandorte
#' aus den XML Qualitätsberichten des Gemeinsamen Bundesausschusses.
#' @note Die Adressdaten beziehen sich auf den Zugang zum Krankenhaus
#' und müssen nicht mit der Geschäftsadresse übereinstimmen.
#'
#' @docType data
#' @usage data(Qualitaetsdaten)
#' @encoding UTF-8
#' #' @md
#' @format Dataframe mit 14 Variablen:
#' \describe{
#'   \item{\code{Ik}}{IK Nummer}
#'   \item{\code{Name}}{Name des Krankenhausstandorts}
#'   \item{\code{Strasse}}{Strasse des Krankenhauszugangs}
#'   \item{\code{Hausnummer}}{Hausnummer des Krankenhauszugangs}
#'   \item{\code{Plz}}{Postleitzahl des Krankenhauszugangs}
#'   \item{\code{Ort}}{Ort des Krankenhauszugangs}
#'   \item{\code{Standortnummer}}{Die 9-stellige Standortnummer. Diese besteht aus der StandortId, gefolgt von einer 0 und den zwei Ziffern des Einrichtungstyps}
#'   \item{\code{Betten}}{Anzahl der Krankenhausbetten}
#'   \item{\code{Notfallstufe}}{Teilnahme Notfallstufe, 0 = Keine Teilnahme}
#'   \item{\code{Kommentar_notfallstufe}}{Eventuell vorhandene Erläuterungen zur Teilnahme Notfallstufe}
#'   \item{\code{Schwerverletztenversorgung}}{Teilnahme am Modul Schwerverletztenversorgung }
#'   \item{\code{Stroke_unit}}{Teilnahme am Modul Schlaganfallversorgung (Stroke Unit)}
#'   \item{\code{Chest_pain_unit}}{Teilnahme am Modul Durchblutungsstörungen am Herzen (Chest Pain Unit)}
#'   \item{\code{Notfallversorgung_kinder}}{Teilnahme am Modul Notfallversorgung Kinder (Basis, erweitert, umfassend)}
#' }
#' @references
#' https://www.g-ba.de/downloads/62-492-3380/Qb-R_2023-12-21_iK-2024-02-17.pdf

# Klinikliste Deutschland

Dieses Repo enthält Skripte und Daten rund um die Kliniken in Deutschland. Ziel ist es eine gemeinsame Datenbasis für eine Datenberichterstattung im Bereich Krankenhauslandschaft in Deutschland aufzubauen. 

Das Projekt soll kooperativ sein. Ihr seid herzlich eingeladen dazu beizutragen, indem ihr z.B. die Codebasis erweitert, Daten validiert/aktualisiert oder neue Daten aus eigenen Recherchen der Sammlung hinzufügt. Bei Fragen wendet euch gerne an lab@sciencemediacenter.de

* Bei der Nutzung der Daten ist auf die unterschiedlichen Datenquellen zu achten. 
* Im Repo enthaltene Daten sind nicht zwingend aktuell. Eine lokale Aktualisierung der Daten kann meist über die beigefügten Skripte geschehen. Die Dokumentation der Datenquellen ist weiter unten zu finden.
* Die verwendeten Daten sind ursprünglich zu unterschiedlichen Zwecken erhoben worden und liegen in unterschiedlicher Aktualität vor, was die Interpretierbarkeit beeinflussen kann.
* Datenfehler können als Issue gemeldet werden.


## Repository Struktur

```
Klinikliste/
├── config.R                    # Zentrale Konfiguration (Jahr, Pfade)
├── README.md                   # Diese Dokumentation
│
├── scripts/                    # Alle R-Skripte
│   ├── 01_preprocess_qualitaetsberichte.R   # Qualitätsberichte verarbeiten
│   ├── 02_preprocess_das_files.R            # DAS-Dateien verarbeiten
│   ├── 03_preprocess_inek_standorte.R       # InEK Standortliste verarbeiten
│   ├── 04_anreicherung_qualitaetsberichte.R # Daten zusammenführen
│   ├── 99_extract_single_hospital.R         # Einzelnes Krankenhaus abfragen
│   │
│   └── utils/                  # Hilfsfunktionen
│       ├── utils_xml.R         # XML-Verarbeitung
│       ├── utils_das.R         # DAS-Datei-Verarbeitung
│       ├── utils_inek.R        # InEK-Daten-Verarbeitung
│       └── utils_extract_single_hospital.R  # Funktionen für Einzelabfrage
│
├── data/                       # Verarbeitete Daten (Output)
│   ├── Qualitaetsberichte/     # Qualitätsberichte RData
│   └── Standortliste_InEK/     # InEK Daten RData
│
├── data-raw/                   # Rohdaten (nicht im Git)
│   ├── README.md               # Anleitung zum Datendownload
│   ├── Qualitaetsberichte/     # XML-Dateien der Qualitätsberichte
│   └── Standortliste_InEK/     # InEK XML-Dateien
│
└── docs/                       # Dokumentation
    └── Qualitaetsdaten.R       # Datenfeld-Beschreibungen
```

## Quick Start

1. **Rohdaten herunterladen** - siehe `data-raw/README.md` für Anleitungen
2. **Konfiguration anpassen** - Jahr in `config.R` setzen
3. **Skripte ausführen** - in numerischer Reihenfolge:
   ```r
   source("scripts/01_preprocess_qualitaetsberichte.R")
   source("scripts/02_preprocess_das_files.R")
   source("scripts/03_preprocess_inek_standorte.R")
   source("scripts/04_anreicherung_qualitaetsberichte.R")
   ```

## Einzelnes Krankenhaus abfragen

Das Skript `scripts/99_extract_single_hospital.R` ermöglicht die schnelle Abfrage einzelner Krankenhäuser, ohne alle Daten verarbeiten zu müssen. Siehe das Skript für Beispiele und verfügbare Datentypen.


## Datenquellen

### InEK Standortverzeichnis

Kern der Datensammlung ist das [Standortverzeichnis](https://krankenhausstandorte.de/) des Instituts für das Entgeltsystem im Krankenhaus GmbH (InEK). Nach einer Registrierung können die Daten als XML-file heruntergeladen werden. Dabei kann das vollständige oder nur das aktuelle Verzeichnis heruntergeladen werden. Das Skript `scripts/03_preprocess_inek_standorte.R` ermöglicht das Einlesen der Daten und das Umformatieren in Rechteckdaten.

**Hinweise zu den Daten:**
* Die Daten werden im Standortverzeichnis wöchentlich aktualisiert.
* Das Verzeichnis enthält eine Liste mit Krankenhäusern (mit mehreren Standorten).
* Das Verzeichnis enthält pro Standort eine oder mehrere Einrichtungen. Werden nur die Standorte benötigt, muss nach `Einrichtung_Einrichtungstyp == "00"` gefiltert werden.
* Die übrigen Einrichtungen enthalten Ambulanzen und Tageskliniken.
* Auch der aktuelle Datensatz enthält bereits nicht mehr gültige Einträge. Variablen mit Gültigkeitszeiträumen finden sich im Datensatz auf Standort-Ebene, auf Einrichtungs-Ebene und bei der Betriebstättennummer und müssen bei Bedarf beachtet werden.
* Die Daten enthalten für einige Einrichtungen mehrere Versionen. Nicht immer ist die neueste Version die aktuell gültige.
* Einige Einrichtungen enthalten keinen gültigen Eintrag mehr, obwohl sie noch aktiv sind.
* Die Versionsnummer für die Einrichtungen ist nicht immer unique.
* Geokoordinaten existieren auf Standort- und auf Einrichtungsebene.
* Die 6-stellige StandortId beginnt immer mit 77 und ist für jeden Standort eindeutig.
* Jede Einrichtung hat noch eine 9-stellige Standortnummer (StandortId + 0 + zweistelliger Einrichtungstyp).

Weitere Informationen: [Handbuch des Standortverzeichnisses](https://krankenhausstandorte.de/storage/manual/Handbuch_Standortverzeichnis.pdf)


### Qualitätsberichte des G-BA

Die Daten der Qualitätsberichte können auf den Seiten des [Gemeinsamen Bundesausschuss (G-BA)](https://qb-referenzdatenbank.g-ba.de/#/login) beantragt und heruntergeladen werden. Die Skripte `scripts/01_preprocess_qualitaetsberichte.R` und `scripts/02_preprocess_das_files.R` ermöglichen das Einlesen.

**Extrahierte Daten:**
* Bettenzahl und Notfallstufen
* Prozeduren (Fallzahlen)
* Diagnosen (Fallzahlen)
* Medizinisches Leistungsangebot
* Fachabteilungsschlüssel
* Dokumentationsraten (aus DAS-Dateien)
* QS-Ergebnisse (aus DAS-Dateien)

**Hinweise zu den Daten:**
* Das Berichtsjahr liegt ein bis zwei Jahre in der Vergangenheit - Angaben können veraltet sein.
* Bei wenigen Standorten war aufgrund fehlerhafter Standortnummern eine automatische Zuordnung nicht möglich (händisch korrigiert in `scripts/04_anreicherung_qualitaetsberichte.R`).
* Einige Angaben zur Notfallversorgung erscheinen nicht plausibel (z.B. fehlende Chest Pain Units bei Universitätskliniken).

**Nutzungsbedingungen:** Bei Verwendung sind die [Allgemeinen Nutzungsbedingungen des G-BA](https://qb-datenportal.g-ba.de/assets/ANB_Nutzung_Qualit%C3%A4tsberichte.pdf) zu beachten.

> "Die Qualitätsberichte der Krankenhäuser werden vorliegend in Verbindung mit anderen Erkenntnisquellen genutzt. Die angegebenen Empfehlungen und Ergebnisse stellen daher keine authentische Wiedergabe der Qualitätsberichte dar. Eine vollständige Darstellung der Qualitätsberichte der Krankenhäuser erhalten Sie unter www.g-ba.de/qualitaetsberichte."

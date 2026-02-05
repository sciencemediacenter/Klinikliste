# Raw Data Sources

This folder contains raw data files that are **not tracked in git** due to their size and licensing requirements.

## How to Obtain the Data

### Qualitätsberichte (G-BA)

The quality reports can be requested from the G-BA (Gemeinsamer Bundesausschuss):

1. Register at https://qb-referenzdatenbank.g-ba.de/
2. Request access to the XML data
3. Download the XML files for the desired year(s)
4. Extract and place them in the corresponding folder:
   - `Qualitaetsberichte/xml_2021/`
   - `Qualitaetsberichte/xml_2022/`
   - `Qualitaetsberichte/xml_2023/`

**Note:** When using data from the quality reports, the [G-BA terms of use](https://qb-datenportal.g-ba.de/assets/ANB_Nutzung_Qualit%C3%A4tsberichte.pdf) apply.

### InEK Standortverzeichnis

The hospital location directory is provided by InEK (Institut für das Entgeltsystem im Krankenhaus):

1. Register at https://krankenhausstandorte.de/
2. Download the current directory as XML
3. Place the file in `Standortliste_InEK/` with naming pattern: `YYYYMMDD_Verzeichnisabruf_aktuell.xml`

**Documentation:** See the [Standortverzeichnis Handbook](https://krankenhausstandorte.de/storage/manual/Handbuch_Standortverzeichnis.pdf)

## Folder Structure

```
data-raw/
├── Qualitaetsberichte/
│   ├── xml_2021/          # XML files for 2021
│   ├── xml_2022/          # XML files for 2022
│   └── xml_2023/          # XML files for 2023
└── Standortliste_InEK/
    └── *.xml              # InEK location directory XML
```

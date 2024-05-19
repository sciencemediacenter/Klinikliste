# Klinikliste Deutschland


Dieses Repo enthält Skripte und Daten rund um die Kliniken in Deutschland. Ziel ist es eine gemeinsame Datenbasis für eine Datenberichterstattung im Bereich Krankenhauslandschaft in Deutschland aufzubauen. 

Das Projekt soll kooperativ sein. Ihr seid herzlich eingeladen dazu beizutragen, indem ihr z.B. die Codebasis erweitert, Daten validiert/aktualisiert oder neue Daten aus eigenen Recherchen der Sammlung hinzufügt. Bei Fragen wendet euch gerne an lab@sciencemediacenter.de

* Bei der Nutzung der Daten ist auf die unterschiedlichen Datenquellen zu achten. 
* Im Repo enthaltene Daten sind nicht zwingend aktuell. Eine lokale Aktualisierung der Daten kann meist über die beigefügten Skripte geschehen. Die Dokumentation der Datenquellen ist weiter unten zu finden.
* Die verwendeten Daten sind ursprünglich zu unterschiedlichen Zwecken erhoben worden und liegen in unterschiedlicher Aktualität vor, was die Interpretierbarkeit beeinflussen kann.
* Datenfehler können als Issue gemeldet werden.



### InEK Standortverzeichnis

Kern der Datensammlung ist das [Standortverzeichnis](https://krankenhausstandorte.de/) des Instituts für das Entgeltsystem im Krankenhaus GmbH (InEK). Nach einer Registrierung können die Daten als XML-file heruntergeladen werden. Dabei kann das vollständige oder nur das aktuelle Verzeichnis heruntergeladen werden.  Das R-Skript Preprocessing_InEK_Krankenhausstandorte.R ermöglicht das Einlesen der Daten und das Umformatieren in Rechteckdaten.

* Die Daten werden im Standortverzeichnis wöchentlich aktualisiert.
* das Verzeichnis enthält eine Liste mit Krankenhäusern (mit mehreren Standorten).
* Das Verzeichnis enthält pro Standort eine oder mehrere Einrichtungen. Werden nur die Standorte benötigt, muss nach Einrichtung_Einrichtungstyp == "00" gefiltert werden.
* Die übrigen Einrichtungen enthalten Ambulanzen und Tageskliniken.
* Auch der aktuelle Datensatz enthält bereits nicht mehr gültige Einträge. Variablen mit Gültigkeitszeiträumen finden sich im Datensatz auf Standort-Ebene, auf Einrichtungs-Ebene und bei der Betriebstättennummer und müssen bei Bedarf beachtet werden.
* Geokoordinaten existieren auf Standort- und auf Einrichtungsebene.
* Die 6-stellige StandortId beginnt immer mit 77 und ist für jeden Standort eindeutig.
* Jede Einrichtung hat noch eine 9-stellige Standortnummer. Diese besteht aus der StandortId, gefolgt von einer 0 und den zwei Ziffern des Einrichtungstyps.

Weitere Informationen zu den Daten finden sich im [Handbuch des Standortverzeichnisses](https://krankenhausstandorte.de/storage/manual/Handbuch_Standortverzeichnis.pdf). 


### Qualitätsberichte des G-BA

Die Daten der Qualitätsberichte können auf den Seiten des [Gemeinsamen Bundesausschuss (G-BA)](https://qb-referenzdatenbank.g-ba.de/#/login) beantragt und heruntergeladen werden. Der aktuelle Datenstand sind die Berichte des Jahres 2022. Diese Daten können mit dem Skript Preprocessing_Qualitaetsberichte_2022.R eingelesen werden. Aktuell werden 
die Bettenzahl und die verschiedenen Notfallstufen aus den 
Qualitätsberichten extrahiert, das Skript darf aber gerne erweitert 
werden.

* Da das Berichtsjahr der aktuellen Qualitätsberichte ein bis zwei Jahre in der Vergangenheit liegt, können Angaben veraltet sein und müssen ggf. auf Aktualität geprüft weren.
* Bei einer kleinen einstelligen Zahl an Standorten war aufgrund von fehlenden / fehlerhaften Standortnummern eine automatische Zuordnung nicht möglich. Diese wurden händisch eingepflegt. Die Änderungen können im Preprocessing-Skript eingesehen werden.  
* Einige Angaben für die Teilnahme an der Notfallversorgung erscheinen
nicht plausibel bzw. sind anscheinend nicht vollständig in den
Qualitätsberichten dokumentiert. Bspw. haben mehrfach große
Universitätskliniken keine Chest Pain Unit oder Stroke Unit,
was Angaben verschiedener Zertifizierungsstellen widerspricht.  


Bei einer Verwendung von Daten aus den Qualitätsberichten sind die [Allgemeinen Nutzungsbedingungen des G-BA](https://qb-datenportal.g-ba.de/assets/ANB_Nutzung_Qualit%C3%A4tsberichte.pdf) zu beachten. 

Auch für diese Sammlung gilt:
"Die Qualitätsberichte der Krankenhäuser werden vorliegend in Verbindung mit anderen Erkenntnisquellen genutzt. Die angegebenen Empfehlungen und Ergebnisse stellen daher keine authentische Wiedergabe der Qualitätsberichte dar. Eine vollständige Darstellung der Qualitätsberichte der Krankenhäuser erhalten Sie unter www.g-ba.de/qualitaetsberichte."

# Zusatzmaterial für den Abschlussbericht des Moduls "Spezielle Themen der Fernerkundung
## `Neuauflage...`
- eigene Implementierung des **Mann-Kendall Tests**
- eigene Implementierung der **Berechnung der Steigung nach Theil und Sen**
- funktioniert *out of the box* nicht als eigenständiges Skript, benötigte Pakete werden in "parent scripts" geladen.
## `custom_functions.R`
- Helferfunktionen für Vorprozessierung u. a.
- funktioniert *out of the box* nicht als eigenständiges Skript, benötigte Pakete werden in "parent scripts" geladen.
## `Abbildungen_Ergebnisse_OpenAQ.R` & `Abbildungen_Ergebnisse_OpenAQ2.R`
- Abbildungen und Datenprozessierung respektive einzelner Messstationen und zusammengerechneter Aufzeichnungen
## `API_call.R`
- Skript zum Herunterladen der Daten von OpenAQs API
  - Stationsauswahl fand in früherem Skript statt
## `Trend_Sentinel.R` & `Trend_OpenAQ.R`
- Trendanalyse mit Mann-Kendall Test

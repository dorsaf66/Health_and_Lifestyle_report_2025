# Health_and_Lifestyle_report_2025

Dieses Projekt visualisiert Gesundheits- und Lebensstil-Daten mithilfe von Elm. Ziel ist es, Nutzern zu ermöglichen, Muster und Zusammenhänge in den Daten zu erkennen und zu analysieren.

📊 Visualisierungen

Scatterplot: Vergleich von zwei Attributen zur Identifizierung von Korrelationen.

Parallele Koordinaten: Darstellung mehrerer Attribute zur Analyse von Mustern.

Korrelationsbaum: Visualisierung der Beziehungen zwischen Variablen.

🚀 Installation

Repository klonen:

git clone https://github.com/dorsaf66/Health_and_Lifestyle_report_2025.git
cd Health_and_Lifestyle_report_2025


Elm installieren (falls noch nicht geschehen):

npm install -g elm


Projekt kompilieren:

elm make src/Main.elm --output=main.js


index.html im Browser öffnen.

🔧 Nutzung

Achsenwahl: Über Dropdown-Menüs Attribute auswählen.

Filter: Daten nach Geschlecht oder Beruf kategorisieren.

Interaktionen: Mit Maus über Punkte fahren für Details; Klicken für Hervorhebung.

🧪 Daten

Quelle: Health and Lifestyle Dataset : https://www.kaggle.com/code/jillanisofttech/sleep-health-and-lifestyle-predication-with-94-ac

Format: CSV

Variablen: Alter, BMI, Schlafdauer, Beruf, Geschlecht, etc.

🧠 Features

Dynamische Achsenwahl

Filterung nach Kategorien

Hover- und Klick-Interaktionen

Drag-and-Drop im Korrelationsbaum

🛠️ Git-Historie

Die Entwicklung erfolgte auf dem main-Branch mit separaten Feature-Branches für Scatterplot, Baum und Filter. Jeder Commit dokumentiert die Implementierung und Optimierung der Visualisierungen.

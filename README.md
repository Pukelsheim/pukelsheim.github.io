# Fair gezählt. Fair gewählt.

Website zur Volksinitiative der Jungen FBP für den doppelten Pukelsheim bei den Landtagswahlen in Liechtenstein: <https://pukelsheim.github.io>

## Aufbau

| Datei / Ordner | Inhalt |
|---|---|
| `index.html`, `argumente.html`, `rechner.html`, `simulator.html`, `doks.html`, `impressum.html`, `datenschutz.html`, `404.html` | Seiten. Oben steht jeweils ein kurzer Kopf (Titel, Beschreibung für Suchmaschinen und Link-Vorschau). |
| `_layouts/standard.html` | Gemeinsamer Rahmen: Kopf, Navigation, Fuss. Änderungen hier gelten für alle Seiten. |
| `_config.yml` | Titel, Beschreibung, Kontaktadresse |
| `assets/js/pukelsheim.js` | Rechenkern (Art. 55 und 56 der Initiative) |
| `assets/js/wahlen.js` | Amtliche Parteistimmen und tatsächliche Sitze 1993–2025 |
| `assets/js/darstellung.js`, `rechner.js`, `beispiele.js` | Darstellung von Rechner und früheren Wahlen |
| `assets/css/style.css` | Gestaltung |
| `assets/fonts/` | Schrift Public Sans, selbst gehostet (SIL Open Font License) |
| `tests/pukelsheim.test.js` | Automatische Tests des Rechenkerns |
| `werkzeuge/` | Lokale Vorschau und Screenshots |

GitHub Pages baut die Seite automatisch mit Jekyll. Es braucht keine weiteren Schritte: Änderungen hochladen, nach ein bis zwei Minuten sind sie online.

## Neue Wahl ergänzen

In `assets/js/wahlen.js` einen Eintrag oben in `WAHLEN` hinzufügen: Parteistimmen Oberland und Unterland aus dem amtlichen Ergebnis sowie die tatsächlich erzielten Sitze. Eine neue Partei zusätzlich in `PARTEIEN` mit Name und Farbe eintragen.

## Tests

```sh
node tests/pukelsheim.test.js
```

Geprüft werden das Rechenbeispiel aus dem Initiativtext (inklusive Wahlschlüssel 610 und Divisoren 9000/6000 und 0.98/1.02), alle Landtagswahlen seit 1993, die Gallagher-Werte aus dem Initiativtext, Randfälle und 20 000 Zufallswahlen gegen eine unabhängige Referenzrechnung.

## Lokale Vorschau

```sh
python3 werkzeuge/vorschau.py
python3 -m http.server -d _vorschau
```

Danach <http://localhost:8000> öffnen. Mit Jekyll geht es auch direkt: `jekyll serve`.

## Lizenz

Programmcode: MIT-Lizenz (siehe `LICENSE`). Texte und PDF-Dokumente: © Junge FBP. Schrift: SIL Open Font License (`assets/fonts/OFL.txt`).

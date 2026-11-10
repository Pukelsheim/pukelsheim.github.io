#!/usr/bin/env python3
"""
Lokale Vorschau ohne Jekyll-Installation.

Bildet genau die wenigen Jekyll-Funktionen nach, die diese Seite verwendet:
Front Matter, Layout, {{ page.x }}, {{ site.x }}, {{ content }} und
{% if page.nav == "x" %} ... {% endif %}. Auf GitHub Pages baut Jekyll selbst.

    python3 werkzeuge/vorschau.py          # baut nach _vorschau/
    python3 -m http.server -d _vorschau    # dann http://localhost:8000
"""
import re
import shutil
import sys
from pathlib import Path

WURZEL = Path(__file__).resolve().parent.parent
ZIEL = WURZEL / "_vorschau"


def lies_yaml(text):
    daten, liste = {}, None
    for zeile in text.splitlines():
        if not zeile.strip() or zeile.lstrip().startswith("#"):
            continue
        if zeile.startswith("  - ") and liste is not None:
            daten[liste].append(zeile[4:].strip())
            continue
        schluessel, _, wert = zeile.partition(":")
        wert = wert.strip()
        if wert == "":
            liste = schluessel.strip()
            daten[liste] = []
        else:
            daten[schluessel.strip()] = wert.strip('"')
            liste = None
    return daten


def front_matter(text):
    m = re.match(r"^---\n(.*?)\n---\n?", text, re.S)
    if not m:
        return None, text
    return lies_yaml(m.group(1)), text[m.end():]


def liquid(text, page, site):
    def wenn(m):
        feld, wert, inhalt = m.group(1), m.group(2), m.group(3)
        return inhalt if str(page.get(feld, "")) == wert else ""
    text = re.sub(r'{%\s*if page\.(\w+)\s*==\s*"([^"]*)"\s*%}(.*?){%\s*endif\s*%}', wenn, text, flags=re.S)
    text = re.sub(r"{{\s*page\.(\w+)\s*}}", lambda m: str(page.get(m.group(1), "")), text)
    text = re.sub(r"{{\s*site\.(\w+)\s*}}", lambda m: str(site.get(m.group(1), "")), text)
    if "{%" in text or re.search(r"{{(?!\s*content\s*}})", text):
        raise SystemExit("Nicht unterstützte Liquid-Anweisung gefunden")
    return text


def main():
    site = lies_yaml((WURZEL / "_config.yml").read_text(encoding="utf-8"))
    site["url"] = ""  # lokal relative Adressen
    ausnahmen = set(site.get("exclude", [])) | {".git", "werkzeuge"}
    if ZIEL.exists():
        shutil.rmtree(ZIEL)
    for quelle in WURZEL.rglob("*"):
        rel = quelle.relative_to(WURZEL)
        if any(teil.startswith((".", "_")) or teil in ausnahmen for teil in rel.parts):
            continue
        if quelle.is_dir():
            continue
        ziel = ZIEL / rel
        ziel.parent.mkdir(parents=True, exist_ok=True)
        if quelle.suffix == ".html":
            fm, inhalt = front_matter(quelle.read_text(encoding="utf-8"))
            if fm is not None:
                fm["url"] = "/" if rel.name == "index.html" else "/" + rel.as_posix()
                inhalt = liquid(inhalt, fm, site)
                layout = (WURZEL / "_layouts" / (fm.get("layout", "standard") + ".html")).read_text(encoding="utf-8")
                seite = liquid(layout, fm, site).replace("{{ content }}", inhalt)
                ziel.write_text(seite, encoding="utf-8")
                continue
        shutil.copy2(quelle, ziel)
    print("Vorschau gebaut:", ZIEL)


if __name__ == "__main__":
    sys.exit(main())

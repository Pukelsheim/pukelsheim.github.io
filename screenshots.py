#!/usr/bin/env python3
"""Screenshots aller Seiten (Desktop und Mobil) aus der lokalen Vorschau.

    python3 werkzeuge/vorschau.py
    python3 werkzeuge/screenshots.py [ausgabeordner] [seite.html ...]
"""
import functools
import http.server
import sys
import threading
from pathlib import Path

from playwright.sync_api import sync_playwright

WURZEL = Path(__file__).resolve().parent.parent
VORSCHAU = WURZEL / "_vorschau"
AUSGABE = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("/tmp/screens")
SEITEN = sys.argv[2:] or ["index.html", "argumente.html", "rechner.html", "simulator.html", "doks.html", "impressum.html", "datenschutz.html", "404.html"]


def main():
    AUSGABE.mkdir(parents=True, exist_ok=True)
    handler = functools.partial(http.server.SimpleHTTPRequestHandler, directory=str(VORSCHAU))
    handler.log_message = lambda *a: None
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    basis = f"http://127.0.0.1:{server.server_address[1]}/"
    fehler = []
    with sync_playwright() as p:
        browser = p.chromium.launch(executable_path="/usr/bin/google-chrome")
        for name, groesse in [("desktop", {"width": 1280, "height": 900}), ("mobil", {"width": 390, "height": 844})]:
            seite = browser.new_page(viewport=groesse)
            seite.on("console", lambda m: fehler.append(f"{m.type}: {m.text}") if m.type in ("error", "warning") else None)
            seite.on("pageerror", lambda e: fehler.append(f"pageerror: {e}"))
            seite.on("requestfailed", lambda r: fehler.append(f"requestfailed: {r.url}"))
            for s in SEITEN:
                seite.goto(basis + s, wait_until="networkidle")
                seite.screenshot(path=str(AUSGABE / f"{Path(s).stem}-{name}.png"), full_page=True)
        browser.close()
    server.shutdown()
    print("\n".join(fehler) if fehler else "Keine Konsolenfehler")


if __name__ == "__main__":
    main()

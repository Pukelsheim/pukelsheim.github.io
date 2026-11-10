/*
 * Gemeinsame Darstellung für Rechner und frühere Wahlen:
 * Zahlenformat, Sitzband und vollständiger Rechenweg.
 */
(function (root) {
  'use strict';

  var ZAHL = new Intl.NumberFormat('de-CH', { maximumFractionDigits: 0 });

  function zahl(x, stellen) {
    if (x === null || x === undefined || !isFinite(x)) return '–';
    if (!stellen) return ZAHL.format(Math.round(x));
    return new Intl.NumberFormat('de-CH', { minimumFractionDigits: stellen, maximumFractionDigits: stellen }).format(x);
  }
  /** Divisoren so knapp wie möglich anzeigen (9000, 0.98, 470.1). */
  function divisor(x) {
    if (x === null || x === undefined) return '–';
    var s = Number(x.toPrecision(10));
    return new Intl.NumberFormat('de-CH', { maximumFractionDigits: 6 }).format(s);
  }
  function prozent(x, stellen) {
    return zahl(x * 100, stellen === undefined ? 2 : stellen) + '\u00a0%';
  }
  function esc(s) {
    return String(s).replace(/[&<>"']/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
    });
  }
  function el(html) {
    var t = document.createElement('template');
    t.innerHTML = html.trim();
    return t.content;
  }
  function punkt(farbe) {
    return '<span class="partei-punkt" style="--sitzfarbe:' + esc(farbe) + '" aria-hidden="true"></span>';
  }

  /**
   * Sitzband: zeilen = [{ titel, notiz, sitze: [{name, farbe, anzahl, wechsel}] }]
   * oder mit Wahlkreisen: sitzeJeKreis = [[{name, farbe, anzahl}], [...]]
   */
  function sitzband(zeilen, legende, beschriftung) {
    var html = '<figure class="sitzband">';
    zeilen.forEach(function (z) {
      var beschreibung = [];
      var li = '';
      function felder(gruppe) {
        gruppe.forEach(function (g) {
          if (g.anzahl > 0) beschreibung.push(g.name + ' ' + g.anzahl);
          for (var i = 0; i < g.anzahl; i++) {
            var wechsel = g.wechsel && i >= g.anzahl - g.wechsel;
            li += '<li style="--sitzfarbe:' + esc(g.farbe) + '"' + (wechsel ? ' class="wechsel"' : '') + '></li>';
          }
        });
      }
      var kreise = !!z.kreise;
      if (kreise) {
        felder(z.kreise[0]);
        li += '<li class="luecke"></li>';
        felder(z.kreise[1]);
      } else {
        felder(z.sitze);
      }
      html += '<div class="sitzband__zeile">' +
        '<div class="sitzband__titel"><span>' + esc(z.titel) + '</span><span>' + esc(z.notiz || '') + '</span></div>' +
        '<ul class="sitze' + (kreise ? ' sitze--kreise' : '') + '" role="img" aria-label="' + esc(z.titel + ': ' + beschreibung.join(', ')) + '">' + li + '</ul>' +
        (kreise ? '<div class="sitze__kreisnamen" aria-hidden="true"><span>Oberland, 15 Sitze</span><span></span><span>Unterland, 10 Sitze</span></div>' : '') +
        '</div>';
    });
    if (legende && legende.length) {
      html += '<ul class="legende">' + legende.map(function (l) {
        return '<li' + (l.wechsel ? ' class="legende--wechsel"' : '') + ' style="--sitzfarbe:' + esc(l.farbe || '#fff') + '">' + esc(l.name) + '</li>';
      }).join('') + '</ul>';
    }
    if (beschriftung) html += '<figcaption>' + beschriftung + '</figcaption>';
    return html + '</figure>';
  }

  /** Sitzband eines Pukelsheim-Ergebnisses nach Wahlkreisen. */
  function sitzbandErgebnis(e, titel) {
    var mitSitzen = e.parteien.filter(function (p) { return p.sitze > 0; });
    return sitzband([{
      titel: titel || 'Sitzverteilung mit dem doppelten Pukelsheim',
      notiz: '25 Sitze',
      kreise: [0, 1].map(function (z) {
        return mitSitzen.map(function (p) { return { name: p.name, farbe: p.farbe, anzahl: p.sitzeJeKreis[z] }; });
      })
    }], mitSitzen.map(function (p) { return { name: p.name + ' ' + p.sitze, farbe: p.farbe }; }));
  }

  /** Vollständiger Rechenweg mit allen Zwischenschritten. */
  function rechenweg(e) {
    var ws = e.wahlschluessel;
    var html = '';

    // Schritt 1
    html += '<h3>Schritt 1: Oberzuteilung (Art. 55)</h3>' +
      '<p class="text">Die Parteistimmen werden durch die Zahl der Sitze im Wahlkreis geteilt (Oberland ÷ 15, Unterland ÷ 10). ' +
      'Das ergibt die Wählerstimmen: Jede Wählerin und jeder Wähler zählt genau einmal. Parteien unter 8&nbsp;% der Wählerstimmen fallen weg. ' +
      'Die übrigen Wählerstimmen werden durch den Wahlschlüssel geteilt und gerundet.</p>' +
      '<div class="tabelle-wrap"><table class="daten"><thead><tr>' +
      '<th scope="col">Partei</th><th class="z" scope="col">Parteistimmen Oberland</th><th class="z" scope="col">Parteistimmen Unterland</th>' +
      '<th class="z" scope="col">Wählerstimmen</th><th class="z" scope="col">Anteil</th>' +
      '<th class="z" scope="col">÷ Wahlschlüssel</th><th class="z" scope="col">Sitze</th></tr></thead><tbody>';
    var sum = { o: 0, u: 0, w: 0, s: 0 };
    e.parteien.forEach(function (p) {
      if (!(p.waehler > 0)) return;
      sum.o += p.stimmen[0]; sum.u += p.stimmen[1]; sum.w += p.waehler; sum.s += p.sitze || 0;
      html += '<tr' + (p.ueberHuerde ? '' : ' class="aus"') + '><th scope="row">' + punkt(p.farbe) + esc(p.name) + '</th>' +
        '<td class="z">' + zahl(p.stimmen[0]) + '</td><td class="z">' + zahl(p.stimmen[1]) + '</td>' +
        '<td class="z">' + zahl(p.waehler, 1) + '</td><td class="z">' + prozent(p.anteil) + '</td>' +
        '<td class="z">' + (p.ueberHuerde ? zahl(p.quotient, 3) : 'unter 8\u00a0%') + '</td>' +
        '<td class="z">' + (p.sitze || 0) + '</td></tr>';
    });
    html += '</tbody><tfoot><tr><th scope="row">Total</th><td class="z">' + zahl(sum.o) + '</td><td class="z">' + zahl(sum.u) +
      '</td><td class="z">' + zahl(sum.w, 1) + '</td><td class="z">100&nbsp;%</td><td></td><td class="z">' + sum.s + '</td></tr></tfoot></table></div>';
    html += '<p class="kennzahl">Wahlschlüssel: <strong>' + divisor(ws.definitiv) + '</strong>' +
      (ws.definitiv !== ws.provisorisch
        ? ' <span class="leise">(Die Wählerstimmen über der Sperrklausel ÷ 25, aufgerundet, ergeben ' + zahl(ws.provisorisch) +
          '. Damit würden nicht genau 25 Sitze vergeben, deshalb wird der Wahlschlüssel angepasst, wie es Art. 55 Abs. 5 vorsieht.)</span>'
        : ' <span class="leise">(Wählerstimmen über der Sperrklausel ÷ 25, aufgerundet)</span>') + '</p>';

    // Schritt 2
    var aktiv = e.parteien.filter(function (p) { return p.sitze > 0; });
    html += '<h3>Schritt 2: Unterzuteilung (Art. 56)</h3>' +
      '<p class="text">Jetzt werden die Sitze jeder Partei auf Oberland und Unterland verteilt. Dazu werden die Parteistimmen durch den Wahlkreis-Divisor ' +
      'und den Parteidivisor geteilt und gerundet. Die Divisoren sind so gewählt, dass das Oberland genau 15 und das Unterland genau 10 Sitze erhält ' +
      'und jede Partei genau ihre Sitze aus Schritt 1. Ein Parteidivisor von 1 bedeutet: keine Korrektur nötig.</p>';
    html += '<div class="zweispaltig">';
    e.wahlkreise.forEach(function (k, z) {
      html += '<div class="tabelle-wrap"><table class="daten"><caption>' + esc(k.name) + ' (Wahlkreis-Divisor ' + divisor(e.wahlkreisDivisoren[z]) + ')</caption>' +
        '<thead><tr><th scope="col">Partei</th><th class="z" scope="col">Parteistimmen</th><th class="z" scope="col">Parteidivisor</th><th class="z" scope="col">Quotient</th><th class="z" scope="col">Sitze</th></tr></thead><tbody>';
      var s = 0;
      aktiv.forEach(function (p) {
        var q = p.stimmen[z] / (e.wahlkreisDivisoren[z] * p.divisor);
        s += p.sitzeJeKreis[z];
        html += '<tr><th scope="row">' + punkt(p.farbe) + esc(p.name) + '</th><td class="z">' + zahl(p.stimmen[z]) + '</td>' +
          '<td class="z">' + divisor(p.divisor) + '</td><td class="z">' + zahl(q, 3) + '</td><td class="z">' + p.sitzeJeKreis[z] + '</td></tr>';
      });
      html += '</tbody><tfoot><tr><th scope="row">Total</th><td></td><td></td><td></td><td class="z">' + s + '</td></tr></tfoot></table></div>';
    });
    html += '</div>';

    // Schritt 3
    html += '<h3>Ergebnis und Kontrolle</h3><div class="tabelle-wrap"><table class="daten"><thead><tr><th scope="col">Partei</th>' +
      '<th class="z" scope="col">Oberland</th><th class="z" scope="col">Unterland</th><th class="z" scope="col">Total</th><th class="z" scope="col">Soll aus Schritt 1</th></tr></thead><tbody>';
    aktiv.forEach(function (p) {
      var t = p.sitzeJeKreis[0] + p.sitzeJeKreis[1];
      html += '<tr><th scope="row">' + punkt(p.farbe) + esc(p.name) + '</th><td class="z">' + p.sitzeJeKreis[0] + '</td><td class="z">' + p.sitzeJeKreis[1] +
        '</td><td class="z">' + t + '</td><td class="z">' + p.sitze + (t === p.sitze ? ' ✓' : ' ✗') + '</td></tr>';
    });
    var k = e.kontrolle || {};
    html += '</tbody><tfoot><tr><th scope="row">Total</th><td class="z">15' + (k.wahlkreise ? ' ✓' : '') + '</td><td class="z">10' + (k.wahlkreise ? ' ✓' : '') +
      '</td><td class="z">25</td><td></td></tr></tfoot></table></div>';
    return html;
  }

  var FARBEN_FREI = ['#12357f', '#b0413e', '#2e7d4f', '#7a4fa3', '#c27c00', '#56606e'];

  root.Darstellung = {
    zahl: zahl, divisor: divisor, prozent: prozent, esc: esc, el: el, punkt: punkt,
    sitzband: sitzband, sitzbandErgebnis: sitzbandErgebnis, rechenweg: rechenweg,
    FARBEN_FREI: FARBEN_FREI
  };
})(window);

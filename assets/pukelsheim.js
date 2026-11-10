/*!
 * Doppelter Pukelsheim für die Landtagswahlen in Liechtenstein
 * (doppeltproportionales Divisorverfahren mit Standardrundung)
 *
 * Umsetzung von Art. 55 (Oberzuteilung) und Art. 56 (Unterzuteilung)
 * gemäss Volksinitiative «Fair gezählt. Fair gewählt.»
 *
 * - Die Sitzzahlen werden exakt mit dem Höchstzahlverfahren nach
 *   Sainte-Laguë bestimmt (gleichwertig zum Divisorverfahren mit
 *   Standardrundung), nicht durch schrittweises Probieren.
 * - Danach werden möglichst «runde» Divisoren gewählt, wie sie die
 *   Regierung im Protokoll angeben würde, und das Ergebnis wird mit
 *   diesen Divisoren nochmals vollständig nachgerechnet.
 *
 * Läuft im Browser (window.Pukelsheim) und in Node (module.exports).
 * Lizenz: MIT
 */
(function (root) {
  'use strict';

  var EPS = 1e-12;

  /** Standardrundung: ab x.5 aufrunden. */
  function runde(x) {
    return Math.floor(x + 0.5);
  }

  /**
   * Divisorverfahren mit Standardrundung (Sainte-Laguë) als Höchstzahlverfahren.
   * Liefert die Sitzverteilung und das Intervall (lo, hi], in dem jeder
   * Divisor genau diese Verteilung ergibt. gleichstand = true, wenn zwei
   * Ansprüche exakt gleich sind (dann entscheidet das Los).
   */
  function hoechstzahl(gewichte, sitze) {
    var quotienten = [];
    gewichte.forEach(function (w, i) {
      if (!(w > 0)) return;
      for (var k = 0; k <= sitze; k++) quotienten.push({ q: w / (k + 0.5), i: i });
    });
    var verteilung = gewichte.map(function () { return 0; });
    if (sitze === 0) {
      var max = quotienten.reduce(function (m, x) { return Math.max(m, x.q); }, 0);
      return { verteilung: verteilung, lo: max, hi: Infinity, gleichstand: false };
    }
    if (quotienten.length < sitze) return null;
    quotienten.sort(function (a, b) { return b.q - a.q; });
    for (var s = 0; s < sitze; s++) verteilung[quotienten[s].i]++;
    var hi = quotienten[sitze - 1].q;
    var lo = quotienten.length > sitze ? quotienten[sitze].q : 0;
    var gleichstand = quotienten.length > sitze && Math.abs(hi - lo) <= EPS * hi;
    return { verteilung: verteilung, lo: lo, hi: hi, gleichstand: gleichstand };
  }

  /**
   * «Runde» Zahl im offenen Intervall (lo, hi): zuerst ganze Tausender,
   * Hunderter, Zehner, Einer, dann Dezimalstellen.
   */
  function rundeZahlImIntervall(lo, hi, bevorzugt) {
    if (bevorzugt !== undefined && bevorzugt > lo && bevorzugt < hi) return bevorzugt;
    if (!isFinite(hi)) hi = Math.max(lo * 2, lo + 1);
    if (!(hi > lo)) return (lo + hi) / 2;
    for (var k = Math.floor(Math.log10(hi)); k > -10; k--) {
      var schritt = Math.pow(10, k);
      var kandidat = Number(((Math.floor(lo / schritt) + 1) * schritt).toPrecision(12));
      if (kandidat > lo && kandidat < hi) return kandidat;
    }
    return (lo + hi) / 2;
  }

  /**
   * Ganze Zahl im Intervall (lo, hi), die möglichst nahe am Startwert liegt.
   * Gibt es keine, wird die rundeste Dezimalzahl im Intervall gewählt.
   */
  function naechsteGanzeZahl(lo, hi, start) {
    var min = Math.floor(lo) + 1, max = Math.ceil(hi) - 1;
    if (min > max) return rundeZahlImIntervall(lo, hi);
    return Math.min(max, Math.max(min, start));
  }

  /** Intervall für einen Divisor d, damit runde(v / (d * f)) = x gilt. */
  function intervallFuer(v, f, x) {
    if (!(v > 0)) return { lo: 0, hi: Infinity };
    return {
      lo: v / (f * (x + 0.5)),
      hi: x >= 1 ? v / (f * (x - 0.5)) : Infinity
    };
  }

  /**
   * Hauptfunktion.
   * eingabe = {
   *   parteien: [{ name, stimmen: [Oberland, Unterland] }],
   *   wahlkreise: [{ name: 'Oberland', sitze: 15 }, { name: 'Unterland', sitze: 10 }],
   *   huerde: 0.08
   * }
   */
  function berechne(eingabe) {
    var wahlkreise = eingabe.wahlkreise || [
      { name: 'Oberland', sitze: 15 },
      { name: 'Unterland', sitze: 10 }
    ];
    var huerde = eingabe.huerde === undefined ? 0.08 : eingabe.huerde;
    var totalSitze = wahlkreise.reduce(function (s, w) { return s + w.sitze; }, 0);

    var parteien = eingabe.parteien.map(function (p) {
      var stimmen = wahlkreise.map(function (_, r) {
        var v = Number(p.stimmen[r]);
        return isFinite(v) && v > 0 ? v : 0;
      });
      var waehlerJeKreis = stimmen.map(function (v, r) { return v / wahlkreise[r].sitze; });
      return {
        name: p.name,
        farbe: p.farbe,
        stimmen: stimmen,
        waehlerJeKreis: waehlerJeKreis,
        waehler: waehlerJeKreis.reduce(function (a, b) { return a + b; }, 0)
      };
    });

    var ergebnis = { parteien: parteien, wahlkreise: wahlkreise, totalSitze: totalSitze, huerde: huerde, fehler: null, hinweise: [] };

    // Art. 55 Abs. 1–3: Wählerstimmen und Sperrklausel
    var totalWaehler = parteien.reduce(function (s, p) { return s + p.waehler; }, 0);
    if (!(totalWaehler > 0)) {
      ergebnis.fehler = 'Bitte mindestens für eine Partei Stimmen eingeben.';
      return ergebnis;
    }
    parteien.forEach(function (p) {
      p.anteil = p.waehler / totalWaehler;
      p.ueberHuerde = p.waehler > 0 && p.anteil >= huerde;
    });
    var berechtigt = parteien.map(function (p) { return p.ueberHuerde ? p.waehler : 0; });
    var summeBerechtigt = berechtigt.reduce(function (a, b) { return a + b; }, 0);
    if (!(summeBerechtigt > 0)) {
      ergebnis.fehler = 'Keine Partei erreicht die Sperrklausel.';
      return ergebnis;
    }

    // Art. 55 Abs. 4–6: Oberzuteilung
    var ober = hoechstzahl(berechtigt, totalSitze);
    var provisorisch = Math.ceil(summeBerechtigt / totalSitze);
    // Gemäss Art. 55 Abs. 4 wird zuerst aufgerundet; passt das nicht, legt die
    // Regierung einen Wahlschlüssel fest, der genau 25 Sitze ergibt (Abs. 5).
    var wahlschluessel = naechsteGanzeZahl(ober.lo, ober.hi, provisorisch);
    if (ober.gleichstand) ergebnis.hinweise.push('In der Oberzuteilung haben zwei Parteien exakt den gleichen Anspruch auf ein Mandat. Gemäss Art. 55 Abs. 6 entscheidet das Los; hier ist eine der möglichen Verteilungen dargestellt.');
    parteien.forEach(function (p, i) {
      p.quotient = berechtigt[i] / wahlschluessel;
      p.sitze = ober.verteilung[i];
    });
    ergebnis.wahlschluessel = { provisorisch: provisorisch, definitiv: wahlschluessel, von: ober.lo, bis: ober.hi };

    // Art. 56: Unterzuteilung (alternierende Skalierung)
    var aktiv = [];
    parteien.forEach(function (p, i) { if (p.sitze > 0) aktiv.push(i); });
    var M = wahlkreise.map(function (_, r) { return aktiv.map(function (i) { return parteien[i].stimmen[r]; }); });
    var R = wahlkreise.map(function (w) { return w.sitze; });
    var S = aktiv.map(function (i) { return parteien[i].sitze; });

    // Machbarkeit: Eine Partei kann in einem Kreis ohne Stimmen keine Sitze erhalten.
    for (var a = 0; a < aktiv.length; a++) {
      var moeglich = 0;
      for (var rr = 0; rr < R.length; rr++) if (M[rr][a] > 0) moeglich += R[rr];
      if (moeglich < S[a]) {
        ergebnis.fehler = 'Die Unterzuteilung ist für diese Eingabe nicht lösbar: ' + parteien[aktiv[a]].name + ' hat mehr Mandate, als in den Wahlkreisen mit ihren Stimmen zu vergeben sind.';
        return ergebnis;
      }
    }

    var c = aktiv.map(function () { return 1; });
    var r = R.map(function () { return 1; });
    var X = null, gleichstandUnter = false, fertig = false;

    function zeilenSchritt() {
      var ok = true;
      X = M.map(function (zeile, z) {
        var res = hoechstzahl(zeile.map(function (v, j) { return v / c[j]; }), R[z]);
        r[z] = isFinite(res.hi) ? (res.lo + res.hi) / 2 : res.lo * 2 + 1;
        if (res.gleichstand) gleichstandUnter = true;
        return res.verteilung;
      });
      for (var j = 0; j < aktiv.length; j++) {
        var s = 0;
        for (var z = 0; z < R.length; z++) s += X[z][j];
        if (s !== S[j]) ok = false;
      }
      return ok;
    }
    function spaltenSchritt() {
      for (var j = 0; j < aktiv.length; j++) {
        var res = hoechstzahl(M.map(function (zeile, z) { return zeile[j] / r[z]; }), S[j]);
        c[j] = isFinite(res.hi) ? (res.lo + res.hi) / 2 : res.lo * 2 + 1;
        if (res.gleichstand) gleichstandUnter = true;
        for (var z = 0; z < R.length; z++) X[z][j] = res.verteilung[z];
      }
      return X.every(function (zeile, z) { return zeile.reduce(function (a, b) { return a + b; }, 0) === R[z]; });
    }

    for (var iter = 0; iter < 1000 && !fertig; iter++) {
      gleichstandUnter = false;
      if (zeilenSchritt()) { fertig = true; break; }
      if (spaltenSchritt()) { fertig = true; break; }
    }
    if (!fertig) {
      ergebnis.fehler = 'Die Unterzuteilung konnte nicht bestimmt werden. Bitte die Eingabe prüfen.';
      return ergebnis;
    }
    if (gleichstandUnter) ergebnis.hinweise.push('In der Unterzuteilung gibt es einen exakten Gleichstand. In diesem Fall entscheidet das Los; hier ist eine der möglichen Verteilungen dargestellt.');

    // «Runde» Divisoren suchen, die genau diese Verteilung ergeben
    function kreisIntervall(z, cc) {
      var lo = 0, hi = Infinity;
      for (var j = 0; j < aktiv.length; j++) {
        var iv = intervallFuer(M[z][j], cc[j], X[z][j]);
        lo = Math.max(lo, iv.lo); hi = Math.min(hi, iv.hi);
      }
      return { lo: lo, hi: hi };
    }
    function parteiIntervall(j, rr) {
      var lo = 0, hi = Infinity;
      for (var z = 0; z < R.length; z++) {
        var iv = intervallFuer(M[z][j], rr[z], X[z][j]);
        lo = Math.max(lo, iv.lo); hi = Math.min(hi, iv.hi);
      }
      return { lo: lo, hi: hi };
    }
    var rn = r.slice(), cn = c.slice();
    for (var runde2 = 0; runde2 < 3; runde2++) {
      rn = rn.map(function (_, z) {
        var iv = kreisIntervall(z, cn);
        var start = M[z].reduce(function (a, b) { return a + b; }, 0) / R[z];
        return rundeZahlImIntervall(iv.lo, iv.hi, Math.round(start));
      });
      cn = cn.map(function (_, j) {
        var iv = parteiIntervall(j, rn);
        return rundeZahlImIntervall(iv.lo, iv.hi, 1);
      });
    }
    // Kontrolle mit den angezeigten Divisoren
    var kontrolleOk = M.every(function (zeile, z) {
      return zeile.every(function (v, j) { return runde(v / (rn[z] * cn[j])) === X[z][j]; });
    });
    if (!kontrolleOk) { rn = r.slice(); cn = c.slice(); }

    parteien.forEach(function (p) {
      p.sitzeJeKreis = wahlkreise.map(function () { return 0; });
      p.divisor = null;
    });
    aktiv.forEach(function (i, j) {
      parteien[i].divisor = cn[j];
      for (var z = 0; z < R.length; z++) parteien[i].sitzeJeKreis[z] = X[z][j];
    });
    ergebnis.wahlkreisDivisoren = rn;

    // Vollständige Schlusskontrolle
    var kontrolle = {
      wahlkreise: wahlkreise.every(function (w, z) {
        return parteien.reduce(function (s, p) { return s + p.sitzeJeKreis[z]; }, 0) === w.sitze;
      }),
      parteien: parteien.every(function (p) {
        return p.sitzeJeKreis.reduce(function (a, b) { return a + b; }, 0) === p.sitze;
      }),
      total: parteien.reduce(function (s, p) { return s + p.sitze; }, 0) === totalSitze
    };
    ergebnis.kontrolle = kontrolle;
    if (!(kontrolle.wahlkreise && kontrolle.parteien && kontrolle.total)) {
      ergebnis.fehler = 'Interne Kontrolle fehlgeschlagen. Bitte melden Sie diese Eingabe an junge@fbp.li.';
    }
    return ergebnis;
  }

  /** Gallagher-Index in Prozentpunkten. anteile und sitze als Arrays. */
  function gallagher(anteile, sitze) {
    var total = sitze.reduce(function (a, b) { return a + b; }, 0);
    var summe = 0;
    for (var i = 0; i < anteile.length; i++) {
      var d = anteile[i] * 100 - (sitze[i] / total) * 100;
      summe += d * d;
    }
    return Math.sqrt(summe / 2);
  }

  var api = { berechne: berechne, gallagher: gallagher, _hoechstzahl: hoechstzahl, _runde: runde };
  if (typeof module === 'object' && module.exports) module.exports = api;
  else root.Pukelsheim = api;
})(typeof window !== 'undefined' ? window : this);

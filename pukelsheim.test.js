/*
 * Tests für assets/js/pukelsheim.js
 * Ausführen mit:  node tests/pukelsheim.test.js
 * (keine Abhängigkeiten nötig)
 */
'use strict';
var assert = require('assert');
var path = require('path');
var P = require(path.join(__dirname, '..', 'assets', 'js', 'pukelsheim.js'));
var W = require(path.join(__dirname, '..', 'assets', 'js', 'wahlen.js'));

var bestanden = 0;
function test(name, fn) {
  try { fn(); bestanden++; console.log('  ✓ ' + name); }
  catch (e) { console.error('  ✗ ' + name + '\n    ' + e.message); process.exitCode = 1; }
}
function sitze(e) { return e.parteien.map(function (p) { return p.sitze; }); }
function kreis(e, z) { return e.parteien.map(function (p) { return p.sitzeJeKreis[z]; }); }
function wahl(id) {
  var w = W.WAHLEN.filter(function (x) { return x.id === id; })[0];
  return P.berechne({ parteien: w.parteien.map(function (x) { return { name: x.p, stimmen: x.stimmen }; }) });
}
/** Prüft, dass die angezeigten Divisoren genau das angezeigte Ergebnis liefern. */
function pruefeDivisoren(e) {
  e.parteien.forEach(function (p) {
    if (!p.sitze) return;
    p.stimmen.forEach(function (v, z) {
      var x = P._runde(v / (e.wahlkreisDivisoren[z] * p.divisor));
      assert.strictEqual(x, p.sitzeJeKreis[z], p.name + ' Kreis ' + z);
    });
  });
  var summe = e.parteien.reduce(function (s, p) { return s + (p.ueberHuerde ? P._runde(p.waehler / e.wahlschluessel.definitiv) : 0); }, 0);
  assert.strictEqual(summe, 25, 'Wahlschlüssel ergibt 25 Sitze');
}

console.log('Rechenbeispiel aus dem Initiativtext');
test('fiktive Wahl Partei A–E', function () {
  var e = P.berechne({ parteien: [
    { name: 'A', stimmen: [57600, 26400] },
    { name: 'B', stimmen: [50400, 27600] },
    { name: 'C', stimmen: [15750, 2400] },
    { name: 'D', stimmen: [11250, 6000] },
    { name: 'E', stimmen: [4500, 2400] }
  ] });
  assert.strictEqual(e.fehler, null);
  assert.deepStrictEqual(sitze(e), [11, 10, 2, 2, 0]);
  assert.deepStrictEqual(kreis(e, 0), [7, 5, 2, 1, 0]);
  assert.deepStrictEqual(kreis(e, 1), [4, 5, 0, 1, 0]);
  assert.strictEqual(e.wahlschluessel.definitiv, 610);
  pruefeDivisoren(e);
});

console.log('Historische Wahlen (Vergleich mit BuA 109/2025, Anhang Marxer, «1 WK mit SK»)');
// 1993: Der Marxer-Anhang nennt 11/12/2 bzw. 10/13/2. Die Gallagher-Werte im
// Initiativtext (1.56 % bzw. 2.00 %) passen aber nur zu 11/11/3 bzw. 11/12/2;
// Handrechnung 1993 (Feb.): Wählerstimmen / 483.7 = 11.21 / 11.24 / 2.55.
var erwartet = {
  '1993-02': [11, 11, 3], '1993-10': [11, 12, 2], '1997': [10, 12, 3], '2001': [13, 10, 2],
  '2005': [12, 10, 3], '2009': [11, 12, 2], '2013': [10, 8, 3, 4], '2017': [9, 8, 3, 5],
  '2021': [10, 9, 3, 0, 3]
};
Object.keys(erwartet).forEach(function (id) {
  test(id, function () {
    var e = wahl(id);
    assert.strictEqual(e.fehler, null);
    assert.deepStrictEqual(sitze(e), erwartet[id]);
    pruefeDivisoren(e);
  });
});
test('2025 (laut Initiative eine Sitzverschiebung gegenüber dem Ist)', function () {
  var e = wahl('2025');
  assert.deepStrictEqual(sitze(e), [7, 9, 3, 6]);
  pruefeDivisoren(e);
});

console.log('Gallagher-Index (Vergleich mit Tabelle im Initiativtext 2026)');
var gallagherTabelle = {
  '2025': [2.45, 1.80], '2021': [5.62, 3.93], '2017': [1.63, 1.63], '2013': [1.17, 1.17],
  '2009': [4.78, 0.78], '2005': [1.78, 1.78], '2001': [3.95, 1.36], '1997': [3.47, 0.58],
  '1993-10': [3.78, 2.00], '1993-02': [2.81, 1.56]
};
Object.keys(gallagherTabelle).forEach(function (id) {
  test(id, function () {
    var w = W.WAHLEN.filter(function (x) { return x.id === id; })[0];
    var e = wahl(id);
    var anteile = e.parteien.map(function (p) { return p.anteil; });
    var ist = P.gallagher(anteile, w.parteien.map(function (x) { return x.ist; }));
    var puk = P.gallagher(anteile, sitze(e));
    assert.ok(Math.abs(ist - gallagherTabelle[id][0]) < 0.006, 'Ist ' + ist.toFixed(3));
    assert.ok(Math.abs(puk - gallagherTabelle[id][1]) < 0.006, 'Pukelsheim ' + puk.toFixed(3));
  });
});

console.log('Randfälle');
test('leere Eingabe gibt Fehlermeldung statt NaN', function () {
  var e = P.berechne({ parteien: [{ name: 'A', stimmen: ['', ''] }, { name: 'B', stimmen: [null, undefined] }] });
  assert.ok(e.fehler);
});
test('eine einzige Partei erhält alle 25 Sitze', function () {
  var e = P.berechne({ parteien: [{ name: 'A', stimmen: [1500, 1000] }] });
  assert.deepStrictEqual(kreis(e, 0), [15]);
  assert.deepStrictEqual(kreis(e, 1), [10]);
});
test('Partei unter 8 % erhält keine Sitze', function () {
  var e = P.berechne({ parteien: [{ name: 'A', stimmen: [100000, 50000] }, { name: 'B', stimmen: [5000, 2000] }] });
  assert.strictEqual(e.parteien[1].ueberHuerde, false);
  assert.strictEqual(e.parteien[1].sitze, 0);
});
test('exakter Gleichstand wird als Losentscheid gemeldet', function () {
  var e = P.berechne({ parteien: [{ name: 'A', stimmen: [15000, 10000] }, { name: 'B', stimmen: [15000, 10000] }] });
  assert.ok(e.hinweise.length > 0);
  assert.strictEqual(sitze(e).reduce(function (a, b) { return a + b; }), 25);
});
test('unlösbare Unterzuteilung wird erkannt', function () {
  // Partei A hat nur Unterländer Stimmen, aber Anspruch auf mehr als 10 Sitze
  var e = P.berechne({ parteien: [{ name: 'A', stimmen: [0, 100000] }, { name: 'B', stimmen: [15000, 0] }] });
  assert.ok(e.fehler);
});

console.log('Zufallstests gegen unabhängige Referenzrechnung');
// Referenz: Divisoren per Bisektion (anderer Lösungsweg als im Rechenkern)
function referenz(o, u) {
  var R = Math.round, w = o.map(function (x, i) { return x / 15 + u[i] / 10; });
  var t = w.reduce(function (a, b) { return a + b; }), ok = w.map(function (x) { return x / t >= 0.08; });
  function app(g, S) {
    var lo = 1e-12, hi = 1e12;
    for (var k = 0; k < 400; k++) {
      var d = Math.sqrt(lo * hi), s = g.reduce(function (a, x) { return a + R(x / d); }, 0);
      if (s > S) lo = d; else if (s < S) hi = d; else return g.map(function (x) { return R(x / d); });
    }
    return null;
  }
  var s = app(w.map(function (x, i) { return ok[i] ? x : 0; }), 25);
  if (!s) return null;
  var M = [o.map(function (x, i) { return ok[i] ? x : 0; }), u.map(function (x, i) { return ok[i] ? x : 0; })];
  var T = [15, 10], r = [1, 1], c = o.map(function () { return 1; });
  for (var it = 0; it < 500; it++) {
    for (var a = 0; a < 2; a++) {
      var lo = 1e-12, hi = 1e12;
      for (var k = 0; k < 400; k++) {
        r[a] = Math.sqrt(lo * hi);
        var z = M[a].reduce(function (q, v, b) { return q + R(v / (r[a] * c[b])); }, 0);
        if (z > T[a]) lo = r[a]; else if (z < T[a]) hi = r[a]; else break;
      }
    }
    for (var b = 0; b < c.length; b++) {
      if (!s[b]) continue;
      var lo2 = 1e-12, hi2 = 1e12;
      for (var k2 = 0; k2 < 400; k2++) {
        c[b] = Math.sqrt(lo2 * hi2);
        var z2 = R(M[0][b] / (r[0] * c[b])) + R(M[1][b] / (r[1] * c[b]));
        if (z2 > s[b]) lo2 = c[b]; else if (z2 < s[b]) hi2 = c[b]; else break;
      }
    }
    var so = M[0].map(function (v, b) { return R(v / (r[0] * c[b])); });
    var su = M[1].map(function (v, b) { return R(v / (r[1] * c[b])); });
    if (so.reduce(function (x, y) { return x + y; }) === 15 && su.reduce(function (x, y) { return x + y; }) === 10 &&
        s.every(function (x, b) { return so[b] + su[b] === x; })) return { s: s, so: so, su: su };
  }
  return null;
}
// Pseudozufall mit festem Startwert, damit die Tests reproduzierbar sind
var seed = 20261109;
function zufall() { seed = (seed * 1103515245 + 12345) % 2147483648; return seed / 2147483648; }
var N = 20000, abweichend = 0, fehler = 0, refOhne = 0, ungueltig = 0;
for (var n = 0; n < N; n++) {
  var k = 2 + Math.floor(zufall() * 5);
  var g = [], o = [], u = [];
  for (var i = 0; i < k; i++) g.push(Math.pow(zufall(), 1.5) + 0.01);
  var sg = g.reduce(function (a, b) { return a + b; });
  for (var i2 = 0; i2 < k; i2++) {
    var neigung = 0.5 + zufall();
    o.push(15 * Math.round(10000 * g[i2] / sg * neigung));
    u.push(10 * Math.round(5800 * g[i2] / sg * (2 - neigung)));
  }
  var e = P.berechne({ parteien: o.map(function (x, j) { return { name: 'P' + j, stimmen: [x, u[j]] }; }) });
  var ref = referenz(o, u);
  if (e.fehler) { fehler++; continue; }
  if (!ref) { refOhne++; }
  else if (JSON.stringify([kreis(e, 0), kreis(e, 1)]) !== JSON.stringify([ref.so, ref.su])) abweichend++;
  try { pruefeDivisoren(e); } catch (err) { ungueltig++; }
}
test(N + ' Zufallswahlen: keine Fehler, keine Abweichung, Divisoren stimmen', function () {
  assert.strictEqual(fehler, 0, 'Fehler: ' + fehler);
  assert.strictEqual(abweichend, 0, 'Abweichungen: ' + abweichend + ' (Referenz ohne Lösung: ' + refOhne + ')');
  assert.strictEqual(ungueltig, 0, 'ungültige Divisoren: ' + ungueltig);
});

console.log('\n' + bestanden + ' Tests bestanden' + (process.exitCode ? ', mindestens einer fehlgeschlagen' : ''));

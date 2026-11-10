/* Rechner: Eingabe, Beispiele, Link mit Eingaben, Ausgabe */
(function () {
  'use strict';
  var D = window.Darstellung, P = window.Pukelsheim, W = window.Wahlen;
  var ZEILEN = 6;
  var form = document.getElementById('rechner');
  var tbody = document.getElementById('zeilen');
  var auswahl = document.getElementById('beispiel');
  var meldungen = document.getElementById('meldungen');
  var ergebnisEl = document.getElementById('ergebnis');

  var BEISPIELE = {
    initiative: {
      titel: 'Rechenbeispiel aus dem Initiativtext',
      parteien: [['Partei A', 57600, 26400], ['Partei B', 50400, 27600], ['Partei C', 15750, 2400], ['Partei D', 11250, 6000], ['Partei E', 4500, 2400]]
    }
  };
  W.WAHLEN.forEach(function (w) {
    BEISPIELE[w.id] = {
      titel: 'Landtagswahl ' + w.titel,
      parteien: w.parteien.map(function (x) { return [x.p, x.stimmen[0], x.stimmen[1]]; })
    };
  });
  Object.keys(BEISPIELE).forEach(function (k) {
    var o = document.createElement('option');
    o.value = k; o.textContent = BEISPIELE[k].titel;
    auswahl.appendChild(o);
  });

  function farbeFuer(name, i) {
    var bekannt = W.PARTEIEN[String(name).trim()];
    return bekannt ? bekannt.farbe : D.FARBEN_FREI[i % D.FARBEN_FREI.length];
  }

  for (var i = 0; i < ZEILEN; i++) {
    var tr = document.createElement('tr');
    tr.innerHTML =
      '<td class="farbe"><i style="--sitzfarbe:' + D.FARBEN_FREI[i] + '"></i></td>' +
      '<td><input type="text" name="name" autocomplete="off" placeholder="Partei ' + (i + 1) + '" aria-label="Name Partei ' + (i + 1) + '"></td>' +
      '<td><input type="number" name="o" min="0" step="1" inputmode="numeric" aria-label="Parteistimmen Oberland, Partei ' + (i + 1) + '"></td>' +
      '<td><input type="number" name="u" min="0" step="1" inputmode="numeric" aria-label="Parteistimmen Unterland, Partei ' + (i + 1) + '"></td>';
    tbody.appendChild(tr);
  }
  var reihen = Array.prototype.slice.call(tbody.querySelectorAll('tr'));

  function farbenAktualisieren() {
    reihen.forEach(function (tr, i) {
      tr.querySelector('.farbe i').style.setProperty('--sitzfarbe', farbeFuer(tr.querySelector('[name=name]').value, i));
    });
  }

  function setzen(parteien) {
    reihen.forEach(function (tr, i) {
      var p = parteien[i] || ['', '', ''];
      tr.querySelector('[name=name]').value = p[0];
      tr.querySelector('[name=o]').value = p[1] === '' ? '' : p[1];
      tr.querySelector('[name=u]').value = p[2] === '' ? '' : p[2];
    });
    farbenAktualisieren();
  }

  function lesen() {
    var fehler = [];
    var parteien = [];
    reihen.forEach(function (tr, i) {
      var name = tr.querySelector('[name=name]').value.trim();
      var werte = ['o', 'u'].map(function (f) {
        var feld = tr.querySelector('[name=' + f + ']');
        var roh = feld.value.trim();
        feld.removeAttribute('aria-invalid');
        if (roh === '') return 0;
        var v = Number(roh);
        if (!isFinite(v) || v < 0 || Math.floor(v) !== v) {
          feld.setAttribute('aria-invalid', 'true');
          fehler.push('Zeile ' + (i + 1) + ': Bitte eine ganze Zahl ab 0 eingeben.');
          return 0;
        }
        return v;
      });
      if (werte[0] + werte[1] > 0 || name) {
        parteien.push({ name: name || 'Partei ' + (i + 1), stimmen: werte, farbe: farbeFuer(name, i), zeile: i });
      }
    });
    return { parteien: parteien, fehler: fehler };
  }

  function meldung(texte, art) {
    meldungen.innerHTML = texte.map(function (t) {
      return '<p class="meldung' + (art === 'hinweis' ? ' meldung--hinweis' : '') + '">' + D.esc(t) + '</p>';
    }).join('');
  }

  function hashSchreiben(parteien) {
    var s = parteien.map(function (p) {
      return [p.name, p.stimmen[0], p.stimmen[1]].map(encodeURIComponent).join(',');
    }).join(';');
    history.replaceState(null, '', '#p=' + s);
  }
  function hashLesen() {
    var m = location.hash.match(/^#p=(.*)$/);
    if (!m) return null;
    return m[1].split(';').slice(0, ZEILEN).map(function (t) {
      var f = t.split(',').map(decodeURIComponent);
      return [f[0] || '', f[1] || '', f[2] || ''];
    });
  }

  function berechnen(fokus) {
    var eingabe = lesen();
    if (eingabe.fehler.length) {
      meldung(eingabe.fehler);
      ergebnisEl.hidden = true;
      return;
    }
    var e = P.berechne({ parteien: eingabe.parteien });
    e.parteien.forEach(function (p, i) { p.farbe = eingabe.parteien[i].farbe; });
    if (e.fehler) {
      meldung([e.fehler]);
      ergebnisEl.hidden = true;
      return;
    }
    meldung(e.hinweise, 'hinweis');
    document.getElementById('sitzbild').innerHTML = D.sitzbandErgebnis(e);
    document.getElementById('rechenweg').innerHTML = D.rechenweg(e);
    ergebnisEl.hidden = false;
    hashSchreiben(eingabe.parteien);
    if (fokus) {
      ergebnisEl.querySelector('h2').setAttribute('tabindex', '-1');
      ergebnisEl.querySelector('h2').focus();
    }
  }

  form.addEventListener('submit', function (ev) { ev.preventDefault(); berechnen(true); });
  tbody.addEventListener('input', function (ev) {
    if (ev.target.name === 'name') farbenAktualisieren();
    auswahl.value = '';
  });
  auswahl.addEventListener('change', function () {
    var b = BEISPIELE[auswahl.value];
    if (!b) return;
    setzen(b.parteien);
    berechnen(false);
  });
  document.getElementById('leeren').addEventListener('click', function () {
    setzen([]);
    auswahl.value = '';
    meldung([]);
    ergebnisEl.hidden = true;
    history.replaceState(null, '', location.pathname);
  });

  var ausLink = hashLesen();
  if (ausLink) { setzen(ausLink); berechnen(false); }
})();

/* Frühere Wahlen: Vergleich tatsächlich / Pukelsheim und Zufallswahl */
(function () {
  'use strict';
  var D = window.Darstellung, P = window.Pukelsheim, W = window.Wahlen;
  var jahre = document.getElementById('jahre');
  var vergleich = document.getElementById('vergleich');

  W.WAHLEN.forEach(function (w, i) {
    var label = document.createElement('label');
    label.innerHTML = '<input type="radio" name="wahl" value="' + w.id + '"' + (i === 0 ? ' checked' : '') + '><span>' + D.esc(w.titel) + '</span>';
    jahre.appendChild(label);
  });

  function aufzaehlung(liste) {
    if (liste.length <= 1) return liste.join('');
    return liste.slice(0, -1).join(', ') + ' und ' + liste[liste.length - 1];
  }

  function zeige(id) {
    var w = W.WAHLEN.filter(function (x) { return x.id === id; })[0] || W.WAHLEN[0];
    var e = P.berechne({ parteien: w.parteien.map(function (x) { return { name: x.p, stimmen: x.stimmen }; }) });
    e.parteien.forEach(function (p, i) { p.farbe = W.PARTEIEN[w.parteien[i].p].farbe; p.ist = w.parteien[i].ist; });

    var mehr = [], weniger = [];
    e.parteien.forEach(function (p) {
      var d = p.sitze - p.ist;
      if (d > 0) mehr.push(p.name + (d > 1 ? ' ' + d + ' Sitze' : ' einen Sitz') + ' mehr');
      if (d < 0) weniger.push(p.name + (d < -1 ? ' ' + (-d) + ' Sitze' : ' einen Sitz') + ' weniger');
    });
    var satz = mehr.length || weniger.length
      ? 'Mit dem doppelten Pukelsheim hätte ' + aufzaehlung(mehr.concat(weniger)) + ' erhalten.'
      : 'Der doppelte Pukelsheim hätte genau die gleiche Sitzverteilung ergeben.';

    var anteile = e.parteien.map(function (p) { return p.anteil; });
    var gIst = P.gallagher(anteile, e.parteien.map(function (p) { return p.ist; }));
    var gPuk = P.gallagher(anteile, e.parteien.map(function (p) { return p.sitze; }));

    var band = D.sitzband([
      { titel: 'Tatsächlich', notiz: 'heutiges Verfahren', sitze: e.parteien.map(function (p) { return { name: p.name, farbe: p.farbe, anzahl: p.ist }; }) },
      { titel: 'Mit dem doppelten Pukelsheim', notiz: '', sitze: e.parteien.map(function (p) { return { name: p.name, farbe: p.farbe, anzahl: p.sitze, wechsel: Math.max(0, p.sitze - p.ist) }; }) }
    ], e.parteien.filter(function (p) { return p.ist || p.sitze; }).map(function (p) { return { name: p.name, farbe: p.farbe }; })
      .concat(mehr.length ? [{ name: 'Sitz, der wechselt', wechsel: true }] : []));

    var tabelle = '<div class="tabelle-wrap" style="max-width:46rem"><table class="daten"><thead><tr><th scope="col">Partei</th>' +
      '<th class="z" scope="col">Wähleranteil</th><th class="z" scope="col">Sitze tatsächlich</th><th class="z" scope="col">Sitze mit Pukelsheim</th><th class="z" scope="col">Unterschied</th></tr></thead><tbody>';
    e.parteien.forEach(function (p) {
      var d = p.sitze - p.ist;
      tabelle += '<tr' + (d ? ' class="markiert"' : '') + '><th scope="row">' + D.punkt(p.farbe) + '<span class="nur-breit">' + D.esc(W.PARTEIEN[p.name].lang) + ' (</span>' + D.esc(p.name) + '<span class="nur-breit">)</span></th>' +
        '<td class="z">' + D.prozent(p.anteil, 1) + (p.ueberHuerde ? '' : ' <span class="leise">unter 8\u00a0%</span>') + '</td>' +
        '<td class="z">' + p.ist + '</td><td class="z">' + p.sitze + '</td>' +
        '<td class="z">' + (d > 0 ? '<span class="plus">+' + d + '</span>' : d < 0 ? '<span class="minus">−' + (-d) + '</span>' : '0') + '</td></tr>';
    });
    tabelle += '</tbody></table></div>';

    vergleich.innerHTML =
      '<h2>Landtagswahl ' + D.esc(w.titel) + '</h2>' +
      '<p class="lead text">' + D.esc(satz) + '</p>' +
      '<div style="max-width:46rem">' + band + '</div>' + tabelle +
      '<p class="kennzahl">Gallagher-Index: <strong>' + D.zahl(gIst, 2) + ' %</strong> tatsächlich, <strong>' + D.zahl(gPuk, 2) + ' %</strong> mit dem doppelten Pukelsheim. ' +
      '<span class="leise">Je tiefer, desto genauer entspricht die Sitzverteilung den Wähleranteilen.</span></p>' +
      '<p class="klein leise text">Wähleranteil: Parteistimmen Oberland ÷ 15 plus Unterland ÷ 10, also jede Wählerin und jeder Wähler einmal gezählt. Die tatsächlichen Sitze stammen aus den amtlichen Ergebnissen.</p>' +
      '<details class="rechenweg"><summary>Ganzer Rechenweg für ' + D.esc(w.titel) + '</summary>' +
      D.sitzbandErgebnis(e, 'Mit dem doppelten Pukelsheim, nach Wahlkreisen') + D.rechenweg(e) +
      '<p><a href="/rechner.html#p=' + w.parteien.map(function (x) { return [x.p, x.stimmen[0], x.stimmen[1]].join(','); }).join(';') + '">Diese Wahl im Rechner öffnen</a></p>' +
      '</details>';
  }

  jahre.addEventListener('change', function (ev) {
    if (ev.target.name !== 'wahl') return;
    history.replaceState(null, '', '#' + ev.target.value);
    zeige(ev.target.value);
  });
  var start = location.hash.slice(1);
  if (start) {
    var r = jahre.querySelector('input[value="' + CSS.escape(start) + '"]');
    if (r) r.checked = true;
  }
  zeige(jahre.querySelector('input:checked').value);

  // Zufallswahl
  var NAMEN = ['Partei A', 'Partei B', 'Partei C', 'Partei D', 'Partei E', 'Partei F'];
  var WAEHLER = [10263, 5908]; // Grössenordnung wie 2025
  document.getElementById('zufall').addEventListener('click', function () {
    var k = Number(document.getElementById('anzahl').value);
    var g = [];
    for (var i = 0; i < k; i++) g.push(Math.pow(Math.random(), 1.4) + 0.03);
    var sg = g.reduce(function (a, b) { return a + b; });
    var parteien = g.map(function (x, i) {
      var neigung = 0.75 + Math.random() * 0.5; // regionale Stärke
      return {
        name: NAMEN[i],
        farbe: D.FARBEN_FREI[i],
        stimmen: [15 * Math.round(WAEHLER[0] * x / sg * neigung), 10 * Math.round(WAEHLER[1] * x / sg * (2 - neigung))]
      };
    });
    var e = P.berechne({ parteien: parteien });
    var ziel = document.getElementById('zufall-ergebnis');
    if (e.fehler) { ziel.innerHTML = '<p class="meldung">' + D.esc(e.fehler) + '</p>'; return; }
    e.parteien.forEach(function (p, i) { p.farbe = parteien[i].farbe; });
    ziel.innerHTML = e.hinweise.map(function (h) { return '<p class="meldung meldung--hinweis">' + D.esc(h) + '</p>'; }).join('') +
      '<div style="max-width:46rem">' + D.sitzbandErgebnis(e) + '</div>' +
      '<details class="rechenweg" open><summary>Rechenweg</summary>' + D.rechenweg(e) +
      '<p><a href="/rechner.html#p=' + parteien.map(function (p) { return [p.name, p.stimmen[0], p.stimmen[1]].map(encodeURIComponent).join(','); }).join(';') + '">Diese Zufallswahl im Rechner öffnen</a></p></details>';
  });
})();

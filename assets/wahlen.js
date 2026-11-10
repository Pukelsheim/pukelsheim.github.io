/*
 * Amtliche Parteistimmen der Landtagswahlen (Oberland, Unterland)
 * und tatsächliche Mandatsverteilung nach geltendem Recht.
 *
 * Quellen: Amtliche Wahlergebnisse (landtagswahlen.li),
 * BuA Nr. 10/2024 und Nr. 109/2025 (Anhang Marxer).
 */
(function (root) {
  'use strict';

  var PARTEIEN = {
    FBP: { name: 'FBP', lang: 'Fortschrittliche Bürgerpartei', farbe: '#1f1f1f' },
    VU: { name: 'VU', lang: 'Vaterländische Union', farbe: '#c8102e' },
    FL: { name: 'FL', lang: 'Freie Liste', farbe: '#3a8c3f' },
    DU: { name: 'DU', lang: 'Die Unabhängigen', farbe: '#e08a00' },
    DpL: { name: 'DpL', lang: 'Demokraten pro Liechtenstein', farbe: '#1c6fb5' }
  };

  // stimmen: [Oberland, Unterland]; ist: tatsächlich erzielte Mandate
  var WAHLEN = [
    { id: '2025', titel: '2025', parteien: [
      { p: 'FBP', stimmen: [38352, 18631], ist: 7 },
      { p: 'VU', stimmen: [58725, 20753], ist: 10 },
      { p: 'FL', stimmen: [16928, 5621], ist: 2 },
      { p: 'DpL', stimmen: [35695, 12675], ist: 6 }
    ] },
    { id: '2021', titel: '2021', parteien: [
      { p: 'FBP', stimmen: [50844, 21475], ist: 10 },
      { p: 'VU', stimmen: [54018, 18343], ist: 10 },
      { p: 'FL', stimmen: [20026, 5917], ist: 3 },
      { p: 'DU', stimmen: [7370, 1186], ist: 0 },
      { p: 'DpL', stimmen: [14127, 8329], ist: 2 }
    ] },
    { id: '2017', titel: '2017', parteien: [
      { p: 'FBP', stimmen: [47747, 20914], ist: 9 },
      { p: 'VU', stimmen: [48789, 16995], ist: 8 },
      { p: 'FL', stimmen: [18882, 5715], ist: 3 },
      { p: 'DU', stimmen: [26452, 9449], ist: 5 }
    ] },
    { id: '2013', titel: '2013', parteien: [
      { p: 'FBP', stimmen: [55233, 22411], ist: 10 },
      { p: 'VU', stimmen: [48586, 16532], ist: 8 },
      { p: 'FL', stimmen: [16058, 5546], ist: 3 },
      { p: 'DU', stimmen: [20748, 8991], ist: 4 }
    ] },
    { id: '2009', titel: '2009', parteien: [
      { p: 'FBP', stimmen: [61033, 25918], ist: 11 },
      { p: 'VU', stimmen: [71469, 23750], ist: 13 },
      { p: 'FL', stimmen: [13733, 4102], ist: 1 }
    ] },
    { id: '2005', titel: '2005', parteien: [
      { p: 'FBP', stimmen: [66389, 28156], ist: 12 },
      { p: 'VU', stimmen: [55372, 18790], ist: 10 },
      { p: 'FL', stimmen: [20319, 4954], ist: 3 }
    ] },
    { id: '2001', titel: '2001', parteien: [
      { p: 'FBP', stimmen: [65878, 26326], ist: 13 },
      { p: 'VU', stimmen: [57816, 18586], ist: 11 },
      { p: 'FL', stimmen: [13106, 3078], ist: 1 }
    ] },
    { id: '1997', titel: '1997', parteien: [
      { p: 'FBP', stimmen: [47170, 18744], ist: 10 },
      { p: 'VU', stimmen: [63889, 18897], ist: 13 },
      { p: 'FL', stimmen: [14386, 5069], ist: 2 }
    ] },
    { id: '1993-10', titel: '1993 (Okt.)', parteien: [
      { p: 'FBP', stimmen: [47125, 17950], ist: 11 },
      { p: 'VU', stimmen: [60437, 18461], ist: 13 },
      { p: 'FL', stimmen: [10728, 2719], ist: 1 }
    ] },
    { id: '1993-02', titel: '1993 (Feb.)', parteien: [
      { p: 'FBP', stimmen: [50992, 20217], ist: 12 },
      { p: 'VU', stimmen: [56511, 16706], ist: 11 },
      { p: 'FL', stimmen: [13187, 3537], ist: 2 }
    ] }
  ];

  var api = { PARTEIEN: PARTEIEN, WAHLEN: WAHLEN };
  if (typeof module === 'object' && module.exports) module.exports = api;
  else root.Wahlen = api;
})(typeof window !== 'undefined' ? window : this);

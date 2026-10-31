/* ANCHOR Studio - interface languages (English, German, Arabic) for the desktop app.
   The app is written in English; this script shows it in German or Arabic by replacing the English texts the page displays
   (labels, buttons, messages, dialogs and the framework content) with their translations from lang_de.js / lang_ar.js.
   It never touches what you type, your drafts, your data, figures or reports (reports stay in English).
   Texts with numbers or names in them are matched by patterns ("Phase %s approved"). Arabic switches the page to right-to-left. */
(function () {
  'use strict';
  var L = window.ANCHOR_I18N || {}; var lang = 'en'; var dict = null, pats = [];
  var SKIP = { SCRIPT: 1, STYLE: 1, TEXTAREA: 1, CODE: 1, PRE: 1, NOSCRIPT: 1, IFRAME: 1, SVG: 1, svg: 1 };
  var ATTRS = ['placeholder', 'title', 'aria-label'];
  var ISO = 'AD AE AF AG AI AL AM AO AQ AR AS AT AU AW AX AZ BA BB BD BE BF BG BH BI BJ BL BM BN BO BQ BR BS BT BV BW BY BZ CA CC CD CF CG CH CI CK CL CM CN CO CR CU CV CW CX CY CZ DE DJ DK DM DO DZ EC EE EG EH ER ES ET FI FJ FK FM FO FR GA GB GD GE GF GG GH GI GL GM GN GP GQ GR GS GT GU GW GY HK HM HN HR HT HU ID IE IL IM IN IO IQ IR IS IT JE JM JO JP KE KG KH KI KM KN KP KR KW KY KZ LA LB LC LI LK LR LS LT LU LV LY MA MC MD ME MF MG MH MK ML MM MN MO MP MQ MR MS MT MU MV MW MX MY MZ NA NC NE NF NG NI NL NO NP NR NU NZ OM PA PE PF PG PH PK PL PM PN PR PS PT PW PY QA RE RO RS RU RW SA SB SC SD SE SG SH SI SJ SK SL SM SN SO SR SS ST SV SX SY SZ TC TD TF TG TH TJ TK TL TM TN TO TR TT TV TW TZ UA UG UM US UY UZ VA VC VE VG VI VN VU WF WS XK YE YT ZA ZM ZW'.split(' ');

  function esc(s) { return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); }
  function build(l) {
    dict = Object.assign({}, L[l] || {}); pats = [];
    // country names, in the chosen language (the region lists of "Before you start")
    try { var en = new Intl.DisplayNames(['en'], { type: 'region' }), to = new Intl.DisplayNames([l], { type: 'region' });
      ISO.forEach(function (c) { var a = en.of(c), b = to.of(c); if (a && b && !(a in dict)) dict[a] = b; }); } catch (e) { /* older browser */ }
    Object.keys(dict).forEach(function (k) {
      if (!/%[sd]|\{\w+\}/.test(k)) return;
      var names = []; var re = esc(k).replace(/%[sd]|\\\{(\w+)\\\}/g, function (m, n) { names.push(n || null); return '([\\s\\S]+?)'; });
      pats.push({ re: new RegExp('^' + re + '$'), names: names, to: dict[k], n: k.length });
    });
    pats.sort(function (a, b) { return b.n - a.n; });
  }
  function fill(p, m) {
    var i = 0, out = p.to;
    if (p.names.some(function (x) { return x; })) p.names.forEach(function (n, j) { out = out.split('{' + n + '}').join(m[j + 1]); });
    else out = out.replace(/%[sd]/g, function () { i++; return m[i] === undefined ? '' : m[i]; });
    return out;
  }
  function tr(s) {
    var t = s.trim(); if (!t || !/[A-Za-z]/.test(t)) return null;
    var lead = s.slice(0, s.indexOf(t)), tail = s.slice(s.indexOf(t) + t.length);
    var core = t.replace(/\s+/g, ' '), pre = '', post = '';
    var mm = /^((?:\d+\s*·\s*)|(?:[·•\-–]\s+))([\s\S]+)$/.exec(core); if (mm) { pre = mm[1]; core = mm[2]; }
    var hit = dict[core];
    if (hit === undefined) { var m2 = /^([\s\S]+?)([:.])$/.exec(core); if (m2 && dict[m2[1]] !== undefined) { hit = dict[m2[1]]; post = m2[2]; } }
    if (hit === undefined && core.length < 400) for (var i = 0; i < pats.length; i++) { var m = pats[i].re.exec(core); if (m) { hit = fill(pats[i], m); break; } }
    return hit === undefined ? null : lead + pre + hit + post + tail;
  }
  function skip(el) { for (var e = el; e && e !== document.body; e = e.parentElement) { if (SKIP[e.tagName] || e.isContentEditable || (e.classList && (e.classList.contains('no-i18n') || e.classList.contains('dataTables_wrapper') && !e.closest('thead')))) return true; } return false; }
  function doText(n) {
    var cur = n.nodeValue;
    if (n.__anchorOut !== undefined && cur !== n.__anchorOut) { n.__anchorEn = cur; }      // the app changed the text since: new English
    if (n.__anchorEn === undefined) n.__anchorEn = cur;
    var en = n.__anchorEn; var want = lang === 'en' ? en : (tr(en) || en);
    if (cur !== want) { n.__anchorOut = want; n.nodeValue = want; } else n.__anchorOut = want;
  }
  function doAttrs(el) {
    ATTRS.forEach(function (a) { if (!el.hasAttribute(a)) return; var cur = el.getAttribute(a); var key = '__anchor_' + a;
      if (el[key + 'Out'] !== undefined && cur !== el[key + 'Out']) el[key] = cur; if (el[key] === undefined) el[key] = cur;
      var want = lang === 'en' ? el[key] : (tr(el[key]) || el[key]); el[key + 'Out'] = want; if (cur !== want) el.setAttribute(a, want); });
  }
  function walk(root) {
    if (!root) return;
    if (root.nodeType === 3) { if (!skip(root.parentElement)) doText(root); return; }
    if (root.nodeType !== 1 || skip(root)) return;
    doAttrs(root);
    var w = document.createTreeWalker(root, NodeFilter.SHOW_TEXT | NodeFilter.SHOW_ELEMENT, { acceptNode: function (n) { return n.nodeType === 1 && SKIP[n.tagName] ? NodeFilter.FILTER_REJECT : NodeFilter.FILTER_ACCEPT; } });
    var n; while ((n = w.nextNode())) { if (n.nodeType === 1) { if (!(n.classList && n.classList.contains('no-i18n'))) doAttrs(n); } else if (!skip(n.parentElement)) doText(n); }
  }
  var queue = [], busy = false, obs = null;
  function flush() { busy = false; var q = queue; queue = []; if (obs) obs.disconnect(); q.forEach(walk); observe(); }
  function observe() { if (!obs) obs = new MutationObserver(function (ms) { ms.forEach(function (m) {
      if (m.type === 'characterData') queue.push(m.target); else if (m.type === 'attributes') queue.push(m.target); else m.addedNodes.forEach(function (x) { queue.push(x); }); });
      if (!busy) { busy = true; (window.requestAnimationFrame || setTimeout)(flush); } });
    obs.observe(document.body, { childList: true, subtree: true, characterData: true, attributes: true, attributeFilter: ATTRS }); }

  window.anchorSetLang = function (l, fromServer) {
    lang = (l === 'de' || l === 'ar') ? l : 'en'; if (lang !== 'en') build(lang);
    var h = document.documentElement; h.lang = lang; h.dir = lang === 'ar' ? 'rtl' : 'ltr';
    try { localStorage.setItem('anchor.lang', lang); } catch (e) { /* ignore */ }
    if (obs) obs.disconnect(); walk(document.body); observe();
    document.querySelectorAll('.anchor-lang-item').forEach(function (a) { a.classList.toggle('active', a.getAttribute('data-lang') === lang); });
    if (!fromServer && window.Shiny && Shiny.setInputValue) Shiny.setInputValue('ui_lang', lang, { priority: 'event' });
  };
  window.anchorT = function (s) { return lang === 'en' ? s : (tr(s) || s); };
  function start() {
    var l = 'en'; try { l = localStorage.getItem('anchor.lang') || 'en'; } catch (e) { /* ignore */ }
    anchorSetLang(l, true);
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('anchor-lang', function (x) { if (x && x !== lang) anchorSetLang(x, true); if (window.Shiny.setInputValue) Shiny.setInputValue('ui_lang', lang); });
    document.addEventListener('shiny:connected', function () { if (window.Shiny.setInputValue) Shiny.setInputValue('ui_lang', lang); });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
})();

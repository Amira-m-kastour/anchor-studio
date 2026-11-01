ANCHOR Studio 1.7.0 for Windows
by A. Mamdouh Kastour · A. Mamdouh Kastour’s Scholarly Anchor
================================

Start ANCHOR Studio by double-clicking "ANCHOR Studio.exe" (or use the Start menu entry
if you installed it). Everything it needs is in this folder: R 4.3, bibliometrix and all
other packages. Nothing else has to be installed.

Internet: the app works fully offline. When online (on by default; Settings > Internet
features), it checks the references at each stage against Crossref, flags retracted or
corrected articles (your focal article and your dataset too), and suggests newer reading
from OpenAlex. Only DOIs and short search terms are sent - never the article text, drafts,
notes or keys. The optional Claude API drafting mode also needs the internet.

The app opens in its own window (Microsoft Edge app mode, or your default browser) after
a short "Starting..." splash. It stops by itself about 20 seconds after you close that
window. Your work is autosaved every minute; on the next start, the Start tab offers
"Resume last session".

If something goes wrong, the log is in:
  %LOCALAPPDATA%\ANCHOR Studio\anchor.log
If the app does not start, run "ANCHOR Studio - troubleshoot" from the Start menu (or the .cmd file in
this folder): it starts the app in a window that shows every message, so you can copy them.

Each stage has a "Lessons learned & what's in it for me" card with key references.

Exports (Report & disclosure tab): HTML, Word and PDF reports, a LaTeX package for Overleaf,
a journal template or a local TeX install (.zip), the references as BibTeX (.bib), the AI-use
disclosure and logs. Each analysis saves its figure as PNG or vector PDF and its table as CSV.
The PDF report is printed with Microsoft Edge (or Chrome); the built-in Typst engine is used
when neither is available, or when you choose it under Settings.

Portable use: keep the whole folder together (ANCHOR Studio.exe, app, runtime) in a place
you can write to, for example Documents or a USB stick. Do not run it from inside the zip.

In tribute to my father, Mamdouh Kastour (1950–2020).

Author: Amira Mamdouh Kastour. Licence: GPL-3 (LICENSE). Third-party software:
NOTICE.md and THIRD-PARTY-LICENSES.txt. Bibliometrics: bibliometrix (Aria & Cuccurullo, 2017).

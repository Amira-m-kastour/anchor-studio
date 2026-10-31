# ANCHOR Studio

*by A. Mamdouh Kastour · A. Mamdouh Kastour’s Scholarly Anchor*

ANCHOR Studio is a desktop app for article-anchored literature reviews. It takes you through the ANCHOR framework
(Anchor, Navigate, Code, Hypothesise, Operationalise, Report) with a decision gate after each phase.
It includes the bibliometric analyses of [bibliometrix](https://www.bibliometrix.org) (Aria & Cuccurullo, 2017),
the engine behind Biblioshiny: 39 analyses covering its main menus, plus a button that opens the full Biblioshiny app.

Exports: HTML, Word and PDF reports; a LaTeX package (.zip, for Overleaf, a journal template or a local TeX install);
references as BibTeX; the AI-use disclosure, prompt log and decision log; figures as PNG or vector PDF and tables as CSV.
The PDF report is printed with Microsoft Edge or Google Chrome when available, otherwise with the bundled Typst engine.

Each of the six stages has a "Lessons learned & what's in it for me" card with checked references. When the computer is online
(on by default, Settings > Internet features), the app verifies those references and your focal article at Crossref, flags retractions
and corrections (also across your bibliometric dataset, via OpenAlex), and suggests newer reading from OpenAlex. Only DOIs and short search
terms are sent; offline, the built-in references are used.

New in 1.4.0: a "Data & methods" step in phase O (Operationalise). Import your own data (WRDS, LSEG, Bloomberg or MSCI exports,
or a coding sheet), or fetch open data from the World Bank, FRED, the Kenneth R. French library, Yahoo Finance (teaching only) and
GDELT. Run descriptive statistics, panel regressions with fixed effects and clustered errors (fixest), event studies, factor-model
alphas with Newey-West errors, meta-analysis (metafor), difference-in-differences, multilevel models and SEM, with ready templates
for the focal article's equations. Every result can go into the report with the R code that reproduces it, and the analysis package
(.zip) holds the script, the open data and a provenance file with SHA-256 checks. Licensed data stay out of project files and packages
unless you include them.

New in 1.4.0: a **Before you start** page (first tab) records the researcher, software, academic affiliation and data licences. It suggests
your region from the computer's time zone, lists the institutions of that region from ROR (or detects yours from its email domain),
pre-ticks the databases your library is known to license, checks the software, and prints everything as an appendix in every report.
Ten free connectors were added: an OpenAlex corpus for the bibliometric analyses (search, citing works, references); the focal
article's references, citing works, open copy and journal facts (phase A); open copies for screening (phase C); a research-front trend
(phase H); SEC EDGAR fundamentals and SIC codes and IMF series (phase O); ORCID and ROR identifiers. Phase O also builds consensus and
disagreement from two or more rating columns and offers three routes when only one ESG rater is available.

Fixed in 1.4.1 to 1.4.3: on some Windows PCs the app stopped at start-up with "sass.dll ... The specified procedure could not be
found" (components built with GCC 13 asked the bundled C++ runtime for a routine it no longer exports; the build now checks that
every import of every shipped DLL resolves); the LaTeX package and references.bib downloads failed in the installed app; figure
download buttons are hidden for interactive figures and tables.

Changed in 1.4.4: the Before you start page works the same for any university. Region comes from the computer's time zone; the
institution list for that region uses ROR's documented query, large countries show a starter list plus a search over all their
institutions, email domains are matched exactly, a library link prefix opens paywalled articles through your own library, and a
database list you save for your institution pre-ticks it in new projects.

Fixed in 1.4.5: on Windows the app started but its window never opened when Microsoft Edge is installed in "Program Files (x86)"
(the path was quoted twice). The launcher now also reports any start that ends before the window opens, with the last lines of the
log, and a "ANCHOR Studio - troubleshoot" Start-menu entry runs the app with all its messages visible. The installer closes a
running copy before replacing it.

New in 1.5.3: after "Check records for retractions" (Bibliometrics > Data), the button **Exclude the retracted records**
removes them from every analysis; the filters cannot bring them back. They appear as "Retracted records removed (retraction check)"
in the PRISMA flow, the decision is logged with their DOIs, and the reports list them as excluded. A new import starts afresh.
Fixed in 1.5.3: vector PDF figures on a Mac without XQuartz (the standard PDF device is used when cairo cannot load).

New in 1.5.2: the app speaks **German and Arabic** as well as English. Choose the language from "Language" (globe) in the top bar;
Arabic is shown right to left, and the choice is remembered. Names of people stay in Latin script, and reports, figures and tables
stay in English. Under Settings, "Language of AI drafts" asks the model to write its drafts in English, German or Arabic (by default
the app's language). Fixed in 1.5.2: projects saved on the phone (ANCHOR Mobile) open ready for analysis, without pressing
"Apply filters" first; records without countries or affiliations no longer stop the bibliometric summary; and a negative annual
growth rate is shown with its minus sign.

Fixed in 1.5.1: saved projects are easy to open again: "Open project" in the top bar and on the Start page (with the last
autosave offered too), and on Windows a double-clicked .anchor file opens in ANCHOR Studio. A clear message explains a file that is
not a project. The tribute to Mamdouh Kastour (1950–2020) now has its own "About" entry in the top bar instead of sitting at the
bottom of Settings. "Open full Biblioshiny" works again in the Windows app: it stopped with "trying to
use CRAN without setting a mirror" because 14 packages Biblioshiny loads at start were not bundled. They are now included, and
Biblioshiny never tries to install packages in the bundled app.

New in 1.5.0: a **Find a focal article** page before the Start page. Search OpenAlex for a subject with filters (years, type,
language, open access only, minimum citations) and four sort orders; retracted articles are flagged. Shortlist up to ten
candidates; legal open copies download into a private folder on this computer (never into project files, reports or shared
packages), and paywalled articles open through your library link or the DOI, with an upload slot for the PDF you obtain.
A comparison shows citations, citations per year, field-weighted impact, journal facts, open-copy version, your own ticks and a
fit score (a guide only), with an optional AI summary of each abstract in your drafting mode. "Use as focal article" fills the
Start page and logs the choice with the alternatives; the report explains how the focal article was chosen.
On the Start page the DOI is now looked up automatically (after a PDF upload or when a DOI is typed), the reference can be
given in seven styles (APA, Harvard, Chicago author-date, IEEE, Vancouver, MLA, Nature) through doi.org, and a reference you
edited yourself is never overwritten: the looked-up version is offered beside it instead.

Fixed in 1.4.6 (Before you start): opening a project now shows its ticked databases again (before, the boxes stayed empty and
the next click could overwrite the saved list); the region is no longer guessed as "United States" from the app's own UTF-8
setting (it now uses the time zone including older zone names, the Windows region setting, then the start-up language);
LSEG Workspace is detected only from its own status answer, not from any program on port 9000; ORCID records with a private
name or no public record are reported clearly; the software checks update when settings change; a library link is checked
and previewed.

Author: Amira Mamdouh Kastour. Developed with Claude (Anthropic).

## Download

Open **Releases** (or the latest run under **Actions**) and download the file for your computer:

| Computer | File |
|---|---|
| Windows 10/11 | `ANCHOR-Studio-<version>-Windows-Setup.exe` (installer, no admin rights) or `...-Windows-Portable.zip` |
| Mac with Apple silicon (M1–M4) | `ANCHOR-Studio-<version>-macOS-arm64.dmg` |
| Mac with Intel processor | `ANCHOR-Studio-<version>-macOS-x86_64.dmg` |

The Windows version includes R and every package, so nothing else needs to be installed.
The Mac version includes the R installer and every package. On first start, it installs R once if R is missing.
The apps are free and unsigned:

- **Windows:** at the SmartScreen warning, click *More info* and then *Run anyway*.
- **macOS 15 or newer:** open System Settings > Privacy & Security and click *Open Anyway*.
- **macOS 14 or older:** right-click the app and choose *Open*.

## Build the apps yourself

The apps are built by GitHub Actions on GitHub's own Windows and Mac machines (`.github/workflows/build.yml`):

1. **Actions** > *Build ANCHOR Studio apps* > **Run workflow**. This takes about 30–60 minutes.
2. Download the three artifacts at the bottom of the run page. Alternatively, create a release with a tag such as `v1.0.0`, and the files are attached to it.

| Folder | Contents |
|---|---|
| `app/` | The Shiny app (`app.R`, `R/`, `www/`) and the standalone launcher `launch.R` |
| `build/common/` | Package list, package installer, smoke test, icons |
| `build/windows/` | `build.ps1` (R + packages + test + Inno Setup installer + portable zip) |
| `build/macos/` | `build.sh` (R installer + packages + test + .app + .dmg), app launcher, Info.plist |

To pin the R version, set `R_VERSION` (Windows, for example `4.5.1`) or `R_PKG_URL` (macOS) in the workflow.
Never commit article PDFs to this repository; `*.pdf` is ignored on purpose.

## Build the Windows version on Linux (no Windows or GitHub needed)

`build/windows-from-linux/build.sh` assembles R 4.3 and every package from conda-forge's Windows builds,
compiles the `ANCHOR Studio.exe` launcher with mingw-w64, and packs a 64-bit NSIS installer plus a portable zip
into `dist-win/`. It needs `curl`, `zip`, `mingw-w64` and `nsis` (Ubuntu: `apt install mingw-w64 nsis zip`);
with `TEST=1` and `wine64` + `xvfb` installed it also runs the smoke test on the Windows build.
`packages-win64.lock.txt` lists the exact package versions of release 1.5.3.
Set `TYPST_VERSION` to change the bundled Typst version (default 0.15.1) in the GitHub builds.

## Run from source (any system with R)

```r
install.packages(readLines("build/common/packages.txt")[-1])
shiny::runApp("app")
```

## Cite

Kastour, A. M. (2026). *ANCHOR Studio* (Version 1.5.3) [Computer software].
Aria, M., & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive science mapping analysis. *Journal of Informetrics, 11*(4), 959–975. https://doi.org/10.1016/j.joi.2017.08.007

## Dedication

<img src="app/www/tribute.png" alt="Portrait of Mamdouh Kastour" width="120" align="left" style="margin-right:16px">

*In tribute to my father,*
**Mamdouh Kastour** (1950–2020)

Photo: Kastour family. All rights reserved; the portrait is not covered by the GPL-3 licence (see `NOTICE.md`).
<br clear="left">

## Licence

GPL-3 (see `LICENSE`). Third-party software and source availability: `NOTICE.md`.

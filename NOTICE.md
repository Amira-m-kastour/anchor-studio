# ANCHOR Studio: licence and notices

**ANCHOR Studio** by A. Mamdouh Kastour (*A. Mamdouh Kastour’s Scholarly Anchor*) © 2026 Amira Mamdouh Kastour. Developed with Claude (Anthropic).
ANCHOR Studio is free software: you can redistribute it and/or modify it under the terms of the
GNU General Public License, version 3 or (at your option) any later version (see `LICENSE`).
It is distributed WITHOUT ANY WARRANTY.

## Third-party software

ANCHOR Studio runs on R and R packages from CRAN. The standalone installers include them unmodified:

- **R**: The R Foundation for Statistical Computing; GPL-2 | GPL-3. https://www.r-project.org
- **bibliometrix**: Massimo Aria and Corrado Cuccurullo; GPL-3. https://www.bibliometrix.org
  Aria, M., & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive science mapping analysis.
  *Journal of Informetrics, 11*(4), 959-975. https://doi.org/10.1016/j.joi.2017.08.007
- **shiny, bslib** (Posit; GPL-3 / MIT), **ggplot2** (MIT), **igraph** (GPL-2 | GPL-3), **pdftools**
  (MIT; uses Poppler, GPL-2 | GPL-3), **officer** (MIT), **plotly** (MIT), and their dependencies.
- **Typst** (Apache-2.0; https://github.com/typst/typst), bundled unmodified as the offline engine for the
  PDF report. Its own embedded fonts (Libertinus, New Computer Modern, DejaVu Sans Mono) are under the
  SIL Open Font License and free licences.
- The LaTeX package export includes Liberation Sans (SIL Open Font License 1.1) as a fallback font.

Each installer contains `THIRD-PARTY-LICENSES.txt`, generated at build time, which lists every bundled
package with its version and licence. Full licence texts are in the bundled R folder
(`R/share/licenses`, and each package's own folder).

## Online services

When internet features are on, the app queries the Crossref REST API (https://www.crossref.org; metadata CC0, retraction data
from Retraction Watch via Crossref) and the OpenAlex API (https://openalex.org; data CC0). Only DOIs and short search terms are
sent. These services are not affiliated with ANCHOR Studio; their terms of use apply.

## Open data sources (Data & methods, phase O)

On request only, the app downloads open data from: the World Bank Indicators API (https://data.worldbank.org; CC BY 4.0, cite
the World Bank); FRED, Federal Reserve Bank of St. Louis (https://fred.stlouisfed.org; free API key; some series carry their
owners' copyright, check each series' notes); the Kenneth R. French Data Library (Dartmouth; free for research, cite the
library); Yahoo Finance chart data (personal and teaching use only under Yahoo's terms; not for publication); and the GDELT
DOC 2.0 API (https://www.gdeltproject.org; free, cite GDELT; last three months only). Each dataset keeps its source, query,
time and SHA-256 in provenance.csv. Data you import yourself (for example WRDS, LSEG/Refinitiv, Bloomberg, MSCI or Sustainalytics
exports) stay under your licence: they are kept out of project files and analysis packages unless you choose to include them.

Statistics use fixest (GPL-3), sandwich (GPL-2|3), lmtest (GPL-2|3), metafor (GPL-2|3), nlme (GPL-2|3) and lavaan (GPL-2|3).

## Connectors added in 1.4.0

On request only: OpenAlex (CC0) for corpora, references, citing works, open-access locations, journal facts and topic trends
(cite Priem et al., 2022); SEC EDGAR company facts and submissions (public US filings; a contact email is sent in the request header
as the SEC requires; at most 10 requests per second); the IMF Data API (IMF terms, attribution); the Research Organization Registry
(ROR, CC0) for affiliations; the ORCID public API (read-only, non-commercial use) for researcher identifiers. "Detect from my network"
asks Cloudflare's public trace page for the country of your connection; by default the region comes from your computer's time zone
without any request. The UB Hagen database list used to pre-tick databases is a summary of the library's flyer; you confirm each one.

## Tribute portrait

The portrait of Mamdouh Kastour (1950–2020), shown in tribute in the app's About panel, the Windows installer and the
documentation (`app/www/tribute.png` and the installer side image), is © the Kastour family. All rights reserved. It is **not**
covered by the GPL-3 licence of ANCHOR Studio: it may not be copied, modified or reused outside unmodified copies of ANCHOR Studio.
Anyone redistributing a modified version must remove or replace it.

## Source code

The source code of ANCHOR Studio is in this repository. The source code of R and of every bundled R
package, in the exact versions included, is available from CRAN (https://cran.r-project.org, including
its Archive). On request, the author will also provide a copy of that source code for three years
after the release of the installer.

## Icons and fonts

The app icon was drawn for ANCHOR Studio using DejaVu Serif (free licence). The interface loads the
IBM Plex Sans and Source Serif 4 fonts (SIL Open Font License) from Google Fonts, and Font Awesome
icons (CC BY 4.0 / SIL OFL) bundled with shiny.

## Added in 1.5.0

Find a focal article: searches OpenAlex (metadata CC0). Open copies are downloaded only from the copy's own legal location
(publisher open access, repository or preprint server) and keep their own licence and copyright; ANCHOR Studio stores them
in a private folder on your computer for your own reading and never puts them in project files, reports or shared packages.
Paywalled articles are opened in your browser through your library link or the DOI; the app never automates library or
publisher sites. Citation styles: the reference text is requested from doi.org by content negotiation (CSL styles from the
Citation Style Language project, CC BY-SA 3.0); the metadata comes from Crossref or DataCite.

## Added in 1.5.2

Interface languages: the German and Arabic texts were translated for ANCHOR Studio (machine-assisted, checked in the app) and are
released under the GPL-3 with the rest of the app. Country names in the region lists come from the browser's own
internationalisation data (Unicode CLDR, Unicode licence). The interface translates only what the app displays; your texts,
drafts, data, figures and reports are never changed.

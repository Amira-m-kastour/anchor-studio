# ANCHOR Studio - "Before you start": researcher, software, academic affiliation and licensing prerequisites.
# Saved in the project (never passwords, keys or email addresses), checked where possible, and printed as an
# appendix in every report. Licence summaries are short and link to the source; they are not legal advice.

DB_CATALOG <- data.frame(stringsAsFactors = FALSE, rbind(
  c("wos", "Web of Science Core Collection", "Literature", "Licensed (Clarivate)", "Export plain text with cited references; import under Bibliometrics > Data", ""),
  c("scopus", "Scopus", "Literature", "Licensed (Elsevier)", "Export CSV or BibTeX with all fields; import under Bibliometrics > Data", ""),
  c("econlit", "EconLit with Full Text", "Literature", "Licensed (AEA via EBSCO)", "Search and screening; export references", ""),
  c("bsu", "Business Source Ultimate", "Literature", "Licensed (EBSCO)", "Search and screening; full texts", ""),
  c("wiso", "wiso Wirtschaftswissenschaften", "Literature", "Licensed (GBI-Genios)", "German-language literature", ""),
  c("emerald", "Emerald Premier", "Literature", "Licensed (Emerald)", "Full texts", ""),
  c("statista", "Statista", "Statistics", "Licensed (Statista)", "Descriptive statistics for context; check export terms", ""),
  c("oecd", "OECD iLibrary / OECD data", "Statistics", "OECD terms", "Macro and policy context", ""),
  c("lseg_ws", "LSEG Workspace for Students (Datastream, Eikon)", "Market data, fundamentals, ESG scores", "Licensed (LSEG)", "Excel add-in or screener exports; import in phase O", "LSEG"),
  c("wrds_crsp", "CRSP / Compustat via WRDS", "Market data, fundamentals", "Licensed (WRDS)", "WRDS web query exports; import in phase O", ""),
  c("wrds_msci", "MSCI ESG ratings via WRDS", "ESG ratings", "Licensed (MSCI, WRDS)", "WRDS exports; import in phase O", "MSCI"),
  c("wrds_sust", "Sustainalytics ESG ratings via WRDS", "ESG ratings", "Licensed (Morningstar Sustainalytics, WRDS)", "WRDS exports; import in phase O", "Sustainalytics"),
  c("wrds_lseg", "Refinitiv / LSEG ESG via WRDS", "ESG ratings", "Licensed (LSEG, WRDS)", "WRDS exports; import in phase O", "LSEG"),
  c("sp_esg", "S&P Global ESG scores", "ESG ratings", "Licensed (S&P Global)", "Exports; import in phase O", "S&P Global"),
  c("factset_tvl", "FactSet Truvalue Labs ESG news (the article's news score)", "ESG news", "Licensed (FactSet)", "Exports; import in phase O", ""),
  c("bloomberg", "Bloomberg Terminal (ESG scores, disclosure score)", "ESG ratings, disclosure", "Licensed (Bloomberg)", "Terminal exports; import in phase O", "Bloomberg"),
  c("hand_rater", "Hand-collected second ESG rating (public rating pages)", "ESG ratings", "Each website's terms; personal collection", "Coding sheet; import in phase O", "Hand-collected")))
names(DB_CATALOG) <- c("id", "name", "kind", "licence", "route", "rater")

# what an institution's library is known to license (prefill only; the researcher confirms)
INSTITUTION_DBS <- list(
  list(domains = c("fernuni-hagen.de"), name_pattern = "fernuniversit", source = "UB Hagen, 'Datenbanken an der UB Hagen: Wirtschaftswissenschaft' (flyer, checked 26 Oct 2026)",
       dbs = c("wos", "scopus", "lseg_ws", "econlit", "bsu", "wiso", "emerald", "statista", "oecd"),
       access = "University network via VPN (Cisco AnyConnect) or proxy; for your own studies and academic work only; no systematic or robot downloads."))
# a list saved by the researcher on this computer wins over the built-in examples
inst_db_file <- function() file.path(user_dir(), "institution_databases.rds")
save_institution_dbs <- function(name, ror_id, domains, dbs, access = "") {
  reg <- if (file.exists(inst_db_file())) tryCatch(readRDS(inst_db_file()), error = function(e) list()) else list()
  key <- if (nzchar(ror_id %||% "")) ror_id else tolower(trimws(name))
  reg[[key]] <- list(name = name, ror_id = ror_id %||% "", domains = domains %||% "", dbs = dbs, access = access %||% "", saved = format(Sys.time(), "%Y-%m-%d"))
  saveRDS(reg, inst_db_file()); invisible(TRUE) }
institution_dbs <- function(name = "", domains = "", ror_id = "") {
  reg <- if (file.exists(inst_db_file())) tryCatch(readRDS(inst_db_file()), error = function(e) list()) else list()
  get <- function(k) if (nzchar(k %||% "") && k %in% names(reg)) reg[[k]] else NULL
  hit <- get(ror_id) %||% get(tolower(trimws(name %||% "")))
  if (!is.null(hit)) return(list(dbs = hit$dbs, source = sprintf("your saved list on this computer (%s)", hit$saved), access = hit$access))
  for (r in INSTITUTION_DBS) if (any(vapply(r$domains, function(d) nzchar(domains %||% "") && grepl(d, domains, fixed = TRUE), TRUE)) || (nzchar(name %||% "") && grepl(r$name_pattern, tolower(name), fixed = TRUE))) return(r)
  NULL }

PREREQ_SKILLS <- c("Reading a regression table (coefficients, standard errors, significance)" = "reg", "Panel data and fixed effects" = "fe",
                   "Event studies and abnormal returns" = "event", "Meta-analysis basics (effect sizes, heterogeneity)" = "meta",
                   "Bibliometric analysis (co-citation, coupling, keywords)" = "biblio", "Systematic review reporting (PRISMA 2020)" = "prisma",
                   "R basics, to rerun the exported scripts (optional)" = "r")
PREREQ_RULES <- c("Good scientific practice code of my university" = "gsp", "Rules on generative AI in assessed work" = "ai",
                  "Research data management and storage rules" = "rdm", "Licence terms of each database I use" = "lic")
OUTPUT_LICENCES <- c("All rights reserved (default for assessed work)" = "arr", "CC BY 4.0" = "cc-by", "CC BY-NC 4.0" = "cc-by-nc", "CC BY-SA 4.0" = "cc-by-sa")
CODE_LICENCES <- c("Same as the report" = "same", "MIT" = "mit", "GPL-3" = "gpl3", "CC0 (public domain)" = "cc0")
ETHICS <- c("Not needed: secondary data, no human participants (confirm with my faculty)" = "none", "Applied for" = "applied", "Approved" = "approved", "Not sure yet" = "unsure")

new_prereq <- function() list(name = "", orcid = "", orcid_name = "", role = "", programme = "", supervisor = "", skills = character(0), timeplan = "",
  country = "", country_how = "", inst_country = "", ror_id = "", institution = "", resolver = "", faculty = "", domains = "", access_route = "", dbs = character(0), db_source = "",
  rules = character(0), rule_links = "", ethics = "", output_licence = "arr", code_licence = "same", tools = character(0), checked_at = "")

# raters available to the researcher (distinct rating families)
raters_available <- function(pr) { r <- DB_CATALOG$rater[DB_CATALOG$id %in% (pr$dbs %||% character(0))]; unique(r[nzchar(r)]) }

pkg_ver <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed")
software_checks <- function(settings = list(), online_ok = NA) {
  br <- tryCatch(find_browser(), error = function(e) NULL); ty <- tryCatch(find_typst(), error = function(e) NULL)
  yes <- function(b) if (isTRUE(b)) "ok" else "missing"
  rows <- list(
    c("ANCHOR Studio", "ok", paste(APP_BRAND, APP_VERSION)),
    c("R", "ok", paste(R.version$major, R.version$minor, sep = ".")),
    c("Bibliometrics: bibliometrix, openalexR", "ok", sprintf("bibliometrix %s; openalexR %s", pkg_ver("bibliometrix"), pkg_ver("openalexR"))),
    c("Statistics: fixest, sandwich, lmtest, metafor, nlme, lavaan", if (any(vapply(c("fixest", "metafor", "lavaan"), pkg_ver, "") == "not installed")) "missing" else "ok",
      paste(sprintf("%s %s", c("fixest", "sandwich", "lmtest", "metafor", "nlme", "lavaan"), vapply(c("fixest", "sandwich", "lmtest", "metafor", "nlme", "lavaan"), pkg_ver, "")), collapse = "; ")),
    c("PDF engine", if (!is.null(br) || !is.null(ty)) "ok" else "missing", paste(c(if (!is.null(br)) paste("browser:", basename(br)), if (!is.null(ty)) "Typst (bundled)"), collapse = "; ")),
    c("Internet features", if (isFALSE(settings$online)) "off" else if (isTRUE(online_ok)) "ok" else if (isFALSE(online_ok)) "offline" else "not checked",
      if (isFALSE(settings$online)) "switched off in Settings" else "Crossref and OpenAlex"),
    c("OpenAlex API key (optional)", if (nzchar(settings$openalex_key %||% "")) "ok" else "optional", "free key raises the daily allowance from $0.10 to $1"),
    c("FRED API key (optional)", if (nzchar(settings$fred_key %||% "")) "ok" else "optional", "needed only for FRED series"),
    c("Contact email (optional)", if (nzchar(settings$crossref_mailto %||% "")) "ok" else "optional", "faster Crossref access; required by SEC EDGAR"),
    c("AI drafting mode", "ok", sprintf("%s%s", settings$mode %||% "prompt", if (identical(settings$mode, "api")) paste0(" (", settings$api_model %||% "", ")") else "")))
  d <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE); names(d) <- c("Item", "Status", "Detail"); d }

# ---- tables for the report appendix
prereq_tables <- function(st) {
  pr <- utils::modifyList(new_prereq(), st$prereq %||% list())
  kv <- function(k, v) data.frame(Item = k, Value = vapply(v, function(x) if (is.null(x) || !length(x) || all(!nzchar(x))) "-" else paste(x, collapse = "; "), ""), stringsAsFactors = FALSE)
  out <- list()
  out$Researcher <- kv(c("Name", "ORCID iD", "Role and programme", "Supervisor or reviewer", "Skills prepared", "Time plan"),
    list(pr$name %|e|% (st$setup$S_name %||% ""), if (nzchar(pr$orcid)) paste0("https://orcid.org/", pr$orcid, if (nzchar(pr$orcid_name)) paste0(" (", pr$orcid_name, ")") else "") else "",
         paste(c(pr$role, pr$programme)[nzchar(c(pr$role, pr$programme))], collapse = ", "), pr$supervisor,
         names(PREREQ_SKILLS)[PREREQ_SKILLS %in% pr$skills], pr$timeplan))
  out$`Academic affiliation` <- kv(c("Institution", "ROR identifier", "Faculty or department", "Institution's country", "Region chosen on this page", "Library access route", "Library link for articles", "Institutional rules read", "Ethics"),
    list(pr$institution %|e|% (st$setup$S_inst %||% ""), pr$ror_id, pr$faculty, unname(COUNTRIES[pr$inst_country %|e|% pr$country]) %||% "", if (nzchar(pr$country %||% "")) paste0(unname(COUNTRIES[pr$country]), if (nzchar(pr$country_how %||% "")) paste0(" (", pr$country_how, ")") else "") else "", pr$access_route, pr$resolver,
         names(PREREQ_RULES)[PREREQ_RULES %in% pr$rules], names(ETHICS)[ETHICS == pr$ethics]))
  cat_rows <- DB_CATALOG[DB_CATALOG$id %in% pr$dbs, ]
  out$`Databases available` <- if (nrow(cat_rows)) data.frame(Database = cat_rows$name, Kind = cat_rows$kind, Licence = cat_rows$licence, Route = cat_rows$route, stringsAsFactors = FALSE) else
    data.frame(Database = "None recorded", Kind = "-", Licence = "-", Route = "-")
  ra <- raters_available(pr)
  out$`Databases available` <- rbind(out$`Databases available`, data.frame(Database = "ESG rating families available", Kind = "ESG ratings", Licence = "-",
    Route = if (length(ra)) paste(ra, collapse = ", ") else "none recorded"))
  ds <- st$analysis$datasets %||% list()
  lic_row <- function(d) { s <- tolower(d$source %||% "")
    lic <- if (isTRUE(d$licensed)) "Licensed: not shared" else if (grepl("world bank", s)) "CC BY 4.0" else if (grepl("fred", s)) "FRED terms; series notes" else if (grepl("french", s)) "Free for research; cite the library" else
      if (grepl("yahoo", s)) "Personal use only" else if (grepl("gdelt", s)) "Free; cite GDELT" else if (grepl("sec edgar", s)) "Public US filings" else if (grepl("imf", s)) "IMF terms; attribution" else "Your own data"
    data.frame(Item = d$name, Value = paste0(d$source, " \u2014 ", lic), stringsAsFactors = FALSE) }
  out$Licensing <- rbind(
    kv(c("ANCHOR Studio", "Literature metadata", "Report licence", "Code licence"),
       list("GPL-3 (bundled software under their own licences, see NOTICE)",
            "Crossref and OpenAlex metadata (CC0); ROR (CC0); ORCID public data",
            names(OUTPUT_LICENCES)[OUTPUT_LICENCES == pr$output_licence], names(CODE_LICENCES)[CODE_LICENCES == pr$code_licence])),
    if (length(ds)) do.call(rbind, lapply(ds, lic_row)),
    kv("Sharing rules applied", "Licensed data and the published article PDF are never put in shared files; open data is shared with its citation."))
  out$Software <- software_checks(st$settings %||% list(), if (!is.null(st$online)) isTRUE(st$online$online) else NA)
  out
}

data_availability <- function(st) {
  ds <- st$analysis$datasets %||% list(); if (!length(ds)) return("No datasets were analysed in phase O.")
  lic <- vapply(ds, function(d) isTRUE(d$licensed), TRUE)
  paste0(if (any(!lic)) sprintf("Open data used (%s) can be downloaded again from the sources listed in provenance.csv, which records each query, retrieval time and SHA-256 checksum; the analysis package contains these files and the R code. ",
           paste(vapply(ds[!lic], function(d) d$name, ""), collapse = ", ")) else "",
         if (any(lic)) sprintf("Licensed data (%s) cannot be shared under the providers' terms; researchers with access can rebuild them from the queries recorded in provenance.csv.",
           paste(vapply(ds[lic], function(d) d$name, ""), collapse = ", ")) else "")
}

html_df <- function(df, n = 200) { e <- function(x) htmltools::htmlEscape(as.character(x)); df <- utils::head(as.data.frame(df), n)
  paste0("<table><tr>", paste0("<th>", e(names(df)), "</th>", collapse = ""), "</tr>", paste(apply(df, 1, function(r) paste0("<tr>", paste0("<td>", e(r), "</td>", collapse = ""), "</tr>")), collapse = ""), "</table>") }
prereq_html <- function(st) { t <- prereq_tables(st)
  c("<h2>Appendix: researcher, resources and licences</h2>", "<p class='cap'>Recorded on the Before you start page. Licence summaries are short and not legal advice; follow each provider's terms.</p>",
    unlist(Map(function(nm, df) c(sprintf("<h3>%s</h3>", htmltools::htmlEscape(nm)), html_df(df)), names(t), t))) }
focal_oa_html <- function(st) { f <- st$focal_oa; if (is.null(f)) return("")
  c("<h3>Focal article in OpenAlex</h3>", sprintf("<p>%d references, cited by %s works (checked %s). %s</p>", f$n_refs %||% 0L, f$n_citing %||% "?", f$retrieved %||% "",
    if (!is.null(f$open)) sprintf("Open copy: %s (%s).", htmltools::htmlEscape(f$open$url), htmltools::htmlEscape(f$open$version)) else "No open copy found; read it through your library."),
    if (!is.null(f$journal)) html_df(f$journal) else "") }

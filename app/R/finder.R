# ANCHOR Studio 1.5.0 - Find a focal article (search, shortlist, legal open copies, comparison) and citation styles for a DOI
#   Search: OpenAlex works (the only source for now). Downloads: legal open copies only (the copy's own PDF link), saved in a
#   private folder on this computer and never put in project files, reports or shared packages.
#   Paywalled articles: "Open in library" (the library link prefix from Before you start, or the plain DOI link).
#   Citation styles: doi.org content negotiation (text/x-bibliography with a CSL style); APA is built from Crossref metadata.

FIND_SORTS <- c("Relevance" = "relevance", "Most cited" = "cited_by_count:desc", "Newest" = "publication_date:desc",
                "Field-normalised impact (FWCI)" = "fwci:desc")
FIND_TYPES <- c("Journal article" = "article", "Review" = "review", "Preprint" = "preprint", "Book chapter" = "book-chapter")
FIND_LANGS <- c("Any language" = "", "English" = "en", "German" = "de", "French" = "fr", "Spanish" = "es", "Arabic" = "ar", "Chinese" = "zh")
FIND_CRITERIA <- c(q = "Clear research question", d = "Design I can reuse", a = "Data I can obtain", m = "Method within my skills")
FIND_MAX_SHORT <- 10L
FIND_SELECT <- paste("id,doi,display_name,publication_year,authorships,primary_location,cited_by_count,fwci,open_access,best_oa_location",
                     "is_retracted,language,type,referenced_works_count,abstract_inverted_index", sep = ",")
VERSION_LABEL <- c(publishedVersion = "Published version", acceptedVersion = "Accepted manuscript", submittedVersion = "Submitted version (preprint)")
vlabel <- function(v) { out <- unname(VERSION_LABEL[v]); out[is.na(out)] <- v[is.na(out)]; out }
VERSION_TO_START <- c(publishedVersion = "Published version of record", acceptedVersion = "Accepted manuscript", submittedVersion = "Preprint")

# citation styles offered on the Start page (CSL style ids understood by doi.org content negotiation)
CITE_STYLES <- c("APA 7th edition" = "apa", "Harvard (Cite Them Right)" = "harvard-cite-them-right", "Chicago author-date" = "chicago-author-date",
                 "IEEE" = "ieee", "Vancouver" = "vancouver", "MLA 9th edition" = "modern-language-association", "Nature" = "nature")

`%nz%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
find_dir <- function() { d <- file.path(user_dir(), "focal_candidates"); dir.create(d, showWarnings = FALSE, recursive = TRUE); d }

# OpenAlex stores abstracts as an inverted index (word -> positions)
oa_abstract <- function(inv) {
  if (!length(inv)) return("")
  pos <- unlist(inv, use.names = FALSE); words <- rep(names(inv), vapply(inv, length, 1L))
  paste(words[order(pos)], collapse = " ")
}

find_rows <- function(ws) {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  rows <- lapply(ws, function(w) {
    au <- vapply(w$authorships %nz% list(), function(a) a$author$display_name %nz% "", "")
    yr <- as.integer(w$publication_year %nz% NA); cites <- as.integer(w$cited_by_count %nz% 0L)
    oa <- w$best_oa_location
    data.frame(id = oa_short(w$id %nz% ""), doi = norm_doi(w$doi %nz% ""), title = w$display_name %nz% (w$title %nz% ""),
      authors = if (length(au) > 3) paste0(paste(au[1:3], collapse = ", "), " et al.") else paste(au, collapse = ", "),
      year = yr, journal = w$primary_location$source$display_name %nz% "", source_id = oa_short(w$primary_location$source$id %nz% ""),
      type = w$type %nz% "", language = w$language %nz% "", cites = cites,
      per_year = if (is.na(yr)) NA_real_ else round(cites / max(1L, this_year - yr + 1L), 1), fwci = as.numeric(w$fwci %nz% NA),
      oa_status = w$open_access$oa_status %nz% "closed", pdf_url = oa$pdf_url %nz% "", oa_url = oa$landing_page_url %nz% (w$open_access$oa_url %nz% ""),
      version = oa$version %nz% "", licence = oa$license %nz% "", host = oa$source$display_name %nz% "",
      retracted = isTRUE(w$is_retracted), n_refs = as.integer(w$referenced_works_count %nz% NA), abstract = oa_abstract(w$abstract_inverted_index),
      stringsAsFactors = FALSE)
  })
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

find_search <- function(cfg, query, from = NULL, to = NULL, types = c("article", "review"), lang = "", oa_only = FALSE, min_cites = 0,
                        sort = "relevance", n = 50) {
  query <- trimws(query %nz% "")
  if (!nzchar(query)) stop("Type a subject to search for, for example: ESG rating disagreement stock returns.")
  f <- oa_filter(character(0), from, to, types, lang)
  if (isTRUE(oa_only)) f <- paste0(f, ",is_oa:true")
  min_cites <- suppressWarnings(as.integer(min_cites)); if (!is.na(min_cites) && min_cites > 0) f <- paste0(f, ",cited_by_count:>", min_cites - 1L)
  q <- oa_q(cfg, list(search = query, filter = f, per_page = max(10L, min(200L, as.integer(n))), select = FIND_SELECT))
  if (!identical(sort, "relevance")) q$sort <- sort
  x <- http_json("https://api.openalex.org/works", q, cfg)
  if (is.null(x)) stop("OpenAlex did not answer the search.")
  list(rows = find_rows(x$results), found = as.integer(x$meta$count %nz% length(x$results)), query = query, filters = f, sort = sort,
       at = format(Sys.time(), "%Y-%m-%d %H:%M"))
}

library_link <- function(doi, resolver = "") {
  d <- paste0("https://doi.org/", doi)
  if (grepl("^https?://[^ ]+$", trimws(resolver %nz% ""))) paste0(trimws(resolver), d) else d
}

# download one legal open copy (the copy's own PDF link) into the private folder; refuses anything that is not a PDF
http_download <- function(url, dest, cfg) {
  mock <- get_mock()
  if (is.function(mock)) { x <- mock(url, list(.download = TRUE))
    if (is.null(x) || inherits(x, "anchor_404")) stop("the server answered HTTP 404")
    body <- if (is.raw(x)) x else charToRaw(paste(as.character(x), collapse = "\n")) }
  else {
    pace(url)
    req <- httr2::request(url) |> httr2::req_user_agent(cfg$ua) |> httr2::req_timeout(90) |> httr2::req_error(is_error = function(r) FALSE)
    r <- httr2::req_perform(req)
    if (httr2::resp_status(r) >= 400) stop(sprintf("the server answered HTTP %d", httr2::resp_status(r)))
    body <- httr2::resp_body_raw(r)
  }
  if (length(body) < 5 || !identical(rawToChar(body[1:4]), "%PDF"))
    stop("the link gave a web page, not a PDF (the host may ask you to click through). Use 'Open copy' and save it from your browser.")
  writeBin(body, dest); dest
}

find_download <- function(r, cfg) {
  if (!nzchar(r$pdf_url %nz% "")) stop("This open copy has no direct PDF link. Use 'Open copy' and save it from the page.")
  dest <- file.path(find_dir(), paste0(gsub("[^A-Za-z0-9._-]", "_", if (nzchar(r$doi)) r$doi else r$id), ".pdf"))
  http_download(r$pdf_url, dest, cfg)
  list(path = dest, size = file.size(dest), at = format(Sys.time(), "%Y-%m-%d %H:%M"), how = "open copy",
       version = r$version, licence = r$licence, url = r$pdf_url)
}

# a guide, never a decision: 60% the researcher's four ticks, 40% fetched signals; a retracted article scores 0
fit_score <- function(r, ticks, pool) {
  if (isTRUE(r$retracted)) return(0L)
  own <- length(intersect(ticks %nz% character(0), names(FIND_CRITERIA))) / length(FIND_CRITERIA)
  py <- suppressWarnings(max(pool$per_year, na.rm = TRUE)); if (!is.finite(py) || py <= 0) py <- 1
  sig <- mean(c(if (is.na(r$fwci)) 0.5 else min(r$fwci / 2, 1), if (is.na(r$per_year)) 0 else min(r$per_year / py, 1),
                if (nzchar(r$pdf_url) || nzchar(r$oa_url)) 1 else 0.5))
  as.integer(round(100 * (0.6 * own + 0.4 * sig)))
}

find_summary_prompt <- function(sh) paste0(
  "You are helping a researcher choose the focal article for a systematic literature review (ANCHOR framework).\n",
  "For each candidate below, use ONLY its abstract. Give four short lines: Question, Data and sample, Method, Main finding. ",
  "Write 'not stated in the abstract' when the abstract does not say. Do not rank, score or recommend the candidates: the researcher decides.\n\n",
  paste(sprintf("=== Candidate %d: %s (%s, %s), doi:%s ===\n%s", seq_len(nrow(sh)), sh$title, sh$authors, sh$year, sh$doi,
                ifelse(nzchar(sh$abstract), sh$abstract, "(no abstract available)")), collapse = "\n\n"))

# reference from OpenAlex metadata, used only until Crossref answers (or when offline)
find_ref <- function(r) sprintf("%s (%s). %s. %s.%s", if (nzchar(r$authors)) r$authors else "[authors]", r$year %nz% "n.d.", r$title,
                                if (nzchar(r$journal)) paste0(" ", r$journal) else "", if (nzchar(r$doi)) paste0(" https://doi.org/", r$doi) else "")

# ---------- citation in a chosen style for a DOI (doi.org content negotiation; works for Crossref and DataCite DOIs)
clean_citation <- function(x) {
  x <- gsub("<[^>]+>", "", paste(x, collapse = " ")); x <- gsub("&amp;", "&", x, fixed = TRUE); x <- gsub("&lt;", "<", x, fixed = TRUE); x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("\\s+", " ", trimws(x)); x <- sub("^\\[1\\]\\s*", "", x); sub("^1\\.\\s*", "", x)
}
cite_doi <- function(doi, style, cfg, meta = NULL) {
  doi <- norm_doi(doi); if (!nzchar(doi)) stop("No DOI.")
  if (identical(style, "apa") && !is.null(meta)) return(ref_apa(meta))
  x <- http_json(paste0("https://doi.org/", utils::URLencode(doi, reserved = FALSE)), list(), cfg, as = "text",
                 headers = list(Accept = sprintf("text/x-bibliography; style=%s; locale=en-GB", style)))
  if (is.null(x) || !nzchar(trimws(x))) stop("doi.org returned no citation for this DOI in that style.")
  clean_citation(x)
}

# references the app itself put in the box (so a later look-up may replace them); anything else is the researcher's own edit
mark_auto <- function(a, txt) { a$cite_auto <- txt; a$cite_autos <- utils::tail(unique(c(a$cite_autos, txt)), 30); a }
cite_is_auto <- function(cur, a) cur %in% c(a$cite_auto, a$cite_autos)

# ---------- what the report shows under phase A
find_report <- function(st) {
  f <- st$finder; if (is.null(f) || is.null(f$chosen)) return(NULL)
  sh <- f$shortlist; if (is.null(sh) || !nrow(sh)) return(NULL)
  tb <- data.frame(Article = sprintf("%s (%s)", sh$title, sh$year), Journal = sh$journal, Citations = sh$cites, `Per year` = sh$per_year,
                   FWCI = ifelse(is.na(sh$fwci), "-", format(round(sh$fwci, 2))),
                   `Open copy` = ifelse(nzchar(sh$pdf_url) | nzchar(sh$oa_url), ifelse(nzchar(sh$version), vlabel(sh$version), "yes"), "none"),
                   `Fit (guide)` = sh$fit %nz% NA, Chosen = ifelse(sh$id == f$chosen, "yes", ""), check.names = FALSE, stringsAsFactors = FALSE)
  ch <- sh[sh$id == f$chosen, , drop = FALSE]
  list(text = sprintf("The focal article was chosen on %s from an OpenAlex search for \"%s\" (%s matches; filters: %s; sorted by %s), comparing %d shortlisted candidate%s. Chosen: %s.%s",
                      f$chosen_at %nz% "", f$query %nz% "", format(f$found %nz% 0L, big.mark = ","), f$filters %nz% "none",
                      names(FIND_SORTS)[FIND_SORTS == (f$sort %nz% "relevance")] %nz% "relevance", nrow(sh), if (nrow(sh) == 1) "" else "s",
                      if (nrow(ch)) sprintf("%s (%s)", ch$title, ch$year) else f$chosen,
                      if (nzchar(f$reason %nz% "")) paste0(" Reason given: ", f$reason) else ""),
       table = tb,
       note = "Fit scores combine the researcher's own ticks (60%) with fetched signals (40%) and are a guide only. No PDF is included in this report.")
}
find_html <- function(st) { f <- find_report(st); if (is.null(f)) return("")
  c("<h3>How the focal article was chosen</h3>", sprintf("<p>%s</p>", htmltools::htmlEscape(f$text)), html_df(f$table), sprintf("<p class='cap'>%s</p>", htmltools::htmlEscape(f$note))) }

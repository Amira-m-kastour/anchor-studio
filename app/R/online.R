# ANCHOR Studio - live internet features (on by default; the app falls back to built-in content offline)
#   Crossref REST API: verify references, retraction / correction / concern notices, focal-article lookup
#   OpenAlex: new-reading suggestions per stage, bulk retraction check of the bibliometric dataset
# Only DOIs and short search terms are sent - never article text, drafts, notes or API keys.

online_cfg <- function(settings = list()) {
  list(enabled = !isFALSE(settings$online), mailto = trimws(settings$crossref_mailto %||% ""),
       oa_key = trimws(settings$openalex_key %||% ""), timeout = 12,
       ua = sprintf("ANCHORStudio/%s (desktop app for literature reviews%s)", APP_VERSION,
                    if (nzchar(trimws(settings$crossref_mailto %||% ""))) paste0("; mailto:", trimws(settings$crossref_mailto)) else ""))
}

norm_doi <- function(x) { x <- tolower(trimws(as.character(x %||% ""))); x <- sub("^https?://(dx\\.)?doi\\.org/", "", x); x <- sub("^doi:\\s*", "", x); x[is.na(x)] <- ""; x }

# polite pacing per host (Crossref since Dec 2025: 1 list request/s anonymous; SEC: max 10/s; OpenAlex, ROR: well below limits)
.anchor_pace <- new.env()
HOST_GAP <- c("api.crossref.org" = 1.05, "data.sec.gov" = 0.15, "www.sec.gov" = 0.15, "api.openalex.org" = 0.12, "api.ror.org" = 0.2,
              "pub.orcid.org" = 0.1, "api.imf.org" = 0.5, "doi.org" = 0.3)
pace <- function(url) { h <- tryCatch(httr2::url_parse(url)$hostname, error = function(e) ""); gap <- HOST_GAP[h]; if (is.na(gap)) return(invisible())
  last <- .anchor_pace[[h]]; if (!is.null(last)) { w <- gap - as.numeric(difftime(Sys.time(), last, units = "secs")); if (w > 0) Sys.sleep(w) }
  assign(h, Sys.time(), envir = .anchor_pace) }
get_mock <- function() { mock <- getOption("anchor.http_mock")
  if (is.null(mock) && nzchar(Sys.getenv("ANCHOR_HTTP_MOCK"))) { e <- new.env(); sys.source(Sys.getenv("ANCHOR_HTTP_MOCK"), e); mock <- e$mock_http; options(anchor.http_mock = mock) }
  mock }

# one GET; tests can set options(anchor.http_mock = function(url, query) <parsed list or text>)
http_json <- function(url, query = list(), cfg, tries = 3, headers = list(), ua = NULL, as = c("json", "text")) {
  as <- match.arg(as); mock <- get_mock()
  if (is.function(mock)) { x <- mock(url, if (length(headers)) c(query, list(.headers = headers)) else query); return(if (inherits(x, "anchor_404")) NULL else x) }   # tests: a 404 answer
  pace(url)
  req <- httr2::request(url)
  if (length(query)) req <- httr2::req_url_query(req, !!!query)
  req <- httr2::req_user_agent(req, ua %||% cfg$ua)
  if (length(headers)) req <- httr2::req_headers(req, !!!headers)
  req <- httr2::req_timeout(req, cfg$timeout %||% 12)
  req <- httr2::req_retry(req, max_tries = tries, is_transient = function(r) httr2::resp_status(r) %in% c(429, 502, 503, 504))
  req <- httr2::req_error(req, is_error = function(r) FALSE)
  r <- httr2::req_perform(req)
  if (httr2::resp_status(r) == 404) return(NULL)
  if (httr2::resp_status(r) >= 400) stop(sprintf("%s answered HTTP %d", httr2::url_parse(url)$hostname, httr2::resp_status(r)))
  if (as == "text") httr2::resp_body_string(r) else httr2::resp_body_json(r, simplifyVector = FALSE)
}

online_ping <- function(cfg) {
  if (!cfg$enabled) return(FALSE)
  isTRUE(tryCatch({ cfg$timeout <- 8; x <- http_json("https://api.crossref.org/works", c(list(rows = 0), if (nzchar(cfg$mailto)) list(mailto = cfg$mailto)), cfg, tries = 1); !is.null(x) },
                  error = function(e) FALSE))
}

`%0%` <- function(a, b) if (is.null(a) || !length(a)) b else a
apa_authors <- function(auth) {
  if (!length(auth)) return("")
  nm <- vapply(auth, function(a) {
    if (!is.null(a$family)) { ini <- if (!is.null(a$given)) paste0(substr(strsplit(a$given, "[ -]+")[[1]], 1, 1), ".", collapse = " ") else ""
      trimws(paste0(a$family, if (nzchar(ini)) paste0(", ", ini) else "")) } else a$name %0% "" }, "")
  nm <- nm[nzchar(nm)]
  if (length(nm) > 20) nm <- c(nm[1:19], "...", nm[length(nm)])
  if (length(nm) == 1) nm else paste0(paste(nm[-length(nm)], collapse = ", "), ", & ", nm[length(nm)])
}
NOTICE_CLASS <- c(retraction = "retracted", withdrawal = "retracted", removal = "retracted",
                  expression_of_concern = "concern", "expression-of-concern" = "concern", concern = "concern",
                  correction = "corrected", erratum = "corrected", corrigendum = "corrected", addendum = "corrected")
notices_df <- function(lst) {
  if (!length(lst)) return(data.frame(type = character(), label = character(), doi = character(), source = character(), date = character()))
  d <- do.call(rbind, lapply(lst, function(u) data.frame(type = tolower(u$type %0% ""), label = u$label %0% (u$type %0% ""), doi = u$DOI %0% "",
    source = u$source %0% "", date = if (!is.null(u$updated$`date-parts`)) paste(unlist(u$updated$`date-parts`[[1]]), collapse = "-") else "")))
  d[!duplicated(d[, c("type", "doi")]), , drop = FALSE]
}
notice_status <- function(nd) {
  cls <- NOTICE_CLASS[nd$type]; cls <- cls[!is.na(cls)]
  if ("retracted" %in% cls) "retracted" else if ("concern" %in% cls) "concern" else if ("corrected" %in% cls) "corrected" else "ok"
}

# Crossref record of one DOI: metadata in the app's reference format + editorial notices
crossref_lookup <- function(doi, cfg) {
  doi <- norm_doi(doi); if (!nzchar(doi)) return(NULL)
  q <- if (nzchar(cfg$mailto)) list(mailto = cfg$mailto) else list()
  x <- http_json(paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE)), q, cfg)
  if (is.null(x)) return(list(doi = doi, found = FALSE, status = "not found", checked_at = format(Sys.time(), "%Y-%m-%d %H:%M")))
  m <- x$message
  yr <- m$issued$`date-parts`[[1]][[1]] %0% (m$published$`date-parts`[[1]][[1]] %0% NA)
  nd <- notices_df(m$`updated-by`)
  # notices that point at this DOI from their own records (covers records without an updated-by list)
  u <- tryCatch(http_json("https://api.crossref.org/works", c(list(filter = paste0("updates:", doi), rows = 20), q), cfg), error = function(e) NULL)
  if (length(u$message$items)) {
    extra <- unlist(lapply(u$message$items, function(it) Filter(function(t) identical(norm_doi(t$DOI), doi), it$`update-to`)), recursive = FALSE)
    if (length(extra)) { nd <- rbind(nd, notices_df(extra)); nd <- nd[!duplicated(nd[, c("type", "doi")]), , drop = FALSE] }
  }
  list(doi = doi, found = TRUE, checked_at = format(Sys.time(), "%Y-%m-%d %H:%M"),
       meta = list(au = apa_authors(m$author), year = yr, title = unlist(m$title)[1] %0% "", journal = unlist(m$`container-title`)[1] %0% "",
                   vol = m$volume %0% "", iss = m$issue %0% "", pages = m$page %0% (m$`article-number` %0% ""), doi = doi),
       notices = nd, status = notice_status(nd))
}

# OpenAlex records for many DOIs at once (50 per request): retraction flag, citations
openalex_by_dois <- function(dois, cfg) {
  dois <- unique(norm_doi(dois)); dois <- dois[nzchar(dois) & grepl("^10\\.", dois) & !grepl("[|,]", dois)]
  out <- list()
  for (ch in split(dois, ceiling(seq_along(dois) / 50))) {
    q <- list(filter = paste0("doi:", paste(ch, collapse = "|")), per_page = 100, select = "id,doi,title,publication_year,is_retracted,cited_by_count")
    if (nzchar(cfg$oa_key)) q$api_key <- cfg$oa_key
    x <- http_json("https://api.openalex.org/works", q, cfg)
    for (w in x$results) out[[length(out) + 1]] <- data.frame(doi = norm_doi(w$doi), title = w$title %0% "", year = w$publication_year %0% NA,
                                                           is_retracted = isTRUE(w$is_retracted), cited_by = w$cited_by_count %0% NA)
  }
  if (length(out)) do.call(rbind, out) else data.frame(doi = character(), title = character(), year = integer(), is_retracted = logical(), cited_by = integer())
}

# recent, well-cited articles for a stage
openalex_suggest <- function(query, cfg, n = 5, years = 3) {
  y <- as.integer(format(Sys.Date(), "%Y"))
  q <- list(search = query, filter = sprintf("publication_year:%d-%d,type:article,is_retracted:false", y - years + 1, y),
            sort = "cited_by_count:desc", per_page = n, select = "id,doi,title,publication_year,cited_by_count,authorships,primary_location,biblio")
  if (nzchar(cfg$oa_key)) q$api_key <- cfg$oa_key
  x <- http_json("https://api.openalex.org/works", q, cfg)
  if (!length(x$results)) return(NULL)
  do.call(rbind, lapply(x$results, function(w) {
    au <- vapply(w$authorships %0% list(), function(a) a$author$display_name %0% "", "")
    au_apa <- vapply(au, function(s) { p <- strsplit(trimws(s), " +")[[1]]; if (length(p) < 2) s else paste0(p[length(p)], ", ", paste0(substr(p[-length(p)], 1, 1), ".", collapse = " ")) }, "")
    data.frame(title = w$title %0% "", year = w$publication_year %0% NA, cited_by = w$cited_by_count %0% 0, doi = norm_doi(w$doi %0% ""),
               journal = w$primary_location$source$display_name %0% "",
               au = if (!length(au_apa)) "" else if (length(au_apa) == 1) au_apa else if (length(au_apa) <= 20) paste0(paste(au_apa[-length(au_apa)], collapse = ", "), ", & ", au_apa[length(au_apa)]) else paste0(paste(au_apa[1:19], collapse = ", "), ", ... ", au_apa[length(au_apa)]),
               vol = w$biblio$volume %0% "", iss = w$biblio$issue %0% "",
               pages = paste(c(w$biblio$first_page %0% NULL, w$biblio$last_page %0% NULL), collapse = "-"), stringsAsFactors = FALSE)
  }))
}

# everything the stage cards need, in one go (runs in a background R process from the app)
online_refresh <- function(cfg, focal_doi = "") {
  res <- list(checked_at = format(Sys.time(), "%Y-%m-%d %H:%M"), online = FALSE, refs = list(), suggestions = list(), focal = NULL, errors = character())
  if (!cfg$enabled) { res$errors <- "Internet features are switched off in Settings."; return(res) }
  if (!online_ping(cfg)) { res$errors <- "No connection to Crossref (offline, blocked by a firewall, or the service is down)."; return(res) }
  res$online <- TRUE
  for (k in names(REFS)) res$refs[[REFS[[k]]$doi]] <- tryCatch(crossref_lookup(REFS[[k]]$doi, cfg), error = function(e) { res$errors <<- c(res$errors, conditionMessage(e)); NULL })
  if (nzchar(norm_doi(focal_doi))) res$focal <- tryCatch(crossref_lookup(focal_doi, cfg), error = function(e) NULL)
  for (id in names(LESSONS)) res$suggestions[[id]] <- tryCatch(openalex_suggest(LESSONS[[id]]$query, cfg), error = function(e) { res$errors <<- c(res$errors, conditionMessage(e)); NULL })
  res
}
online_dataset_check <- function(M, cfg) {
  dois <- if ("DI" %in% names(M)) norm_doi(M$DI) else character()
  res <- list(checked_at = format(Sys.time(), "%Y-%m-%d %H:%M"), n_records = nrow(M), n_doi = sum(nzchar(dois)), flagged = NULL, error = NULL)
  oa <- tryCatch(openalex_by_dois(dois, cfg), error = function(e) { res$error <<- conditionMessage(e); NULL })
  if (is.null(oa)) return(res)
  res$n_found <- nrow(oa)
  fl <- oa[oa$is_retracted, , drop = FALSE]
  if (nrow(fl)) {   # confirm with Crossref notices (first 30)
    fl$notice <- vapply(seq_len(nrow(fl)), function(i) if (i > 30) "not confirmed (limit)" else
      tryCatch({ cr <- crossref_lookup(fl$doi[i], cfg); if (nrow(cr$notices)) paste(unique(cr$notices$label), collapse = "; ") else "flagged by OpenAlex" }, error = function(e) "flagged by OpenAlex"), "")
    i <- match(fl$doi, dois); fl$record <- if ("SR" %in% names(M)) as.character(M$SR[i]) else rownames(M)[i]
  }
  res$flagged <- fl
  res
}
bg_online <- function(app_dir, libs, what, cfg, focal_doi = "", M = NULL) {
  .libPaths(libs)
  suppressPackageStartupMessages(library(shiny))
  for (f in list.files(file.path(app_dir, "R"), full.names = TRUE, pattern = "\\.R$")) source(f, local = FALSE, encoding = "UTF-8")
  if (what == "refresh") online_refresh(cfg, focal_doi) else online_dataset_check(M, cfg)
}

# reference as shown and exported: built-in fields, with live Crossref fields filling any gaps
ref_live <- function(key, st) {
  r <- REFS[[key]]; live <- st$online$refs[[r$doi]]
  if (isTRUE(live$found)) for (f in c("vol", "iss", "pages")) if (!nzchar(r[[f]] %||% "") && nzchar(live$meta[[f]] %||% "")) r[[f]] <- live$meta[[f]]
  r$status <- if (isTRUE(live$found)) live$status else if (!is.null(live)) "not found" else "unchecked"
  r$checked_at <- live$checked_at %||% ""
  r$notices <- live$notices
  r
}
NOTICE_NAME <- list(retracted = "retraction notice", concern = "expression of concern", corrected = "correction notice", ok = "", "not found" = "", unchecked = "")
status_label <- function(r) switch(r$status %||% "unchecked",
  ok = paste("verified online", substr(r$checked_at, 1, 10)), retracted = "RETRACTED - do not cite without explanation",
  concern = "expression of concern published", corrected = "correction published - check the corrected version",
  "not found" = "DOI not found online", paste("not checked online; as of", LESSONS_ASOF))

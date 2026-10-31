# ANCHOR Studio - free connectors added in 1.4.0
#   OpenAlex: corpus for bibliometrix (phase N); focal article's references, citing works, open copies and journal facts (phase A);
#             research-front trend (phase H)
#   SEC EDGAR: US firm fundamentals and SIC codes (phase O); IMF Data API: macro series (phase O)
#   ROR, ORCID: affiliation and researcher identifiers (Before you start); region from the time zone or the network
# All requests go through http_json() (pacing, retries, test hook). Nothing licensed and no article PDF is stored.

oa_q <- function(cfg, q) { if (nzchar(cfg$oa_key %||% "")) q$api_key <- cfg$oa_key; if (nzchar(cfg$mailto %||% "")) q$mailto <- cfg$mailto; q }
oa_short <- function(x) sub("^https?://openalex\\.org/", "", x %||% "")

# ---------- OpenAlex works with cursor paging
oa_works <- function(cfg, filter = NULL, search = NULL, max = 500, select = NULL, progress = NULL) {
  out <- list(); cursor <- "*"; found <- NA_integer_
  repeat {
    q <- oa_q(cfg, list(per_page = min(200, max), cursor = cursor))
    if (!is.null(filter) && nzchar(filter)) q$filter <- filter
    if (!is.null(search) && nzchar(search)) q$search <- search
    if (!is.null(select)) q$select <- select
    x <- http_json("https://api.openalex.org/works", q, cfg)
    if (is.null(x)) break
    found <- x$meta$count %||% found
    out <- c(out, x$results)
    if (is.function(progress)) progress(length(out), found)
    cursor <- x$meta$next_cursor %||% NULL
    if (is.null(cursor) || !length(x$results) || length(out) >= max) break
  }
  list(works = utils::head(out, max), found = as.integer(found %||% length(out)))
}

oa_filter <- function(base = character(0), from = NULL, to = NULL, types = NULL, lang = NULL) {
  f <- base
  if (!is.null(from) && !is.null(to) && !is.na(from) && !is.na(to)) f <- c(f, sprintf("publication_year:%d-%d", as.integer(from), as.integer(to)))
  if (length(types)) f <- c(f, paste0("type:", paste(types, collapse = "|")))
  if (length(lang) && nzchar(lang)) f <- c(f, paste0("language:", lang))
  paste(c(f, "is_paratext:false"), collapse = ",")
}

# OpenAlex raw works -> bibliometrix data frame (bibliometrix reads openalexR "list" output from an .RData file)
oa_to_M <- function(works) {
  if (!length(works)) stop("OpenAlex returned no records for this request.")
  f <- tempfile(fileext = ".RData"); oa_list <- works; save(oa_list, file = f); on.exit(unlink(f))
  M <- suppressMessages(suppressWarnings(utils::capture.output(res <- bibliometrix::convert2df(f, dbsource = "openalex_api", format = "api")))); M <- res
  if (!nrow(M)) stop("The OpenAlex records could not be converted for bibliometrix.")
  # OpenAlex gives countries per author (AU_CO) but no address strings, so the first author's country is taken from AU_CO
  if ("AU_CO" %in% names(M)) M$AU1_CO <- ifelse(is.na(M$AU_CO) | !nzchar(M$AU_CO), NA_character_, trimws(sub(";.*$", "", M$AU_CO)))
  if (!"RP" %in% names(M) || all(is.na(M$RP))) M$RP <- NA_character_
  normalize_M(M)
}

focal_work <- function(doi, cfg) {
  d <- norm_doi(doi); if (!nzchar(d)) stop("Enter the focal article's DOI on the Start page first.")
  w <- http_json(paste0("https://api.openalex.org/works/doi:", d), oa_q(cfg, list()), cfg)
  if (is.null(w)) stop("OpenAlex has no record for DOI ", d, ".")
  w
}

# corpus for phase N: search, citing works, references, or citing + references of the focal article
oa_corpus <- function(mode, cfg, query = "", doi = "", from = NULL, to = NULL, types = c("article", "review"), lang = "", max = 500, progress = NULL) {
  log <- list(); works <- list(); found <- 0L
  add <- function(label, r) { works <<- c(works, r$works); found <<- found + r$found
    log[[length(log) + 1]] <<- data.frame(file = label, database = "openalex (live)", records = length(r$works), found = r$found) }
  if (mode == "search") { if (!nzchar(trimws(query))) stop("Write a search query, for example: ESG rating disagreement stock returns.")
    add(sprintf("OpenAlex search: %s", query), oa_works(cfg, oa_filter(character(0), from, to, types, lang), query, max, progress = progress)) }
  else { fw <- focal_work(doi, cfg); id <- oa_short(fw$id)
    if (mode %in% c("citing", "both")) add(sprintf("Works citing %s", id), oa_works(cfg, oa_filter(paste0("cites:", id), from, to, types, lang), NULL, max, progress = progress))
    if (mode %in% c("refs", "both")) add(sprintf("References of %s", id), oa_works(cfg, oa_filter(paste0("cited_by:", id), NULL, NULL, NULL, ""), NULL, max, progress = progress)) }
  ids <- vapply(works, function(w) w$id %||% "", ""); works <- works[!duplicated(ids)]
  M <- oa_to_M(works)
  lg <- do.call(rbind, log)
  list(M = M, log = lg[, c("file", "database", "records")], found = found, dup = sum(lg$records) - length(works),
       note = if (found > sum(lg$records)) sprintf("OpenAlex found %d records; the first %d by relevance or date were fetched (raise the limit to fetch more).", found, sum(lg$records)) else "")
}

# ---------- phase A: references, citing works, open copy, journal facts
oa_brief <- function(w) data.frame(title = w$display_name %||% w$title %||% "", year = w$publication_year %||% NA, cited_by = w$cited_by_count %||% 0,
  journal = w$primary_location$source$display_name %||% "", doi = norm_doi(w$doi %||% ""),
  open_copy = w$best_oa_location$pdf_url %||% w$best_oa_location$landing_page_url %||% w$open_access$oa_url %||% "", stringsAsFactors = FALSE)
oa_rows <- function(ws) if (!length(ws)) data.frame(title = character(), year = integer(), cited_by = integer(), journal = character(), doi = character(), open_copy = character()) else do.call(rbind, lapply(ws, oa_brief))

focal_links <- function(doi, cfg, n = 200) {
  fw <- focal_work(doi, cfg); id <- oa_short(fw$id)
  sel <- "id,doi,display_name,publication_year,cited_by_count,primary_location,best_oa_location,open_access"
  refs <- oa_works(cfg, paste0("cited_by:", id), NULL, n, select = sel)
  citing <- tryCatch({ q <- oa_q(cfg, list(filter = paste0("cites:", id), sort = "cited_by_count:desc", per_page = min(200, n), select = sel))
    x <- http_json("https://api.openalex.org/works", q, cfg); list(works = x$results, found = x$meta$count %||% length(x$results)) }, error = function(e) list(works = list(), found = NA))
  src <- fw$primary_location$source; jf <- NULL
  if (!is.null(src$id)) jf <- tryCatch(journal_facts(oa_short(src$id), cfg), error = function(e) NULL)
  oa <- fw$best_oa_location
  list(id = id, title = fw$display_name %||% "", year = fw$publication_year %||% NA, cited_by = fw$cited_by_count %||% NA,
       refs = oa_rows(refs$works), n_refs = length(fw$referenced_works %||% list()), citing = oa_rows(citing$works), n_citing = citing$found,
       open = if (is.null(oa)) NULL else list(url = oa$pdf_url %||% oa$landing_page_url %||% "", version = oa$version %||% "", license = oa$license %||% "", host = oa$source$display_name %||% ""),
       oa_status = fw$open_access$oa_status %||% "", journal = jf, retrieved = format(Sys.time(), "%Y-%m-%d %H:%M"))
}

journal_facts <- function(source_id, cfg) {
  s <- http_json(paste0("https://api.openalex.org/sources/", source_id), oa_q(cfg, list()), cfg); if (is.null(s)) return(NULL)
  ss <- s$summary_stats %||% list()
  data.frame(Journal = s$display_name %||% "", Publisher = s$host_organization_name %||% "", ISSN = s$issn_l %||% "",
             `Two-year mean citedness` = round(as.numeric(ss$`2yr_mean_citedness` %||% NA), 2), `h-index` = ss$h_index %||% NA, `i10-index` = ss$i10_index %||% NA,
             `Works` = s$works_count %||% NA, `Open access journal` = if (isTRUE(s$is_oa)) "yes" else "no", `In DOAJ` = if (isTRUE(s$is_in_doaj)) "yes" else "no",
             `APC (USD)` = s$apc_usd %||% NA, check.names = FALSE, stringsAsFactors = FALSE)
}

# open copies for a set of DOIs (phase C screening): links only
oa_open_copies <- function(dois, cfg) {
  dois <- unique(norm_doi(dois)); dois <- dois[nzchar(dois) & grepl("^10\\.", dois) & !grepl("[|,]", dois)]; out <- list()
  for (ch in split(dois, ceiling(seq_along(dois) / 50))) {
    x <- http_json("https://api.openalex.org/works", oa_q(cfg, list(filter = paste0("doi:", paste(ch, collapse = "|")), per_page = 100,
                   select = "id,doi,display_name,publication_year,cited_by_count,primary_location,best_oa_location,open_access")), cfg)
    out <- c(out, x$results) }
  r <- oa_rows(out); r$status <- ifelse(nzchar(r$open_copy), "open copy found", "no open copy (use the library)"); r
}

# ---------- phase H: papers per year for the construct terms
oa_trend <- function(terms, cfg, from = 2000, to = as.integer(format(Sys.Date(), "%Y"))) {
  terms <- trimws(terms); if (!nzchar(terms)) stop("Write the construct terms, for example: ESG rating disagreement.")
  x <- http_json("https://api.openalex.org/works", oa_q(cfg, list(search = terms, filter = sprintf("publication_year:%d-%d,is_paratext:false", as.integer(from), as.integer(to)),
                 group_by = "publication_year")), cfg)
  g <- x$group_by %||% list(); if (!length(g)) stop("OpenAlex found no papers for these terms.")
  d <- data.frame(year = as.integer(vapply(g, function(k) as.character(k$key %||% NA), "")), papers = vapply(g, function(k) as.numeric(k$count %||% 0), 0))
  d <- d[order(d$year), ]; d$terms <- terms; d
}
trend_plot <- function(d) function() {
  op <- graphics::par(mar = c(4, 4.5, 2.5, 1)); on.exit(graphics::par(op))
  graphics::plot(d$year, d$papers, type = "h", lwd = 8, lend = 1, col = PAL[["accent"]], xlab = "Publication year", ylab = "Papers (OpenAlex)", main = sprintf("Research front: \"%s\"", d$terms[1]), las = 1, bty = "l")
}

# ---------- phase O: SEC EDGAR company facts (US listed firms)
sec_ua <- function(cfg) { m <- trimws(cfg$mailto %||% "")
  if (!nzchar(m)) stop("SEC EDGAR asks every program to name a contact. Enter a contact email under Settings > Internet features (it is sent only to the SEC and Crossref, and never saved in project files).")
  sprintf("ANCHORStudio/%s %s", APP_VERSION, m) }
SEC_TAGS <- c(assets = "Assets", liabilities = "Liabilities", equity = "StockholdersEquity", revenue = "Revenues", revenue2 = "RevenueFromContractWithCustomerExcludingAssessedTax",
              net_income = "NetIncomeLoss", capex = "PaymentsToAcquirePropertyPlantAndEquipment", ppe_net = "PropertyPlantAndEquipmentNet",
              sga = "SellingGeneralAndAdministrativeExpense", rnd = "ResearchAndDevelopmentExpense", advertising = "AdvertisingExpense")
sec_cik_map <- function(cfg) { x <- http_json("https://www.sec.gov/files/company_tickers.json", list(), cfg, ua = sec_ua(cfg))
  data.frame(ticker = toupper(vapply(x, function(r) r$ticker %||% "", "")), cik = sprintf("%010d", as.integer(vapply(x, function(r) as.numeric(r$cik_str %||% NA), 0))), name = vapply(x, function(r) r$title %||% "", "")) }
fetch_sec <- function(ids, from, to, cfg) {
  ids <- toupper(split_ids(ids)); if (!length(ids)) stop("Give tickers or CIK numbers of US-listed firms, for example AAPL, MSFT.")
  ua <- sec_ua(cfg); map <- NULL; rows <- list()
  for (t in ids) {
    cik <- if (grepl("^[0-9]+$", t)) sprintf("%010d", as.integer(t)) else { if (is.null(map)) map <- sec_cik_map(cfg); map$cik[match(t, map$ticker)] }
    if (is.na(cik)) { warning("Ticker not found at the SEC: ", t); next }
    sub <- http_json(sprintf("https://data.sec.gov/submissions/CIK%s.json", cik), list(), cfg, ua = ua)
    fx <- http_json(sprintf("https://data.sec.gov/api/xbrl/companyfacts/CIK%s.json", cik), list(), cfg, ua = ua)
    if (is.null(fx)) next
    g <- fx$facts$`us-gaap` %||% list()
    # companyfacts repeats prior years inside each 10-K, so values are keyed by the period end date, not by the filing's fy
    val <- function(tag) { u <- g[[tag]]$units$USD; if (is.null(u)) return(NULL)
      d <- do.call(rbind, lapply(u, function(o) if (grepl("^10-K", o$form %||% "")) data.frame(start = o$start %||% NA_character_, end = o$end, val = as.numeric(o$val), filed = o$filed) else NULL))
      if (is.null(d)) return(NULL)
      dur <- as.numeric(as.Date(d$end) - as.Date(d$start)); d <- d[is.na(d$start) | (!is.na(dur) & dur >= 330 & dur <= 400), ]
      if (!nrow(d)) return(NULL)
      d <- d[order(d$end, d$filed), ]; d <- d[!duplicated(d$end, fromLast = TRUE), ]; d$fy <- as.integer(substr(d$end, 1, 4))
      d <- d[!duplicated(d$fy, fromLast = TRUE), ]; stats::setNames(d$val, d$fy) }
    v <- lapply(SEC_TAGS, val); yrs <- sort(unique(as.integer(unlist(lapply(v, names))))); yrs <- yrs[yrs >= from & yrs <= to]
    if (!length(yrs)) next
    pick <- function(k) { x <- v[[k]]; if (is.null(x)) rep(NA_real_, length(yrs)) else unname(x[as.character(yrs)]) }
    d <- data.frame(ticker = t, cik = cik, name = fx$entityName %||% "", fy = yrs, sic = sub$sic %||% NA, sic_description = sub$sicDescription %||% NA,
                    assets = pick("assets"), liabilities = pick("liabilities"), equity = pick("equity"), revenue = ifelse(is.na(pick("revenue")), pick("revenue2"), pick("revenue")),
                    net_income = pick("net_income"), capex = pick("capex"), ppe_net = pick("ppe_net"), sga = pick("sga"), rnd = pick("rnd"), advertising = pick("advertising"))
    rows[[t]] <- d
  }
  if (!length(rows)) stop("No annual 10-K facts were found for these firms and years.")
  df <- do.call(rbind, rows); rownames(df) <- NULL
  df$roe <- df$net_income / df$equity; df$leverage <- df$liabilities / df$assets; df$capex_ppe <- df$capex / df$ppe_net
  df$sga_sales <- df$sga / df$revenue; df$rnd_sales <- df$rnd / df$revenue; df$adv_sales <- df$advertising / df$revenue
  new_dataset(paste0("sec_", gsub("[^A-Za-z0-9]+", "_", paste(utils::head(ids, 4), collapse = "_"))), df, "SEC EDGAR XBRL company facts and submissions (data.sec.gov)",
              sprintf("firms=%s; fiscal years %s-%s; 10-K annual values", paste(ids, collapse = ","), from, to),
              note = "Public US filings (free). Market cap and market-to-book need prices: merge with a price dataset. Firms that do not report a tag show NA.")
}

# ---------- phase O: IMF Data API (SDMX 3.0, CSV)
fetch_imf <- function(dataflow, key, start, cfg, agency = "IMF.STA") {
  dataflow <- trimws(dataflow); key <- trimws(key); if (!nzchar(dataflow) || !nzchar(key)) stop("Give a dataflow (for example CPI) and a series key (for example EGY.CPI._T.IX.M).")
  url <- sprintf("https://api.imf.org/external/sdmx/3.0/data/dataflow/%s/%s/~/%s", agency, dataflow, key)
  q <- if (nzchar(start %||% "")) stats::setNames(list(paste0("ge:", start)), "c[TIME_PERIOD]") else list()
  txt <- http_json(url, q, cfg, headers = list(Accept = "text/csv"), as = "text")
  if (is.null(txt) || !nzchar(txt)) stop("The IMF returned no data for this dataflow and key.")
  raw <- utils::read.csv(text = txt, check.names = FALSE, stringsAsFactors = FALSE)
  tp <- grep("^TIME_PERIOD$", names(raw), value = TRUE); ov <- grep("^OBS_VALUE$", names(raw), value = TRUE)
  if (!length(tp) || !length(ov)) stop("Unexpected IMF answer (no TIME_PERIOD/OBS_VALUE columns).")
  dims <- setdiff(names(raw), c(tp, ov, grep("STATUS|OBS_|DECIMALS|UNIT_MULT|SCALE|COMMENT|^DATAFLOW|^STRUCTURE|ACTION", names(raw), value = TRUE)))
  df <- data.frame(period = raw[[tp]], series = if (length(dims)) do.call(paste, c(raw[dims], sep = ".")) else key, value = suppressWarnings(as.numeric(raw[[ov]])), stringsAsFactors = FALSE)
  new_dataset(paste0("imf_", dataflow, "_", substr(gsub("[^A-Za-z0-9]+", "_", key), 1, 30)), df, "IMF Data API (SDMX 3.0)", sprintf("%s/%s key=%s from %s", agency, dataflow, key, start %||% "start"),
              note = "IMF data: free to use with attribution under the IMF's terms; check each dataset's notes.")
}

# ---------- Before you start: region, affiliation (ROR), ORCID, database detection
# Region without any network request: 1) the computer's time zone; 2) on Windows, the "Country or region" setting;
# 3) the language setting the user started with (LANG/LC_ALL; never the locale the app switches to for UTF-8)
country_from_locale <- function(loc) {
  loc <- loc %||% ""; if (!nzchar(loc) || grepl("^(C|POSIX)([._]|$)", loc)) return(NA_character_)
  m <- regmatches(loc, regexpr("^[a-z]{2,3}_([A-Z]{2})", loc)); if (length(m)) return(sub(".*_", "", m))     # de_DE.UTF-8
  nm <- sub("\\..*$", "", sub("^[^_]*_", "", loc)); hit <- names(COUNTRIES)[tolower(COUNTRIES) == tolower(nm)]     # English_Egypt.1252
  if (length(hit)) hit[1] else NA_character_ }
detect_country_tz <- function(tz = tryCatch(Sys.timezone(), error = function(e) NA_character_), win_geo = NULL, lang = Sys.getenv(c("LC_ALL", "LANG"))) {
  cc <- if (!is.na(tz %||% NA) && nzchar(tz)) unname(TZ_COUNTRY[tz]) else NA_character_
  if (length(cc) && !is.na(cc)) return(list(cc = cc, how = sprintf("from your computer's time zone (%s)", tz)))
  if (is.null(win_geo) && .Platform$OS.type == "windows")
    win_geo <- tryCatch(utils::readRegistry("Control Panel\\International\\Geo", hive = "HCU")$Name, error = function(e) NULL)
  if (length(win_geo) && grepl("^[A-Z]{2}$", win_geo) && win_geo %in% names(COUNTRIES)) return(list(cc = win_geo, how = "from your Windows region setting"))
  for (l in lang) { c2 <- country_from_locale(l); if (!is.na(c2) && c2 %in% names(COUNTRIES)) return(list(cc = c2, how = sprintf("from your computer's language setting (%s)", l))) }
  list(cc = NA_character_, how = "could not be detected (choose it)") }
detect_country_net <- function(cfg) { txt <- http_json("https://www.cloudflare.com/cdn-cgi/trace", list(), cfg, as = "text")
  m <- regmatches(txt, regexpr("loc=[A-Z]{2}", txt)); if (!length(m)) stop("The network location could not be read.")
  list(cc = sub("loc=", "", m), how = "from your network (Cloudflare)") }

# ROR (Research Organization Registry, CC0). Following ROR's guidance: a country starter list via the documented advanced query
# (field:value, AND), type-ahead with ?query= plus one country filter, free text always allowed, display name = ror_display.
ror_items <- function(x) { its <- x$items %||% list()
  if (!length(its)) return(data.frame(id = character(), name = character(), city = character(), country = character(), domains = character(), types = character()))
  d <- do.call(rbind, lapply(its, function(o) { nm <- o$names %||% list(); disp <- Filter(function(n) "ror_display" %in% unlist(n$types), nm)
    loc <- o$locations[[1]]$geonames_details %||% list()
    data.frame(id = o$id %||% "", name = (if (length(disp)) disp[[1]]$value else nm[[1]]$value) %||% "", city = loc$name %||% "", country = loc$country_code %||% "",
               domains = paste(unlist(o$domains %||% list()), collapse = " "), types = paste(unlist(o$types %||% list()), collapse = " "), stringsAsFactors = FALSE) }))
  d[!duplicated(d$id), ] }
# Elasticsearch query-string escaping for query.advanced values (a hyphen would otherwise mean NOT)
es_escape <- function(x) { ch <- strsplit(x, "")[[1]]; paste0(ifelse(ch %in% strsplit("+-=&|><!(){}[]^\"~*?:\\/", "")[[1]], paste0("\\", ch), ch), collapse = "") }
ror_cache_file <- function(cc) file.path(user_dir(), paste0("ror2_", cc, ".rds"))
ror_country <- function(cc, cfg, max_pages = 10) {
  f <- ror_cache_file(cc); if (file.exists(f) && difftime(Sys.time(), file.mtime(f), units = "days") < 30) return(readRDS(f))
  out <- list(); total <- NA_integer_
  for (pg in seq_len(max_pages)) {
    x <- http_json("https://api.ror.org/v2/organizations", list(`query.advanced` = sprintf("types:education AND locations.geonames_details.country_code:%s", toupper(cc)), page = pg), cfg)
    total <- as.integer(x$number_of_results %||% total); r <- ror_items(x); out[[pg]] <- r; if (nrow(r) < 20) break }
  d <- do.call(rbind, out); d <- d[!duplicated(d$id), ]; d <- d[order(d$name), ]; attr(d, "total") <- total
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE); try(saveRDS(d, f), silent = TRUE); d }
# type-ahead: any organisation type, education first
ror_search <- function(q, cfg, cc = "") {
  q <- trimws(q %||% ""); if (nchar(q) < 2) return(ror_items(list()))
  d <- ror_items(http_json("https://api.ror.org/v2/organizations", c(list(query = q), if (nzchar(cc %||% "")) list(filter = paste0("country.country_code:", toupper(cc)))), cfg))
  d[order(!grepl("education", d$types)), ] }
GENERIC_SLD <- c("ac", "edu", "co", "com", "gov", "org", "net", "sch", "mil", "res", "uni")
ror_by_domain <- function(domain, cfg) {
  d <- tolower(trimws(sub(".*@", "", domain %||% ""))); d <- sub("^https?://", "", sub("/.*$", "", d))
  if (!nzchar(d) || !grepl("\\.", d)) stop("Type your institution's email domain, for example university.edu (only the part after @ is used).")
  parts <- strsplit(d, ".", fixed = TRUE)[[1]]
  for (k in seq_len(length(parts) - 1)) { rest <- parts[k:length(parts)]
    if (length(rest) == 2 && rest[1] %in% GENERIC_SLD) break          # never match a whole suffix such as edu.eg or ac.uk
    dom <- paste(rest, collapse = ".")
    r <- tryCatch(ror_items(http_json("https://api.ror.org/v2/organizations", list(`query.advanced` = paste0("domains:", es_escape(dom))), cfg)), error = function(e) NULL)
    if (!is.null(r) && nrow(r)) return(r[1, ]) }
  stop("No institution in ROR lists the domain ", d, ". Choose it from the list or type its name.") }

orcid_ok <- function(id) { id <- toupper(gsub("^https?://orcid.org/", "", trimws(id %||% "")))
  if (!grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", id)) return(FALSE)
  d <- gsub("-", "", id); tot <- 0; for (ch in strsplit(substr(d, 1, 15), "")[[1]]) tot <- (tot + as.integer(ch)) * 2
  r <- (12 - tot %% 11) %% 11; identical(substr(d, 16, 16), if (r == 10) "X" else as.character(r)) }
orcid_name <- function(id, cfg) { id <- toupper(gsub("^https?://orcid.org/", "", trimws(id)))
  x <- http_json(sprintf("https://pub.orcid.org/v3.0/%s/person", id), list(), cfg, headers = list(Accept = "application/json"))
  if (is.null(x)) stop("ORCID has no public record for ", id, ". Check the iD.")
  nm <- trimws(paste(x$name$`given-names`$value %||% "", x$name$`family-name`$value %||% ""))
  if (!nzchar(nm)) nm <- trimws(x$name$`credit-name`$value %||% "")
  if (nzchar(nm)) nm else "record found; name not public" }

# LSEG Workspace / Eikon desktop runs a local API proxy while it is running (ports 9000 or 9060). Only its own status answer
# ("ST_PROXY_READY") counts, so another local service on those ports is not mistaken for it. Absence proves nothing.
probe_lseg <- function() { for (port in c(9000L, 9060L)) { ok <- tryCatch({ r <- httr2::request(sprintf("http://127.0.0.1:%d/api/status", port)) |> httr2::req_timeout(1.5) |>
      httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform(); httr2::resp_status(r) == 200 && grepl("ST_PROXY", httr2::resp_body_string(r), fixed = TRUE) }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE) }
  FALSE }

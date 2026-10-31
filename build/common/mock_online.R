# Test double for the smoke test: answers shaped like the Crossref and OpenAlex REST APIs (no network needed).
# Mock Crossref/OpenAlex answers shaped like the documented JSON (parsed with simplifyVector = FALSE)
mock_http <- function(url, query) {
  if (grepl("api.crossref.org/works$", url) && identical(query$rows, 0)) return(list(status = "ok", message = list(`total-results` = 1)))
  if (grepl("api.crossref.org/works$", url) && !is.null(query$filter)) {
    d <- sub("^updates:", "", query$filter)
    if (d == "10.9999/retracted.1") return(list(message = list(items = list(list(DOI = "10.9999/notice.1", `update-to` = list(list(DOI = d, type = "retraction", label = "Retraction", source = "publisher", updated = list(`date-parts` = list(list(2025, 3, 1))))))))))
    return(list(message = list(items = list())))
  }
  if (grepl("api.crossref.org/works/", url)) {
    d <- utils::URLdecode(sub(".*/works/", "", url))
    if (d == "10.0000/missing") return(NULL)
    m <- list(DOI = d, title = list(paste("Title of", d)), author = list(list(given = "Weng Marc", family = "Lim"), list(given = "Satish", family = "Kumar")),
              `container-title` = list("Journal X"), volume = "15", issue = "4", page = "616-626", issued = list(`date-parts` = list(list(2024, 5))))
    if (d == "10.9999/corrected.1") m$`updated-by` <- list(list(DOI = "10.9999/corr", type = "correction", label = "Correction", source = "publisher", updated = list(`date-parts` = list(list(2024, 2, 2)))))
    return(list(status = "ok", message = m))
  }
  if (grepl("api.openalex.org/works", url) && grepl("^doi:", query$filter %||% "")) {
    ds <- strsplit(sub("^doi:", "", query$filter), "|", fixed = TRUE)[[1]]
    return(list(results = lapply(ds, function(d) list(id = "W1", doi = paste0("https://doi.org/", d), title = paste("T", d), publication_year = 2020L,
                                                       is_retracted = grepl("retracted", d), cited_by_count = 3L))))
  }
  if (grepl("api.openalex.org/works", url) && !is.null(query$search)) {
    return(list(results = lapply(1:3, function(i) list(id = paste0("W", i), doi = paste0("https://doi.org/10.1234/new.", i), title = paste(query$search, "study", i),
      publication_year = 2026L, cited_by_count = 40L - i, authorships = list(list(author = list(display_name = "Weng Marc Lim")), list(author = list(display_name = "Satish Kumar"))),
      primary_location = list(source = list(display_name = "Journal of Reviews")), biblio = list(volume = "7", issue = "1", first_page = "1", last_page = "20")))))
  }
  stop("unexpected request: ", url)
}

# ---- open-data sources for the analysis step (shapes follow each API's documented JSON / file layout)
mock_open_data <- function(url, query) {
  if (grepl("api.worldbank.org/v2/country/", url)) {
    ind <- sub(".*/indicator/", "", url); yrs <- as.integer(strsplit(query$date, ":")[[1]])
    recs <- unlist(lapply(c("Egypt, Arab Rep.", "Morocco"), function(cn) lapply(yrs[1]:yrs[2], function(y) list(indicator = list(id = ind, value = ind),
      country = list(id = substr(cn, 1, 2), value = cn), countryiso3code = if (grepl("Egypt", cn)) "EGY" else "MAR", date = as.character(y), value = 1000 + y))), recursive = FALSE)
    return(list(list(page = 1, pages = 1, per_page = 20000, total = length(recs)), recs))
  }
  if (grepl("api.stlouisfed.org", url)) return(list(observations = lapply(0:4, function(i) list(date = as.character(as.Date(query$observation_start) + i), value = if (i == 2) "." else as.character(1.5 + i / 10)))))
  if (grepl("ken.french/ftp/", url)) {
    daily <- grepl("daily", url, ignore.case = TRUE)
    rows <- if (daily) sprintf("%s,  0.10,  -0.02,  0.03,  0.01, -0.01,  0.01", format(as.Date("2024-01-02") + 0:4, "%Y%m%d")) else sprintf("%s,  1.20,  -0.50,  0.30,  0.20, -0.10,  0.40", sprintf("2024%02d", 1:6))
    txt <- c("This file was created using the 202409 CRSP database.", "", ",Mkt-RF,SMB,HML,RMW,CMA,RF", rows, "", " Annual Factors: January-December ", ",Mkt-RF,SMB,HML,RMW,CMA,RF", "  2023,  20.0, -3.0, -5.0, 3.0, -2.0, 5.0")
    d <- tempfile("ffm"); dir.create(d); f <- file.path(d, "factors.csv"); writeLines(txt, f); z <- tempfile(fileext = ".zip")
    old <- setwd(d); on.exit(setwd(old)); zip::zip(z, "factors.csv"); return(readBin(z, "raw", file.size(z)))
  }
  if (grepl("finance.yahoo.com/v8/finance/chart/", url)) {
    ts <- as.integer(as.POSIXct(as.Date("2024-01-01") + 0:4)); cl <- c(10, 10.2, 10.1, 10.4, 10.5)
    return(list(chart = list(result = list(list(timestamp = as.list(ts), indicators = list(quote = list(list(close = as.list(cl), volume = as.list(rep(1000, 5)))), adjclose = list(list(adjclose = as.list(cl)))))))))
  }
  if (grepl("api.gdeltproject.org", url)) return(list(timeline = list(list(series = "Average Tone", data = lapply(0:4, function(i) list(date = paste0(format(as.Date("2026-08-01") + i, "%Y%m%d"), "T000000Z"), value = -1 + i / 4))))))
  NULL
}
local({ base <- mock_http; mock_http <<- function(url, query) { x <- mock_open_data(url, query); if (!is.null(x)) x else base(url, query) } })

# ---- 1.4.0 connectors: OpenAlex corpus / focal links / trend, ROR, ORCID, network region, SEC EDGAR, IMF (shapes of the real answers, made-up values)
mock_work <- function(i, cites = character(0)) list(
  id = sprintf("https://openalex.org/W40000%05d", i), doi = sprintf("https://doi.org/10.9999/test.%d", i),
  title = sprintf("ESG ratings and stock returns study %d", i), display_name = sprintf("ESG ratings and stock returns study %d", i),
  publication_year = 2010L + i %% 14, publication_date = sprintf("%d-03-01", 2010 + i %% 14), language = "en", type = if (i %% 7 == 0) "review" else "article",
  ids = list(openalex = sprintf("https://openalex.org/W40000%05d", i), doi = sprintf("https://doi.org/10.9999/test.%d", i)),
  primary_location = list(is_oa = FALSE, landing_page_url = sprintf("https://doi.org/10.9999/test.%d", i), pdf_url = NULL,
    source = list(id = sprintf("https://openalex.org/S7000%d", i %% 4), display_name = c("Journal of Finance Tests", "Review of Test Studies", "Accounting Test Review", "ESG Test Quarterly")[i %% 4 + 1],
                  issn_l = "1234-5678", issn = list("1234-5678"), is_oa = FALSE, is_in_doaj = FALSE, host_organization = "https://openalex.org/P1", host_organization_name = "Test Publisher", type = "journal"),
    license = NULL, version = "publishedVersion"),
  open_access = list(is_oa = i %% 3 == 0, oa_status = if (i %% 3 == 0) "green" else "closed", oa_url = if (i %% 3 == 0) sprintf("https://repository.example.org/%d.pdf", i) else NULL, any_repository_has_fulltext = i %% 3 == 0),
  best_oa_location = if (i %% 3 == 0) list(is_oa = TRUE, landing_page_url = sprintf("https://repository.example.org/%d", i), pdf_url = sprintf("https://repository.example.org/%d.pdf", i), license = "cc-by", version = "acceptedVersion",
                                          source = list(display_name = "Test Repository", type = "repository")) else NULL,
  authorships = lapply(seq_len(1 + i %% 3), function(k) list(author_position = if (k == 1) "first" else "middle",
    author = list(id = sprintf("https://openalex.org/A%d%d", i, k), display_name = c("Alice Example", "Bob Sample", "Chen Test", "Dina Mock")[(i + k) %% 4 + 1], orcid = NULL),
    institutions = list(list(id = sprintf("https://openalex.org/I%d", k), display_name = c("Test University", "Mock Institute", "Cairo Test College")[k], ror = sprintf("https://ror.org/00test00%d", k), country_code = c("US", "DE", "EG")[k], type = "education")),
    countries = list(c("US", "DE", "EG")[k]), is_corresponding = k == 1, raw_author_name = "x", raw_affiliation_strings = list(c("Test University", "Mock Institute", "Cairo Test College")[k]))),
  cited_by_count = 100L - i, biblio = list(volume = "1", issue = "2", first_page = "10", last_page = "20"), is_retracted = FALSE, is_paratext = FALSE,
  keywords = list(list(id = "https://openalex.org/keywords/esg", display_name = "ESG", score = 0.6), list(id = "https://openalex.org/keywords/x", display_name = c("Rating disagreement", "Stock returns", "Sustainability")[i %% 3 + 1], score = 0.4)),
  concepts = list(list(id = "https://openalex.org/C1", wikidata = "https://www.wikidata.org/wiki/Q1", display_name = "Finance", level = 1L, score = 0.5)),
  topics = list(list(id = "https://openalex.org/T1", display_name = "ESG and corporate sustainability", score = 0.99, subfield = list(id = "https://openalex.org/subfields/1408", display_name = "Strategy and Management"),
                     field = list(id = "https://openalex.org/fields/14", display_name = "Business, Management and Accounting"), domain = list(id = "https://openalex.org/domains/2", display_name = "Social Sciences"))),
  mesh = list(), sustainable_development_goals = list(), grants = list(),
  referenced_works = as.list(cites), referenced_works_count = length(cites), related_works = list(),
  abstract_inverted_index = list(We = list(0L), study = list(1L), ESG = list(2L), ratings = list(3L), and = list(4L), returns = list(5L)),
  counts_by_year = list(list(year = 2025L, cited_by_count = 5L)), corresponding_author_ids = list(), corresponding_institution_ids = list())
MOCK_WORKS <- local({ set.seed(7); lapply(1:120, function(i) mock_work(i, sprintf("https://openalex.org/W40000%05d", sample(setdiff(1:120, i), 6)))) })

mock_connectors <- function(url, query) {
  if (grepl("api.openalex.org/works/doi:", url)) { w <- mock_work(999, sprintf("https://openalex.org/W40000%05d", 1:30)); w$id <- "https://openalex.org/W4999"; w$cited_by_count <- 250L; return(w) }
  if (grepl("api.openalex.org/sources/", url)) return(list(id = url, display_name = "Review of Test Studies", host_organization_name = "Test Publisher", issn_l = "1380-0000", works_count = 2100L,
    is_oa = FALSE, is_in_doaj = FALSE, apc_usd = 3290L, summary_stats = list(`2yr_mean_citedness` = 4.2, h_index = 95L, i10_index = 1500L)))
  if (grepl("api.openalex.org/works$", url) && !is.null(query$group_by)) return(list(meta = list(count = 400L),
    group_by = lapply(2015:2025, function(y) list(key = as.character(y), key_display_name = as.character(y), count = 5L * (y - 2013L)))))
  if (grepl("api.openalex.org/works$", url) && !is.null(query$cursor)) {
    f <- query$filter %||% ""; pool <- if (grepl("cited_by:", f)) MOCK_WORKS[1:30] else if (grepl("cites:", f)) MOCK_WORKS[31:120] else MOCK_WORKS
    per <- as.integer(query$per_page %||% 200); start <- if (identical(query$cursor, "*")) 1L else as.integer(query$cursor)
    idx <- seq(start, min(length(pool), start + per - 1)); nxt <- if (max(idx) < length(pool)) as.character(max(idx) + 1L) else NULL
    return(list(meta = list(count = length(pool), next_cursor = nxt), results = pool[idx])) }
  if (grepl("api.openalex.org/works$", url) && grepl("cites:", query$filter %||% "") && is.null(query$search))
    return(list(meta = list(count = 90L), results = MOCK_WORKS[31:60]))
  if (grepl("api.openalex.org/works$", url) && grepl("^doi:10\\.9999/test", query$filter %||% "")) {
    ds <- sub("^doi:", "", strsplit(query$filter, "|", fixed = TRUE)[[1]]); ds <- sub("^doi:", "", ds); ix <- as.integer(sub(".*test\\.", "", ds))
    return(list(meta = list(count = length(ix)), results = MOCK_WORKS[ix[!is.na(ix) & ix <= 120]])) }
  if (grepl("api.ror.org/v2/organizations", url)) {
    org <- function(id, name, city, cc, dom, type = "education") list(id = paste0("https://ror.org/", id), names = list(list(value = paste(name, "(alias)"), types = list("alias"), lang = NULL), list(value = name, types = list("ror_display", "label"), lang = "en")),
      locations = list(list(geonames_id = 1L, geonames_details = list(name = city, country_code = cc, country_name = cc))), domains = as.list(dom), types = list(type), status = "active")
    fu <- org("0test0fu1", "FernUniversit\u00e4t in Hagen", "Hagen", "DE", "fernuni-hagen.de")
    adv <- query$`query.advanced` %||% ""; pg <- as.integer(query$page %||% 1)
    page_of <- function(all) list(number_of_results = length(all), items = all[seq_len(min(20, max(0, length(all) - (pg - 1) * 20))) + (pg - 1) * 20])
    if (grepl("^domains:", adv)) {                     # the hyphen must arrive escaped (Elasticsearch query string)
      if (identical(adv, "domains:fernuni\\-hagen.de")) return(list(number_of_results = 1L, items = list(fu)))
      if (identical(adv, "domains:tit.edu")) return(list(number_of_results = 1L, items = list(org("0test0us1", "Test Institute of Technology", "Boston", "US", "tit.edu"))))
      return(list(number_of_results = 0L, items = list())) }
    if (grepl("types:education AND locations.geonames_details.country_code:", adv, fixed = TRUE)) {
      cc <- sub(".*country_code:", "", adv)
      all <- switch(cc, EG = c(lapply(1:20, function(i) org(sprintf("0test%03d", i), sprintf("Test University of Egypt %d", i), "Cairo", "EG", sprintf("tue%d.edu.eg", i))), list(org("0test099", "Mock Institute of Alexandria", "Alexandria", "EG", "mia.edu.eg"))),
                    DE = lapply(1:420, function(i) org(sprintf("0de%04d", i), sprintf("Test Hochschule %03d", i), "Berlin", "DE", sprintf("th%d.de", i))),
                    list(org("0xx0001", paste("Test University of", cc), "Capital", cc, "tu.example")))
      return(page_of(all)) }
    q <- tolower(query$query %||% ""); f <- query$filter %||% ""
    if (nzchar(q)) { hits <- list()
      if (grepl("hagen", q) && (f == "" || grepl("DE", f))) hits <- list(org("0test0kh1", "Klinikum Hagen", "Hagen", "DE", "kh.de", "healthcare"), fu)
      if (grepl("cairo", q) && (f == "" || grepl("EG", f))) hits <- list(org("0test0cu1", "Test Cairo University", "Giza", "EG", "tcu.edu.eg"))
      return(list(number_of_results = length(hits), items = hits)) }
    return(list(number_of_results = 0L, items = list())) }
  if (grepl("pub.orcid.org/v3.0/0000-0001-5109-3700", url)) return(list(name = NULL))                 # record exists, name private
  if (grepl("pub.orcid.org/v3.0/0000-0003-1415-9269", url)) return(structure(list(), class = "anchor_404"))   # 404: no such record
  if (grepl("pub.orcid.org/v3.0/", url)) return(list(name = list(`given-names` = list(value = "Test"), `family-name` = list(value = "Researcher"))))
  if (grepl("cloudflare.com/cdn-cgi/trace", url)) return("fl=1\nh=www.cloudflare.com\nip=192.0.2.1\nloc=EG\ncolo=CAI\n")
  if (grepl("sec.gov/files/company_tickers.json", url)) return(list(`0` = list(cik_str = 1234567L, ticker = "TEST", title = "Test Corp"), `1` = list(cik_str = 7654321L, ticker = "MOCK", title = "Mock Inc")))
  if (grepl("data.sec.gov/submissions/", url)) return(list(sic = "3571", sicDescription = "Electronic Computers", name = "Test Corp"))
  if (grepl("data.sec.gov/api/xbrl/companyfacts/", url)) {
    fy <- function(v, dur = TRUE) list(USD = unlist(lapply(2019:2023, function(y) list(
      list(start = if (dur) sprintf("%d-01-01", y) else NULL, end = sprintf("%d-12-31", y), val = v * (1 + (y - 2019) / 10), fy = y, fp = "FY", form = "10-K", filed = sprintf("%d-02-15", y + 1)),
      list(start = if (dur) sprintf("%d-01-01", y - 1) else NULL, end = sprintf("%d-12-31", y - 1), val = v * (1 + (y - 1 - 2019) / 10), fy = y, fp = "FY", form = "10-K", filed = sprintf("%d-02-15", y + 1)))), recursive = FALSE))
    return(list(entityName = "Test Corp", facts = list(`us-gaap` = list(Assets = list(units = fy(1000, FALSE)), Liabilities = list(units = fy(600, FALSE)), StockholdersEquity = list(units = fy(400, FALSE)),
      Revenues = list(units = fy(800)), NetIncomeLoss = list(units = fy(80)), PaymentsToAcquirePropertyPlantAndEquipment = list(units = fy(50)), PropertyPlantAndEquipmentNet = list(units = fy(300, FALSE)),
      SellingGeneralAndAdministrativeExpense = list(units = fy(120)), ResearchAndDevelopmentExpense = list(units = fy(60)))))) }
  if (grepl("api.imf.org/external/sdmx/3.0/data/", url))
    return(paste(c("DATAFLOW,COUNTRY,INDEX_TYPE,COICOP_1999,TYPE_OF_TRANSFORMATION,FREQUENCY,TIME_PERIOD,OBS_VALUE,STATUS",
                   sprintf("IMF.STA:CPI(5.0.0),EGY,CPI,_T,IX,M,2024-M%02d,%s,", 1:6, 300 + 1:6)), collapse = "\n"))
  NULL
}
local({ base <- mock_http; mock_http <<- function(url, query) { x <- mock_connectors(url, query); if (!is.null(x)) x else base(url, query) } })

# ---- 1.5.0: Find a focal article (OpenAlex search with filters and sort), legal open-copy PDFs, doi.org citation styles
mock_pdf <- local({ cache <- list(); function(i) { k <- as.character(i); if (!is.null(cache[[k]])) return(cache[[k]])
  f <- tempfile(fileext = ".pdf"); grDevices::pdf(f, width = 8.27, height = 11.69); graphics::plot.new()
  txt <- c(sprintf("ESG ratings and stock returns study %d", i), "Alice Example, Bob Sample", sprintf("https://doi.org/10.9999/test.%d", i), "",
           "Abstract", "We study ESG ratings and returns with a panel of firms.", "", "1. Introduction", "Text of the open copy.", "", "References",
           "Aria, M., & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive science mapping analysis. Journal of Informetrics, 11(4), 959-975.")
  for (j in seq_along(txt)) graphics::text(0, 1 - j * 0.05, txt[j], adj = 0, cex = 0.9)
  grDevices::dev.off(); b <- readBin(f, "raw", file.size(f)); unlink(f); cache[[k]] <<- b; b } })
mock_find <- function(url, query) {
  if (grepl("api.openalex.org/works$", url) && grepl("abstract_inverted_index", query$select %||% "") && !is.null(query$search)) {
    pool <- lapply(seq_along(MOCK_WORKS), function(i) { w <- MOCK_WORKS[[i]]; w$fwci <- round(0.3 + (i %% 10) / 4, 2); w$is_retracted <- i == 9; w })
    f <- query$filter %||% ""
    if (grepl("is_oa:true", f, fixed = TRUE)) pool <- Filter(function(w) isTRUE(w$open_access$is_oa), pool)
    if (grepl("cited_by_count:>", f, fixed = TRUE)) { m <- as.integer(sub(".*cited_by_count:>(\\d+).*", "\\1", f)); pool <- Filter(function(w) w$cited_by_count > m, pool) }
    if (identical(query$sort, "cited_by_count:desc")) pool <- pool[order(-vapply(pool, function(w) w$cited_by_count, 1L))]
    if (identical(query$sort, "publication_date:desc")) pool <- pool[order(-vapply(pool, function(w) w$publication_year, 1L))]
    if (identical(query$sort, "fwci:desc")) pool <- pool[order(-vapply(pool, function(w) w$fwci, 1))]
    return(list(meta = list(count = length(pool) * 7L), results = utils::head(pool, as.integer(query$per_page %||% 25)))) }
  if (grepl("^https://repository.example.org/\\d+\\.pdf$", url)) { i <- as.integer(sub(".*/(\\d+)\\.pdf$", "\\1", url))
    if (i %% 6 == 0) return("<!doctype html><html><body>Please accept the terms to download</body></html>")   # a landing page, not a PDF
    return(mock_pdf(i)) }
  if (grepl("^https://doi.org/", url)) { acc <- query$.headers$Accept %||% ""; style <- sub(".*style=([^;]+).*", "\\1", acc); d <- sub("^https://doi.org/", "", url)
    if (grepl("nature", style)) return(structure(list(), class = "anchor_404"))
    return(sprintf("%sLim, W. M., &amp; Kumar, S. (2024). <i>Title of %s</i> [style %s]. Journal X, 15(4), 616-626. https://doi.org/%s\n", if (style == "ieee") "[1]" else "", d, style, d)) }
  NULL }
local({ base <- mock_http; mock_http <<- function(url, query) { x <- mock_find(url, query); if (!is.null(x)) x else base(url, query) } })

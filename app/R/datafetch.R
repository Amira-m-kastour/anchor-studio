# ANCHOR Studio - data for the analysis step (phase O): imports and free open-data sources
# Licensed data (LSEG, WRDS, FactSet, Bloomberg, ...) is imported from the researcher's own exports and is not
# written into project files unless the researcher chooses so. Open data is fetched live with its provenance.

FETCH_SOURCES <- c("World Bank: World Development Indicators" = "worldbank", "FRED: economic time series (free API key)" = "fred",
                   "Kenneth French Data Library: factors" = "french", "Yahoo Finance: prices (teaching and pilots only)" = "yahoo",
                   "GDELT: news tone, last 3 months" = "gdelt", "SEC EDGAR: US firm fundamentals and SIC codes (free; contact email)" = "sec",
                   "IMF Data API: macro series (free)" = "imf")
FRENCH_SETS <- c("Fama/French 3 factors (US, monthly)" = "F-F_Research_Data_Factors", "Fama/French 3 factors (US, daily)" = "F-F_Research_Data_Factors_daily",
                 "Fama/French 5 factors 2x3 (US, monthly)" = "F-F_Research_Data_5_Factors_2x3", "Fama/French 5 factors 2x3 (US, daily)" = "F-F_Research_Data_5_Factors_2x3_daily",
                 "Developed markets 5 factors (monthly)" = "Developed_5_Factors", "Developed markets 5 factors (daily)" = "Developed_5_Factors_Daily",
                 "Emerging markets 5 factors (monthly)" = "Emerging_5_Factors")

# raw download with the same test hook as http_json (a mock may return a raw vector)
http_raw <- function(url, cfg) {
  mock <- get_mock(); if (is.function(mock)) return(mock(url, list()))
  pace(url); req <- httr2::request(url) |> httr2::req_user_agent(cfg$ua) |> httr2::req_timeout(60) |> httr2::req_retry(max_tries = 2)
  httr2::resp_body_raw(httr2::req_perform(req))
}
sha256_of <- function(x) { if (is.character(x) && length(x) == 1 && file.exists(x)) x <- readBin(x, "raw", file.size(x)); as.character(openssl::sha256(if (is.raw(x)) x else charToRaw(paste(x, collapse = "\n")))) }
df_sha <- function(df) sha256_of(paste(utils::capture.output(utils::write.csv(df, stdout(), row.names = FALSE)), collapse = "\n"))
split_ids <- function(x) { x <- trimws(unlist(strsplit(x %||% "", "[,; ]+"))); x[nzchar(x)] }

new_dataset <- function(name, df, source, query, licensed = FALSE, file_sha = NULL, note = "") {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  list(name = make.names(name), source = source, query = query, licensed = licensed, note = note,
       retrieved = format(Sys.time(), "%Y-%m-%d %H:%M"), rows = nrow(df), cols = ncol(df),
       sha256 = file_sha %||% df_sha(df), data = df)
}

import_dataset <- function(path, filename, name = NULL, licensed = TRUE) {
  ext <- tolower(tools::file_ext(filename))
  df <- switch(ext, csv = utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
               txt = , tsv = utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE),
               xlsx = , xls = as.data.frame(readxl::read_excel(path)), rds = as.data.frame(readRDS(path)),
               stop("Use a CSV, TSV, Excel or RDS file."))
  names(df) <- make.unique(make.names(names(df)))
  for (k in names(df)) if (is.character(df[[k]])) { d <- suppressWarnings(as.Date(df[[k]], optional = TRUE)); if (mean(!is.na(d[nzchar(df[[k]])])) > 0.9 && any(!is.na(d))) df[[k]] <- d }
  new_dataset(name %|e|% tools::file_path_sans_ext(filename), df, source = "Imported file", query = filename, licensed = licensed, file_sha = sha256_of(path),
              note = if (licensed) "Marked as licensed: kept out of project files and analysis packages unless you include it." else "")
}

fetch_worldbank <- function(indicators, countries, from, to, cfg) {
  ind <- split_ids(indicators); cty <- paste(tolower(split_ids(countries %|e|% "all")), collapse = ";")
  if (!length(ind)) stop("Give at least one indicator code, for example NY.GDP.PCAP.KD.")
  rows <- list()
  for (i in ind) { page <- 1; repeat {
    x <- http_json(sprintf("https://api.worldbank.org/v2/country/%s/indicator/%s", cty, i),
                   list(format = "json", date = paste0(from, ":", to), per_page = 20000, page = page), cfg)
    if (length(x) < 2 || is.null(x[[2]])) { if (page == 1) stop("The World Bank returned no data for ", i, " (check the code and countries).") else break }
    rows[[length(rows) + 1]] <- do.call(rbind, lapply(x[[2]], function(r) data.frame(country = r$country$value %||% NA, iso3 = r$countryiso3code %||% NA,
      year = as.integer(r$date %||% NA), indicator = r$indicator$id %||% i, value = if (is.null(r$value)) NA_real_ else as.numeric(r$value))))
    if (page >= (x[[1]]$pages %||% 1)) break; page <- page + 1 } }
  long <- do.call(rbind, rows)
  wide <- stats::reshape(long, idvar = c("country", "iso3", "year"), timevar = "indicator", direction = "wide")
  names(wide) <- sub("^value\\.", "", names(wide)); names(wide) <- make.names(names(wide))
  wide <- wide[order(wide$country, wide$year), ]; rownames(wide) <- NULL
  new_dataset(paste0("wdi_", gsub("[^A-Za-z0-9]+", "_", paste(ind, collapse = "_"))), wide, "World Bank World Development Indicators (api.worldbank.org, v2)",
              sprintf("indicators=%s; countries=%s; years=%s-%s", paste(ind, collapse = ","), cty, from, to), note = "World Bank data: CC BY 4.0; cite the World Bank as the source.")
}

fetch_fred <- function(series, key, from, to, cfg) {
  s <- split_ids(series); if (!length(s)) stop("Give at least one FRED series id, for example DGS10.")
  if (!nzchar(key %||% "")) stop("FRED needs a free API key: create one at fred.stlouisfed.org and enter it in Settings.")
  out <- NULL
  for (id in s) {
    x <- http_json("https://api.stlouisfed.org/fred/series/observations", list(series_id = id, api_key = key, file_type = "json",
                   observation_start = as.character(from), observation_end = as.character(to)), cfg)
    d <- data.frame(date = as.Date(vapply(x$observations, `[[`, "", "date")),
                    v = suppressWarnings(as.numeric(vapply(x$observations, function(o) o$value %||% ".", ""))))
    names(d)[2] <- make.names(id); out <- if (is.null(out)) d else merge(out, d, by = "date", all = TRUE)
  }
  new_dataset(paste0("fred_", gsub("[^A-Za-z0-9]+", "_", paste(s, collapse = "_"))), out, "FRED, Federal Reserve Bank of St. Louis (api.stlouisfed.org)",
              sprintf("series=%s; %s to %s", paste(s, collapse = ","), from, to), note = "Missing values ('.') are NA. Cite FRED and each series' original source.")
}

parse_french_csv <- function(lines) {
  h <- grep("Mkt-RF", lines)[1]; if (is.na(h)) stop("Unexpected file layout from the French Data Library.")
  hdr <- trimws(strsplit(lines[h], ",")[[1]]); hdr[1] <- "date"
  body <- character(0)
  for (l in lines[(h + 1):length(lines)]) { f <- trimws(strsplit(l, ",")[[1]]); if (!length(f) || !grepl("^[0-9]{6,8}$", f[1])) break; body <- c(body, l) }
  df <- utils::read.csv(text = body, header = FALSE, col.names = make.names(hdr), strip.white = TRUE)
  d <- as.character(df$date)
  df$date <- if (all(nchar(d) == 8)) as.Date(d, "%Y%m%d") else as.Date(paste0(d, "01"), "%Y%m%d")
  for (k in names(df)[-1]) df[[k]] <- as.numeric(df[[k]]) / 100   # the library reports percent
  df
}
fetch_french <- function(set, cfg) {
  url <- sprintf("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/%s_CSV.zip", set)
  raw <- http_raw(url, cfg); tf <- tempfile(fileext = ".zip"); writeBin(raw, tf)
  ex <- tempfile("ff"); utils::unzip(tf, exdir = ex); f <- list.files(ex, full.names = TRUE, recursive = TRUE)[1]
  df <- parse_french_csv(readLines(f, warn = FALSE))
  new_dataset(paste0("ff_", gsub("[^A-Za-z0-9]+", "_", set)), df, "Kenneth R. French Data Library (Dartmouth)", sprintf("file=%s_CSV.zip", set),
              note = "Returns converted from percent to decimals. Cite Fama & French and the Data Library.")
}

fetch_yahoo <- function(tickers, from, to, cfg) {
  tk <- split_ids(tickers); if (!length(tk)) stop("Give at least one ticker, for example COMI.CA.")
  p1 <- as.integer(as.POSIXct(as.Date(from))); p2 <- as.integer(as.POSIXct(as.Date(to) + 1))
  out <- list()
  for (t in tk) {
    x <- http_json(paste0("https://query1.finance.yahoo.com/v8/finance/chart/", utils::URLencode(t, reserved = TRUE)), list(period1 = p1, period2 = p2, interval = "1d"), cfg)
    r <- x$chart$result[[1]]; if (is.null(r)) stop("Yahoo returned no data for ", t)
    num <- function(v) vapply(v %||% list(), function(z) if (is.null(z)) NA_real_ else as.numeric(z), 0)
    q <- r$indicators$quote[[1]]; adj <- r$indicators$adjclose[[1]]$adjclose
    d <- data.frame(ticker = t, date = as.Date(as.POSIXct(unlist(r$timestamp), origin = "1970-01-01", tz = "UTC")), close = num(q$close),
                    adjusted = if (!is.null(adj)) num(adj) else num(q$close), volume = num(q$volume))
    d$return <- c(NA, diff(d$adjusted) / utils::head(d$adjusted, -1)); out[[t]] <- d
  }
  new_dataset(paste0("yahoo_", gsub("[^A-Za-z0-9]+", "_", paste(tk, collapse = "_"))), do.call(rbind, out), "Yahoo Finance (unofficial chart endpoint)",
              sprintf("tickers=%s; %s to %s", paste(tk, collapse = ","), from, to), note = "For teaching and pilots only: Yahoo data is for personal use and may change without notice. Use licensed data for research.")
}

fetch_gdelt <- function(query, cfg) {
  if (!nzchar(trimws(query %||% ""))) stop("Give a search query, for example \"Commercial International Bank\" sustainability.")
  x <- http_json("https://api.gdeltproject.org/api/v2/doc/doc", list(query = query, mode = "timelinetone", format = "json", timespan = "3months"), cfg)
  s <- x$timeline[[1]]$data; if (!length(s)) stop("GDELT returned no articles for this query in the last three months.")
  df <- data.frame(date = as.Date(substr(vapply(s, function(p) as.character(p$date), ""), 1, 8), "%Y%m%d"), tone = vapply(s, function(p) as.numeric(p$value %||% NA), 0))
  new_dataset(paste0("gdelt_", substr(gsub("[^A-Za-z0-9]+", "_", query), 1, 30)), df, "GDELT DOC 2.0 API (timelinetone)", sprintf("query=%s; last 3 months", query),
              note = "Average tone of matching online news, last three months only; not equivalent to TruValue Labs ESG scores. Validate before use.")
}

merge_datasets <- function(a, b, keys, name) {
  m <- merge(a$data, b$data, by = keys, all.x = TRUE, suffixes = c("", paste0(".", b$name)))
  new_dataset(name %|e|% paste0(a$name, "_x_", b$name), m, "Merged in ANCHOR Studio", sprintf("%s LEFT JOIN %s BY %s", a$name, b$name, paste(keys, collapse = ", ")),
              licensed = isTRUE(a$licensed) || isTRUE(b$licensed))
}
provenance_df <- function(ds) if (!length(ds)) NULL else do.call(rbind, lapply(ds, function(d) data.frame(Dataset = d$name, Source = d$source, Query = d$query,
  Retrieved = d$retrieved, Rows = d$rows, Columns = d$cols, Licensed = if (isTRUE(d$licensed)) "yes" else "no", SHA256 = substr(d$sha256, 1, 16))))

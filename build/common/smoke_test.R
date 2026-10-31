# Smoke test run by the build on each platform before packaging.
#   Rscript smoke_test.R <app folder> [<package library>]
args <- commandArgs(trailingOnly = TRUE)
smoke_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
app_dir <- normalizePath(args[1])
if (length(args) > 1) .libPaths(c(normalizePath(args[2]), .libPaths()))
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
fails <- character(0)
# a picture "has content" when its pixels are not all the same colour
has_content <- function(f) {
  if (!file.exists(f)) return(FALSE)
  if (requireNamespace("png", quietly = TRUE)) { m <- png::readPNG(f); return(stats::sd(as.numeric(m[, , 1:min(3, dim(m)[3])])) > 0.02) }
  file.size(f) > 2000
}
step <- function(name, expr) {
  t0 <- Sys.time()
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) { cat("  FAIL", name, ":", conditionMessage(e), "\n"); FALSE })
  if (ok) cat(sprintf("  ok   %-34s %5.1fs\n", name, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  else fails <<- c(fails, name)
}

cat("R", as.character(getRversion()), "on", R.version$platform, "\n")
app <- NULL
step("load app (packages, UI, server)", { old <- setwd(app_dir); on.exit(setwd(old)); app <- source("app.R")$value; stopifnot(inherits(app, "shiny.appobj")) })

data(management, package = "bibliometrixData")
Rres <- bibliometrix::biblioAnalysis(management, sep = ";")
for (id in names(ANALYSES)) step(paste("analysis", id), {
  out <- run_analysis(id, management, Rres, list(field = "DE", nnodes = 30))
  if (out$kind == "base") { tf <- tempfile(fileext = ".png"); grDevices::png(tf, 1200, 800, res = 120); out$plot(); grDevices::dev.off(); stopifnot(has_content(tf)) }
  if (out$kind == "gg") { tf <- tempfile(fileext = ".png"); ggplot2::ggsave(tf, out$plot, width = 8, height = 5, dpi = 80, device = gg_device()); stopifnot(has_content(tf)) }
})

step("read a PDF (pdftools)", {
  tf <- tempfile(fileext = ".pdf"); grDevices::pdf(tf); plot.new(); text(.5, .5, "Test article 10.1007/s11142-022-09675-3"); grDevices::dev.off()
  a <- read_article(tf, "test.pdf"); stopifnot(a$doi == "10.1007/s11142-022-09675-3")
})
step("report HTML + Word + disclosure", {
  st <- new_state(); st$setup <- list(S_name = "Smoke Test"); st$article <- list(citation = "Test, A. (2026). Title. Journal, 1(1), 1-2.", title = "Title")
  st$biblio$M <- management
  h <- build_report_html(st); stopifnot(nchar(h) > 1000)
  tf <- tempfile(fileext = ".docx"); build_report_docx(st, tf); stopifnot(file.size(tf) > 5000)
  stopifnot(grepl("Smoke Test", disclosure_text(st)))
})
step("figure as PDF (vector)", {
  o <- run_analysis("annual_prod", management, Rres, list()); tf <- tempfile(fileext = ".pdf")
  save_pdf_fig(o, tf); stopifnot(file.size(tf) > 3000, identical(readBin(tf, "raw", 4), charToRaw("%PDF")))
})
step("LaTeX package + references.bib", {
  st <- new_state(); st$setup <- list(S_name = "Smoke Test"); st$article <- list(citation = "Test, A., & Other, B. (2026). Title. Journal, 1(2), 1-2.", title = "Title & 100% test")
  st$biblio$M <- management; st$phases$A$draft <- "## Claim\n\nText with & % $ # _ { } \\ and α.\n\n| A | B |\n|---|---|\n| 1 | 2 |"
  tf <- tempfile(fileext = ".zip"); build_latex_zip(st, tf, app_dir)
  z <- zip::zip_list(tf)$filename; stopifnot(any(grepl("main.tex$", z)), any(grepl("content.tex$", z)), any(grepl("references.bib$", z)), any(grepl("latexmkrc$", z)))
  b <- build_bib(st); stopifnot(sum(gregexpr("\n@", b)[[1]] > 0) >= nrow(management))
})
step("PDF report (Typst)", {
  ty <- find_typst(app_dir); if (is.null(ty)) stop("typst not found")
  st <- new_state(); st$setup <- list(S_name = "Smoke Test"); st$article <- list(citation = "Test, A. (2026). Title.", title = "Title")
  st$phases$A$draft <- "## Claim\n\n**Bold** #not-code $x$ α [link](https://example.org)\n\n- a\n- b"
  tf <- tempfile(fileext = ".pdf"); stopifnot(build_report_pdf(st, tf, "typst", app_dir) == "typst", file.size(tf) > 5000)
})
step("lessons + references in every report", {
  st <- new_state(); st$setup <- list(S_name = "Smoke Test"); st$article <- list(citation = "Test, A. (2026). Title.", title = "Title", doi = "10.1/x")
  st$phases$C$my_lessons <- "My own lesson \u03b1"
  h <- build_report_html(st); stopifnot(lengths(regmatches(h, gregexpr("Lessons learned &amp; what", h))) == 6, grepl("10.1016/j.jbusres.2021.04.070", h, fixed = TRUE), grepl("My own lesson", h))
  b <- build_bib(st); stopifnot(grepl("@article{donthu2021", b, fixed = TRUE), grepl("@article{flemyng2025", b, fixed = TRUE))
  tf <- tempfile(fileext = ".docx"); build_report_docx(st, tf); stopifnot(file.size(tf) > 5000)
  stopifnot(grepl("cite{donthu2021", build_content_tex(st, character(0)), fixed = TRUE))
})
step("live features with test data (Crossref/OpenAlex shapes)", {
  e <- new.env(); sys.source(file.path(smoke_dir, "mock_online.R"), e)
  options(anchor.http_mock = e$mock_http); on.exit(options(anchor.http_mock = NULL), add = TRUE)
  cfg <- online_cfg(list()); r <- online_refresh(cfg, "10.9999/retracted.1")
  stopifnot(isTRUE(r$online), length(r$refs) == length(REFS), r$focal$status == "retracted", nrow(r$suggestions$C) == 3)
  stopifnot(crossref_lookup("10.9999/corrected.1", cfg)$status == "corrected")
  d <- online_dataset_check(data.frame(DI = c("10.1/a", "10.9999/retracted.1"), SR = c("A", "B")), cfg); stopifnot(nrow(d$flagged) == 1)
  st <- new_state(); st$online <- r; stopifnot(grepl("verified live", online_note(st)), ref_live("khraisha2024", st)$vol == "15")
})
step("open-data fetchers with test data (World Bank, FRED, French, Yahoo, GDELT)", {
  e <- new.env(); sys.source(file.path(smoke_dir, "mock_online.R"), e)
  options(anchor.http_mock = e$mock_http); on.exit(options(anchor.http_mock = NULL), add = TRUE)
  cfg <- online_cfg(list())
  w <- fetch_worldbank("NY.GDP.PCAP.KD", "egy", 2010, 2025, cfg); stopifnot(nrow(w$data) > 0, !isTRUE(w$licensed))
  f <- fetch_french("F-F_Research_Data_Factors", cfg); stopifnot(nrow(f$data) > 0, all(abs(f$data[[2]]) < 1, na.rm = TRUE))
  fr <- fetch_fred("DGS10", "TESTKEY", "2020-01-01", "2020-12-31", cfg); stopifnot(nrow(fr$data) > 0)
  y <- fetch_yahoo("MSFT", "2020-01-01", "2020-12-31", cfg); stopifnot(nrow(y$data) > 0)
  g <- fetch_gdelt("ESG", cfg); stopifnot(nrow(g$data) > 0)
  stopifnot(nrow(provenance_df(list(w, f))) == 2)
})
step("statistical methods on simulated data (fixest, sandwich, metafor, nlme, lavaan)", {
  set.seed(1); n <- 2000
  d <- data.frame(firm = sample(1:200, n, TRUE), date = sample(1:100, n, TRUE)); d$industry <- d$firm %% 11
  for (v in c("log_mcap", "mtb", "roe")) d[[v]] <- rnorm(n); d$leverage <- runif(n)
  d$rating <- rnorm(n, 50, 10); d$disagreement <- abs(rnorm(n, 8, 3)); d$news_score <- 50 + .2 * d$rating + rnorm(n, 0, 8)
  d$high_disagreement <- as.integer(d$disagreement > 8)
  r <- run_method("regress", d, TEMPLATES[[1]]); stopifnot(nrow(r$table) >= 7, grepl("feols", r$code))
  f <- data.frame(date = seq(as.Date("2010-01-01"), by = "month", length.out = 120)); for (v in c("Mkt.RF", "SMB", "HML", "RMW", "CMA")) f[[v]] <- rnorm(120, 0, .03)
  f$port_excess <- .003 + f$Mkt.RF + rnorm(120, 0, .01); a <- run_method("alpha", f, TEMPLATES[[grep("Eq. 4", names(TEMPLATES), fixed = TRUE)]]); stopifnot(grepl("NeweyWest", a$code))
  ma <- data.frame(study = paste("S", 1:20), yi = rnorm(20, .3, .2), vi = runif(20, .01, .05)); m <- run_method("meta", ma, TEMPLATES[[grep("Meta-analysis", names(TEMPLATES), fixed = TRUE)]]); stopifnot(length(m$plots) == 2)
  ml <- data.frame(g = rep(1:30, each = 20)); ml$x <- rnorm(600); ml$y <- rnorm(30)[ml$g] + .5 * ml$x + rnorm(600)
  stopifnot(nrow(run_method("multilevel", ml, list(fixed = "y ~ x", random = "~ 1 | g"))$table) == 2)
  s <- data.frame(f0 = rnorm(400)); for (k in 1:3) s[[paste0("e", k)]] <- .7 * s$f0 + rnorm(400, 0, .6)
  stopifnot(nrow(run_method("sem", s, list(model = "F =~ e1 + e2 + e3"))$table) > 0)
  stopifnot(inherits(tryCatch(run_method("regress", d, list(y = "nope", rhs = "roe", fe = "", family = "ols")), error = function(e) e), "error"))
  st <- new_state(); st$analysis$datasets <- list(lic = new_dataset("lic", d, "Imported file", "x.csv", licensed = TRUE))
  r$dataset <- "lic"; st$analysis$results <- list(r); z <- tempfile(fileext = ".zip"); build_analysis_zip(st, z, include_licensed = FALSE)
  zl <- zip::zip_list(z)$filename; stopifnot(any(grepl("analysis.R$", zl)), !any(grepl("data/lic.csv", zl)))
})
step("OpenAlex corpus, focal links, trend and open copies (test data)", {
  e <- new.env(); sys.source(file.path(smoke_dir, "mock_online.R"), e)
  options(anchor.http_mock = e$mock_http); on.exit(options(anchor.http_mock = NULL), add = TRUE); cfg <- online_cfg(list())
  r <- oa_corpus("both", cfg, doi = "10.1007/s11142-022-09675-3", max = 1000); stopifnot(nrow(r$M) == 120, all(r$M$DB == "OPENALEX"), sum(r$log$records) == 120)
  R <- bibliometrix::biblioAnalysis(r$M, sep = ";"); for (id in c("main_info", "annual_prod", "rel_sources", "cocit_net", "coupling_net", "cooc_net", "thematic_map"))
    if (inherits(tryCatch(run_analysis(id, r$M, R), error = function(e) e), "error")) stop("analysis failed on OpenAlex records: ", id)
  stopifnot(inherits(tryCatch(run_analysis("rpys", r$M, R), error = function(e) e), "error"))
  f <- focal_links("10.1007/s11142-022-09675-3", cfg); stopifnot(f$n_refs == 30, nrow(f$citing) > 0, !is.null(f$open), nrow(f$journal) == 1)
  stopifnot(nrow(oa_open_copies(r$M$DI, cfg)) == 120, nrow(oa_trend("esg", cfg, 2015, 2025)) == 11)
})
step("SEC EDGAR, IMF, ROR, ORCID and region (test data)", {
  e <- new.env(); sys.source(file.path(smoke_dir, "mock_online.R"), e)
  options(anchor.http_mock = e$mock_http); on.exit(options(anchor.http_mock = NULL), add = TRUE)
  stopifnot(inherits(tryCatch(fetch_sec("TEST", 2019, 2023, online_cfg(list())), error = function(e) e), "error"))   # SEC needs a contact
  cfg <- online_cfg(list(crossref_mailto = "test@example.org"))
  s <- fetch_sec("TEST", 2019, 2023, cfg); stopifnot(nrow(s$data) == 5, all(c("roe", "leverage", "sic") %in% names(s$data)), abs(s$data$assets[1] - 1000) < 1e-6)
  im <- fetch_imf("CPI", "EGY.CPI._T.IX.M", "2024-M01", cfg); stopifnot(nrow(im$data) == 6)
  for (cc in c("EG", "DE", "FR")) unlink(ror_cache_file(cc))
  stopifnot(nrow(ror_country("EG", cfg)) == 21)                                         # small country: complete list
  de <- ror_country("DE", cfg); stopifnot(nrow(de) == 200, attr(de, "total") == 420)     # large country: starter list + total
  stopifnot(nrow(ror_country("FR", cfg)) == 1)                                          # any other country works the same way
  h <- ror_search("hagen", cfg, "DE"); stopifnot(grepl("FernUniversit", h$name[1]), grepl("education", h$types[1]))   # type-ahead, education first
  stopifnot(grepl("Hagen", ror_by_domain("studium.fernuni-hagen.de", cfg)$name), grepl("Technology", ror_by_domain("me@tit.edu", cfg)$name))
  stopifnot(inherits(tryCatch(ror_by_domain("x@unknown.edu.eg", cfg), error = function(e) e), "error"))           # never matches a bare suffix
  for (cc in c("EG", "DE", "FR")) unlink(ror_cache_file(cc))
  old <- if (file.exists(inst_db_file())) readRDS(inst_db_file()); on.exit(if (is.null(old)) unlink(inst_db_file()) else saveRDS(old, inst_db_file()), add = TRUE)
  save_institution_dbs("Test University of Egypt 1", "https://ror.org/0test001", "tue1.edu.eg", c("scopus", "lseg_ws"))
  stopifnot(identical(institution_dbs("Test University of Egypt 1", "", "https://ror.org/0test001")$dbs, c("scopus", "lseg_ws")), is.null(institution_dbs("Another University", "", "")))
  stopifnot(orcid_ok("0000-0002-1825-0097"), !orcid_ok("0000-0002-1825-0098"), orcid_ok("https://orcid.org/0000-0002-1825-0097"), !orcid_ok("0000-0002-1825"))
  stopifnot(orcid_name("0000-0002-1825-0097", cfg) == "Test Researcher", grepl("not public", orcid_name("0000-0001-5109-3700", cfg)),
            inherits(tryCatch(orcid_name("0000-0003-1415-9269", cfg), error = function(e) e), "error"))
  # region without the network: time zone (incl. old names), Windows region setting, start-up language; never the app's own UTF-8 switch
  stopifnot(detect_country_tz("Africa/Cairo")$cc == "EG", detect_country_tz("Egypt")$cc == "EG", detect_country_tz("Asia/Calcutta")$cc == "IN",
            detect_country_tz(NA, win_geo = "DE", lang = "")$cc == "DE", detect_country_tz("UTC", win_geo = character(0), lang = c("", "fr_FR.UTF-8"))$cc == "FR",
            detect_country_tz("UTC", win_geo = character(0), lang = "English_Egypt.1252")$cc == "EG", is.na(detect_country_tz("UTC", win_geo = character(0), lang = c("C.UTF-8", "C"))$cc))
  stopifnot(detect_country_net(cfg)$cc == "EG", is.list(detect_country_tz()))
})
step("Before you start: appendix and data statement in every report", {
  st <- new_state(); st$setup <- list(S_name = "Smoke Test"); st$article <- list(citation = "Test, A. (2026). Title.", title = "Title")
  st$prereq <- utils::modifyList(new_prereq(), list(name = "Smoke Test", orcid = "0000-0002-1825-0097", institution = "FernUniversit\u00e4t in Hagen", dbs = institution_dbs("", "fernuni-hagen.de")$dbs, ethics = "none"))
  stopifnot(identical(raters_available(st$prereq), "LSEG"))
  st$analysis$datasets <- list(a = new_dataset("a", data.frame(x = 1:3), "World Bank", "q"), b = new_dataset("b", data.frame(x = 1:3), "Imported file", "f.csv", licensed = TRUE))
  h <- build_report_html(st); stopifnot(grepl("Appendix: researcher, resources and licences", h, fixed = TRUE), grepl("Data availability statement", h, fixed = TRUE), grepl("LSEG Workspace for Students", h, fixed = TRUE))
  tf <- tempfile(fileext = ".docx"); build_report_docx(st, tf); stopifnot(file.size(tf) > 5000)
  stopifnot(grepl("Appendix: researcher", build_content_tex(st, character(0)), fixed = TRUE))
  ty <- find_typst(app_dir); if (!is.null(ty)) { tp <- tempfile(fileext = ".pdf"); stopifnot(build_report_pdf(st, tp, "typst", app_dir) == "typst", file.size(tp) > 5000) }
  d <- data.frame(t = rep(1:3, 4), a = rnorm(12), b = rnorm(12)); b <- build_rating_vars(d, c("a", "b"), "t"); stopifnot(all(c("rating", "disagreement", "high_disagreement") %in% names(b)))
})
step("Find a focal article and citation styles (test data)", {
  e <- new.env(); sys.source(file.path(smoke_dir, "mock_online.R"), e)
  options(anchor.http_mock = e$mock_http); on.exit(options(anchor.http_mock = NULL), add = TRUE); cfg <- online_cfg(list())
  r <- find_search(cfg, "esg rating disagreement", 2000, 2026, c("article", "review"), "en", FALSE, 0, "relevance", 50)
  stopifnot(nrow(r$rows) == 50, r$found > 50, sum(r$rows$retracted) == 1, nzchar(r$rows$abstract[1]), !is.na(r$rows$fwci[1]))
  o <- find_search(cfg, "esg", NULL, NULL, NULL, "", TRUE, 50, "cited_by_count:desc", 50)
  stopifnot(all(nzchar(o$rows$pdf_url) | nzchar(o$rows$oa_url)), all(o$rows$cites >= 50), !is.unsorted(rev(o$rows$cites)))
  stopifnot(inherits(tryCatch(find_search(cfg, "  "), error = function(e) e), "error"))
  w3 <- r$rows[r$rows$id == "W4000000003", ]; w6 <- r$rows[r$rows$id == "W4000000006", ]
  f <- find_download(w3, cfg); on.exit(unlink(f$path), add = TRUE)
  stopifnot(file.exists(f$path), startsWith(normalizePath(f$path), normalizePath(find_dir())), identical(rawToChar(readBin(f$path, "raw", 4)), "%PDF"))
  a <- read_article(f$path, basename(f$path)); stopifnot(a$doi == "10.9999/test.3")
  stopifnot(inherits(tryCatch(find_download(w6, cfg), error = function(e) e), "error"))                       # landing page, not a PDF
  stopifnot(inherits(tryCatch(find_download(r$rows[r$rows$pdf_url == "", ][1, ], cfg), error = function(e) e), "error"))   # paywalled: no download
  stopifnot(library_link("10.1/x", "https://proxy.example.edu/login?url=") == "https://proxy.example.edu/login?url=https://doi.org/10.1/x", library_link("10.1/x", "") == "https://doi.org/10.1/x")
  stopifnot(fit_score(r$rows[r$rows$retracted, ], names(FIND_CRITERIA), r$rows) == 0, fit_score(w3, names(FIND_CRITERIA), r$rows) > fit_score(w3, character(0), r$rows))
  stopifnot(grepl("Candidate 2", find_summary_prompt(r$rows[1:3, ])), grepl("Do not rank", find_summary_prompt(r$rows[1:3, ])))
  m <- crossref_lookup("10.1234/abc", cfg)$meta
  stopifnot(startsWith(cite_doi("10.1234/abc", "apa", cfg, m), "Lim, W. M."), grepl("style ieee", cite_doi("10.1234/abc", "ieee", cfg, m)),
            !grepl("^\\[1\\]|<i>|&amp;", cite_doi("10.1234/abc", "ieee", cfg, m)), grepl("style harvard-cite-them-right", cite_doi("10.1234/abc", "harvard-cite-them-right", cfg, m)),
            inherits(tryCatch(cite_doi("10.1234/abc", "nature", cfg, m), error = function(e) e), "error"))
  a1 <- mark_auto(list(), "X"); a1 <- mark_auto(a1, "Y"); stopifnot(cite_is_auto("X", a1), cite_is_auto("Y", a1), !cite_is_auto("my edit", a1))
  st <- new_state(); st$setup <- list(S_name = "Smoke Test"); st$article <- list(citation = "Test, A. (2026). Title.", title = "Title")
  sh <- r$rows[1:3, ]; sh$fit <- c(80L, 50L, 20L)
  st$finder <- list(shortlist = sh, chosen = sh$id[2], chosen_at = "2026-10-27 10:00", query = "esg rating disagreement", found = r$found, filters = r$filters, sort = "relevance", reason = "Clear design")
  h <- build_report_html(st); stopifnot(grepl("How the focal article was chosen", h, fixed = TRUE), grepl("Clear design", h, fixed = TRUE), !grepl("%PDF|focal_candidates", h))
  stopifnot(!grepl("Tribute|Kastour family", h))
  tf <- tempfile(fileext = ".docx"); build_report_docx(st, tf); stopifnot(file.size(tf) > 5000)
  stopifnot(grepl("How the focal article was chosen", build_content_tex(st, character(0)), fixed = TRUE))
  st$prompts <- data.frame(time = "2026-10-27 10:00", phase = "Find", mode = "prompt", prompt = "p"); stopifnot(grepl("only to summarise the abstracts", disclosure_text(st)))
})
step("exports work as in the running app (without app.R's own objects)", {
  # shiny::runApp() evaluates app.R in its own environment, so functions in R/ must not rely on objects created in app.R
  own <- intersect(c("PHASE_IDS", "new_state", "q_input", "phase_shell", "analysis_ui", "dm_ui", "lessons_card", "theme", "ui", "server", "rscript_path"), ls(globalenv()))
  st <- new_state(); keep <- mget(own, envir = globalenv()); rm(list = own, envir = globalenv()); on.exit(list2env(keep, globalenv()), add = TRUE)
  st$setup <- list(S_name = "Smoke Test"); st$article <- list(citation = "Test, A. (2026). Title.", title = "Title", doi = "10.1/x"); st$biblio$M <- management
  st$phases$A$accepted <- list(list(title = "T", doi = "10.1/y", year = 2025, au = "A, B.", journal = "J"))
  stopifnot(nchar(build_report_html(st)) > 1000, nchar(build_bib(st)) > 100)
  tf <- tempfile(fileext = ".docx"); build_report_docx(st, tf); tz <- tempfile(fileext = ".zip"); build_latex_zip(st, tz, app_dir)
  ta <- tempfile(fileext = ".zip"); build_analysis_zip(st, ta); stopifnot(file.size(tf) > 5000, file.size(tz) > 1000, file.exists(ta))
  ty <- find_typst(app_dir); if (!is.null(ty)) { tp <- tempfile(fileext = ".pdf"); stopifnot(build_report_pdf(st, tp, "typst", app_dir) == "typst") }
})
# live check against the real services: informative only (a service outage must not fail a build)
local({ options(anchor.http_mock = NULL); cfg <- online_cfg(list()); t0 <- Sys.time()
  ok <- tryCatch(online_ping(cfg), error = function(e) FALSE)
  if (!ok) cat("  skip live internet check: Crossref not reachable from this machine\n") else {
    r <- tryCatch({ x <- crossref_lookup("10.1016/j.jbusres.2021.04.070", cfg); s <- openalex_suggest("bibliometric analysis guidelines", cfg, n = 3)
      sprintf("Crossref: %s (%s); OpenAlex suggestions: %d", substr(x$meta$title, 1, 50), x$status, NROW(s)) }, error = function(e) paste("problem:", conditionMessage(e)))
    cat(sprintf("  live internet check (%.1fs): %s\n", as.numeric(difftime(Sys.time(), t0, units = "secs")), r)) } })
step("app answers over HTTP", {
  port <- httpuv::randomPort()
  p <- processx::process$new(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
                             c("-e", sprintf("shiny::runApp('%s', port = %d, launch.browser = FALSE)", gsub("\\\\", "/", app_dir), port)),
                             stdout = "|", stderr = "|")
  on.exit(p$kill(), add = TRUE)
  ok <- FALSE
  for (k in 1:90) { Sys.sleep(1)
    r <- tryCatch(httr2::req_perform(httr2::request(sprintf("http://127.0.0.1:%d", port))), error = function(e) NULL)
    if (!is.null(r) && grepl("ANCHOR Studio", httr2::resp_body_string(r))) { ok <- TRUE; break } }
  if (!ok) stop("no answer; log: ", paste(p$read_all_error_lines(), collapse = " | "))
})

cat("\n", length(fails), "failure(s)\n")
if (length(fails)) { cat(paste(" -", fails), sep = "\n"); quit(status = 1) }

# ANCHOR Studio - methods library for the analysis step (phase O)
# Every run returns: label, table (data.frame), extra tables, printed summary, plots, and the R code that reproduces it.

METHODS <- c("Descriptive statistics and correlations" = "describe",
             "Regression: OLS / panel fixed effects / IV / logit / Poisson (fixest)" = "regress",
             "Event study: market-model abnormal returns" = "event",
             "Factor-model alpha (e.g. Fama-French), Newey-West errors" = "alpha",
             "Meta-analysis of effect sizes (metafor)" = "meta",
             "Difference-in-differences, staggered (Sun & Abraham, fixest)" = "did",
             "Multilevel / mixed model (nlme)" = "multilevel",
             "Structural equation model / CFA (lavaan)" = "sem")

TEMPLATES <- list(
  "S&Y (2023) Eq. 1: ESG news score on consensus rating x disagreement" = list(method = "regress", y = "news_score",
     rhs = "rating * disagreement + log_mcap + mtb + roe + leverage", fe = "industry + date", cluster = c("firm", "date"), split = "", family = "ols", needs_raters = TRUE,
     note = "As in the article: consensus = mean of standardised ratings, disagreement = their standard deviation across at least two raters (MSCI, Sustainalytics, Asset4). Build both under Data > Build rating variables."),
  "S&Y (2023) Eq. 2: 3-day return on positive news x high rating, split by disagreement" = list(method = "regress", y = "car_m1_p1",
     rhs = "positive_news * high_rating + log_mcap + mtb + roe + leverage", fe = "industry + date", cluster = c("firm", "date"), split = "high_disagreement", family = "ols", needs_raters = TRUE,
     note = "high_disagreement = 1 when disagreement is above its median; needs at least two raters."),
  "Disagreement option 1: second rater hand-collected (Eq. 1)" = list(method = "regress", y = "news_score",
     rhs = "rating * disagreement + log_mcap + mtb + roe + leverage", fe = "industry + date", cluster = c("firm", "date"), split = "", family = "ols",
     note = "Use when you have one licensed rater (e.g. LSEG ESG) and collect a second rating by hand for each firm (public rating pages, within their terms). Build rating and disagreement from the two columns under Data > Build rating variables. Closest to the article; report how and when the second rating was collected."),
  "Disagreement option 2: LSEG pillar dispersion as a proxy (Eq. 1)" = list(method = "regress", y = "news_score",
     rhs = "rating * pillar_dispersion + log_mcap + mtb + roe + leverage", fe = "industry + date", cluster = c("firm", "date"), split = "", family = "ols",
     note = "Uses the spread of one rater's E, S and G pillar scores instead of disagreement between raters. Build it under Data > Build rating variables (pillars). It measures uneven performance, not rater disagreement: state this as a limitation."),
  "Disagreement option 3: consensus only (Eq. 1 without disagreement)" = list(method = "regress", y = "news_score",
     rhs = "rating + log_mcap + mtb + roe + leverage", fe = "industry + date", cluster = c("firm", "date"), split = "", family = "ols",
     note = "Tests only whether the rating level relates to ESG news; drops the article's disagreement hypothesis. Use when no second rater can be obtained; say which hypotheses are not tested."),
  "S&Y (2023) Eq. 4: five-factor alpha of a rating-sorted portfolio" = list(method = "alpha", y = "port_excess",
     factors = c("Mkt.RF", "SMB", "HML", "RMW", "CMA"), time = "date", lag = 6),
  "Event study: CAR(-1,+1) around news days, market model" = list(method = "event", id = "firm", date = "date", ret = "ret", mkt = "mkt_ret", event = "event",
     est_from = -260, est_to = -11, ev_from = -1, ev_to = 1),
  "Meta-analysis: random effects on effect sizes coded in the review" = list(method = "meta", yi = "yi", vi = "vi", slab = "study", mods = character(0)))

# consensus and disagreement (or pillar dispersion) from two or more rating columns, standardised within a period
build_rating_vars <- function(df, cols, by = "", kind = "raters") {
  cols <- cols[nzchar(cols %||% "")]; if (length(cols) < 2) stop("Choose at least two columns (two raters, or the E, S and G pillars).")
  need_cols(df, c(cols, by)); z <- function(x) { s <- stats::sd(x, na.rm = TRUE); if (is.na(s) || s == 0) x * NA else (x - mean(x, na.rm = TRUE)) / s }
  Z <- sapply(cols, function(k) if (nzchar(by %||% "")) stats::ave(as.numeric(df[[k]]), df[[by]], FUN = z) else z(as.numeric(df[[k]])))
  if (is.null(dim(Z))) Z <- matrix(Z, nrow = 1)
  n <- rowSums(!is.na(Z)); df$rating <- ifelse(n >= 2, rowMeans(Z, na.rm = TRUE), NA)
  dis <- ifelse(n >= 2, apply(Z, 1, stats::sd, na.rm = TRUE), NA)
  if (kind == "pillars") df$pillar_dispersion <- dis else { df$disagreement <- dis; df$high_disagreement <- as.integer(dis > stats::median(dis, na.rm = TRUE)) }
  df$n_raters <- n; df$high_rating <- as.integer(df$rating > stats::median(df$rating, na.rm = TRUE)); df }

`%or%` <- function(a, b) if (is.null(a) || !length(a) || all(is.na(a)) || identical(a, "")) b else a
need_cols <- function(df, cols) { cols <- unique(cols[nzchar(cols)]); miss <- setdiff(cols, names(df))
  if (length(miss)) stop("These columns are not in the dataset: ", paste(miss, collapse = ", "), ". Rename them or choose the right columns.") }
rhs_vars <- function(rhs) all.vars(stats::as.formula(paste("~", rhs)))
qcode <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
code_header <- function(ds) sprintf("dat <- read.csv(\"data/%s.csv\")   # %s; %s", ds, "see provenance.csv for source and SHA-256", "dates may need as.Date()")
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", ""))))
tidy_ct <- function(ct, model = "") { ct <- as.data.frame(ct); if (!ncol(ct)) return(NULL)
  data.frame(model = model, term = rownames(ct), estimate = signif(ct[[1]], 4), std.error = signif(ct[[2]], 4), statistic = signif(ct[[3]], 4),
             p.value = signif(ct[[4]], 3), sig = stars(ct[[4]]), row.names = NULL) }

run_method <- function(method, df, o, ds_name = "dataset") {
  df <- as.data.frame(df)
  switch(method,
  describe = {
    num <- names(df)[vapply(df, is.numeric, TRUE)]; if (!length(num)) stop("The dataset has no numeric columns.")
    if (length(o$vars)) num <- intersect(o$vars, num)
    st <- do.call(rbind, lapply(num, function(v) { x <- df[[v]]; q <- stats::quantile(x, c(.25, .5, .75), na.rm = TRUE)
      data.frame(variable = v, n = sum(!is.na(x)), mean = signif(mean(x, na.rm = TRUE), 4), sd = signif(stats::sd(x, na.rm = TRUE), 4), min = signif(min(x, na.rm = TRUE), 4),
                 p25 = signif(q[[1]], 4), median = signif(q[[2]], 4), p75 = signif(q[[3]], 4), max = signif(max(x, na.rm = TRUE), 4)) }))
    cm <- if (length(num) > 1) round(stats::cor(df[num], use = "pairwise.complete.obs"), 3) else NULL
    list(label = "Descriptive statistics", table = st, tables = if (!is.null(cm)) list(Correlations = data.frame(variable = rownames(cm), cm, row.names = NULL, check.names = FALSE)),
         text = sprintf("%d observations, %d numeric variables.", nrow(df), length(num)),
         code = paste(code_header(ds_name), sprintf("vars <- c(%s)", paste(qcode(num), collapse = ", ")), "summary(dat[vars])", "round(cor(dat[vars], use = \"pairwise.complete.obs\"), 3)", sep = "\n"))
  },
  regress = {
    y <- o$y %or% stop("Choose the dependent variable."); rhs <- o$rhs %or% stop("Write the right-hand side, e.g. x1 * x2 + control.")
    fe <- trimws(o$fe %or% ""); iv <- trimws(o$iv %or% ""); cl <- o$cluster %or% character(0); sp <- o$split %or% ""
    need_cols(df, c(y, rhs_vars(rhs), if (nzchar(fe)) all.vars(stats::as.formula(paste("~", fe))), if (nzchar(iv)) all.vars(stats::as.formula(iv)), cl, sp))
    fml_txt <- paste0(y, " ~ ", rhs, if (nzchar(fe)) paste0(" | ", fe) else if (nzchar(iv)) "" else "", if (nzchar(iv)) paste0(" | ", iv) else "")
    fml <- stats::as.formula(fml_txt)
    vc <- if (length(cl)) stats::as.formula(paste("~", paste(cl, collapse = " + "))) else "hetero"
    fam <- o$family %or% "ols"
    args <- list(fml = fml, data = df, vcov = vc); if (nzchar(sp)) args$split <- stats::as.formula(paste("~", sp))
    m <- switch(fam, ols = do.call(fixest::feols, args), logit = do.call(fixest::feglm, c(args, list(family = stats::binomial("logit")))), poisson = do.call(fixest::fepois, args))
    ms <- if (inherits(m, "fixest_multi")) as.list(m) else list(all = m)
    tab <- do.call(rbind, Map(function(mm, nm) tidy_ct(fixest::coeftable(mm), nm), ms, names(ms)))
    fit <- do.call(rbind, Map(function(mm, nm) data.frame(model = nm, n = stats::nobs(mm), r2 = tryCatch(signif(fixest::r2(mm, "r2"), 4), error = function(e) NA),
                                                          within_r2 = tryCatch(signif(fixest::r2(mm, "wr2"), 4), error = function(e) NA)), ms, names(ms)))
    vtxt <- if (length(cl)) sprintf("~ %s", paste(cl, collapse = " + ")) else "\"hetero\""
    fun <- switch(fam, ols = "feols", logit = "feglm", poisson = "fepois")
    code <- paste(code_header(ds_name), "library(fixest)",
      sprintf("m <- %s(%s, data = dat, vcov = %s%s%s)", fun, fml_txt, vtxt, if (fam == "logit") ", family = binomial(\"logit\")" else "", if (nzchar(sp)) paste0(", split = ~ ", sp) else ""),
      "etable(m)   # tex = TRUE gives a LaTeX table", sep = "\n")
    list(label = sprintf("Regression (%s): %s", fam, fml_txt), table = tab, tables = list(`Fit statistics` = fit),
         text = paste(utils::capture.output(print(fixest::etable(m))), collapse = "\n"), code = code,
         plots = list(function() { tt <- tab[!grepl("Intercept", tab$term), ]; if (!nrow(tt)) return(invisible())
           k <- seq_len(min(nrow(tt), 25)); tt <- tt[k, ]; lo <- tt$estimate - 1.96 * tt$std.error; hi <- tt$estimate + 1.96 * tt$std.error
           graphics::par(mar = c(4, 14, 2, 1)); graphics::plot(tt$estimate, rev(k), xlim = range(c(lo, hi, 0)), yaxt = "n", pch = 19, xlab = "Estimate with 95% CI", ylab = "", main = "Coefficients")
           graphics::axis(2, rev(k), labels = paste(tt$model, tt$term), las = 1, cex.axis = .7); graphics::segments(lo, rev(k), hi, rev(k)); graphics::abline(v = 0, lty = 2) }))
  },
  event = {
    id <- o$id; dt <- o$date; r <- o$ret; mk <- o$mkt; ev <- o$event; need_cols(df, c(id, dt, r, mk, ev))
    ef <- as.integer(o$est_from %or% -260); et <- as.integer(o$est_to %or% -11); wf <- as.integer(o$ev_from %or% -1); wt <- as.integer(o$ev_to %or% 1)
    if (!(ef < et && et < wf && wf <= wt)) stop("Windows must be ordered: estimation start < estimation end < event start <= event end.")
    d <- df[order(df[[id]], df[[dt]]), c(id, dt, r, mk, ev)]; names(d) <- c("id", "date", "ret", "mkt", "event")
    res <- list(); ars <- list()
    for (g in split(d, d$id)) { e <- which(g$event %in% c(1, TRUE)); for (k in e) {
      est <- (k + ef):(k + et); win <- (k + wf):(k + wt)
      if (min(est) < 1 || max(win) > nrow(g)) next
      ok <- stats::complete.cases(g$ret[est], g$mkt[est]); if (sum(ok) < max(30, (et - ef) / 2)) next
      b <- stats::coef(stats::lm(g$ret[est] ~ g$mkt[est])); ar <- g$ret[win] - (b[1] + b[2] * g$mkt[win])
      res[[length(res) + 1]] <- data.frame(id = g$id[k], event_date = g$date[k], alpha = signif(b[1], 4), beta = signif(b[2], 4), car = signif(sum(ar), 5))
      ars[[length(ars) + 1]] <- data.frame(rel = wf:wt, ar = ar) } }
    if (!length(res)) stop("No event had enough data around it. Check the event column (1 on event days) and the windows.")
    ev_tab <- do.call(rbind, res); a <- do.call(rbind, ars); aar <- stats::aggregate(ar ~ rel, a, mean)
    n <- nrow(ev_tab); mcar <- mean(ev_tab$car); tcs <- mcar / (stats::sd(ev_tab$car) / sqrt(n)); pos <- sum(ev_tab$car > 0)
    summ <- data.frame(statistic = c("Events", "Mean CAR", "Median CAR", "Cross-sectional t", "p-value (t)", "Positive CARs", "Sign-test p-value", "Estimation window", "Event window"),
      value = c(n, signif(mcar, 4), signif(stats::median(ev_tab$car), 4), signif(tcs, 4), signif(2 * stats::pt(-abs(tcs), n - 1), 3), pos,
                signif(stats::binom.test(pos, n)$p.value, 3), sprintf("[%d, %d]", ef, et), sprintf("[%d, %d]", wf, wt)))
    code <- paste(code_header(ds_name), sprintf("# event study: market model on days [%d, %d], CAR over [%d, %d] (positions in each firm's trading days)", ef, et, wf, wt),
      sprintf("d <- dat[order(dat$%s, dat$%s), ]", id, dt),
      sprintf("cars <- do.call(rbind, lapply(split(d, d$%s), function(g) do.call(rbind, lapply(which(g$%s == 1), function(k) {", id, ev),
      sprintf("  est <- (k + %d):(k + %d); win <- (k + %d):(k + %d)", ef, et, wf, wt),
      "  if (min(est) < 1 || max(win) > nrow(g)) return(NULL)",
      sprintf("  b <- coef(lm(g$%s[est] ~ g$%s[est])); data.frame(id = g$%s[k], car = sum(g$%s[win] - (b[1] + b[2] * g$%s[win])))", r, mk, id, r, mk),
      "}))))", "t.test(cars$car)", sep = "\n")
    list(label = sprintf("Event study, CAR[%d,%d]", wf, wt), table = summ, tables = list(`CAR by event (first 500)` = utils::head(ev_tab, 500)),
         text = sprintf("%d events; mean CAR %.4f (t = %.2f).", n, mcar, tcs), code = code,
         plots = list(function() { graphics::par(mar = c(4, 4, 2, 1)); graphics::barplot(aar$ar, names.arg = aar$rel, col = "#B4561A", border = NA,
           xlab = "Day relative to event", ylab = "Average abnormal return", main = "AAR and CAAR"); graphics::lines(seq(0.7, by = 1.2, length.out = nrow(aar)), cumsum(aar$ar), lwd = 2, col = "#14213D") }))
  },
  alpha = {
    y <- o$y; f <- o$factors; tm <- o$time; lag <- as.integer(o$lag %or% 6); need_cols(df, c(y, f, tm))
    fml_txt <- paste(y, "~", paste(f, collapse = " + ")); d <- df[order(df[[tm]]), ]
    m <- stats::lm(stats::as.formula(fml_txt), d); V <- sandwich::NeweyWest(m, lag = lag, prewhite = FALSE)
    tab <- tidy_ct(lmtest::coeftest(m, vcov. = V)[, 1:4, drop = FALSE]); tab$term[tab$term == "(Intercept)"] <- "alpha (intercept)"
    list(label = sprintf("Factor alpha: %s", fml_txt), table = tab, tables = list(Fit = data.frame(n = stats::nobs(m), r2 = signif(summary(m)$r.squared, 4), nw_lag = lag)),
         text = paste(utils::capture.output(print(lmtest::coeftest(m, vcov. = V))), collapse = "\n"),
         code = paste(code_header(ds_name), "library(sandwich); library(lmtest)", sprintf("dat <- dat[order(dat$%s), ]", tm),
                      sprintf("m <- lm(%s, data = dat)", fml_txt), sprintf("coeftest(m, vcov. = NeweyWest(m, lag = %d, prewhite = FALSE))   # the intercept is the alpha per period", lag), sep = "\n"))
  },
  meta = {
    yi <- o$yi; vi <- o$vi %or% ""; sei <- o$sei %or% ""; sl <- o$slab %or% ""; mods <- o$mods %or% character(0)
    need_cols(df, c(yi, vi, sei, sl, mods)); if (!nzchar(vi) && !nzchar(sei)) stop("Choose the sampling variance (vi) or the standard error column.")
    args <- list(yi = df[[yi]], method = o$tau_method %or% "REML"); if (nzchar(vi)) args$vi <- df[[vi]] else args$sei <- df[[sei]]
    if (length(mods)) args$mods <- stats::as.formula(paste("~", paste(mods, collapse = " + "))); if (length(mods)) args$data <- df
    if (nzchar(sl)) args$slab <- as.character(df[[sl]])
    res <- do.call(metafor::rma, args)
    tab <- data.frame(term = rownames(res$beta), estimate = signif(as.numeric(res$beta), 4), std.error = signif(res$se, 4), ci.lb = signif(res$ci.lb, 4), ci.ub = signif(res$ci.ub, 4),
                      p.value = signif(res$pval, 3), sig = stars(res$pval))
    het <- data.frame(statistic = c("Studies (k)", "tau^2", "I^2 (%)", "H^2", "Q", "Q p-value", if (!length(mods)) "Egger test p-value"),
      value = c(res$k, signif(res$tau2, 4), signif(res$I2, 4), signif(res$H2, 4), signif(res$QE, 4), signif(res$QEp, 3),
                if (!length(mods)) signif(tryCatch(metafor::regtest(res)$pval, error = function(e) NA), 3)))
    vtxt <- if (nzchar(vi)) sprintf("vi = %s", vi) else sprintf("sei = %s", sei)
    code <- paste(code_header(ds_name), "library(metafor)", sprintf("res <- rma(yi = %s, %s, %sdata = dat, method = %s%s)", yi, vtxt,
      if (length(mods)) paste0("mods = ~ ", paste(mods, collapse = " + "), ", ") else "", qcode(o$tau_method %or% "REML"), if (nzchar(sl)) paste0(", slab = ", sl) else ""),
      "summary(res); forest(res); funnel(res)", if (!length(mods)) "regtest(res)   # Egger's test" else "", sep = "\n")
    list(label = sprintf("Meta-analysis (%s, k = %d)", o$tau_method %or% "REML", res$k), table = tab, tables = list(Heterogeneity = het),
         text = paste(utils::capture.output(print(res)), collapse = "\n"), code = code,
         plots = list(function() metafor::forest(res, header = TRUE), function() metafor::funnel(res, main = "Funnel plot")))
  },
  did = {
    y <- o$y; id <- o$id; tm <- o$time; co <- o$cohort; ctrl <- trimws(o$rhs %or% ""); need_cols(df, c(y, id, tm, co, if (nzchar(ctrl)) rhs_vars(ctrl)))
    fml_txt <- sprintf("%s ~ sunab(%s, %s)%s | %s + %s", y, co, tm, if (nzchar(ctrl)) paste(" +", ctrl) else "", id, tm)
    m <- fixest::feols(stats::as.formula(fml_txt), df, cluster = stats::as.formula(paste("~", id)))
    ct <- tidy_ct(fixest::coeftable(m)); att <- tidy_ct(fixest::coeftable(summary(m, agg = "ATT")))
    rel <- ct[grepl("::", ct$term), ]; rel$rel_time <- suppressWarnings(as.numeric(sub(".*::", "", rel$term)))
    list(label = sprintf("Staggered DiD (Sun & Abraham): %s", y), table = att, tables = list(`Event-time effects` = rel),
         text = paste(utils::capture.output(print(summary(m, agg = "ATT"))), collapse = "\n"),
         code = paste(code_header(ds_name), "library(fixest)", sprintf("# %s: first treated period; never-treated units coded as a value outside the sample (e.g. 10000)", co),
                      sprintf("m <- feols(%s, data = dat, cluster = ~ %s)", fml_txt, id), "iplot(m); summary(m, agg = \"ATT\")", sep = "\n"),
         plots = list(function() { r <- rel[order(rel$rel_time), ]; lo <- r$estimate - 1.96 * r$std.error; hi <- r$estimate + 1.96 * r$std.error
           graphics::par(mar = c(4, 4, 2, 1)); graphics::plot(r$rel_time, r$estimate, ylim = range(c(lo, hi, 0), na.rm = TRUE), pch = 19, xlab = "Time relative to treatment", ylab = "Effect", main = "Event-time effects")
           graphics::segments(r$rel_time, lo, r$rel_time, hi); graphics::abline(h = 0, lty = 2); graphics::abline(v = -0.5, lty = 3) }))
  },
  multilevel = {
    fx <- o$fixed %or% stop("Write the fixed part, e.g. y ~ x1 + x2."); rd <- o$random %or% stop("Write the random part, e.g. ~ 1 | group.")
    need_cols(df, c(all.vars(stats::as.formula(fx)), all.vars(stats::as.formula(rd))))
    m <- nlme::lme(stats::as.formula(fx), random = stats::as.formula(rd), data = df, method = "REML", na.action = stats::na.omit)
    tt <- as.data.frame(summary(m)$tTable)
    tab <- data.frame(term = rownames(tt), estimate = signif(tt$Value, 4), std.error = signif(tt$Std.Error, 4), df = tt$DF, statistic = signif(tt$`t-value`, 4), p.value = signif(tt$`p-value`, 3), sig = stars(tt$`p-value`))
    vc <- nlme::VarCorr(m); v <- suppressWarnings(as.numeric(vc[, "Variance"]))
    icc <- if (length(v) == 2 && all(!is.na(v))) signif(v[1] / sum(v), 3) else NA
    list(label = sprintf("Multilevel model: %s, random %s", fx, rd), table = tab,
         tables = list(`Variance components` = data.frame(component = rownames(vc), variance = vc[, "Variance"], sd = vc[, "StdDev"], row.names = NULL), ICC = data.frame(icc = icc)),
         text = paste(utils::capture.output(print(summary(m))), collapse = "\n"),
         code = paste(code_header(ds_name), "library(nlme)", sprintf("m <- lme(%s, random = %s, data = dat, method = \"REML\", na.action = na.omit)", fx, rd), "summary(m); VarCorr(m)", sep = "\n"))
  },
  sem = {
    mod <- o$model %or% stop("Write the model in lavaan syntax, e.g.  ESG =~ e1 + e2 + e3   and   ret ~ ESG.")
    fit <- lavaan::sem(mod, data = df, std.lv = TRUE)
    pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
    tab <- data.frame(lhs = pe$lhs, op = pe$op, rhs = pe$rhs, estimate = signif(pe$est, 4), std.error = signif(pe$se, 4), z = signif(pe$z, 4), p.value = signif(pe$pvalue, 3), std.all = signif(pe$std.all, 3))
    fm <- lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
    list(label = "Structural equation model (lavaan)", table = tab, tables = list(`Fit measures` = data.frame(measure = names(fm), value = signif(as.numeric(fm), 4))),
         text = paste(utils::capture.output(lavaan::summary(fit, fit.measures = TRUE, standardized = TRUE)), collapse = "\n"),
         code = paste(code_header(ds_name), "library(lavaan)", sprintf("model <- %s", qcode(mod)), "fit <- sem(model, data = dat, std.lv = TRUE)",
                      "summary(fit, fit.measures = TRUE, standardized = TRUE)", sep = "\n"))
  },
  stop("Unknown method."))
}

# a self-contained R script for every run, plus the data and provenance (licensed data only if the researcher includes it)
build_analysis_zip <- function(st, zipfile, include_licensed = FALSE) {
  root <- file.path(tempfile("anchor_analysis"), "ANCHOR_analysis"); dir.create(file.path(root, "data"), recursive = TRUE)
  ds <- st$analysis$datasets %||% list(); res <- st$analysis$results %||% list()
  skipped <- character(0)
  for (d in ds) { if (isTRUE(d$licensed) && !include_licensed) { skipped <- c(skipped, d$name); next }
    if (!is.null(d$data)) utils::write.csv(d$data, file.path(root, "data", paste0(d$name, ".csv")), row.names = FALSE) }
  pv <- provenance_df(ds); if (!is.null(pv)) utils::write.csv(pv, file.path(root, "provenance.csv"), row.names = FALSE)
  script <- c(sprintf("# %s %s - analysis script", APP_BRAND, APP_VERSION), sprintf("# Project: %s", st$article$title %||% ""), sprintf("# Generated %s. Run from this folder: source(\"analysis.R\")", format(Sys.time(), "%Y-%m-%d %H:%M")),
    if (length(skipped)) sprintf("# Licensed datasets not included (place your own export in data/): %s", paste(skipped, collapse = ", ")), "",
    unlist(lapply(seq_along(res), function(i) c(sprintf("# ---- %d. %s (%s)", i, res[[i]]$label, res[[i]]$time), res[[i]]$code, ""))),
    "sessionInfo()")
  writeLines(script, file.path(root, "analysis.R"))
  writeLines(c(utils::capture.output(utils::sessionInfo())), file.path(root, "sessionInfo_at_export.txt"))
  writeLines(c(sprintf("%s %s - analysis package", APP_BRAND, APP_VERSION), "", "analysis.R        every model you ran, in order, as plain R code",
    "data/             the datasets (open data; licensed data only if you chose to include it)", "provenance.csv    source, query, retrieval time and SHA-256 of each dataset",
    "", "R packages used: fixest, metafor, nlme, lavaan, sandwich (install them with install.packages())."), file.path(root, "README.txt"))
  old <- setwd(dirname(root)); on.exit(setwd(old)); zip::zip(zipfile, basename(root), recurse = TRUE, mode = "mirror"); invisible(zipfile)
}

knit_table <- function(df, n = 50) { df <- utils::head(as.data.frame(df), n); e <- htmltools::htmlEscape
  paste0("<table class='table table-sm small'><tr>", paste0("<th>", e(names(df)), "</th>", collapse = ""), "</tr>",
         paste(apply(df, 1, function(r) paste0("<tr>", paste0("<td>", e(as.character(r)), "</td>", collapse = ""), "</tr>")), collapse = ""), "</table>") }
items_for <- function(st, phase) Filter(function(it) identical(it$phase %||% "C", phase), st$report_items %||% list())

# ANCHOR Studio - bibliometric analyses built on bibliometrix
# (Aria & Cuccurullo, 2017). Every analysis returns a list with
#   kind  = "gg" (ggplot), "base" (a function that draws), "plotly" or "table"
#   plot  = the object, table = a data frame shown under the plot, note = text

DB_SOURCES <- c("Scopus" = "scopus", "Web of Science" = "wos", "Dimensions" = "dimensions",
                "PubMed" = "pubmed", "Lens.org" = "lens", "Cochrane" = "cochrane", "OpenAlex" = "openalex")
DB_FORMATS <- c("CSV (.csv)" = "csv", "BibTeX (.bib)" = "bibtex", "Plain text (.txt)" = "plaintext",
                "Excel (.xlsx)" = "excel", "EndNote (.ciw)" = "endnote")

PAL <- c(navy = "#14213D", accent = "#B4561A", light = "#E9A36B", grey = "#5B6675")

# networkPlot() only draws when verbose = TRUE; it is run once with verbose = FALSE
# and the igraph object it returns (layout, colours and labels stored as attributes) is drawn here.
net_drawer <- function(net) function() { op <- graphics::par(mar = c(0, 0, 2, 0)); on.exit(graphics::par(op)); igraph::plot.igraph(net$graph) }
net_table <- function(net) { cr <- net$cluster_res; if (!is.data.frame(cr)) return(NULL)
  cr[, -1] <- lapply(cr[, -1], function(v) if (is.numeric(v)) round(v, 4) else v); names(cr) <- c("Node", "Cluster", "Betweenness", "Closeness", "PageRank"); cr }

# Charts use a bundled font (Liberation Sans, SIL OFL) so they look the same on every computer,
# including Windows machines where font lookup fails. Registered under the generic name "sans".
register_fonts <- function(app_dir = getwd()) {
  ff <- file.path(app_dir, "www", "fonts")
  f <- file.path(ff, paste0("LiberationSans-", c("Regular", "Bold", "Italic", "BoldItalic"), ".ttf"))
  if (all(file.exists(f)) && requireNamespace("systemfonts", quietly = TRUE))
    try(systemfonts::register_font("sans", plain = f[1], bold = f[2], italic = f[3], bolditalic = f[4]), silent = TRUE)
  invisible(TRUE)
}

gg_device <- function() if (.Platform$OS.type == "windows") grDevices::png else "png"

theme_anchor <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", colour = PAL[["navy"]]),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(colour = PAL[["grey"]]))
}

# ---------- import ----------
import_files <- function(paths, names, dbsource, format) {
  ms <- list(); log <- data.frame(file = character(), database = character(), records = integer())
  for (i in seq_along(paths)) {
    p <- paths[i]
    ext <- tolower(tools::file_ext(names[i]))
    target <- file.path(tempdir(), paste0("imp_", i, "_", basename(names[i])))
    file.copy(p, target, overwrite = TRUE)
    if (ext == "rds" || ext == "rdata") {
      M <- if (ext == "rds") readRDS(target) else { e <- new.env(); load(target, envir = e); get(ls(e)[1], envir = e) }
    } else if (format == "excel" && dbsource == "bibliometrix") {
      M <- as.data.frame(readxl::read_excel(target))
    } else {
      M <- tryCatch(suppressMessages(suppressWarnings(bibliometrix::convert2df(file = target, dbsource = dbsource, format = format))),
        error = function(e) stop(sprintf("%s could not be read as a %s %s export (%s). Check the Database and File format choices; for OpenAlex, update bibliometrix to the current CRAN version.",
                                         names[i], dbsource, format, conditionMessage(e)), call. = FALSE))
    }
    if (!nrow(M)) stop(sprintf("No records could be read from %s. Check that the database and file format chosen match the export.", names[i]))
    M <- normalize_M(M)
    log <- rbind(log, data.frame(file = names[i], database = dbsource, records = nrow(M)))
    ms[[i]] <- M
  }
  list(list = ms, log = log)
}

merge_imports <- function(ms, remove_dup = TRUE) {
  if (length(ms) == 1) {
    M <- ms[[1]]
    n0 <- nrow(M)
    if (remove_dup && "TI" %in% names(M)) M <- M[!duplicated(toupper(gsub("[^A-Za-z0-9]", "", M$TI))), ]
    return(list(M = M, dup = n0 - nrow(M)))
  }
  n0 <- sum(vapply(ms, nrow, 1L))
  M <- suppressMessages(do.call(bibliometrix::mergeDbSources, c(ms, list(remove.duplicated = remove_dup, verbose = FALSE))))
  list(M = M, dup = n0 - nrow(M))
}

apply_filters <- function(M, years = NULL, types = NULL, langs = NULL, min_tc = 0) {
  keep <- rep(TRUE, nrow(M))
  if (!is.null(years) && "PY" %in% names(M)) keep <- keep & !is.na(M$PY) & M$PY >= years[1] & M$PY <= years[2]
  if (length(types) && "DT" %in% names(M)) keep <- keep & toupper(M$DT) %in% toupper(types)
  if (length(langs) && "LA" %in% names(M)) keep <- keep & toupper(M$LA) %in% toupper(langs)
  if (!is.null(min_tc) && min_tc > 0 && "TC" %in% names(M)) keep <- keep & !is.na(M$TC) & M$TC >= min_tc
  M2 <- M[keep, , drop = FALSE]
  class(M2) <- class(M)
  M2
}

# analyses that read the text of cited references (authors, years, sources)
need_refs <- function(M) {
  need(M, "CR", "cited references")
  cr <- trimws(unlist(strsplit(head(stats::na.omit(as.character(M$CR)), 30), ";")))
  if (length(cr) && mean(grepl("^\\d{3}-\\d{3}-\\d{3}-\\d{3}-\\d{3}", cr)) > .8)
    stop("This export lists cited works as Lens ID numbers, not full references, so this analysis cannot run on it. Use a Scopus or Web of Science export for reference-based analyses; bibliographic coupling still works.", call. = FALSE)
  invisible(TRUE)
}
has <- function(M, f) f %in% names(M) && any(!is.na(M[[f]]) & nzchar(as.character(M[[f]])))
need <- function(M, f, what) if (!has(M, f)) stop(sprintf("This analysis needs %s (field %s), which this dataset does not contain.", what, f))

bar_plot <- function(df, x, y, title, xlab = NULL, n = 20, colour = PAL[["navy"]]) {
  df <- head(df[order(-df[[y]]), , drop = FALSE], n)
  lab <- as.character(df[[x]])
  short <- ifelse(nchar(lab) > 48, paste0(substr(lab, 1, 47), "\u2026"), lab)   # full names stay in the table
  df[[x]] <- factor(make.unique(short, sep = " "), levels = rev(make.unique(short, sep = " ")))
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_col(fill = colour, width = 0.7) + ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = NULL, y = xlab %||% y) + theme_anchor() +
    ggplot2::theme(plot.title.position = "plot")
}

tab_to_df <- function(tb, name = "Item") {
  df <- data.frame(Item = trimws(names(tb)), N = as.integer(tb), stringsAsFactors = FALSE)
  names(df)[1] <- name
  df
}

country_coords_needed <- FALSE

# ---------- analyses ----------
ANALYSES <- list(
  # ---- Overview
  main_info = list(group = "Overview", label = "Main information",
    run = function(M, R, o) {
      s <- summary(R, k = 10, pause = FALSE, verbose = FALSE)
      df <- s$MainInformationDF
      names(df) <- c("Description", "Results")
      # bibliometrix strips the minus sign from the numbers and leaves it at the end of the label (a negative growth rate read as positive)
      neg <- grepl("-\\s*$", df$Description); df$Results[neg] <- paste0("-", df$Results[neg]); df$Description <- trimws(sub("-\\s*$", "", df$Description))
      list(kind = "table", table = df, note = "Equivalent to Biblioshiny > Overview > Main information.")
    }),
  annual_prod = list(group = "Overview", label = "Annual scientific production",
    run = function(M, R, o) {
      need(M, "PY", "publication years")
      df <- as.data.frame(table(M$PY)); names(df) <- c("Year", "Articles")
      df$Year <- as.integer(as.character(df$Year))
      df <- merge(data.frame(Year = seq(min(df$Year), max(df$Year))), df, all.x = TRUE); df$Articles[is.na(df$Articles)] <- 0
      g <- ggplot2::ggplot(df, ggplot2::aes(Year, Articles)) + ggplot2::geom_area(fill = PAL[["light"]], alpha = .35) +
        ggplot2::geom_line(colour = PAL[["accent"]], linewidth = 1) + ggplot2::geom_point(colour = PAL[["accent"]]) +
        ggplot2::labs(title = "Annual scientific production", y = "Documents") + theme_anchor()
      yrs <- range(df$Year); n <- df$Articles[df$Articles > 0]
      cagr <- if (length(n) > 1) round(((df$Articles[nrow(df)] / max(1, df$Articles[which(df$Articles > 0)[1]]))^(1 / max(1, diff(yrs))) - 1) * 100, 2) else NA
      list(kind = "gg", plot = g, table = df, note = paste0("Annual growth rate: ", cagr, "%"))
    }),
  avg_citations = list(group = "Overview", label = "Average citations per year",
    run = function(M, R, o) {
      need(M, "TC", "citation counts"); need(M, "PY", "publication years")
      now <- as.integer(format(Sys.Date(), "%Y"))
      d <- data.frame(PY = M$PY, TC = as.numeric(M$TC))
      d <- d[!is.na(d$PY), ]
      df <- do.call(rbind, lapply(split(d, d$PY), function(x) data.frame(Year = x$PY[1], N = nrow(x),
              MeanTCperArt = round(mean(x$TC, na.rm = TRUE), 2), MeanTCperYear = round(mean(x$TC, na.rm = TRUE) / max(1, now - x$PY[1] + 1), 2))))
      g <- ggplot2::ggplot(df, ggplot2::aes(Year, MeanTCperYear)) + ggplot2::geom_line(colour = PAL[["navy"]], linewidth = 1) +
        ggplot2::geom_point(colour = PAL[["navy"]]) + ggplot2::labs(title = "Average citations per year", y = "Mean citations per article per year") + theme_anchor()
      list(kind = "gg", plot = g, table = df)
    }),
  three_field = list(group = "Overview", label = "Three-field plot",
    opts = c("tf"), run = function(M, R, o) {
      f <- o$tf %||% c("AU", "DE", "SO")
      lab <- c(AU = "authors", DE = "author keywords", SO = "sources", AU_CO = "countries", CR_SO = "cited sources")
      for (x in f) if (!has(M, x) && x != "CR_SO" && x != "AU_CO") need(M, x, lab[[x]] %||% x)
      if ("CR_SO" %in% f) need(M, "CR", "cited references")
      if ("AU_CO" %in% f && !has(M, "AU_CO")) M <- bibliometrix::metaTagExtraction(M, "AU_CO", sep = ";")
      if ("CR_SO" %in% f) M <- bibliometrix::metaTagExtraction(M, "CR_SO", sep = ";")
      list(kind = "plotly", plot = bibliometrix::threeFieldsPlot(M, fields = f, n = c(15, 15, 15)))
    }),

  # ---- Sources
  rel_sources = list(group = "Sources", label = "Most relevant sources",
    run = function(M, R, o) { need(M, "SO", "source titles")
      df <- tab_to_df(sort(table(M$SO), decreasing = TRUE), "Source"); names(df)[2] <- "Documents"
      list(kind = "gg", plot = bar_plot(df, "Source", "Documents", "Most relevant sources", "Documents"), table = head(df, 50)) }),
  local_cited_sources = list(group = "Sources", label = "Most local cited sources",
    run = function(M, R, o) { need_refs(M)
      M2 <- bibliometrix::metaTagExtraction(M, "CR_SO", sep = ";")
      tb <- bibliometrix::tableTag(M2, "CR_SO")
      df <- tab_to_df(tb, "Source"); names(df)[2] <- "Citations"
      list(kind = "gg", plot = bar_plot(df, "Source", "Citations", "Most local cited sources", "Citations in the dataset's references", colour = PAL[["accent"]]), table = head(df, 50)) }),
  bradford = list(group = "Sources", label = "Bradford's law",
    run = function(M, R, o) { need(M, "SO", "source titles"); b <- bibliometrix::bradford(M)
      list(kind = "gg", plot = b$graph, table = head(b$table, 60), note = "Zone 1 = core sources (Bradford, 1934).") }),
  source_impact = list(group = "Sources", label = "Sources' local impact (h-index)",
    run = function(M, R, o) { need(M, "TC", "citation counts")
      h <- bibliometrix::Hindex(M, field = "source", sep = ";")$H
      h <- h[order(-h$h_index, -h$TC), ]
      list(kind = "gg", plot = bar_plot(h, "Element", "h_index", "Sources' local impact", "h-index"), table = head(h, 50)) }),
  source_dynamics = list(group = "Sources", label = "Sources' production over time",
    run = function(M, R, o) { need(M, "SO", "source titles"); need(M, "PY", "years")
      sg <- bibliometrix::sourceGrowth(M, top = 5, cdf = TRUE)
      long <- tidyr::pivot_longer(sg, -Year, names_to = "Source", values_to = "Cumulative")
      g <- ggplot2::ggplot(long, ggplot2::aes(Year, Cumulative, colour = Source)) + ggplot2::geom_line(linewidth = 1) +
        ggplot2::labs(title = "Sources' production over time", y = "Cumulative documents") + theme_anchor() +
        ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
      list(kind = "gg", plot = g, table = sg) }),

  # ---- Authors
  rel_authors = list(group = "Authors", label = "Most relevant authors",
    run = function(M, R, o) { need(M, "AU", "authors")
      df <- data.frame(Author = names(R$Authors), Documents = as.integer(R$Authors))
      fr <- R$AuthorsFrac; names(fr) <- c("Author", "Fractionalized")
      df <- merge(df, fr, by = "Author", all.x = TRUE); df <- df[order(-df$Documents), ]
      list(kind = "gg", plot = bar_plot(df, "Author", "Documents", "Most relevant authors", "Documents"), table = head(df, 50)) }),
  local_cited_authors = list(group = "Authors", label = "Most local cited authors",
    run = function(M, R, o) { need_refs(M)
      lc <- bibliometrix::localCitations(M, fast.search = TRUE)
      df <- as.data.frame(lc$Authors); names(df) <- c("Author", "LocalCitations")
      list(kind = "gg", plot = bar_plot(df, "Author", "LocalCitations", "Most local cited authors", "Local citations", colour = PAL[["accent"]]), table = head(df, 50)) }),
  author_time = list(group = "Authors", label = "Authors' production over time",
    run = function(M, R, o) { need(M, "AU", "authors"); need(M, "PY", "years")
      ap <- bibliometrix::authorProdOverTime(M, k = 10, graph = FALSE)
      list(kind = "gg", plot = ap$graph, table = head(ap$dfAU, 60)) }),
  lotka = list(group = "Authors", label = "Lotka's law",
    run = function(M, R, o) { need(M, "AU", "authors")
      # bibliometrix 4.x takes the biblioAnalysis() result; 5.x takes the records
      L <- if ("M" %in% names(formals(bibliometrix::lotka))) bibliometrix::lotka(M) else bibliometrix::lotka(R)
      tab <- L$AuthorProd
      names(tab)[1:3] <- c("Documents", "Authors", "Proportion")
      g <- ggplot2::ggplot(tab, ggplot2::aes(Documents, Proportion)) + ggplot2::geom_line(colour = PAL[["navy"]], linewidth = 1) +
        ggplot2::geom_point(colour = PAL[["navy"]]) + ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(title = "Author productivity through Lotka's law", x = "Documents written", y = "% of authors") + theme_anchor()
      list(kind = "gg", plot = g, table = tab, note = sprintf("Estimated beta = %.2f, C = %.2f, R2 = %.2f", L$Beta %||% NA, L$C %||% NA, L$R2 %||% NA)) }),
  author_impact = list(group = "Authors", label = "Authors' local impact (h-index)",
    run = function(M, R, o) { need(M, "TC", "citation counts")
      h <- bibliometrix::Hindex(M, field = "author", sep = ";")$H
      h <- h[order(-h$h_index, -h$TC), ]
      list(kind = "gg", plot = bar_plot(h, "Element", "h_index", "Authors' local impact", "h-index"), table = head(h, 50)) }),
  affiliations = list(group = "Authors", label = "Most relevant affiliations",
    run = function(M, R, o) { need(M, "C1", "affiliations")
      df <- tab_to_df(R$Affiliations, "Affiliation"); names(df)[2] <- "Articles"
      df <- df[!df$Affiliation %in% c("NOTREPORTED", "NOTDECLARED", "NA"), ]
      list(kind = "gg", plot = bar_plot(df, "Affiliation", "Articles", "Most relevant affiliations", "Articles"), table = head(df, 50)) }),
  aff_time = list(group = "Authors", label = "Affiliations' production over time",
    run = function(M, R, o) { need(M, "C1", "affiliations")
      prod_over_time(M, "AU_UN", "Affiliation", "Affiliations' production over time") }),
  country_time = list(group = "Authors", label = "Countries' production over time",
    run = function(M, R, o) { need(M, "C1", "affiliations")
      prod_over_time(M, "AU_CO", "Country", "Countries' production over time") }),
  corr_country = list(group = "Authors", label = "Corresponding author's countries (SCP/MCP)",
    run = function(M, R, o) { need(M, "C1", "author affiliations"); cc <- R$CountryCollaboration
      if (is.null(cc) || !nrow(cc)) stop("No country information in this dataset.")
      cc <- head(cc[order(-(cc$SCP + cc$MCP)), ], 20)
      long <- rbind(data.frame(Country = cc$Country, Type = "Single-country publications (SCP)", N = cc$SCP),
                    data.frame(Country = cc$Country, Type = "Multiple-country publications (MCP)", N = cc$MCP))
      long$Country <- factor(long$Country, levels = rev(cc$Country))
      g <- ggplot2::ggplot(long, ggplot2::aes(Country, N, fill = Type)) + ggplot2::geom_col(width = .7) + ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = c(PAL[["navy"]], PAL[["light"]])) +
        ggplot2::labs(title = "Corresponding author's countries", x = NULL, y = "Documents", fill = NULL) + theme_anchor() +
        ggplot2::theme(legend.position = "bottom")
      cc$MCP_ratio <- round(cc$MCP / (cc$SCP + cc$MCP), 3)
      list(kind = "gg", plot = g, table = cc) }),
  country_prod = list(group = "Authors", label = "Countries' scientific production (map)",
    run = function(M, R, o) { need(M, "C1", "affiliations")
      M2 <- bibliometrix::metaTagExtraction(M, "AU_CO", sep = ";")
      df <- tab_to_df(bibliometrix::tableTag(M2, "AU_CO"), "Country"); names(df)[2] <- "Frequency"
      df$Country <- tools::toTitleCase(tolower(df$Country)); df$Country[df$Country == "Usa"] <- "United States"
      df$Country[df$Country == "Uk"] <- "United Kingdom"
      # static map drawn from the 'maps' package: works offline and exports to the report
      w <- ggplot2::map_data("world"); w <- w[w$region != "Antarctica", ]
      key <- toupper(df$Country); key[key == "UNITED STATES"] <- "USA"; key[key == "UNITED KINGDOM"] <- "UK"
      key[key == "KOREA"] <- "SOUTH KOREA"
      w$Frequency <- df$Frequency[match(toupper(w$region), key)]
      g <- ggplot2::ggplot(w, ggplot2::aes(long, lat, group = group, fill = Frequency)) +
        ggplot2::geom_polygon(colour = "white", linewidth = .1) + ggplot2::coord_quickmap() +
        ggplot2::scale_fill_gradient(low = "#F3D9C4", high = PAL[["accent"]], na.value = "#E4E1D8") +
        ggplot2::labs(title = "Countries' scientific production (author appearances)", x = NULL, y = NULL) +
        theme_anchor() + ggplot2::theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), legend.position = "right")
      list(kind = "gg", plot = g, table = head(df, 60)) }),
  cited_countries = list(group = "Authors", label = "Most cited countries",
    run = function(M, R, o) { need(M, "TC", "citation counts"); need(M, "C1", "author affiliations")
      if (is.null(R$CO)) stop("No country information in this dataset.")
      d <- data.frame(Country = R$CO, TC = as.numeric(M$TC)); d <- d[!is.na(d$Country), ]
      df <- do.call(rbind, lapply(split(d, d$Country), function(x) data.frame(Country = x$Country[1], TotalCitations = sum(x$TC, na.rm = TRUE), AvgCitations = round(mean(x$TC, na.rm = TRUE), 1))))
      df <- df[order(-df$TotalCitations), ]
      list(kind = "gg", plot = bar_plot(df, "Country", "TotalCitations", "Most cited countries", "Total citations", colour = PAL[["accent"]]), table = head(df, 40)) }),

  # ---- Documents
  global_cited_docs = list(group = "Documents", label = "Most global cited documents",
    run = function(M, R, o) { need(M, "TC", "citation counts")
      df <- as.data.frame(R$MostCitedPapers)
      names(df)[1:2] <- c("Paper", "DOI")[seq_len(min(2, ncol(df)))]
      df <- df[order(-df$TC), ]
      list(kind = "gg", plot = bar_plot(df, "Paper", "TC", "Most global cited documents", "Global citations"), table = head(df, 50)) }),
  local_cited_docs = list(group = "Documents", label = "Most local cited documents",
    run = function(M, R, o) { need_refs(M)
      lc <- bibliometrix::localCitations(M, fast.search = TRUE)
      df <- as.data.frame(lc$Papers)
      list(kind = "gg", plot = bar_plot(df, "Paper", "LCS", "Most local cited documents", "Local citations", colour = PAL[["accent"]]), table = head(df, 50)) }),
  local_cited_refs = list(group = "Documents", label = "Most local cited references",
    run = function(M, R, o) { need_refs(M)
      cr <- bibliometrix::citations(M, field = "article", sep = ";")
      df <- data.frame(Reference = names(cr$Cited), Citations = as.integer(cr$Cited))
      df <- df[nzchar(df$Reference), ]
      df$Short <- substr(df$Reference, 1, 70)
      list(kind = "gg", plot = bar_plot(df, "Short", "Citations", "Most local cited references", "Citations"), table = head(df[, 1:2], 60)) }),
  rpys = list(group = "Documents", label = "Reference spectroscopy (RPYS)",
    run = function(M, R, o) { need_refs(M)
      rp <- bibliometrix::rpys(M, graph = FALSE)
      list(kind = "gg", plot = rp$spectroscopy, table = head(rp$rpysTable, 100), note = "Peaks show the historical roots of the field.") }),

  # ---- Words
  freq_words = list(group = "Words", label = "Most frequent words",
    opts = c("field"), run = function(M, R, o) { f <- o$field %||% "DE"
      if (f %in% c("TI", "AB")) need(M, f, "titles/abstracts") else need(M, f, "keywords")
      tb <- bibliometrix::tableTag(M, f, ngrams = o$ngrams %||% 1)
      df <- tab_to_df(tb, "Word"); names(df)[2] <- "Occurrences"
      list(kind = "gg", plot = bar_plot(df, "Word", "Occurrences", paste("Most frequent words -", field_label(f)), "Occurrences"), table = head(df, 100)) }),
  word_cloud = list(group = "Words", label = "Word cloud",
    opts = c("field"), run = function(M, R, o) { f <- o$field %||% "DE"; need(M, f, "keywords or text")
      tb <- bibliometrix::tableTag(M, f, ngrams = o$ngrams %||% 1)
      df <- head(tab_to_df(tb, "Word"), 80); names(df)[2] <- "Occurrences"
      if (requireNamespace("wordcloud", quietly = TRUE)) {
        drw <- function() { op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op))
          wordcloud::wordcloud(tolower(df$Word), df$Occurrences, max.words = 80, random.order = FALSE, rot.per = .15,
                               colors = c(PAL[["grey"]], PAL[["navy"]], PAL[["accent"]])) }
        return(list(kind = "base", plot = drw, table = df))
      }
      set.seed(1); df$x <- stats::runif(nrow(df)); df$y <- stats::runif(nrow(df))
      g <- ggplot2::ggplot(df, ggplot2::aes(x, y, label = tolower(Word), size = Occurrences)) +
        ggrepel::geom_text_repel(colour = PAL[["navy"]], segment.color = NA, max.overlaps = 100) +
        ggplot2::scale_size(range = c(3, 12), guide = "none") + ggplot2::theme_void()
      list(kind = "gg", plot = g, table = df[, 1:2]) }),
  treemap = list(group = "Words", label = "Tree map",
    opts = c("field"), run = function(M, R, o) { f <- o$field %||% "DE"; need(M, f, "keywords or text")
      df <- head(tab_to_df(bibliometrix::tableTag(M, f), "Word"), 50); names(df)[2] <- "Occurrences"
      df$pct <- round(100 * df$Occurrences / sum(df$Occurrences), 1)
      p <- plotly::plot_ly(df, type = "treemap", labels = ~paste0(Word, " (", pct, "%)"), parents = "", values = ~Occurrences,
                           marker = list(colorscale = list(c(0, "#ECE8DE"), c(1, PAL[["accent"]])), colors = ~Occurrences))
      list(kind = "plotly", plot = p, table = df) }),
  word_dynamics = list(group = "Words", label = "Words' frequency over time",
    opts = c("field"), run = function(M, R, o) { f <- o$field %||% "DE"; need(M, f, "keywords")
      kg <- bibliometrix::KeywordGrowth(M, Tag = f, sep = ";", top = 8, cdf = TRUE)
      long <- tidyr::pivot_longer(kg, -Year, names_to = "Word", values_to = "Cumulative")
      g <- ggplot2::ggplot(long, ggplot2::aes(Year, Cumulative, colour = Word)) + ggplot2::geom_line(linewidth = 1) +
        ggplot2::labs(title = "Words' cumulative frequency over time", y = "Cumulative occurrences") + theme_anchor() +
        ggplot2::theme(legend.position = "bottom")
      list(kind = "gg", plot = g, table = kg) }),
  trend_topics = list(group = "Words", label = "Trend topics",
    opts = c("field"), run = function(M, R, o) { f <- o$field %||% "DE"; need(M, f, "keywords"); need(M, "PY", "years")
      fy <- bibliometrix::fieldByYear(M, field = f, min.freq = 3, n.items = 4, graph = FALSE)
      list(kind = "gg", plot = fy$graph, table = fy$df_graph) }),

  # ---- Conceptual structure
  cooc_net = list(group = "Conceptual structure", label = "Co-occurrence network",
    opts = c("field", "nnodes"), run = function(M, R, o) { f <- o$field %||% "DE"; need(M, f, "keywords")
      net_type <- switch(f, DE = "author_keywords", ID = "keywords", TI = "titles", AB = "abstracts", "author_keywords")
      NM <- bibliometrix::biblioNetwork(M, analysis = "co-occurrences", network = net_type, sep = ";")
      n <- o$nnodes %||% 40
      net <- bibliometrix::networkPlot(NM, n = n, Title = paste("Co-occurrence network -", field_label(f)),
                                                    type = "fruchterman", size.cex = TRUE, size = 15, labelsize = .8, remove.multiple = FALSE,
                                                    edgesize = 3, cluster = "louvain", remove.isolates = TRUE, verbose = FALSE)
      list(kind = "base", plot = net_drawer(net), table = net_table(net), note = "Colours are clusters (Louvain). Node size = occurrences.") }),
  thematic_map = list(group = "Conceptual structure", label = "Thematic map",
    opts = c("field"), run = function(M, R, o) { f <- o$field %||% "DE"; need(M, f, "keywords")
      tm <- bibliometrix::thematicMap(M, field = f, n = 250, minfreq = 5, stemming = FALSE, size = .5, n.labels = 3, repel = TRUE)
      tab <- as.data.frame(tm$clusters)[, intersect(c("name", "freq", "centrality", "density", "rcentrality", "rdensity"), names(tm$clusters))]
      list(kind = "gg", plot = tm$map, table = tab, note = "Upper right = motor themes; upper left = niche; lower left = emerging or declining; lower right = basic themes.") }),
  thematic_evolution = list(group = "Conceptual structure", label = "Thematic evolution", slow = TRUE,
    opts = c("field", "cuts"), run = function(M, R, o) { f <- o$field %||% "DE"; need(M, f, "keywords"); need(M, "PY", "years")
      cuts <- suppressWarnings(as.integer(strsplit(o$cuts %||% "", "[,; ]+")[[1]]))
      cuts <- cuts[!is.na(cuts)]
      if (!length(cuts)) { r <- range(M$PY, na.rm = TRUE); cuts <- round(r[1] + diff(r) * c(1 / 3, 2 / 3)) }
      te <- bibliometrix::thematicEvolution(M, field = f, years = cuts, n = 250, minFreq = 2)
      p <- bibliometrix::plotThematicEvolution(te$Nodes, te$Edges)
      list(kind = "plotly", plot = p, note = paste("Cutting years:", paste(cuts, collapse = ", "))) }),
  factorial = list(group = "Conceptual structure", label = "Factorial analysis (MCA)",
    opts = c("field"), run = function(M, R, o) { f <- o$field %||% "DE"; need(M, f, "keywords")
      cs <- bibliometrix::conceptualStructure(M, field = f, method = "MCA", minDegree = 4, clust = "auto", k.max = 8,
                                               stemming = FALSE, labelsize = 10, documents = 10, graph = FALSE)
      list(kind = "gg", plot = cs$graph_terms, note = "Conceptual structure map (multiple correspondence analysis).") }),

  # ---- Intellectual structure
  cocit_net = list(group = "Intellectual structure", label = "Co-citation network", slow = TRUE,
    opts = c("nnodes"), run = function(M, R, o) { need_refs(M)
      NM <- bibliometrix::biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
      n <- o$nnodes %||% 40
      net <- bibliometrix::networkPlot(NM, n = n, Title = "Co-citation network", type = "fruchterman", size.cex = TRUE,
                                                    size = 15, labelsize = .7, remove.multiple = FALSE, edgesize = 3, cluster = "louvain",
                                                    remove.isolates = TRUE, verbose = FALSE)
      list(kind = "base", plot = net_drawer(net), table = net_table(net), note = "Colours are clusters (Louvain). Node size = degree.") }),
  coupling_net = list(group = "Intellectual structure", label = "Bibliographic coupling network", slow = TRUE,
    opts = c("clevel", "nnodes"), run = function(M, R, o) { need(M, "CR", "cited references")
      lv <- o$clevel %||% "references"
      NM <- bibliometrix::biblioNetwork(M, analysis = "coupling", network = lv, sep = ";")
      unit <- c(references = "documents", sources = "sources", authors = "authors")[[lv]]
      net <- bibliometrix::networkPlot(NM, n = o$nnodes %||% 40, Title = paste("Bibliographic coupling of", unit), type = "fruchterman",
                                       size.cex = TRUE, size = 15, labelsize = .7, remove.multiple = FALSE, edgesize = 3, cluster = "louvain",
                                       remove.isolates = TRUE, verbose = FALSE)
      list(kind = "base", plot = net_drawer(net), table = net_table(net),
           note = paste("Two", unit, "are linked when they cite the same references. Colours are clusters (Louvain).")) }),
  coupling_map = list(group = "Intellectual structure", label = "Clustering by coupling", slow = TRUE,
    run = function(M, R, o) { need(M, "CR", "cited references")
      cm <- bibliometrix::couplingMap(M, analysis = "documents", field = "CR", n = 250, impact.measure = "local",
                                      minfreq = 3, size = .5, n.labels = 2, repel = TRUE)
      tab <- as.data.frame(cm$clusters)
      list(kind = "gg", plot = cm$map, table = tab,
           note = "Clusters of documents that share references, placed by centrality (x) and impact (y), as in Biblioshiny's Clustering by Coupling.") }),
  historiograph = list(group = "Intellectual structure", label = "Historiograph", slow = TRUE,
    run = function(M, R, o) { need_refs(M)
      hn <- bibliometrix::histNetwork(M, min.citations = 1, sep = ";", verbose = FALSE)
      hp <- bibliometrix::histPlot(hn, n = 20, size = 5, labelsize = 4, verbose = FALSE)
      g <- hp$g %||% NULL
      if (inherits(g, "ggplot")) return(list(kind = "gg", plot = g, table = head(hn$histData, 50)))
      list(kind = "base", plot = function() bibliometrix::histPlot(hn, n = 20, size = 5, labelsize = 4, verbose = FALSE), table = head(hn$histData, 50)) }),

  # ---- Social structure
  collab_net = list(group = "Social structure", label = "Collaboration network",
    opts = c("level", "nnodes"), run = function(M, R, o) { lv <- o$level %||% "authors"
      if (lv == "countries") M <- bibliometrix::metaTagExtraction(M, "AU_CO", sep = ";")
      if (lv == "universities") M <- bibliometrix::metaTagExtraction(M, "AU_UN", sep = ";")
      NM <- bibliometrix::biblioNetwork(M, analysis = "collaboration", network = lv, sep = ";")
      n <- o$nnodes %||% 40
      net <- bibliometrix::networkPlot(NM, n = n, Title = paste("Collaboration network -", lv), type = "fruchterman",
                                                    size.cex = TRUE, size = 15, labelsize = .8, remove.multiple = FALSE, edgesize = 3,
                                                    cluster = "louvain", remove.isolates = TRUE, verbose = FALSE)
      list(kind = "base", plot = net_drawer(net), table = net_table(net), note = "Colours are clusters (Louvain). Node size = degree.") }),
  country_collab_map = list(group = "Social structure", label = "Countries' collaboration world map",
    run = function(M, R, o) { need(M, "C1", "affiliations")
      M2 <- bibliometrix::metaTagExtraction(M, "AU_CO", sep = ";")
      pairs <- do.call(rbind, lapply(strsplit(M2$AU_CO, ";"), function(x) { x <- unique(trimws(x)); x <- x[nzchar(x) & x != "NA"]
        if (length(x) < 2) return(NULL); cmb <- utils::combn(sort(x), 2); data.frame(From = cmb[1, ], To = cmb[2, ]) }))
      if (is.null(pairs)) stop("No international co-authorships in this dataset.")
      df <- stats::aggregate(list(Frequency = rep(1, nrow(pairs))), by = pairs, FUN = sum)
      df <- df[order(-df$Frequency), ]
      top <- head(df, 40)
      nice <- function(x) { x <- tools::toTitleCase(tolower(x)); x[x == "Usa"] <- "United States"; x[x == "Uk"] <- "United Kingdom"; x }
      # static map (works offline): links drawn between country centres taken from the 'maps' package
      w <- ggplot2::map_data("world"); w <- w[w$region != "Antarctica", ]
      main <- do.call(rbind, lapply(split(w, w$region), function(d) { g <- names(which.max(table(d$group))); d <- d[d$group == as.numeric(g), ]
        data.frame(region = d$region[1], lon = mean(range(d$long)), lat = mean(range(d$lat))) }))
      key <- function(x) { x <- toupper(trimws(x)); x[x == "UNITED STATES"] <- "USA"; x[x == "UNITED KINGDOM"] <- "UK"; x[x == "KOREA"] <- "SOUTH KOREA"; x }
      main$KEY <- toupper(main$region)
      top$x <- main$lon[match(key(top$From), main$KEY)]; top$y <- main$lat[match(key(top$From), main$KEY)]
      top$xend <- main$lon[match(key(top$To), main$KEY)]; top$yend <- main$lat[match(key(top$To), main$KEY)]
      seg <- top[stats::complete.cases(top[, c("x", "y", "xend", "yend")]), ]
      g <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = w, ggplot2::aes(long, lat, group = group), fill = "#E4E1D8", colour = "white", linewidth = .1) +
        ggplot2::geom_curve(data = seg, ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linewidth = Frequency),
                            colour = PAL[["accent"]], alpha = .6, curvature = .2) +
        ggplot2::scale_linewidth(range = c(.3, 2.5)) + ggplot2::coord_quickmap() +
        ggplot2::labs(title = "Countries' collaboration (top 40 links)", x = NULL, y = NULL) + theme_anchor() +
        ggplot2::theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), legend.position = "right")
      list(kind = "gg", plot = g, table = head(df, 100)) })
)

`%|%` <- function(a, b) ifelse(is.na(a), b, a)

# yearly production of the top items of a tag (affiliations or countries), cumulative, as in Biblioshiny
prod_over_time <- function(M, tag, what, title, top = 5) {
  M2 <- if (has(M, tag)) M else bibliometrix::metaTagExtraction(M, tag, sep = ";")
  rows <- do.call(rbind, lapply(seq_len(nrow(M2)), function(i) {
    x <- unique(trimws(strsplit(as.character(M2[[tag]][i]), ";")[[1]]))
    x <- x[!is.na(x) & nzchar(x) & !x %in% c("NA", "NOTREPORTED", "NOTDECLARED")]
    if (!length(x) || is.na(M2$PY[i])) return(NULL)
    data.frame(Item = x, Year = as.integer(M2$PY[i]))
  }))
  if (is.null(rows) || !nrow(rows)) stop(sprintf("No %s could be read from this dataset.", tolower(what)))
  keep <- names(sort(table(rows$Item), decreasing = TRUE))[seq_len(min(top, length(unique(rows$Item))))]
  yrs <- seq(min(rows$Year), max(rows$Year))
  tab <- do.call(rbind, lapply(keep, function(k) { n <- table(factor(rows$Year[rows$Item == k], levels = yrs))
    nm <- tools::toTitleCase(tolower(k)); nm <- c(Usa = "USA", Uk = "UK")[nm] %|% nm
    data.frame(Item = nm, Year = yrs, Articles = as.integer(cumsum(n))) }))
  g <- ggplot2::ggplot(tab, ggplot2::aes(Year, Articles, colour = Item)) + ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(title = title, y = "Cumulative articles", colour = what) + theme_anchor() + ggplot2::theme(legend.position = "bottom")
  names(tab)[1] <- what
  list(kind = "gg", plot = g, table = tab)
}

# Analyses marked slow run here, in a separate R process (callr), so the app stays responsive.
bg_analysis <- function(app_dir, id, M, R, o, libs, png_path, pdf_path = NULL) {
  .libPaths(libs)
  suppressPackageStartupMessages({ library(shiny); library(bibliometrix) })
  for (f in list.files(file.path(app_dir, "R"), full.names = TRUE, pattern = "\\.R$")) source(f, local = FALSE, encoding = "UTF-8")
  register_fonts(app_dir)
  out <- run_analysis(id, M, R, o)
  if (out$kind == "base") {
    grDevices::png(png_path, width = 1500, height = 930, res = 150); on.exit(grDevices::dev.off())
    out$plot(); out$png <- png_path
    if (!is.null(pdf_path)) try({ pdf_device()(pdf_path, width = 10, height = 6.2); out$plot(); grDevices::dev.off(); out$pdf <- pdf_path }, silent = TRUE)
    out$plot <- NULL
  }
  out
}

# Turn bibliometrix's technical errors into advice the researcher can act on.
friendly_error <- function(msg, M) {
  if (grepl("^(This analysis needs|This export|No |Analysis cancelled|Field )", msg)) return(msg)
  n <- if (is.null(M)) 0 else nrow(M)
  if (n < 50) return(sprintf("This analysis could not run on these %d records. Most bibliometric maps need more documents (aim for at least 50 to 100) with enough shared keywords, references or co-authors. Technical detail: %s", n, msg))
  paste0("This analysis could not run on this dataset. Check that your export includes all fields (keywords, cited references, affiliations), or try fewer nodes or another field. Technical detail: ", msg)
}

# Some exports (e.g. Scopus or Lens CSV) lack columns that bibliometrix functions expect; add them as empty.
normalize_M <- function(M) {
  cls <- class(M)
  chr <- c("AU", "TI", "SO", "DE", "ID", "CR", "C1", "DT", "PP", "VL", "DI", "J9", "JI", "AB", "LA", "RP", "DB", "BP", "EP")
  for (f in setdiff(chr, names(M))) M[[f]] <- NA_character_
  # countries and affiliations are derived from C1 by bibliometrix when the column is absent; an empty placeholder column
  # would stop that (international co-authorship then showed 0% and every country as multi-country), so drop empty ones
  for (f in c("AU_CO", "AU_UN", "AU1_CO")) if (f %in% names(M) && all(is.na(M[[f]]) | !nzchar(as.character(M[[f]])))) M[[f]] <- NULL
  for (f in setdiff(c("PY", "TC"), names(M))) M[[f]] <- NA_real_
  # OpenAlex exports keep keywords in KW_Merged rather than DE
  if (!has(M, "DE") && "KW_Merged" %in% names(M)) M$DE <- toupper(M$KW_Merged)
  M$PY <- suppressWarnings(as.numeric(M$PY)); M$TC <- suppressWarnings(as.numeric(M$TC))
  if (!"SR" %in% names(M)) M <- tryCatch(bibliometrix::metaTagExtraction(M, "SR"), error = function(e) { M$SR <- paste(sub(";.*", "", M$AU), M$PY, M$SO, sep = ", "); M })
  class(M) <- unique(c(cls, "data.frame"))
  M
}

field_label <- function(f) switch(f, DE = "author keywords", ID = "Keywords Plus", TI = "titles", AB = "abstracts", f)

OA_UNSUPPORTED <- c("local_cited_sources", "local_cited_refs", "rpys")
run_analysis <- function(id, M, R, o = list()) {
  if (id %in% OA_UNSUPPORTED && "DB" %in% names(M) && all(toupper(M$DB) == "OPENALEX", na.rm = TRUE))
    stop("This analysis needs cited references written out with source names and years, which OpenAlex gives only as work ids. Use a Web of Science or Scopus export for it; all other analyses run on OpenAlex records.", call. = FALSE)
  a <- ANALYSES[[id]]
  res <- a$run(M, R, o)
  res$label <- a$label; res$group <- a$group
  res
}

# Short text summary of the dataset for the Phase C prompt
biblio_summary_text <- function(M, R) {
  if (is.null(M)) return("(no bibliometric data imported)")
  out <- c(sprintf("Documents: %d; years %s-%s.", nrow(M), min(M$PY, na.rm = TRUE), max(M$PY, na.rm = TRUE)))
  if (has(M, "SO")) out <- c(out, paste("Top sources:", paste(head(names(sort(table(M$SO), decreasing = TRUE)), 8), collapse = "; ")))
  if (!is.null(R) && length(R$Authors)) out <- c(out, paste("Top authors:", paste(head(names(R$Authors), 8), collapse = "; ")))
  if (has(M, "DE")) out <- c(out, paste("Top author keywords:", paste(head(trimws(names(bibliometrix::tableTag(M, "DE"))), 15), collapse = "; ")))
  if (!is.null(R) && length(R$MostCitedPapers)) out <- c(out, paste("Most cited:", paste(head(R$MostCitedPapers[[1]], 8), collapse = "; ")))
  paste(out, collapse = "\n")
}

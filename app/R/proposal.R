# ANCHOR Studio - Proposal / thesis tab (1.7.0): a research proposal built from the approved phases, your own text,
# a timeline, figures and attachments, exported to Word on your own template.
# Decisions (Proposal and Thesis Tab Audit, 31 Oct 2026): proposal first; upload any template, one preset as an example;
# AI marked everywhere (the tab itself drafts nothing); attachments inside the project up to 20 MB; citation style from the
# preset or your choice, APA 7 otherwise; language chosen per document; the same tab on the phone.

PROPOSAL_ATTACH_MAX <- 20 * 1024^2
PROP_PHASES <- c("A", "N", "C", "H", "O", "R")   # the phase ids (app.R has its own copy; R/ files cannot see it)

prop_words <- local({ cache <- NULL; function(lang = "en") {
  if (is.null(cache)) { f <- c(file.path(getOption("anchor.app_dir", getwd()), "www", "proposal_i18n.json"), file.path("www", "proposal_i18n.json"))
    f <- f[file.exists(f)][1]; cache <<- jsonlite::fromJSON(f, simplifyVector = FALSE) }
  cache[[if (lang %in% names(cache)) lang else "en"]] } })

PROPOSAL_PRESETS <- list(
  general = list(name = "Research proposal (general outline)", cite = "apa", toc = TRUE, page_limit = NA,
    layout = list(font = "Times New Roman", size = 12, line = 1.5, margins = c(l = 2.5, r = 2.5, t = 2.5, b = 2.5)),
    sections = list(list(kind = "intro", links = c("A", "H")), list(kind = "rq", links = "H"), list(kind = "state", links = c("N", "C")),
                    list(kind = "method", links = "O"), list(kind = "timeline"), list(kind = "resources"), list(kind = "ethics"),
                    list(kind = "contribution"), list(kind = "appendix"))),
  fernuni_expose = list(name = "FernUniversit\u00e4t in Hagen, Expos\u00e9 (Politikwissenschaft I guide)", cite = "apa", toc = FALSE, page_limit = 8,
    source = "https://www.fernuni-hagen.de/KSW/bapo/studieninhalte/2014ws/pdf/leitfaden-expose.pdf",
    layout = list(font = "Times New Roman", size = 12, line = 1.5, margins = c(l = 2.5, r = 2.5, t = 2.5, b = 2.5)),
    sections = list(list(kind = "intro", links = c("H", "A")), list(kind = "concepts", links = "C"), list(kind = "state", links = c("N", "C")),
                    list(kind = "outline")))
)
PROPOSAL_KINDS <- c("intro", "rq", "concepts", "state", "method", "outline", "timeline", "resources", "ethics", "contribution", "text", "appendix")

prop_id <- function() paste0("s", format(as.numeric(Sys.time()) * 1000, scientific = FALSE, digits = 15), sample(100:999, 1))
prop_section <- function(kind, lang = "en", links = character(0), title = NULL)
  list(id = prop_id(), kind = kind, title = title %||% prop_words(lang)$kinds[[kind]] %||% kind, links = as.character(links),
       link_text = list(), text = "", figs = character(0))

new_proposal <- function(preset = "general", lang = "en", st = NULL) {
  p <- PROPOSAL_PRESETS[[preset]] %||% PROPOSAL_PRESETS$general
  secs <- lapply(p$sections, function(s) { x <- prop_section(s$kind, lang, s$links %||% character(0)); Sys.sleep(0.002); x })
  pr <- list(kind = "proposal", preset = preset, lang = lang, cite_style = p$cite, toc = p$toc, page_limit = p$page_limit, word_limit = NA_real_,
             layout = p$layout, template = NULL, only_cited = TRUE,
             title = st$article$title %||% "", subtitle = "", author = st$setup$S_name %||% st$prereq$name %||% "", programme = "", supervisor = "", place = "",
             sections = secs, milestones = data.frame(milestone = character(), start = character(), end = character(), stringsAsFactors = FALSE),
             attachments = list(), refs = list(), created = format(Sys.time(), "%Y-%m-%d %H:%M"))
  if (!is.null(st)) for (k in seq_along(pr$sections)) pr$sections[[k]] <- prop_link_now(pr$sections[[k]], st)
  pr
}

# the text of a phase as it goes into a proposal: the draft without the model's questions
phase_text <- function(st, id) trimws(strip_q(st$phases[[id]]$draft %||% ""))
# a link remembers the phase text it was made with; when the phase changes, the section is flagged until you accept the new text
prop_link_now <- function(sec, st) { sec$link_text <- setNames(lapply(sec$links, function(j) phase_text(st, j)), sec$links); sec }
prop_stale_links <- function(sec, st) Filter(function(j) !identical(sec$link_text[[j]] %||% "", phase_text(st, j)), sec$links)

# AI provenance of a phase's current text, from the versions of its draft (and the prompt log for older projects)
phase_ai <- function(st, id) {
  v <- st$draft_versions %||% data.frame(phase = character(), source = character(), text = character())
  v <- v[v$phase == id, , drop = FALSE]; ai <- v[grepl("^AI ", v$source), , drop = FALSE]
  cur <- st$phases[[id]]$draft %||% ""
  if (!nrow(ai)) { n <- sum((st$prompts$phase %||% character(0)) == id)
    return(if (n) list(ai = TRUE, unchanged = FALSE, tool = "") else list(ai = FALSE)) }
  tool <- sub("^.*\\((.*)\\)$", "\\1", ai$source[nrow(ai)]); if (identical(tool, ai$source[nrow(ai)])) tool <- ""
  list(ai = TRUE, unchanged = any(ai$text == cur), tool = tool)
}

# ---------------------------------------------------------------- text, words, citations
md_blocks <- function(x) {
  L <- strsplit(x %||% "", "\n", fixed = TRUE)[[1]]; out <- list(); i <- 1; para <- character(0)
  flush <- function() { if (length(para)) out[[length(out) + 1]] <<- list(type = "p", text = paste(trimws(para), collapse = " ")); para <<- character(0) }
  while (i <= length(L)) { l <- L[i]
    if (!nzchar(trimws(l))) { flush(); i <- i + 1; next }
    if (grepl("^#{1,6} ", l)) { flush(); out[[length(out) + 1]] <- list(type = "h", level = nchar(sub(" .*$", "", l)), text = trimws(sub("^#+ ", "", l))); i <- i + 1; next }
    if (grepl("^\\s*\\|", l)) { flush(); rows <- character(0)
      while (i <= length(L) && grepl("^\\s*\\|", L[i])) { rows <- c(rows, L[i]); i <- i + 1 }
      cells <- lapply(rows[!grepl("^\\s*\\|[-:| ]+\\|\\s*$", rows)], function(r) trimws(strsplit(sub("^\\s*\\|", "", sub("\\|\\s*$", "", r)), "|", fixed = TRUE)[[1]]))
      w <- max(lengths(cells)); cells <- lapply(cells, function(r) c(r, rep("", w - length(r))))
      if (length(cells)) out[[length(out) + 1]] <- list(type = "table", rows = cells); next }
    if (grepl("^\\s*[-*\u2022] ", l)) { flush(); out[[length(out) + 1]] <- list(type = "li", text = trimws(sub("^\\s*[-*\u2022] ", "", l))); i <- i + 1; next }
    if (grepl("^\\s*\\d+[.)] ", l)) { flush(); out[[length(out) + 1]] <- list(type = "ol", text = trimws(sub("^\\s*", "", l))); i <- i + 1; next }
    para <- c(para, l); i <- i + 1 }
  flush(); out
}
md_plain <- function(x) gsub("\\*\\*|__|`", "", x)
count_words <- function(x) { x <- md_plain(gsub("(?m)^\\s*\\|?[-:| ]+\\|\\s*$", "", x %||% "", perl = TRUE)); x <- gsub("[#|*>]", " ", x); length(Filter(nzchar, strsplit(trimws(x), "\\s+")[[1]])) }

# the body text of a section, in the order it is exported: linked phases first, then your own text
prop_section_text <- function(sec, st) paste(c(vapply(sec$links, function(j) phase_text(st, j), ""), sec$text %||% ""), collapse = "\n\n")
prop_counts <- function(pr, st) {
  body <- Filter(function(s) !s$kind %in% c("appendix"), pr$sections)
  n <- vapply(body, function(s) count_words(prop_section_text(s, st)), 0)
  data.frame(section = vapply(body, function(s) s$title, ""), words = n, stringsAsFactors = FALSE)
}

# in-text citations, author-year: "(Lim et al., 2021; Aria & Cuccurullo, 2017)" and "Lim et al. (2021)"
extract_citations <- function(x) {
  x <- md_plain(x %||% ""); out <- list()
  for (g in regmatches(x, gregexpr("\\(([^()]*?\\d{4}[a-z]?[^()]*?)\\)", x, perl = TRUE))[[1]]) for (part in strsplit(gsub("^\\(|\\)$", "", g), ";")[[1]]) {
    m <- regmatches(part, regexec("^\\s*(?:(?:see|e\\.g\\.,?|cf\\.|vgl\\.)\\s+)?([^,;()0-9]+?)\\s*,?\\s+(\\d{4})[a-z]?\\b", part, perl = TRUE))[[1]]
    if (length(m) == 3) out[[length(out) + 1]] <- c(first_surname(m[2]), m[3], trimws(part)) }
  for (m in regmatches(x, gregexpr("\\b([A-Z][\\p{L}'\\-]+)(?: et al\\.| (?:&|and|und) [A-Z][\\p{L}'\\-]+)? \\((\\d{4})[a-z]?\\)", x, perl = TRUE))[[1]]) {
    p <- regmatches(m, regexec("^(.+?) \\((\\d{4})", m, perl = TRUE))[[1]]; out[[length(out) + 1]] <- c(first_surname(p[2]), p[3], m) }
  if (!length(out)) return(data.frame(surname = character(), year = character(), text = character(), stringsAsFactors = FALSE))
  d <- unique(as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)); names(d) <- c("surname", "year", "text")
  d[nzchar(d$surname) & !tolower(d$surname) %in% c("table", "figure", "phase", "section", "eq", "equation", "model"), , drop = FALSE]
}
first_surname <- function(a) { a <- trimws(sub("\\s+et al\\.?.*$", "", a)); a <- strsplit(a, "\\s*(?:&|\\band\\b|\\bund\\b|,)\\s*", perl = TRUE)[[1]][1]
  w <- strsplit(trimws(a %||% ""), "\\s+")[[1]]; tolower(w[length(w)] %||% "") }

# every reference the proposal can cite: the anchor article, the accepted references of all phases, the built-in method
# references of the linked phases and the ones you add here
prop_ref_pool <- function(pr, st) {
  pool <- list()
  m <- regmatches(st$article$citation %||% "", regexec("^(.+?) \\((\\d{4})", st$article$citation %||% "", perl = TRUE))[[1]]
  if (length(m) == 3) pool[[length(pool) + 1]] <- list(key = "focal", au = m[2], year = m[3], text = st$article$citation, doi = st$article$doi %||% "")
  linked <- unique(unlist(lapply(pr$sections, `[[`, "links")))
  for (j in PROP_PHASES) { for (r in st$phases[[j]]$accepted %||% list()) pool[[length(pool) + 1]] <- c(list(key = paste0("acc_", j)), r)
    if (j %in% linked && exists("LESSONS")) for (k in LESSONS[[j]]$refs %||% character(0)) pool[[length(pool) + 1]] <- c(list(key = k), ref_live(k, st)) }
  for (r in pr$refs %||% list()) pool[[length(pool) + 1]] <- c(list(key = "added"), r)
  for (k in seq_along(pool)) { pool[[k]]$surname <- first_surname(sub(",.*$", "", pool[[k]]$au %||% "")); pool[[k]]$year <- as.character(pool[[k]]$year %||% "") }
  pool[!duplicated(vapply(pool, function(r) tolower(paste(r$doi %|e|% r$title %|e|% r$text, r$year)), ""))]
}
prop_check_citations <- function(pr, st) {
  cites <- extract_citations(paste(vapply(pr$sections, function(s) prop_section_text(s, st), ""), collapse = "\n"))
  pool <- prop_ref_pool(pr, st); keys <- vapply(pool, function(r) paste(r$surname, r$year), "")
  hit <- paste(cites$surname, cites$year) %in% keys
  used <- keys %in% paste(cites$surname, cites$year) | vapply(pool, function(r) identical(r$key, "added"), TRUE)
  list(citations = cites, missing = cites[!hit, , drop = FALSE], refs = pool[used], unused = pool[!used])
}

format_ref <- function(r, style = "apa") {
  if (!is.null(r$text) && is.null(r$title)) return(r$text)          # the anchor article, already in your chosen style
  if (identical(style, "harvard")) {
    au <- gsub(",\\s*&\\s*", " and ", r$au %||% ""); vi <- paste0(if (nzchar(r$vol %||% "")) paste0(", ", r$vol) else "", if (nzchar(r$iss %||% "")) paste0("(", r$iss, ")") else "")
    return(paste0(au, " (", r$year, ") '", sub("\\.$", "", r$title %||% ""), "', ", r$journal %||% "", vi, if (nzchar(r$pages %||% "")) paste0(", pp. ", gsub("-", "\u2013", r$pages)) else "",
                  if (nzchar(r$doi %||% "")) paste0(". doi:", r$doi) else "", "."))
  }
  ref_apa(r)
}

# what is still missing before export (the tab's checks)
prop_checks <- function(pr, st) {
  out <- character(0)
  if (!nzchar(trimws(pr$title %||% ""))) out <- c(out, "Give the proposal a title.")
  for (s in pr$sections) {
    if (s$kind %in% c("appendix")) next
    if (s$kind == "timeline" && !NROW(pr$milestones)) out <- c(out, sprintf("Section '%s': add at least one milestone.", s$title))
    else if (s$kind != "timeline" && !nzchar(trimws(prop_section_text(s, st)))) out <- c(out, sprintf("Section '%s' is empty.", s$title))
    for (j in s$links) if (!identical(st$phases[[j]]$status, "approved")) out <- c(out, sprintf("Section '%s' uses phase %s, which is not approved.", s$title, j))
    for (j in prop_stale_links(s, st)) out <- c(out, sprintf("Section '%s': phase %s changed since you linked it.", s$title, j))
  }
  cc <- prop_check_citations(pr, st)
  if (nrow(cc$missing)) out <- c(out, sprintf("No reference found for: %s.", paste(unique(cc$missing$text), collapse = "; ")))
  w <- sum(prop_counts(pr, st)$words); lim <- suppressWarnings(as.numeric(pr$word_limit))
  if (!is.na(lim) && lim > 0 && (w > lim * 1.1 || w < lim * 0.9)) out <- c(out, sprintf("%d words: outside %d \u00b1 10%%.", w, as.integer(lim)))
  out
}

# ---------------------------------------------------------------- images and Word export
img_dims <- function(raw) {
  if (length(raw) > 24 && identical(raw[1:4], as.raw(c(0x89, 0x50, 0x4e, 0x47)))) {
    be <- function(b) sum(as.integer(b) * 256^(3:0)); return(c(be(raw[17:20]), be(raw[21:24]))) }
  if (length(raw) > 4 && identical(raw[1:2], as.raw(c(0xff, 0xd8)))) { i <- 3
    while (i < length(raw) - 9) { if (raw[i] != as.raw(0xff)) { i <- i + 1; next }; mk <- as.integer(raw[i + 1])
      if (mk %in% c(0xc0:0xc3, 0xc5:0xc7, 0xc9:0xcb, 0xcd:0xcf)) return(c(as.integer(raw[i + 7]) * 256 + as.integer(raw[i + 8]), as.integer(raw[i + 5]) * 256 + as.integer(raw[i + 6])))
      i <- i + 2 + as.integer(raw[i + 2]) * 256 + as.integer(raw[i + 3]) } }
  c(1000, 620)
}
img_ext <- function(raw) if (length(raw) > 2 && identical(raw[1:2], as.raw(c(0xff, 0xd8)))) ".jpg" else ".png"

# a .dotx template is a .docx with another content type; officer reads .docx only
template_as_docx <- function(raw) {
  d <- tempfile("tpl"); dir.create(d); z <- tempfile(fileext = ".zip"); writeBin(raw, z); utils::unzip(z, exdir = d)
  ct <- file.path(d, "[Content_Types].xml")
  if (file.exists(ct)) { x <- readLines(ct, warn = FALSE, encoding = "UTF-8"); writeLines(gsub("wordprocessingml.template.main+xml", "wordprocessingml.document.main+xml", x, fixed = TRUE), ct, useBytes = TRUE) }
  out <- tempfile(fileext = ".docx"); old <- setwd(d); on.exit(setwd(old)); zip::zip(out, list.files(".", recursive = TRUE, all.files = TRUE)); out
}

# TRUE when the template's "heading 1" style carries automatic numbering (then the export does not add its own numbers)
template_numbers_headings <- function(raw) {
  d <- tempfile("tplh"); dir.create(d); z <- tempfile(fileext = ".zip"); writeBin(raw, z); x <- tryCatch(utils::unzip(z, files = "word/styles.xml", exdir = d), error = function(e) character(0))
  if (!length(x) || !file.exists(file.path(d, "word", "styles.xml"))) return(FALSE)
  s <- paste(readLines(file.path(d, "word", "styles.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  m <- regmatches(s, regexpr('<w:style [^>]*>\\s*<w:name w:val="heading 1"/>.*?</w:style>', s, perl = TRUE, ignore.case = TRUE))
  length(m) == 1 && grepl("<w:numPr>", m)
}

# officer's own base document numbers its headings; the proposal numbers its sections itself, so the numbering is removed
plain_base_docx <- function() {
  d <- tempfile("base"); dir.create(d); utils::unzip(system.file("template", "template.docx", package = "officer"), exdir = d)
  sx <- file.path(d, "word", "styles.xml"); x <- paste(readLines(sx, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  x <- gsub("<w:numPr>.*?</w:numPr>", "", x, perl = TRUE); writeLines(x, sx, useBytes = TRUE)
  out <- tempfile(fileext = ".docx"); old <- setwd(d); on.exit(setwd(old)); zip::zip(out, list.files(".", recursive = TRUE, all.files = TRUE)); out
}

build_proposal_docx <- function(st, path) {
  pr <- st$proposal; W <- prop_words(pr$lang %||% "en")
  doc <- officer::read_docx(if (!is.null(pr$template$raw)) template_as_docx(pr$template$raw) else plain_base_docx())
  si <- officer::styles_info(doc, type = "paragraph")
  sty <- function(ids, fallback = NULL) { k <- match(tolower(ids), tolower(si$style_id)); k <- k[!is.na(k)]; if (length(k)) si$style_name[k[1]] else fallback }
  normal <- sty(c("Normal", "Standard"), si$style_name[si$is_default][1] %||% "Normal")
  H <- function(l) sty(c(paste0("Heading", l), paste0("berschrift", l), paste0("Titre", l)), normal)
  s_title <- sty(c("Title", "Titel"), NULL); s_cap <- sty(c("Caption", "Beschriftung"), normal); s_li <- sty(c("ListBullet", "Aufzhlungszeichen"), normal); s_ol <- sty(c("ListNumber", "Listennummer"), normal)
  heading_numbered <- !is.null(pr$template$raw) && template_numbers_headings(pr$template$raw)   # the template numbers its chapters itself
  template_has_body <- !is.null(pr$template$raw) && nrow(officer::docx_summary(doc)) > 0 && any(nzchar(trimws(officer::docx_summary(doc)$text %||% "")))
  if (is.null(pr$template$raw)) {   # no template: the layout of the preset
    lay <- pr$layout %||% PROPOSAL_PRESETS$general$layout; cm <- function(x) x / 2.54
    doc <- officer::body_set_default_section(doc, officer::prop_section(page_margins = officer::page_mar(left = cm(lay$margins[["l"]]), right = cm(lay$margins[["r"]]), top = cm(lay$margins[["t"]]), bottom = cm(lay$margins[["b"]]), header = 0.5, footer = 0.5, gutter = 0)))
    doc <- tryCatch(officer::docx_set_paragraph_style(doc, style_id = "Normal", style_name = "Normal", fp_p = officer::fp_par(line_spacing = lay$line, padding.bottom = 6),
                                                       fp_t = officer::fp_text_lite(font.size = lay$size, font.family = lay$font)), error = function(e) doc)
  }
  # own styles for figures (single line spacing, so a picture is never cut to one line) and the centred title page
  doc <- tryCatch(officer::docx_set_paragraph_style(doc, style_id = "AnchorFigure", style_name = "Anchor figure", base_on = si$style_id[si$style_name == normal][1] %||% "Normal",
                    fp_p = officer::fp_par(text.align = "center", line_spacing = 1, padding.top = 6, padding.bottom = 2)), error = function(e) doc)
  doc <- tryCatch(officer::docx_set_paragraph_style(doc, style_id = "AnchorTitlePage", style_name = "Anchor title page", base_on = si$style_id[si$style_name == normal][1] %||% "Normal",
                    fp_p = officer::fp_par(text.align = "center", line_spacing = 1.15, padding.bottom = 8)), error = function(e) doc)
  si2 <- officer::styles_info(doc, type = "paragraph")
  s_fig <- if ("Anchor figure" %in% si2$style_name) "Anchor figure" else normal; s_tp <- if ("Anchor title page" %in% si2$style_name) "Anchor title page" else normal
  runs <- function(txt) { parts <- strsplit(txt, "**", fixed = TRUE)[[1]]
    do.call(officer::fpar, lapply(seq_along(parts), function(k) officer::ftext(gsub("`", "", parts[k], fixed = TRUE), if (k %% 2 == 0) officer::fp_text_lite(bold = TRUE) else NULL))) }
  par <- function(d, txt, style = normal) officer::body_add_fpar(d, runs(txt), style = style)
  add_md <- function(d, x, base = 1) {
    for (b in md_blocks(x)) d <- switch(b$type,
      h = par(d, b$text, H(min(base + b$level - 1, 3))),
      li = if (identical(s_li, normal)) par(d, paste0("\u2022 ", b$text)) else par(d, b$text, s_li),
      ol = par(d, b$text, normal),
      table = { m <- b$rows; df <- as.data.frame(do.call(rbind, m[-1]), stringsAsFactors = FALSE); if (!nrow(df)) df <- as.data.frame(t(m[[1]]), stringsAsFactors = FALSE)
                names(df) <- make.unique(md_plain(m[[1]])); officer::body_add_table(d, df) },
      par(d, b$text))
    d }
  img <- function(d, raw, caption) { if (is.null(raw) || !length(raw)) return(d)
    f <- tempfile(fileext = img_ext(raw)); writeBin(raw, f); wh <- img_dims(raw); w <- min(6, wh[1] / 150); h <- w * wh[2] / wh[1]
    d <- officer::body_add_img(d, f, width = w, height = h, style = s_fig); fig_n <<- fig_n + 1
    par(d, sprintf("%s %d: %s", W$figure, fig_n, caption), s_cap) }
  fig_n <- 0

  # title page: the template's own when it has one, otherwise ours
  if (!template_has_body) {
    for (i in 1:6) doc <- par(doc, "", s_tp)   # the title sits about a third down the page
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(pr$title %|e|% "Untitled", officer::fp_text_lite(font.size = 20, bold = TRUE))), style = s_tp)
    if (nzchar(pr$subtitle %||% "")) doc <- par(doc, pr$subtitle, s_tp)
    doc <- par(doc, "", s_tp); doc <- par(doc, if (identical(pr$preset, "fernuni_expose")) W$doc_expose else W$doc_proposal, s_tp); doc <- par(doc, "", s_tp)
    for (k in c("author", "programme", "supervisor")) if (nzchar(pr[[k]] %||% "")) doc <- par(doc, paste0(W[[k]], ": ", pr[[k]]), s_tp)
    doc <- par(doc, "", s_tp); doc <- par(doc, paste(c(if (nzchar(pr$place %||% "")) pr$place, format(Sys.Date(), "%d.%m.%Y")), collapse = ", "), s_tp)
    doc <- officer::body_add_break(doc)
  }
  if (isTRUE(pr$toc)) { doc <- par(doc, W$contents, H(1)); doc <- officer::body_add_toc(doc, level = 2); doc <- officer::body_add_break(doc) }

  items <- st$report_items %||% list(); att <- pr$attachments %||% list(); n <- 0
  for (s in pr$sections) {
    if (s$kind == "appendix") next
    n <- n + 1; doc <- par(doc, if (heading_numbered) s$title else paste(n, s$title), H(1))
    for (j in s$links) doc <- add_md(doc, phase_text(st, j), base = 2)
    if (nzchar(trimws(s$text %||% ""))) doc <- add_md(doc, s$text, base = 2)
    if (s$kind == "timeline" && NROW(pr$milestones)) { m <- pr$milestones; names(m) <- c(W$milestone, W$start, W$end); doc <- officer::body_add_table(doc, m)
      g <- tryCatch(prop_gantt_png(pr$milestones), error = function(e) NULL); if (!is.null(g)) doc <- img(doc, g, W$kinds$timeline) }
    for (f in s$figs %||% character(0)) {
      if (startsWith(f, "item:")) { it <- Filter(function(i) identical(i$label, sub("^item:", "", f)), items); if (length(it)) doc <- img(doc, it[[1]]$png_raw, paste(c(it[[1]]$label, it[[1]]$note), collapse = ". ")) }
      else { a <- Filter(function(a) identical(a$id, f), att); if (length(a) && length(a[[1]]$raw)) doc <- img(doc, a[[1]]$raw, a[[1]]$caption %|e|% a[[1]]$name) }
    }
  }
  # references: the cited ones, alphabetical
  cc <- prop_check_citations(pr, st); refs <- if (isTRUE(pr$only_cited %||% TRUE)) cc$refs else c(cc$refs, cc$unused)
  if (length(refs)) { doc <- par(doc, W$references, H(1))
    txt <- vapply(refs, format_ref, "", style = pr$cite_style %||% "apa"); for (r in txt[order(tolower(txt))]) doc <- par(doc, r) }
  # appendix: your appendix files (images shown, other files named); private attachments never leave the project
  app_secs <- Filter(function(s) s$kind == "appendix", pr$sections); apx <- Filter(function(a) identical(a$role, "appendix"), att)
  if (length(app_secs) && (length(apx) || any(nzchar(vapply(app_secs, function(s) s$text %||% "", ""))))) {
    doc <- par(doc, W$appendix, H(1)); for (s in app_secs) if (nzchar(trimws(s$text %||% ""))) doc <- add_md(doc, s$text, base = 2)
    for (a in apx) doc <- if (length(a$raw) && grepl("\\.(png|jpe?g)$", tolower(a$name))) img(doc, a$raw, a$caption %|e|% a$name) else par(doc, paste0(W$attached, ": ", a$name, if (nzchar(a$caption %||% "")) paste0(" \u2013 ", a$caption) else ""))
  }
  # declaration and AI statement (decision 3: AI use is marked, section by section)
  doc <- officer::body_add_break(doc); doc <- par(doc, W$ai_statement, H(1))
  lines <- character(0); n <- 0
  for (s in pr$sections) { if (s$kind == "appendix") next; n <- n + 1
    for (j in s$links) { a <- phase_ai(st, j); if (isTRUE(a$ai)) lines <- c(lines, sprintf("%d %s (phase %s): %s", n, s$title, j, gsub("{tool}", a$tool %|e|% "?", if (isTRUE(a$unchanged)) W$ai_unchanged else W$ai_edited, fixed = TRUE))) } }
  doc <- par(doc, if (length(lines)) W$ai_intro else W$ai_none); for (l in lines) doc <- if (identical(s_li, normal)) par(doc, paste0("\u2022 ", l)) else par(doc, l, s_li)
  doc <- par(doc, W$declaration, H(1)); doc <- par(doc, W$declaration_text)
  doc <- par(doc, paste0(W$place_date, ": ______________________    ", W$signature, ": ______________________"))
  print(doc, target = path); docx_fix_spacing(path); invisible(path)
}

# officer writes line spacing without a line rule, which some readers (LibreOffice) take as an exact height and so cut pictures
# down to one line; the rule "auto" (a multiple of single spacing) is what Word means. Also removes Normal's reference to itself.
docx_fix_spacing <- function(path) {
  d <- tempfile("fix"); dir.create(d); utils::unzip(path, exdir = d)
  for (f in file.path(d, "word", c("styles.xml", "document.xml"))) if (file.exists(f)) {
    x <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    x <- gsub('(<w:spacing (?:(?!lineRule)[^>])*?w:line="\\d+")((?:(?!lineRule)[^>])*/>)', '\\1 w:lineRule="auto"\\2', x, perl = TRUE)
    x <- gsub('(w:styleId="Normal">\\s*<w:name w:val="Normal"/>)\\s*<w:basedOn w:val="Normal"/>', '\\1', x, perl = TRUE)
    writeLines(x, f, useBytes = TRUE) }
  old <- setwd(d); on.exit(setwd(old)); unlink(path); zip::zip(path, list.files(".", recursive = TRUE, all.files = TRUE)); invisible(path)
}

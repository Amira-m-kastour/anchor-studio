# ANCHOR Studio - Proposal / thesis tab: PDF (printed from HTML by Edge or Chrome), LaTeX package and the timeline chart.
# The Word export is in proposal.R; all three exports share prop_parts(), so they always hold the same content.

# timeline as a Gantt chart: bars from start to end, a diamond for a one-day milestone
prop_gantt_png <- function(ms) {
  if (!NROW(ms)) return(NULL)
  d <- data.frame(task = ms$milestone, start = suppressWarnings(as.Date(ms$start)), end = suppressWarnings(as.Date(ms$end)), stringsAsFactors = FALSE)
  d <- d[!is.na(d$start), , drop = FALSE]; if (!nrow(d)) return(NULL)
  d$end[is.na(d$end)] <- d$start[is.na(d$end)]; d$task <- factor(d$task, levels = rev(unique(d$task)))
  g <- ggplot2::ggplot(d) +
    ggplot2::geom_segment(ggplot2::aes(x = start, xend = end, y = task, yend = task), data = d[d$end > d$start, , drop = FALSE], linewidth = 6, colour = "#B4561A") +
    ggplot2::geom_point(ggplot2::aes(x = start, y = task), data = d[d$end <= d$start, , drop = FALSE], shape = 18, size = 5, colour = "#14213D") +
    ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  f <- tempfile(fileext = ".png"); ggplot2::ggsave(f, g, width = 8, height = 1.2 + 0.42 * nrow(d), dpi = 160, bg = "white")
  readBin(f, "raw", file.info(f)$size)
}

# everything an export needs, in reading order
prop_parts <- function(st) {
  pr <- st$proposal; W <- prop_words(pr$lang %||% "en"); items <- st$report_items %||% list(); att <- pr$attachments %||% list()
  fig <- function(f) {
    if (startsWith(f, "item:")) { it <- Filter(function(i) identical(i$label, sub("^item:", "", f)), items)
      if (length(it) && length(it[[1]]$png_raw)) return(list(raw = it[[1]]$png_raw, caption = paste(c(it[[1]]$label, it[[1]]$note), collapse = ". "))) }
    else { a <- Filter(function(a) identical(a$id, f), att); if (length(a) && length(a[[1]]$raw)) return(list(raw = a[[1]]$raw, caption = a[[1]]$caption %|e|% a[[1]]$name)) }
    NULL }
  secs <- list(); n <- 0
  for (s in pr$sections) { if (s$kind == "appendix") next; n <- n + 1
    body <- paste(c(vapply(s$links, function(j) phase_text(st, j), ""), if (nzchar(trimws(s$text %||% ""))) s$text), collapse = "\n\n")
    secs[[length(secs) + 1]] <- list(n = n, title = s$title, md = body, figs = Filter(Negate(is.null), lapply(s$figs %||% character(0), fig)),
      milestones = if (s$kind == "timeline" && NROW(pr$milestones)) pr$milestones else NULL,
      gantt = if (s$kind == "timeline" && NROW(pr$milestones)) prop_gantt_png(pr$milestones) else NULL) }
  cc <- prop_check_citations(pr, st); refs <- if (isTRUE(pr$only_cited %||% TRUE)) cc$refs else c(cc$refs, cc$unused)
  rt <- vapply(refs, format_ref, "", style = pr$cite_style %||% "apa"); rt <- rt[order(tolower(rt))]
  app_secs <- Filter(function(s) s$kind == "appendix", pr$sections); apx <- Filter(function(a) identical(a$role, "appendix"), att)
  ai <- character(0); n <- 0
  for (s in pr$sections) { if (s$kind == "appendix") next; n <- n + 1
    for (j in s$links) { a <- phase_ai(st, j); if (isTRUE(a$ai)) ai <- c(ai, sprintf("%d %s (phase %s): %s", n, s$title, j, gsub("{tool}", a$tool %|e|% "?", if (isTRUE(a$unchanged)) W$ai_unchanged else W$ai_edited, fixed = TRUE))) } }
  list(pr = pr, W = W, sections = secs, refs = rt,
       appendix_text = paste(Filter(nzchar, vapply(app_secs, function(s) trimws(s$text %||% ""), "")), collapse = "\n\n"),
       appendix_files = lapply(apx, function(a) list(name = a$name, caption = a$caption %||% "", raw = if (grepl("\\.(png|jpe?g)$", tolower(a$name))) a$raw else NULL)),
       has_appendix = length(app_secs) > 0 && (length(apx) > 0 || any(nzchar(vapply(app_secs, function(s) trimws(s$text %||% ""), "")))),
       ai = ai, kind_label = if (identical(pr$preset, "fernuni_expose")) W$doc_expose else W$doc_proposal,
       date = format(Sys.Date(), "%d.%m.%Y"))
}

# ---------------------------------------------------------------- HTML (and PDF printed from it)
prop_md_html <- function(x, base = 2) {
  e <- function(s) { s <- htmltools::htmlEscape(md_plain_keep_bold(s)); gsub("\\*\\*(.+?)\\*\\*", "<b>\\1</b>", s, perl = TRUE) }
  out <- character(0); list_open <- ""
  close_list <- function() { if (nzchar(list_open)) { out <<- c(out, paste0("</", list_open, ">")); list_open <<- "" } }
  for (b in md_blocks(x)) {
    if (b$type %in% c("li", "ol")) { tag <- if (b$type == "li") "ul" else "ol"
      if (!identical(list_open, tag)) { close_list(); out <- c(out, paste0("<", tag, ">")); list_open <- tag }
      out <- c(out, paste0("<li>", e(if (b$type == "ol") sub("^\\d+[.)]\\s*", "", b$text) else b$text), "</li>")); next }
    close_list()
    out <- c(out, switch(b$type,
      h = sprintf("<h%d>%s</h%d>", min(base + b$level - 1, 4), e(b$text), min(base + b$level - 1, 4)),
      table = paste0("<table><thead><tr>", paste0("<th>", vapply(b$rows[[1]], e, ""), "</th>", collapse = ""), "</tr></thead><tbody>",
                     paste(vapply(b$rows[-1], function(r) paste0("<tr>", paste0("<td>", vapply(r, e, ""), "</td>", collapse = ""), "</tr>"), ""), collapse = ""), "</tbody></table>"),
      paste0("<p>", e(b$text), "</p>")))
  }
  close_list(); paste(out, collapse = "\n")
}
md_plain_keep_bold <- function(x) gsub("__|`", "", x)

build_proposal_html <- function(st) {
  P <- prop_parts(st); pr <- P$pr; W <- P$W; lay <- pr$layout %||% PROPOSAL_PRESETS$general$layout
  e <- htmltools::htmlEscape; rtl <- identical(pr$lang, "ar"); fig_n <- 0
  img <- function(raw, cap) { fig_n <<- fig_n + 1
    sprintf("<figure><img src='data:image/%s;base64,%s'><figcaption>%s %d: %s</figcaption></figure>", if (img_ext(raw) == ".jpg") "jpeg" else "png",
            jsonlite::base64_enc(raw), e(W$figure), fig_n, e(cap)) }
  css <- sprintf("@page{size:A4;margin:%scm %scm %scm %scm}body{font-family:'%s',serif;font-size:%spt;line-height:%s;color:#000;margin:0}
h1{font-size:1.3em;margin:1.4em 0 .5em}h2{font-size:1.12em}h3{font-size:1em}p{margin:0 0 .6em;text-align:justify}
.title{text-align:center;page-break-after:always;padding-top:28%%}.title h1{font-size:1.8em;margin:0 0 .4em}.title p{text-align:center}
table{border-collapse:collapse;margin:.6em 0;font-size:.9em}th,td{border:1px solid #888;padding:3px 6px;text-align:start;vertical-align:top}
figure{margin:1em 0;text-align:center}figure img{max-width:100%%}figcaption{font-size:.9em;margin-top:.3em}
.refs p{padding-inline-start:2em;text-indent:-2em;text-align:start}.decl{page-break-before:always}.sign{margin-top:3em}",
    lay$margins[["t"]], lay$margins[["r"]], lay$margins[["b"]], lay$margins[["l"]], lay$font, lay$size, lay$line)
  H <- c(sprintf("<!DOCTYPE html><html lang='%s' dir='%s'><head><meta charset='utf-8'><title>%s</title><style>%s</style></head><body>",
                 pr$lang %||% "en", if (rtl) "rtl" else "ltr", e(pr$title %|e|% "Untitled"), css),
    "<div class='title'>", sprintf("<h1>%s</h1>", e(pr$title %|e|% "Untitled")), if (nzchar(pr$subtitle %||% "")) sprintf("<p>%s</p>", e(pr$subtitle)),
    sprintf("<p>%s</p>", e(P$kind_label)),
    vapply(c("author", "programme", "supervisor"), function(k) if (nzchar(pr[[k]] %||% "")) sprintf("<p>%s: %s</p>", e(W[[k]]), e(pr[[k]])) else "", ""),
    sprintf("<p>%s</p>", e(paste(c(if (nzchar(pr$place %||% "")) pr$place, P$date), collapse = ", "))), "</div>")
  if (isTRUE(pr$toc)) H <- c(H, sprintf("<h1>%s</h1><ol>", e(W$contents)), vapply(P$sections, function(s) sprintf("<li>%s</li>", e(s$title)), ""), "</ol>")
  for (s in P$sections) {
    H <- c(H, sprintf("<h1>%d %s</h1>", s$n, e(s$title)), prop_md_html(s$md))
    if (!is.null(s$milestones)) { m <- s$milestones
      H <- c(H, sprintf("<table><thead><tr><th>%s</th><th>%s</th><th>%s</th></tr></thead><tbody>%s</tbody></table>", e(W$milestone), e(W$start), e(W$end),
                         paste(sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>", e(m$milestone), e(m$start), e(m$end)), collapse = "")))
      if (!is.null(s$gantt)) H <- c(H, img(s$gantt, W$kinds$timeline)) }
    for (f in s$figs) H <- c(H, img(f$raw, f$caption))
  }
  if (length(P$refs)) H <- c(H, sprintf("<h1>%s</h1><div class='refs'>", e(W$references)), paste0("<p>", e(P$refs), "</p>"), "</div>")
  if (P$has_appendix) { H <- c(H, sprintf("<h1>%s</h1>", e(W$appendix)), if (nzchar(P$appendix_text)) prop_md_html(P$appendix_text))
    for (a in P$appendix_files) H <- c(H, if (!is.null(a$raw)) img(a$raw, a$caption %|e|% a$name) else sprintf("<p>%s: %s%s</p>", e(W$attached), e(a$name), if (nzchar(a$caption)) paste0(" \u2013 ", e(a$caption)) else "")) }
  H <- c(H, sprintf("<div class='decl'><h1>%s</h1><p>%s</p>", e(W$ai_statement), e(if (length(P$ai)) W$ai_intro else W$ai_none)),
         if (length(P$ai)) c("<ul>", paste0("<li>", e(P$ai), "</li>"), "</ul>"),
         sprintf("<h1>%s</h1><p>%s</p><p class='sign'>%s: ______________________ &nbsp;&nbsp; %s: ______________________</p></div>", e(W$declaration), e(W$declaration_text), e(W$place_date), e(W$signature)),
         "</body></html>")
  paste(H, collapse = "\n")
}

build_proposal_pdf <- function(st, out) {
  br <- find_browser(); if (is.null(br)) stop("No Microsoft Edge or Google Chrome was found on this computer. Export the Word file and save it as PDF from Word.")
  d <- tempfile("anchor_prop"); dir.create(d); html <- file.path(d, "proposal.html"); writeLines(enc2utf8(build_proposal_html(st)), html, useBytes = TRUE)
  url <- paste0("file:///", sub("^/", "", gsub("\\\\", "/", normalizePath(html))))
  args <- c("--headless", "--disable-gpu", "--no-first-run", "--no-default-browser-check", "--disable-extensions", paste0("--user-data-dir=", file.path(d, "profile")),
            "--no-pdf-header-footer", "--print-to-pdf-no-header", paste0("--print-to-pdf=", normalizePath(out, mustWork = FALSE)),
            if (.Platform$OS.type == "unix" && identical(Sys.info()[["effective_user"]], "root")) "--no-sandbox", url)
  r <- processx::run(br, args, timeout = 120, error_on_status = FALSE)
  if (!file.exists(out) || file.size(out) < 1000) stop("The browser did not produce a PDF (", trimws(substr(r$stderr, 1, 200)), ").")
  invisible(out)
}

# ---------------------------------------------------------------- LaTeX package (for Overleaf or a local TeX; compile with XeLaTeX)
prop_tex_esc <- function(x) { x <- gsub("\\\\", "\\\\textbackslash{}", x); x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x); x <- gsub("\\^", "\\\\textasciicircum{}", x); x }
prop_tex_inline <- function(x) { x <- prop_tex_esc(md_plain_keep_bold(x)); gsub("\\*\\*(.+?)\\*\\*", "\\\\textbf{\\1}", x, perl = TRUE) }
prop_md_tex <- function(x) {
  out <- character(0); env <- ""
  close_env <- function() { if (nzchar(env)) { out <<- c(out, paste0("\\end{", env, "}")); env <<- "" } }
  for (b in md_blocks(x)) {
    if (b$type %in% c("li", "ol")) { e2 <- if (b$type == "li") "itemize" else "enumerate"
      if (!identical(env, e2)) { close_env(); out <- c(out, paste0("\\begin{", e2, "}")); env <- e2 }
      out <- c(out, paste0("  \\item ", prop_tex_inline(if (b$type == "ol") sub("^\\d+[.)]\\s*", "", b$text) else b$text))); next }
    close_env()
    out <- c(out, switch(b$type,
      h = paste0(c("\\subsection*{", "\\subsubsection*{", "\\paragraph*{")[min(b$level, 3)], prop_tex_inline(b$text), "}"),
      table = { w <- length(b$rows[[1]]); c("\\begin{center}\\small", paste0("\\begin{tabular}{", paste(rep("p{", w), sprintf("%.2f\\linewidth}", 0.9 / w), collapse = ""), "}"), "\\toprule",
                paste(paste(vapply(b$rows[[1]], prop_tex_inline, ""), collapse = " & "), "\\\\"), "\\midrule",
                vapply(b$rows[-1], function(r) paste(paste(vapply(r, prop_tex_inline, ""), collapse = " & "), "\\\\"), ""), "\\bottomrule", "\\end{tabular}", "\\end{center}") },
      c(prop_tex_inline(b$text), "")))
  }
  close_env(); out
}

build_proposal_latex_zip <- function(st, zipfile) {
  P <- prop_parts(st); pr <- P$pr; W <- P$W; lay <- pr$layout %||% PROPOSAL_PRESETS$general$layout
  root <- file.path(tempfile("anchor_prop_tex"), "proposal"); dir.create(file.path(root, "figures"), recursive = TRUE)
  fig_n <- 0
  fig <- function(raw, cap) { fig_n <<- fig_n + 1; fn <- sprintf("fig%02d%s", fig_n, img_ext(raw)); writeBin(raw, file.path(root, "figures", fn))
    c("\\begin{figure}[htbp]\\centering", sprintf("\\includegraphics[width=\\linewidth,height=0.42\\textheight,keepaspectratio]{figures/%s}", fn),
      sprintf("\\caption{%s}", prop_tex_inline(cap)), "\\end{figure}") }
  lang <- c(en = "english", de = "german", ar = "arabic")[[pr$lang %||% "en"]]
  L <- c(sprintf("\\documentclass[%dpt,a4paper]{article}", as.integer(lay$size)),
    "% Your university or chair uses its own class? Replace the line above with it; the rest compiles unchanged in most classes.",
    "\\usepackage{fontspec}", "\\usepackage{polyglossia}", sprintf("\\setmainlanguage{%s}", lang), if (lang != "english") "\\setotherlanguage{english}",
    if (lang == "arabic") "\\newfontfamily\\arabicfont[Script=Arabic]{Amiri}" else sprintf("\\setmainfont{%s}", if (lay$font %in% c("Times New Roman")) "TeX Gyre Termes" else "TeX Gyre Termes"),
    sprintf("\\usepackage[left=%scm,right=%scm,top=%scm,bottom=%scm]{geometry}", lay$margins[["l"]], lay$margins[["r"]], lay$margins[["t"]], lay$margins[["b"]]),
    "\\usepackage{setspace}", sprintf("\\setstretch{%s}", lay$line), "\\usepackage{graphicx}", "\\usepackage{booktabs}", "\\usepackage{array}",
    "\\usepackage[hidelinks]{hyperref}", "\\setlength{\\parskip}{0.4em}", "\\setlength{\\parindent}{0pt}",
    "\\begin{document}", "\\begin{titlepage}\\centering\\vspace*{0.25\\textheight}",
    sprintf("{\\LARGE\\bfseries %s\\par}\\vspace{1em}", prop_tex_inline(pr$title %|e|% "Untitled")), if (nzchar(pr$subtitle %||% "")) sprintf("{\\large %s\\par}\\vspace{1em}", prop_tex_inline(pr$subtitle)),
    sprintf("{\\large %s\\par}\\vspace{2em}", prop_tex_inline(P$kind_label)),
    unlist(lapply(c("author", "programme", "supervisor"), function(k) if (nzchar(pr[[k]] %||% "")) sprintf("%s: %s\\par", prop_tex_inline(W[[k]]), prop_tex_inline(pr[[k]])))),
    sprintf("\\vfill %s\\par", prop_tex_inline(paste(c(if (nzchar(pr$place %||% "")) pr$place, P$date), collapse = ", "))), "\\end{titlepage}",
    if (isTRUE(pr$toc)) c(sprintf("\\renewcommand{\\contentsname}{%s}", prop_tex_inline(W$contents)), "\\tableofcontents", "\\clearpage"))
  for (s in P$sections) {
    L <- c(L, sprintf("\\section{%s}", prop_tex_inline(s$title)), prop_md_tex(s$md))
    if (!is.null(s$milestones)) { m <- s$milestones
      L <- c(L, "\\begin{center}\\small\\begin{tabular}{p{0.5\\linewidth}ll}\\toprule", sprintf("%s & %s & %s \\\\ \\midrule", prop_tex_inline(W$milestone), prop_tex_inline(W$start), prop_tex_inline(W$end)),
             sprintf("%s & %s & %s \\\\", vapply(m$milestone, prop_tex_inline, ""), vapply(m$start, prop_tex_inline, ""), vapply(m$end, prop_tex_inline, "")), "\\bottomrule\\end{tabular}\\end{center}")
      if (!is.null(s$gantt)) L <- c(L, fig(s$gantt, W$kinds$timeline)) }
    for (f in s$figs) L <- c(L, fig(f$raw, f$caption))
  }
  if (length(P$refs)) L <- c(L, sprintf("\\section*{%s}", prop_tex_inline(W$references)), sprintf("\\addcontentsline{toc}{section}{%s}", prop_tex_inline(W$references)),
                             "\\begin{list}{}{\\setlength{\\leftmargin}{2em}\\setlength{\\itemindent}{-2em}}", paste0("\\item ", vapply(P$refs, prop_tex_inline, "")), "\\end{list}")
  if (P$has_appendix) { L <- c(L, sprintf("\\section*{%s}", prop_tex_inline(W$appendix)), sprintf("\\addcontentsline{toc}{section}{%s}", prop_tex_inline(W$appendix)), if (nzchar(P$appendix_text)) prop_md_tex(P$appendix_text))
    for (a in P$appendix_files) L <- c(L, if (!is.null(a$raw)) fig(a$raw, a$caption %|e|% a$name) else sprintf("%s: %s%s\\par", prop_tex_inline(W$attached), prop_tex_inline(a$name), if (nzchar(a$caption)) paste0(" -- ", prop_tex_inline(a$caption)) else "")) }
  L <- c(L, "\\clearpage", sprintf("\\section*{%s}", prop_tex_inline(W$ai_statement)), prop_tex_inline(if (length(P$ai)) W$ai_intro else W$ai_none),
         if (length(P$ai)) c("\\begin{itemize}", paste0("  \\item ", vapply(P$ai, prop_tex_inline, "")), "\\end{itemize}"),
         sprintf("\\section*{%s}", prop_tex_inline(W$declaration)), prop_tex_inline(W$declaration_text), "",
         sprintf("\\vspace{3em}%s: \\rule{5cm}{0.4pt}\\hfill %s: \\rule{5cm}{0.4pt}", prop_tex_inline(W$place_date), prop_tex_inline(W$signature)), "\\end{document}")
  writeLines(enc2utf8(L), file.path(root, "main.tex"), useBytes = TRUE)
  writeLines(c("$pdf_mode = 5;   # XeLaTeX: needed for the fonts and for Arabic", "$xelatex = 'xelatex -interaction=nonstopmode %O %S';"), file.path(root, "latexmkrc"))
  writeLines(c("Compile main.tex with XeLaTeX (Overleaf: Menu > Compiler > XeLaTeX). The latexmkrc file sets this for a local TeX installation.",
               "Arabic needs the Amiri font (included in Overleaf and TeX Live)."), file.path(root, "README.txt"))
  old <- setwd(dirname(root)); on.exit(setwd(old)); if (file.exists(zipfile)) unlink(zipfile)
  zip::zip(normalizePath(zipfile, mustWork = FALSE), "proposal"); invisible(zipfile)
}

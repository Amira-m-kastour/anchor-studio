# ANCHOR Studio - PDF figures, LaTeX package, BibTeX and PDF report (version 1.1)

# ------------------------------------------------------------------ figures as vector PDF
pdf_device <- function() if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf

save_pdf_fig <- function(r, file) {
  if (r$kind == "gg") return(invisible(ggplot2::ggsave(file, r$plot, width = 10, height = 6.2, bg = "white", device = pdf_device())))
  if (r$kind == "base") {
    if (!is.null(r$pdf) && file.exists(r$pdf)) return(invisible(file.copy(r$pdf, file, overwrite = TRUE)))
    if (is.function(r$plot)) { pdf_device()(file, width = 10, height = 6.2); on.exit(grDevices::dev.off()); r$plot(); return(invisible(TRUE)) }
  }
  stop("This figure is interactive and has no PDF version; use the camera icon on the figure, or add the table.")
}

# ------------------------------------------------------------------ helpers
`%|e|%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a[1]) || !nzchar(trimws(a[1]))) b else a
safe_name <- function(x, n = 40) { x <- gsub("[^A-Za-z0-9]+", "_", substr(x %|e|% "review", 1, n)); gsub("^_|_$", "", x) }

# shift Markdown headings so the highest one in a draft becomes level `top`
md_shift_headings <- function(md, top = 2) {
  lines <- strsplit(md %||% "", "\n", fixed = TRUE)[[1]]
  h <- regmatches(lines, regexpr("^#{1,6}(?= )", lines, perl = TRUE))
  if (!length(h)) return(md)
  d <- top - min(nchar(h))
  out <- vapply(lines, function(l) { m <- regmatches(l, regexpr("^#{1,6}(?= )", l, perl = TRUE))
    if (length(m)) paste0(strrep("#", max(1, min(6, nchar(m) + d))), substring(l, nchar(m) + 1)) else l }, "", USE.NAMES = FALSE)
  paste(out, collapse = "\n")
}

# ------------------------------------------------------------------ LaTeX
tex_esc <- function(x) {
  x <- as.character(x %||% ""); x[is.na(x)] <- ""
  x <- gsub("\\\\", "\001", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x); x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("<", "\\\\textless{}", x); x <- gsub(">", "\\\\textgreater{}", x)
  gsub("\001", "\\\\textbackslash{}", x)
}
md_tex <- function(md) {
  md <- strip_q(md)
  if (!nzchar(trimws(md))) return("\\emph{Not completed.}\n")
  # pipe tables: commonmark's LaTeX writer has no tables, so lift them out and set them with tabularx
  lines <- strsplit(md_shift_headings(md, 2), "\n", fixed = TRUE)[[1]]; tabs <- list(); i <- 1
  while (i < length(lines)) {
    if (grepl("^\\s*\\|", lines[i]) && grepl("^\\s*\\|?\\s*:?-{2,}", lines[i + 1])) {
      j <- i + 1; while (j < length(lines) && grepl("^\\s*\\|", lines[j + 1])) j <- j + 1
      cells <- lapply(lines[c(i, (i + 2):j)][if (j >= i + 2) TRUE else 1], function(l) trimws(strsplit(gsub("^\\s*\\||\\|\\s*$", "", l), "|", fixed = TRUE)[[1]]))
      nc <- length(cells[[1]]); body <- do.call(rbind, lapply(cells[-1], function(r) r[seq_len(nc)]))
      df <- as.data.frame(if (is.null(body)) matrix(character(0), 0, nc) else body, stringsAsFactors = FALSE); names(df) <- cells[[1]]
      df[] <- lapply(df, function(v) { v[is.na(v)] <- ""; gsub("\\*\\*|__|`", "", v) })
      tabs[[length(tabs) + 1]] <- tex_table(df, 200)
      lines <- c(lines[seq_len(i - 1)], "", sprintf("ANCHORTABLE%d", length(tabs)), "", lines[-seq_len(j)])
    }
    i <- i + 1
  }
  # commonmark escapes every special character, so AI drafts cannot inject LaTeX commands
  out <- commonmark::markdown_latex(paste(lines, collapse = "\n"), extensions = TRUE, smart = FALSE)
  for (k in seq_along(tabs)) out <- sub(sprintf("ANCHORTABLE%d\n", k), paste0(paste(tabs[[k]], collapse = "\n"), "\n"), out, fixed = TRUE)
  out
}
tex_table <- function(df, n = 15, size = "\\small") {
  df <- head(as.data.frame(df), n); if (!ncol(df) || !nrow(df)) return("")
  num <- vapply(df, is.numeric, TRUE)
  spec <- paste0(ifelse(num, "r", "X"), collapse = "")
  c(sprintf("{%s\\begin{tabularx}{\\linewidth}{%s}\\toprule", size, spec),
    paste0(paste(sprintf("\\textbf{%s}", tex_esc(names(df))), collapse = " & "), "\\\\\\midrule"),
    apply(df, 1, function(r) paste0(paste(tex_esc(trimws(as.character(r))), collapse = " & "), "\\\\")),
    "\\bottomrule\\end{tabularx}}")
}

build_content_tex <- function(st, fig_names) {
  a <- st$article
  L <- c("% ANCHOR Studio - review content. \\input{content} from main.tex or from your journal template.",
    "% Needs: graphicx, booktabs, tabularx, float, hyperref (see README.txt).", "",
    sprintf("\\noindent\\textbf{Focal article:} %s~\\cite{focal}", tex_esc(a$citation)), "",
    sprintf("{\\small\\emph{%s}}", tex_esc(online_note(st))), "",
    "\\section*{Progress and decisions}", tex_table(data.frame(Phase = vapply(PHASES, function(p) paste(p$id, p$name), ""),
      Status = vapply(PHASES, function(p) st$phases[[p$id]]$status %||% "", ""),
      Approved = vapply(PHASES, function(p) st$phases[[p$id]]$approved_at %|e|% "--", ""))), "")
  for (p in PHASES) {
    ph <- st$phases[[p$id]]
    L <- c(L, sprintf("\\section{%s: %s}", p$id, tex_esc(p$name)), sprintf("\\emph{%s}\n", tex_esc(p$question)), md_tex(ph$draft))
    if (length(p$questions)) L <- c(L, "\\subsection*{Researcher's decisions}",
      tex_table(data.frame(Question = vapply(p$questions, `[[`, "", "label"),
        Answer = vapply(p$questions, function(q) paste(ph$answers[[q$id]] %||% "-", collapse = "; "), "")), 50))
    if (nzchar(ph$notes %||% "")) L <- c(L, "\\subsection*{Notes}", tex_esc(ph$notes))
    if (p$id == "N" && !is.null(f <- ds_findings(st))) L <- c(L, paste0("\\textbf{", tex_esc(f$text), "}\n"), if (!is.null(f$table)) tex_table(f$table, 200, "\\footnotesize"))
    L <- c(L, lessons_tex(p$id, st))
    if (p$id == "N" && !is.null(st$prisma)) { pr <- st$prisma
      L <- c(L, "\\subsection*{PRISMA 2020 flow}", tex_table(data.frame(Stage = c("Records identified from databases", "Duplicates removed",
        "Removed by filters (year, type, language)", "Records screened (title/abstract)", "Excluded at screening", "Reports assessed for eligibility",
        "Reports excluded (with reasons)", "Studies included in the review"),
        n = vapply(list(pr$identified, pr$duplicates, pr$filtered, pr$screened, pr$excluded_screen, pr$assessed, pr$excluded_full, pr$included),
                   function(v) as.character(v %|e|% "pending"), ""))))
    }
    if (p$id == "O" && !is.null(pv <- provenance_df(st$analysis$datasets))) L <- c(L, "\\subsection*{Data and provenance}", tex_table(pv[, c("Dataset", "Source", "Query", "Retrieved", "Rows", "Licensed")], 100, "\\footnotesize"))
    if (p$id == "A" && !is.null(fr <- find_report(st))) L <- c(L, "\\subsection*{How the focal article was chosen}", tex_esc(fr$text), "", tex_table(fr$table, 10, "\\footnotesize"), tex_esc(fr$note), "")
    if (p$id == "A" && !is.null(f <- st$focal_oa)) L <- c(L, "\\subsection*{Focal article in OpenAlex}", tex_esc(sprintf("%d references, cited by %s works (checked %s). %s", f$n_refs %||% 0L, f$n_citing %||% "?", f$retrieved %||% "",
        if (!is.null(f$open)) sprintf("Open copy: %s (%s).", f$open$url, f$open$version) else "No open copy found; read it through your library.")), "", if (!is.null(f$journal)) tex_table(f$journal, 5, "\\footnotesize"))
    if (p$id == "R") L <- c(L, "\\subsection*{Data availability statement}", tex_esc(data_availability(st)), "")
    if (p$id %in% c("C", "H", "O") && length(items_for(st, p$id))) {
      L <- c(L, switch(p$id, C = "\\subsection*{Bibliometric figures and tables}", H = "\\subsection*{Research-front trend}", "\\subsection*{Analysis results}"))
      for (k in seq_along(st$report_items)) { it <- st$report_items[[k]]; if (!identical(it$phase %||% "C", p$id)) next
        if (!is.na(fig_names[k])) L <- c(L, "\\begin{figure}[H]\\centering", sprintf("\\includegraphics[width=\\linewidth]{figures/%s}", fig_names[k]),
          sprintf("\\caption{%s.%s}", tex_esc(it$label), if (nzchar(it$note %||% "")) paste0(" ", tex_esc(it$note)) else ""), "\\end{figure}")
        else L <- c(L, sprintf("\\paragraph{%s.} %s", tex_esc(it$label), tex_esc(it$note)))
        if (!is.null(it$table)) L <- c(L, tex_table(it$table, 15, "\\footnotesize"), "")
        if (!is.null(it$code)) L <- c(L, "{\\footnotesize R code:}", "\\begin{verbatim}", it$code, "\\end{verbatim}")
      }
    }
  }
  pt <- prereq_tables(st)
  L <- c(L, "\\section*{Appendix: researcher, resources and licences}", "{\\small Recorded on the Before you start page. Licence summaries are short and not legal advice; follow each provider's terms.}", "",
    unlist(Map(function(nm, df) c(sprintf("\\subsection*{%s}", tex_esc(nm)), tex_table(df, 200, "\\footnotesize")), names(pt), pt)))
  L <- c(L, "\\section*{Decision log}",
    if (nrow(st$decisions)) tex_table(st$decisions[, c("time", "phase", "decision", "note")], 500, "\\footnotesize") else "No decisions yet.",
    "\\section*{AI-use disclosure}", "\\begin{quote}\\small", paste(tex_esc(strsplit(disclosure_text(st), "\n")[[1]]), collapse = "\\\\\n"), "\\end{quote}",
    "Bibliometric analyses: bibliometrix~\\cite{aria2017}; framework: ANCHOR~\\cite{kastour2026}.", "",
    "\\section*{Prompt log}",
    if (nrow(st$prompts)) unlist(lapply(seq_len(nrow(st$prompts)), function(i) c(sprintf("\\subsection*{%s, phase %s, %s}", tex_esc(st$prompts$time[i]), tex_esc(st$prompts$phase[i]), tex_esc(st$prompts$mode[i])),
      "{\\footnotesize", paste(tex_esc(strsplit(substr(st$prompts$prompt[i], 1, 4000), "\n")[[1]]), collapse = "\\\\\n"), "}"))) else "No prompts were used.")
  paste(L, collapse = "\n")
}

main_tex <- function(st) {
  a <- st$article; s <- st$setup
  paste(c("% !TEX program = xelatex",
    "% ANCHOR Studio report. Compile with XeLaTeX or LuaLaTeX (Overleaf: picked automatically from latexmkrc).",
    "\\documentclass[11pt,a4paper]{article}",
    "\\usepackage{iftex}",
    "\\ifPDFTeX",
    "  % pdfLaTeX fallback: Latin text plus common Greek letters; XeLaTeX is recommended for other scripts",
    "  \\usepackage[utf8]{inputenc}\\usepackage[T1]{fontenc}\\usepackage{textcomp,newunicodechar}\\IfFileExists{lmodern.sty}{\\usepackage{lmodern}}{}",
    paste0("  ", paste(sprintf("\\newunicodechar{%s}{\\ensuremath{\\%s}}", c("\u03b1","\u03b2","\u03b3","\u03b4","\u03b5","\u03bb","\u03bc","\u03c0","\u03c1","\u03c3","\u03c4","\u03c7","\u03c9","\u0394","\u03a3","\u03a9"),
      c("alpha","beta","gamma","delta","varepsilon","lambda","mu","pi","rho","sigma","tau","chi","omega","Delta","Sigma","Omega")), collapse = "")),
    "\\else",
    "  \\usepackage{fontspec}",
    "  \\IfFontExistsTF{Libertinus Serif}{\\setmainfont{Libertinus Serif}}{%",
    "    \\IfFontExistsTF{Noto Serif}{\\setmainfont{Noto Serif}}{%",
    "      \\setmainfont{LiberationSans}[Path=fonts/, Extension=.ttf, UprightFont=*-Regular, BoldFont=*-Bold, ItalicFont=*-Italic, BoldItalicFont=*-BoldItalic]}}",
    "\\fi",
    "\\usepackage[margin=2.5cm]{geometry}",
    "\\usepackage{graphicx,booktabs,tabularx,float,xcolor}",
    "\\usepackage[hidelinks]{hyperref}",
    "\\providecommand{\\tightlist}{\\setlength{\\itemsep}{0pt}\\setlength{\\parskip}{0pt}}",
    "\\IfFileExists{ulem.sty}{\\usepackage[normalem]{ulem}}{}\\providecommand{\\sout}[1]{#1}",
    "\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0.5em}",
    sprintf("\\title{%s}", tex_esc(a$title %|e|% "ANCHOR review")),
    sprintf("\\author{%s%s}", tex_esc(s$S_name), if (nzchar(s$S_inst %||% "")) paste0("\\\\\\small ", tex_esc(s$S_inst)) else ""),
    sprintf("\\date{Generated %s with %s %s}", format(Sys.Date(), "%d %B %Y"), tex_esc(APP_BRAND), APP_VERSION),
    "\\IfFileExists{fancyhdr.sty}{\\usepackage{fancyhdr}\\pagestyle{fancy}\\fancyhf{}\\renewcommand{\\headrulewidth}{0pt}",
    sprintf("  \\fancyfoot[L]{\\footnotesize %s}\\fancyfoot[R]{\\footnotesize\\thepage}\\fancypagestyle{plain}{\\fancyhf{}\\fancyfoot[L]{\\footnotesize %s}\\fancyfoot[R]{\\footnotesize\\thepage}\\renewcommand{\\headrulewidth}{0pt}}}{}", tex_esc(APP_BRAND), tex_esc(APP_BRAND)),
    "\\begin{document}", "\\maketitle", "\\input{content}", "",
    "\\bibliographystyle{apalike}", "\\bibliography{references}", "\\end{document}"), collapse = "\n")
}

# ------------------------------------------------------------------ BibTeX
bib_esc <- function(x) { x <- as.character(x %||% ""); x[is.na(x)] <- ""; x <- gsub("[{}]", "", x); x <- gsub("\\\\", "", x); gsub("([&%$#_])", "\\\\\\1", x) }
sentence_case <- function(x) { x <- tolower(x); paste0(toupper(substr(x, 1, 1)), substring(x, 2)) }
bib_authors <- function(au) {
  a <- trimws(strsplit(au %|e|% "", ";")[[1]]); a <- a[nzchar(a)]
  if (!length(a)) return("")
  paste(vapply(a, function(x) { p <- strsplit(x, " ")[[1]]
    if (length(p) < 2) return(tools::toTitleCase(tolower(x)))
    ini <- p[length(p)]; sur <- paste(p[-length(p)], collapse = " ")
    if (!grepl("^[A-Z]{1,4}$", ini)) return(tools::toTitleCase(tolower(x)))
    paste0(tools::toTitleCase(tolower(sur)), ", ", paste0(strsplit(ini, "")[[1]], ".", collapse = " ")) }, ""), collapse = " and ")
}
build_bib <- function(st) {
  a <- st$article
  m <- regmatches(a$citation %||% "", regexec("^(.+?) \\((\\d{4})[a-z]?\\)\\. (.+?)\\. ([^.]+?), (\\d+)(?:\\((\\d+)\\))?, ([0-9A-Za-z-]+)\\.?", a$citation %||% "", perl = TRUE))[[1]]
  focal <- if (length(m) >= 8) sprintf("@article{focal,\n  author = {%s},\n  year = {%s},\n  title = {{%s}},\n  journal = {%s},\n  volume = {%s},\n  number = {%s},\n  pages = {%s},\n  doi = {%s}\n}",
      bib_esc(paste(trimws(strsplit(m[2], "(?<=\\.),\\s*(?:&\\s*)?|\\s+&\\s+", perl = TRUE)[[1]]), collapse = " and ")), m[3], bib_esc(a$title %|e|% m[4]), bib_esc(m[5]), m[6], m[7], gsub("-", "--", m[8]), bib_esc(a$doi))
    else sprintf("@misc{focal,\n  title = {{%s}},\n  note = {%s},\n  doi = {%s}\n}", bib_esc(a$title), bib_esc(a$citation), bib_esc(a$doi))
  fixed <- c(focal,
    "@article{aria2017,\n  author = {Aria, Massimo and Cuccurullo, Corrado},\n  year = {2017},\n  title = {bibliometrix: An {R}-tool for comprehensive science mapping analysis},\n  journal = {Journal of Informetrics},\n  volume = {11},\n  number = {4},\n  pages = {959--975},\n  doi = {10.1016/j.joi.2017.08.007}\n}",
    "@misc{kastour2026,\n  author = {Kastour, Amira Mamdouh},\n  year = {2026},\n  title = {{ANCHOR}: A replicable framework for {AI}-assisted article-anchored literature reviews},\n  note = {ANCHOR Studio software}\n}")
  live <- vapply(setdiff(names(REFS), "aria2017"), function(k) ref_bib(k, ref_live(k, st)), "")
  accs <- unlist(lapply(names(PHASES), function(id) st$phases[[id]]$accepted %||% list()), recursive = FALSE)
  accs <- accs[!duplicated(vapply(accs, `[[`, "", "doi"))]
  fixed <- c(fixed, live, vapply(accs, function(r) ref_bib(acc_key(r$doi), r), ""))
  M <- st$biblio$M
  recs <- character(0)
  if (!is.null(M) && nrow(M)) {
    g <- function(f) if (f %in% names(M)) as.character(M[[f]]) else rep(NA_character_, nrow(M))
    AU <- g("AU"); TI <- g("TI"); SO <- g("SO"); PY <- g("PY"); VL <- g("VL"); DI <- g("DI"); DT <- toupper(g("DT")); BP <- g("BP"); EP <- g("EP"); PP <- g("PP")
    first <- tolower(gsub("[^A-Za-z]", "", sub("[ ,;].*$", "", AU))); first[!nzchar(first) | is.na(first)] <- "anon"
    w <- tolower(gsub("[^A-Za-z]", "", vapply(strsplit(ifelse(is.na(TI), "", TI), " "), function(z) { z <- z[nchar(z) > 3]; if (length(z)) z[1] else "" }, "")))
    keys <- make.unique(paste0(first, ifelse(is.na(PY), "", PY), w), sep = "_")
    pages <- ifelse(!is.na(BP) & nzchar(BP), paste0(BP, ifelse(!is.na(EP) & nzchar(EP), paste0("--", EP), "")), ifelse(is.na(PP), "", PP))
    type <- ifelse(grepl("CONFERENCE|PROCEEDINGS", DT), "inproceedings", ifelse(grepl("BOOK CHAPTER", DT), "incollection", ifelse(DT %in% "BOOK", "book", "article")))
    venue <- ifelse(type == "article", "journal", ifelse(type == "book", "publisher", "booktitle"))
    recs <- vapply(seq_len(nrow(M)), function(i) {
      f <- c(author = bib_authors(AU[i]), year = PY[i], title = if (!is.na(TI[i])) paste0("{", bib_esc(sentence_case(TI[i])), "}") else NA,
             v = if (!is.na(SO[i])) bib_esc(tools::toTitleCase(tolower(SO[i]))) else NA, volume = VL[i], pages = pages[i], doi = bib_esc(DI[i]))
      names(f)[names(f) == "v"] <- venue[i]
      f <- f[!is.na(f) & nzchar(f)]
      paste0("@", type[i], "{", keys[i], ",\n", paste(sprintf("  %s = {%s}", names(f), f), collapse = ",\n"), "\n}") }, "")
  }
  paste(c(sprintf("%% %s %s - references.", APP_BRAND, APP_VERSION), "% focal, the stage references and any references you added are cited in the report;",
          sprintf("%% the other %d entries are the bibliometric records (titles converted from capitals to sentence case: check acronyms).", length(recs)),
          "", fixed, "", recs), collapse = "\n")
}

# ------------------------------------------------------------------ LaTeX package (.zip)
build_latex_zip <- function(st, zipfile, app_dir = getwd()) {
  root <- file.path(tempfile("anchor_tex"), paste0("ANCHOR_", safe_name(st$article$title)))
  dir.create(file.path(root, "figures"), recursive = TRUE); dir.create(file.path(root, "fonts"))
  fig_names <- rep(NA_character_, length(st$report_items))
  for (k in seq_along(st$report_items)) { it <- st$report_items[[k]]
    nm <- sprintf("fig%02d_%s", k, safe_name(it$label, 30))
    if (!is.null(it$pdf_raw)) { writeBin(it$pdf_raw, file.path(root, "figures", paste0(nm, ".pdf"))); fig_names[k] <- paste0(nm, ".pdf") }
    else if (!is.null(it$png_raw)) { writeBin(it$png_raw, file.path(root, "figures", paste0(nm, ".png"))); fig_names[k] <- paste0(nm, ".png") }
  }
  writeLines(enc2utf8(main_tex(st)), file.path(root, "main.tex"), useBytes = TRUE)
  writeLines(enc2utf8(build_content_tex(st, fig_names)), file.path(root, "content.tex"), useBytes = TRUE)
  writeLines(enc2utf8(build_bib(st)), file.path(root, "references.bib"), useBytes = TRUE)
  writeLines(c("# XeLaTeX (Overleaf and latexmk read this file)", "$pdf_mode = 5;", "$bibtex_use = 2;"), file.path(root, "latexmkrc"))
  ff <- list.files(file.path(app_dir, "www", "fonts"), "^LiberationSans-.*\\.ttf$|OFL", full.names = TRUE)
  file.copy(ff, file.path(root, "fonts"))
  writeLines(c(sprintf("%s %s - LaTeX package", APP_BRAND, APP_VERSION), "",
    "main.tex         complete document (title, \\input{content}, references)",
    "content.tex      the review itself, with no preamble - put it into any journal template",
    "references.bib   focal article, bibliometrix, ANCHOR, plus every bibliometric record",
    "figures/         report figures (vector PDF where available)",
    "latexmkrc        tells Overleaf and latexmk to use XeLaTeX",
    "fonts/           Liberation Sans (SIL Open Font Licence), used only if Libertinus or Noto Serif is missing", "",
    "OVERLEAF: New Project > Upload Project > choose this .zip. It compiles with XeLaTeX automatically.", "",
    "LOCAL TeX (TeX Live, MiKTeX, MacTeX): latexmk main.tex   (or: xelatex main, bibtex main, xelatex main, xelatex main)", "",
    "JOURNAL TEMPLATE: copy content.tex, figures/ and references.bib next to the template's main file, add",
    "  \\usepackage{graphicx,booktabs,tabularx,float,hyperref}",
    "to its preamble, put \\input{content} where the text goes, and use the template's own \\bibliography command",
    "(add references to the template's .bib, or point it at references.bib). Delete sections you do not need,",
    "for example the prompt log, which most journals take as supplementary material instead.", "",
    "pdfLaTeX also works for Latin text and common Greek letters; for other scripts use XeLaTeX or LuaLaTeX."), file.path(root, "README.txt"))
  old <- setwd(dirname(root)); on.exit(setwd(old))
  zip::zip(zipfile, basename(root), recurse = TRUE, mode = "mirror")
  invisible(zipfile)
}

# ------------------------------------------------------------------ PDF report: browser (Edge/Chrome) or Typst
find_browser <- function() {
  cand <- if (.Platform$OS.type == "windows") c(
      file.path(Sys.getenv("ProgramFiles(x86)"), "Microsoft/Edge/Application/msedge.exe"), file.path(Sys.getenv("ProgramFiles"), "Microsoft/Edge/Application/msedge.exe"),
      file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft/Edge/Application/msedge.exe"), file.path(Sys.getenv("ProgramFiles"), "Google/Chrome/Application/chrome.exe"),
      file.path(Sys.getenv("ProgramFiles(x86)"), "Google/Chrome/Application/chrome.exe"), file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe"))
    else if (Sys.info()[["sysname"]] == "Darwin") c("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
      "/Applications/Chromium.app/Contents/MacOS/Chromium", path.expand("~/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
    else unname(Sys.which(c("google-chrome", "chromium", "chromium-browser", "microsoft-edge")))
  cand <- c(Sys.getenv("ANCHOR_BROWSER"), cand)
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (length(cand)) cand[1] else NULL
}
find_typst <- function(app_dir = getwd()) {
  exe <- if (.Platform$OS.type == "windows") "typst.exe" else "typst"
  cand <- c(Sys.getenv("ANCHOR_TYPST"), file.path(app_dir, "bin", exe), unname(Sys.which("typst")),
            file.path(R.home(), "..", "..", "Library", "bin", exe))   # Windows build: runtime\Library\bin
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (length(cand)) normalizePath(cand[1]) else NULL
}

pdf_via_browser <- function(st, out, browser) {
  d <- tempfile("anchor_pdf"); dir.create(d); html <- file.path(d, "report.html")
  writeLines(enc2utf8(build_report_html(st)), html, useBytes = TRUE)
  url <- paste0("file:///", sub("^/", "", gsub("\\\\", "/", normalizePath(html))))
  args <- c("--headless", "--disable-gpu", "--no-first-run", "--no-default-browser-check", "--disable-extensions",
            paste0("--user-data-dir=", file.path(d, "profile")), "--no-pdf-header-footer", "--print-to-pdf-no-header",
            paste0("--print-to-pdf=", normalizePath(out, mustWork = FALSE)),
            if (.Platform$OS.type == "unix" && identical(Sys.info()[["effective_user"]], "root")) "--no-sandbox", url)
  r <- processx::run(browser, args, timeout = 90, error_on_status = FALSE)
  if (!file.exists(out) || file.size(out) < 1000) stop("the browser did not produce a PDF (", trimws(substr(r$stderr, 1, 200)), ")")
  invisible(out)
}

# --- Markdown -> Typst (through commonmark's XML tree; every text run becomes a Typst string, so drafts cannot inject code)
typ_str <- function(x) paste0("\"", gsub("\n", "\\\\n", gsub("\"", "\\\\\"", gsub("\\\\", "\\\\\\\\", x))), "\"")
typ_txt <- function(x) if (nzchar(x %||% "")) paste0("#", typ_str(x), ";") else ""
typ_md <- function(md) {
  md <- strip_q(md)
  if (!nzchar(trimws(md))) return("#emph[Not completed.]\n")
  doc <- xml2::read_xml(commonmark::markdown_xml(md_shift_headings(md, 2), extensions = TRUE))
  xml2::xml_ns_strip(doc)
  conv <- function(n) {
    kids <- function() paste(vapply(xml2::xml_contents(n), function(k) if (inherits(k, "xml_node") && xml2::xml_type(k) == "element") conv(k) else "", ""), collapse = "")
    switch(xml2::xml_name(n),
      document = kids(),
      paragraph = paste0(kids(), "\n\n"),
      heading = sprintf("#heading(level: %d)[%s]\n\n", as.integer(xml2::xml_attr(n, "level")), kids()),
      text = typ_txt(xml2::xml_text(n)),
      softbreak = " ", linebreak = "#linebreak()",
      emph = sprintf("#emph[%s]", kids()), strong = sprintf("#strong[%s]", kids()), strikethrough = sprintf("#strike[%s]", kids()),
      code = sprintf("#raw(%s);", typ_str(xml2::xml_text(n))),
      code_block = sprintf("#raw(block: true, %s)\n\n", typ_str(xml2::xml_text(n))),
      link = sprintf("#link(%s)[%s]", typ_str(xml2::xml_attr(n, "destination")), kids()),
      list = { items <- vapply(xml2::xml_children(n), function(i) paste0("[", gsub("\n\n$", "", paste(vapply(xml2::xml_children(i), conv, ""), collapse = "")), "]"), "")
        if (identical(xml2::xml_attr(n, "type"), "ordered")) sprintf("#enum(start: %s, %s)\n\n", xml2::xml_attr(n, "start") %|e|% "1", paste(items, collapse = ", "))
        else sprintf("#list(%s)\n\n", paste(items, collapse = ", ")) },
      block_quote = sprintf("#quote(block: true)[%s]\n\n", kids()),
      thematic_break = "#line(length: 100%)\n\n",
      table = { rows <- xml2::xml_children(n); nc <- length(xml2::xml_children(rows[[1]]))
        cells <- unlist(lapply(rows, function(r) vapply(xml2::xml_children(r), function(c) paste0("[", conv_kids(c), "]"), "")))
        sprintf("#table(columns: %d, %s)\n\n", nc, paste(cells, collapse = ", ")) },
      "")
  }
  conv_kids <- function(n) paste(vapply(xml2::xml_children(n), conv, ""), collapse = "")
  conv(xml2::xml_root(doc))
}
typ_table <- function(df, n = 15) {
  df <- head(as.data.frame(df), n); if (!ncol(df) || !nrow(df)) return("")
  num <- vapply(df, is.numeric, TRUE)
  cells <- c(sprintf("[*%s*]", vapply(names(df), typ_txt, "")), apply(df, 1, function(r) sprintf("[%s]", vapply(trimws(as.character(r)), typ_txt, ""))))
  sprintf("#table(columns: (%s), align: (%s), %s)\n\n", paste(ifelse(num, "auto", "1fr"), collapse = ", "),
          paste(ifelse(num, "right", "left"), collapse = ", "), paste(cells, collapse = ", "))
}
build_report_typ <- function(st, figs) {
  a <- st$article; s <- st$setup
  L <- c("#set document(title: ", typ_str(paste("ANCHOR review -", a$title %|e|% "")), ", author: ", typ_str(s$S_name %|e|% "ANCHOR Studio"), ")\n",
    "#set page(paper: \"a4\", margin: (x: 2.2cm, y: 2.4cm), numbering: \"1 / 1\", footer: context [#set text(size: 8pt, fill: rgb(\"#5B6675\")); ", typ_txt(APP_BRAND), " #h(1fr) #counter(page).display(\"1 / 1\", both: true)])\n",
    "#set text(font: (\"Libertinus Serif\", \"Liberation Sans\"), size: 10.5pt, lang: \"en\")\n",
    "#set par(justify: true, leading: 0.62em)\n",
    "#show heading: set text(font: \"Liberation Sans\", fill: rgb(\"#14213D\"))\n",
    "#show heading.where(level: 1): it => { v(1em); it; v(-0.4em); line(length: 100%, stroke: 1.2pt + rgb(\"#14213D\")); v(0.3em) }\n",
    "#show link: set text(fill: rgb(\"#B4561A\"))\n",
    "#set table(stroke: (x, y) => (bottom: 0.5pt + rgb(\"#D5D9DF\")), fill: (x, y) => if y == 0 { rgb(\"#ECE8DE\") }, inset: 5pt)\n",
    "#show table: set text(size: 9pt)\n",
    "#show figure: set block(breakable: false)\n\n",
    "#text(font: \"Liberation Sans\", size: 8.5pt, fill: rgb(\"#B4561A\"), tracking: 0.08em, weight: \"bold\")[ANCHOR REVIEW REPORT]\n\n",
    sprintf("#text(size: 20pt, weight: \"bold\", fill: rgb(\"#14213D\"))[%s]\n\n", typ_txt(a$title %|e|% "Untitled review")),
    sprintf("%s%s #linebreak() %s\n\n", typ_txt(s$S_name), if (nzchar(s$S_inst %||% "")) typ_txt(paste0(" \u00b7 ", s$S_inst)) else "",
            typ_txt(sprintf("Generated %s with %s %s", format(Sys.time(), "%d %B %Y %H:%M"), APP_BRAND, APP_VERSION))),
    sprintf("*Focal article:* %s\n\n", typ_txt(a$citation)),
    sprintf("#text(size: 9pt, fill: rgb(\"#5B6675\"))[%s]\n\n", typ_txt(online_note(st))),
    "= Progress and decisions\n\n",
    typ_table(data.frame(Phase = vapply(PHASES, function(p) paste(p$id, "\u00b7", p$name), ""), Status = vapply(PHASES, function(p) st$phases[[p$id]]$status %||% "", ""),
                         Approved = vapply(PHASES, function(p) st$phases[[p$id]]$approved_at %|e|% "-", ""))))
  for (p in PHASES) {
    ph <- st$phases[[p$id]]
    L <- c(L, sprintf("= %s\n\n", typ_txt(paste(p$id, "\u00b7", p$name))), sprintf("#emph[%s]\n\n", typ_txt(p$question)), typ_md(ph$draft))
    if (length(p$questions)) L <- c(L, "#heading(level: 3)[Researcher's decisions]\n\n",
      typ_table(data.frame(Question = vapply(p$questions, `[[`, "", "label"), Answer = vapply(p$questions, function(q) paste(ph$answers[[q$id]] %||% "-", collapse = "; "), "")), 50))
    if (nzchar(ph$notes %||% "")) L <- c(L, "#heading(level: 3)[Notes]\n\n", typ_txt(ph$notes), "\n\n")
    if (p$id == "N" && !is.null(f <- ds_findings(st))) L <- c(L, sprintf("*%s*\n\n", typ_txt(f$text)), if (!is.null(f$table)) typ_table(f$table, 200))
    L <- c(L, lessons_typ(p$id, st))
    if (p$id == "N" && !is.null(st$prisma)) { pr <- st$prisma
      L <- c(L, "#heading(level: 3)[PRISMA 2020 flow]\n\n", typ_table(data.frame(Stage = c("Records identified from databases", "Duplicates removed",
        "Removed by filters (year, type, language)", "Records screened (title/abstract)", "Excluded at screening", "Reports assessed for eligibility",
        "Reports excluded (with reasons)", "Studies included in the review"),
        n = vapply(list(pr$identified, pr$duplicates, pr$filtered, pr$screened, pr$excluded_screen, pr$assessed, pr$excluded_full, pr$included),
                   function(v) as.character(v %|e|% "pending"), "")))) }
    if (p$id == "O" && !is.null(pv <- provenance_df(st$analysis$datasets))) L <- c(L, "#heading(level: 3)[Data and provenance]\n\n", typ_table(pv[, c("Dataset", "Source", "Query", "Retrieved", "Rows", "Licensed")], 100))
    if (p$id == "A" && !is.null(fr <- find_report(st))) L <- c(L, "#heading(level: 3)[How the focal article was chosen]\n\n", sprintf("%s\n\n", typ_txt(fr$text)), typ_table(fr$table, 10), sprintf("%s\n\n", typ_txt(fr$note)))
    if (p$id == "A" && !is.null(f <- st$focal_oa)) L <- c(L, "#heading(level: 3)[Focal article in OpenAlex]\n\n", sprintf("%s\n\n", typ_txt(sprintf("%d references, cited by %s works (checked %s). %s", f$n_refs %||% 0L, f$n_citing %||% "?", f$retrieved %||% "",
        if (!is.null(f$open)) sprintf("Open copy: %s (%s).", f$open$url, f$open$version) else "No open copy found; read it through your library."))), if (!is.null(f$journal)) typ_table(f$journal, 5))
    if (p$id == "R") L <- c(L, "#heading(level: 3)[Data availability statement]\n\n", sprintf("%s\n\n", typ_txt(data_availability(st))))
    if (p$id %in% c("C", "H", "O") && length(items_for(st, p$id))) {
      L <- c(L, switch(p$id, C = "#heading(level: 3)[Bibliometric figures and tables]\n\n", H = "#heading(level: 3)[Research-front trend]\n\n", "#heading(level: 3)[Analysis results]\n\n"))
      for (k in seq_along(st$report_items)) { it <- st$report_items[[k]]; if (!identical(it$phase %||% "C", p$id)) next
        cap <- typ_txt(paste0(it$label, ".", if (nzchar(it$note %||% "")) paste0(" ", it$note) else ""))
        if (!is.na(figs[k])) L <- c(L, sprintf("#figure(image(%s, width: 100%%), caption: [%s])\n\n", typ_str(figs[k]), cap))
        else L <- c(L, sprintf("*%s*\n\n", cap))
        if (!is.null(it$table)) L <- c(L, typ_table(it$table, 15))
        if (!is.null(it$code)) L <- c(L, sprintf("#block(fill: rgb(\"#F4F6F5\"), inset: 8pt, width: 100%%)[#set text(size: 8pt); #raw(block: true, lang: \"r\", %s)]\n\n", typ_str(it$code)))
      }
    }
  }
  pt <- prereq_tables(st)
  L <- c(L, "= Appendix: researcher, resources and licences\n\n", "#text(size: 9pt)[Recorded on the Before you start page. Licence summaries are short and not legal advice; follow each provider's terms.]\n\n",
    unlist(Map(function(nm, df) c(sprintf("#heading(level: 2)[%s]\n\n", typ_txt(nm)), typ_table(df, 200)), names(pt), pt)))
  L <- c(L, "= Decision log\n\n",
    if (nrow(st$decisions)) typ_table(st$decisions[, c("time", "phase", "decision", "note")], 500) else "No decisions yet.\n\n",
    "= AI-use disclosure\n\n", sprintf("#block(fill: rgb(\"#F4F6F5\"), inset: 10pt, width: 100%%)[#set text(size: 9pt); #raw(block: true, %s)]\n\n", typ_str(disclosure_text(st))),
    "= Prompt log\n\n",
    if (nrow(st$prompts)) paste(vapply(seq_len(nrow(st$prompts)), function(i) sprintf("#heading(level: 3)[%s]\n\n#block(fill: rgb(\"#F4F6F5\"), inset: 8pt, width: 100%%)[#set text(size: 8pt); #raw(block: true, %s)]\n\n",
      typ_txt(paste(st$prompts$time[i], "\u00b7 phase", st$prompts$phase[i], "\u00b7", st$prompts$mode[i])), typ_str(substr(st$prompts$prompt[i], 1, 4000))), ""), collapse = "")
    else "No prompts were used.\n",
    sprintf("\n#v(2em) #line(length: 100%%, stroke: 0.5pt + rgb(\"#D5D9DF\")) #text(size: 9pt, fill: rgb(\"#5B6675\"))[%s]\n", typ_txt(paste0("Generated with ", APP_BRAND, " ", APP_VERSION, " \u00b7 ", APP_TAGLINE))))
  paste(L, collapse = "")
}
pdf_via_typst <- function(st, out, typst, app_dir = getwd()) {
  d <- tempfile("anchor_typ"); dir.create(d)
  figs <- rep(NA_character_, length(st$report_items))
  for (k in seq_along(st$report_items)) { it <- st$report_items[[k]]
    if (!is.null(it$png_raw)) { figs[k] <- sprintf("fig%02d.png", k); writeBin(it$png_raw, file.path(d, figs[k])) } }
  writeLines(enc2utf8(build_report_typ(st, figs)), file.path(d, "report.typ"), useBytes = TRUE)
  r <- processx::run(typst, c("compile", "--font-path", normalizePath(file.path(app_dir, "www", "fonts")), file.path(d, "report.typ"), normalizePath(out, mustWork = FALSE)),
                     timeout = 120, error_on_status = FALSE, wd = d)
  if (r$status != 0 || !file.exists(out)) stop("Typst could not build the PDF: ", trimws(substr(paste(r$stderr, r$stdout), 1, 400)))
  invisible(out)
}

# engine: "auto" (browser if found, else Typst), "browser" or "typst". Returns the engine used.
build_report_pdf <- function(st, out, engine = "auto", app_dir = getwd()) {
  br <- if (engine %in% c("auto", "browser")) find_browser() else NULL
  if (!is.null(br)) {
    ok <- tryCatch({ pdf_via_browser(st, out, br); TRUE }, error = function(e) if (engine == "browser") stop(e) else FALSE)
    if (ok) return("browser")
  } else if (engine == "browser") stop("No Microsoft Edge or Google Chrome was found. Choose 'Typst' as the PDF engine in Settings.")
  ty <- find_typst(app_dir)
  if (is.null(ty)) stop("No PDF engine was found: install Microsoft Edge or Google Chrome, or use a build of ANCHOR Studio that includes Typst.")
  pdf_via_typst(st, out, ty, app_dir); "typst"
}

# ANCHOR Studio - report, disclosure and project files

APP_VERSION <- "1.7.0"
# endorsement branding: the product keeps its name; the author's brand rides with it
APP_NAME <- "ANCHOR Studio"
APP_BYLINE <- "by A. Mamdouh Kastour"
APP_BRAND <- paste(APP_NAME, APP_BYLINE)
APP_TAGLINE <- "A. Mamdouh Kastour\u2019s Scholarly Anchor"

disclosure_text <- function(st) {
  s <- st$setup; mode <- st$settings$mode
  name <- if (nzchar(trimws(s$S_name %||% ""))) trimws(s$S_name) else if (nzchar(trimws(st$prereq$name %||% ""))) trimws(st$prereq$name) else "[your name]"
  # name every tool that was actually used, from the prompt log (not from the current setting)
  tool_name <- function(m) switch(m,
    api = sprintf("Claude (%s), Anthropic, accessed through the Anthropic API", st$settings$api_model %||% "model not recorded"),
    prompt = "Claude (claude.ai), Anthropic, used through copy-and-paste prompts",
    ollama = sprintf("a local language model (%s) served by Ollama", st$settings$ollama_model %||% "model not recorded"),
    NULL)
  modes <- unique(as.character(st$prompts$mode %||% character(0)))
  tool <- unlist(lapply(modes, tool_name))
  tool <- if (length(tool)) paste(tool, collapse = "; and ") else NULL
  used_ai <- nrow(st$prompts) > 0 && !is.null(tool)
  d <- if (used_ai) suppressWarnings(as.Date(substr(as.character(st$prompts$time), 1, 10))) else as.Date(NA)
  d <- d[!is.na(d)]; dates <- if (length(d)) range(d) else rep(Sys.Date(), 2)
  when <- if (dates[1] == dates[2]) paste("on", format(dates[1], "%d %B %Y")) else paste("between", format(dates[1], "%d %B %Y"), "and", format(dates[2], "%d %B %Y"))
  lines <- c(
    "AI-USE AND METHODS DISCLOSURE",
    "",
    sprintf("%s applied the ANCHOR framework (Kastour, 2026) to %s using ANCHOR Studio %s.", name,
            sub("\\.\\s*$", "", st$article$citation %||% "[focal article]"), APP_VERSION),
    sprintf("Bibliometric analyses were run with the bibliometrix R package (Aria & Cuccurullo, 2017)%s.",
            if (!is.null(st$biblio$M)) sprintf(" on %d records", nrow(st$biblio$M)) else ""),
    if (!is.null(tool) && used_ai && all(st$prompts$phase == "Find")) sprintf("Generative AI (%s) was used %s only to summarise the abstracts of candidate articles while the focal article was chosen; the choice was the author's, and no text of the review was drafted with AI.", tool, when) else
    if (!is.null(tool) && used_ai) sprintf("Generative AI (%s) was used %s to draft parts of phases %s. Every draft was reviewed, edited and approved by the author at a decision gate; all figures were checked against the primary source.",
            tool, when, paste(setdiff(unique(st$prompts$phase), "Find"), collapse = ", ")) else "No generative AI was used to draft the text.",
    if (!is.null(tool) && used_ai && any(st$prompts$phase == "Find") && !all(st$prompts$phase == "Find")) "AI also summarised the abstracts of candidate articles while the focal article was chosen; the choice was the author's." else NULL,
    sprintf("%s takes full responsibility for the content.", name),
    "",
    "References",
    "Aria, M., & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive science mapping analysis. Journal of Informetrics, 11(4), 959-975. https://doi.org/10.1016/j.joi.2017.08.007",
    "Kastour, A. M. (2026). ANCHOR: A replicable framework for AI-assisted article-anchored literature reviews. Developed with Claude (Opus 5.5), Anthropic.",
    if (used_ai && any(modes %in% c("api", "prompt"))) sprintf("Anthropic. (%s). Claude [Large language model]. https://claude.ai", format(Sys.Date(), "%Y")) else NULL,
    if (nzchar(st$article$citation %||% "")) st$article$citation else NULL
  )
  paste(lines[!is.null(lines)], collapse = "\n")
}

md_html <- function(x) if (nzchar(x %||% "")) sanitize_html(as.character(shiny::markdown(x))) else "<p><i>Not completed.</i></p>"
# drafts can come from an AI model: remove anything that could run code when the report is opened
sanitize_html <- function(h) {
  h <- gsub("(?is)<(script|style|iframe|object|embed|form|textarea)\\b[^>]*>.*?</\\1\\s*>", "", h, perl = TRUE)
  h <- gsub("(?i)</?(script|style|iframe|object|embed|form|textarea|link|meta|base)\\b[^>]*>", "", h, perl = TRUE)
  h <- gsub("(?i)\\s+on[a-z]+\\s*=\\s*(\"[^\"]*\"|'[^']*'|[^\\s>]+)", "", h, perl = TRUE)
  gsub("(?i)(href|src)\\s*=\\s*([\"']?)\\s*(javascript|vbscript|data):", "\\1=\\2#blocked:", h, perl = TRUE)
}
# the questions block is reported separately with the answers
strip_q <- function(x) sub("(?is)\\s*QUESTIONS FOR THE RESEARCHER:.*$", "", x %||% "", perl = TRUE)
esc <- function(x) htmltools::htmlEscape(x %||% "")

png_data_uri <- function(path) paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size)))

answers_html <- function(answers, qs) {
  rows <- vapply(qs, function(q) sprintf("<tr><td>%s</td><td>%s</td></tr>", esc(q$label),
                 esc(paste(answers[[q$id]] %||% "-", collapse = "; "))), "")
  paste0("<table class='qa'>", paste(rows, collapse = ""), "</table>")
}

build_report_html <- function(st) {
  css <- "body{font-family:'IBM Plex Sans',Arial,sans-serif;color:#14213D;max-width:900px;margin:40px auto;padding:0 20px;line-height:1.55}
  h1,h2,h3{font-family:'Source Serif 4',Georgia,serif}h1{font-size:30px;margin-bottom:4px}h2{border-bottom:2px solid #14213D;padding-bottom:4px;margin-top:40px}
  .eye{color:#B4561A;text-transform:uppercase;letter-spacing:.08em;font-size:12px;font-weight:600}table{border-collapse:collapse;width:100%;font-size:14px;margin:10px 0}
  td,th{border-bottom:1px solid #D5D9DF;padding:5px 8px;text-align:left;vertical-align:top}th{background:#ECE8DE}.qa td:first-child{width:45%;color:#3D4A5C}
  .status{display:inline-block;padding:2px 10px;border-radius:99px;background:#E2F1E7;color:#2F7A4B;font-size:12px;font-weight:600}
  .pending{background:#F6ECDC;color:#965E0F}img{max-width:100%;border:1px solid #D5D9DF}pre{white-space:pre-wrap;background:#F4F6F5;padding:12px;font-size:13px}
  .fig{margin:18px 0}.cap{font-size:13px;color:#5B6675}.lessons{background:#F6F3EA;border-left:4px solid #B4561A;padding:4px 16px 8px;margin:18px 0}.refs{font-size:13px}
  @page{size:A4;margin:18mm 16mm 20mm;@bottom-right{content:counter(page) ' / ' counter(pages);font:9px Arial,sans-serif;color:#5B6675}@bottom-left{content:'ANCHOR Studio by A. Mamdouh Kastour';font:9px Arial,sans-serif;color:#5B6675}}
  .brandfoot{margin-top:40px;padding-top:10px;border-top:1px solid #D5D9DF;font-size:12px;color:#5B6675}
  @media print{body{margin:0 auto;max-width:none;padding:0}h1,h2,h3{break-after:avoid}.fig,img,tr,pre,.qa{break-inside:avoid}h2{margin-top:28px}}"
  s <- st$setup; a <- st$article
  parts <- c(sprintf("<!doctype html><html lang='en'><head><meta charset='utf-8'><title>ANCHOR review - %s</title><style>%s</style></head><body>", esc(a$title), css),
    "<p class='eye'>ANCHOR review report</p>",
    sprintf("<h1>%s</h1>", esc(a$title %||% "Untitled review")),
    sprintf("<p>%s%s<br>Generated %s with %s</p>", esc(s$S_name %||% ""), if (nzchar(s$S_inst %||% "")) paste0(" \u00b7 ", esc(s$S_inst)) else "",
            format(Sys.time(), "%d %B %Y %H:%M"), esc(paste(APP_BRAND, APP_VERSION))),
    sprintf("<p><b>Focal article:</b> %s</p>", esc(a$citation)),
    if (isTRUE(st$online$focal$found) && identical(st$online$focal$doi, norm_doi(a$doi)) && st$online$focal$status != "ok")
      sprintf("<p style='color:#B3261E'><b>Warning:</b> the focal article has a published %s (checked %s).</p>", esc(NOTICE_NAME[[st$online$focal$status]]), esc(st$online$focal$checked_at)) else "",
    sprintf("<p class='cap'>%s</p>", esc(online_note(st))),
    "<h2>Progress and decisions</h2><table><tr><th>Phase</th><th>Status</th><th>Approved</th></tr>",
    vapply(PHASES, function(p) { ph <- st$phases[[p$id]]
      sprintf("<tr><td>%s \u00b7 %s</td><td><span class='status %s'>%s</span></td><td>%s</td></tr>", p$id, esc(p$name),
              if (ph$status == "approved") "" else "pending", esc(ph$status), esc(ph$approved_at %||% "-")) }, ""),
    "</table>")
  for (p in PHASES) {
    ph <- st$phases[[p$id]]
    parts <- c(parts, sprintf("<h2>%s \u00b7 %s</h2><p><i>%s</i></p>", p$id, esc(p$name), esc(p$question)),
               md_html(strip_q(ph$draft)),
               if (length(p$questions)) paste0("<h3>Researcher's decisions</h3>", answers_html(ph$answers, p$questions)) else "",
               if (length(ph$ai_q)) paste0("<h3>Questions raised and answered</h3><table>", paste(sprintf("<tr><td>%s</td><td>%s</td></tr>", esc(ph$ai_q), esc(unlist(ph$ai_a[seq_along(ph$ai_q)]) %||% "")), collapse = ""), "</table>") else "",
               if (nzchar(ph$notes %||% "")) paste0("<h3>Notes</h3><p>", esc(ph$notes), "</p>") else "")
    if (p$id == "N" && !is.null(st$prisma)) parts <- c(parts, prisma_html(st$prisma))
    if (p$id == "N" && !is.null(f <- ds_findings(st))) parts <- c(parts, sprintf("<p><b>%s</b></p>", esc(f$text)),
      if (!is.null(f$table)) paste0("<table><tr><th>Record</th><th>DOI</th><th>Notice</th></tr>", paste(sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>", esc(f$table$Record), esc(f$table$DOI), esc(f$table$Notice)), collapse = ""), "</table>") else "")
    if (p$id == "O" && !is.null(pv <- provenance_df(st$analysis$datasets))) parts <- c(parts, "<h3>Data and provenance</h3>",
      paste0("<table><tr>", paste0("<th>", esc(names(pv)), "</th>", collapse = ""), "</tr>", paste(apply(pv, 1, function(r) paste0("<tr>", paste0("<td>", esc(as.character(r)), "</td>", collapse = ""), "</tr>")), collapse = ""), "</table>"))
    if (p$id == "A") parts <- c(parts, find_html(st), focal_oa_html(st))
    if (p$id == "R") parts <- c(parts, "<h3>Data availability statement</h3>", sprintf("<p>%s</p>", esc(data_availability(st))))
    if (p$id %in% c("C", "H", "O") && length(items_for(st, p$id))) {
      parts <- c(parts, switch(p$id, C = "<h3>Bibliometric figures and tables</h3>", H = "<h3>Research-front trend</h3>", "<h3>Analysis results</h3>"))
      for (it in items_for(st, p$id)) {
        parts <- c(parts, "<div class='fig'>",
          if (!is.null(it$png) && file.exists(it$png)) sprintf("<img src='%s' alt='%s'>", png_data_uri(it$png), esc(it$label)) else "",
          sprintf("<p class='cap'><b>%s.</b> %s</p>", esc(it$label), esc(it$note %||% "")),
          if (!is.null(it$table)) paste0("<table>", "<tr>", paste0("<th>", esc(names(it$table)), "</th>", collapse = ""), "</tr>",
            paste(apply(head(it$table, 15), 1, function(r) paste0("<tr>", paste0("<td>", esc(as.character(r)), "</td>", collapse = ""), "</tr>")), collapse = ""), "</table>") else "",
          if (!is.null(it$code)) sprintf("<details><summary class='cap'>R code</summary><pre>%s</pre></details>", esc(it$code)) else "",
          "</div>")
      }
    }
    parts <- c(parts, lessons_html(p$id, st))
  }
  parts <- c(parts, prereq_html(st), "<h2>Decision log</h2>",
    if (nrow(st$decisions)) paste0("<table><tr><th>Time</th><th>Phase</th><th>Decision</th><th>Note</th></tr>",
      paste(sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>", esc(st$decisions$time), esc(st$decisions$phase), esc(st$decisions$decision), esc(st$decisions$note)), collapse = ""), "</table>") else "<p>No decisions yet.</p>",
    "<h2>AI-use disclosure</h2>", sprintf("<pre>%s</pre>", esc(disclosure_text(st))),
    "<h2>Prompt log</h2>",
    if (nrow(st$prompts)) paste(sprintf("<h3>%s \u00b7 phase %s \u00b7 %s</h3><pre>%s</pre>", esc(st$prompts$time), esc(st$prompts$phase), esc(st$prompts$mode), esc(substr(st$prompts$prompt, 1, 4000))), collapse = "") else "<p>No prompts were used.</p>",
    sprintf("<p class='brandfoot'>Generated with %s %s &middot; %s</p>", esc(APP_BRAND), APP_VERSION, esc(APP_TAGLINE)),
    "</body></html>")
  paste(parts, collapse = "\n")
}

# the PRISMA 2020 stages as one table (HTML, LaTeX and PDF reports use the same rows); the retracted-records row
# appears only when retracted records were excluded
prisma_rows <- function(pr) {
  v <- function(x) { x <- x %||% NULL; if (is.null(x) || (length(x) == 1 && is.na(x))) "pending" else as.character(x) }
  st <- c("Records identified from databases", "Duplicates removed", if (isTRUE((pr$retracted %||% 0) > 0)) "Retracted records removed (retraction check)",
          "Records removed by filters (year, type, language)", "Records screened (title/abstract)", "Records excluded at screening",
          "Reports assessed for eligibility", "Reports excluded (with reasons)", "Studies included in the review")
  n <- c(v(pr$identified), v(pr$duplicates), if (isTRUE((pr$retracted %||% 0) > 0)) v(pr$retracted), v(pr$filtered), v(pr$screened),
         v(pr$excluded_screen), v(pr$assessed), v(pr$excluded_full), v(pr$included))
  data.frame(Stage = st, n = n, stringsAsFactors = FALSE)
}
prisma_html <- function(pr) {
  box <- function(t, n) sprintf("<div style='border:1px solid #14213D;border-radius:6px;padding:8px 10px;margin:6px 0'><b>%s</b><br>n = %s</div>", t, n)
  r <- prisma_rows(pr)
  paste0("<h3>PRISMA 2020 flow</h3><div style='max-width:560px'>", paste(mapply(box, r$Stage, r$n), collapse = ""), "</div>")
}

build_report_docx <- function(st, path) {
  doc <- officer::read_docx()
  add <- function(d, txt, style = "Normal") officer::body_add_par(d, txt, style = style)
  doc <- add(doc, "ANCHOR review report", "heading 1")
  doc <- add(doc, st$article$title %||% "")
  doc <- add(doc, paste0(st$setup$S_name %||% "", " \u00b7 generated ", format(Sys.Date(), "%d %B %Y"), " with ", APP_BRAND, " ", APP_VERSION))
  doc <- add(doc, paste("Focal article:", st$article$citation %||% ""))
  doc <- add(doc, online_note(st))
  for (p in PHASES) {
    ph <- st$phases[[p$id]]
    doc <- add(doc, paste0(p$id, " \u00b7 ", p$name, " (", ph$status, ")"), "heading 1")
    for (ln in strsplit(strip_q(ph$draft %||% "Not completed."), "\n")[[1]]) {
      if (grepl("^#{1,3} ", ln)) doc <- add(doc, sub("^#+ ", "", ln), "heading 2")
      else if (nzchar(trimws(ln))) doc <- add(doc, gsub("\\*\\*", "", ln))
    }
    if (length(p$questions)) { doc <- add(doc, "Researcher's decisions", "heading 2")
      for (q in p$questions) doc <- add(doc, paste0(q$label, ": ", paste(ph$answers[[q$id]] %||% "-", collapse = "; "))) }
    if (p$id == "O" && !is.null(pv <- provenance_df(st$analysis$datasets))) { doc <- add(doc, "Data and provenance", "heading 2"); doc <- officer::body_add_table(doc, pv, style = "table_template") }
    if (p$id == "A" && !is.null(fr <- find_report(st))) { doc <- add(doc, "How the focal article was chosen", "heading 2"); doc <- add(doc, fr$text)
      doc <- officer::body_add_table(doc, fr$table, style = "table_template"); doc <- add(doc, fr$note) }
    if (p$id == "A" && !is.null(f <- st$focal_oa)) { doc <- add(doc, "Focal article in OpenAlex", "heading 2")
      doc <- add(doc, sprintf("%d references, cited by %s works (checked %s). %s", f$n_refs %||% 0L, f$n_citing %||% "?", f$retrieved %||% "",
        if (!is.null(f$open)) sprintf("Open copy: %s (%s).", f$open$url, f$open$version) else "No open copy found; read it through your library."))
      if (!is.null(f$journal)) doc <- officer::body_add_table(doc, f$journal, style = "table_template") }
    if (p$id == "R") { doc <- add(doc, "Data availability statement", "heading 2"); doc <- add(doc, data_availability(st)) }
    if (p$id %in% c("C", "H", "O")) for (it in items_for(st, p$id)) {
      doc <- add(doc, it$label, "heading 2")
      if (!is.null(it$png) && file.exists(it$png)) doc <- officer::body_add_img(doc, it$png, width = 6.3, height = 3.9)
      if (!is.null(it$table)) doc <- officer::body_add_table(doc, head(as.data.frame(it$table), 15), style = "table_template")
      if (!is.null(it$code)) { doc <- add(doc, "R code"); for (ln in strsplit(it$code, "\n")[[1]]) doc <- add(doc, ln) }
    }
    if (p$id == "N" && !is.null(f <- ds_findings(st))) { doc <- add(doc, f$text); if (!is.null(f$table)) doc <- officer::body_add_table(doc, f$table, style = "table_template") }
    doc <- lessons_docx(doc, p$id, st)
  }
  doc <- add(doc, "Appendix: researcher, resources and licences", "heading 1")
  doc <- add(doc, "Recorded on the Before you start page. Licence summaries are short and not legal advice; follow each provider's terms.")
  pt <- prereq_tables(st); for (nm in names(pt)) { doc <- add(doc, nm, "heading 2"); doc <- officer::body_add_table(doc, pt[[nm]], style = "table_template") }
  doc <- add(doc, "AI-use disclosure", "heading 1")
  for (ln in strsplit(disclosure_text(st), "\n")[[1]]) if (nzchar(ln)) doc <- add(doc, ln)
  doc <- add(doc, "")
  doc <- add(doc, paste0("Generated with ", APP_BRAND, " ", APP_VERSION, " \u00b7 ", APP_TAGLINE))
  print(doc, target = path)
}

user_dir <- function() { d <- tools::R_user_dir("ANCHORStudio", "data"); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }

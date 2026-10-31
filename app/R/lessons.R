# ANCHOR Studio - "Lessons learned & what's in it for me" for each stage, with built-in references.
# References checked by hand on 25 October 2026; the app re-checks them live (Crossref) when online.

LESSONS_ASOF <- "2026-10-25"

REFS <- list(
  aria2017 = list(au = "Aria, M., & Cuccurullo, C.", year = 2017, title = "bibliometrix: An R-tool for comprehensive science mapping analysis",
                  journal = "Journal of Informetrics", vol = "11", iss = "4", pages = "959-975", doi = "10.1016/j.joi.2017.08.007"),
  breslin2023 = list(au = "Breslin, D., & Gatrell, C.", year = 2023, title = "Theorizing through literature reviews: The miner-prospector continuum",
                     journal = "Organizational Research Methods", vol = "26", iss = "1", pages = "139-167", doi = "10.1177/1094428120943288"),
  donthu2021 = list(au = "Donthu, N., Kumar, S., Mukherjee, D., Pandey, N., & Lim, W. M.", year = 2021, title = "How to conduct a bibliometric analysis: An overview and guidelines",
                    journal = "Journal of Business Research", vol = "133", iss = "", pages = "285-296", doi = "10.1016/j.jbusres.2021.04.070"),
  flemyng2025 = list(au = "Flemyng, E., Noel-Storr, A., Macura, B., Gartlehner, G., Thomas, J., Meerpohl, J. J., ... Grainger, M.", year = 2025,
                     title = "Position statement on artificial intelligence (AI) use in evidence synthesis across Cochrane, the Campbell Collaboration, JBI and the Collaboration for Environmental Evidence 2025",
                     journal = "Cochrane Database of Systematic Reviews", vol = "", iss = "11", pages = "ED000178", doi = "10.1002/14651858.ED000178"),
  ganjavi2024 = list(au = "Ganjavi, C., Eppler, M. B., Pekcan, A., Biedermann, B., Abreu, A., Collins, G. S., Gill, I. S., & Cacciamani, G. E.", year = 2024,
                     title = "Publishers' and journals' instructions to authors on use of generative artificial intelligence in academic and scientific publishing: Bibliometric analysis",
                     journal = "BMJ", vol = "384", iss = "", pages = "e077192", doi = "10.1136/bmj-2023-077192"),
  khraisha2024 = list(au = "Khraisha, Q., Put, S., Kappenberg, J., Warraitch, A., & Hadfield, K.", year = 2024,
                      title = "Can large language models replace humans in systematic reviews? Evaluating GPT-4's efficacy in screening and extracting data from peer-reviewed and grey literature in multiple languages",
                      journal = "Research Synthesis Methods", vol = "", iss = "", pages = "", doi = "10.1002/jrsm.1715"),
  kraus2022 = list(au = "Kraus, S., Breier, M., Lim, W. M., Dabic, M., Kumar, S., Kanbach, D., ... Ferreira, J. J.", year = 2022,
                   title = "Literature reviews as independent studies: Guidelines for academic practice", journal = "Review of Managerial Science",
                   vol = "16", iss = "8", pages = "2577-2595", doi = "10.1007/s11846-022-00588-8"),
  kunisch2023 = list(au = "Kunisch, S., Denyer, D., Bartunek, J. M., Menz, M., & Cardinal, L. B.", year = 2023, title = "Review research as scientific inquiry",
                     journal = "Organizational Research Methods", vol = "26", iss = "1", pages = "3-45", doi = "10.1177/10944281221127292"),
  limkumar2024 = list(au = "Lim, W. M., & Kumar, S.", year = 2024, title = "Guidelines for interpreting the results of bibliometric analysis: A sensemaking approach",
                      journal = "Global Business and Organizational Excellence", vol = "43", iss = "2", pages = "17-26", doi = "10.1002/joe.22229"),
  lim2022 = list(au = "Lim, W. M., Kumar, S., & Ali, F.", year = 2022, title = "Advancing knowledge through literature reviews: 'What', 'why', and 'how to contribute'",
                 journal = "The Service Industries Journal", vol = "42", iss = "7-8", pages = "481-513", doi = "10.1080/02642069.2022.2047941"),
  ozturk2024 = list(au = "\u00d6zt\u00fcrk, O., Kocaman, R., & Kanbach, D. K.", year = 2024, title = "How to design bibliometric research: An overview and a framework proposal",
                    journal = "Review of Managerial Science", vol = "18", iss = "11", pages = "3333-3361", doi = "10.1007/s11846-024-00738-0"),
  page2021 = list(au = "Page, M. J., McKenzie, J. E., Bossuyt, P. M., Boutron, I., Hoffmann, T. C., Mulrow, C. D., ... Moher, D.", year = 2021,
                  title = "The PRISMA 2020 statement: An updated guideline for reporting systematic reviews", journal = "BMJ", vol = "372", iss = "", pages = "n71", doi = "10.1136/bmj.n71"),
  paul2021 = list(au = "Paul, J., Lim, W. M., O'Cass, A., Hao, A. W., & Bresciani, S.", year = 2021, title = "Scientific procedures and rationales for systematic literature reviews (SPAR-4-SLR)",
                  journal = "International Journal of Consumer Studies", vol = "45", iss = "4", pages = "O1-O16", doi = "10.1111/ijcs.12695"),
  post2020 = list(au = "Post, C., Sarala, R., Gatrell, C., & Prescott, J. E.", year = 2020, title = "Advancing theory with review articles",
                  journal = "Journal of Management Studies", vol = "57", iss = "2", pages = "351-376", doi = "10.1111/joms.12549"),
  rethlefsen2021 = list(au = "Rethlefsen, M. L., Kirtley, S., Waffenschmidt, S., Ayala, A. P., Moher, D., Page, M. J., Koffel, J. B., & PRISMA-S Group", year = 2021,
                        title = "PRISMA-S: An extension to the PRISMA Statement for Reporting Literature Searches in Systematic Reviews", journal = "Systematic Reviews",
                        vol = "10", iss = "", pages = "39", doi = "10.1186/s13643-020-01542-z")
)

LESSONS <- list(
  A = list(lessons = c("Confirm you hold the version of record before extracting any numbers.",
                       "Check every figure in the draft against the article's own tables and page numbers.",
                       "Write the focal claim as one sentence before reading further."),
           wiifm = "A precise, defensible starting point that a supervisor or reviewer can trace to page and table.",
           refs = c("kraus2022", "lim2022"),
           query = "literature review guidelines contribution"),
  N = list(lessons = c("Record every database, date and search string as you go, not afterwards.",
                       "Export complete records (cited references, affiliations); some exports, such as Lens IDs, lack them.",
                       "Fill the PRISMA counts at each screening step."),
           wiifm = "A search anyone can repeat, and a PRISMA flow diagram that writes itself.",
           refs = c("page2021", "rethlefsen2021", "paul2021"),
           query = "systematic review search reporting PRISMA"),
  C = list(lessons = c("Match each bibliometric technique to a question before running it.",
                       "Treat clusters as prompts for reading, not as findings.",
                       "Aim for 50-100+ records before trusting science maps."),
           wiifm = "Figures you can explain in your methods section, not just show.",
           refs = c("donthu2021", "ozturk2024", "limkumar2024", "aria2017"),
           query = "bibliometric analysis guidelines science mapping"),
  H = list(lessons = c("Distinguish gaps you mine from the literature from gaps you prospect beyond it.",
                       "Tie each hypothesis to a theory, not only to an absence of studies."),
           wiifm = "Hypotheses that read as a theoretical contribution, which journals reward.",
           refs = c("breslin2023", "post2020"),
           query = "theorizing literature review research gaps"),
  O = list(lessons = c("Write the design as a review protocol: unit, sample, period, variables and identification.",
                       "Check that each variable exists in a source you can access before committing."),
           wiifm = "A research agenda you can start next month, not a wish list.",
           refs = c("kunisch2023", "paul2021"),
           query = "review research design research agenda methodology"),
  R = list(lessons = c("Disclose AI use where it shaped judgements (screening, extraction, appraisal), not where it fixed wording.",
                       "Check the target journal's AI policy before submitting.",
                       "Keep the prompt and decision logs as supplementary files."),
           wiifm = "A submission that meets journal and evidence-synthesis rules on AI the first time.",
           refs = c("flemyng2025", "ganjavi2024", "khraisha2024"),
           query = "generative AI evidence synthesis disclosure")
)

# APA 7 text for a reference (built-in list entry or a live/accepted record with the same fields)
ref_apa <- function(r) {
  vi <- paste0(if (nzchar(r$vol %||% "")) paste0(", ", r$vol) else "", if (nzchar(r$iss %||% "")) paste0("(", r$iss, ")") else "")
  x <- paste0(r$au, " (", r$year, "). ", sub("\\.$", "", r$title), ". ", r$journal, vi, if (nzchar(r$pages %||% "")) paste0(", ", gsub("-", "\u2013", r$pages)) else "",
              ". https://doi.org/", r$doi)
  Encoding(x) <- "UTF-8"; x
}
ref_bib <- function(key, r) {
  a <- trimws(strsplit(gsub("\\.\\.\\.\\s*", "", r$au), "(?<=\\.),\\s*(?:&\\s*)?|,\\s*&\\s*", perl = TRUE)[[1]]); a <- a[nzchar(a)]
  a <- bib_esc(a); a <- ifelse(grepl(",", a), a, paste0("{", a, "}"))
  if (grepl("\\.\\.\\.", r$au)) a <- c(a, "others")
  f <- c(author = paste(a, collapse = " and "), year = r$year, title = paste0("{", bib_esc(r$title), "}"),
         journal = bib_esc(r$journal), volume = r$vol, number = r$iss, pages = gsub("-", "--", r$pages %||% ""), doi = r$doi)
  f <- f[!is.na(f) & nzchar(f)]
  paste0("@article{", key, ",\n", paste(sprintf("  %s = {%s}", names(f), f), collapse = ",\n"), "\n}")
}

# ---------------------------------------------------------------- UI helpers for the stage cards
js_str <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
online_pill <- function(o, job = NULL, long = FALSE) {
  if (!is.null(job)) return(span(class = "badge text-bg-info", icon("spinner", class = "fa-spin"), " checking online"))
  if (is.null(o)) return(span(class = "badge text-bg-secondary", if (long) "Not checked yet" else "built-in"))
  if (isTRUE(o$online)) return(span(class = "badge text-bg-success", icon("globe"), paste(" live", if (long) paste("\u00b7 checked", o$checked_at) else substr(o$checked_at, 12, 16))))
  span(class = "badge text-bg-warning", icon("plug-circle-xmark"), if (long) paste(" offline -", o$errors[1] %||% "", "Built-in references in use.") else " offline")
}
ref_tag <- function(r) {
  cls <- switch(r$status %||% "unchecked", ok = "text-success", retracted = "text-danger fw-bold", concern = "text-danger", corrected = "text-warning", "text-muted")
  tags$li(ref_apa(r), " ", tags$a(href = paste0("https://doi.org/", r$doi), target = "_blank", icon("arrow-up-right-from-square")),
          tags$br(), span(class = paste("small", cls), icon(if (identical(r$status, "ok")) "circle-check" else if (r$status %in% c("retracted", "concern", "corrected")) "triangle-exclamation" else "circle"), " ", status_label(r)))
}
lessons_body <- function(id, s) {
  L <- LESSONS[[id]]; o <- s$online
  refs <- lapply(L$refs, ref_live, st = s)
  acc <- s$phases[[id]]$accepted %||% list()
  sg <- o$suggestions[[id]]
  if (!is.null(sg)) sg <- sg[!sg$doi %in% c(vapply(acc, `[[`, "", "doi"), vapply(refs, `[[`, "", "doi")) & nzchar(sg$doi), , drop = FALSE]
  tagList(
    layout_columns(col_widths = c(7, 5),
      div(h6("Lessons learned"), tags$ul(class = "small", lapply(L$lessons, tags$li))),
      div(h6("What's in it for me"), p(class = "small", L$wiifm))),
    h6("Key references"), tags$ol(class = "small", lapply(refs, ref_tag)),
    if (length(acc)) tagList(h6("References you added"), tags$ol(class = "small", lapply(acc, function(r) tags$li(ref_apa(r), " ",
      tags$a(href = "#", class = "small", onclick = sprintf("Shiny.setInputValue('%s_unaccept', %s, {priority: 'event'}); return false;", id, js_str(r$doi)), "remove"))))),
    if (!is.null(sg) && nrow(sg)) tags$details(class = "small", tags$summary(icon("globe"), sprintf(" Newer reading, live from OpenAlex (%d, most cited first)", nrow(sg))),
      tags$ol(lapply(seq_len(nrow(sg)), function(i) tags$li(ref_apa(as.list(sg[i, ])), sprintf(" Cited %s times. ", sg$cited_by[i]),
        tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('%s_accept', %s, {priority: 'event'}); return false;", id, js_str(sg$doi[i])), icon("plus"), " add to my references")))),
      p(class = "text-muted", "Suggestions are not checked by a person. Read an article before you cite it."))
    else if (isTRUE(o$online)) p(class = "small text-muted", "No newer reading found online for this stage.")
    else p(class = "small text-muted", icon("plug-circle-xmark"), sprintf(" Live suggestions appear when the app is online. References as of %s.", LESSONS_ASOF)))
}

# ---------------------------------------------------------------- report helpers (HTML, Word, Typst, LaTeX)
acc_key <- function(doi) paste0("acc", substr(gsub("[^a-z0-9]", "", tolower(doi)), 1, 40))
lessons_data <- function(id, st) {
  L <- LESSONS[[id]]; ph <- st$phases[[id]]
  list(lessons = L$lessons, wiifm = L$wiifm, refs = setNames(lapply(L$refs, ref_live, st = st), L$refs),
       acc = ph$accepted %||% list(), my_lessons = ph$my_lessons %||% "", my_wiifm = ph$my_wiifm %||% "")
}
online_note <- function(st) {
  o <- st$online
  if (isTRUE(o$online)) sprintf("References verified live at Crossref on %s; new-reading suggestions from OpenAlex.", o$checked_at)
  else sprintf("References as built into ANCHOR Studio %s (checked %s); not re-checked online for this report.", APP_VERSION, LESSONS_ASOF)
}
ds_findings <- function(st) {
  d <- st$online_ds; if (is.null(d) || !is.null(d$error)) return(NULL)
  fl <- d$flagged
  list(text = if (is.null(fl) || !nrow(fl)) sprintf("Retraction check of the bibliometric dataset (%s): %d of %d records have a DOI; none is retracted according to OpenAlex.", d$checked_at, d$n_doi, d$n_records)
              else sprintf("Retraction check of the bibliometric dataset (%s): %d retracted record(s) found among %d records with a DOI.", d$checked_at, nrow(fl), d$n_doi),
       table = if (!is.null(fl) && nrow(fl)) data.frame(Record = fl$record, DOI = fl$doi, Notice = fl$notice) else NULL)
}
lessons_html <- function(id, st) {
  d <- lessons_data(id, st); e <- htmltools::htmlEscape
  li <- function(x) paste0("<li>", x, "</li>", collapse = "")
  ref_li <- function(r) sprintf("%s%s", e(ref_apa(r)), if ((r$status %||% "") %in% c("retracted", "concern", "corrected")) sprintf(" <b style='color:#B3261E'>[%s]</b>", e(status_label(r))) else "")
  paste0("<div class='lessons'><h3>Lessons learned &amp; what's in it for me</h3><ul>", li(e(d$lessons)), "</ul>",
    "<p><b>What's in it for me:</b> ", e(d$wiifm), "</p>",
    if (nzchar(d$my_lessons)) paste0("<p><b>My lessons learned:</b> ", e(d$my_lessons), "</p>") else "",
    if (nzchar(d$my_wiifm)) paste0("<p><b>What's in it for me (my view):</b> ", e(d$my_wiifm), "</p>") else "",
    "<p class='cap'><b>References</b></p><ol class='refs'>", li(vapply(c(d$refs, d$acc), ref_li, "")), "</ol></div>")
}
lessons_docx <- function(doc, id, st) {
  d <- lessons_data(id, st); add <- function(dd, txt, style = "Normal") officer::body_add_par(dd, txt, style = style)
  doc <- add(doc, "Lessons learned & what's in it for me", "heading 2")
  for (x in d$lessons) doc <- add(doc, paste("\u2022", x))
  doc <- add(doc, paste("What's in it for me:", d$wiifm))
  if (nzchar(d$my_lessons)) doc <- add(doc, paste("My lessons learned:", d$my_lessons))
  if (nzchar(d$my_wiifm)) doc <- add(doc, paste("What's in it for me (my view):", d$my_wiifm))
  doc <- add(doc, "References", "heading 3")
  for (r in c(d$refs, d$acc)) doc <- add(doc, paste0(ref_apa(r), if ((r$status %||% "") %in% c("retracted", "concern", "corrected")) paste0(" [", status_label(r), "]") else ""))
  doc
}
lessons_typ <- function(id, st) {
  d <- lessons_data(id, st)
  paste0("#heading(level: 3)[Lessons learned & what's in it for me]\n\n",
    "#list(", paste(sprintf("[%s]", vapply(d$lessons, typ_txt, "")), collapse = ", "), ")\n\n",
    "*What's in it for me:* ", typ_txt(d$wiifm), "\n\n",
    if (nzchar(d$my_lessons)) paste0("*My lessons learned:* ", typ_txt(d$my_lessons), "\n\n") else "",
    if (nzchar(d$my_wiifm)) paste0("*What's in it for me (my view):* ", typ_txt(d$my_wiifm), "\n\n") else "",
    "#text(size: 9pt)[#enum(", paste(vapply(c(d$refs, d$acc), function(r) sprintf("[%s%s]", typ_txt(ref_apa(r)),
      if ((r$status %||% "") %in% c("retracted", "concern", "corrected")) sprintf(" #text(fill: rgb(\"#B3261E\"), weight: \"bold\")[%s]", typ_txt(paste0("[", status_label(r), "]"))) else ""), ""), collapse = ", "), ")]\n\n")
}
lessons_tex <- function(id, st) {
  d <- lessons_data(id, st)
  keys <- c(names(d$refs), vapply(d$acc, function(r) acc_key(r$doi), ""))
  c("\\subsection*{Lessons learned \\& what's in it for me}", "\\begin{itemize}", paste("\\item", tex_esc(d$lessons)), "\\end{itemize}",
    paste0("\\textbf{What's in it for me:} ", tex_esc(d$wiifm), "\n"),
    if (nzchar(d$my_lessons)) paste0("\\textbf{My lessons learned:} ", tex_esc(d$my_lessons), "\n"),
    if (nzchar(d$my_wiifm)) paste0("\\textbf{What's in it for me (my view):} ", tex_esc(d$my_wiifm), "\n"),
    paste0("\\textbf{References:} \\cite{", paste(keys, collapse = ","), "}.",
           paste(vapply(c(d$refs, d$acc), function(r) if ((r$status %||% "") %in% c("retracted", "concern", "corrected")) paste0(" \\textbf{", tex_esc(r$title), ": ", tex_esc(status_label(r)), ".}") else "", ""), collapse = ""), "\n"))
}

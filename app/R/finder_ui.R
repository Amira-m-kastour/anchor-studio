# ANCHOR Studio 1.5.0 - "Find a focal article" page (before the Start page) and its server logic

find_ui <- function() {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  div(class = "pt-3",
    card(fill = FALSE, card_header(icon("magnifying-glass"), " Find a focal article", span(class = "small text-muted ms-2", "optional: start from a subject instead of an article you already have")),
      p(class = "small text-muted mb-2", "Search OpenAlex, shortlist up to ten candidates, read them, compare them and choose one. Legal open copies download into a private folder on this computer; ",
        "paywalled articles open through your library (link prefix on ", actionLink("fd_goto_prereq", "Before you start"), "). Downloaded PDFs are never put in project files, reports or shared packages. ",
        "Already have the article? ", actionLink("fd_skip", "Go to Start and upload it.")),
      div(class = "d-flex gap-2 align-items-start flex-wrap",
          div(style = "flex:1;min-width:260px", textInput("fd_q", NULL, placeholder = "Subject in plain words, e.g. ESG rating disagreement stock returns", width = "100%")),
          actionButton("fd_go", "Search", icon = icon("magnifying-glass"), class = "btn-primary")),
      div(class = "d-flex gap-3 flex-wrap align-items-end small fd-filters",
          numericInput("fd_from", "From year", 2000, 1900, yr, width = "110px"), numericInput("fd_to", "To year", yr, 1900, yr, width = "110px"),
          checkboxGroupInput("fd_types", "Type", FIND_TYPES, selected = c("article", "review"), inline = TRUE),
          selectInput("fd_lang", "Language", FIND_LANGS, selected = "", width = "150px"),
          numericInput("fd_mincites", "Min. citations", 0, 0, 100000, width = "120px"),
          selectInput("fd_sort", "Sort by", FIND_SORTS, width = "230px"),
          checkboxInput("fd_oa", "Open access only", FALSE)),
      uiOutput("fd_status"),
      DT::DTOutput("fd_results", height = "auto", fill = FALSE),
      div(class = "d-flex gap-2 mt-2 align-items-center", actionButton("fd_add", "Add selected rows to the shortlist", icon = icon("star"), class = "btn-sm btn-outline-primary"),
          span(class = "small text-muted", "Select rows in the table (click), then add them. Flags: retracted articles are marked in red."))),
    card(fill = FALSE, card_header(icon("star"), " Shortlist ", uiOutput("fd_count", inline = TRUE)), uiOutput("fd_short_ui")),
    card(fill = FALSE, card_header(icon("table-columns"), " Compare the candidates"),
      p(class = "small text-muted", "Facts fetched from OpenAlex, your own ticks and a fit score. The score combines your ticks (60%) with citations per year, field-weighted citation impact and open-copy availability (40%); a retracted article scores 0. It is a guide only: you choose."),
      uiOutput("fd_compare"),
      uiOutput("fd_ai_ui")),
    card(fill = FALSE, card_header(icon("anchor"), " Use as focal article"),
      uiOutput("fd_choice_ui"),
      textAreaInput("fd_reason", "Why this article? (goes into the decision log and the report)", rows = 2, width = "100%"),
      actionButton("fd_use", "Use as focal article and open Start", icon = icon("arrow-right"), class = "btn-primary"),
      uiOutput("fd_used")))
}

find_server <- function(input, output, session, st, log_decision, log_prompt, load_article) {
  res <- reactiveVal(NULL)                                    # current search result (not saved)
  store <- reactiveValues(short = NULL, files = list(), ticks = list(), notes = list(), jf = list(), summary = "", ver = 0)
  cfg <- function() online_cfg(isolate(st$settings))
  online_off <- function() isFALSE(isolate(input$online))
  observeEvent(input$fd_goto_prereq, nav_select("nav", "prereq")); observeEvent(input$fd_skip, nav_select("nav", "start"))

  # restore from a project (metadata only; files are looked up again in the private folder)
  last_sync <- NULL
  observeEvent(st$finder, { f <- st$finder; if (is.null(f) || identical(f, last_sync)) return()
    if (!is.null(f$shortlist)) { store$files <- list(); store$jf <- list()
      store$short <- f$shortlist; store$ticks <- f$ticks %||% list(); store$notes <- f$notes %||% list(); store$summary <- f$summary %||% ""
      for (i in seq_len(nrow(f$shortlist))) { p <- file.path(find_dir(), paste0(gsub("[^A-Za-z0-9._-]", "_", if (nzchar(f$shortlist$doi[i])) f$shortlist$doi[i] else f$shortlist$id[i]), ".pdf"))
        if (file.exists(p)) store$files[[f$shortlist$id[i]]] <- list(path = p, size = file.size(p), at = format(file.mtime(p), "%Y-%m-%d %H:%M"), how = "on this computer") }
      if (nzchar(f$query %||% "")) updateTextInput(session, "fd_q", value = f$query)
      store$ver <- isolate(store$ver) + 1 } })

  observeEvent(input$fd_go, {
    if (online_off()) return(showNotification("Searching needs the internet features (Settings). You can still upload an article on the Start page.", type = "warning", duration = 8))
    r <- withProgress(message = "Searching OpenAlex...", value = .4, tryCatch(
      find_search(cfg(), input$fd_q, input$fd_from, input$fd_to, input$fd_types, input$fd_lang, isTRUE(input$fd_oa), input$fd_mincites %||% 0, input$fd_sort %||% "relevance", 50),
      error = function(e) e))
    if (inherits(r, "error")) return(showNotification(paste("Search failed:", conditionMessage(r)), type = "error", duration = 10))
    res(r) })
  output$fd_status <- renderUI({ r <- res()
    if (is.null(r)) return(p(class = "small text-muted mt-2", if (online_off()) "Internet features are off (Settings), so search is not available." else "No search yet."))
    if (is.null(r$rows)) return(div(class = "alert alert-secondary small mt-2", "No matches. Widen the years, remove filters or use other words."))
    p(class = "small mt-2", sprintf("%s matches in OpenAlex (%s); showing the first %d. ", format(r$found, big.mark = ","), r$at, nrow(r$rows)),
      if (any(r$rows$retracted)) span(class = "text-danger", sprintf("%d retracted.", sum(r$rows$retracted)))) })
  output$fd_results <- DT::renderDT({ r <- res(); req(r$rows); d <- r$rows
    oa <- ifelse(nzchar(d$pdf_url) | nzchar(d$oa_url), paste0("open", ifelse(nzchar(d$version), paste0(" (", vlabel(d$version), ")"), "")), "closed")
    tb <- data.frame(Title = ifelse(d$retracted, paste0("<span class='text-danger fw-bold'>[RETRACTED]</span> ", htmltools::htmlEscape(d$title)), htmltools::htmlEscape(d$title)),
                     Authors = htmltools::htmlEscape(d$authors), Year = d$year, Journal = htmltools::htmlEscape(d$journal), Citations = d$cites, `Per year` = d$per_year,
                     FWCI = round(d$fwci, 2), Access = oa, check.names = FALSE)
    DT::datatable(tb, rownames = FALSE, escape = FALSE, selection = "multiple", fillContainer = FALSE, options = list(pageLength = 10, scrollX = TRUE, order = list()))
  })

  observeEvent(input$fd_add, { r <- res(); s <- input$fd_results_rows_selected
    if (is.null(r$rows) || !length(s)) return(showNotification("Select one or more rows in the results first.", type = "warning"))
    cur <- isolate(store$short); new <- r$rows[s, , drop = FALSE]; if (!is.null(cur)) new <- new[!new$id %in% cur$id, , drop = FALSE]
    room <- FIND_MAX_SHORT - (if (is.null(cur)) 0L else nrow(cur))
    if (!nrow(new)) return(showNotification("Already on the shortlist.", type = "message"))
    if (room <= 0) return(showNotification(sprintf("The shortlist holds %d candidates. Remove one first.", FIND_MAX_SHORT), type = "warning"))
    if (nrow(new) > room) { showNotification(sprintf("Only %d more fit; the first %d were added.", room, room), type = "warning"); new <- new[seq_len(room), , drop = FALSE] }
    store$short <- rbind(cur, new)
    for (sid in setdiff(unique(new$source_id[nzchar(new$source_id)]), names(isolate(store$jf))))       # journal facts, once per journal
      store$jf[[sid]] <- tryCatch(journal_facts(sid, cfg()), error = function(e) NULL)
    store$ver <- isolate(store$ver) + 1; sync() })

  # one event channel for the buttons inside the shortlist
  observeEvent(input$fd_act, { a <- input$fd_act; sh <- isolate(store$short); i <- match(a$id, sh$id); if (is.na(i)) return(); r <- sh[i, ]
    if (a$act == "remove") { store$short <- sh[-i, , drop = FALSE]; if (!nrow(store$short)) store$short <- NULL; store$ver <- isolate(store$ver) + 1; sync(); return() }
    if (a$act == "dl") {
      if (online_off()) return(showNotification("Downloading needs the internet features (Settings).", type = "warning"))
      f <- withProgress(message = "Downloading the open copy...", value = .5, tryCatch(find_download(r, cfg()), error = function(e) e))
      if (inherits(f, "error")) return(showNotification(paste("Download failed:", conditionMessage(f)), type = "error", duration = 12))
      store$files[[r$id]] <- f; store$ver <- isolate(store$ver) + 1
      log_decision("Find", "Open copy downloaded (private folder)", sprintf("doi:%s; %s; licence: %s", r$doi, vlabel(r$version), if (nzchar(r$licence)) r$licence else "not stated"))
      showNotification(sprintf("Saved to %s (%s KB). Kept on this computer only.", f$path, round(f$size / 1024)), type = "message", duration = 8) } })

  # PDFs the researcher obtained (through the library) and uploads per candidate
  seen_up <- list()
  observe({ sh <- store$short; if (is.null(sh)) return()
    for (id in sh$id) { f <- input[[paste0("fd_up_", id)]]
      if (!is.null(f) && !identical(seen_up[[id]], f$datapath)) { seen_up[[id]] <<- f$datapath
        r <- sh[sh$id == id, ]; dest <- file.path(find_dir(), paste0(gsub("[^A-Za-z0-9._-]", "_", if (nzchar(r$doi)) r$doi else r$id), ".pdf"))
        ok <- tryCatch({ b <- readBin(f$datapath, "raw", 5); identical(rawToChar(b[1:4]), "%PDF") }, error = function(e) FALSE)
        if (!ok) { showNotification("That file is not a PDF.", type = "error"); next }
        file.copy(f$datapath, dest, overwrite = TRUE)
        isolate(store$files[[id]] <- list(path = dest, size = file.size(dest), at = format(Sys.time(), "%Y-%m-%d %H:%M"), how = "uploaded by you"))
        isolate(store$ver <- store$ver + 1); showNotification("PDF kept in the private folder on this computer.", type = "message") } } })

  # ticks and notes are stored without redrawing the shortlist
  observe({ sh <- store$short; if (is.null(sh)) return()
    for (id in sh$id) { t <- input[[paste0("fd_tick_", id)]]; n <- input[[paste0("fd_note_", id)]]
      if (!is.null(n)) isolate(store$notes[[id]] <- n)
      if (!is.null(t) || !is.null(input[[paste0("fd_note_", id)]])) isolate(store$ticks[[id]] <- t %||% character(0)) }
    sync() })

  output$fd_count <- renderUI(span(class = "small text-muted", sprintf("%d of %d", if (is.null(store$short)) 0L else nrow(store$short), FIND_MAX_SHORT)))
  act <- function(act, id, label, icn, cls = "btn-outline-secondary") tags$button(type = "button", class = paste("btn btn-sm", cls),
    onclick = sprintf("Shiny.setInputValue('fd_act',{act:'%s',id:'%s',n:Math.random()},{priority:'event'})", act, id), icon(icn), label)
  output$fd_short_ui <- renderUI({ store$ver; sh <- isolate(store$short)
    if (is.null(sh)) return(p(class = "small text-muted", "Nothing shortlisted yet. Search, select rows and add them."))
    resolver <- isolate(st$prereq$resolver) %||% ""
    tagList(lapply(seq_len(nrow(sh)), function(i) { r <- sh[i, ]; f <- isolate(store$files[[r$id]])
      vl <- if (nzchar(r$version)) vlabel(r$version) else ""
      div(class = "fd-cand border rounded p-2 mb-2",
        div(class = "d-flex justify-content-between gap-2 flex-wrap",
            div(tags$b(r$title), if (r$retracted) span(class = "badge bg-danger ms-2", "retracted"),
                div(class = "small text-muted", sprintf("%s (%s). %s", r$authors, r$year, r$journal), if (nzchar(r$doi)) tagList(" \u00b7 doi:", r$doi))),
            act("remove", r$id, "Remove", "xmark")),
        div(class = "d-flex gap-2 flex-wrap align-items-center mt-1 small",
            if (nzchar(r$pdf_url) || nzchar(r$oa_url)) span(class = "badge bg-success", paste0("open copy", if (nzchar(vl)) paste0(": ", vl) else ""), if (nzchar(r$licence)) paste0(" \u00b7 ", r$licence)) else span(class = "badge bg-secondary", "no open copy"),
            if (nzchar(r$pdf_url) && is.null(f)) act("dl", r$id, "Download open copy", "download", "btn-outline-primary"),
            if (nzchar(r$oa_url)) tags$a(class = "btn btn-sm btn-outline-secondary", href = r$oa_url, target = "_blank", icon("up-right-from-square"), " Open copy"),
            if (nzchar(r$doi)) tags$a(class = "btn btn-sm btn-outline-secondary", href = library_link(r$doi, resolver), target = "_blank", icon("building-columns"), if (nzchar(resolver)) " Open in library" else " Open at publisher (DOI)"),
            if (!is.null(f)) span(class = "text-success", icon("file-pdf"), sprintf(" PDF on this computer (%s, %s KB)", f$how, round(f$size / 1024)))),
        if (is.null(f)) div(class = "small mt-1 fd-up", fileInput(paste0("fd_up_", r$id), NULL, accept = ".pdf", buttonLabel = "Upload the PDF you obtained...", placeholder = "kept privately", width = "420px")),
        layout_columns(col_widths = c(6, 6),
          checkboxGroupInput(paste0("fd_tick_", r$id), "Your judgement", setNames(names(FIND_CRITERIA), FIND_CRITERIA), selected = isolate(store$ticks[[r$id]]) %||% character(0)),
          textAreaInput(paste0("fd_note_", r$id), "Notes", value = isolate(store$notes[[r$id]]) %||% "", rows = 3, width = "100%")),
        if (nzchar(r$abstract)) tags$details(class = "small", tags$summary("Abstract (OpenAlex)"), p(r$abstract)) else p(class = "small text-muted", "No abstract in OpenAlex."))
    })) })

  cmp <- reactive({ sh <- store$short; if (is.null(sh)) return(NULL); ticks <- store$ticks
    sh$ticks <- vapply(sh$id, function(id) length(ticks[[id]] %||% character(0)), 1L)
    sh$fit <- vapply(seq_len(nrow(sh)), function(i) fit_score(sh[i, ], ticks[[sh$id[i]]], sh), 1L); sh })
  output$fd_compare <- renderUI({ sh <- cmp(); if (is.null(sh)) return(p(class = "small text-muted", "Shortlist candidates to compare them."))
    jf <- store$jf
    j2 <- vapply(sh$source_id, function(s) { j <- jf[[s]]; if (is.null(j)) "-" else sprintf("%s / h %s", j$`Two-year mean citedness`, j$`h-index`) }, "")
    vl <- ifelse(nzchar(sh$version), vlabel(sh$version), "")
    df <- data.frame(Candidate = sprintf("%s (%s)", substr(sh$title, 1, 70), sh$year), Retracted = ifelse(sh$retracted, "YES", "no"), Citations = sh$cites, `Per year` = sh$per_year,
                     FWCI = ifelse(is.na(sh$fwci), "-", format(round(sh$fwci, 2))), `Journal: 2-yr citedness / h-index` = j2,
                     `Open copy` = ifelse(nzchar(sh$pdf_url) | nzchar(sh$oa_url), ifelse(nzchar(vl), vl, "yes"), "none"), References = sh$n_refs,
                     `PDF here` = ifelse(sh$id %in% names(store$files), "yes", "no"), `Your ticks` = paste0(sh$ticks, "/4"), `Fit (guide)` = sh$fit, check.names = FALSE)
    div(style = "overflow-x:auto", HTML(html_df(df))) })

  # optional AI summary of the abstracts, in the current drafting mode
  output$fd_ai_ui <- renderUI({ sh <- store$short; if (is.null(sh)) return(NULL); m <- input$mode %||% "prompt"
    out <- if (nzchar(store$summary)) div(class = "border rounded p-2 mt-2 small", tags$b("AI summary of the abstracts (labelled as AI output; check it against the abstracts)"), tags$pre(style = "white-space:pre-wrap", store$summary))
    head <- h6(class = "mt-3", "Optional: AI summary of each candidate's abstract")
    if (m == "manual") return(tagList(head, p(class = "small text-muted", "Your drafting mode is Manual (no AI), so no summary is offered. Read the abstracts above."), out))
    if (m == "prompt") return(tagList(head, p(class = "small text-muted", "Copy this prompt into claude.ai, then paste the answer back. The prompt is logged only when you paste an answer."),
      textAreaInput("fd_ai_prompt", NULL, value = find_summary_prompt(sh), rows = 5, width = "100%"),
      tags$button(type = "button", class = "btn btn-sm btn-outline-secondary mb-2", onclick = "anchorCopy('fd_ai_prompt')", icon("copy"), " Copy prompt"),
      textAreaInput("fd_ai_paste", "Paste the answer here", rows = 4, width = "100%"), actionButton("fd_ai_save", "Save the summary", class = "btn-sm"), out))
    tagList(head, p(class = "small text-muted", sprintf("Sends the abstracts (not your notes) to %s.", if (m == "api") "the Claude API" else "your local Ollama model")),
            actionButton("fd_ai_run", "Summarise the abstracts", icon = icon("wand-magic-sparkles"), class = "btn-sm"), out) })
  observeEvent(input$fd_ai_run, { sh <- isolate(store$short); req(sh); pr <- find_summary_prompt(sh)
    txt <- withProgress(message = "Summarising the abstracts...", value = .5, tryCatch(ai_draft(input$mode, pr, isolate(st$settings)), error = function(e) e))
    if (inherits(txt, "error")) return(showNotification(conditionMessage(txt), type = "error", duration = 10))
    log_prompt("Find", pr); store$summary <- trimws(txt); sync() })
  observeEvent(input$fd_ai_save, { t <- trimws(input$fd_ai_paste %||% ""); if (!nzchar(t)) return(showNotification("Paste the answer first.", type = "warning"))
    log_prompt("Find", input$fd_ai_prompt %||% find_summary_prompt(isolate(store$short))); store$summary <- trimws(t); sync()
    showNotification("Summary saved; the prompt is in the prompt log.", type = "message") })

  output$fd_choice_ui <- renderUI({ sh <- store$short; if (is.null(sh)) return(p(class = "small text-muted", "Shortlist at least one candidate."))
    selectInput("fd_choice", "Candidate", setNames(sh$id, sprintf("%s (%s)%s", substr(sh$title, 1, 90), sh$year, ifelse(sh$retracted, " [RETRACTED]", ""))), width = "100%") })
  use_it <- function() { sh <- cmp(); id <- isolate(input$fd_choice); r <- sh[sh$id == id, ]; if (!nrow(r)) return()
    f <- isolate(store$files[[id]])
    ref <- find_ref(r); cur <- isolate(input$art_citation) %||% ""     # placeholder until Crossref answers (set first, so the look-up wins)
    if (!nzchar(trimws(cur)) || cite_is_auto(cur, isolate(st$article))) { updateTextAreaInput(session, "art_citation", value = ref); st$article <- mark_auto(isolate(st$article), ref) }
    if (!is.null(f) && file.exists(f$path)) load_article(f$path, basename(f$path))
    updateTextInput(session, "art_title", value = r$title); updateTextInput(session, "art_doi", value = r$doi)
    if (!is.null(f) && identical(f$how, "open copy") && !is.na(VERSION_TO_START[r$version])) updateRadioButtons(session, "art_version", selected = unname(VERSION_TO_START[r$version]))
    alts <- sh[sh$id != id, ]
    sync(chosen = id, reason = isolate(input$fd_reason) %||% "", chosen_at = format(Sys.time(), "%Y-%m-%d %H:%M"))
    log_decision("Find", "Focal article chosen from search", sprintf("%s (%s) doi:%s; fit %d; %d alternative(s)%s%s", r$title, r$year, r$doi, r$fit, nrow(alts),
      if (nrow(alts)) paste0(": ", paste(sprintf("%s (doi:%s, fit %d)", substr(alts$title, 1, 60), alts$doi, alts$fit), collapse = "; ")) else "",
      if (nzchar(isolate(input$fd_reason) %||% "")) paste0("; reason: ", isolate(input$fd_reason)) else ""))
    showNotification(if (is.null(f)) "Start page filled. Upload the PDF there once you have it through your library." else "Start page filled and the PDF loaded from the private folder.", type = "message", duration = 8)
    nav_select("nav", "start") }
  observeEvent(input$fd_use, { sh <- isolate(store$short); id <- input$fd_choice
    if (is.null(sh) || is.null(id)) return(showNotification("Shortlist and pick a candidate first.", type = "warning"))
    r <- sh[sh$id == id, ]
    if (isTRUE(r$retracted)) return(showModal(modalDialog(title = "This article is retracted", "OpenAlex marks this article as retracted. Anchoring a review on it undermines the review. Use it anyway? The choice will be logged.",
      footer = tagList(modalButton("Cancel"), actionButton("fd_use_anyway", "Use it anyway", class = "btn-danger")))))
    use_it() })
  observeEvent(input$fd_use_anyway, { removeModal(); log_decision("Find", "Retracted article chosen despite warning", isolate(input$fd_choice)); use_it() })
  output$fd_used <- renderUI({ f <- st$finder; if (is.null(f$chosen)) return(NULL)
    p(class = "small text-success mt-2", icon("circle-check"), sprintf(" Chosen %s; logged with %d alternative(s).", f$chosen_at %||% "", max(0, nrow(f$shortlist %||% data.frame()) - 1))) })

  # what is kept in the project: metadata, ticks, notes, summary, choice (no PDFs, no file paths)
  sync <- function(...) { sh <- isolate(cmp()); old <- isolate(st$finder) %||% list(); r <- isolate(res())
    keep <- if (!is.null(sh)) sh[, setdiff(names(sh), "ticks"), drop = FALSE] else NULL
    st$finder <- modifyList(old, c(list(shortlist = keep, ticks = isolate(reactiveValuesToList(store))$ticks, notes = isolate(store$notes), summary = isolate(store$summary),
      query = r$query %||% old$query, filters = r$filters %||% old$filters, sort = r$sort %||% old$sort, found = r$found %||% old$found, searched_at = r$at %||% old$searched_at), list(...)))
    last_sync <<- isolate(st$finder) }
}

# ANCHOR Studio - the Proposal / thesis tab (interface and server logic). The model and the Word export are in proposal.R.

proposal_ui <- function() {
  layout_columns(col_widths = c(4, 8),
    div(uiOutput("prop_doc_card"), uiOutput("prop_att_card"), uiOutput("prop_check_card"),
        card(card_header("Export"),
             p(class = "small text-muted", "Word, on your template if you uploaded one. Word asks to update the table of contents when the file opens; answer yes."),
             downloadButton("prop_dl_docx", "Proposal (Word)", class = "btn-primary"),
             downloadButton("prop_dl_pdf", "Proposal (PDF)"), downloadButton("prop_dl_tex", "LaTeX package (.zip)"),
             p(class = "small text-muted mt-2", "PDF is printed with Microsoft Edge or Google Chrome. LaTeX compiles with XeLaTeX (Overleaf: Menu > Compiler > XeLaTeX)."),
             p(class = "small text-muted mt-2", "Arabic: the text is exported as written; right-to-left page layout comes from your template's styles."))),
    div(uiOutput("prop_start"), uiOutput("prop_outline"), uiOutput("prop_add"))
  )
}

proposal_server <- function(input, output, session, st, log_decision, snap, typed, ui_lang) {
  sec_key <- function(sid) paste0("P:", sid)
  act <- function(sid, a, k = 0) sprintf("Shiny.setInputValue('prop_act', {sid: '%s', a: '%s', k: %d, n: Math.random()}, {priority: 'event'}); return false;", sid, a, k)
  set_pr <- function(pr) st$proposal <- pr
  struct <- reactiveVal(0); bump <- function() struct(isolate(struct()) + 1)

  # ---------- start
  output$prop_start <- renderUI({
    if (!is.null(st$proposal)) return(NULL)
    card(card_header("Start a research proposal"),
      p("The proposal is built from your approved phases, your own text, a timeline, figures and attachments, and exported to Word, PDF and LaTeX. A thesis (front matter, lists of figures and tables, results chapters) follows in a later version."),
      layout_columns(col_widths = c(6, 6),
        selectInput("prop_preset", "Outline", setNames(names(PROPOSAL_PRESETS), vapply(PROPOSAL_PRESETS, `[[`, "", "name"))),
        selectInput("prop_lang0", "Language of the proposal", c("English" = "en", "Deutsch" = "de", "\u0627\u0644\u0639\u0631\u0628\u064a\u0629" = "ar"), selected = isolate(ui_lang()) %||% "en"))
      , actionButton("prop_create", "Create the outline", icon = icon("list-ol"), class = "btn-primary"))
  })
  observeEvent(input$prop_create, {
    pr <- new_proposal(input$prop_preset, input$prop_lang0, reactiveValuesToList(st)); set_pr(pr); bump()
    log_decision("Proposal", "Proposal outline created", paste0(PROPOSAL_PRESETS[[input$prop_preset]]$name, "; language ", input$prop_lang0))
  })

  # ---------- document settings
  output$prop_doc_card <- renderUI({ struct(); pr <- isolate(st$proposal); if (is.null(pr)) return(NULL)
    card(card_header("Document"),
      textInput("prop_title", "Title", pr$title), textInput("prop_subtitle", "Subtitle (optional)", pr$subtitle),
      textInput("prop_author", "Submitted by", pr$author), textInput("prop_programme", "Programme", pr$programme),
      textInput("prop_supervisor", "Supervisor", pr$supervisor), textInput("prop_place", "Place", pr$place),
      selectInput("prop_lang", "Language of the proposal", c("English" = "en", "Deutsch" = "de", "\u0627\u0644\u0639\u0631\u0628\u064a\u0629" = "ar"), pr$lang),
      selectInput("prop_cite", "Citation style of the references", c("APA 7" = "apa", "Harvard" = "harvard"), pr$cite_style),
      checkboxInput("prop_toc", "Table of contents", isTRUE(pr$toc)),
      numericInput("prop_wlimit", "Word limit (optional)", if (is.na(pr$word_limit)) NA else pr$word_limit, min = 0, step = 100),
      if (!is.na(pr$page_limit %||% NA)) p(class = "small text-muted", sprintf("This outline's guide allows at most %d pages.", as.integer(pr$page_limit))),
      fileInput("prop_template", "Your Word template (.docx or .dotx)", accept = c(".docx", ".dotx")),
      if (!is.null(pr$template)) div(class = "small mb-2", icon("file-word"), " ", pr$template$name, " ", tags$a(href = "#", onclick = act("", "notemplate"), "remove")) else
        p(class = "small text-muted", "Without a template the preset's layout is used: 12 pt, line spacing 1.5, margins 2.5 cm."),
      tags$a(href = "#", class = "small text-danger", onclick = act("", "reset"), "Start again with a new outline"))
  })
  # (reads whether a proposal exists before req(): an observer that stops before reading its inputs never runs again)
  observe({ struct(); if (is.null(st$proposal)) return(); pr <- isolate(st$proposal); req(!is.null(input$prop_title))
    new <- list(title = input$prop_title, subtitle = input$prop_subtitle, author = input$prop_author, programme = input$prop_programme, supervisor = input$prop_supervisor,
                place = input$prop_place, lang = input$prop_lang, cite_style = input$prop_cite, toc = isTRUE(input$prop_toc), word_limit = input$prop_wlimit %||% NA)
    new <- Filter(Negate(is.null), new); if (!length(new)) return()
    ch <- vapply(names(new), function(k) !identical(pr[[k]], new[[k]]) && !(is.na(new[[k]][1]) && is.na(pr[[k]] %||% NA)[1]), TRUE)
    if (any(ch)) { for (k in names(new)[ch]) pr[[k]] <- new[[k]]; set_pr(pr) } })
  observeEvent(input$prop_template, { f <- input$prop_template; pr <- isolate(st$proposal); req(pr, f)
    if (f$size > PROPOSAL_ATTACH_MAX) return(showNotification("The template is larger than 20 MB.", type = "error"))
    raw <- readBin(f$datapath, "raw", f$size)
    ok <- tryCatch({ officer::read_docx(template_as_docx(raw)); TRUE }, error = function(e) FALSE)
    if (!ok) return(showNotification("This file could not be read as a Word template.", type = "error"))
    pr$template <- list(name = f$name, raw = raw); set_pr(pr); bump(); log_decision("Proposal", "Word template added", f$name) })

  # ---------- attachments (decision 7: inside the project up to 20 MB, a link to the file on this computer above)
  output$prop_att_card <- renderUI({ struct(); pr <- st$proposal; if (is.null(pr)) return(NULL)
    att <- pr$attachments %||% list()
    card(card_header("Figures and attachments"),
      p(class = "small text-muted", "Figures (PNG or JPEG) can be placed in sections; appendix files are listed in the appendix; private files (ethics letters, data agreements, feedback) stay in the project and are never exported."),
      selectInput("prop_att_role", NULL, c("Figure for a section" = "figure", "Appendix file" = "appendix", "Private (never exported)" = "private")),
      textInput("prop_att_caption", NULL, placeholder = "Caption or short description"),
      fileInput("prop_att_file", NULL, multiple = TRUE, buttonLabel = "Add files..."),
      tags$details(class = "small", tags$summary("A file over 20 MB"), textInput("prop_att_path", "Its path on this computer", placeholder = "C:\\Users\\...\\file.pdf"),
                   actionButton("prop_att_link", "Add as a link", class = "btn-sm")),
      if (length(att)) tags$ul(class = "small mt-2", lapply(att, function(a) tags$li(tags$b(a$name), sprintf(" (%s%s)", a$role, if (nzchar(a$path %||% "")) ", link" else sprintf(", %.1f MB", (a$size %||% 0) / 1024^2)),
        if (nzchar(a$caption %||% "")) span(class = "text-muted", " \u2013 ", a$caption), " ", tags$a(href = "#", onclick = act(a$id, "rmatt"), "remove")))))
  })
  observeEvent(input$prop_att_file, { fs <- input$prop_att_file; pr <- isolate(st$proposal); req(pr, fs)
    for (i in seq_len(nrow(fs))) {
      if (fs$size[i] > PROPOSAL_ATTACH_MAX) { showNotification(sprintf("%s is larger than 20 MB: add it as a link instead.", fs$name[i]), type = "warning", duration = 10); next }
      if (input$prop_att_role == "figure" && !grepl("\\.(png|jpe?g)$", tolower(fs$name[i]))) { showNotification(sprintf("%s: figures must be PNG or JPEG.", fs$name[i]), type = "warning"); next }
      pr$attachments[[length(pr$attachments) + 1]] <- list(id = paste0("att", format(as.numeric(Sys.time()) * 1000, scientific = FALSE, digits = 15), i), name = fs$name[i], role = input$prop_att_role,
        caption = input$prop_att_caption %||% "", raw = readBin(fs$datapath[i], "raw", fs$size[i]), path = "", size = fs$size[i], added = format(Sys.time(), "%Y-%m-%d %H:%M"))
      log_decision("Proposal", "Attachment added", sprintf("%s (%s)", fs$name[i], input$prop_att_role)) }
    set_pr(pr); bump(); updateTextInput(session, "prop_att_caption", value = "") })
  observeEvent(input$prop_att_link, { p0 <- trimws(input$prop_att_path %||% ""); pr <- isolate(st$proposal); req(pr, nzchar(p0))
    pr$attachments[[length(pr$attachments) + 1]] <- list(id = paste0("att", format(as.numeric(Sys.time()) * 1000, scientific = FALSE, digits = 15)), name = basename(gsub("\\\\", "/", p0)), role = input$prop_att_role,
      caption = input$prop_att_caption %||% "", raw = NULL, path = p0, size = if (file.exists(p0)) file.size(p0) else NA, added = format(Sys.time(), "%Y-%m-%d %H:%M"))
    set_pr(pr); bump(); log_decision("Proposal", "Attachment linked", p0); updateTextInput(session, "prop_att_path", value = "") })

  # ---------- the outline
  fig_choices <- function(pr) {
    its <- isolate(st$report_items) %||% list(); figs <- Filter(function(a) identical(a$role, "figure") && length(a$raw), pr$attachments %||% list())
    lab <- vapply(its, function(i) i$label, ""); ids <- vapply(figs, function(a) a$id, ""); cap <- vapply(figs, function(a) a$caption %|e|% a$name, "")
    c(if (length(lab)) setNames(paste0("item:", lab), paste("Report figure:", lab)), if (length(ids)) setNames(ids, paste("Your figure:", cap)))   # empty lists give no choices
  }
  # what decides whether the outline is drawn again: its structure, the links and the state of the linked phases
  sig <- reactive({ pr <- st$proposal; if (is.null(pr)) return("none"); s0 <- reactiveValuesToList(st)
    paste(struct(), paste(vapply(pr$sections, function(s) paste(s$id, s$kind, paste(s$links, collapse = ""), paste(prop_stale_links(s, s0), collapse = ""),
      paste(vapply(s$links, function(j) s0$phases[[j]]$status, ""), collapse = ""), sum(s0$draft_versions$phase == sec_key(s$id))), ""), collapse = "|"),
      length(s0$report_items), length(pr$attachments)) })
  sig_d <- reactiveVal(""); observe({ x <- sig(); if (!identical(x, isolate(sig_d()))) sig_d(x) })
  output$prop_outline <- renderUI({ sig_d(); pr <- isolate(st$proposal); if (is.null(pr)) return(NULL); s0 <- isolate(reactiveValuesToList(st))
    choices <- fig_choices(pr); n <- 0
    tagList(lapply(seq_along(pr$sections), function(k) { s <- pr$sections[[k]]; sid <- s$id
      num <- if (s$kind == "appendix") "" else { n <<- n + 1; n }
      stale <- prop_stale_links(s, s0)
      v <- s0$draft_versions; v <- v[v$phase == sec_key(sid), , drop = FALSE]
      card(class = "prop-sec",
        card_header(class = "d-flex align-items-center gap-2",
          span(class = "fw-bold", num), div(class = "flex-grow-1", textInput(paste0("ps_", sid, "_title"), NULL, s$title, width = "100%")),
          span(class = "small text-muted no-i18n", s$kind),
          tags$a(href = "#", title = "Move up", onclick = act(sid, "up"), icon("arrow-up")), tags$a(href = "#", title = "Move down", onclick = act(sid, "down"), icon("arrow-down")),
          tags$a(href = "#", title = "Remove this section", class = "text-danger", onclick = act(sid, "remove"), icon("trash"))),
        if (!s$kind %in% c("timeline", "appendix")) div(
          checkboxGroupInput(paste0("ps_", sid, "_links"), "Use the approved text of", inline = TRUE, choices = setNames(PROP_PHASES, PROP_PHASES), selected = s$links),
          if (length(s$links)) div(class = "small mb-2", lapply(s$links, function(j) { a <- phase_ai(s0, j); stj <- s0$phases[[j]]$status
            span(class = "me-3", tags$b(j), ": ", span(class = if (identical(stj, "approved")) "text-success" else "text-warning", stj),
                 if (isTRUE(a$ai)) span(class = "badge bg-info text-dark ms-1", if (isTRUE(a$unchanged)) "AI-drafted" else "AI-drafted, edited"),
                 sprintf(" \u00b7 %d words", count_words(phase_text(s0, j)))) })),
          if (length(stale)) div(class = "alert alert-warning small py-2", icon("triangle-exclamation"),
            sprintf(" Phase %s changed since you linked it. Read the new text, then accept it.", paste(stale, collapse = ", ")), " ",
            tags$a(href = "#", onclick = act(sid, "accept"), "Accept the current text")),
          if (length(s$links)) tags$details(class = "small mb-2", tags$summary("Show the linked text"),
            tags$pre(class = "no-i18n", style = "white-space:pre-wrap;max-height:40vh", paste(vapply(s$links, function(j) phase_text(s0, j), ""), collapse = "\n\n")))),
        if (s$kind == "timeline") textAreaInput(paste0("ps_", sid, "_ms"), "Milestones: one per line, as: name; start date; end date",
            paste(apply(pr$milestones, 1, function(r) paste(r, collapse = "; ")), collapse = "\n"), rows = 5, width = "100%",
            placeholder = "Expos\u00e9 to supervisor; 2026-11-15; 2026-11-15"),
        textAreaInput(paste0("ps_", sid, "_text"), if (s$kind == "appendix") "Text of the appendix (optional)" else if (length(s$links)) "Your own text after the linked text" else "Your text",
                      s$text, rows = if (s$kind %in% c("timeline", "appendix")) 3 else 7, width = "100%",
                      placeholder = "Markdown works: ## subheading, - bullet, **bold**, | tables |. Cite as (Author, 2021) or Author (2021)."),
        if (!s$kind %in% c("timeline", "appendix") && length(choices)) selectizeInput(paste0("ps_", sid, "_figs"), "Figures in this section", choices, s$figs, multiple = TRUE, width = "100%"),
        if (nrow(v)) tags$details(class = "small", tags$summary(icon("clock-rotate-left"), sprintf(" Versions of this text (%d)", nrow(v))),
          tags$table(class = "table table-sm mb-0", tags$tbody(lapply(rev(seq_len(nrow(v))), function(i) tags$tr(tags$td(v$time[i]), tags$td(v$source[i]),
            tags$td(sprintf("%d words", count_words(v$text[i]))), tags$td(class = "text-end", tags$a(href = "#", onclick = act(sid, "vrestore", which(s0$draft_versions$phase == sec_key(sid))[i]), "restore"))))))))
    }))
  })
  output$prop_add <- renderUI({ if (is.null(st$proposal)) return(NULL)
    div(class = "d-flex gap-2 align-items-end mb-4", selectInput("prop_add_kind", "Add a part", setNames(PROPOSAL_KINDS, vapply(PROPOSAL_KINDS, function(k) prop_words("en")$kinds[[k]], ""))),
        actionButton("prop_add_go", "Add", icon = icon("plus"), class = "mb-3")) })
  observeEvent(input$prop_add_go, { pr <- isolate(st$proposal); req(pr)
    pr$sections[[length(pr$sections) + 1]] <- prop_section(input$prop_add_kind, pr$lang); set_pr(pr); bump()
    log_decision("Proposal", "Section added", prop_words(pr$lang)$kinds[[input$prop_add_kind]]) })

  # every section's inputs, written back to the proposal
  observe({ struct(); if (is.null(st$proposal)) return(); pr <- isolate(st$proposal); changed <- FALSE; relink <- FALSE; s0 <- isolate(reactiveValuesToList(st))
    for (k in seq_along(pr$sections)) { s <- pr$sections[[k]]; sid <- s$id
      ti <- input[[paste0("ps_", sid, "_title")]]; tx <- input[[paste0("ps_", sid, "_text")]]; li <- input[[paste0("ps_", sid, "_links")]]
      fi <- input[[paste0("ps_", sid, "_figs")]]; ms <- input[[paste0("ps_", sid, "_ms")]]
      if (!is.null(ti) && !identical(ti, s$title)) { s$title <- ti; changed <- TRUE }
      if (!is.null(tx) && !identical(tx, s$text)) { s$text <- tx; changed <- TRUE; typed[[sec_key(sid)]] <- TRUE }
      if (!s$kind %in% c("timeline", "appendix") && !is.null(input[[paste0("ps_", sid, "_title")]])) { li <- li %||% character(0)
        if (!identical(sort(li), sort(s$links))) { add <- setdiff(li, s$links); s$links <- li[order(match(li, PROP_PHASES))]
          for (j in add) s$link_text[[j]] <- phase_text(s0, j); s$link_text <- s$link_text[intersect(names(s$link_text), s$links)]; changed <- TRUE; relink <- TRUE } }
      if (!is.null(ti) && !s$kind %in% c("timeline", "appendix") && length(fig_choices(pr))) { fi <- fi %||% character(0)
        if (!identical(fi, s$figs %||% character(0))) { s$figs <- fi; changed <- TRUE } }
      if (!is.null(ms) && s$kind == "timeline") { rows <- Filter(nzchar, trimws(strsplit(ms, "\n")[[1]]))
        m <- do.call(rbind, lapply(rows, function(r) { x <- trimws(strsplit(r, ";")[[1]]); x <- c(x, "", "")[1:3]; x }))
        m <- if (is.null(m)) data.frame(milestone = character(), start = character(), end = character(), stringsAsFactors = FALSE) else stats::setNames(as.data.frame(m, stringsAsFactors = FALSE), c("milestone", "start", "end"))
        if (!identical(m, pr$milestones)) { pr$milestones <- m; changed <- TRUE } }
      pr$sections[[k]] <- s }
    if (changed) { set_pr(pr); if (relink) bump() } })

  observeEvent(input$prop_act, { a <- input$prop_act; pr <- isolate(st$proposal); req(pr); s0 <- isolate(reactiveValuesToList(st))
    k <- match(a$sid, vapply(pr$sections, `[[`, "", "id"))
    switch(a$a,
      up = if (!is.na(k) && k > 1) { pr$sections[c(k - 1, k)] <- pr$sections[c(k, k - 1)]; set_pr(pr); bump() },
      down = if (!is.na(k) && k < length(pr$sections)) { pr$sections[c(k, k + 1)] <- pr$sections[c(k + 1, k)]; set_pr(pr); bump() },
      remove = if (!is.na(k)) showModal(modalDialog(title = "Remove this section?", pr$sections[[k]]$title, p(class = "small text-muted", "Its own text stays in the versions list."),
                 footer = tagList(modalButton("Keep it"), actionButton("prop_rm_go", "Remove", class = "btn-danger")))),
      accept = if (!is.na(k)) { s <- pr$sections[[k]]; stale <- prop_stale_links(s, s0); pr$sections[[k]] <- prop_link_now(s, s0); set_pr(pr); bump()
                 log_decision("Proposal", "Changed phase text accepted", sprintf("%s: phase %s", s$title, paste(stale, collapse = ", "))) },
      vrestore = if (!is.na(k)) { v <- s0$draft_versions[a$k, ]; if (identical(v$phase, sec_key(a$sid))) {
                 snap(sec_key(a$sid), "your text before a restore", pr$sections[[k]]$text); pr$sections[[k]]$text <- v$text; set_pr(pr)
                 updateTextAreaInput(session, paste0("ps_", a$sid, "_text"), value = v$text); snap(sec_key(a$sid), paste("restored version of", v$time), v$text)
                 log_decision("Proposal", "Earlier version restored", paste(pr$sections[[k]]$title, "-", v$time)); bump() } },
      rmatt = { att <- pr$attachments; i <- match(a$sid, vapply(att, `[[`, "", "id")); if (!is.na(i)) { log_decision("Proposal", "Attachment removed", att[[i]]$name)
                 pr$attachments <- att[-i]; for (j in seq_along(pr$sections)) pr$sections[[j]]$figs <- setdiff(pr$sections[[j]]$figs, a$sid); set_pr(pr); bump() } },
      notemplate = { pr$template <- NULL; set_pr(pr); bump(); log_decision("Proposal", "Word template removed", "") },
      reset = showModal(modalDialog(title = "Start again?", "The outline, your text in it, the timeline and the attachments are removed. The phases are not touched; the versions of your text stay in the project.",
                 footer = tagList(modalButton("Keep the proposal"), actionButton("prop_reset_go", "Start again", class = "btn-danger")))))
    if (identical(a$a, "remove")) rm_target(a$sid)
  })
  rm_target <- reactiveVal(NULL)
  observeEvent(input$prop_rm_go, { removeModal(); pr <- isolate(st$proposal); k <- match(rm_target(), vapply(pr$sections, `[[`, "", "id")); req(!is.na(k))
    snap(sec_key(pr$sections[[k]]$id), "your text before the section was removed", pr$sections[[k]]$text)
    log_decision("Proposal", "Section removed", pr$sections[[k]]$title); pr$sections <- pr$sections[-k]; set_pr(pr); bump() })
  observeEvent(input$prop_reset_go, { removeModal(); pr <- isolate(st$proposal)
    for (s in pr$sections) snap(sec_key(s$id), "your text before starting again", s$text)
    st$proposal <- NULL; log_decision("Proposal", "Proposal removed to start again", ""); bump() })

  # ---------- checks
  output$prop_check_card <- renderUI({ pr <- st$proposal; if (is.null(pr)) return(NULL); s0 <- reactiveValuesToList(st)
    cnt <- prop_counts(pr, s0); iss <- prop_checks(pr, s0); cc <- prop_check_citations(pr, s0); lim <- suppressWarnings(as.numeric(pr$word_limit))
    card(card_header("Checks"),
      tags$table(class = "table table-sm small mb-2", tags$tbody(lapply(seq_len(nrow(cnt)), function(i) tags$tr(tags$td(cnt$section[i]), tags$td(class = "text-end", sprintf("%d words", cnt$words[i])))),
        tags$tr(class = "fw-bold", tags$td("Total"), tags$td(class = "text-end", sprintf("%d words%s", sum(cnt$words), if (!is.na(lim) && lim > 0) sprintf(" of %d", as.integer(lim)) else ""))))),
      p(class = "small", sprintf("%d citations found; %d references will be listed.", nrow(cc$citations), length(cc$refs))),
      if (length(iss)) div(class = "alert alert-warning small py-2", tags$ul(class = "mb-0", lapply(iss, tags$li))) else
        div(class = "alert alert-success small py-2", icon("circle-check"), " Nothing missing."))
  })

  output$prop_dl_docx <- downloadHandler(function() paste0("ANCHOR_proposal_", gsub("[^A-Za-z0-9]+", "_", substr(isolate(st$proposal$title) %|e|% "untitled", 1, 40)), "_", Sys.Date(), ".docx"),
    function(file) { s0 <- isolate(reactiveValuesToList(st)); for (s in s0$proposal$sections) snap(sec_key(s$id), "your edits", s$text)
      build_proposal_docx(s0, file); log_decision("Proposal", "Proposal exported (Word)", paste(length(prop_checks(s0$proposal, s0)), "open check(s)")) })
  fname <- function(ext) paste0("ANCHOR_proposal_", gsub("[^A-Za-z0-9]+", "_", substr(isolate(st$proposal$title) %|e|% "untitled", 1, 40)), "_", Sys.Date(), ext)
  output$prop_dl_pdf <- downloadHandler(function() fname(".pdf"), function(file) { s0 <- isolate(reactiveValuesToList(st))
    tryCatch({ build_proposal_pdf(s0, file); log_decision("Proposal", "Proposal exported (PDF)", paste(length(prop_checks(s0$proposal, s0)), "open check(s)")) },
             error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 15); stop(e) }) })
  output$prop_dl_tex <- downloadHandler(function() fname("_LaTeX.zip"), function(file) { s0 <- isolate(reactiveValuesToList(st))
    build_proposal_latex_zip(s0, file); log_decision("Proposal", "Proposal exported (LaTeX)", paste(length(prop_checks(s0$proposal, s0)), "open check(s)")) })
}

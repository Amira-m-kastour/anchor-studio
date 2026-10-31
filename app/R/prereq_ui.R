# ANCHOR Studio - UI and server for "Before you start" and the 1.4.0 connector cards (phases A, C, H; Bibliometrics > Data)

prereq_ui <- function() div(class = "pt-2",
  div(class = "alert alert-light border small", icon("list-check"), tags$b(" Before you start. "),
      "Record who is doing the review, with what software, under which affiliation and with which data licences. The app checks what it can, phase O uses your database list ",
      "(for example, rating disagreement needs two raters), and everything here is printed as an appendix in your reports. Passwords, keys and email addresses are never stored."),
  layout_columns(col_widths = c(6, 6),
    card(fill = FALSE, card_header(icon("user"), " 1 \u00b7 Researcher"),
      textInput("pr_name", "Name as it should appear on outputs", width = "100%"),
      div(class = "d-flex gap-2 align-items-end", div(style = "flex:1", textInput("pr_orcid", "ORCID iD (optional)", placeholder = "0000-0000-0000-0000", width = "100%")),
          actionButton("pr_orcid_check", "Check", icon = icon("id-card"), class = "btn-sm mb-3")), uiOutput("pr_orcid_status"),
      layout_columns(fill = FALSE, selectInput("pr_role", "Role", c("", "Student (bachelor)", "Student (master)", "Doctoral researcher", "Researcher or staff", "Other"), width = "100%"),
                     textInput("pr_programme", "Programme or module", width = "100%")),
      textInput("pr_supervisor", "Supervisor or reviewer (optional)", width = "100%"),
      checkboxGroupInput("pr_skills", "I am prepared in", choices = PREREQ_SKILLS),
      p(class = "small text-muted", "Not yet? Each stage's 'Lessons learned' card lists checked references to prepare."),
      textAreaInput("pr_timeplan", "Time plan per phase (optional)", rows = 2, width = "100%")),
    card(fill = FALSE, card_header(icon("laptop-code"), " 2 \u00b7 Software"),
      uiOutput("pr_sw"), actionButton("pr_sw_check", "Check again", icon = icon("rotate"), class = "btn-sm mb-2"),
      checkboxGroupInput("pr_tools", "Outside tools I will also use (optional)", choices = c("LaTeX or Overleaf" = "latex", "RStudio" = "rstudio",
        "Python with the LSEG Data Library" = "python_lseg", "Stata" = "stata", "A reference manager (Zotero, Citavi, EndNote)" = "refman")))),
  layout_columns(col_widths = c(6, 6),
    card(fill = FALSE, card_header(icon("building-columns"), " 3 \u00b7 Academic affiliation"),
      layout_columns(fill = FALSE, col_widths = c(7, 5), selectInput("pr_country", "Region (country)", c("(choose)" = "", stats::setNames(names(COUNTRIES), unname(COUNTRIES))), width = "100%"),
        div(class = "pt-4", actionLink("pr_detect_net", "Detect from my network instead", class = "small"))),
      uiOutput("pr_country_how"),
      selectizeInput("pr_inst", "Institution", choices = NULL, width = "100%", options = list(placeholder = "Choose from the institutions in this region")),
      textInput("pr_inst_q", NULL, placeholder = "Not in the list? Search all institutions in this region by name", width = "100%"),
      uiOutput("pr_inst_how"),
      div(class = "d-flex gap-2 align-items-end", div(style = "flex:1", textInput("pr_domain", "Or detect from your institution's email domain", placeholder = "e.g. name@university.edu (only the part after @ is used)", width = "100%")),
          actionButton("pr_domain_go", "Detect", icon = icon("magnifying-glass"), class = "btn-sm mb-3")),
      textInput("pr_inst_other", "Not listed? Type the institution's name", width = "100%"),
      layout_columns(fill = FALSE, textInput("pr_faculty", "Faculty or department", width = "100%"), textInput("pr_access_route", "Library access route", placeholder = "e.g. VPN, campus network, proxy", width = "100%")),
      textInput("pr_resolver", "Library link for articles (optional)", placeholder = "your library's proxy or link-resolver prefix, e.g. https://proxy.example.edu/login?url=", width = "100%"),
      uiOutput("pr_resolver_note"),
      checkboxGroupInput("pr_rules", "Institutional rules I have read", choices = PREREQ_RULES),
      textAreaInput("pr_rule_links", "Links to those rules (optional)", rows = 2, width = "100%"),
      radioButtons("pr_ethics", "Ethics approval", choices = ETHICS, selected = character(0))),
    card(fill = FALSE, card_header(icon("scale-balanced"), " 4 \u00b7 Databases and licensing"),
      p(class = "small text-muted", "Tick what you can actually use. The app pre-ticks what your library is known to license and detects what it can; you confirm."),
      div(class = "d-flex gap-2 mb-2 flex-wrap", actionButton("pr_db_detect", "Detect", icon = icon("satellite-dish"), class = "btn-sm"),
          actionButton("pr_db_remember", "Remember this list for my institution", icon = icon("bookmark"), class = "btn-sm btn-outline-secondary"), uiOutput("pr_db_note", inline = TRUE)),
      uiOutput("pr_db_ui"), uiOutput("pr_raters"),
      layout_columns(fill = FALSE, selectInput("pr_out_lic", "Licence for my report", OUTPUT_LICENCES, width = "100%"), selectInput("pr_code_lic", "Licence for my analysis code", CODE_LICENCES, width = "100%")),
      tags$details(tags$summary(class = "small", "What may be shared"),
        tags$ul(class = "small mb-0",
          tags$li("ANCHOR Studio is GPL-3; bundled software keeps its own licences (see NOTICE). The tribute portrait is \u00a9 the Kastour family and not under GPL-3."),
          tags$li("Crossref, OpenAlex and ROR metadata are CC0; World Bank data CC BY 4.0; FRED, French library, GDELT, IMF and SEC data under their own terms (cite them)."),
          tags$li("Licensed data (LSEG, WRDS, FactSet, Bloomberg, MSCI, Sustainalytics) and the published article PDF are never put in files the app makes for sharing."),
          tags$li("These are short summaries, not legal advice; follow each provider's terms."))))),
  div(class = "d-flex gap-2 align-items-center mb-4", actionButton("pr_done", "Save and continue to Start", icon = icon("arrow-right"), class = "btn-primary"), uiOutput("pr_saved", inline = TRUE)))

oa_card_A <- function() card(card_header(icon("diagram-project"), " Focal article in OpenAlex: references, citing works, open copy, journal"),
  p(class = "small text-muted", "Free, live (OpenAlex, CC0 metadata). Only the DOI is sent. Open copies are listed as links; the app never stores the article PDF in files for sharing."),
  div(class = "d-flex gap-2 flex-wrap", actionButton("a_oa_fetch", "Fetch", icon = icon("cloud-arrow-down"), class = "btn-sm btn-primary"),
      actionButton("a_oa_seed", "Use references + citing works as the phase N corpus", icon = icon("arrow-right"), class = "btn-sm")),
  uiOutput("a_oa_summary"),
  navset_underline(nav_panel("References", DT::DTOutput("a_oa_refs")), nav_panel("Citing works (most cited first)", DT::DTOutput("a_oa_citing")), nav_panel("Journal", uiOutput("a_oa_journal"))))

oa_card_H <- function() card(card_header(icon("chart-line"), " Research-front trend (OpenAlex)"),
  layout_columns(col_widths = c(6, 3, 3), textInput("h_terms", "Construct terms", "ESG rating disagreement", width = "100%"),
                 numericInput("h_from", "From", 2000), numericInput("h_to", "To", as.integer(format(Sys.Date(), "%Y")))),
  div(class = "d-flex gap-2", actionButton("h_trend_go", "Show trend", icon = icon("chart-line"), class = "btn-sm btn-primary"), actionButton("h_trend_add", "Add to report (phase H)", icon = icon("plus"), class = "btn-sm")),
  plotOutput("h_trend_plot", height = "300px"), p(class = "small text-muted", "Counts of papers matching the terms per year in OpenAlex; use them to argue whether the question is current, not as evidence for the hypotheses."))

oa_card_C <- function() card(card_header(icon("unlock"), " Open copies of the records in use (OpenAlex)"),
  p(class = "small text-muted", "Finds legal open-access copies for screening (up to 300 most-cited records with a DOI). Links only; paywalled papers: use your library."),
  actionButton("c_oa_copies", "Find open copies", icon = icon("magnifying-glass"), class = "btn-sm"), DT::DTOutput("c_oa_table"))

bib_oa_panel <- function() conditionalPanel("input.bib_source == 'openalex'",
  radioButtons("bib_oa_mode", "Records", choices = c("Search query" = "search", "Works citing the focal article" = "citing", "The focal article's references" = "refs", "Citing works + references" = "both")),
  conditionalPanel("input.bib_oa_mode == 'search'", textInput("bib_oa_query", "Search (titles, abstracts, full text index)", "ESG rating disagreement stock returns")),
  layout_columns(numericInput("bib_oa_from", "From", 2000), numericInput("bib_oa_to", "To", as.integer(format(Sys.Date(), "%Y")))),
  checkboxGroupInput("bib_oa_types", "Document types", c("article", "review", "book-chapter", "preprint"), selected = c("article", "review"), inline = TRUE),
  layout_columns(selectInput("bib_oa_lang", "Language", c("Any" = "", "English" = "en", "German" = "de", "Arabic" = "ar", "French" = "fr")), numericInput("bib_oa_max", "Max records", 500, min = 50, max = 5000, step = 50)),
  p(class = "small text-muted", "Free and live (OpenAlex, CC0). With no API key the daily free allowance is small; a free key (Settings) covers several thousand records a day."))

db_badges <- function(id, pr, det) { b <- list()
  if (id %in% (det$library %||% character(0))) b <- c(b, list(span(class = "badge text-bg-light border ms-1", "listed by your library")))
  if (id %in% (det$running %||% character(0))) b <- c(b, list(span(class = "badge text-bg-success ms-1", "running on this computer")))
  if (id %in% (det$used %||% character(0))) b <- c(b, list(span(class = "badge text-bg-info ms-1", "used in this project")))
  b }

prereq_server <- function(input, output, session, st, rv, log_decision) {
  det <- reactiveVal(list()); ror_list <- reactiveVal(NULL); known <- reactiveVal(NULL); sw <- reactiveVal(NULL); geo <- reactiveVal(detect_country_tz())
  cfg <- function() online_cfg(isolate(st$settings))
  online_on <- function() !isFALSE(isolate(input$online))
  pr_get <- function() utils::modifyList(new_prereq(), isolate(st$prereq) %||% list())
  pr_set <- function(...) { p <- pr_get(); v <- list(...); for (k in names(v)) p[[k]] <- v[[k]]; st$prereq <- p }

  # restore inputs from the project
  fill <- function() isolate({ p <- pr_get()
    updateTextInput(session, "pr_name", value = p$name %|e|% (isolate(st$setup$S_name) %||% "")); updateTextInput(session, "pr_orcid", value = p$orcid)
    updateSelectInput(session, "pr_role", selected = p$role); updateTextInput(session, "pr_programme", value = p$programme); updateTextInput(session, "pr_supervisor", value = p$supervisor)
    updateCheckboxGroupInput(session, "pr_skills", selected = p$skills); updateTextAreaInput(session, "pr_timeplan", value = p$timeplan)
    updateCheckboxGroupInput(session, "pr_tools", selected = p$tools); updateTextInput(session, "pr_faculty", value = p$faculty); updateTextInput(session, "pr_access_route", value = p$access_route); updateTextInput(session, "pr_resolver", value = p$resolver %||% "")
    updateCheckboxGroupInput(session, "pr_rules", selected = p$rules); updateTextAreaInput(session, "pr_rule_links", value = p$rule_links)
    if (nzchar(p$ethics)) updateRadioButtons(session, "pr_ethics", selected = p$ethics)
    updateSelectInput(session, "pr_out_lic", selected = p$output_licence); updateSelectInput(session, "pr_code_lic", selected = p$code_licence)
    cc <- p$country %|e|% (geo()$cc %||% ""); if (!is.na(cc) && nzchar(cc)) updateSelectInput(session, "pr_country", selected = cc)
    if (nzchar(p$institution) && !nzchar(p$ror_id)) updateTextInput(session, "pr_inst_other", value = p$institution)
    if (nzchar(p$ror_id)) updateSelectizeInput(session, "pr_inst", choices = stats::setNames(p$ror_id, p$institution), selected = p$ror_id)
    updateCheckboxGroupInput(session, "pr_dbs", selected = p$dbs %||% character(0)); detect_dbs(FALSE) })
  session$onFlushed(fill, once = TRUE)
  observeEvent(rv$reload, fill(), ignoreInit = TRUE)

  # keep the record in sync with the inputs
  for (k in c("pr_name", "pr_orcid", "pr_role", "pr_programme", "pr_supervisor", "pr_timeplan", "pr_faculty", "pr_access_route", "pr_resolver", "pr_rule_links", "pr_ethics", "pr_out_lic", "pr_code_lic"))
    local({ k <- k; observeEvent(input[[k]], { f <- c(pr_name = "name", pr_orcid = "orcid", pr_role = "role", pr_programme = "programme", pr_supervisor = "supervisor", pr_timeplan = "timeplan",
      pr_faculty = "faculty", pr_access_route = "access_route", pr_resolver = "resolver", pr_rule_links = "rule_links", pr_ethics = "ethics", pr_out_lic = "output_licence", pr_code_lic = "code_licence")[[k]]
      v <- input[[k]] %||% ""; if (k == "pr_orcid") v <- toupper(gsub("^https?://orcid.org/", "", trimws(v))); do.call(pr_set, stats::setNames(list(v), f)) }, ignoreInit = TRUE) })
  for (k in c("pr_skills", "pr_tools", "pr_rules", "pr_dbs")) local({ k <- k; observeEvent(input[[k]], { f <- c(pr_skills = "skills", pr_tools = "tools", pr_rules = "rules", pr_dbs = "dbs")[[k]]
    do.call(pr_set, stats::setNames(list(input[[k]] %||% character(0)), f)) }, ignoreInit = TRUE, ignoreNULL = FALSE) })
  observeEvent(input$pr_name, if (nzchar(input$pr_name %||% "") && !identical(input$pr_name, input$S_name)) updateTextInput(session, "S_name", value = input$pr_name), ignoreInit = TRUE)
  observeEvent(input$S_name, if (nzchar(input$S_name %||% "") && !nzchar(input$pr_name %||% "")) updateTextInput(session, "pr_name", value = input$S_name), ignoreInit = TRUE)
  observeEvent(input$S_inst, if (nzchar(input$S_inst %||% "") && !nzchar(isolate(pr_get()$institution))) updateTextInput(session, "pr_inst_other", value = input$S_inst), ignoreInit = TRUE)

  # ORCID
  output$pr_orcid_status <- renderUI({ p <- st$prereq; id <- p$orcid %||% ""; if (!nzchar(id)) return(NULL)
    if (!orcid_ok(id)) return(p(class = "small text-danger", icon("triangle-exclamation"), " This is not a valid ORCID iD (format or check digit)."))
    p(class = "small text-success", icon("check"), paste0(" Valid ORCID iD", if (nzchar(p$orcid_name %||% "")) paste0(" (public record: ", p$orcid_name, ")") else "")) })
  observeEvent(input$pr_orcid_check, { id <- pr_get()$orcid; if (!orcid_ok(id)) return(showNotification("Enter a valid ORCID iD first.", type = "warning"))
    if (!online_on()) return(showNotification("The format is valid. Switch on internet features to look up the public record.", type = "message"))
    nm <- tryCatch(orcid_name(id, cfg()), error = function(e) { showNotification(conditionMessage(e), type = "error"); "" }); pr_set(orcid_name = nm) })

  # software
  run_sw <- function() isolate(sw(software_checks(isolate(st$settings) %||% list(), if (!is.null(isolate(st$online))) isTRUE(isolate(st$online)$online) else NA)))
  session$onFlushed(run_sw, once = TRUE); observeEvent(input$pr_sw_check, run_sw())
  observeEvent(list(st$settings, st$online), run_sw(), ignoreInit = TRUE)
  output$pr_sw <- renderUI({ d <- sw(); req(d); col <- c(ok = "success", missing = "danger", offline = "warning", off = "secondary", optional = "light border", `not checked` = "light border")
    tags$table(class = "table table-sm small", lapply(seq_len(nrow(d)), function(i) tags$tr(tags$td(d$Item[i]), tags$td(span(class = paste0("badge text-bg-", col[[d$Status[i]]] %||% "light"), d$Status[i])), tags$td(class = "text-muted", d$Detail[i])))) })

  # region and institution
  output$pr_country_how <- renderUI({ g <- geo(); cc <- input$pr_country %||% ""
    if (!nzchar(cc)) return(p(class = "small text-muted", "Region ", g$how %||% "", ". Choose it to list institutions."))
    p(class = "small text-muted", sprintf("Region %s %s.", unname(COUNTRIES[cc]) %||% cc, if (identical(cc, g$cc)) g$how else "(chosen by you)")) })
  observeEvent(input$pr_detect_net, { if (!online_on()) return(showNotification("Switch on internet features first.", type = "warning"))
    g <- tryCatch(detect_country_net(cfg()), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }); if (is.null(g)) return()
    geo(g); updateSelectInput(session, "pr_country", selected = g$cc) })
  observeEvent(input$pr_country, { cc <- input$pr_country %||% ""; if (!nzchar(cc)) return(); pr_set(country = cc, country_how = if (identical(cc, geo()$cc)) geo()$how else "chosen")
    if (!online_on()) { if (file.exists(ror_cache_file(cc))) ror_list(readRDS(ror_cache_file(cc))) else ror_list(NULL); return() }
    d <- withProgress(message = sprintf("Loading institutions in %s (ROR)...", unname(COUNTRIES[cc]) %||% cc), value = .5, tryCatch(ror_country(cc, cfg()), error = function(e) NULL))
    ror_list(d); remember(d); offer(d) }, ignoreInit = TRUE)
  observeEvent(input$online, { cc <- input$pr_country %||% ""; if (isTRUE(input$online) && nzchar(cc) && is.null(ror_list())) {
      d <- tryCatch(ror_country(cc, cfg()), error = function(e) NULL); ror_list(d); remember(d); offer(d) } }, ignoreInit = TRUE)
  remember <- function(d) isolate({ if (is.null(d) || !nrow(d)) return(); k <- known(); k <- if (is.null(k)) d else rbind(d, k); known(k[!duplicated(k$id), ]) })
  offer <- function(d) isolate({ p <- pr_get(); keep <- if (nzchar(p$ror_id)) stats::setNames(p$ror_id, p$institution) else NULL
    ch <- if (!is.null(d) && nrow(d)) stats::setNames(d$id, sprintf("%s%s", d$name, ifelse(nzchar(d$city), paste0(" (", d$city, ")"), ""))) else NULL
    updateSelectizeInput(session, "pr_inst", choices = c(keep, ch[!ch %in% keep]), selected = if (nzchar(p$ror_id)) p$ror_id else character(0), server = TRUE) })
  # type-ahead search within the region (ROR recommends ?query= for forms); debounced so typing sends few requests
  inst_q <- debounce(reactive(trimws(input$pr_inst_q %||% "")), 700)
  observeEvent(inst_q(), { q <- inst_q(); if (nchar(q) < 3) return(); if (!online_on()) return(showNotification("Searching needs the internet features switched on.", type = "warning"))
    r <- tryCatch(ror_search(q, cfg(), input$pr_country %||% ""), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }); if (is.null(r)) return()
    rv_search(nrow(r)); remember(r); offer(r) }, ignoreInit = TRUE)
  rv_search <- reactiveVal(NA)
  output$pr_inst_how <- renderUI({ d <- ror_list(); cc <- input$pr_country %||% ""; if (!nzchar(cc)) return(NULL)
    n_s <- rv_search(); tot <- attr(d, "total")
    if (is.null(d)) return(p(class = "small text-muted", "Institution list not loaded (offline or not reachable). Search by name when online, or type the name below."))
    p(class = "small text-muted", if (!is.na(n_s)) sprintf("%d match%s for your search. ", n_s, if (n_s == 1) "" else "es") else "",
      if (!is.null(tot) && !is.na(tot) && tot > nrow(d)) sprintf("The list shows %d of %d institutions in this region; search by name to find the others. ", nrow(d), tot) else sprintf("%d institution%s in this region. ", nrow(d), if (nrow(d) == 1) "" else "s"),
      "Source: Research Organization Registry (ROR, CC0).") })
  choose_inst <- function(id, name, domains, how, country = "") isolate({ pr_set(ror_id = id, institution = name, domains = domains, inst_country = country %||% "")
    updateTextInput(session, "S_inst", value = name)
    r <- institution_dbs(name, domains, id); d0 <- det(); d0$library <- NULL; d0$source <- NULL; det(d0)
    if (!is.null(r)) { d <- det(); d$library <- r$dbs; d$source <- r$source; det(d)
      if (!length(pr_get()$dbs)) { pr_set(dbs = r$dbs, db_source = r$source); updateCheckboxGroupInput(session, "pr_dbs", selected = r$dbs) }
      if (!nzchar(pr_get()$access_route)) updateTextInput(session, "pr_access_route", value = r$access) }
    log_decision("Setup", "Affiliation", sprintf("%s (%s) %s", name, id, how)) })
  observeEvent(input$pr_inst, { id <- input$pr_inst %||% ""; if (!nzchar(id) || identical(id, pr_get()$ror_id)) return()
    d <- known(); r <- if (!is.null(d)) d[d$id == id, ] else NULL; if (is.null(r) || !nrow(r)) return()
    choose_inst(id, r$name[1], r$domains[1], "chosen from the list", r$country[1]) }, ignoreInit = TRUE)
  observeEvent(input$pr_domain_go, { if (!online_on()) return(showNotification("Switch on internet features first.", type = "warning"))
    r <- tryCatch(ror_by_domain(input$pr_domain, cfg()), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }); if (is.null(r)) return()
    remember(r); updateSelectizeInput(session, "pr_inst", choices = stats::setNames(r$id, r$name), selected = r$id)
    if (nzchar(r$country) && !identical(r$country, input$pr_country)) showNotification(sprintf("%s is in %s; your region stays %s. Both are recorded.", r$name, unname(COUNTRIES[r$country]) %||% r$country, unname(COUNTRIES[input$pr_country %||% ""]) %||% "unset"), type = "message", duration = 10)
    choose_inst(r$id, r$name, r$domains, sprintf("detected from the email domain %s", sub(".*@", "", input$pr_domain)), r$country)
    showNotification(sprintf("Detected: %s.", r$name), type = "message") })
  observeEvent(input$pr_inst_other, { v <- trimws(input$pr_inst_other %||% ""); if (!nzchar(v) || identical(v, isolate(pr_get()$institution))) return(); pr_set(institution = v, ror_id = "", domains = "", inst_country = "")
    updateTextInput(session, "S_inst", value = v); r <- institution_dbs(v, ""); if (!is.null(r)) { d <- det(); d$library <- r$dbs; d$source <- r$source; det(d) } }, ignoreInit = TRUE)

  # databases: library list + local detection + use in this project; the researcher confirms
  detect_dbs <- function(probe = TRUE) isolate({ p <- pr_get(); d <- det()
    r <- institution_dbs(p$institution, p$domains, p$ror_id); d$library <- r$dbs; d$source <- r$source
    if (probe) d$running <- if (isTRUE(probe_lseg())) "lseg_ws" else character(0)
    src <- isolate(st$biblio$log$database) %||% character(0); d$used <- c(if (any(src == "wos")) "wos", if (any(src == "scopus")) "scopus")
    det(d); d })
  session$onFlushed(function() detect_dbs(FALSE), once = TRUE)
  observeEvent(input$pr_db_detect, { d <- detect_dbs(TRUE); p <- pr_get(); add <- unique(c(d$library, d$running, d$used))
    sel <- unique(c(p$dbs, add)); updateCheckboxGroupInput(session, "pr_dbs", selected = sel); pr_set(dbs = sel, db_source = d$source %||% "")
    showNotification(sprintf("%d databases suggested from your library list and this computer. Untick any you cannot use.", length(add)), type = "message") })
  observeEvent(input$pr_db_remember, { p <- pr_get(); if (!nzchar(p$institution)) return(showNotification("Choose your institution first.", type = "warning"))
    if (!length(p$dbs)) return(showNotification("Tick the databases you can use first.", type = "warning"))
    save_institution_dbs(p$institution, p$ror_id, p$domains, p$dbs, p$access_route); d <- det(); d$library <- p$dbs; d$source <- "your saved list on this computer"; det(d)
    showNotification(sprintf("Saved: new projects with %s will start with these %d databases ticked.", p$institution, length(p$dbs)), type = "message") })
  output$pr_db_note <- renderUI({ d <- det(); if (!is.null(d$source)) span(class = "small text-muted", "Library list: ", d$source) else span(class = "small text-muted", "No library list known for your institution: tick what you can use.") })
  output$pr_db_ui <- renderUI({ d <- det(); p <- isolate(pr_get())
    checkboxGroupInput("pr_dbs", NULL, selected = p$dbs, choiceValues = DB_CATALOG$id,
      choiceNames = lapply(seq_len(nrow(DB_CATALOG)), function(i) span(DB_CATALOG$name[i], span(class = "small text-muted", paste0(" \u00b7 ", DB_CATALOG$licence[i])), db_badges(DB_CATALOG$id[i], p, d)))) })
  output$pr_raters <- renderUI({ ra <- raters_available(st$prereq %||% list())
    cls <- if (length(ra) >= 2) "alert-success" else "alert-warning"
    div(class = paste("alert small", cls), tags$b(sprintf("ESG rating families available: %d", length(ra))), if (length(ra)) paste0(" (", paste(ra, collapse = ", "), ")."),
        if (length(ra) < 2) " The anchor article's rating disagreement needs at least two raters. Phase O offers three routes: a hand-collected second rater, LSEG pillar dispersion as a proxy, or consensus only.") })
  observeEvent(input$pr_done, { pr_set(checked_at = format(Sys.time(), "%Y-%m-%d %H:%M")); log_decision("Setup", "Before you start saved", sprintf("%d databases, %d rating families", length(pr_get()$dbs), length(raters_available(pr_get()))))
    nav_select("nav", "start") })
  output$pr_resolver_note <- renderUI({ v <- trimws(st$prereq$resolver %||% ""); if (!nzchar(v)) return(NULL)
    if (!grepl("^https?://[^ ]+$", v)) return(p(class = "small text-danger", "This should be a web address starting with https://, as your library gives it (ask the library for its proxy or link-resolver prefix)."))
    p(class = "small text-muted", "Articles will open as: ", tags$code(paste0(v, "https://doi.org/10.1000/example"))) })
  output$pr_saved <- renderUI({ t <- st$prereq$checked_at %||% ""; if (nzchar(t)) span(class = "small text-muted", "Saved ", t) })
}

connectors_server <- function(input, output, session, st, rv, set_biblio, log_decision, add_report_item) {
  cfg <- function() online_cfg(isolate(st$settings)); need_online <- function() { if (isFALSE(isolate(input$online))) { showNotification("Switch on the internet features in Settings first.", type = "warning"); FALSE } else TRUE }
  # phase A
  observeEvent(input$a_oa_fetch, { if (!need_online()) return()
    f <- withProgress(message = "Fetching from OpenAlex...", value = .5, tryCatch(focal_links(isolate(st$article$doi) %||% isolate(input$art_doi), cfg()), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL }))
    if (is.null(f)) return(); st$focal_oa <- f; log_decision("A", "Focal article in OpenAlex", sprintf("%d references, cited by %s", f$n_refs, f$n_citing)) })
  output$a_oa_summary <- renderUI({ f <- st$focal_oa; if (is.null(f)) return(p(class = "small text-muted mt-2", "Not fetched yet."))
    div(class = "mt-2 small", tags$b(sprintf("%d references \u00b7 cited by %s works \u00b7 checked %s. ", f$n_refs, f$n_citing, f$retrieved)),
        if (!is.null(f$open)) tagList("Open copy: ", tags$a(href = f$open$url, target = "_blank", f$open$host %|e|% "link"), sprintf(" (%s%s).", f$open$version, if (nzchar(f$open$license)) paste(",", f$open$license) else ""))
        else "No open copy found: read it through your library.") })
  link_dt <- function(d) { if (is.null(d) || !nrow(d)) return(DT::datatable(data.frame(Note = "None"), rownames = FALSE, options = list(dom = "t")))
    d$open_copy <- ifelse(nzchar(d$open_copy), sprintf("<a href='%s' target='_blank'>open copy</a>", htmltools::htmlEscape(d$open_copy)), "")
    DT::datatable(d, rownames = FALSE, escape = setdiff(names(d), "open_copy"), options = list(pageLength = 8, scrollX = TRUE)) }
  output$a_oa_refs <- DT::renderDT(link_dt(st$focal_oa$refs)); output$a_oa_citing <- DT::renderDT(link_dt(st$focal_oa$citing))
  output$a_oa_journal <- renderUI({ j <- st$focal_oa$journal; if (is.null(j)) return(p(class = "small text-muted", "No journal record.")); HTML(html_df(j)) })
  fetch_corpus <- function(mode, query = "", from = NULL, to = NULL, types = c("article", "review"), lang = "", max = 500) {
    withProgress(message = "Fetching records from OpenAlex...", value = .1, {
      r <- tryCatch(oa_corpus(mode, cfg(), query, isolate(st$article$doi) %||% isolate(input$art_doi), from, to, types, lang, max,
                              progress = function(n, found) incProgress(0, detail = sprintf("%d of %s", n, found))), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 15); NULL })
      if (is.null(r)) return(invisible(NULL))
      set_biblio(r$M, r$log, r$dup, "openalex")
      log_decision("N", "Corpus fetched from OpenAlex", sprintf("mode %s; %s; %d records (%d found)%s", mode, if (mode == "search") paste("query:", query) else "focal article", nrow(r$M), r$found, if (nzchar(r$note)) paste0("; ", r$note) else ""))
      showNotification(sprintf("%d records ready for analysis.%s", nrow(r$M), if (nzchar(r$note)) paste0(" ", r$note) else ""), type = "message", duration = 12); r }) }
  observeEvent(input$a_oa_seed, { if (!need_online()) return(); if (!is.null(fetch_corpus("both", max = 1000))) nav_select("nav", "bib_data") })
  # phase C
  output$c_oa_table <- DT::renderDT(link_dt(rv$c_copies))
  observeEvent(input$c_oa_copies, { if (!need_online()) return(); M <- isolate(st$biblio$M); if (is.null(M)) return(showNotification("Import or fetch records under Bibliometrics > Data first.", type = "warning"))
    M <- M[order(-M$TC), ]; dois <- utils::head(M$DI[!is.na(M$DI) & nzchar(M$DI)], 300)
    rv$c_copies <- withProgress(message = "Looking for open copies...", value = .5, tryCatch(oa_open_copies(dois, cfg()), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })) })
  # phase H
  observeEvent(input$h_trend_go, { if (!need_online()) return()
    rv$trend <- withProgress(message = "Counting papers per year...", value = .5, tryCatch(oa_trend(input$h_terms, cfg(), input$h_from, input$h_to), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })) })
  output$h_trend_plot <- renderPlot({ d <- rv$trend; req(d); trend_plot(d)() }, res = 96)
  observeEvent(input$h_trend_add, { d <- rv$trend; if (is.null(d)) return(showNotification("Show the trend first.", type = "warning"))
    add_report_item(sprintf("Research-front trend: papers per year on \"%s\" (OpenAlex, %s)", d$terms[1], format(Sys.Date(), "%d %b %Y")), trend_plot(d), d[, c("year", "papers")], "H",
                    sprintf("library(openalexR)\n# papers per year in OpenAlex for the construct terms\noa_fetch(entity = \"works\", search = %s, publication_year = \"%d-%d\", group_by = \"publication_year\")", qcode(d$terms[1]), min(d$year), max(d$year))) })
  list(fetch_corpus = fetch_corpus)
}

# Windows: load the bundled GCC runtime DLLs by full path before any package, so that a copy of libstdc++-6.dll or
# libgcc_s_seh-1.dll installed by another program in C:\Windows\System32 (searched before PATH) cannot be picked instead.
if (.Platform$OS.type == "windows") local({
  for (b in unique(c(file.path(R.home(), "..", "..", "Library", "bin"), file.path(R.home(), "..", "..", "Library", "mingw-w64", "bin")))) {
    if (!dir.exists(b)) next
    for (f in c("libwinpthread-1.dll", "libgcc_s_seh-1.dll", "libstdc++-6.dll", "libgomp-1.dll", "libquadmath-0.dll", "libgfortran-5.dll")) {
      p <- normalizePath(file.path(b, f), winslash = "\\", mustWork = FALSE)
      if (file.exists(p) && !any(vapply(getLoadedDLLs(), function(d) identical(tolower(d[["path"]]), tolower(p)), TRUE))) try(dyn.load(p, local = FALSE, now = TRUE), silent = TRUE)
    }
  }
})

# ANCHOR Studio
# Article-anchored literature reviews with decision gates, built on the ANCHOR
# framework (Kastour, 2026) and the bibliometrix R package (Aria & Cuccurullo, 2017).
# Start it with the launcher for your system, or in R:  shiny::runApp("path/to/ANCHOR_Studio")

suppressPackageStartupMessages({
  library(shiny); library(bslib)
  library(bibliometrix)  # attached: some bibliometrix functions load internal data sets
})
options(shiny.maxRequestSize = 300 * 1024^2)
# text with accents, Greek or dashes must stay UTF-8 even where the system locale is not (some Windows set-ups, C locale)
if (!isTRUE(l10n_info()[["UTF-8"]])) for (loc in c("C.UTF-8", "en_US.UTF-8", ".UTF-8", "English_United States.utf8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f, local = FALSE, encoding = "UTF-8")

PHASE_IDS <- names(PHASES)
register_fonts()
# On Windows, draw with Windows' own graphics (GDI), which always finds the system fonts
if (.Platform$OS.type == "windows") options(shiny.useragg = FALSE)

# ------------------------------------------------------------------ state
new_state <- function() {
  list(
    settings = list(mode = "prompt", api_key = "", api_model = "claude-sonnet-5-5", ollama_url = "http://localhost:11434",
                    ollama_model = "llama3.1", include_text = TRUE, online = TRUE, crossref_mailto = "", openalex_key = "", fred_key = ""),
    setup = list(), setup_ok = FALSE,
    article = list(),
    phases = setNames(lapply(PHASE_IDS, function(i) list(status = "locked", draft = "", answers = list(), checks = character(0),
                                                      notes = "", ai_q = character(0), ai_a = list(), approved_at = NULL,
                                                      my_lessons = "", my_wiifm = "", accepted = list())), PHASE_IDS),
    decisions = data.frame(time = character(), phase = character(), decision = character(), note = character(), stringsAsFactors = FALSE),
    prompts = data.frame(time = character(), phase = character(), mode = character(), prompt = character(), stringsAsFactors = FALSE),
    biblio = list(M = NULL, M_all = NULL, R = NULL, log = NULL, dup = 0, source = ""),
    prisma = NULL, report_items = list(),
    online = NULL, online_ds = NULL,
    analysis = list(datasets = list(), results = list(), include_data = FALSE),
    prereq = new_prereq(), focal_oa = NULL, finder = NULL
  )
}

# ------------------------------------------------------------------ UI helpers
tribute_block <- function() div(class = "tribute", tags$img(src = "tribute.png", alt = "Portrait of Mamdouh Kastour", width = 96, height = 96),
  div(p(class = "tribute-cap mb-1", tags$em("In tribute to my father,"), tags$br(), tags$b("Mamdouh Kastour"), " (1950\u20132020)"),
      p(class = "small text-muted mb-0", "Photo: Kastour family. All rights reserved; not covered by the app's GPL-3 licence.")))
q_input <- function(q) {
  switch(q$type,
    radio = radioButtons(q$id, q$label, choices = q$choices, selected = character(0)),
    checkbox = checkboxGroupInput(q$id, q$label, choices = q$choices),
    textarea = textAreaInput(q$id, q$label, rows = 4, width = "100%"),
    textInput(q$id, q$label, width = "100%"))
}

phase_shell <- function(p, extra = NULL) {
  id <- p$id
  layout_columns(col_widths = c(8, 4),
    div(
      div(class = "phase-head", span(class = "phase-letter", id),
          div(h2(p$name), p(class = "phase-q", p$question))),
      uiOutput(paste0(id, "_banner")),
      card(card_header("What this phase needs"),
           layout_columns(col_widths = c(4, 4, 4),
             div(h6("Input"), p(p$input)), div(h6("Actions"), p(p$actions)), div(h6("Output"), p(p$output)))),
      card(card_header(class = "d-flex justify-content-between align-items-center",
                       span("Draft of this phase"),
                       div(actionButton(paste0(id, "_draft_btn"), "Draft this phase", icon = icon("wand-magic-sparkles"), class = "btn-primary btn-sm"),
                           actionButton(paste0(id, "_prompt_btn"), "Show prompt", icon = icon("clipboard"), class = "btn-outline-secondary btn-sm"))),
           uiOutput(paste0(id, "_modeinfo")),
           textAreaInput(paste0(id, "_draft"), NULL, rows = 22, width = "100%",
                         placeholder = "The draft appears here. Edit it freely: this text is what goes into your report. In copy-and-paste mode, paste Claude's answer here.")),
      extra,
      lessons_card(id)
    ),
    div(
      card(card_header("Questions for you"),
           p(class = "small text-muted", "Your answers steer the draft and are required before the gate opens."),
           tagList(lapply(p$questions, q_input)),
           uiOutput(paste0(id, "_aiq"))),
      card(card_header("Quality controls"), tags$ul(class = "small", lapply(p$controls, function(k) tags$li(tags$b(k), " ", CONTROLS[[k]])))),
      card(class = "gate-card", card_header(paste("Gate", id)),
           checkboxGroupInput(paste0(id, "_checks"), NULL, choices = p$checks),
           textAreaInput(paste0(id, "_notes"), "Notes and open items", rows = 3, width = "100%"),
           div(class = "d-grid gap-2",
               actionButton(paste0(id, "_approve"), "Approve and open the next phase", icon = icon("check"), class = "btn-success"),
               actionButton(paste0(id, "_revise"), "Ask for a revision", icon = icon("rotate"), class = "btn-outline-warning"),
               actionButton(paste0(id, "_reopen"), "Reopen this phase", icon = icon("lock-open"), class = "btn-outline-secondary btn-sm")))
    )
  )
}

analysis_ui <- function(group) {
  ns <- NS(paste0("an_", gsub("[^a-z]", "", tolower(group))))
  ids <- names(Filter(function(a) a$group == group, ANALYSES))
  labels <- vapply(ANALYSES[ids], `[[`, "", "label")
  layout_sidebar(
    sidebar = sidebar(width = 300,
      selectInput(ns("which"), "Analysis", choices = setNames(ids, labels)),
      uiOutput(ns("opts")),
      actionButton(ns("run"), "Run", icon = icon("play"), class = "btn-primary"),
      hr(),
      actionButton(ns("add"), "Add to report", icon = icon("plus")),
      conditionalPanel("output.fig_static", ns = ns, class = "anchor-inline",
        downloadButton(ns("dl_png"), "Figure (PNG)", class = "btn-sm"), downloadButton(ns("dl_pdf"), "Figure (PDF)", class = "btn-sm")),
      conditionalPanel("output.fig_interactive", ns = ns, p(class = "small text-muted mt-2 mb-1", "Interactive figure: save it with the camera icon on the figure.")),
      conditionalPanel("output.has_table", ns = ns, class = "anchor-inline", downloadButton(ns("dl_csv"), "Table (CSV)", class = "btn-sm")),
      p(class = "small text-muted mt-2", "Figures you add appear in phase C and in the report.")),
    uiOutput(ns("status")), uiOutput(ns("plot_ui")), uiOutput(ns("note")), DT::DTOutput(ns("table")))
}

dm_ui <- function() card(class = "analysis-card", full_screen = TRUE,
  card_header(class = "d-flex justify-content-between align-items-center", span(icon("flask"), " Data & methods: run the analysis"), uiOutput("dm_badge", inline = TRUE)),
  div(class = "d-flex gap-3 align-items-end flex-wrap", div(style = "min-width:320px", selectInput("dm_active", "Dataset in use", choices = NULL, width = "100%")),
      p(class = "small text-muted mb-3", "Methods run on this dataset; the preview under Data shows it.")),
  navset_underline(id = "dm_tabs",
    nav_panel("1 \u00b7 Data", value = "data", div(class = "pt-3",
      layout_columns(col_widths = c(6, 6),
        div(h6("Import your data"), p(class = "small text-muted", "CSV, Excel, TSV or RDS exported from LSEG Workspace, WRDS, FactSet, Bloomberg or your own coding sheet."),
            fileInput("dm_file", NULL, accept = c(".csv", ".xlsx", ".xls", ".tsv", ".txt", ".rds"), buttonLabel = "Choose file...", width = "100%"),
            textInput("dm_name", "Dataset name (optional)", width = "100%"),
            checkboxInput("dm_licensed", "Licensed data (keep it out of project files and analysis packages)", TRUE),
            actionButton("dm_import", "Import", icon = icon("file-import"), class = "btn-primary btn-sm")),
        div(h6("Fetch open data"), selectInput("dm_src", NULL, FETCH_SOURCES, width = "100%"), uiOutput("dm_src_opts"),
            actionButton("dm_fetch", "Fetch", icon = icon("cloud-arrow-down"), class = "btn-primary btn-sm"))),
      hr(), h6("Your datasets"), DT::DTOutput("dm_ds_table"),
      layout_columns(col_widths = c(4, 4, 4),
        div(),
        div(selectizeInput("dm_merge_with", "Merge with", choices = NULL, width = "100%"), selectizeInput("dm_merge_keys", "by columns", choices = NULL, multiple = TRUE, width = "100%")),
        div(class = "d-grid gap-2 mt-4", actionButton("dm_merge", "Merge (left join)", icon = icon("code-merge"), class = "btn-sm"),
            actionButton("dm_remove", "Remove dataset", icon = icon("trash"), class = "btn-sm btn-outline-danger"))),
      tags$details(class = "mb-2", tags$summary(tags$b("Build rating variables: consensus and disagreement")),
        p(class = "small text-muted mt-2", "Choose two or more rater columns (for example LSEG plus a hand-collected second rating), or the E, S and G pillar scores of one rater as a weaker proxy. Each column is standardised within the period (z-score), then averaged (consensus) and its standard deviation taken (disagreement or pillar dispersion)."),
        layout_columns(col_widths = c(5, 3, 4), selectizeInput("dm_rv_cols", "Rater or pillar columns", choices = NULL, multiple = TRUE, width = "100%"),
          selectizeInput("dm_rv_by", "Standardise within", choices = NULL, width = "100%"),
          radioButtons("dm_rv_kind", "These columns are", c("Two or more raters" = "raters", "Pillars of one rater (proxy)" = "pillars"))),
        actionButton("dm_rv_go", "Add consensus and disagreement columns", icon = icon("wand-magic-sparkles"), class = "btn-sm")),
      checkboxInput("dm_include_data", "Include open datasets in the project file (licensed datasets are never included)", FALSE),
      DT::DTOutput("dm_preview"))),
    nav_panel("2 \u00b7 Methods", value = "methods", div(class = "pt-3",
      layout_columns(col_widths = c(6, 6),
        selectInput("dm_method", "Method", METHODS, width = "100%"),
        selectInput("dm_template", "Start from a template (optional)", c("(none)" = "", names(TEMPLATES)), width = "100%")),
      p(class = "small text-muted", "Templates fill in variable names; map them to your columns before running. The dataset is the one chosen under Data."),
      uiOutput("dm_tpl_note"),
      uiOutput("dm_opts"),
      actionButton("dm_run", "Run", icon = icon("play"), class = "btn-primary"))),
    nav_panel("3 \u00b7 Results", value = "results", div(class = "pt-3",
      uiOutput("dm_result_head"), uiOutput("dm_plots_ui"), DT::DTOutput("dm_result_table"), uiOutput("dm_extra_tables"),
      tags$details(tags$summary("Full output"), verbatimTextOutput("dm_text")),
      h6(class = "mt-3", "R code that reproduces this result"), verbatimTextOutput("dm_code"),
      div(class = "d-flex gap-2 flex-wrap", actionButton("dm_add", "Add to report (phase O)", icon = icon("plus"), class = "btn-sm"),
          downloadButton("dm_dl_table", "Table (CSV)", class = "btn-sm"), downloadButton("dm_dl_pkg", "Analysis package (.zip)", class = "btn-sm"),
          checkboxInput("dm_pkg_licensed", "Include licensed data in the package", FALSE))))))

lessons_card <- function(id) card(class = "lessons-card",
  card_header(class = "d-flex justify-content-between align-items-center", span(icon("graduation-cap"), " Lessons learned & what's in it for me"), uiOutput(paste0(id, "_live_pill"), inline = TRUE)),
  uiOutput(paste0(id, "_lessons")),
  layout_columns(col_widths = c(6, 6),
    textAreaInput(paste0(id, "_mylessons"), "My lessons learned in this stage", rows = 3, width = "100%"),
    textAreaInput(paste0(id, "_mywiifm"), "What's in it for me", rows = 3, width = "100%")),
  p(class = "small text-muted mb-0", "Both boxes are optional and appear in your reports."))

theme <- bs_theme(version = 5, primary = "#B4561A", secondary = "#5B6675", success = "#2F7A4B", "navbar-bg" = "#14213D",
                  bg = "#FBFAF6", fg = "#14213D",
                  base_font = font_collection("IBM Plex Sans", "Segoe UI", "Arial", "sans-serif"),
                  heading_font = font_collection("Source Serif 4", "Georgia", "serif"))

ui <- page_navbar(
  title = span(icon("anchor"), " ANCHOR Studio", span(class = "brand-by", APP_BYLINE)), window_title = paste(APP_BRAND, "\u00b7", APP_TAGLINE),
  id = "nav", theme = theme, fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;600&family=Source+Serif+4:wght@600&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "lang_de.js"), tags$script(src = "lang_ar.js"), tags$script(src = "i18n.js"),
    tags$script(HTML("function anchorCopy(id){var t=document.getElementById(id);t.select();navigator.clipboard.writeText(t.value).then(()=>{},()=>document.execCommand('copy'));}"))),
  nav_panel("Before you start", value = "prereq", icon = icon("list-check"), prereq_ui()),
  nav_panel("Find a focal article", value = "find", icon = icon("magnifying-glass"), find_ui()),
  nav_panel("Start", value = "start", icon = icon("house"),
    layout_columns(col_widths = c(7, 5),
      div(
        card(card_header("1 \u00b7 Upload the article this review is anchored on"),
             p(class = "small text-muted mb-2", "No article yet? ", actionLink("goto_find", "Find a focal article"), " by searching a subject. Continuing a review? ",
               actionLink("open_project_start", "Open a saved project (.anchor)"), "."),
             fileInput("article_file", NULL, accept = c(".pdf", ".docx", ".txt"), buttonLabel = "Choose PDF...", placeholder = "PDF, Word or text file", width = "100%"),
             uiOutput("article_info"),
             textInput("art_title", "Title", width = "100%"),
             div(class = "d-flex gap-2 align-items-end", div(style = "flex:1", textInput("art_doi", "DOI", width = "100%")),
                 actionButton("doi_lookup", "Look up online", icon = icon("magnifying-glass"), class = "btn-sm mb-3")),
             uiOutput("focal_live"),
             div(class = "d-flex gap-2 align-items-end flex-wrap", div(style = "flex:1;min-width:220px", tags$label(class = "form-label", `for` = "art_citation", "Full reference")),
                 selectInput("art_style", "Citation style", CITE_STYLES, selected = "apa", width = "230px")),
             textAreaInput("art_citation", NULL, rows = 2, width = "100%"),
             uiOutput("cite_note"),
             radioButtons("art_version", "Which version is this file?", selected = character(0),
                          choices = c("Published version of record", "Accepted manuscript", "Working paper", "Preprint"))),
        card(card_header("2 \u00b7 About you"), p(class = "small text-muted", icon("list-check"), " Name and institution are filled from ", actionLink("goto_prereq", "Before you start"), ", which also records software, affiliation and data licences."),
             tagList(lapply(SETUP_QUESTIONS, q_input)))
      ),
      div(
        card(card_header("3 \u00b7 How should drafts be produced? (your decision)"),
             radioButtons("mode_start", NULL, choices = AI_MODES, selected = "prompt"),
             tags$table(class = "table table-sm small",
               tags$tr(tags$th("Mode"), tags$th("Strength"), tags$th("Limit")),
               tags$tr(tags$td("Copy-and-paste"), tags$td("Uses your claude.ai account; no key"), tags$td("More clicks")),
               tags$tr(tags$td("API key"), tags$td("One click per phase"), tags$td("Paid API; text leaves your PC")),
               tags$tr(tags$td("Ollama"), tags$td("Offline, private, free"), tags$td("Weaker drafts; needs a strong PC")),
               tags$tr(tags$td("Manual"), tags$td("Full control"), tags$td("Slowest"))),
             p(class = "small text-muted", "You can change this at any time under Settings.")),
        card(card_header("4 \u00b7 Progress"), uiOutput("progress_ui"),
             div(class = "d-grid gap-2 mt-2",
                 actionButton("confirm_setup", "Confirm set-up and open phase A", icon = icon("arrow-right"), class = "btn-primary"),
                 uiOutput("resume_ui")))
      ))),
  !!!unname(lapply(PHASES, function(p) nav_panel(paste(p$id, "\u00b7", sub(" .*$", "", p$name)), value = p$id,
      phase_shell(p, extra = switch(p$id,
        N = card(card_header("PRISMA 2020 flow"), p(class = "small text-muted", "Identification counts come from the data you import under Bibliometrics > Data. Enter the screening counts as you screen; leave empty to show 'pending'."),
                 layout_columns(col_widths = c(6, 6), uiOutput("prisma_auto"),
                   div(numericInput("pr_excl_screen", "Excluded at title/abstract screening", NA, min = 0),
                       numericInput("pr_assessed", "Full texts assessed", NA, min = 0),
                       numericInput("pr_excl_full", "Full texts excluded (reasons in notes)", NA, min = 0),
                       numericInput("pr_included", "Studies included", NA, min = 0)))),
        A = oa_card_A(),
        C = tagList(card(card_header("Bibliometric figures chosen for the report"), uiOutput("report_items_ui"),
                 actionLink("goto_biblio", "Open the Bibliometrics analyses")), oa_card_C()),
        H = oa_card_H(),
        O = dm_ui(),
        NULL))))),
  nav_menu("Bibliometrics", icon = icon("chart-column"),
    nav_panel("Data", value = "bib_data",
      layout_sidebar(sidebar = sidebar(width = 340,
          radioButtons("bib_source", "Data source", choices = c("Database export files" = "files", "OpenAlex (fetch live, free)" = "openalex", "The article's own reference list" = "refs",
                                                                "Sample dataset (bibliometrix 'management')" = "sample")),
          conditionalPanel("input.bib_source == 'files'",
            selectInput("bib_db", "Database", choices = DB_SOURCES),
            selectInput("bib_format", "File format", choices = DB_FORMATS),
            fileInput("bib_files", "Export file(s)", multiple = TRUE, accept = c(".csv", ".bib", ".txt", ".xlsx", ".ciw", ".rds"))),
          bib_oa_panel(),
          checkboxInput("bib_dedup", "Remove duplicates across files", TRUE),
          actionButton("bib_import", "Import", icon = icon("file-import"), class = "btn-primary"),
          hr(), h6("Filters"), uiOutput("bib_filters"),
          actionButton("bib_apply", "Apply filters", icon = icon("filter")),
          hr(),
          downloadButton("bib_xlsx", "Dataset for Biblioshiny (.xlsx)", class = "btn-sm"),
          downloadButton("bib_rds", "Dataset (.rds)", class = "btn-sm"),
          actionButton("ds_check", "Check records for retractions (online)", icon = icon("shield-halved"), class = "btn-sm btn-outline-secondary mt-2"),
          actionButton("open_biblioshiny", "Open full Biblioshiny", icon = icon("up-right-from-square"), class = "btn-outline-primary mt-2")),
        uiOutput("bib_status"), uiOutput("ds_status"), DT::DTOutput("bib_log"), DT::DTOutput("bib_table"))),
    !!!unname(lapply(unique(vapply(ANALYSES, `[[`, "", "group")), function(g) nav_panel(g, analysis_ui(g))))),
  nav_panel("Report & disclosure", value = "report", icon = icon("file-lines"),
    layout_columns(col_widths = c(4, 8),
      card(card_header("Export"),
           downloadButton("dl_html", "Report (HTML)"), downloadButton("dl_docx", "Report (Word)"), downloadButton("dl_pdf_report", "Report (PDF)"),
           downloadButton("dl_latex", "LaTeX package (.zip)"), downloadButton("dl_bib", "References (.bib)"),
           downloadButton("dl_disc", "AI-use disclosure (.txt)"), downloadButton("dl_prompts", "Prompt log (.csv)"),
           downloadButton("dl_decisions", "Decision log (.csv)"), downloadButton("dl_project", "Project file (.anchor)"),
           p(class = "small text-muted mt-2", icon("triangle-exclamation"), " The project file contains the full text of the article you uploaded. Share it only with people allowed to read that article (for example your supervisor), never publicly."),
           hr(), h6("Disclosure"), verbatimTextOutput("disc_preview")),
      card(card_header("Report preview"), uiOutput("report_preview")))),
  nav_panel("Settings", value = "settings", icon = icon("gear"),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Drafting mode"),
           radioButtons("mode", NULL, choices = AI_MODES, selected = "prompt"),
           checkboxInput("include_text", "Include the article's text in prompts (otherwise attach the PDF in Claude yourself)", TRUE),
           selectInput("draft_lang", "Language of AI drafts", c("Same as the app" = "", "English" = "en", "Deutsch" = "de", "\u0627\u0644\u0639\u0631\u0628\u064a\u0629" = "ar"), width = "100%"),
           p(class = "small text-muted", span("Prompts ask Claude to write in this language; the framework instructions stay in English."), " ", span("Reports are written in English, the language of the framework and of most journals.")),
           h6("Claude API"), passwordInput("api_key", "Anthropic API key"), textInput("api_model", "Model", "claude-sonnet-5-5"),
           checkboxInput("remember_key", "Remember the key on this computer", FALSE),
           p(class = "small text-muted", "A remembered key is stored unencrypted in your personal ANCHOR Studio settings file on this computer. It is never written into project files, reports or logs. Leave the box unticked on shared computers."),
           h6("Ollama"), textInput("ollama_url", "Ollama address", "http://localhost:11434"), textInput("ollama_model", "Model", "llama3.1"),
           actionButton("test_ai", "Test connection", icon = icon("plug"))),
      card(card_header("Project"),
           fileInput("project_file", "Open a project file (.anchor)", accept = c(".anchor", ".rds")),
           actionButton("save_now", "Save a copy now", icon = icon("floppy-disk")),
           p(class = "small text-muted mt-2", "Project and autosave files include the article's full text; keep them private."),
           p(class = "small text-muted mt-2", textOutput("autosave_info")),
           hr(), h6(icon("globe"), " Internet features"),
           checkboxInput("online", "Live reference checks, retraction alerts and new-reading suggestions (Crossref, OpenAlex)", TRUE),
           p(class = "small text-muted", "Only DOIs and short search terms are sent; never the article text, drafts, notes or keys. Without a connection the app uses its built-in references."),
           textInput("crossref_mailto", "Contact email (optional): faster Crossref access; required by SEC EDGAR", "", placeholder = "leave empty to stay anonymous"),
           passwordInput("openalex_key", "OpenAlex API key (optional, for heavy use)", ""),
           passwordInput("fred_key", "FRED API key (free; for economic series in phase O)", ""),
           div(class = "d-flex gap-2 align-items-center", actionButton("online_now", "Check now", icon = icon("rotate"), class = "btn-sm"), uiOutput("online_status", inline = TRUE)),
           hr(), radioButtons("pdf_engine", "PDF report engine", inline = TRUE, selected = "auto",
                              choices = c("Automatic" = "auto", "Edge / Chrome" = "browser", "Typst (built in)" = "typst")),
           p(class = "small text-muted", "Automatic prints the report with Microsoft Edge or Google Chrome when one is installed (closest to the HTML report), otherwise with the built-in Typst engine. Both work offline."),
           hr(), div(class = "brand-about", p(class = "mb-1", tags$b(APP_BRAND), " ", APP_VERSION), p(class = "brand-tag mb-1", APP_TAGLINE),
             p(class = "small text-muted", "ANCHOR framework by Amira Mamdouh Kastour \u00b7 bibliometric engine: bibliometrix (Aria & Cuccurullo, 2017) \u00b7 developed with Claude (Anthropic)."),
             tribute_block()))))
  ,
  nav_spacer(),
  nav_item(actionLink("open_project_top", span(icon("folder-open"), " Open project"))),
  nav_item(downloadLink("dl_project_top", span(icon("floppy-disk"), " Save project"))),
  nav_item(actionLink("about_top", span(icon("circle-info"), " About"))),
  nav_menu(span(icon("globe"), " Language"), align = "right",
    nav_item(tags$a(class = "dropdown-item anchor-lang-item no-i18n", `data-lang` = "en", href = "#", onclick = "anchorSetLang('en');return false;", "English")),
    nav_item(tags$a(class = "dropdown-item anchor-lang-item no-i18n", `data-lang` = "de", href = "#", onclick = "anchorSetLang('de');return false;", "Deutsch")),
    nav_item(tags$a(class = "dropdown-item anchor-lang-item no-i18n", `data-lang` = "ar", lang = "ar", dir = "rtl", href = "#", onclick = "anchorSetLang('ar');return false;", "\u0627\u0644\u0639\u0631\u0628\u064a\u0629")))
)

# ------------------------------------------------------------------ server
# Standalone app: stop R when the last browser window has been closed for 20 s
.anchor_sessions <- new.env(); .anchor_sessions$n <- 0L; .anchor_sessions$ever <- FALSE
# child processes started by the app (full Biblioshiny, background analyses) are stopped when the app stops
.anchor_children <- new.env()
shiny::onStop(function() for (k in ls(.anchor_children)) try(.anchor_children[[k]]$kill_tree(), silent = TRUE))
# ...and if no window ever connected (browser did not open), stop after 5 minutes
if (isTRUE(getOption("anchor.standalone"))) later::later(function() if (!.anchor_sessions$ever) shiny::stopApp(), 300)
server <- function(input, output, session) {
  if (isTRUE(getOption("anchor.standalone"))) {
    .anchor_sessions$n <- .anchor_sessions$n + 1L; .anchor_sessions$ever <- TRUE
    session$onSessionEnded(function() {
      .anchor_sessions$n <- .anchor_sessions$n - 1L
      later::later(function() if (.anchor_sessions$n <= 0L) shiny::stopApp(), 20)
    })
  }
  st <- do.call(reactiveValues, new_state())
  rv <- reactiveValues(reload = 0, bib_ver = 0, c_copies = NULL, trend = NULL)

  # settings stored on this computer (never inside project files)
  sfile <- file.path(user_dir(), "settings.rds")
  if (file.exists(sfile)) { s <- readRDS(sfile)
    updateTextInput(session, "api_model", value = s$api_model %||% "claude-sonnet-5-5")
    if (nzchar(s$api_key %||% "")) { updateTextInput(session, "api_key", value = s$api_key); updateCheckboxInput(session, "remember_key", value = TRUE) }
    updateTextInput(session, "ollama_url", value = s$ollama_url %||% "http://localhost:11434")
    updateTextInput(session, "ollama_model", value = s$ollama_model %||% "llama3.1")
    if (!is.null(s$mode)) { updateRadioButtons(session, "mode", selected = s$mode); updateRadioButtons(session, "mode_start", selected = s$mode) }
    if (!is.null(s$online)) updateCheckboxInput(session, "online", value = isTRUE(s$online))
    updateTextInput(session, "crossref_mailto", value = s$crossref_mailto %||% ""); updateTextInput(session, "openalex_key", value = s$openalex_key %||% ""); updateTextInput(session, "fred_key", value = s$fred_key %||% "") }

  # interface language (English, German, Arabic): kept with the computer's settings and shown by www/i18n.js
  ui_lang <- reactiveVal(NULL)
  local({ s0 <- if (file.exists(sfile)) tryCatch(readRDS(sfile), error = function(e) list()) else list()
    if (!is.null(s0$ui_lang)) { ui_lang(s0$ui_lang); session$sendCustomMessage("anchor-lang", s0$ui_lang) }
    if (!is.null(s0$draft_lang)) updateSelectInput(session, "draft_lang", selected = s0$draft_lang) })
  observeEvent(input$ui_lang, if (input$ui_lang %in% c("en", "de", "ar") && !identical(input$ui_lang, ui_lang())) ui_lang(input$ui_lang))
  draft_language <- function() { l <- isolate(input$draft_lang) %||% ""; if (!nzchar(l)) l <- isolate(ui_lang()) %||% "en"; l }

  observeEvent(input$mode_start, if (!identical(input$mode, input$mode_start)) updateRadioButtons(session, "mode", selected = input$mode_start))
  observeEvent(input$mode, if (!identical(input$mode, input$mode_start)) updateRadioButtons(session, "mode_start", selected = input$mode))
  observe({
    s <- list(mode = input$mode %||% "prompt", api_key = input$api_key %||% "", api_model = input$api_model %||% "", ollama_url = input$ollama_url %||% "",
              ollama_model = input$ollama_model %||% "", include_text = isTRUE(input$include_text),
              online = !isFALSE(input$online), crossref_mailto = input$crossref_mailto %||% "", openalex_key = input$openalex_key %||% "", fred_key = input$fred_key %||% "")
    st$settings <- s
    saveRDS(list(mode = s$mode, api_model = s$api_model, ollama_url = s$ollama_url, ollama_model = s$ollama_model,
                 api_key = if (isTRUE(input$remember_key)) s$api_key else "", online = s$online, crossref_mailto = s$crossref_mailto,
                 openalex_key = s$openalex_key, fred_key = s$fred_key, ui_lang = ui_lang(), draft_lang = input$draft_lang %||% ""), sfile)
  })
  observeEvent(input$test_ai, {
    m <- input$mode
    if (!m %in% c("api", "ollama")) { showNotification("Copy-and-paste and manual modes need no connection.", type = "message"); return() }
    res <- tryCatch(ai_draft(m, "Reply with the single word: ready", isolate(st$settings)), error = function(e) paste("Error:", conditionMessage(e)))
    showNotification(paste("Response:", substr(res, 1, 200)), type = if (startsWith(res, "Error")) "error" else "message", duration = 8)
  })

  # ---------------- live internet features (background processes keep the app responsive)
  online_job <- reactiveVal(NULL); ds_job <- reactiveVal(NULL)
  start_online <- function(force = FALSE) {
    if (!is.null(isolate(online_job()))) return()
    cfg <- online_cfg(isolate(st$settings)); if (!cfg$enabled) return()
    p <- tryCatch(callr::r_bg(bg_online, args = list(app_dir = getwd(), libs = .libPaths(), what = "refresh", cfg = cfg, focal_doi = isolate(st$article$doi) %||% ""),
                              supervise = TRUE, package = FALSE), error = function(e) NULL)
    if (!is.null(p)) { .anchor_children[[session$ns("online")]] <- p; online_job(list(p = p, t0 = Sys.time())) }
  }
  observe({ j <- online_job(); req(j)
    if (j$p$is_alive()) { if (difftime(Sys.time(), j$t0, units = "secs") > 300) try(j$p$kill_tree(), silent = TRUE); invalidateLater(800); return() }
    res <- tryCatch(j$p$get_result(), error = function(e) NULL); online_job(NULL)
    if (is.null(res)) return()
    if (isTRUE(res$online) || is.null(isolate(st$online)) || !isTRUE(isolate(st$online)$online)) st$online <- res
    bad <- Filter(function(x) isTRUE(x$found) && x$status != "ok", res$refs)
    if (isTRUE(res$focal$found) && res$focal$status != "ok") showNotification(paste("The focal article has a published", NOTICE_NAME[[res$focal$status]], "- see the Start page."), type = "warning", duration = NULL)
    if (length(bad)) showNotification(sprintf("%d built-in reference(s) now have a correction, concern or retraction notice.", length(bad)), type = "warning", duration = 20)
  })
  # first check shortly after start, then every 6 hours while the app is open
  observe({ invalidateLater(6 * 3600 * 1000); if (!isFALSE(input$online)) later::later(function() start_online(), 3) })
  observeEvent(input$online_now, { if (isFALSE(input$online)) return(showNotification("Switch on the internet features first.", type = "warning")); start_online(TRUE) })
  output$online_status <- renderUI(online_pill(st$online, online_job(), long = TRUE))
  # ---------------- DOI: Crossref look-up and the reference in the chosen style (automatic; your own edits are never overwritten)
  cite_key <- reactiveVal("")
  apply_citation <- function(cit, d) {
    cur <- isolate(input$art_citation) %||% ""
    if (!nzchar(trimws(cur)) || cite_is_auto(cur, isolate(st$article))) { updateTextAreaInput(session, "art_citation", value = cit); st$article <- mark_auto(isolate(st$article), cit); st$article$cite_pending <- NULL; return(TRUE) }
    if (!identical(cur, cit)) st$article$cite_pending <- cit
    FALSE }
  do_lookup <- function(d, style = isolate(input$art_style) %||% "apa", why = "auto") {
    loud <- why == "button"
    if (!nzchar(d)) return(if (loud) showNotification("Enter the DOI first.", type = "warning"))
    if (isFALSE(isolate(input$online))) return(if (loud) showNotification("Internet features are switched off in Settings.", type = "warning"))
    cite_key(paste(d, style)); cfg <- online_cfg(isolate(st$settings))
    f <- isolate(st$online$focal)
    r <- if (!loud && !is.null(f) && identical(f$doi, d) && isTRUE(f$found)) f else
      withProgress(message = "Looking up the DOI at Crossref...", value = .5, tryCatch(crossref_lookup(d, cfg), error = function(e) e))
    if (inherits(r, "error")) return(showNotification(paste("Could not reach Crossref:", conditionMessage(r)), type = if (loud) "error" else "warning", duration = 10))
    if (!isTRUE(r$found)) return(showNotification(sprintf("Crossref has no record for doi:%s. Check it for typos.", d), type = "warning"))
    o <- isolate(st$online) %||% list(); o$focal <- r; st$online <- o
    if (!nzchar(isolate(input$art_title) %||% "")) updateTextInput(session, "art_title", value = r$meta$title)
    cit <- tryCatch(cite_doi(d, style, cfg, r$meta), error = function(e) e)
    if (inherits(cit, "error")) { showNotification(sprintf("doi.org gave no %s reference (%s); APA built from Crossref is used instead.", names(CITE_STYLES)[CITE_STYLES == style], conditionMessage(cit)), type = "warning", duration = 10)
      cit <- ref_apa(r$meta); style <- "apa"; cite_key(paste(d, "apa")); updateSelectInput(session, "art_style", selected = "apa") }
    st$article$cite_style <- style
    used <- apply_citation(cit, d)
    log_decision("Set-up", if (used) "Focal reference filled from the DOI" else "Reference from the DOI offered (your edited reference kept)",
                 paste0("doi:", d, " \u00b7 style: ", names(CITE_STYLES)[CITE_STYLES == style], if (r$status != "ok") paste(" - notice:", r$status) else ""))
  }
  observeEvent(input$doi_lookup, do_lookup(norm_doi(input$art_doi), why = "button"))
  doi_typed <- debounce(reactive(norm_doi(input$art_doi)), 1200)
  observeEvent(doi_typed(), { d <- doi_typed(); s <- isolate(input$art_style) %||% "apa"
    if (grepl("^10\\.\\d{4,9}/\\S+$", d) && !identical(isolate(cite_key()), paste(d, s))) do_lookup(d, s) }, ignoreInit = TRUE)
  observeEvent(input$art_style, { d <- norm_doi(input$art_doi); s <- input$art_style
    if (grepl("^10\\.\\d{4,9}/\\S+$", d) && !identical(isolate(cite_key()), paste(d, s))) do_lookup(d, s, why = "style") }, ignoreInit = TRUE)
  output$cite_note <- renderUI({ pend <- st$article$cite_pending; cur <- input$art_citation %||% ""
    if (is.null(pend) || identical(pend, cur)) return(p(class = "small text-muted", "Filled from the DOI in the chosen style. If you edit it, your version is kept."))
    div(class = "alert alert-info small py-2", "Your edited reference was kept. From the DOI: ", tags$em(pend), " ",
        actionLink("cite_replace", "Use this instead")) })
  observeEvent(input$cite_replace, { p <- isolate(st$article$cite_pending); req(p)
    updateTextAreaInput(session, "art_citation", value = p); st$article <- mark_auto(isolate(st$article), p); st$article$cite_pending <- NULL
    log_decision("Set-up", "Reference replaced with the one from the DOI", norm_doi(input$art_doi)) })
  observeEvent(input$goto_find, nav_select("nav", "find"))
  output$focal_live <- renderUI({ f <- st$online$focal; d <- norm_doi(input$art_doi)
    if (is.null(f) || !identical(f$doi, d)) return(NULL)
    if (!isTRUE(f$found)) return(div(class = "alert alert-warning small", "Crossref has no record for this DOI."))
    if (f$status == "ok") return(div(class = "small text-success mb-2", icon("circle-check"), sprintf(" Verified at Crossref %s; no correction or retraction notices.", f$checked_at)))
    div(class = paste("alert small", if (f$status == "corrected") "alert-warning" else "alert-danger"), icon("triangle-exclamation"), tags$b(sprintf(" This article has a published %s: ", NOTICE_NAME[[f$status]])),
        paste(sprintf("%s (%s, doi:%s)", f$notices$label, f$notices$date, f$notices$doi), collapse = "; "), ". Read the notice before anchoring a review on it.") })
  start_ds <- function() {
    M <- isolate(st$biblio$M); if (is.null(M) || !is.null(isolate(ds_job()))) return()
    cfg <- online_cfg(isolate(st$settings)); if (!cfg$enabled) return()
    p <- tryCatch(callr::r_bg(bg_online, args = list(app_dir = getwd(), libs = .libPaths(), what = "dataset", cfg = cfg, M = as.data.frame(M)[, intersect(c("DI", "SR", "TI", "PY"), names(M)), drop = FALSE]),
                              supervise = TRUE, package = FALSE), error = function(e) NULL)
    if (!is.null(p)) { .anchor_children[[session$ns("ds")]] <- p; ds_job(list(p = p, t0 = Sys.time())) }
  }
  observe({ j <- ds_job(); req(j)
    if (j$p$is_alive()) { invalidateLater(800); return() }
    res <- tryCatch(j$p$get_result(), error = function(e) list(error = conditionMessage(e))); ds_job(NULL); st$online_ds <- res
    if (!is.null(res$flagged) && nrow(res$flagged)) showNotification(sprintf("%d record(s) in your dataset are retracted. See Bibliometrics > Data.", nrow(res$flagged)), type = "warning", duration = NULL) })
  observeEvent(input$ds_check, { if (isFALSE(input$online)) return(showNotification("Switch on the internet features in Settings first.", type = "warning")); start_ds() })
  output$ds_status <- renderUI({
    if (!is.null(ds_job())) return(div(class = "alert alert-info small", icon("spinner", class = "fa-spin"), " Checking the records for retractions online..."))
    d <- st$online_ds; if (is.null(d)) return(NULL)
    if (!is.null(d$error)) return(div(class = "alert alert-secondary small", "Retraction check could not run: ", d$error))
    fl <- d$flagged
    if (is.null(fl) || !nrow(fl)) return(div(class = "alert alert-success small", icon("shield-halved"), sprintf(" Retraction check %s: %d of %d records have a DOI; %d found in OpenAlex; none retracted.", d$checked_at, d$n_doi, d$n_records, d$n_found %||% 0)))
    div(class = "alert alert-danger small", icon("triangle-exclamation"), tags$b(sprintf(" %d retracted record(s) found (%s). ", nrow(fl), d$checked_at)),
        "Decide whether to exclude them (filters) and say so in your PRISMA notes.", tags$ul(lapply(seq_len(nrow(fl)), function(i) tags$li(sprintf("%s - %s (doi:%s) - %s", fl$record[i], fl$title[i], fl$doi[i], fl$notice[i]))))) })

  # ---------------- article upload
  load_article <- function(path, name) {
    info <- tryCatch(read_article(path, name), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(info)) return(invisible(NULL))
    st$article <- modifyList(isolate(st$article), info)
    updateTextInput(session, "art_title", value = info$title_guess)
    updateTextInput(session, "art_doi", value = info$doi)
    if (info$scanned) showNotification("Very little text could be read. The PDF may be scanned; run OCR first or upload a text version.", type = "warning", duration = 12)
    d <- norm_doi(info$doi %||% "")      # DOI found in the file: look it up straight away
    if (grepl("^10\\.\\d{4,9}/\\S+$", d)) do_lookup(d)
    invisible(info)
  }
  observeEvent(input$article_file, { f <- input$article_file; load_article(f$datapath, f$name) })
  observe({ st$article$title <- input$art_title; st$article$doi <- input$art_doi; st$article$citation <- input$art_citation; st$article$version <- input$art_version })
  output$article_info <- renderUI({
    a <- st$article
    if (is.null(a$text)) return(div(class = "alert alert-secondary small", "Upload the article to begin. ANCHOR Studio reads its text, DOI, abstract and reference list. The article text never leaves your computer unless you choose the API mode; the live reference checks send only DOIs and search terms."))
    div(class = "alert alert-success small",
        tags$b(a$file), sprintf(" \u00b7 %s pages \u00b7 %s words \u00b7 %d references found", a$pages %||% "?", format(a$words, big.mark = ","), length(a$references)),
        if (nzchar(a$abstract %||% "")) tags$details(tags$summary("Abstract found"), p(a$abstract)),
        p(class = "mb-0", "Check the title, DOI and reference below and correct them if needed."))
  })

  # ---------------- setup
  observe({ for (q in SETUP_QUESTIONS) st$setup[[q$id]] <- input[[q$id]] })
  output$progress_ui <- renderUI({
    items <- lapply(PHASES, function(p) { s <- st$phases[[p$id]]$status
      tags$li(class = paste("prog", gsub(" ", "-", s)), tags$b(p$id), " ", p$name, span(class = "badge-status", s)) })
    tagList(p(if (isTRUE(st$setup_ok)) "Set-up confirmed." else "Set-up not yet confirmed."), tags$ul(class = "prog-list", items))
  })
  output$resume_ui <- renderUI({
    af <- file.path(user_dir(), "autosave.anchor")
    if (file.exists(af) && is.null(st$article$text)) actionButton("resume", paste("Resume last session (", format(file.mtime(af), "%d %b %H:%M"), ")"), icon = icon("clock-rotate-left"))
  })
  observeEvent(input$confirm_setup, {
    miss <- c(if (is.null(st$article$text)) "Upload the article",
              if (!nzchar(input$art_citation %||% "")) "Enter the full reference of the article",
              if (is.null(input$art_version)) "Say which version of the article you uploaded",
              unlist(lapply(SETUP_QUESTIONS, function(q) if (is.null(input[[q$id]]) || !nzchar(paste(input[[q$id]], collapse = ""))) q$label)))
    if (length(miss)) return(showModal(modalDialog(title = "Before we start", tags$ul(lapply(miss, tags$li)), easyClose = TRUE)))
    st$setup_ok <- TRUE
    ph <- st$phases; if (ph$A$status == "locked") ph$A$status <- "open"; st$phases <- ph
    log_decision("Set-up", "Confirmed", paste("Mode:", input$mode, "\u00b7 Version:", input$art_version))
    nav_select("nav", "A")
  })

  log_decision <- function(phase, decision, note = "") {
    st$decisions <- rbind(isolate(st$decisions), data.frame(time = format(Sys.time(), "%Y-%m-%d %H:%M"), phase = phase, decision = decision, note = note %||% ""))
    autosave()
  }
  log_prompt <- function(phase, prompt, mode = isolate(input$mode)) {
    st$prompts <- rbind(isolate(st$prompts), data.frame(time = format(Sys.time(), "%Y-%m-%d %H:%M"), phase = phase, mode = mode, prompt = prompt))
  }

  # ---------------- phases
  context_for <- function(id) {
    i <- match(id, PHASE_IDS); if (i == 1) return("")
    ph <- isolate(st$phases)
    txt <- vapply(PHASE_IDS[seq_len(i - 1)], function(j) if (ph[[j]]$status == "approved") paste0("## Phase ", j, " (approved)\n", substr(ph[[j]]$draft, 1, 8000)) else "", "")
    extra <- c(if (id %in% c("N", "C", "R") && !is.null(isolate(st$prisma))) paste("PRISMA counts:", paste(names(isolate(st$prisma)), unlist(isolate(st$prisma)), sep = " = ", collapse = "; ")),
               if (id %in% c("C", "H", "R")) paste0("## Bibliometric summary\n", biblio_summary_text(isolate(st$biblio$M), isolate(st$biblio$R))))
    paste(c(txt[nzchar(txt)], extra), collapse = "\n\n")
  }
  answers_for <- function(p) {
    a <- list(`Article version` = isolate(input$art_version), `Review purpose` = isolate(input$S_purpose))
    for (q in p$questions) a[[q$label]] <- isolate(input[[q$id]]) %||% "(not answered)"
    a
  }

  lapply(PHASES, function(p) {
    id <- p$id
    is_locked <- reactive({ i <- match(id, PHASE_IDS); !isTRUE(st$setup_ok) || (i > 1 && st$phases[[PHASE_IDS[i - 1]]]$status != "approved") })
    output[[paste0(id, "_banner")]] <- renderUI({
      s <- st$phases[[id]]$status
      if (is_locked()) return(div(class = "alert alert-secondary", icon("lock"), if (!isTRUE(st$setup_ok)) " Confirm the set-up on the Start page first." else
        paste0(" Phase ", PHASE_IDS[match(id, PHASE_IDS) - 1], " must be approved before this phase opens.")))
      switch(s, approved = div(class = "alert alert-success", icon("circle-check"), paste(" Approved", st$phases[[id]]$approved_at %||% ""), ". Reopen it to make changes."),
             `needs review` = div(class = "alert alert-warning", icon("triangle-exclamation"), " An earlier phase was changed after this one was approved. Review and approve again."),
             div(class = "alert alert-info small mb-2", icon("circle-info"), " Answer the questions on the right, produce the draft, tick the checks, then decide at the gate."))
    })
    output[[paste0(id, "_modeinfo")]] <- renderUI({
      m <- input$mode
      txt <- switch(m, prompt = "Copy-and-paste mode: 'Draft this phase' shows a prompt. Paste it into claude.ai, then paste Claude's answer below.",
                    api = paste("API mode: 'Draft this phase' asks", input$api_model, "and puts the draft below for you to edit."),
                    ollama = paste("Local mode: 'Draft this phase' asks", input$ollama_model, "via Ollama on this computer."),
                    manual = "Manual mode: write this phase yourself in the box below.")
      p(class = "small text-muted", txt)
    })
    # keep state in sync with inputs
    # copy-and-paste mode: a shown prompt counts as AI use only once an answer is pasted into the draft box
    pending_prompt <- reactiveVal(NULL)
    observeEvent(input[[paste0(id, "_draft")]], { ph <- st$phases; ph[[id]]$draft <- input[[paste0(id, "_draft")]]
      ph[[id]]$ai_q <- extract_ai_questions(ph[[id]]$draft); st$phases <- ph
      pp <- pending_prompt()
      if (!is.null(pp) && nchar(trimws(ph[[id]]$draft)) >= 50) { log_prompt(id, pp, mode = "prompt"); pending_prompt(NULL) } }, ignoreInit = TRUE)
    observeEvent(input[[paste0(id, "_mylessons")]], { ph <- st$phases; ph[[id]]$my_lessons <- input[[paste0(id, "_mylessons")]]; st$phases <- ph }, ignoreInit = TRUE)
    observeEvent(input[[paste0(id, "_mywiifm")]], { ph <- st$phases; ph[[id]]$my_wiifm <- input[[paste0(id, "_mywiifm")]]; st$phases <- ph }, ignoreInit = TRUE)
    output[[paste0(id, "_live_pill")]] <- renderUI(online_pill(st$online, online_job()))
    output[[paste0(id, "_lessons")]] <- renderUI(lessons_body(id, reactiveValuesToList(st)))
    observeEvent(input[[paste0(id, "_accept")]], {
      d <- input[[paste0(id, "_accept")]]; sg <- st$online$suggestions[[id]]; i <- match(d, sg$doi); req(!is.na(i))
      ph <- st$phases; acc <- ph[[id]]$accepted %||% list()
      if (!d %in% vapply(acc, `[[`, "", "doi")) acc[[length(acc) + 1]] <- as.list(sg[i, c("au", "year", "title", "journal", "vol", "iss", "pages", "doi")])
      ph[[id]]$accepted <- acc; st$phases <- ph
      log_decision(id, "Reference added (live suggestion)", paste0(sg$au[i], " (", sg$year[i], "). ", sg$title[i], ". doi:", d))
      showNotification("Added to this stage's references and to references.bib.", type = "message") })
    observeEvent(input[[paste0(id, "_unaccept")]], {
      d <- input[[paste0(id, "_unaccept")]]; ph <- st$phases
      ph[[id]]$accepted <- Filter(function(r) !identical(r$doi, d), ph[[id]]$accepted %||% list()); st$phases <- ph
      log_decision(id, "Reference removed", paste0("doi:", d)) })
    observeEvent(input[[paste0(id, "_notes")]], { ph <- st$phases; ph[[id]]$notes <- input[[paste0(id, "_notes")]]; st$phases <- ph }, ignoreInit = TRUE)
    observeEvent(input[[paste0(id, "_checks")]], { ph <- st$phases; ph[[id]]$checks <- input[[paste0(id, "_checks")]] %||% character(0); st$phases <- ph }, ignoreInit = TRUE, ignoreNULL = FALSE)
    lapply(p$questions, function(q) observeEvent(input[[q$id]], { ph <- st$phases; ph[[id]]$answers[[q$id]] <- input[[q$id]]; st$phases <- ph }, ignoreInit = TRUE))

    # the model's own questions, shown as required answers
    # re-render only when the list of questions itself changes, so typed answers keep focus
    aiq_now <- reactiveVal(character(0))
    observe({ q <- st$phases[[id]]$ai_q %||% character(0); if (!identical(q, isolate(aiq_now()))) aiq_now(q) })
    output[[paste0(id, "_aiq")]] <- renderUI({
      rv$reload
      qs <- aiq_now()
      if (!length(qs)) return(NULL)
      prev <- isolate(st$phases[[id]]$ai_a)
      tagList(hr(), h6("Questions raised in the draft"),
              lapply(seq_along(qs), function(k) textAreaInput(paste0(id, "_aia_", k), qs[k], value = prev[[k]] %||% "", rows = 2, width = "100%")))
    })
    observe({ qs <- st$phases[[id]]$ai_q; if (!length(qs)) return()
      vals <- lapply(seq_along(qs), function(k) input[[paste0(id, "_aia_", k)]] %||% "")
      isolate({ ph <- st$phases; ph[[id]]$ai_a <- vals; st$phases <- ph }) })

    make_prompt <- function(revision = NULL) {
      pr <- build_prompt(p, isolate(st$article$text) %||% "", context_for(id), answers_for(p), include_text = isolate(input$include_text), lang = draft_language())
      if (!is.null(revision)) pr <- paste0(pr, "\n\n=== YOUR PREVIOUS DRAFT ===\n", isolate(st$phases[[id]]$draft),
                                          "\n\n=== REVISION REQUESTED BY THE RESEARCHER ===\n", revision, "\nRevise the draft accordingly.")
      pr
    }
    show_prompt <- function(pr) {
      pending_prompt(pr)
      showModal(modalDialog(title = paste("Prompt for phase", id), size = "xl", easyClose = TRUE,
        p("1. Copy the prompt. 2. Open ", tags$a("claude.ai", href = "https://claude.ai", target = "_blank"),
          if (!isTRUE(isolate(input$include_text))) " and attach the article PDF" else "", ". 3. Paste and send. 4. Paste Claude's full answer into the draft box."),
        p(class = "small text-muted", "The prompt is added to your AI-use disclosure only when you paste an answer into the draft box. If you do not use it, nothing is recorded."),
        tags$textarea(id = paste0(id, "_prompt_text"), class = "form-control prompt-box", rows = 18, readonly = NA, pr),
        footer = tagList(tags$button(type = "button", class = "btn btn-primary", onclick = sprintf("anchorCopy('%s_prompt_text')", id), icon("copy"), " Copy prompt"),
                         modalButton("Close"))))
    }
    run_draft <- function(revision = NULL) {
      if (is_locked()) return(showNotification("This phase is still locked.", type = "warning"))
      m <- input$mode
      pr <- make_prompt(revision)
      if (m == "manual") return(showNotification("Manual mode: write the phase in the draft box. Switch mode under Settings to get AI drafts.", type = "message"))
      if (m == "prompt") return(show_prompt(pr))
      log_prompt(id, pr)
      out <- withProgress(message = paste("Drafting phase", id, "..."), value = .3,
                          tryCatch(ai_draft(m, pr, isolate(st$settings)), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 15); NULL }))
      if (!is.null(out)) { updateTextAreaInput(session, paste0(id, "_draft"), value = out)
        showNotification("Draft ready. Read it, edit it, answer any questions it raises, then decide at the gate.", type = "message", duration = 8) }
    }
    observeEvent(input[[paste0(id, "_draft_btn")]], {
      if (nzchar(isolate(st$phases[[id]]$draft)) && input$mode %in% c("api", "ollama"))
        showModal(modalDialog(title = "Replace the current draft?", "The draft box already has text. A new draft will replace it.",
                              footer = tagList(modalButton("Keep my text"), actionButton(paste0(id, "_draft_go"), "Replace with a new draft", class = "btn-warning"))))
      else run_draft()
    })
    observeEvent(input[[paste0(id, "_draft_go")]], { removeModal(); run_draft() })
    observeEvent(input[[paste0(id, "_prompt_btn")]], show_prompt(make_prompt()))

    observeEvent(input[[paste0(id, "_approve")]], {
      if (is_locked()) return(showNotification("This phase is still locked.", type = "warning"))
      ph <- st$phases[[id]]
      miss <- c(if (!nzchar(trimws(ph$draft))) "The draft is empty",
                setdiff(p$checks, ph$checks) |> (\(x) if (length(x)) paste("Tick:", x))(),
                unlist(lapply(p$questions, function(q) if (is.null(input[[q$id]]) || !nzchar(paste(input[[q$id]], collapse = ""))) paste("Answer:", q$label))),
                unlist(lapply(seq_along(ph$ai_q), function(k) if (!nzchar(trimws(ph$ai_a[[k]] %||% ""))) paste("Answer the draft's question:", ph$ai_q[k]))),
                if (id == "N" && is.null(st$biblio$M) && !identical(input$N_fallback, PHASES$N$questions[[6]]$choices[2])) "Import database exports under Bibliometrics > Data, or choose to continue with snowballing")
      if (length(miss)) return(showModal(modalDialog(title = paste("Gate", id, "is not ready"), tags$ul(lapply(miss, tags$li)), easyClose = TRUE)))
      showModal(modalDialog(title = paste("Approve phase", id, "?"),
        p("You confirm that you have read and checked this phase and take responsibility for it."),
        footer = tagList(modalButton("Not yet"), actionButton(paste0(id, "_approve_go"), "Yes, approve", class = "btn-success"))))
    })
    observeEvent(input[[paste0(id, "_approve_go")]], {
      removeModal()
      phs <- st$phases; phs[[id]]$status <- "approved"; phs[[id]]$approved_at <- format(Sys.time(), "%Y-%m-%d %H:%M")
      i <- match(id, PHASE_IDS)
      if (i < length(PHASE_IDS) && phs[[PHASE_IDS[i + 1]]]$status == "locked") phs[[PHASE_IDS[i + 1]]]$status <- "open"
      st$phases <- phs
      log_decision(id, "Approved", input[[paste0(id, "_notes")]])
      if (i < length(PHASE_IDS)) { showNotification(paste("Phase", id, "approved. Phase", PHASE_IDS[i + 1], "is open."), type = "message"); nav_select("nav", PHASE_IDS[i + 1]) }
      else { showNotification("All six gates passed. Export your report.", type = "message"); nav_select("nav", "report") }
    })
    observeEvent(input[[paste0(id, "_revise")]], {
      showModal(modalDialog(title = paste("Revision of phase", id),
        textAreaInput(paste0(id, "_rev_note"), "What should change?", rows = 4, width = "100%"),
        p(class = "small text-muted", if (input$mode %in% c("api", "ollama")) "A new draft will be requested with your instructions." else
          if (input$mode == "prompt") "A revision prompt will be shown for you to paste into Claude." else "Your note is logged; edit the draft yourself."),
        footer = tagList(modalButton("Cancel"), actionButton(paste0(id, "_rev_go"), "Request revision", class = "btn-warning"))))
    })
    observeEvent(input[[paste0(id, "_rev_go")]], {
      note <- input[[paste0(id, "_rev_note")]]; removeModal()
      phs <- st$phases; if (phs[[id]]$status == "approved") phs[[id]]$status <- "open"; st$phases <- phs
      log_decision(id, "Revision requested", note)
      run_draft(revision = note)
    })
    observeEvent(input[[paste0(id, "_reopen")]], {
      phs <- st$phases
      if (phs[[id]]$status != "approved") return(showNotification("Only approved phases can be reopened.", type = "message"))
      phs[[id]]$status <- "open"
      later <- PHASE_IDS[seq_along(PHASE_IDS) > match(id, PHASE_IDS)]
      for (j in later) if (phs[[j]]$status == "approved") phs[[j]]$status <- "needs review"
      st$phases <- phs
      log_decision(id, "Reopened", "Later approved phases marked 'needs review'")
    })
  })

  # ---------------- PRISMA
  observe({
    b <- st$biblio
    if (is.null(b$log)) return()
    ident <- sum(b$log$records); filtered <- nrow(b$M_all) - nrow(b$M)
    st$prisma <- list(identified = ident, duplicates = b$dup, filtered = filtered, screened = nrow(b$M),
                      excluded_screen = input$pr_excl_screen %|na|% NULL, assessed = input$pr_assessed %|na|% NULL,
                      excluded_full = input$pr_excl_full %|na|% NULL, included = input$pr_included %|na|% NULL)
  })
  output$prisma_auto <- renderUI({
    pr <- st$prisma
    if (is.null(pr)) return(div(class = "alert alert-secondary small", "No database exports imported yet. Counts are 'pending'."))
    HTML(prisma_html(pr))
  })

  # ---------------- bibliometrics: data
  output$bib_filters <- renderUI({
    rv$bib_ver; M <- isolate(st$biblio$M_all); if (is.null(M)) return(p(class = "small text-muted", "Import data first."))
    yrs <- range(M$PY, na.rm = TRUE)
    tagList(sliderInput("f_years", "Publication years", min = yrs[1], max = yrs[2], value = yrs, sep = "", step = 1),
            if ("DT" %in% names(M)) checkboxGroupInput("f_types", "Document types", choices = sort(unique(M$DT)), selected = unique(M$DT)),
            if ("LA" %in% names(M)) selectizeInput("f_langs", "Languages", choices = sort(unique(M$LA)), selected = unique(M$LA), multiple = TRUE),
            numericInput("f_tc", "Minimum citations", 0, min = 0))
  })
  set_biblio <- function(M_all, log, dup, source) {
    R <- tryCatch(bibliometrix::biblioAnalysis(M_all, sep = ";"), error = function(e) NULL)
    st$biblio <- list(M = M_all, M_all = M_all, R = R, log = log, dup = dup, source = source)
    rv$bib_ver <- rv$bib_ver + 1
  }
  observeEvent(input$bib_import, {
    if (input$bib_source == "openalex") { if (isFALSE(input$online)) return(showNotification("Switch on the internet features in Settings first.", type = "warning"))
      r <- conn$fetch_corpus(input$bib_oa_mode, input$bib_oa_query, input$bib_oa_from, input$bib_oa_to, input$bib_oa_types, input$bib_oa_lang, min(5000, max(50, input$bib_oa_max %||% 500)))
      if (!is.null(r)) { st$online_ds <- NULL; start_ds() }; return() }
    withProgress(message = "Importing...", value = .4, {
      res <- tryCatch({
        if (input$bib_source == "sample") { e <- new.env(); utils::data("management", package = "bibliometrixData", envir = e)
          M <- e$management; list(M = M, log = data.frame(file = "bibliometrixData::management", database = "wos", records = nrow(M)), dup = 0) }
        else if (input$bib_source == "refs") { M <- refs_to_M(st$article$references %||% character(0))
          if (is.null(M) || !nrow(M)) stop("No references could be read from the article. Upload database exports instead.")
          list(M = M, log = data.frame(file = "Article reference list", database = "article", records = nrow(M)), dup = 0) }
        else { f <- input$bib_files; if (is.null(f)) stop("Choose one or more export files first.")
          im <- import_files(f$datapath, f$name, input$bib_db, input$bib_format)
          mg <- merge_imports(im$list, input$bib_dedup); list(M = mg$M, log = im$log, dup = mg$dup) }
      }, error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 15); NULL })
      if (is.null(res)) return()
      set_biblio(res$M, res$log, res$dup, input$bib_source)
      showNotification(sprintf("%d records ready for analysis.", nrow(res$M)), type = "message")
      st$online_ds <- NULL; if (!isFALSE(input$online)) start_ds()
    })
  })
  observeEvent(input$bib_apply, {
    M <- st$biblio$M_all; req(M)
    M2 <- apply_filters(M, input$f_years, input$f_types, input$f_langs, input$f_tc)
    b <- st$biblio; b$M <- M2; b$R <- tryCatch(bibliometrix::biblioAnalysis(M2, sep = ";"), error = function(e) NULL); st$biblio <- b
    showNotification(sprintf("%d of %d records kept.", nrow(M2), nrow(M)), type = "message")
  })
  output$bib_status <- renderUI({
    b <- st$biblio
    if (is.null(b$M)) return(div(class = "alert alert-info", tags$b("Import your data. "),
      "Export records from Scopus (CSV or BibTeX, all fields including cited references) or Web of Science (Plain text, 'Full record and cited references'), then upload them here. ",
      "You can also start from the article's own reference list, or try the sample dataset."))
    div(class = "alert alert-success", sprintf("%d records in use (of %d imported, %d duplicates removed). Years %s-%s.",
        nrow(b$M), nrow(b$M_all), b$dup, min(b$M$PY, na.rm = TRUE), max(b$M$PY, na.rm = TRUE)),
        if (b$source == "refs") p(class = "mb-0 small", "Note: the article's reference list has no keywords, citations or affiliations, so only some analyses can run."),
        if (b$source == "openalex") p(class = "mb-0 small", "Source: OpenAlex (CC0), fetched live. Cited references are OpenAlex work ids, so co-citation and historiograph use those ids; cite OpenAlex (Priem et al., 2022) in your methods."))
  })
  output$bib_log <- DT::renderDT({ req(st$biblio$log); DT::datatable(st$biblio$log, options = list(dom = "t"), rownames = FALSE) })
  output$bib_table <- DT::renderDT({ M <- st$biblio$M; req(M)
    cols <- intersect(c("AU", "PY", "TI", "SO", "TC", "DT", "DB"), names(M))
    DT::datatable(as.data.frame(M)[, cols], rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$bib_xlsx <- downloadHandler(function() paste0("ANCHOR_bibliometrix_", Sys.Date(), ".xlsx"),
    function(file) { req(st$biblio$M); openxlsx::write.xlsx(as.data.frame(st$biblio$M), file) })
  output$bib_rds <- downloadHandler(function() paste0("ANCHOR_bibliometrix_", Sys.Date(), ".rds"), function(file) saveRDS(st$biblio$M, file))
  observeEvent(input$open_biblioshiny, {
    if (!is.null(.anchor_children$biblioshiny) && .anchor_children$biblioshiny$is_alive())
      return(showNotification("Biblioshiny is already running in another browser tab.", type = "message"))
    rs <- rscript_path()
    ok <- tryCatch({ .anchor_children$biblioshiny <- processx::process$new(rs, c("-e", biblioshiny_expr(isTRUE(getOption("anchor.standalone")), biblioshiny_browser())), supervise = TRUE, cleanup = FALSE,
        stdout = file.path(user_dir(), "biblioshiny.log"), stderr = "2>&1"); TRUE },
                   error = function(e) { showNotification(conditionMessage(e), type = "error"); FALSE })
    if (ok) showModal(modalDialog(title = "Biblioshiny is starting", easyClose = TRUE,
      p("The full Biblioshiny interface opens in a new browser tab in a few seconds. It closes automatically when ANCHOR Studio stops."),
      p("To analyse the same records there: download 'Dataset for Biblioshiny (.xlsx)' here, then in Biblioshiny choose Data > Load data > 'Load bibliometrix file'.")))
  })

  # ---------------- bibliometrics: analyses (one module per group)
  for (g in unique(vapply(ANALYSES, `[[`, "", "group"))) local({
    group <- g
    moduleServer(paste0("an_", gsub("[^a-z]", "", tolower(group))), function(input, output, session) {
      ns <- session$ns
      res <- reactiveVal(NULL); err <- reactiveVal(NULL)
      output$opts <- renderUI({
        o <- ANALYSES[[input$which]]$opts
        tagList(
          if ("field" %in% o) selectInput(ns("field"), "Field", c("Author keywords" = "DE", "Keywords Plus" = "ID", "Titles" = "TI", "Abstracts" = "AB")),
          if ("nnodes" %in% o) sliderInput(ns("nnodes"), "Number of nodes", 10, 100, 40, step = 5),
          if ("level" %in% o) selectInput(ns("level"), "Level", c("Authors" = "authors", "Institutions" = "universities", "Countries" = "countries")),
          if ("clevel" %in% o) selectInput(ns("clevel"), "Unit", c("Documents" = "references", "Sources" = "sources", "Authors" = "authors")),
          if (isTRUE(ANALYSES[[input$which]]$slow)) p(class = "small text-muted", icon("clock"), " Runs in the background (about 10-60 seconds); you can keep working meanwhile."),
          if ("cuts" %in% o) textInput(ns("cuts"), "Cutting years (e.g. 2010, 2018)", ""),
          if ("tf" %in% o) tagList(
            selectInput(ns("tf1"), "Left field", c("Authors" = "AU", "Keywords" = "DE", "Sources" = "SO", "Countries" = "AU_CO", "Cited sources" = "CR_SO"), "AU"),
            selectInput(ns("tf2"), "Middle field", c("Keywords" = "DE", "Authors" = "AU", "Sources" = "SO", "Countries" = "AU_CO"), "DE"),
            selectInput(ns("tf3"), "Right field", c("Sources" = "SO", "Keywords" = "DE", "Authors" = "AU", "Countries" = "AU_CO"), "SO")))
      })
      job <- reactiveVal(NULL)
      finish <- function(out, which) {
        if (!is.null(out) && out$kind == "base" && is.null(out$png)) {
          # draw base-graphics networks once; the screen, PNG download and report reuse the file
          tf <- tempfile(fileext = ".png")
          ok <- tryCatch({ grDevices::png(tf, width = 1500, height = 930, res = 150); out$plot(); TRUE },
                         error = function(e) { err(conditionMessage(e)); FALSE }, finally = grDevices::dev.off())
          if (ok && file.exists(tf)) out$png <- tf else out <- NULL
          if (!is.null(out)) { tp <- tempfile(fileext = ".pdf")   # vector copy for the PDF download and the LaTeX package
            if (isTRUE(tryCatch({ pdf_device()(tp, width = 10, height = 6.2); out$plot(); TRUE }, error = function(e) FALSE, finally = grDevices::dev.off()))) out$pdf <- tp }
        }
        if (!is.null(out)) { err(NULL); out$id <- which; res(out) } else res(NULL)
      }
      observeEvent(input$run, {
        b <- st$biblio
        if (is.null(b$M)) { err("Import data first under Bibliometrics > Data."); res(NULL); return() }
        if (!is.null(job())) return(showNotification("An analysis is already running in this group. Wait for it or press Cancel.", type = "warning"))
        o <- list(field = input$field, nnodes = input$nnodes, level = input$level, clevel = input$clevel, cuts = input$cuts, tf = c(input$tf1, input$tf2, input$tf3))
        o$tf <- if (length(o$tf) == 3) o$tf else NULL
        which <- input$which
        if (isTRUE(ANALYSES[[which]]$slow)) {
          # slow analyses run in a separate R process so the app stays responsive
          p <- tryCatch(callr::r_bg(bg_analysis, args = list(app_dir = getwd(), id = which, M = b$M, R = b$R, o = o, libs = .libPaths(), png_path = tempfile(fileext = ".png"), pdf_path = tempfile(fileext = ".pdf")),
                                    supervise = TRUE, package = FALSE), error = function(e) NULL)
          if (!is.null(p)) { .anchor_children[[session$ns("job")]] <- p
            job(list(p = p, which = which, t0 = Sys.time())); err(NULL); res(NULL); return() }
        }
        withProgress(message = paste("Running", ANALYSES[[which]]$label, "..."), value = .5, {
          out <- tryCatch(run_analysis(which, b$M, b$R, o), error = function(e) { err(friendly_error(conditionMessage(e), b$M)); NULL })
        })
        finish(out, which)
      })
      observe({ j <- job(); req(j)
        if (j$p$is_alive()) { invalidateLater(700); return() }
        out <- tryCatch(j$p$get_result(), error = function(e) { m <- conditionMessage(e)
          if (!is.null(e$parent)) m <- conditionMessage(e$parent); err(friendly_error(m, isolate(st$biblio$M))); NULL })
        job(NULL); rm(list = intersect(session$ns("job"), ls(.anchor_children)), envir = .anchor_children)
        finish(out, j$which)
      })
      observeEvent(input$cancel, { j <- job(); if (!is.null(j)) { try(j$p$kill_tree(), silent = TRUE); job(NULL); err("Analysis cancelled.") } })
      output$status <- renderUI({ j <- job()
        if (!is.null(j)) { invalidateLater(1000)
          return(div(class = "alert alert-info d-flex align-items-center gap-3", icon("spinner", class = "fa-spin"),
                     span(sprintf("Running %s in the background (%d s). You can keep working.", ANALYSES[[j$which]]$label, round(as.numeric(difftime(Sys.time(), j$t0, units = "secs"))))),
                     actionButton(session$ns("cancel"), "Cancel", class = "btn-sm btn-outline-secondary ms-auto"))) }
        if (!is.null(err())) div(class = "alert alert-warning", err()) else if (is.null(res())) div(class = "alert alert-secondary small", "Choose an analysis and press Run.") })
      output$plot_ui <- renderUI({ r <- res(); req(r)
        if (r$kind == "plotly") plotly::plotlyOutput(ns("plotly"), height = "620px") else if (r$kind == "table") NULL
        else if (r$kind == "base") imageOutput(ns("img"), height = "auto") else plotOutput(ns("plot"), height = "620px") })
      output$img <- renderImage({ r <- res(); req(r, r$kind == "base", r$png)
        list(src = r$png, contentType = "image/png", style = "width:100%;height:auto", alt = r$label) }, deleteFile = FALSE)
      output$plot <- renderPlot({ r <- res(); req(r, r$kind %in% c("gg", "base")); if (r$kind == "gg") print(r$plot) else r$plot() }, res = 96)
      output$plotly <- plotly::renderPlotly({ r <- res(); req(r, r$kind == "plotly"); r$plot })
      output$note <- renderUI({ r <- res(); req(r); if (!is.null(r$note)) p(class = "small text-muted", r$note) })
      output$table <- DT::renderDT({ r <- res(); req(r, !is.null(r$table)); DT::datatable(as.data.frame(r$table), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
      save_png <- function(r, file) {
        if (r$kind == "gg") ggplot2::ggsave(file, r$plot, width = 10, height = 6.2, dpi = 150, bg = "white", device = gg_device())
        else if (r$kind == "base") file.copy(r$png, file, overwrite = TRUE)
        else stop("Interactive figures cannot be saved as PNG here; use the camera icon on the figure, or add the table.")
      }
      output$fig_static <- reactive({ r <- res(); isTRUE(!is.null(r) && r$kind %in% c("gg", "base")) })
      output$fig_interactive <- reactive({ r <- res(); isTRUE(!is.null(r) && identical(r$kind, "plotly")) })
      output$has_table <- reactive({ r <- res(); isTRUE(!is.null(r) && !is.null(r$table) && NROW(r$table) > 0) })
      for (o in c("fig_static", "fig_interactive", "has_table")) outputOptions(output, o, suspendWhenHidden = FALSE)
      output$dl_png <- downloadHandler(function() paste0(res()$id %||% "figure", ".png"), function(file) save_png(res(), file))
      output$dl_pdf <- downloadHandler(function() paste0(res()$id %||% "figure", ".pdf"), function(file) save_pdf_fig(res(), file))
      output$dl_csv <- downloadHandler(function() paste0(res()$id %||% "table", ".csv"), function(file) utils::write.csv(as.data.frame(res()$table), file, row.names = FALSE))
      observeEvent(input$add, {
        r <- res(); if (is.null(r)) return(showNotification("Run an analysis first.", type = "warning"))
        png_raw <- NULL; pdf_raw <- NULL
        if (r$kind %in% c("gg", "base")) { tf <- tempfile(fileext = ".png"); try(save_png(r, tf), silent = TRUE)
          if (file.exists(tf)) png_raw <- readBin(tf, "raw", file.info(tf)$size)
          tp <- tempfile(fileext = ".pdf"); try(save_pdf_fig(r, tp), silent = TRUE)
          if (file.exists(tp) && file.size(tp) > 0) pdf_raw <- readBin(tp, "raw", file.info(tp)$size) }
        item <- list(label = r$label, note = paste(c(r$note, if (r$kind == "plotly") "Interactive figure in the app; table shown."), collapse = " "),
                     table = if (!is.null(r$table)) head(as.data.frame(r$table), 30) else NULL, png_raw = png_raw, pdf_raw = pdf_raw)
        st$report_items <- c(isolate(st$report_items), list(item))
        showNotification(paste(r$label, "added to the report (phase C)."), type = "message")
      })
    })
  })

  output$report_items_ui <- renderUI({
    its <- st$report_items
    if (!length(its)) return(p(class = "small text-muted", "None yet. Run analyses under Bibliometrics and press 'Add to report'."))
    tags$ol(lapply(seq_along(its), function(k) tags$li(its[[k]]$label, if (identical(its[[k]]$phase, "O")) span(class = "text-muted small", " (analysis result, shown in phase O)"), " ", actionLink(paste0("rm_item_", k), "remove", class = "small"))))
  })
  observe({ n <- length(st$report_items); lapply(seq_len(n), function(k) observeEvent(input[[paste0("rm_item_", k)]], {
    its <- isolate(st$report_items); if (k <= length(its)) st$report_items <- its[-k] }, once = TRUE, ignoreInit = TRUE)) })
  observeEvent(input$goto_biblio, nav_select("nav", "bib_data"))
  observeEvent(input$goto_prereq, nav_select("nav", "prereq"))
  add_report_item <- function(label, plot_fn, table, phase, code = NULL, note = "") {
    png_raw <- NULL; pdf_raw <- NULL
    tp <- tempfile(fileext = ".png"); ok <- tryCatch({ grDevices::png(tp, 1500, 930, res = 150); plot_fn(); TRUE }, error = function(e) FALSE, finally = grDevices::dev.off())
    if (ok && file.exists(tp)) png_raw <- readBin(tp, "raw", file.size(tp))
    tf <- tempfile(fileext = ".pdf"); ok <- tryCatch({ pdf_device()(tf, width = 10, height = 6.2); plot_fn(); TRUE }, error = function(e) FALSE, finally = grDevices::dev.off())
    if (ok && file.exists(tf)) pdf_raw <- readBin(tf, "raw", file.size(tf))
    st$report_items <- c(isolate(st$report_items), list(list(label = label, note = note, table = table, png_raw = png_raw, pdf_raw = pdf_raw, phase = phase, code = code)))
    log_decision(phase, "Added to the report", label); showNotification(sprintf("Added to the report (phase %s).", phase), type = "message") }
  prereq_server(input, output, session, st, rv, log_decision)
  find_server(input, output, session, st, log_decision, log_prompt, load_article)
  conn <- connectors_server(input, output, session, st, rv, set_biblio, log_decision, add_report_item)

  # ---------------- analysis step (phase O): data and methods
  ds_list <- reactive(st$analysis$datasets %||% list())
  set_ds <- function(ds) { a <- isolate(st$analysis) %||% list(); a$datasets <- ds; st$analysis <- a }
  want_active <- reactiveVal(NULL)
  add_ds <- function(d) { ds <- isolate(ds_list()); d$name <- make.unique(c(names(ds), d$name))[length(ds) + 1]; ds[[d$name]] <- d; want_active(d$name); set_ds(ds)
    updateSelectInput(session, "dm_active", choices = names(ds), selected = d$name)
    log_decision("O", "Dataset added", sprintf("%s: %s (%s), %d rows, sha256 %s", d$name, d$source, d$query, d$rows, substr(d$sha256, 1, 16)))
    showNotification(sprintf("%s: %d rows, %d columns.", d$name, d$rows, d$cols), type = "message") }
  observe({ ds <- ds_list(); cur <- isolate(want_active()) %||% isolate(input$dm_active); want_active(NULL)
    updateSelectInput(session, "dm_active", choices = names(ds), selected = if (length(cur) && cur %in% names(ds)) cur else utils::tail(names(ds), 1))
    updateSelectizeInput(session, "dm_merge_with", choices = setdiff(names(ds), cur)) })
  observeEvent(input$dm_include_data, { a <- st$analysis %||% list(); a$include_data <- isTRUE(input$dm_include_data); st$analysis <- a }, ignoreInit = TRUE)
  observeEvent(input$dm_import, { f <- input$dm_file; if (is.null(f)) return(showNotification("Choose a file first.", type = "warning"))
    d <- tryCatch(import_dataset(f$datapath, f$name, input$dm_name, isTRUE(input$dm_licensed)), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL })
    if (!is.null(d)) add_ds(d) })
  output$dm_src_opts <- renderUI(switch(input$dm_src %||% "worldbank",
    worldbank = tagList(textInput("dm_wb_ind", "Indicator codes", "NY.GDP.PCAP.KD, SP.POP.TOTL", width = "100%"), textInput("dm_wb_cty", "Countries (ISO3, or all)", "EGY", width = "100%"),
                        layout_columns(numericInput("dm_wb_from", "From", 2010), numericInput("dm_wb_to", "To", as.integer(format(Sys.Date(), "%Y")) - 1))),
    fred = tagList(textInput("dm_fred_ids", "Series ids", "DGS10, CPIAUCSL", width = "100%"), dateRangeInput("dm_fred_dates", NULL, start = "2010-01-01", end = Sys.Date()),
                   p(class = "small text-muted", "Needs a free FRED API key under Settings.")),
    french = selectInput("dm_ff_set", "Factor set", FRENCH_SETS, width = "100%"),
    yahoo = tagList(textInput("dm_y_tk", "Tickers", "COMI.CA", width = "100%"), dateRangeInput("dm_y_dates", NULL, start = Sys.Date() - 365, end = Sys.Date()),
                    p(class = "small text-warning", "Teaching and pilots only: Yahoo data is for personal use. Use licensed data for research.")),
    gdelt = tagList(textInput("dm_g_q", "News search", "\"Commercial International Bank\" sustainability", width = "100%"),
                    p(class = "small text-muted", "Average tone of matching online news, last three months only. Not an ESG score; validate before use.")),
    sec = tagList(textInput("dm_sec_ids", "Tickers or CIK numbers (US-listed firms)", "AAPL, MSFT", width = "100%"),
                  layout_columns(numericInput("dm_sec_from", "Fiscal years from", 2010), numericInput("dm_sec_to", "to", as.integer(format(Sys.Date(), "%Y")) - 1)),
                  p(class = "small text-muted", "Annual 10-K values: assets, liabilities, equity, revenue, net income, capex, PP&E, SG&A, R&D, advertising, SIC; ratios ROE, leverage, capex/PP&E, SG&A, R&D and advertising to sales. Free substitute for Compustat controls (US only). Needs a contact email under Settings.")),
    imf = tagList(layout_columns(col_widths = c(4, 8), textInput("dm_imf_flow", "Dataflow", "CPI"), textInput("dm_imf_key", "Series key", "EGY.CPI._T.IX.M")),
                  textInput("dm_imf_start", "From (period)", "2010-M01"),
                  p(class = "small text-muted", "IMF SDMX 3.0 API; key = dimensions joined by dots (here: Egypt, consumer prices, all items, index, monthly). Browse dataflows and keys at data.imf.org."))))
  observeEvent(input$dm_fetch, {
    if (isFALSE(input$online)) return(showNotification("Switch on the internet features in Settings first.", type = "warning"))
    cfg <- online_cfg(isolate(st$settings))
    d <- withProgress(message = "Fetching open data...", value = .5, tryCatch(switch(input$dm_src,
      worldbank = fetch_worldbank(input$dm_wb_ind, input$dm_wb_cty, input$dm_wb_from, input$dm_wb_to, cfg),
      fred = fetch_fred(input$dm_fred_ids, input$fred_key, input$dm_fred_dates[1], input$dm_fred_dates[2], cfg),
      french = fetch_french(input$dm_ff_set, cfg), yahoo = fetch_yahoo(input$dm_y_tk, input$dm_y_dates[1], input$dm_y_dates[2], cfg),
      gdelt = fetch_gdelt(input$dm_g_q, cfg), sec = fetch_sec(input$dm_sec_ids, input$dm_sec_from, input$dm_sec_to, cfg),
      imf = fetch_imf(input$dm_imf_flow, input$dm_imf_key, input$dm_imf_start, cfg)), error = function(e) { showNotification(paste("Could not fetch:", conditionMessage(e)), type = "error", duration = 15); NULL }))
    if (!is.null(d)) add_ds(d) })
  output$dm_ds_table <- DT::renderDT({ pv <- provenance_df(ds_list()); if (is.null(pv)) pv <- data.frame(Dataset = character(), Source = character())
    DT::datatable(pv, rownames = FALSE, options = list(dom = "t", pageLength = 50, scrollX = TRUE)) })
  active_ds <- reactive({ ds <- ds_list(); n <- input$dm_active; if (is.null(n) || !n %in% names(ds)) NULL else ds[[n]] })
  output$dm_preview <- DT::renderDT({ d <- active_ds(); req(d, d$data); DT::datatable(utils::head(d$data, 500), rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE),
    caption = sprintf("%s \u00b7 %s \u00b7 %s%s", d$name, d$source, d$query, if (nzchar(d$note %||% "")) paste(" \u00b7", d$note) else "")) })
  observe({ a <- active_ds(); w <- ds_list()[[input$dm_merge_with %||% ""]]; if (is.null(a) || is.null(w)) return()
    updateSelectizeInput(session, "dm_merge_keys", choices = intersect(names(a$data), names(w$data)), selected = isolate(input$dm_merge_keys)) })
  observeEvent(input$dm_merge, { a <- active_ds(); w <- ds_list()[[input$dm_merge_with %||% ""]]
    if (is.null(a) || is.null(w) || !length(input$dm_merge_keys)) return(showNotification("Choose two datasets and the key columns.", type = "warning"))
    d <- tryCatch(merge_datasets(a, w, input$dm_merge_keys, ""), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }); if (!is.null(d)) add_ds(d) })
  observeEvent(input$dm_remove, { n <- input$dm_active; ds <- ds_list(); if (is.null(n) || !n %in% names(ds)) return(); ds[[n]] <- NULL; set_ds(ds)
    log_decision("O", "Dataset removed", n) })
  output$dm_badge <- renderUI({ n <- length(ds_list()); r <- length(st$analysis$results %||% list())
    span(class = "badge text-bg-secondary", sprintf("%d dataset%s \u00b7 %d result%s", n, if (n == 1) "" else "s", r, if (r == 1) "" else "s")) })

  # methods: options built from the chosen dataset's columns; templates prefill them
  tpl <- reactiveVal(list())
  observeEvent(input$dm_template, { t <- TEMPLATES[[input$dm_template %||% ""]]; if (is.null(t)) return(); tpl(t); updateSelectInput(session, "dm_method", selected = t$method) })
  output$dm_opts <- renderUI({
    d <- active_ds(); m <- input$dm_method; t <- tpl(); if (!identical(t$method, m)) t <- list()
    if (is.null(d)) return(div(class = "alert alert-secondary small", "Import or fetch a dataset under Data first."))
    cols <- names(d$data); num <- cols[vapply(d$data, is.numeric, TRUE)]
    sel <- function(id, label, choices, value, multiple = FALSE) {
      ch <- unique(c(value[nzchar(value %||% "")], choices))
      if (!multiple) ch <- c("(none)" = "", stats::setNames(ch, ch))
      selectizeInput(id, label, choices = ch, selected = value %||% "", multiple = multiple, width = "100%", options = list(create = TRUE)) }
    txt <- function(id, label, value, ph = NULL) textInput(id, label, value %||% "", width = "100%", placeholder = ph)
    switch(m,
      describe = sel("dm_o_vars", "Variables (empty = all numeric)", num, t$vars %||% character(0), TRUE),
      regress = tagList(layout_columns(sel("dm_o_y", "Dependent variable", cols, t$y %||% ""), selectInput("dm_o_family", "Model", c("Linear (OLS)" = "ols", "Logit" = "logit", "Poisson" = "poisson"), t$family %||% "ols")),
        txt("dm_o_rhs", "Right-hand side (use * for interactions)", t$rhs, "x1 * x2 + control1 + control2"),
        layout_columns(txt("dm_o_fe", "Fixed effects (optional)", t$fe, "industry + date"), txt("dm_o_iv", "Instrumental variables (optional)", t$iv, "endogenous ~ instrument")),
        layout_columns(sel("dm_o_cluster", "Cluster standard errors by (empty = robust)", cols, t$cluster %||% character(0), TRUE), sel("dm_o_split", "Estimate separately by (optional)", cols, t$split %||% ""))),
      event = tagList(layout_columns(sel("dm_o_id", "Firm id", cols, t$id %||% ""), sel("dm_o_date", "Date", cols, t$date %||% ""), sel("dm_o_ret", "Stock return", cols, t$ret %||% "")),
        layout_columns(sel("dm_o_mkt", "Market return", cols, t$mkt %||% ""), sel("dm_o_event", "Event day (1 = event)", cols, t$event %||% "")),
        layout_columns(numericInput("dm_o_ef", "Estimation from", t$est_from %||% -260), numericInput("dm_o_et", "to", t$est_to %||% -11),
                       numericInput("dm_o_wf", "Event window from", t$ev_from %||% -1), numericInput("dm_o_wt", "to", t$ev_to %||% 1))),
      alpha = tagList(layout_columns(sel("dm_o_y", "Portfolio (excess) return", cols, t$y %||% ""), sel("dm_o_time", "Time", cols, t$time %||% ""), numericInput("dm_o_lag", "Newey-West lags", t$lag %||% 6)),
        sel("dm_o_factors", "Factors", num, t$factors %||% character(0), TRUE)),
      meta = tagList(layout_columns(sel("dm_o_yi", "Effect size", num, t$yi %||% ""), sel("dm_o_vi", "Sampling variance (or leave empty)", num, t$vi %||% ""), sel("dm_o_sei", "Standard error (if no variance)", num, "")),
        layout_columns(sel("dm_o_slab", "Study label", cols, t$slab %||% ""), sel("dm_o_mods", "Moderators (optional)", cols, t$mods %||% character(0), TRUE),
                       selectInput("dm_o_tau", "Between-study variance", c("REML", "DL", "ML", "PM", "EE (fixed effect)" = "EE")))),
      did = tagList(layout_columns(sel("dm_o_y", "Outcome", cols, ""), sel("dm_o_id", "Unit id", cols, ""), sel("dm_o_time", "Time period", cols, ""), sel("dm_o_cohort", "First treated period", cols, "")),
        txt("dm_o_rhs", "Controls (optional)", "", "x1 + x2"), p(class = "small text-muted", "Never-treated units: give a first-treated value outside the sample period (for example 10000).")),
      multilevel = tagList(txt("dm_o_fixed", "Fixed part", "", "y ~ x1 + x2"), txt("dm_o_random", "Random part", "", "~ 1 | group")),
      sem = textAreaInput("dm_o_model", "Model (lavaan syntax)", "", rows = 6, width = "100%", placeholder = "ESG =~ e1 + e2 + e3\nret ~ ESG + size"))
  })
  observe({ d <- active_ds(); if (is.null(d) || is.null(d$data)) return(); cols <- names(d$data); num <- cols[vapply(d$data, is.numeric, TRUE)]
    updateSelectizeInput(session, "dm_rv_cols", choices = num, selected = intersect(isolate(input$dm_rv_cols), num))
    updateSelectizeInput(session, "dm_rv_by", choices = c("(whole sample)" = "", cols), selected = intersect(isolate(input$dm_rv_by), cols)) })
  observeEvent(input$dm_rv_go, { d <- active_ds(); if (is.null(d) || is.null(d$data)) return(showNotification("Choose a dataset first.", type = "warning"))
    r <- tryCatch(build_rating_vars(d$data, input$dm_rv_cols, input$dm_rv_by, input$dm_rv_kind), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL }); if (is.null(r)) return()
    nd <- new_dataset(paste0(d$name, "_ratings"), r, "Derived in ANCHOR Studio", sprintf("%s: consensus and %s of %s (z-scores within %s)", d$name, if (input$dm_rv_kind == "pillars") "pillar dispersion" else "disagreement",
      paste(input$dm_rv_cols, collapse = ", "), input$dm_rv_by %|e|% "the whole sample"), licensed = isTRUE(d$licensed),
      note = if (input$dm_rv_kind == "pillars") "Pillar dispersion is a weaker proxy for rater disagreement; say so in the limitations." else "")
    add_ds(nd) })
  output$dm_tpl_note <- renderUI({ t <- TEMPLATES[[input$dm_template %||% ""]]; if (is.null(t)) return(NULL); ra <- raters_available(st$prereq %||% list())
    tagList(if (nzchar(t$note %||% "")) div(class = "alert alert-light border small", t$note),
            if (isTRUE(t$needs_raters) && length(ra) < 2) div(class = "alert alert-warning small", icon("triangle-exclamation"),
              sprintf(" Before you start records %d ESG rating famil%s (%s). This template needs disagreement across at least two raters: use option 1 (hand-collected second rater), option 2 (pillar dispersion) or option 3 (consensus only).",
                      length(ra), if (length(ra) == 1) "y" else "ies", if (length(ra)) paste(ra, collapse = ", ") else "none"))) })
  dm_res <- reactiveVal(NULL)
  observeEvent(input$dm_run, {
    d <- active_ds(); if (is.null(d) || is.null(d$data)) return(showNotification("Choose a dataset under Data first (licensed data must be imported again after reopening a project).", type = "warning"))
    o <- list(vars = input$dm_o_vars, y = input$dm_o_y, rhs = input$dm_o_rhs, fe = input$dm_o_fe, iv = input$dm_o_iv, cluster = input$dm_o_cluster, split = input$dm_o_split,
              family = input$dm_o_family, id = input$dm_o_id, date = input$dm_o_date, ret = input$dm_o_ret, mkt = input$dm_o_mkt, event = input$dm_o_event,
              est_from = input$dm_o_ef, est_to = input$dm_o_et, ev_from = input$dm_o_wf, ev_to = input$dm_o_wt, time = input$dm_o_time, lag = input$dm_o_lag,
              factors = input$dm_o_factors, yi = input$dm_o_yi, vi = input$dm_o_vi, sei = input$dm_o_sei, slab = input$dm_o_slab, mods = input$dm_o_mods,
              tau_method = input$dm_o_tau, cohort = input$dm_o_cohort, fixed = input$dm_o_fixed, random = input$dm_o_random, model = input$dm_o_model)
    r <- withProgress(message = "Running...", value = .5, tryCatch(run_method(input$dm_method, d$data, o, d$name), error = function(e) e))
    if (inherits(r, "error")) return(showNotification(conditionMessage(r), type = "error", duration = 15))
    r$dataset <- d$name; r$time <- format(Sys.time(), "%Y-%m-%d %H:%M"); r$method <- input$dm_method
    a <- isolate(st$analysis) %||% list(); a$results <- c(a$results %||% list(), list(r[c("label", "code", "dataset", "time", "method")])); st$analysis <- a
    dm_res(r); nav_select("dm_tabs", "results") })
  output$dm_result_head <- renderUI({ r <- dm_res(); if (is.null(r)) return(div(class = "alert alert-secondary small", "Run a method to see results here."))
    div(h5(r$label), p(class = "small text-muted", sprintf("Dataset %s \u00b7 %s", r$dataset, r$time))) })
  output$dm_result_table <- DT::renderDT({ r <- dm_res(); req(r, r$table); DT::datatable(r$table, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "tip")) })
  output$dm_extra_tables <- renderUI({ r <- dm_res(); req(r); if (!length(r$tables)) return(NULL)
    tagList(lapply(names(r$tables), function(nm) tagList(h6(class = "mt-3", nm), tags$div(class = "table-responsive", HTML(knit_table(r$tables[[nm]])))))) })
  output$dm_plots_ui <- renderUI({ r <- dm_res(); req(r); if (!length(r$plots)) return(NULL)
    layout_columns(col_widths = rep(12 / min(2, length(r$plots)), length(r$plots)), !!!lapply(seq_along(r$plots), function(k) plotOutput(paste0("dm_plot_", k), height = "420px"))) })
  observe({ r <- dm_res(); req(r); lapply(seq_along(r$plots), function(k) output[[paste0("dm_plot_", k)]] <- renderPlot(r$plots[[k]](), res = 96)) })
  output$dm_text <- renderText({ r <- dm_res(); req(r); r$text })
  output$dm_code <- renderText({ r <- dm_res(); req(r); r$code })
  output$dm_dl_table <- downloadHandler(function() paste0("ANCHOR_", dm_res()$method %||% "result", ".csv"), function(file) utils::write.csv(dm_res()$table, file, row.names = FALSE))
  output$dm_dl_pkg <- downloadHandler(function() stamp("_analysis.zip"), function(file) build_analysis_zip(reactiveValuesToList(st), file, isTRUE(input$dm_pkg_licensed)))
  observeEvent(input$dm_add, { r <- dm_res(); if (is.null(r)) return()
    png_raw <- NULL; pdf_raw <- NULL
    if (length(r$plots)) { tp <- tempfile(fileext = ".png"); ok <- tryCatch({ grDevices::png(tp, 1500, 930, res = 150); r$plots[[1]](); TRUE }, error = function(e) FALSE, finally = grDevices::dev.off())
      if (ok && file.exists(tp)) png_raw <- readBin(tp, "raw", file.size(tp))
      tf <- tempfile(fileext = ".pdf"); ok <- tryCatch({ pdf_device()(tf, width = 10, height = 6.2); r$plots[[1]](); TRUE }, error = function(e) FALSE, finally = grDevices::dev.off())
      if (ok && file.exists(tf)) pdf_raw <- readBin(tf, "raw", file.size(tf)) }
    item <- list(label = r$label, note = sprintf("Dataset %s; %s.", r$dataset, r$time), table = utils::head(as.data.frame(r$table), 30), png_raw = png_raw, pdf_raw = pdf_raw, phase = "O", code = r$code)
    st$report_items <- c(isolate(st$report_items), list(item)); log_decision("O", "Analysis result added to the report", r$label)
    showNotification("Added to the report (phase O).", type = "message") })

  # ---------------- report and disclosure
  snapshot <- function() {
    s <- reactiveValuesToList(st); s$settings$api_key <- ""; s$settings$crossref_mailto <- ""; s$settings$openalex_key <- ""; s$settings$fred_key <- ""
    s$report_items <- lapply(s$report_items, function(it) { if (!is.null(it$png_raw)) { tf <- tempfile(fileext = ".png"); writeBin(it$png_raw, tf); it$png <- tf }; it })
    s
  }
  output$disc_preview <- renderText(disclosure_text(snapshot()))
  output$report_preview <- renderUI(tags$iframe(srcdoc = build_report_html(snapshot()), sandbox = "", style = "width:100%;height:75vh;border:1px solid #D5D9DF;border-radius:8px;background:white"))
  stamp <- function(ext) paste0("ANCHOR_", safe_name(st$article$title), "_", Sys.Date(), ext)
  output$dl_html <- downloadHandler(function() stamp(".html"), function(file) writeLines(build_report_html(snapshot()), file, useBytes = TRUE))
  output$dl_docx <- downloadHandler(function() stamp(".docx"), function(file) build_report_docx(snapshot(), file))
  output$dl_pdf_report <- downloadHandler(function() stamp(".pdf"), function(file) {
    id <- showNotification("Building the PDF report...", duration = NULL, type = "message"); on.exit(removeNotification(id))
    used <- tryCatch(build_report_pdf(snapshot(), file, input$pdf_engine %||% "auto"), error = function(e) {
      showNotification(paste("PDF report failed:", conditionMessage(e)), type = "error", duration = 15); stop(e) })
    showNotification(paste("PDF report built with", if (used == "browser") "Edge/Chrome." else "Typst."), type = "message") })
  output$dl_latex <- downloadHandler(function() stamp("_LaTeX.zip"), function(file) build_latex_zip(snapshot(), file))
  output$dl_bib <- downloadHandler(function() stamp("_references.bib"), function(file) writeLines(enc2utf8(build_bib(snapshot())), file, useBytes = TRUE))
  output$dl_disc <- downloadHandler(function() stamp("_disclosure.txt"), function(file) writeLines(disclosure_text(snapshot()), file))
  output$dl_prompts <- downloadHandler(function() stamp("_prompts.csv"), function(file) utils::write.csv(st$prompts, file, row.names = FALSE))
  output$dl_decisions <- downloadHandler(function() stamp("_decisions.csv"), function(file) utils::write.csv(st$decisions, file, row.names = FALSE))
  strip_data <- function(s) { inc <- isTRUE(s$analysis$include_data)
    s$analysis$datasets <- lapply(s$analysis$datasets %||% list(), function(d) { if (isTRUE(d$licensed) || !inc) d$data <- NULL; d }); s }
  project_writer <- function(file) { s <- strip_data(reactiveValuesToList(st)); s$settings$api_key <- ""; s$settings$crossref_mailto <- ""; s$settings$openalex_key <- ""; s$settings$fred_key <- ""; s$app_version <- APP_VERSION; saveRDS(s, file) }
  output$dl_project <- downloadHandler(function() stamp(".anchor"), project_writer)
  output$dl_project_top <- downloadHandler(function() stamp(".anchor"), project_writer)

  # ---------------- saving and restoring
  autosave <- function() { if (!is.null(isolate(st$article$text))) { s <- isolate(reactiveValuesToList(st)); s$settings$api_key <- ""; s$settings$crossref_mailto <- ""; s$settings$openalex_key <- ""; s$settings$fred_key <- ""
    try(saveRDS(s, file.path(user_dir(), "autosave.anchor")), silent = TRUE) } }
  observe({ invalidateLater(60000); autosave() })
  output$autosave_info <- renderText({ invalidateLater(60000); af <- file.path(user_dir(), "autosave.anchor")
    paste("Autosave every minute to", af, if (file.exists(af)) paste("(last:", format(file.mtime(af), "%H:%M"), ")") else "") })
  observeEvent(input$save_now, { autosave(); showNotification("Saved.", type = "message") })

  restore <- function(s) {
    for (k in intersect(names(s), names(new_state()))) if (k != "settings") st[[k]] <- s[[k]]
    # projects saved by ANCHOR Mobile carry the records but not bibliometrix's summary object: rebuild it here
    if (!is.null(s$biblio$M) && is.null(s$biblio$R)) { b <- st$biblio; b$R <- tryCatch(bibliometrix::biblioAnalysis(s$biblio$M, sep = ";"), error = function(e) NULL); st$biblio <- b }
    a <- s$article
    updateTextInput(session, "art_title", value = a$title %||% ""); updateTextInput(session, "art_doi", value = a$doi %||% "")
    updateTextAreaInput(session, "art_citation", value = a$citation %||% "")
    updateSelectInput(session, "art_style", selected = a$cite_style %||% "apa"); cite_key(paste(norm_doi(a$doi %||% ""), a$cite_style %||% "apa"))
    if (!is.null(a$version)) updateRadioButtons(session, "art_version", selected = a$version)
    for (q in SETUP_QUESTIONS) restore_q(q, s$setup[[q$id]])
    for (p in PHASES) { ph <- s$phases[[p$id]]
      updateTextAreaInput(session, paste0(p$id, "_draft"), value = ph$draft %||% "")
      updateTextAreaInput(session, paste0(p$id, "_notes"), value = ph$notes %||% "")
      updateCheckboxGroupInput(session, paste0(p$id, "_checks"), selected = ph$checks %||% character(0))
      updateTextAreaInput(session, paste0(p$id, "_mylessons"), value = ph$my_lessons %||% "")
      updateTextAreaInput(session, paste0(p$id, "_mywiifm"), value = ph$my_wiifm %||% "")
      for (q in p$questions) restore_q(q, ph$answers[[q$id]]) }
    if (!is.null(s$biblio$M_all)) rv$bib_ver <- rv$bib_ver + 1
    rv$reload <- rv$reload + 1
    showNotification("Project restored.", type = "message")
  }
  restore_q <- function(q, v) { if (is.null(v)) return()
    switch(q$type, radio = updateRadioButtons(session, q$id, selected = v), checkbox = updateCheckboxGroupInput(session, q$id, selected = v),
           textarea = updateTextAreaInput(session, q$id, value = v), updateTextInput(session, q$id, value = v)) }
  open_project <- function(path, name = basename(path)) {
    s <- tryCatch(readRDS(path), error = function(e) e)
    if (inherits(s, "error")) return(showNotification(sprintf("%s could not be read (%s). Is it an ANCHOR Studio project file (.anchor)? A PDF or Word file is uploaded on the Start page instead.",
                                                              name, conditionMessage(s)), type = "error", duration = 15))
    if (!is.list(s) || is.null(s$phases)) return(showNotification(sprintf("%s is not an ANCHOR Studio project.", name), type = "error", duration = 12))
    removeModal(); restore(s)
    log_decision("Set-up", "Project opened", sprintf("%s (saved with version %s)", name, s$app_version %||% "before 1.1"))
    nav_select("nav", if (isTRUE(s$setup_ok)) "A" else "start") }
  open_dialog <- function() { af <- file.path(user_dir(), "autosave.anchor")
    showModal(modalDialog(title = "Open a saved project", easyClose = TRUE, footer = modalButton("Close"),
      fileInput("project_file_modal", "Project file (.anchor)", accept = c(".anchor", ".rds"), buttonLabel = "Choose file...", width = "100%"),
      p(class = "small text-muted", "Project files are saved with Save project (top right) or on Report & disclosure. On Windows you can also double-click a .anchor file."),
      if (file.exists(af)) actionButton("resume_modal", paste("Or resume the last session (autosaved", format(file.mtime(af), "%d %b %H:%M"), ")"), icon = icon("clock-rotate-left")))) }
  observeEvent(input$open_project_top, open_dialog()); observeEvent(input$open_project_start, open_dialog())
  observeEvent(input$project_file, open_project(input$project_file$datapath, input$project_file$name))
  observeEvent(input$project_file_modal, open_project(input$project_file_modal$datapath, input$project_file_modal$name))
  observeEvent(input$resume, open_project(file.path(user_dir(), "autosave.anchor"), "Last session"))
  observeEvent(input$resume_modal, open_project(file.path(user_dir(), "autosave.anchor"), "Last session"))
  # a .anchor file double-clicked in Windows Explorer (passed on by the launcher)
  local({ of <- getOption("anchor.open_file")
    if (!is.null(of) && nzchar(of) && file.exists(of)) { options(anchor.open_file = NULL)
      session$onFlushed(function() isolate(open_project(of)), once = TRUE) } })
  observeEvent(input$about_top, showModal(modalDialog(title = NULL, easyClose = TRUE, footer = modalButton("Close"),
    div(class = "brand-about", h5(class = "mb-1", APP_BRAND, " ", APP_VERSION), p(class = "brand-tag mb-1", APP_TAGLINE),
        p(class = "small text-muted", "ANCHOR framework by Amira Mamdouh Kastour \u00b7 bibliometric engine: bibliometrix (Aria & Cuccurullo, 2017) \u00b7 developed with Claude (Anthropic)."),
        tribute_block()))))
}

`%|na|%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# Biblioshiny installs any package it misses from CRAN at start. R started with --vanilla has no CRAN mirror set, which stopped it
# with "trying to use CRAN without setting a mirror". Set a mirror; in the standalone app every package is bundled, so installing
# is skipped there (Biblioshiny also asks for webshot2 0.1.0 exactly; the bundled 0.1.2 works).
biblioshiny_expr <- function(bundled, browser = "") paste0(
  "options(repos = c(CRAN = 'https://cloud.r-project.org')); ",
  if (nzchar(browser)) sprintf("Sys.setenv(PAGEDOWN_CHROME = '%s'); ", gsub("'", "\\\\'", browser)) else "",
  if (bundled) "install.packages <- function(pkgs, ...) message('ANCHOR Studio: not installing ', paste(pkgs, collapse = ', '), ' (bundled app)'); " else "",
  "bibliometrix::biblioshiny(launch.browser = TRUE)")

# Biblioshiny's server looks for Chrome or Edge in the registry at start (pagedown::find_chrome) and stops if it is not found
# there; point it at the browser directly when one is installed in a usual place
biblioshiny_browser <- function() {
  if (nzchar(Sys.getenv("PAGEDOWN_CHROME"))) return("")
  b <- if (.Platform$OS.type == "windows") c(file.path(Sys.getenv("ProgramFiles(x86)"), "Microsoft/Edge/Application/msedge.exe"),
    file.path(Sys.getenv("ProgramFiles"), "Microsoft/Edge/Application/msedge.exe"), file.path(Sys.getenv("ProgramFiles"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("ProgramFiles(x86)"), "Google/Chrome/Application/chrome.exe"), file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe"))
    else c("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge")
  b <- b[file.exists(b)]
  if (length(b)) normalizePath(b[1], winslash = "/") else ""
}

rscript_path <- function() {
  exe <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  cand <- c(file.path(R.home("bin"), exe), Sys.which("Rscript"))
  cand[file.exists(cand)][1]
}

shinyApp(ui, server)
